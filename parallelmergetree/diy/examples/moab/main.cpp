//---------------------------------------------------------------------------
//
// example of reading a MOAB mesh and using it to create a DIY decomposition
// and then using DIY to do a ghost cell exchange of a certain size ghost region
//
// requires having MOAB installed, which can be checked out from svn:
// https://svn.mcs.anl.gov/repos/ITAPS/moab/trunk
// the sample mesh  64bricks_512hex.h5m can be found in MOAB's
// MeshFiles/unittest directory
//
// this example assumes a decomposition of 1 DIY block = 1 MOAB hex
//
// run as follows (for example, with 2 processes, where the executable is
//  named moab)):
// mpiexec -n 2 ./moab <oath to sample data>/64bricks_512hex.h5m
//
// Tom Peterka
// Argonne National Laboratory
// 9700 S. Cass Ave.
// Argonne, IL 60439
// tpeterka@mcs.anl.gov
//
// (C) 2011 by Argonne National Laboratory.
// See COPYRIGHT in top-level directory.
//
//--------------------------------------------------------------------------

#include <iostream> 
#include "mpi.h"
#include "iMesh.h"
#include "MBiMesh.hpp"
#include "MBCore.hpp"
#include "MBRange.hpp"
#include "MBTagConventions.hpp"
#include "moab/ParallelComm.hpp"
#include <set>
#include <stddef.h>

#include "diy.h"

using namespace std;
using namespace moab;

#define MAX_NUM_NEIGHBORS 256 // maximum number of neighbors for a block
                              // temporarury solution for this example
#define MAX_NUM_CELLS 512 // maximum number of cells for a block plus its 
                          // ghost-cells
#define QUERY_EXTEND_T 0.0001 // the size of the ghost zone t

struct gb_send {  //global block to send, no vid infomation comparing to gb_t
  int gid; // global block id
  int proc; // process to which block is assigned
  struct bb_t bb; // block bounds 
};

// function prototypes
int main( int argc, char *argv[] );
void Decompose(Range blocks, ParallelComm *pcomm, Interface *mb);
void DecomposeBlocks(int *gids, bb_t *bounds, vector<ri_t> *rem_data,
		     vector<int> *vids, gb_t **neighbors, int *num_neighbors, 
		     Range blocks, ParallelComm *pcomm, Interface *mb);
void BlockBounds(bb_t *bounds, Range verts, Interface *mb);
int BlockNeighbors(gb_t *neighbors, vector<ri_t> &rem_data, 
		   vector<int> &vids, EntityHandle hex, ParallelComm *pcomm, 
		   Interface *mb);
void RemoteData(vector<ri_t> &rem_data, vector<int> &vids, Range verts, 
		ParallelComm *pcomm, Interface *mb);
void error(char *function, char *command);

void ItemDtype(DIY_Datatype *type);

//--------------------------------------------------------------------------

int main( int argc, char *argv[] ) {

  int nblocks; // my local number of blocks (1 block = 1 hex for now)
  int num_threads = 4; // number of threads DIY can use
  ErrorCode rval; // moab return value
  int rank;
  
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  Interface *mb = new Core();
  const char* options = ";;PARALLEL=READ_PART;PARALLEL_RESOLVE_SHARED_ENTS;"
    "PARTITION=MATERIAL_SET;PARTITION_DISTRIBUTE";
  EntityHandle file_set;
  rval = mb->create_meshset(MESHSET_SET, file_set);


  if (rval != MB_SUCCESS)
    error((char *)"main()", (char *)"mb->create_meshset");
  rval = mb->load_file(argv[1], &file_set, options);
  if (rval != MB_SUCCESS)
    error((char *)"main()", (char *)"mb->load_file");

  Range blocks;
  rval = mb->get_entities_by_dimension(0, 3, blocks);
  if (rval != MB_SUCCESS)
    error((char *)"main()", (char *)"mb->get_entities_by_dimension");
  nblocks = blocks.size();

  // debug: report the number of 3d entities
  fprintf(stderr, "Number of blocks = %d\n", nblocks);

  // initialize DIY after initializing MPI
  DIY_Init(3, NULL, num_threads, MPI_COMM_WORLD);

  // create a parallel comm for the moab instance
  ParallelComm *pcomm = ParallelComm::get_pcomm(mb, 0);

  // decompose the domain
  Decompose(blocks, pcomm, mb);  

  DIY_Finalize();
  MPI_Finalize();

  fflush(stderr);
  if (rank == 0)
    fprintf(stderr, "\n---Completed successfully---\n");

  return true;

}
//--------------------------------------------------------------------------
//
// creates decomposition data
//
// blocks: moab range of local blocks
// pcomm: moab parallel communicator
// mb: moab interface
//
void Decompose(Range blocks, ParallelComm *pcomm, Interface *mb) {

  int nblocks = blocks.size();
  vector<ri_t> *rem_data = new vector<ri_t>[nblocks]; // remote data
  int *gids = new int[nblocks]; // gids of my local blocks
  bb_t *bounds = new bb_t[nblocks]; // extents of my local blocks
  vector<int> *vids = new vector<int>[nblocks]; // local vertex ids
  int did; // domain id

  // neighbors
  gb_t **neighbors = new gb_t*[nblocks];
  int *num_neighbors = new int[nblocks];;
  for (int i = 0; i < nblocks; i++) {
    // todo: don't understand why MAX_NEIGHBORS does not work
    neighbors[i] = new gb_t[MAX_NUM_NEIGHBORS];
    num_neighbors[i] = 0;
  }

  DecomposeBlocks(gids, bounds, rem_data, vids, neighbors, num_neighbors, 
		  blocks, pcomm, mb);

  // repackage remote data for C-style DIY API
  ri_t **rem_data_array = new ri_t*[nblocks];
  int *num_rem_data = new int[nblocks];
  int **vids_array = new int*[nblocks];
  int *num_vids = new int[nblocks];
  for (int i = 0; i < nblocks; i++) {  
    rem_data_array[i] = new ri_t[rem_data[i].size()];
    num_rem_data[i] = rem_data[i].size();
    vids_array[i] = new int[vids[i].size()];
    num_vids[i] = vids[i].size();
    for (int j = 0; j < num_rem_data[i]; j++) {
      rem_data_array[i][j].vid = rem_data[i][j].vid;
      rem_data_array[i][j].proc = rem_data[i][j].proc;
    }
    for (int j = 0; j < num_vids[i]; j++)
      vids_array[i][j] = vids[i][j];
  }

  // decompose the domain according to the data gathered from moab
  did = DIY_Decomposed(nblocks, gids, bounds, rem_data_array, num_rem_data, 
		 vids_array, num_vids, neighbors, num_neighbors, 0);

  // init ghost cell exchange
  int done = 0;
  gb_send **blocks_ghostcells = new gb_send*[nblocks]; // store all ghost cell
                                                       // info for each block
  int *num_cells = new int[nblocks];
  int *num_sent = new int[nblocks];

  // initialize each block with its own cell(s)
  // currently, 1 block has 1 cell at the begining
  gb_send current_gb;                              
  current_gb.proc=pcomm->rank();
  for(int lid = 0; lid < nblocks; lid++) {
    current_gb.gid=gids[lid];
    current_gb.bb=bounds[lid];
    blocks_ghostcells[lid] = new gb_send[MAX_NUM_CELLS];
    blocks_ghostcells[lid][0]=current_gb;
    num_cells[lid]=1;
    num_sent[lid]=0;
  }

  // ghost cell exchange
  while (!done) { 

    // for each block
    for(int lid = 0; lid < nblocks; lid++) {

      // for each (not sent) cell/ghost-cell in the block 
      for (int cid=num_sent[lid]; cid < num_cells[lid]; cid++) {

	int num_x = 0;
	int *gids_x = new int[MAX_NUM_NEIGHBORS];

	// check potential intersection
	DIY_Bounds_intersect_neighbors(did, lid, 
				       blocks_ghostcells[lid][cid].bb, 
				       QUERY_EXTEND_T, &num_x, gids_x);
			
	// post cell to neighbors
	gb_send to_send;
	to_send=blocks_ghostcells[lid][cid];
	DIY_Enqueue_item_gids(did, lid, (void *)&(to_send), NULL, 
			      sizeof(gb_send), gids_x, num_x, NULL);

	delete[] gids_x;

      }

      num_sent[lid]=num_cells[lid];

    }
	
    // receiving cells
    void ***items = new void**[nblocks]; // received items
    int *num_items = new int[nblocks]; // number of received items in each block
    int total_num_items;

    // exchange neighbors
    DIY_Exchange_neighbors(did, items, num_items, 1.0, &ItemDtype);

    total_num_items = 0;

    // store received cells
    for (int lid = 0; lid < nblocks; lid++) {

      if (num_items[lid]) {

	set<int> cell_gids;
	pair<set<int>::iterator, bool> new_cell;
	cell_gids.clear();

	// find current ghost cells' gids
	for (int k = 0; k< num_cells[lid]; k++)
	  cell_gids.insert(blocks_ghostcells[lid][k].gid);
		 
	int num_accepted = 0;

	// for all received cells, if this block doesn't have it, 
	// then new cell found
	for (int j = 0; j < num_items[lid]; j++) { 
	  new_cell = cell_gids.insert((*((gb_send *)items[lid][j])).gid);
	  if (new_cell.second) { // new cell found
	    blocks_ghostcells[lid][num_cells[lid]] = *((gb_send *)items[lid][j]);
	    num_cells[lid]++;
	    num_accepted++;
	  } // new cell found
	}

	// debug
// 	fprintf(stderr, "%d ghost cells received by block %d , "
// 		"%d are accepted. \n", num_items[lid], gids[lid], num_accepted);

	total_num_items += num_accepted;
	cell_gids.clear();

      }

    }

    done = DIY_Check_done_all(!total_num_items); 

    DIY_Flush_neighbors(did, items, num_items, &ItemDtype);
    delete[] num_items;

  } // ghost cell exchange

  // cleanup
  for (int i = 0; i < nblocks; i++) {
    delete[] blocks_ghostcells[i];
    delete[] neighbors[i];
    delete[] rem_data_array[i];
    delete[] vids_array[i];
  }
  delete[] num_sent;
  delete[] num_cells;
  delete[] gids;
  delete[] vids;
  delete[] vids_array;
  delete[] num_vids;
  delete[] bounds;
  delete[] neighbors;
  delete[] num_neighbors;
  delete[] rem_data;
  delete[] rem_data_array;
  delete[] num_rem_data;

}
//--------------------------------------------------------------------------
//
// parses local blocks and finds decomposition
//
// gids: global ids of local blocks (ouptut)
// bounds: block bounds (output)
// rem_data: remote data used for neighbor discovery (output)
// vids: local vertex ids for each block (output)
// neighbors: neighbors for each local block (output)
// num_neighbors: number of neighbors for each local block (output)
// blocks: moab range of local blocks
// pcomm: moab parallel communicator
// mb: moab interface
//
void DecomposeBlocks(int *gids, bb_t *bounds, vector<ri_t> *rem_data, 
		     vector<int> *vids, gb_t **neighbors, int *num_neighbors, 
		     Range blocks, ParallelComm *pcomm, Interface *mb) {

  int lid = 0; // current local block
  ErrorCode rval;

  // get the tag for the block gid
  Tag gid_tag;
  rval = mb->tag_get_handle("GLOBAL_ID", 1, MB_TYPE_INTEGER, gid_tag);
  if (rval != MB_SUCCESS)
    error((char *)"DecomposeBlocks()", (char *)"mb->tag_get_handle");
  int *moab_gids; // gids from GLOBAL_ID tags (allocated by moab)
  int n_gids; // number of gids returned from moab
  rval = mb->tag_iterate(gid_tag, blocks.begin(), blocks.end(), n_gids, 
			 (void *&)moab_gids);
  if (rval != MB_SUCCESS)
    error((char *)"DecomposeBlocks()", (char *)"mb->tag_iterate");
  assert(n_gids == (int)blocks.size()); // sanity

  // for all blocks
  for (Range::iterator hexes_it = blocks.begin(); 
       hexes_it != blocks.end(); hexes_it++) {

      // get vertices in the hex
      Range adj_verts; // vertices in the current hex
      rval = mb->get_adjacencies(&*hexes_it, 1, 0, true, adj_verts);
      if (rval != MB_SUCCESS)
	error((char *)"DecomposeBlocks()", 
	      (char *)"mb->get_adjacencies for verts");

      // get extents of the hex
      BlockBounds(&(bounds[lid]), adj_verts, mb);

      // save gid
      gids[lid] = moab_gids[lid];

      // find hexes adjacent to the current hex
      num_neighbors[lid] = 
	BlockNeighbors(neighbors[lid], rem_data[lid], vids[lid], *hexes_it, 
		       pcomm, mb);

      lid++;

  } // for all blocks

  // cleanup
  mb->tag_delete(gid_tag);

}
//--------------------------------------------------------------------------
//
// get block bounds given the vertices for a current block
//
// bounds: bounds for current block (output)
// verts; current block (hex for now) vertices
// pcomm: moab parallel communicator
//
void BlockBounds(bb_t *bounds, Range verts, Interface *mb) {

  ErrorCode rval;
  double *x, *y, *z; // vertex coords

  x = new double[verts.size()];
  y = new double[verts.size()];
  z = new double[verts.size()];
  rval = mb->get_coords(verts, x, y, z);
  assert(rval == MB_SUCCESS);

  double min[3] = {0.0, 0.0, 0.0};
  double max[3] = {0.0, 0.0, 0.0};;
  for (int i = 0; i < (int)verts.size(); i++) {
    if (i == 0) {
      min[0] = max[0] = x[i];
      min[1] = max[1] = y[i];
      min[2] = max[2] = z[i];
    }
    else {
      min[0] = (x[i] < min[0] ? x[i] : min[0]);
      min[1] = (y[i] < min[1] ? y[i] : min[1]);
      min[2] = (z[i] < min[2] ? z[i] : min[2]);
      max[0] = (x[i] > max[0] ? x[i] : max[0]);
      max[1] = (y[i] > max[1] ? y[i] : max[1]);
      max[2] = (z[i] > max[2] ? z[i] : max[2]);
    }
  }

  // save the block info
  bounds->min[0] = min[0];
  bounds->min[1] = min[1];
  bounds->min[2] = min[2];
  bounds->max[0] = max[0];
  bounds->max[1] = max[1];
  bounds->max[2] = max[2];

}
//--------------------------------------------------------------------------
//
// find blocks (hexes) neighboring the current block (hex)
//
// neighbors: array of neighbor blocks (output)
// rem_data: remote data used to discover neighboring blocks (output)
// vids: local vertex ids for the current block (output)
// hex; current block (hex for now)
// pcomm: moab parallel communicator
// mb: moab interface
//
// returns: number of neighbors found
//
int BlockNeighbors(gb_t *neighbors, vector<ri_t> &rem_data, 
		   vector<int> &vids, EntityHandle hex, ParallelComm *pcomm, 
		   Interface *mb) {

  ErrorCode rval; // moab return value
  int num_neighbors = 0; // number of neighbors found so far
  Range verts; // vertices in the input block (hex)
  set<int> block_gids; // unique block gids
  pair<set<int>::iterator, bool> new_block; // whether a new block was found

  // get vertices in the hex
  rval = mb->get_adjacencies(&hex, 1, 0, true, verts);
  if (rval != MB_SUCCESS)
    error((char *)"BlockNeighbors()", (char *)"mb->get_adjacencies for verts");
  assert(verts.size() == 8); // sanity

  // step 1: look for local neighbors on my process

  // for all the vertices
  for (Range::iterator verts_it = verts.begin(); 
       verts_it != verts.end(); verts_it++) {

    Range adj_hexes; // hexes adjacent to the vertices
    rval = mb->get_adjacencies(&*verts_it, 1, 3, false, adj_hexes);
    if (rval != MB_SUCCESS)
      error((char *)"BlockNeighbors()", 
	    (char *)"mb->get_adjacencies for adj_hexes");

    // get gids
    Tag gid_tag;
    rval = mb->tag_get_handle("GLOBAL_ID", 1, MB_TYPE_INTEGER, gid_tag);
    if (rval != MB_SUCCESS)
      error((char *)"BlockNeighbors()", (char *)"mb->tag_get_handle");
    int moab_gids[MAX_NUM_NEIGHBORS];
    rval = mb->tag_get_data(gid_tag, adj_hexes, (void *)moab_gids);
    if (rval != MB_SUCCESS)
      error((char *)"BlockNeighbors()", (char *)"mb->tag_get_data");

    // for all hexes adjacent to a vertex
    int n= 0;
    for (Range::iterator hexes_it = adj_hexes.begin(); 
	 hexes_it != adj_hexes.end(); hexes_it++) {

      new_block = block_gids.insert(moab_gids[n]);

      // new block found
      if (new_block.second) { 
	neighbors[num_neighbors].gid = moab_gids[n];
	neighbors[num_neighbors].proc = pcomm->rank();
	num_neighbors++;
      } // new block found

      n++;

    } // for all hexes adjacent to a vertex

  } // for all vertices

  // step 2: look for remote data about neighbors on other processes
  RemoteData(rem_data, vids, verts, pcomm, mb);

  return num_neighbors;

}
//--------------------------------------------------------------------------
//
// find remote data about blocks neighboring the current block (hex)
//
// rem_data: remote data for aligning neighboring blocks (output)
// vids: local vertex ids for current block (output)
// verts; vertices of current block (hex for now)
// pcomm: moab parallel communicator
// mb: moab interface
//
// returns: updated total number of neighbors found
// side effects: appends to neighbors array
//
void RemoteData(vector<ri_t> &rem_data, vector<int> &vids, 
		Range verts, ParallelComm *pcomm, Interface *mb) {

  ErrorCode rval; // moab return value

  // get the subset of the verts that lie on the interface
  Range interface_verts;
  set<int> procs;
  rval = pcomm->get_pstatus_entities(0, PSTATUS_INTERFACE, interface_verts);
  if (rval != MB_SUCCESS)
    error((char *)"BlockNeighbors()", (char *)"pcomm->get_pstatus_entities");
  Range intersect_verts = intersect(verts, interface_verts);

  // get sharing data for the intersect verts
  // all the procs that share the entity and the handles by which that
  // entity is identified on those procs. The owner proc is listed first on
  // both lists. caveat: When there are only two procs, myself and one more,
  // only the other one is listed, not myself. When there are > 2 procs, all
  // are listed including myself
  int ps[MAX_SHARING_PROCS]; // all sharing procs
  EntityHandle hs[MAX_SHARING_PROCS]; // handle of the entity on the other procs
  int num_ps = 0; // number of elements in ps, returned by moab
  unsigned char pstat; // pstatus returned by moab
  for (Range::iterator verts_it = intersect_verts.begin(); 
       verts_it != intersect_verts.end(); verts_it++) {

    // local vertex id
    vids.push_back(mb->id_from_handle(*verts_it));

    rval = pcomm->get_sharing_data(*verts_it, ps, hs, pstat, num_ps);
    if (rval != MB_SUCCESS)
      error((char *)"BlockNeighbors()", (char *)"pcomm->get_sharing_data");

    for (int i = 0; i < num_ps; i++) {
      if (ps[i] == (int)pcomm->rank()) // skip myself
	continue;
      ri_t rd; // remote data
      rd.vid = mb->id_from_handle(hs[i]);
      rd.proc = ps[i];
      rem_data.push_back(rd);
    }

  }

}
//--------------------------------------------------------------------------
//
// error handler
//
void error(char *function, char *command) {

  fprintf(stderr, "Error: function = %s command = %s\n", function, command);
  exit(0);

}
//--------------------------------------------------------------------------
//
// makes a datatype for sending and receiving one item
//
void ItemDtype(DIY_Datatype *type) {

  DIY_Datatype btype;
  struct map_block_t bmap[] = {
    { MPI_FLOAT, OFST, DIY_MAX_DIM, offsetof(struct bb_t, min) },
    { MPI_FLOAT, OFST, DIY_MAX_DIM, offsetof(struct bb_t, max) },
  };
  DIY_Create_struct_datatype(0, 2, bmap, &btype);

  struct map_block_t map[] = {
    { MPI_INT, OFST, 1,            offsetof(struct gb_send, gid)      },
    { MPI_INT, OFST, 1,            offsetof(struct gb_send, proc)     },
    { btype,   OFST, 1,            offsetof(struct gb_send, bb)       },
  };
  DIY_Create_struct_datatype(0, 3, map, type); 

  DIY_Destroy_datatype(&btype);

}
//--------------------------------------------------------------------------
