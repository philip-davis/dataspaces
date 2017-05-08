//---------------------------------------------------------------------------
//
// example of using DIY to perform swap-based global reduction in an
// image compositing example
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
#include "mpi.h"
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <stddef.h>
#include "diy.h"

// some globals for the sample problem
int num_pixels = 50; // number of histogram bins
int dim = 3; // number of dimensions in the problem
int tot_blocks = 12; // total number of blocks
int data_size[3] = {10, 10, 10}; // data size
int given[3] = {0, 0, 0}; // constraints on blocking (none)
int ghost[6] = {0, 0, 0, 0, 0, 0}; // -x, +x, -y, +y, -z, +z ghost
int rounds = 3; // two rounds of merging
int kvalues[3] = {2, 3, 2}; // k-way swapping, eg 2-way swap
char infile[] = "test.dat";
char outfile[] = "test.out";

//
// "Renders" an image by setting it to a constant intensity
//
void Render(int *data, unsigned char *image, int did, int lid) {

  // as a substitute for actual rendering, sets the image to a constant
  // intensity between 0-255, based on the global id of the block
  unsigned char intensity = (unsigned char)((float)DIY_Gid(did, lid) / 
					    (tot_blocks  - 1) * 255);
  memset(image, intensity, num_pixels);

}
//
// user-defined callback function for reducing an array of items
// in this example we "compose" an image by selecting maximal intensity pixels
// user should write this function so the result of the reduction is in items[0]
//
// items: pointers to input and output items, reduced in place
//   char * is used as a generic pointers to bytes, not necessarily to strings
//   items are partial size (size of current active part)
// gids: gloabl ids of items to be reduced (not used in this example, but
//  needed for example, when reduction is noncommutative and item order matters
// num_items: number of items to reduce
// num_elems: number of elements in item to reduce
//
// returns: pointer to resulting items
//
void Composite(char **items, int *gids, int num_items, int num_elems) {

  // debug
//   fprintf(stderr, "gids being composed: ");
//   for(int i = 0; i < num_items; i++)
//     fprintf(stderr, "%d ", gids[i]);
//   fprintf(stderr, "\n");

  for (int i = 1; i < num_items; i++) { // items, result in i = 0
    for (int j = 0; j < num_elems; j++) { // index in received items
      // take care to cast all accesses back to their original data type
      // in this case unsigned char
      unsigned char old_pixel = ((unsigned char **)items)[0][j];
      unsigned char new_pixel = ((unsigned char **)items)[i][j];
      ((unsigned char **)items)[0][j] = 
				(new_pixel > old_pixel ? new_pixel : old_pixel);
    }
  }

}
//
// user-defined callback function for creating a received item
//
// hdr: quantity information for allocating custom parts of the item
//  (not used in this example)
// char * is used as a generic pointers to bytes, not necessarily to strings
// num_elems: number of elements in this item, less than the number of elements
//  in a complete item
//
// side effects: allocates the item
//
// returns: pointer to the item
//
char *CreateRecvItem(int *hdr, int num_elems) {

  unsigned char *image = new unsigned char[num_elems];
  return (char *)image;

}
//
// user-defined callback function for destroying a received item
//
// item: item to be destroyed
//
void DestroyRecvItem(void *item) {

  delete[] (unsigned char *)item;

}
//
// user-defined callback function for creating an MPI datatype for sending
//   swapped item
//
// item: pointer to the item
// dtype: pointer to the datatype
// start_elem: starting element position to be sent
// num_elems: number of elements to be sent (less than number of elements
//   in the complete item
//
// side effects: commits the MPI datatype
//
// returns: base address associated with the datatype
//
void *SendType(void *item, DIY_Datatype *dtype, int start_elem,
	       int num_elems) {

  DIY_Create_vector_datatype(num_elems, 1, DIY_BYTE, dtype);

  // user's job to compute the return address correctly by scaling the
  // pointer to item by the size of an element, in this example unsigned char
  return ((unsigned char *)item + start_elem);

}
//
// user-defined callback function for creating an MPI datatype for receiving
//   swapped item
//
// item: pointer to the item
// dtype: pointer to the datatype
// num_elems: number of elements in the received datatyep (less than number
//   of elements in the complete item)
//
// side effects: commits the MPI datatype
//
void RecvType(void *item, DIY_Datatype *dtype, int num_elems) {

  DIY_Create_vector_datatype(num_elems, 1, DIY_BYTE, dtype);

}
//
// main
//
int main(int argc, char **argv) {

  int min[3], size[3]; // block extents
  int nblocks; // my local number of blocks
  int num_threads = 4; // number of threads DIY can use
  int rank; // MPI process
  int *starts, *sizes; // starting pixel and number of pixels in each
                       // resulting block after the swap is complete
  int did; // domain id

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  // examples don't do any error checking, but real apps should

  // initialize DIY after initializing MPI
  DIY_Init(dim, data_size,
	   num_threads, MPI_COMM_WORLD);

  // decompose domain
  did = DIY_Decompose(ROUND_ROBIN_ORDER, tot_blocks, &nblocks, 0, ghost, 
		      given);

  // read data, assume integer, raw format
  int *data[nblocks];
  memset(data, 0, sizeof(int*) * nblocks); // memset tells DIY to allocate
                                           // data for us
  for (int i = 0; i < nblocks; i++) {
    DIY_Block_starts_sizes(did, i, min, size);
    fprintf(stderr, "block %d min = %d %d %d "
	    "size = %d %d %d\n", i, min[0], min[1], min[2], 
	    size[0], size[1], size[2]);
    DIY_Add_data_raw(min, size, infile, DIY_INT, (void**)&(data[i]));
  }
  DIY_Read_data_all();

  // perform a local analysis, for example, generate an image for each block
  unsigned char **images; // grayscale image for each data block
  images = new unsigned char*[nblocks];
  for (int b = 0; b < nblocks; b++) { // all my blocks
    images[b] = new unsigned char[num_pixels];
    Render(data[b], images[b], did, b);
  }

  // reduce the analysis results
  starts = new int[nblocks];
  sizes = new int[nblocks];
  DIY_Swap_blocks(did, (char**)images, (int **)NULL, num_pixels, rounds, 
		  kvalues, starts, sizes, &Composite, &CreateRecvItem, 
		  &DestroyRecvItem, &SendType, &RecvType);

  // print the output images
  if (rank == 0)
    fprintf(stderr, "Finished sections of images:\n");
  for (int b = 0; b < nblocks; b++) {
    for (int i = 0; i < sizes[b]; i++)
      fprintf(stderr, "image[%d][%d] = %d\n", b, starts[b] + i, 
	      images[b][i]);
  }

  // cleanup
  for (int b = 0; b < nblocks; b++)
    free(images[b]); // malloc'd (not new'ed) by CreateRecvItem()
                     // therefore, use free instead of delete[]
  delete[] images;
  DIY_Finalize();
  MPI_Finalize();

  fflush(stderr);
  if (rank == 0)
    fprintf(stderr, "\n---Completed successfully---\n");
  return 0;

}
