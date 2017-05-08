/*
 * comprehensive_parallel_merge_tree_test.cpp
 * 
 * This test extract random test block of random size from the given input file 
 * and computes the merge tree of the test block using
 * 1. serial code - serialCompute written to mt_9999999.dot
 * 2. parallel computation - parallelCompute written as distributed tree files
 *
 * The distributed files are merged into a single file written as full_tree.dot
 * The output trees of 1. and 2. are compared and the result is reported.
 *
 * NOTE: the files mt_9999999.dot and full_tree.dot may not be exactly identical
 * as the edges may be ordered differently in both files. Hence, a 'diff' on the
 * files may show differences but the trees may be identical.
 *
 *
 *  Created on: July 8, 2015
 *  Author: landge1
 */

#include <cstdio>
#include <cfloat>
#include <unistd.h>
#include <cstdlib>
#include <ctime>
#include "mpi.h"
#include "DataFlow/Controller.h"
#include "KWayMerge.h"
#include "KWayTaskMap.h"
#include "SortedUnionFindAlgorithm.h"
#include "SortedJoinAlgorithm.h"
#include "LocalCorrectionAlgorithm.h"
#include "MergeTree.h"
#include "AugmentedMergeTree.h"
#include "diy.h"
#include <cstdlib>

//! The number of bits used for prefixing scatter tasks
static const uint8_t sPrefixSize = 4;

//! The number of non-prefix bits
static const uint8_t sPostfixSize = sizeof(TaskId)*8 - sPrefixSize;

//! Bit mask for scatter tasks
static const TaskId sPrefixMask = ((1 << sPrefixSize) - 1) << sPostfixSize;

////////////////////////////////////////////////////////////////////////////////

int local_compute(std::vector<DataBlock>& inputs, 
                  std::vector<DataBlock>& output, TaskId task){
  
  sorted_union_find_algorithm(inputs, output, task);

  MergeTree t;

  //fprintf(stderr,"LOCAL COMPUTE performed by task %d\n", task);
  t.decode(output[0]);

  t.writeToFile(task+2000);

  // Deleting input data
  for (int i=0; i<inputs.size(); i++){
    delete[] (char*)inputs[i].buffer;
  }
  inputs.clear();

  return 1;
}
////////////////////////////////////////////////////////////////////////////////


int join(std::vector<DataBlock>& inputs, 
         std::vector<DataBlock>& output, TaskId task){

  
  //fprintf(stderr, "Task : %d : Started with join algorithm\n", task);
  sorted_join_algorithm(inputs, output, task);

  //MergeTree join_tree;

  //join_tree.decode(output[0]);
  //join_tree.writeToFile(task+1000);
  
  // Deleting input data
  for (int i=0; i<inputs.size(); i++){
    delete[] (char*)inputs[i].buffer;
  }
  inputs.clear();

  return 0;
}
////////////////////////////////////////////////////////////////////////////////

int local_correction(std::vector<DataBlock>& inputs, 
                     std::vector<DataBlock>& output, TaskId task){

  local_correction_algorithm(inputs, output, task);

  // Deleting input data
  for (int i=0; i<inputs.size(); i++){
    delete[] (char*)inputs[i].buffer;
  }
  inputs.clear();

  //fprintf(stderr,"CORRECTION performed by task %d\n", task & ~sPrefixMask);
  return 1;
}
////////////////////////////////////////////////////////////////////////////////

int write_results(std::vector<DataBlock>& inputs, 
                  std::vector<DataBlock>& output, TaskId task){

  AugmentedMergeTree t;
  t.decode(inputs[0]);
  t.writeToFile(task & ~sPrefixMask);
  t.writeToFileBinary(task & ~sPrefixMask);

  // Deleting input data
  for (int i=0; i<inputs.size(); i++){
    delete[] (char*)inputs[i].buffer;
  }
  inputs.clear();

  assert(output.size() == 0);
  //fprintf(stderr,"WRITING RESULTS performed by %d\n", task & ~sPrefixMask);
  return 1;
}

////////////////////////////////////////////////////////////////////////////////

int extractBlock(FILE *fp, int data_size[], int test_block_size[]) {
  
  std::cout << "Data size: (x y z): " << data_size[0] << " " 
                                      << data_size[1] << " " 
                                      << data_size[2] << "\n";

  std::cout << "Test block size: (x y z): " << test_block_size[0] << " " 
                                            << test_block_size[1] << " "
                                            << test_block_size[2] << "\n";

  // create a random offset into the input data
  int offset_x = 0;
  int offset_y = 0;
  int offset_z = 0;

  //srand(time(NULL));

  offset_x = rand()%(data_size[0] - test_block_size[0]);
  offset_y = rand()%(data_size[1] - test_block_size[1]);
  offset_z = rand()%(data_size[2] - test_block_size[2]);

  std::cout << "Offsets: " << offset_x << " " 
                           << offset_y << " " 
                           << offset_z << "\n";

  uint32_t linear_idx = (data_size[0]*data_size[1])*offset_z 
                        + (data_size[0]*offset_y) + offset_x;

  float *buffer = (float*)malloc(sizeof(FunctionType)*test_block_size[0]*
                                        test_block_size[1]* test_block_size[2]);
  float *ptr = buffer;
  int file_offset = 0;
  for (int k=0; k<test_block_size[2]; k++) {
    for (int j=0; j<test_block_size[1]; j++) {
      file_offset = linear_idx + j*data_size[0] + data_size[1]*data_size[0]*k;
      fseek(fp, file_offset*sizeof(FunctionType), SEEK_SET);
      fread((void*)ptr, sizeof(FunctionType), test_block_size[0], fp);
      ptr = ptr+test_block_size[0];
    }
  }

  char outputfile[64];
  sprintf(outputfile, "test_block.raw");
  fp = fopen(outputfile, "wb");
  fwrite((const void*)buffer, sizeof(FunctionType), 
          test_block_size[0]*test_block_size[1]*test_block_size[2], fp);
  fclose(fp);
  free(buffer);
  buffer = NULL;
  ptr = NULL;

  std::cout << "Block extracted from data... \n";
  return 0;
}

////////////////////////////////////////////////////////////////////////////////

int serialCompute(char* data, const int test_block_size[], FunctionType threshold,
                 std::map<GlobalIndexType, GlobalIndexType> &edge_map) {

  std::cout << "Serial Compute : Computing the full merge tree using local compute... \n";
  clock_t start,finish;
  double proc_time;

  GlobalIndexType data_dim[3];
  data_dim[0] = test_block_size[0];
  data_dim[1] = test_block_size[1];
  data_dim[2] = test_block_size[2];

  GlobalIndexType low[3], high[3];
  low[0] = 0;
  low[1] = 0;
  low[2] = 0;

  high[0] = data_dim[0]-1;
  high[1] = data_dim[1]-1;
  high[2] = data_dim[2]-1;

  MergeTree::setDimension(data_dim);

  std::vector<DataBlock> inputs, outputs(2);
  DataBlock data_block;
  data_block = make_local_block((FunctionType*)data, low, high, threshold); 
  inputs.push_back(data_block);
  //fprintf(stderr, "Data read : %ld vertices\n", file_size/(sizeof(FunctionType)));
  
  start = clock();
  sorted_union_find_algorithm(inputs, outputs, 0);
  finish = clock();

  proc_time = (double(finish)-double(start))/CLOCKS_PER_SEC;
  AugmentedMergeTree result;

  result.decode(outputs[1]);
  result.createEdgeMap(edge_map);

  result.writeToFile(9999999);
  std::cout << "Serial Compute : Full merge tree using local compute done. \n";
  std::cout << "Serial Compute : Time taken : " << proc_time << "\n"; 

  return 0;
}

////////////////////////////////////////////////////////////////////////////////


int parallelCompute(int rank, uint32_t block_decomp[], int data_size[], 
                    int valence, FunctionType threshold) {

  if (rank == 0)
    std::cout << "Parallel Compute : Begin\n";

  // Parameters used by DIY
  int dim = 3;
  int tot_blocks;
  int min[3], max[3], size[3];  // block extents
  int nblocks;                  // my local number of blocks
  int ghost[6] = {0, 0, 0, 0, 0, 0};
  int share_face = 1;           // share a face among the blocks
  int num_threads = 1;

  int given[3] = {(int)block_decomp[0],
                  (int)block_decomp[1],
                  (int)block_decomp[2]};
  
  tot_blocks = block_decomp[0]*block_decomp[1]*block_decomp[2];
  int num_processes = tot_blocks;

  clock_t start,finish;
  double proc_time = 0.0;
  double diy_time = 0.0;

  start = clock();
  // initialize DIY after initializing MPI
  DIY_Init(dim, data_size, num_threads, MPI_COMM_WORLD);

  // decompose domain
  DIY_Decompose(ROUND_ROBIN_ORDER, tot_blocks,
                &nblocks, share_face, ghost, given);
  
  // allocate pointers to data, in this example, the data type is int
  // the memset to 0 is needed to tell DIY to allocate the memory for us
  FunctionType *data_diy[nblocks];
  memset(data_diy, 0, sizeof(FunctionType*) * nblocks);
  
  // read blocks and print block bounds
  // Each rank gets only one block
  DIY_Block_starts_sizes(0, 0, min, size);
  
  char test_block_file[64] = {"test_block.raw"};
  if (sizeof(FunctionType) == 4)
    DIY_Add_data_raw(min, size, test_block_file, DIY_FLOAT, (void**)&(data_diy[0]));
  if (sizeof(FunctionType) == 2)
    DIY_Add_data_raw(min, size, test_block_file, DIY_SHORT, (void**)&(data_diy[0]));
  if (sizeof(FunctionType) == 8)
    DIY_Add_data_raw(min, size, test_block_file, DIY_DOUBLE, (void**)&(data_diy[0]));
  
  for (int j = 0; j < 3; j++)
  max[j] = min[j] + size[j]-1;
  //fprintf(stderr, "Process rank = %d block local id = %d min = [%d %d %d] "
  //         "max = [%d %d %d] size = [%d %d %d]\n", rank, 0, 
  //         min[0], min[1], min[2], max[0], max[1], max[2],
  //         size[0], size[1], size[2]);
  //fprintf(stderr, "\n");
  
  // read all the blocks that were posted
  DIY_Read_data_all();
  finish = clock();
  diy_time = (double(finish)-double(start))/CLOCKS_PER_SEC;

  
  start = clock();
  // Define the task graph, task map
  KWayMerge graph(block_decomp, valence);
  KWayTaskMap task_map(num_processes, &graph);

  Controller master;

  GlobalIndexType data_dim[3];
  data_dim[0] = data_size[0]; 
  data_dim[1] = data_size[1];
  data_dim[2] = data_size[2];
  MergeTree::setDimension(data_dim);

  data_dim[0] = size[0]; 
  data_dim[1] = size[1];
  data_dim[2] = size[2];

  GlobalIndexType low[3], high[3];
  low[0] = min[0];
  low[1] = min[1];
  low[2] = min[2];

  high[0] = max[0];
  high[1] = max[1];
  high[2] = max[2];
  
  // Output the task graph
  //FILE* output = fopen("task_graph.dot","w");
  //graph.output_graph(num_processes, &task_map, output);
  //fclose(output);

  // Pass input to task
  std::map<TaskId,DataBlock> inputs;
  
  DataBlock data_block;
  data_block = make_local_block(data_diy[0], low, high, 
                                (FunctionType)threshold);
  inputs[rank] = data_block;
  
  // Register the callbacks
  ControllerMap c_map;
  master.initialize(graph, &task_map, MPI_COMM_WORLD, &c_map);
  master.registerCallback(1, local_compute);
  master.registerCallback(2, join);
  master.registerCallback(3, local_correction);
  master.registerCallback(4, write_results);
  master.run(inputs);

  finish = clock();
  proc_time = (double(finish)-double(start))/CLOCKS_PER_SEC;

  start = clock();
 // DIY_Finalize();
  finish = clock();

  diy_time+= (double(finish)-double(start))/CLOCKS_PER_SEC;

  double max_proc_time = 0.0;
  double max_diy_time = 0.0;
  MPI_Reduce(&proc_time, &max_proc_time, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);
  MPI_Reduce(&diy_time, &max_diy_time, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);


  if (rank == 0) {
    std::cout << "Parallel Compute : DIY Time : " << max_diy_time << "\n";
    std::cout << "Parallel Compute : Procesing Time : " << max_proc_time << "\n";
  }
  
  delete[] data_diy[0];
  return 0;

}

////////////////////////////////////////////////////////////////////////////////

int checkOutput(int num_processes, 
                std::map<GlobalIndexType, GlobalIndexType> &edge_map) {
  
  // first we need to construct the final tree from the output of each rank
  char input_file_prefix[64] = {"mt_"};
  int num_files;
  
  num_files = num_processes;

  std::cout << "Constructing the full tree... \n";
  std::cout << "Reading input files starting with : " << input_file_prefix << "\n";
  std::cout << "Number of files: " << num_files << "\n";

  std::map<GlobalIndexType, GlobalIndexType> arcs_map;
  std::map<GlobalIndexType, GlobalIndexType>::iterator it;

  for (int i=0; i<num_files; i++) {
    
    char input_filename[64];
    sprintf(input_filename, "%s%d.dat", input_file_prefix, i);

    FILE *fp = fopen(input_filename, "rb");
    if (fp == NULL) {
      std::cerr << "Error in opening distributed tree file : " << input_filename << "\n";
      return 1;
    }

    int file_size;
    fseek(fp , 0 , SEEK_END);
    file_size = ftell(fp);
    rewind (fp);

    char* buffer;

    if (file_size > 0) {
      buffer = (char*)malloc(sizeof(char)*file_size);
      fread(buffer, 1, file_size, fp);
    }
    else {
      //std::cerr << "Skipping empty file: " << input_filename << "\n";
      fclose(fp);
      continue;
    }

    //std::cerr << "Reading file : " << input_filename 
    //          << " : bytes : " << file_size << "\n";

    int num_arcs = file_size/(sizeof(GlobalIndexType)*2);
    //std::cerr << "Number of arcs: " << num_arcs << "\n";

    GlobalIndexType* arcs_buffer = (GlobalIndexType*)buffer;

    for (int k=0; k<num_arcs; k++) {
      GlobalIndexType arc[2];
      
      arc[0] = *arcs_buffer;
      arcs_buffer++;
      arc[1] = *arcs_buffer;
      arcs_buffer++;

      it = arcs_map.find(arc[0]);

      if (it!=arcs_map.end()) {
        if (it->second != arc[1]) {
          std::cerr << "ERROR: Found different node to : " << it->first
                    << " existing node : " << it->second
                    << " new node : " << arc[1]
                    << " Task : " << i << "\n";
          assert(it->second == arc[1]);
        }
      }
      else {
        arcs_map[arc[0]] = arc[1];
      }
    }
    free(buffer);
    fclose(fp);
  }

  std::cout << "Number of edges in final tree: " << arcs_map.size() << "\n";

  FILE *op;
  op = fopen("full_tree.dot", "w");

  fprintf(op, "digraph G {\n");
  for (it=arcs_map.begin(); it!=arcs_map.end(); it++) {
    fprintf(op, "%llu->%llu\n", it->first, it->second);
  }
  fprintf(op, "}\n");

  fclose(op);

  if (arcs_map == edge_map)
    return 0;
  else {
    std::cerr << "Output from serial computation differs from parallel compute!!\n";
    return 1;
  }
}

////////////////////////////////////////////////////////////////////////////////

int main(int argc, char *argv[]) {
  
  if (argc < 12) {
    fprintf(stderr,"Usage: %s <input_data> <Xdim> <Ydim> <Zdim> \
                   <min_test_block_dx> <min_test_block_dy> <min_test_block_dz> \
                   <num_proc_x> <num_proc_y> <num_proc_z> <fanin> <threshold>\n",
                   argv[0]);
    return 0;
  }

  MPI_Init(&argc, &argv);
  int rank;
  int num_processes;
  int num_threads = 1;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &num_processes);


  // Parse command line args
  //int num_experiments = atoi(argv[1]); // {<num_experiments>}
  
  int data_size[3];             // {Xdim, Ydim, Zdim}
  data_size[0] = atoi(argv[2]);
  data_size[1] = atoi(argv[3]);
  data_size[2] = atoi(argv[4]);

  // {<min_test_block_dx> <min_test_block_dy> <min_test_block_dz>}
  int min_test_block_size[3];
  min_test_block_size[0] = atoi(argv[5]);
  min_test_block_size[1] = atoi(argv[6]);
  min_test_block_size[2] = atoi(argv[7]);

  
  uint32_t block_decomp[3];     // {<num_proc_x> <num_proc_y> <num_proc_z>}
  block_decomp[0] = atoi(argv[8]);
  block_decomp[1] = atoi(argv[9]);
  block_decomp[2] = atoi(argv[10]);

  uint32_t valence = atoi(argv[11]); // <fanin>
  
  FunctionType threshold;       // <threshold>
  if (argc == 13)
    threshold = (FunctionType)atof(argv[12]);
  else 
    threshold = (FunctionType)(-1)*FLT_MAX; 

  // map to store the edges of the full tree constructed by the serialCompute
  // used for comparing the output of serialCompute and parallelCompute
  std::map<GlobalIndexType, GlobalIndexType> edge_map;

  int test_block_size[3];

  //for (int i=0; i<num_experiments; i++) {
    // rank 0 extracts the test block from the data set and writes it to disk
    if (rank == 0) {
    
      //std::cout << "EXPERIMENT : " << i+1 << " of " << num_experiments << "\n"; 
      // randomly generate the test block size which is atleast as big as the min
      
      // block size entered by the user
      
      //int seed = 1437167718;//time(NULL);
      int seed = time(NULL);
      srand(seed);
      std::cout << "Seed : " << seed << "\n";


      test_block_size[0] = rand()%data_size[0];
      if (test_block_size[0] < min_test_block_size[0]) 
        test_block_size[0] = min_test_block_size[0];

       if (test_block_size[0] > 5*min_test_block_size[0]) 
        test_block_size[0] = 5*min_test_block_size[0];


      test_block_size[1] = rand()%data_size[1];
      if (test_block_size[1] < min_test_block_size[1]) 
        test_block_size[1] = min_test_block_size[1];
      if (test_block_size[1] > 5*min_test_block_size[1]) 
        test_block_size[1] = 5*min_test_block_size[1];

      test_block_size[2] = rand()%data_size[2];
      if (test_block_size[2] < min_test_block_size[2]) 
        test_block_size[2] = min_test_block_size[2];
      if (test_block_size[2] > 5*min_test_block_size[2]) 
        test_block_size[2] = 5*min_test_block_size[2];


      // Read the input from the file
      char file_name[256];
      sprintf(file_name,"%s", argv[1]);
      FILE* input = fopen(file_name, "rb");
      if (input == NULL) {
        fprintf(stderr, "Input File not found!\n");
        return 1;
      }

      // extract block from file
      extractBlock(input, data_size, test_block_size);
      fclose(input);

      // compute the merge tree for the whole block using local_compute
      FILE *fp = fopen("test_block.raw", "rb");
      if (fp == NULL) {
        fprintf(stderr, "Test block file not found!\n");
        return 1;
      }
      
      // read the contents
      long int file_size = sizeof(FunctionType)*test_block_size[0]*
                                   test_block_size[1]*test_block_size[2];
      char* buffer = (char*)malloc(file_size);
      fread(buffer, sizeof(char), file_size, fp);
      fclose(fp);

      serialCompute(buffer, test_block_size, threshold, edge_map); 
      free(buffer);
    }

    if (num_processes < block_decomp[0]*block_decomp[1]*block_decomp[2]) {
      std::cerr << "ERROR :: Number of MPI processes are insufficient!!\n";
      MPI_Finalize();
      return 1;
    }

       // broadcast selected test block size to all the processes
    MPI_Bcast(test_block_size, 3, MPI_INT, 0, MPI_COMM_WORLD);

   

    MPI_Barrier(MPI_COMM_WORLD);

    // deploy the parallel job
    parallelCompute(rank, block_decomp, test_block_size, valence, threshold);

    MPI_Barrier(MPI_COMM_WORLD);

    if (rank == 0) {
      int result = false;
      result = checkOutput(num_processes, edge_map);

      if (!result) {
        std::cout << "***** Test PASSED! *****\n\n";
        remove("test_block.raw");
      }
      else {
        std::cerr << "\nTest FAILED!\n";
        std::cerr << "Test block written to file test_block.raw\n";
        return 1;
      }
    }

    edge_map.clear();
  //} // end for

  DIY_Finalize();
  MPI_Finalize();
  return 0;
}

