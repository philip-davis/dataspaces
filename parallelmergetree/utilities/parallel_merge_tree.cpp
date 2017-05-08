/*
 * parallel_merge_tree.cpp
 *
 *  Created on: Feb 23, 2014
 *      Author: landge1, bremer5
 */

#include <cstdio>
#include <cfloat>
#include <unistd.h>
#include <cstdlib>
#include "mpi.h"
#include "DataFlow/Controller.h"
#include "KWayMerge.h"
#include "KWayTaskMap.h"
#include "SortedUnionFindAlgorithm.h"
#include "SortedJoinAlgorithm.h"
#include "MergeTree.h"
#include "AugmentedMergeTree.h"
#include "diy.h"

//! The number of bits used for prefixing scatter tasks
static const uint8_t sPrefixSize = 4;

//! The number of non-prefix bits
static const uint8_t sPostfixSize = sizeof(TaskId)*8 - sPrefixSize;

//! Bit mask for scatter tasks
static const TaskId sPrefixMask = ((1 << sPrefixSize) - 1) << sPostfixSize;

int arr_length = 4;

int local_compute(std::vector<DataBlock>& inputs, 
                  std::vector<DataBlock>& output, TaskId task){
  
  sorted_union_find_algorithm(inputs, output, task);

  AugmentedMergeTree t;

  t.decode(output[0]);

  t.writeToFile(task);

  // Sending dummy output to the rest of the tasks
 // DataBlock data_block;
 // data_block.size = arr_length*sizeof(int);
 // data_block.buffer = (char*)(new int[data_block.size]);

 // int *arr = (int*)data_block.buffer;
 // // Initialize the array with ints
 // for (int i=0; i<arr_length; i++) 
 //   arr[i] = i;

 // // Collect the sum in the first element
 // for (int i=1; i<arr_length; i++) 
 //   arr[0] += arr[i];

 // output[1] = data_block;

  // Deleting input data
  for (int i=0; i<inputs.size(); i++){
    delete[] (char*)inputs[i].buffer;
  }
  inputs.clear();

   
  //fprintf(stderr,"LOCAL COMPUTE performed by task %d\n", task);
  return 1;
}


int join(std::vector<DataBlock>& inputs, 
         std::vector<DataBlock>& output, TaskId task){

  
  sorted_join_algorithm(inputs, output, task);
  fprintf(stderr, "Task : %d : Done with join algorithm\n", task);

  MergeTree join_tree;

  join_tree.decode(output[0]);
  //join_tree.writeToFile(task+1000);
  
  // Sending dummy output to the rest of the tasks
  std::vector<DataBlock> data_block;
  data_block.resize(output.size());
  data_block[0].size = arr_length*sizeof(int);
  data_block[0].buffer = (char*)(new int[data_block[0].size]);

  int *arr = (int*)data_block[0].buffer;
  // Initialize the array with ints
  for (int i=0; i<arr_length; i++) 
    arr[i] = i;

  // Collect the sum in the first element
  for (int i=1; i<arr_length; i++) 
    arr[0] += arr[i];

  // Creating output for only upward tasks
  for (int i=1; i<output.size(); i++) {
    data_block[i].size = data_block[0].size;
    data_block[i].buffer = (char*)(new int[data_block[0].size]);
    memcpy(data_block[i].buffer, data_block[0].buffer, data_block[0].size);
  }
  
  // Send dummy outputs only to correction tasks
  for (int i=1; i<output.size(); i++) {
    output[i] = data_block[i];
  }

  // Deleting input data
  for (int i=0; i<inputs.size(); i++){
    delete[] (char*)inputs[i].buffer;
  }
  inputs.clear();

  return 0;
}

int local_correction(std::vector<DataBlock>& inputs, 
                  std::vector<DataBlock>& output, TaskId task){

 for (int j=0; j<inputs.size(); j++) {
    int* arr = (int*)inputs[j].buffer;
    int sum = arr[0];
    arr[0] = 0;
    for (int i=1; i<arr_length; i++) {
      arr[0] += arr[i]; 
    }

    if (sum != arr[0])
      fprintf(stderr,"CORRECTION : Sum Incorrect: %d %d TASK: %d FAILED\n", 
              sum, arr[0], task & ~sPrefixMask);
  }

  int r = rand() % 3000;
  usleep(r);

  // Creating output
  for (int i=0; i<output.size(); i++) {
    output[i].size = inputs[0].size;
    output[i].buffer = (char*)(new int[inputs[0].size]);
    memcpy(output[i].buffer, inputs[0].buffer, inputs[0].size);
  }

  // Deleting input data
  for (int i=0; i<inputs.size(); i++){
    delete[] (char*)inputs[i].buffer;
  }
  inputs.clear();

  //fprintf(stderr,"CORRECTION performed by task %d\n", task & ~sPrefixMask);
  return 1;
}

int dummy_write_results(std::vector<DataBlock>& inputs, 
                  std::vector<DataBlock>& output, TaskId task){

   for (int j=0; j<inputs.size(); j++) {
    int* arr = (int*)inputs[j].buffer;
    int sum = arr[0];
    arr[0] = 0;
    for (int i=1; i<arr_length; i++) {
      arr[0] += arr[i]; 
    }

    if (sum != arr[0])
      fprintf(stderr,"WRITING RESULT : Sum Incorrect: %d %d TASK: %d FAILED\n", 
              sum, arr[0], task & ~sPrefixMask);
  }

  int r = rand() % 3000000;
  usleep(r);

  // Deleting input data
  for (int i=0; i<inputs.size(); i++){
    delete[] (char*)inputs[i].buffer;
  }
  inputs.clear();

  assert(output.size() == 0);
  fprintf(stderr,"WRITING RESULTS performed by %d\n", task & ~sPrefixMask);
  return 1;
}


int main(int argc, char *argv[])
{

  if (argc < 9) {
    fprintf(stderr,"Usage: %s <input_data> <Xdim> <Ydim> <Zdim> \
                    <dx> <dy> <dz> <fanin> <threshold>\n", argv[0]);
    return 0;
  }

  MPI_Init(&argc, &argv);
  int rank;
  int num_processes;
  int num_threads = 1;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &num_processes);

  int dim = 3;
  int tot_blocks;
  int data_size[3];             // {x_size, y_size, z_size}
  uint32_t block_decomp[3];     // block decomposition
  int min[3], max[3], size[3];  // block extents
  int nblocks;                  // my local number of blocks
  int ghost[6] = {0, 0, 0, 0, 0, 0};
  int share_face = 1;           // share a face among the blocks

  data_size[0] = atoi(argv[2]);
  data_size[1] = atoi(argv[3]);
  data_size[2] = atoi(argv[4]);
  
  
  block_decomp[0] = atoi(argv[5]);
  block_decomp[1] = atoi(argv[6]);
  block_decomp[2] = atoi(argv[7]);
  
  int given[3] = {(int)block_decomp[0],
                  (int)block_decomp[1],
                  (int)block_decomp[2]};
  
  tot_blocks = block_decomp[0]*block_decomp[1]*block_decomp[2];

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
  
  if (sizeof(FunctionType) == 4)
    DIY_Add_data_raw(min, size, argv[1], DIY_FLOAT, (void**)&(data_diy[0]));
  if (sizeof(FunctionType) == 2)
    DIY_Add_data_raw(min, size, argv[1], DIY_SHORT, (void**)&(data_diy[0]));
  if (sizeof(FunctionType) == 8)
    DIY_Add_data_raw(min, size, argv[1], DIY_DOUBLE, (void**)&(data_diy[0]));
  
  for (int j = 0; j < 3; j++)
  max[j] = min[j] + size[j]-1;
  fprintf(stderr, "Process rank = %d block local id = %d min = [%d %d %d] "
           "max = [%d %d %d] size = [%d %d %d]\n", rank, 0, 
           min[0], min[1], min[2], max[0], max[1], max[2],
           size[0], size[1], size[2]);
  fprintf(stderr, "\n");
  
  // read all the blocks that were posted
  DIY_Read_data_all();
  MPI_Barrier(MPI_COMM_WORLD);
  
  uint32_t valence = atoi(argv[8]);
  
  FunctionType threshold;
  if (argc == 10)
    threshold = (FunctionType)atof(argv[9]);
  else 
    threshold = (FunctionType)(-1)*FLT_MAX; 

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
  FILE* output = fopen("task_graph.dot","w");
  graph.output_graph(num_processes, &task_map, output);
  fclose(output);

  // Pass input to task
  std::map<TaskId,DataBlock> inputs;
  
  DataBlock data_block;
  data_block = make_local_block(data_diy[0], low, high, 
                                (FunctionType)(-1)*FLT_MAX);
  inputs[rank] = data_block;
  
  // Register the callbacks
  master.initialize(graph, &task_map, MPI_COMM_WORLD);
  master.registerCallback(1, local_compute);
  master.registerCallback(2, join);
  master.registerCallback(3, local_correction);
  master.registerCallback(4, dummy_write_results);
  master.run(inputs);
  
  DIY_Finalize();
  MPI_Finalize();
  fprintf(stderr, "Done\n");
  return 0;
}



