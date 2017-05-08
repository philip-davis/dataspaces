/*
 * multi_task_local_compute.cpp
 *
 *  Created on: Feb 9, 2014
 *      Author: landge1, bremer5
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

//! The number of bits used for prefixing scatter tasks
static const uint8_t sPrefixSize = 4;

//! The number of non-prefix bits
static const uint8_t sPostfixSize = sizeof(TaskId)*8 - sPrefixSize;

//! Bit mask for scatter tasks
static const TaskId sPrefixMask = ((1 << sPrefixSize) - 1) << sPostfixSize;

//! TODO Find better way of cleaning data
int local_compute(std::vector<DataBlock>& inputs, 
                  std::vector<DataBlock>& output, TaskId task){
  
  sorted_union_find_algorithm(inputs, output, task);

  //MergeTree t;

  //fprintf(stderr,"LOCAL COMPUTE performed by task %d\n", task);
  //t.decode(output[0]);

  //t.writeToFile(task);

  // Deleting input data
  for (int i=0; i<inputs.size(); i++){
    delete[] (char*)inputs[i].buffer;
  }
  inputs.clear();

  return 1;
}


int join(std::vector<DataBlock>& inputs, 
         std::vector<DataBlock>& output, TaskId task){

  
  //fprintf(stderr, "Task : %d : Started with join algorithm\n", task);
  sorted_join_algorithm(inputs, output, task);
  //fprintf(stderr, "Task : %d : Done with join algorithm\n", task);

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

int local_correction(std::vector<DataBlock>& inputs, 
                     std::vector<DataBlock>& output, TaskId task){

  //if ((task & ~sPrefixMask) == 237)
  local_correction_algorithm(inputs, output, task);

  // Deleting input data
  for (int i=0; i<inputs.size(); i++){
    delete[] (char*)inputs[i].buffer;
  }
  inputs.clear();

  //fprintf(stderr,"CORRECTION performed by task %d\n", task & ~sPrefixMask);
  return 1;
}

int write_results(std::vector<DataBlock>& inputs, 
                  std::vector<DataBlock>& output, TaskId task){

  AugmentedMergeTree t;
  t.decode(inputs[0]);
  t.id(task & ~sPrefixMask);
  //t.writeToFile(task & ~sPrefixMask);
  t.computeSegmentation();
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


int main(int argc, char *argv[])
{

  //! TODO Expand usage message to define arguments
  if (argc < 9) {
    fprintf(stderr,"Usage: %s <input_data> <Xdim> <Ydim> <Zdim> \
                    <dx> <dy> <dz> <fanin> <threshold>\n", argv[0]);
    return 0;
  }

  clock_t start,finish, file_io_start, file_io_end;
  double proc_time, diy_time, file_io_time;
  start = clock();

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

  int test_block_size[3];
  
  data_size[0] = atoi(argv[2]);
  data_size[1] = atoi(argv[3]);
  data_size[2] = atoi(argv[4]);

 // if (rank == 0) {
 // // Read the input from the file
 //     char file_name[256];
 //     sprintf(file_name,"%s", argv[1]);
 //     FILE* input = fopen(file_name, "rb");
 //     if (input == NULL) {
 //       fprintf(stderr, "Input File not found!\n");
 //       return 1;
 //     }

 //     test_block_size[0] = 280;
 //     test_block_size[1] = 280;
 //     test_block_size[2] = 280;

 //     // extract block from file
 //     extractBlock(input, data_size, test_block_size);
 //     fclose(input);
 // }
  
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
  //fprintf(stderr, "Process rank = %d block local id = %d min = [%d %d %d] "
  //         "max = [%d %d %d] size = [%d %d %d]\n", rank, 0, 
  //         min[0], min[1], min[2], max[0], max[1], max[2],
  //         size[0], size[1], size[2]);
  //fprintf(stderr, "\n");
  
  // read all the blocks that were posted
  DIY_Read_data_all();
  finish = clock();
  diy_time = (double(finish)-double(start))/CLOCKS_PER_SEC;

  MPI_Barrier(MPI_COMM_WORLD);
  if (rank ==0)
    std::cerr << "Data Loaded! Time : "<< diy_time <<"\n";
  
  start = clock();
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
  ControllerMap c_map;
  
  // Register the callbacks
  master.initialize(graph, &task_map, MPI_COMM_WORLD, &c_map);
  master.registerCallback(1, local_compute);
  master.registerCallback(2, join);
  master.registerCallback(3, local_correction);
  master.registerCallback(4, write_results);
  master.run(inputs);
  
  finish = clock();
  proc_time += (double(finish)-double(start))/CLOCKS_PER_SEC;

  start = clock();
  DIY_Finalize();
  finish = clock();
  diy_time += (double(finish)-double(start))/CLOCKS_PER_SEC;
  
  double max_proc_time;
  double max_diy_time;
  double max_file_io_time;
  MPI_Reduce(&proc_time, &max_proc_time, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);
  MPI_Reduce(&diy_time, &max_diy_time, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);
  //MPI_Reduce(&file_io_time, &max_file_io_time, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);  

  if(rank == 0) {
    std::cout << "max proc time without finalize= " << max_proc_time << std::endl;
  }
  start = clock();

  MPI_Finalize();

  finish = clock();
  max_proc_time += (double(finish)-double(start))/CLOCKS_PER_SEC;
  if(rank == 0) {
    std::cout << "max proc time = " << max_proc_time << std::endl;
    std::cout << "max diy time = " << max_diy_time << std::endl;
    //std::cout << "max file i/o time = " << max_file_io_time << std::endl;
  }

  //fprintf(stderr, "Done\n");
  return 0;
}


