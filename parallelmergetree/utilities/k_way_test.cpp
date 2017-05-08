/*
 * k_way_test.cpp
 *
 *  Created on: Feb 3, 2015
 *      Author: landge1, bremer5
 */

#include <cstdio>
#include <unistd.h>
#include <cstdlib>
#include <cassert>

#include "mpi.h"
#include "Dataflow/Controller.h"
#include "KWayMerge.h"
#include "KWayTaskMap.h"

//! The number of bits used for prefixing scatter tasks
static const uint8_t sPrefixSize = 4;

//! The number of non-prefix bits
static const uint8_t sPostfixSize = sizeof(TaskId)*8 - sPrefixSize;

//! Bit mask for scatter tasks
static const TaskId sPrefixMask = ((1 << sPrefixSize) - 1) << sPostfixSize;


// The array length that we use to pass around in the messages
int arr_length=0;

int local_compute(std::vector<DataBlock>& inputs, 
                  std::vector<DataBlock>& output, TaskId task){
  
  for (int j=0; j<inputs.size(); j++) {
    int* arr = (int*)inputs[j].buffer;
    int sum = arr[0];
    arr[0] = 0;
    for (int i=1; i<arr_length; i++) {
      arr[0] += arr[i]; 
    }

    if (sum != arr[0])
      printf("LOCAL COMPUTE : Sum Incorrect: %d %d TASK: %d FAILED\n", 
             sum, arr[0], task);
  }

  int r = rand() % 3000000;
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
 
  //printf("LOCAL COMPUTE performed by task %d\n", task);
  return 1;
}

int join(std::vector<DataBlock>& inputs, 
         std::vector<DataBlock>& output, TaskId task){
  
  for (int j=0; j<inputs.size(); j++) {
    int* arr = (int*)inputs[j].buffer;
    int sum = arr[0];
    arr[0] = 0;
    for (int i=1; i<arr_length; i++) {
      arr[0] += arr[i]; 
    }

    if (sum != arr[0])
      printf("JOIN : Sum Incorrect: %d %d TASK: %d FAILED\n", 
             sum, arr[0], task);
  }

  int r = rand() % 3000000;
  usleep(r);
  //printf("Task : %d Join: inputs: %d outputs : %d\n", 
  //        task, inputs.size(), output.size());
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

  //printf("JOIN performed by task %d\n", task);
  return 1;
}

int correction(std::vector<DataBlock>& inputs, 
               std::vector<DataBlock>& output, TaskId task){
  
  for (int j=0; j<inputs.size(); j++) {
    int* arr = (int*)inputs[j].buffer;
    int sum = arr[0];
    arr[0] = 0;
    for (int i=1; i<arr_length; i++) {
      arr[0] += arr[i]; 
    }

    if (sum != arr[0])
      printf("CORRECTION : Sum Incorrect: %d %d TASK: %d FAILED\n", 
              sum, arr[0], task & ~sPrefixMask);
  }

  int r = rand() % 3000000;
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

  //printf("CORRECTION performed by task %d\n", task & ~sPrefixMask);
  return 1;
}

int write_results(std::vector<DataBlock>& inputs, 
                  std::vector<DataBlock>& output, TaskId task){
  for (int j=0; j<inputs.size(); j++) {
    int* arr = (int*)inputs[j].buffer;
    int sum = arr[0];
    arr[0] = 0;
    for (int i=1; i<arr_length; i++) {
      arr[0] += arr[i]; 
    }

    if (sum != arr[0])
      printf("WRITING RESULT : Sum Incorrect: %d %d TASK: %d FAILED\n", 
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
  printf("WRITING RESULTS performed by %d\n", task & ~sPrefixMask);
  return 1;
}

int main(int argc, char *argv[])
{

  if (argc < 6) {
    fprintf(stderr,"Usage: %s <dx> <dy> <dz> <fanin> <int array size>\n", 
            argv[0]);
    return 0;
  }

  MPI_Init(&argc, &argv);

  // FInd out how many controllers we need
  int mpi_width;
  MPI_Comm_size(MPI_COMM_WORLD, &mpi_width);

  int rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  
  if (rank == 0)
    fprintf(stderr,"Using %d processes\n",mpi_width);

  uint32_t dim[3];
  dim[0] = atoi(argv[1]);
  dim[1] = atoi(argv[2]);
  dim[2] = atoi(argv[3]);

  uint32_t valence = atoi(argv[4]);

  KWayMerge graph(dim,valence);
  KWayTaskMap task_map(mpi_width,&graph);

  Controller master;

  FILE* output = fopen("task_graph.dot","w");
  graph.output_graph(mpi_width,&task_map,output);
  fclose(output);
  
  master.initialize(graph, &task_map, MPI_COMM_WORLD);
  master.registerCallback(1, local_compute);
  master.registerCallback(2, join);
  master.registerCallback(3, correction);
  master.registerCallback(4, write_results);
  
  std::map<TaskId,DataBlock> inputs;

  arr_length = atoi(argv[5]);
  DataBlock data;
  data.size = arr_length*sizeof(int);
  data.buffer = (char*)(new int[data.size]);

  int *arr = (int*)data.buffer;
  // Initialize the array with ints
  for (int i=0; i<arr_length; i++) 
    arr[i] = i;

  // Collect the sum in the first element
  for (int i=1; i<arr_length; i++) 
    arr[0] += arr[i];

  inputs[rank] = data;

  if (rank == 0)
    printf("Array Size: %d Sum: %d\n", arr_length, arr[0]);

  master.run(inputs);

  MPI_Finalize();
  return 0;
}
