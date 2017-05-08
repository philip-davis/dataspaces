/*
 * main.cpp
 *
 *  Created on: Dec 14, 2014
 *      Author: bremer5
 */

#include <cstdio>
#include <unistd.h>
#include <cstring>
#include <iostream>
#include <cstdlib>

#include "mpi.h"
#include "Broadcast.h"
#include "ModuloMap.h"
#include "Controller.h"

uint32_t gCount = 0;
int arr_length = 0;

int print_message(std::vector<DataBlock>& inputs, std::vector<DataBlock>& output, TaskId task)
{
  //char* str = (char*)inputs[0].buffer;
  int* arr = (int*)inputs[0].buffer;
  int sum = arr[0];
  arr[0] = 0;
  for (int i=1; i<arr_length; i++) {
    arr[0] += arr[i]; 
  }

  if (sum != arr[0])
    printf("Sum Incorrect: %d %d TASK: %d FAILED\n", sum, arr[0], task);
  int r = rand() % 3000000;
  usleep(r);

  //printf("Got message \"%s\"  %d\n",str,task);

  return 1;
}


int main(int argc, char* argv[])
{
  if (argc < 4) {
    fprintf(stderr,"Usage: %s <nr-of-leafs> <fanout> <size of int array> \n", argv[0]);
    return 0;
  }

  MPI_Init(&argc, &argv);

  // FInd out how many controllers we need
  int mpi_width;
  MPI_Comm_size(MPI_COMM_WORLD, &mpi_width);

  int rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  if (rank == 0)
    fprintf(stderr,"My rank %d\n",rank);


  uint32_t leafs = atoi(argv[1]);
  uint32_t valence = atoi(argv[2]);

  Broadcast graph(leafs,valence);
  //std::cout << "graph size " << graph.size() << "\n";
  ModuloMap task_map(mpi_width,graph.size());

  Controller master;

  FILE* output = fopen("task_graph.dot","w");
  graph.output_graph(mpi_width,&task_map,output);
  fclose(output);

  master.initialize(graph,&task_map);
  master.registerCallback(1,print_message);

  
  std::map<TaskId,DataBlock> inputs;

  arr_length = atoi(argv[3]);
  if (rank ==0 ) {
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

    //memcpy(data.buffer,argv[3],data.size);

    inputs[0] = data;
    printf("Array Size: %d Sum: %d\n", arr_length, arr[0]);
  }

  master.run(inputs);
  

  fprintf(stderr,"Done\n");
  MPI_Finalize();
  return 0;
}


