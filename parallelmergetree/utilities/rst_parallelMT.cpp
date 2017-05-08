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
#include "PIDX_data_structs.h"
#include "PIDX_rst.h"


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

  //AugmentedMergeTree t;
  //t.decode(output[1]);
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
  //t.writeToFile(task & ~sPrefixMask);
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


int main(int argc, char *argv[])
{

  if (argc < 10) {
    fprintf(stderr,"Usage: %s <input_data> <Xdim> <Ydim> <Zdim> \
                    <dx> <dy> <dz> <fanin> <rstInputFile> <threshold>\n", argv[0]);
    return 0;
  }

  clock_t start,finish, block_start, block_end;
  double proc_time, diy_time, rst_time, insitu_setup_time;
  double block_setup_time = 0.0;
  start = clock();

  MPI_Init(&argc, &argv);
  int rank;
  int num_processes;
  int num_threads = 1;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &num_processes);
  
  ////////////////////////////////////////////////////////////////////////////
  // Using DIY to load the data from the input file
  // /////////////////////////////////////////////////////////////////////////

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
  //num_threads = tot_blocks;

  // initialize DIY after initializing MPI
  DIY_Init(dim, data_size, num_threads, MPI_COMM_WORLD);

  // decompose domain
  DIY_Decompose(ROUND_ROBIN_ORDER, tot_blocks,
                &nblocks, share_face, ghost, given);
  
  //std::cerr << "nblocks : " << nblocks << "\n";
  // allocate pointers to data, in this example, the data type is int
  // the memset to 0 is needed to tell DIY to allocate the memory for us
  FunctionType *data_diy[nblocks];
  memset(data_diy, 0, sizeof(FunctionType*) * nblocks);
  
  // read blocks and print block bounds
  for (int i=0; i<nblocks; i++) {
  DIY_Block_starts_sizes(0, i, min, size);
  
  if (sizeof(FunctionType) == 4)
    DIY_Add_data_raw(min, size, argv[1], DIY_FLOAT, (void**)&(data_diy[i]));
  if (sizeof(FunctionType) == 2)
    DIY_Add_data_raw(min, size, argv[1], DIY_SHORT, (void**)&(data_diy[i]));
  if (sizeof(FunctionType) == 8)
    DIY_Add_data_raw(min, size, argv[1], DIY_DOUBLE, (void**)&(data_diy[i]));
  
  
  for (int j = 0; j < 3; j++)
  max[j] = min[j] + size[j]-1;
  //fprintf(stderr, "Process rank = %d block local id = %d min = [%d %d %d] "
  //         "max = [%d %d %d] size = [%d %d %d]\n", rank, 0, 
  //         min[0], min[1], min[2], max[0], max[1], max[2],
  //         size[0], size[1], size[2]);
  //fprintf(stderr, "\n");
  }
  // read all the blocks that were posted
  DIY_Read_data_all();
  finish = clock();
  diy_time = (double(finish)-double(start))/CLOCKS_PER_SEC;
  MPI_Barrier(MPI_COMM_WORLD);
  
  ////////////////////////////////////////////////////////////////////////////
  // Data Loaded by DIY. Each process now has its own block                 //
  ////////////////////////////////////////////////////////////////////////////
  
  ////////////////////////////////////////////////////////////////////////////
  // Restructuring the data using PIDX based on input configuration         //
  ////////////////////////////////////////////////////////////////////////////
  
  char* rst_input_file;
  rst_input_file = argv[9];

  int set_expanded_box_dimension[5];

  // Read the restructuring configuration file
  if(rank == 0)
  {
    FILE* fp;
    fp = fopen(rst_input_file, "r");
    if (!fp) std::cout << "Unable to read rstInputFile!!\n";

    fscanf(fp, "%d %d %d", set_expanded_box_dimension, 
                           set_expanded_box_dimension+1, 
                           set_expanded_box_dimension+2);
    fclose(fp);
    //printf("READ:  %d %d %d\n", set_expanded_box_dimension[0], 
    //                            set_expanded_box_dimension[1], 
    //                            set_expanded_box_dimension[2]);
  }
  
  set_expanded_box_dimension[3] = 1;
  set_expanded_box_dimension[4] = 1;
    
  MPI_Bcast(set_expanded_box_dimension, 5, MPI_INT, 0, MPI_COMM_WORLD);

  start = clock();
  int extents[5];
  extents[0] = data_size[0];
  extents[1] = data_size[1];
  extents[2] = data_size[2];
  extents[3] = 1;
  extents[4] = 1;

  int per_process_data_dim[5];
  //per_process_data_dim[0] = data_size[0] / block_decomp[0];
  //per_process_data_dim[1] = data_size[1] / block_decomp[1];
  //per_process_data_dim[2] = data_size[2] / block_decomp[2];
  per_process_data_dim[0] = size[0]; 
  per_process_data_dim[1] = size[1]; 
  per_process_data_dim[2] = size[2];
  per_process_data_dim[3] = 1;
  per_process_data_dim[4] = 1;

  //std::cerr << " Per proc data dim: " << per_process_data_dim[0] 
  //          << " : " << per_process_data_dim[1] 
  //          << " : " << per_process_data_dim[2] 
  //          << "\n";

  int offset_local[5];
  offset_local[0] = min[0];
  offset_local[1] = min[1];
  offset_local[2] = min[2];
  offset_local[3] = 0;
  offset_local[4] = 0;
  
  //std::cout << "Reading of rstInputFile Done... initializing restructuring.." 
  //          << data_size[0] << data_size[1] << data_size[2] 
  //          << block_decomp[0] << block_decomp[1] << block_decomp[2] << "\n";

  //if (rank == 0)
    //std::cout << data_diy[0][0] << "\t" << data_diy[0][1] << "\n";

  rstData *rst_data;
  rst_data = PIDX_restructure_data(MPI_COMM_WORLD, num_processes, rank, extents,
                                   set_expanded_box_dimension,
                                   per_process_data_dim, offset_local, data_diy[0]);

  finish = clock();
  rst_time = (double(finish)-double(start))/CLOCKS_PER_SEC;

  ////////////////////////////////////////////////////////////////////////////
  // Setting up analysis ranks and new communicator
  // /////////////////////////////////////////////////////////////////////////

  //start = clock();
  //int insitu_ranks[num_processes];
  //MPI_Group insitu_group;
  //MPI_Group comm_world_group;
  //MPI_Comm insitu_comm;

  //// Read restructuring output log to determine which ranks have data
  //if (rank == 0) {
  //  FILE *fp = fopen("output", "r");
  //  if (!fp) std::cout << "Unable to read rstInputFile!!\n";
  //  uint32_t var, rank_rst, off0, off1, off2;
  //  uint32_t count0, count1, count2;
  //  uint32_t rank_rst_status = 0;

  //  for (int i=0; i<num_processes; i++) { 
  //    //std::cout << "hey rank: " << i << "\n";
  //    fscanf(fp, "%d %d %d %d %d %d %d %d %d", &var, &rank_rst, 
  //           &rank_rst_status, &off0, &off1, &off2, &count0, &count1, &count2); 
  //    if (rank_rst_status == 1) {
  //      insitu_ranks[insitu_rank_count+1] = rank_rst;
  //      insitu_rank_count++;
  //    }
  //  }

  //  fclose(fp);

  //  // We store the count of ranks in the first location of the array
  //  insitu_ranks[0] = insitu_rank_count;
  //}

  //// Rank 0 broadcasts the array containing ranks of MPI processes holding
  //// restructured data
  //MPI_Bcast((void*)insitu_ranks, num_processes, MPI_INT, 0, 
  //          MPI_COMM_WORLD);

  //// get the number of ranks with restructured data
  //insitu_rank_count = insitu_ranks[0];

  //MPI_Comm_group(MPI_COMM_WORLD, &comm_world_group);
  //MPI_Group_incl(comm_world_group, insitu_rank_count, insitu_ranks+1, 
  //               &insitu_group);

  ////int group_size;
  ////MPI_Group_size(insitu_group, &group_size);
  ////std::cout << "Group Size:: " << group_size << "\n";

  //MPI_Comm_create(MPI_COMM_WORLD, insitu_group, &insitu_comm);
  //
  //finish = clock();
  //insitu_setup_time = (double(finish)-double(start))/CLOCKS_PER_SEC;

  ////////////////////////////////////////////////////////////////////////////
  // Analysis code begins here
  // /////////////////////////////////////////////////////////////////////////
  
  start = clock();
  int insitu_rank_count=0;

  //if (new_rank != MPI_UNDEFINED) {
  if (rst_data->hasData == 1) {

    int new_rank;
    MPI_Comm_rank(rst_data->comm, &new_rank);
    MPI_Comm_size(rst_data->comm, &insitu_rank_count);
    //std::cout << "Rank :: " << rank << " New: " << new_rank << "\n";
    block_start = clock();
    uint32_t valence = atoi(argv[8]);
    uint32_t restructured_block_decomp[3];
    
    FunctionType threshold;
    if (argc == 11)
      threshold = (FunctionType)atof(argv[10]);
    else 
      threshold = (FunctionType)(-1)*FLT_MAX; 

    restructured_block_decomp[0] = data_size[0]/set_expanded_box_dimension[0];
    restructured_block_decomp[1] = data_size[1]/set_expanded_box_dimension[1];
    restructured_block_decomp[2] = data_size[2]/set_expanded_box_dimension[2];

    
    Controller master;

    // Global data dimensions
    GlobalIndexType data_dim[3];
    data_dim[0] = data_size[0]; 
    data_dim[1] = data_size[1];
    data_dim[2] = data_size[2];
    MergeTree::setDimension(data_dim);

    // Local block dimension including ghost
    data_dim[0] = rst_data->dataDim[0]; 
    data_dim[1] = rst_data->dataDim[1]; 
    data_dim[2] = rst_data->dataDim[2]; 

    GlobalIndexType low[3], high[3];
    low[0] = rst_data->dataOffset[0];
    low[1] = rst_data->dataOffset[1];
    low[2] = rst_data->dataOffset[2];

    high[0] = rst_data->dataOffset[0] + rst_data->dataDim[0] - 1 ;
    high[1] = rst_data->dataOffset[1] + rst_data->dataDim[1] - 1 ;
    high[2] = rst_data->dataOffset[2] + rst_data->dataDim[2] - 1 ;
    
    //fprintf(stderr, "Process rank = %d block local id = %d min = [%d %d %d] "
    //       "max = [%d %d %d] size = [%d %d %d]\n\n", new_rank, 0, 
    //       low[0], low[1], low[2], high[0], high[1], high[2],
    //       data_dim[0], data_dim[1], data_dim[2]);
    
    DataBlock data_block;
    data_block = make_local_block(rst_data->data, low, high, 
                                  (FunctionType)threshold);
    block_end = clock();
    block_setup_time = (double(finish)-double(start))/CLOCKS_PER_SEC;
    //char block_file[32] = "rst_data_file";
    //sprintf(block_file, "%s_%d.raw", block_file, new_rank);
    //FILE *fp = fopen(block_file, "wb");

    //int file_size = data_dim[0]*data_dim[1]*data_dim[2];
    //fwrite(rst_data->data, sizeof(float), file_size, fp);
    //fclose(fp);

    // Define the task graph, task map
    KWayMerge graph(restructured_block_decomp, valence);
    KWayTaskMap task_map(insitu_rank_count, &graph);

    // Output the task graph
    //FILE* output = fopen("task_graph.dot","w");
    //graph.output_graph(insitu_rank_count, &task_map, output);
    //fclose(output);

    // Pass input to task
    std::map<TaskId,DataBlock> inputs;

    inputs[new_rank] = data_block;
    
    // Register the callbacks
    master.initialize(graph, &task_map, rst_data->comm);
    master.registerCallback(1, local_compute);
    master.registerCallback(2, join);
    master.registerCallback(3, local_correction);
    master.registerCallback(4, write_results);
    master.run(inputs);
  }
  
  finish = clock();
  proc_time = (double(finish)-double(start))/CLOCKS_PER_SEC;

  start = clock();
  DIY_Finalize();
  finish = clock();
  diy_time += (double(finish)-double(start))/CLOCKS_PER_SEC;
  
  double max_proc_time;
  double max_diy_time;
  double max_rst_time;
  double max_insitu_setup_time=0;
  double max_block_setup_time;
  MPI_Reduce(&proc_time, &max_proc_time, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);
  MPI_Reduce(&diy_time, &max_diy_time, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);
  MPI_Reduce(&rst_time, &max_rst_time, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);  
  //MPI_Reduce(&insitu_setup_time, &max_insitu_setup_time, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);  
  MPI_Reduce(&block_setup_time, &max_block_setup_time, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);  

  if(rank == 0) {
    std::cout << "max diy time = " << max_diy_time << std::endl;
    std::cout << "max restructuring time = " << max_rst_time << std::endl;
    //std::cout << "max insitu setup time = " << max_insitu_setup_time << std::endl;
    std::cout << "max proc time without finalize= " << max_proc_time << std::endl;
    std::cout << "max insitu proc time without finalize= " << max_proc_time+max_insitu_setup_time << std::endl;
    std::cout << "max insitu with restructuring without finalize = " << max_rst_time+max_proc_time+max_insitu_setup_time << std::endl;
  }
  start = clock();

  MPI_Finalize();

  finish = clock();
  max_proc_time += (double(finish)-double(start))/CLOCKS_PER_SEC;
  if(rank == 0) {
    std::cout << "After finalize:\n";
    std::cout << "max proc time = " << max_proc_time << std::endl;
    std::cout << "max insitu proc time = " << max_proc_time+max_insitu_setup_time << std::endl;
    std::cout << "max insitu with restructuring time = " << max_rst_time+max_proc_time+max_insitu_setup_time << std::endl;
  }
  
  return 0;
}


