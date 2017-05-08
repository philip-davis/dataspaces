/***************************************************
** ViSUS Visualization Project                    **
** Copyright (c) 2010 University of Utah          **
** Scientific Computing and Imaging Institute     **
** 72 S Central Campus Drive, Room 3750           **
** Salt Lake City, UT 84112                       **
**                                                **
** For information about this project see:        **
** http://www.pascucci.org/visus/                 **
**                                                **
**      or contact: pascucci@sci.utah.edu         **
**                                                **
****************************************************/

#define _XOPEN_SOURCE 600
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <arpa/inet.h>
#include <mpi.h>
#include <stdio.h>
#include <math.h>

#define MPI 1
#define PIDX_MAX_DIMENSIONS 5
#define PIDX_MAX_DIMS 5
#define RSCOUNT 16384

#ifndef __PIDX_DATA_STRUCTS_H
#define __PIDX_DATA_STRUCTS_H

typedef float dataType;

struct PIDX_variable_struct
{
    char* var_name;
    MPI_Datatype datatype;
    int samples_per_variable;
    dataType* buffer;
    MPI_Datatype memory_layout;
    const int* global_index;
    const int* count;
};
typedef struct PIDX_variable_struct* PIDX_variable;

struct idx_file_struct
{
    int variable_count;
    PIDX_variable variable[1024];
    
    char* data_set_path;
    int bits_per_block;
    int samples_per_block;
    int blocks_per_file;
    int dimension;
    int* global_bounds;
    char bitSequence[512];
    char bitPattern[512];
    int maxh;
    
    int nfiles;
    int* existing_file;
    int* existing_block_per_file;
    //block_bitmap* bitmap;
};
typedef struct idx_file_struct* IDX_meta_data;

struct PIDX_Ndim_buffer_struct
{
      int dims;
      int create_flag;
      int lower_bounds[PIDX_MAX_DIMENSIONS];
      int upper_bounds[PIDX_MAX_DIMENSIONS];
      int regular_lower_bounds[PIDX_MAX_DIMENSIONS];
      int regular_upper_bounds[PIDX_MAX_DIMENSIONS];
      MPI_Datatype datatype;
      dataType* buffer;
      int buffer_size;
      int sample_per_variable;
      char* var_name;
      int variable_index;
      //Some more parameters
};
typedef struct PIDX_Ndim_buffer_struct PIDX_Ndim_buffer; 

struct PIDX_HZ_buffer_struct
{
    int create_flag;
    int HZ_level_from;
    int HZ_level_to;
    long long HZ_index_from;
    long long HZ_index_to;
    int num_samples;
    MPI_Datatype datatype;
    dataType** buffer;
    int sample_per_variable;
    char* var_name;
    int variable_index;
    
    int chunk_type;
    int regular_lbounds[PIDX_MAX_DIMENSIONS];
    int regular_ubounds[PIDX_MAX_DIMENSIONS];
    int** n_samples ;
    int **allign_offset;
    int **allign_count;
    long long *allign_start_hz;
    long long *allign_end_hz;
    
    //some more parameters
};
typedef struct PIDX_HZ_buffer_struct PIDX_HZ_buffer;

struct PIDX_HZ_Agg_buffer_struct
{
    //storing info for all variables
    int variable_count;
    MPI_Datatype datatype[1024];
    char *variable_name[1024];
    int sample_per_variable[1024];
    int variable_index[1024];
    
    //Aggregation Buffer info
    dataType* buffer;
    int buffer_size;
    long long starting_HZ;
    long long ending_HZ;
    
    //aggregator general info
    int file_number;
    int var_number;
    int sample_number;
    int agg_factor;
    int file_factor;
    int variable_factor;
    float float_used_agg_slot;
};
typedef struct PIDX_HZ_Agg_buffer_struct PIDX_HZ_Agg_buffer;




#endif /* __PIDX_DATA_STRUCTS_H */

