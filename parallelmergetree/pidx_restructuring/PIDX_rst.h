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
#include "PIDX_data_structs.h"
#ifndef __PIDX_RST_NEW_H
#define __PIDX_RST_NEW_H

#ifdef __cplusplus
extern "C" {
#endif

struct PIDX_rst_struct;
typedef struct PIDX_rst_struct* PIDX_rst_id;

struct rstData_t {
  dataType* data;
  uint32_t dataOffset[3];
  uint32_t dataDim[3];
  MPI_Comm comm;
  int hasData;
};

typedef struct rstData_t rstData;

PIDX_rst_id PIDX_rst_init( MPI_Comm comm, int dimension, int* global_dimension, 
                           int* count, int* global_index, int set_box_dim, 
                           int* box_dim, int* num_output_buffers);

int PIDX_rst_buf_init(PIDX_Ndim_buffer* in_buf, int dims, int* lb, int* ub, 
                      dataType* buffer, int spv, MPI_Datatype datatype, 
                      char* var_name, int index);
   
/* actually do the restructuring, using pre-calculated data associated with the id */
int  PIDX_rst_restructure(PIDX_rst_id id, PIDX_Ndim_buffer* in_buf, 
                          PIDX_Ndim_buffer** out_buf_array, 
                          int num_output_buffers);

int PIDX_rst_restructure_IO(PIDX_rst_id rst_id, PIDX_Ndim_buffer* in_buf, 
                            PIDX_Ndim_buffer** out_buf_array, 
                            int num_output_buffers, int MODE);
  
/* tear down the various buffer structs. In the case of the output structs this 
 * function should also free the memory buffers as well */
int PIDX_rst_buf_destroy(PIDX_Ndim_buffer* in_buf);

/* tear down whatever was calculated for this particular combination of 
 * dimensions and bounds */ 
int PIDX_rst_finalize(PIDX_rst_id id/*, int r, int ts_count, int ts, int var*/);  

int* PIDX_rst_get_box_dimension(PIDX_rst_id id);

void PIDX_rst_print_error(char *error_message, char* file, int line);

int HELPER_rst(PIDX_Ndim_buffer** out_buf_array1, PIDX_rst_id rst_id, 
               int num_output_buffers, int spv);

rstData* PIDX_restructure_data(MPI_Comm comm, int nprocs, int rank, 
                               int extents[5],
                               int set_expanded_box_dimension[5],
                               int count_local[5],
                               int offset_local[5],
                               dataType *buff );

#ifdef __cplusplus
}
#endif


#endif /* __PIDX_RST_NEW_H */

