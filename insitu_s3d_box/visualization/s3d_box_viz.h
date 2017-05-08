#ifndef S3D_BOX_RENDER_H
#define S3D_BOX_RENDER_H

#include <stdint.h>

#ifndef S3D_BOX_INTRANSIT

#include "multifab_tools.h"

/**
 * s3d_box_viz_init
 *
 */
void s3d_box_viz_init(int *id, 
                      MPI_Comm *comm,
                      int *xyzpes, 
                      int *xyzid,
                      MultiFab *fab);

/**
 * s3d_box_insitu_render_tstep
 *
 */
void s3d_box_insitu_render_tstep(int *id, 
                                 MPI_Comm *mpicomm, 
                                 int time);

/**
 * s3d_box_insitu_reduction_tstep
 *
 */
void s3d_box_insitu_reduction_tstep(int *id, 
                                    MPI_Comm *mpicomm, 
                                    int time,
                                    int sample_step);


/**
 * s3d_box_viz_finalize
 *
 */
void s3d_box_viz_finalize(int *id, 
                          MPI_Comm *mpicomm);


#else

#include "dataspaces_api.h"

/**
 * s3d_box_intransit_render_reduction_tstep
 *
 */
void s3d_box_intransit_render_reduction_tstep(MPI_Comm *mpicomm,
                                              uint8_t **data_buffer,
                                              int data_num,
                                              int time);


/**
 * perform_intransit_viz
 *
 */
int perform_intransit_viz(MPI_Comm mpicomm, 
                          struct list_head *data_list);


#endif

#endif
