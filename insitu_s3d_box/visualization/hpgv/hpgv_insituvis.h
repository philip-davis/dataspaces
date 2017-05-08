/**
 * hpgv_insituvis.h
 *
 * Copyright (c) 2008 Hongfeng Yu
 *
 * Contact:
 * Hongfeng Yu
 * hfstudio@gmail.com
 * 
 * 
 * All rights reserved.  May not be used, modified, or copied 
 * without permission.
 *
 */

#ifndef HPGV_INSITUVIS_H
#define HPGV_INSITUVIS_H

#include "hpgv_block.h"

/**
 * hpgv_insituvis_init_
 *
 */
void hpgv_insituvis_init_(int myid, 
                          MPI_Comm mpicomm,
                          int renderroot,
                          int npx, int npy, int npz,
                          int mypx, int mypy, int mypz,
                          int domain_grid_size[3],
                          double *species);

/**
 * hpgv_insitu_render_tstep_
 *
 */
void hpgv_insitu_render_tstep_(int myid, 
                               MPI_Comm mpicomm, 
                               double time, 
                               quantize_t *quantize);

/**
 * hpgv_insitu_downsample_tstep_
 *
 */
void hpgv_insitu_downsample_tstep_(int myid, 
                                   MPI_Comm mpicomm, 
                                   double time,
                                   int sample_step, 
                                   uint8_t **data_reduced, 
                                   int *data_size);


/**
 * hpgv_insituvis_fini_
 *
 */
void hpgv_insituvis_fini_(int myid, 
                          MPI_Comm gcomm);




/**
 * hpgv_intransit_render_downsample
 *
 */
void
hpgv_intransit_render_downsample(MPI_Comm mpicomm,
                                 uint8_t **data_buffer, 
                                 int data_num, 
                                 quantize_t *quantize,
                                 double time);


#endif
