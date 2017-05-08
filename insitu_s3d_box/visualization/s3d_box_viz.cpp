#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>
#include <mpi.h>
#include <iostream>
#include "hpgv.h"
#include "s3d_box_viz.h"

#define DATASPACE

#ifdef DATASPACE
#include "dataspaces_api.h"
#endif

#define RENDER_ROOT 0

#define RENDER_VAR  3
//#define RENDER_VAR  4

double *theSpecies = 0;

/**
 * my_data_quantize
 *
 */
float my_data_quantize(float value, int varname)
{
    float min, max;
  
    if (3 == RENDER_VAR) {
        //min = 2.5;
        //max = 15;
        min = 1.22;
        max = 7.2;
    }

    if (4 == RENDER_VAR) {
        min = 1.22;
        max = 7.2;
    }

    //min = -5.5e-04;
    //max = 5.5e-04;
    
    if( max == min) {
        return value;
    }
    
    float v = (value - min) / (max - min);
    
    if (v < 0) { 
        v = 0;
    }
    if (v > 1) {
        v = 1;
    }
    return v;
}

#ifndef S3D_BOX_INTRANSIT

/**
 * s3d_box_viz_init
 *
 */
void s3d_box_viz_init(int *id, 
                      MPI_Comm *comm,
                      int *xyzpes, 
                      int *xyzid,
                      MultiFab *fab)
{
    MPI_Comm mpicomm = *comm;
    
    int npx = xyzpes[0];
    int npy = xyzpes[1];
    int npz = xyzpes[2];
    
    int mypx = xyzid[0];
    int mypy = xyzid[1];
    int mypz = xyzid[2];
    
    int domain_grid_size[3];
    
    int los[3], his[3];
    int  global_min[3], global_max[3];
    
    for (MFIter mfi(*(fab)); mfi.isValid(); ++mfi){ 
        const int *ll = mfi.validbox().loVect();
        const int *hh = mfi.validbox().hiVect();
        for (int i = 0; i < 3; i++) {
            los[i] = ll[i];
            his[i] = hh[i];
        }
    }
    
    MPI_Allreduce(los, (global_min), 3, MPI_INTEGER, MPI_MIN, mpicomm);
    MPI_Allreduce(his, (global_max), 3, MPI_INTEGER, MPI_MAX, mpicomm);
    
    for (int i = 0; i < 3; i++) {
        domain_grid_size[i] = global_max[i] + 1;
    }
    
    theSpecies = data_ptr(fab);
    
    int localsize 
        = domain_grid_size[0] * domain_grid_size[1] * domain_grid_size[2] /
          (npx * npy * npz);
    
    hpgv_insituvis_init_(*id,
                         *comm,
                         RENDER_ROOT,
                         npx, npy, npz,
                         mypx, mypy, mypz,
                         domain_grid_size,
                         &(theSpecies[localsize * RENDER_VAR]));
    
}

/**
 * s3d_box_insitu_render_tstep
 *
 */
void s3d_box_insitu_render_tstep(int *id, 
                                 MPI_Comm *mpicomm, 
                                 int time)                                 
{
    hpgv_insitu_render_tstep_(*id, *mpicomm, time, my_data_quantize);
}


/**
 * s3d_box_insitu_reduction_tstep
 *
 */
void s3d_box_insitu_reduction_tstep(int *id, 
                                    MPI_Comm *mpicomm, 
                                    int time,
                                    int sample_step)
{
    uint8_t *data_reduced = 0;
    int     data_size = 0;    
    
    hpgv_insitu_downsample_tstep_(*id, 
                                  *mpicomm, 
                                  time,
                                  sample_step,
                                  &data_reduced,
                                  &data_size);    
    
#ifdef DATASPACE
    struct data_descriptor desc;
    int rank, groupsize;
    
    MPI_Comm_size(*mpicomm, &groupsize);
    MPI_Comm_rank(*mpicomm, &rank);
    
    desc.size = data_size;
    desc.rank = rank;
    desc.tstep = time;
    desc.type = VISUALIZATION;
    desc.num_obj = groupsize;
    
    //fprintf(stdout, "%d put obj size %d %d at %ld\n", *id, data_size, desc.size, (uint64_t)(data_reduced));
    
    ds_put_obj_data((void *)data_reduced, &desc);
#endif
    
    free(data_reduced);
}

/**
 * s3d_box_viz_finalize
 *
 */
void s3d_box_viz_finalize(int *id, 
                          MPI_Comm *mpicomm)
{
    hpgv_insituvis_fini_(*id, *mpicomm);
}

#else

/**
 * s3d_box_intransit_render_reduction_tstep
 *
 */
void s3d_box_intransit_render_reduction_tstep(MPI_Comm *mpicomm,
                                              uint8_t **data_buffer,
                                              int data_num,
                                              int time)
{
    
    hpgv_intransit_render_downsample(*mpicomm,
                                     data_buffer, 
                                     data_num, 
                                     my_data_quantize,
                                     time);
    
}

/**
 * perform_intransit_viz
 *
 */
int perform_intransit_viz(MPI_Comm mpicomm, struct list_head *data_list)
{
    struct data_item *item = 0;
    int ts, count = 0;
    int rank = 0;
    uint8_t **data_buffer = 0;
    int data_num = 0;
    int recv_size = 0;

    list_for_each_entry(item, data_list, item_entry)
    {
        count++;
        recv_size += item->desc.size;
    }

    fprintf(stdout, " intransit viz recv %d\n", recv_size);

    data_num = count;
    data_buffer = (uint8_t **)calloc(data_num, sizeof(uint8_t*));

    if (data_buffer == 0) {
        fprintf(stderr, "Out of Memory");
        exit(0);
    }

    count = 0;

    list_for_each_entry(item, data_list, item_entry)
    {
        data_buffer[count++] = (uint8_t *)(item->buf);
        ts = item->desc.tstep;
    }

    s3d_box_intransit_render_reduction_tstep(&mpicomm,
                                             data_buffer,
                                             data_num,
                                             ts);

    free(data_buffer);

    return 0;
}

#endif


