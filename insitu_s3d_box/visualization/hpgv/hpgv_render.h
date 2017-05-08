/**
 * hpgv_render.h
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


#ifndef HPGV_RENDER_H
#define HPGV_RENDER_H

#ifdef __cplusplus
extern "C" {
#endif
#include <mpi.h>    
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hpgv_gl.h"    
#include "hpgv_block.h"
#include "hpgv_utilmath.h"
#include "hpgv_util.h"
#include "hpgv_composite.h"
#include "hpgv_parameter.h"
    
void hpgv_vis_para(para_input_t *para_input);

void hpgv_vis_render(block_t *block, int root, MPI_Comm comm, int opt);

const void * hpgv_vis_get_imageptr();

int hpgv_vis_get_imagetype();

int hpgv_vis_get_imageformat();
        
void hpgv_vis_init(MPI_Comm comm, int root);
        
void hpgv_vis_finalize();

int hpgv_vis_valid();

void hpgv_vis_render_one_block(block_t *block, int root, MPI_Comm comm, int blend);

float vis_get_block_eyedepth(block_t *block_t);

#ifdef __cplusplus
}
#endif

#endif
