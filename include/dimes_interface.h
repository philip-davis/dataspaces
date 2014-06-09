/*
 * Copyright (c) 2009, NSF Cloud and Autonomic Computing Center, Rutgers University
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided
 * that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, this list of conditions and
 * the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
 * the following disclaimer in the documentation and/or other materials provided with the distribution.
 * - Neither the name of the NSF Cloud and Autonomic Computing Center, Rutgers University, nor the names of its
 * contributors may be used to endorse or promote products derived from this software without specific prior
 * written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

/*
*  Fan Zhang (2012)  TASSL Rutgers University
*  zhangfan@cac.rutgers.edu
*  Qian Sun (2014) TASSL Rutgers University
*  qiansun@cac.rutgers.edu
*/
#ifndef __DIMES_H__
#define __DIMES_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

/** @file dimes_interface.h
 *  @brief DIMES data coupling APIs for performing RDMA-based memory-to-memory
 *  redistribution of global array between tightly coupled applications.
 *  
 *  DataSpaces APIs dspaces_put and dspaces_get enable memroy-to-memory data sharing
 *  using dedicated staging servers. DIMES APIs provides the same high-level 
 *  put/get programming abstraction, but implements the parallel data
 *  redistribution though RDMA-based point-to-point transferes directly between data
 *  inserting and retrieving application processes, and bypass the staging servers.
 *
 */

/**
 * @brief Query the space to insert data specified by a geometric
 *    descriptor.
 * 
 * Memory buffer pointed by pointer "data" is a sub-region of the
 * global n-dimensional array in user application, which is described
 * by the local bounding box {(lb[0],lb[1],..,lb[n-1]), (ub[0],ub[1],..,ub[n-1])}.
 *
 * This routine is non-blocking, and successful return of the routine does not 
 * guarantee the completion of data transfer from data inserting processes to 
 * data retrieving processes. 
 * 
 * Note: ordering of dimension (fast->slow) is 0, 1, ..., n-1. For C row-major
 * array, the dimensions need to be reordered to construct the bounding box. For
 * example, the bounding box for C array c[2][4] is lb: {0,0}, ub: {3,1}. 
 * 
 * @param[in] var_name:     Name of the variable.
 * @param[in] ver:      Version of the variable.
 * @param[in] size:     Size (in bytes) for each element of the global
 *              array.
 * @param[in] ndim:     the number of dimensions for the local bounding
 *              box. 
 * @param[in] lb:       coordinates for the lower corner of the local
 *                  bounding box.
 * @param[in] ub:       coordinates for the upper corner of the local
 *                  bounding box. 
 * @param[in] data:     Pointer to user data buffer. 
 *
 * @return  0 indicates success.
 */
int dimes_put (const char *var_name,
        unsigned int ver, int size,
        int ndim, uint64_t *lb, uint64_t *ub, 
        void *data); 


/**
 * @brief Query the space to retrieve data specified by a geometric descriptor.
 *
 * Memory buffer pointed by pointer "data" is a sub-region of the
 * global n-dimensional array in user application, which is described
 * by the local bounding box {(lb[0],lb[1],..,lb[n-1]), (ub[0],ub[1],..,ub[n-1])}.
 *
 * After successful return of the routine, received data is copied into user
 * buffer pointed by "data".
 *
 * Note: ordering of dimension (fast->slow) is 0, 1, ..., n-1. For row-major
 * array, the dimensions need to be reordered to construct the bounding box. For
 * example, the bounding box for C array c[2][4] is lb: {0,0}, ub: {3,1}.
 *
 * @param[in] var_name:     Name of the variable.
 * @param[in] ver:      Version of the variable.
 * @param[in] size:     Size (in bytes) for each element of the global
 *              array.
 * @param[in] ndim:     the number of dimensions for the local bounding
 *              box. 
 * @param[in] lb:       coordinates for the lower corner of the local
 *              bounding box.
 * @param[in] ub:       coordinates for the upper corner of the local
 *              bounding box.
 * @param[in] data:     Pointer to user data buffer. 
 * 
 * @return  0 indicates success.
 */
int dimes_get (const char *var_name,
        unsigned int ver, int size,
        int ndim, uint64_t *lb, uint64_t *ub,
        void *data);

/**
 * @brief Define the global dimension for array variable.
 *
 * Note: ordering of dimension (fast->slow) is 0, 1, ..., n-1. For C row-major
 * array, the dimensions need to be reordered to construct the bounding box. For
 * example, the bounding box for C array c[2][4] is lb: {0,0}, ub: {3,1}.
 *
 * @param[in] var_name:     Name of the variable.
 * @param[in] ndim:     the number of dimensions for the global array. 
 * @param[in] gdim:     dimension for the global array.
 * 
 * @return  0 indicates success.
 */
void dimes_define_gdim (const char *var_name,
        int ndim, uint64_t *gdim);

/**
 * @brief This function is non-blocking, and will just free all data buffers 
 * allocated for dimes_put() since last call to dimes_put_sync_all(). 
 * In this case, the users are responsible to coordinate 
 * (e.g. using our lock/unlock APIs in dataspaces.h) between the coupled 
 * applications to make sure that data is fetched by the reader appication 
 * before freed.
 *
 * @return  0 indicates success.
 */
int dimes_put_sync_all(void);

/**
 * @brief Start the scope of a specific group.
 *
 * @param[in] group_name:   Name of the group.
 * @param[in] version:      Current time step. 
 *
 * @return  0 indicates success.
 */
int dimes_put_set_group(const char *group_name, int version);

/**
 * @brief End the scope of a group. Variables written (by dimes_put()) inside
 * a matching pair of dimes_put_set_group() and dimes_put_unset_group() are
 * associated with group 'group_name'.
 *
 * Note: the scopes of different groups can
 * not be interleaved or nested, and need to be non-overlapping.
 *
 * @return  0 indicates success.
 */
int dimes_put_unset_group();

/**
 * @brief This function is similar to dimes_put_sync_all(), with the exception
 * that it only applies to data variables that are associated with a particular
 * group. It will free all data buffers allocated for dimes_put() since last
 * call to dimes_put_sync_group, for data variables of the specified group.
 *
 * @param[in] group_name:   Name of the group.
 * @param[in] version:      Current time step.   
 *
 * @return  0 indicates success.
 */
int dimes_put_sync_group(const char *group_name, int version);


#ifdef __cplusplus
}
#endif

#endif

