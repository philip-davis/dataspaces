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
 * @brief Set types of memory layout for multi-dimensional matrix.
 *
 * Type of memory layout needs to be properly set before calling put/get APIs.
 * 
 * The following enum type defines the types of memory layout:
 * enum storage_type {row_major, column_major};
 *
 * @param[in] fst:	Type of memory layout for the matrix.
 *
 * @return:	Void.
 */
void dimes_set_storage_type (int fst);

/**
 * @brief Query the space to insert data specified by a geometric
 * 	  descriptor.
 * 
 * Memory buffer pointed by pointer "data" is a sub-region of the
 * global n-dimensional array (n <= 3) in user application, which is described
 * by the local bounding box {(xl, yl, zl), (xu, yu, zu)}.
 *
 * This routine is non-blocking, and successful return of the routine does not 
 * guarantee the completion of data transfer from data inserting processes to 
 * data retrieving processes. 
 * 
 * @param[in] var_name:		Name of the variable.
 * @param[in] ver:		Version of the variable.
 * @param[in] size:		Size (in bytes) for each element of the global
 *				array.
 * @param[in] xl:		x-coordinate for the lower corner of the local
 *				bounding box.
 * @param[in] yl:		y-coordinate for the lower corner of the local
 *				bounding box.
 * @param[in] zl:		z-coordinate for the lower corner of the local
 *				bounding box.
 * @param[in] xu:		x-coordinate for the upper corner of the local
 *				bounding box.
 * @param[in] yu:		y-coordinate for the upper corner of the local
 *				bounding box.
 * @param[in] zu:		z-coordinate for the upper corner of the local
 *				bounding box.
 * @param[in] data:		Pointer to user data buffer. 
 *
 * @return	0 indicates success.
 */
int dimes_put (const char *var_name,
        unsigned int ver, int size,
        uint64_t xl, uint64_t yl, uint64_t zl,
        uint64_t xu, uint64_t yu, uint64_t zu,
        void *data);

/**
 * @brief Query the space to retrieve data specified by a geometric descriptor.
 *
 * Memory buffer pointed by pointer "data" is a sub-region of the
 * global n-dimensional array (n <= 3) in user application, which is described
 * by the local bounding box {(xl, yl, zl), (xu, yu, zu)}.
 *
 * After successful return of the routine, received data is copied into user
 * buffer pointed by "data".
 *
 * @param[in] var_name:		Name of the variable.
 * @param[in] ver:		Version of the variable.
 * @param[in] size:		Size (in bytes) for each element of the global
 *				array.
 * @param[in] xl:		x-coordinate for the lower corner of the local
 *				bounding box.
 * @param[in] yl:		y-coordinate for the lower corner of the local
 *				bounding box.
 * @param[in] zl:		z-coordinate for the lower corner of the local
 *				bounding box.
 * @param[in] xu:		x-coordinate for the upper corner of the local
 *				bounding box.
 * @param[in] yu:		y-coordinate for the upper corner of the local
 *				bounding box.
 * @param[in] zu:		z-coordinate for the upper corner of the local
 *				bounding box.
 * @param[in] data:		Pointer to user data buffer. 
 * 
 * @return	0 indicates success.
 */
int dimes_get (const char *var_name,
        unsigned int ver, int size,
        uint64_t xl, uint64_t yl, uint64_t zl,
        uint64_t xu, uint64_t yu, uint64_t zu,
        void *data);

/**
 * @brief When configure option "enable_dimes_ack" is set, this function
 * will block till all in-memory data inserted by dimes_put() operations
 * have been retrieved by remote reading application, then free the data
 * buffers allocated for dimes_put(). 
 *
 * When configure option "enable_dimes_ack" is not set, this function is
 * non-blocking, and will just free all data buffers allocated for
 * dimes_put() since last call to dimes_put_sync_all(). In this case, the
 * users are responsible to coordinate (e.g. using our lock/unlock APIs in 
 * dspaces.h) between the coupled applications to make sure that data is 
 * fetched by the reading appication before freed.
 *
 * @return	0 indicates success.
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
 * associated with group 'group_name'. Note: the scopes of different groups can
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

int dimes_put_with_gdim (const char *var_name,
        unsigned int ver, int size,
        int ndim, uint64_t *lb, uint64_t *ub, uint64_t *gdim,
        void *data);
int dimes_get_with_gdim (const char *var_name,
        unsigned int ver, int size,
        int ndim, uint64_t *lb, uint64_t *ub, uint64_t *gdim,
        void *data);

#ifdef __cplusplus
}
#endif

#endif

