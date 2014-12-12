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
*  Ciprian Docan (2009)  TASSL Rutgers University
*  docan@cac.rutgers.edu
*  Tong Jin (2011) TASSL Rutgers University
*  tjin@cac.rutgers.edu
*  Hoang Bui (2012-2013) TASSL Rutgers University
*  hbui@cac.rutgers.edu
*  Fan Zhang (2013) TASSL Rutgers University
*  zhangfan@cac.rutgers.edu
*  Qian Sun (2014) TASSL Rutgers University
*  qiansun@cac.rutgers.edu
*
*/

#ifndef __DATASPACES_H_
#define __DATASPACES_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

/**
 * @file dataspaces.h
 * @brief DataSpaces APIs.
 */

/**
 * @brief Initialize dataspaces client library.
 *
 * @param[in] num_peers:    Number of peers in the client application.
 * @param[in] appid:        Unique id for client application.
 * @param[in] comm:     Pointer to the MPI communicator.
 * @param[in] parameters:   A series of name=value pairs separated by ";". 
 *
 * Note: current implementation does NOT have any user-provided parameters, and 
 * 'parameters' is reserved for future use. User application can just pass NULL
 * for 'parameters'. The input communicator 'comm' is optional. 
 *
 * @return  0 indicates success.
 */
int dspaces_init(int num_peers, int appid, void *comm, const char *parameters);

/**
 * @brief Finalize dataspaces client library.
 *
 * @return  Void.
 */
void dspaces_finalize (void);

/**
 * @brief Return the rank for current process.
 *
 * For client application with num_peers processes, the returned rank
 * values range from 0~(num_peers-1).
 *
 * Note: this rank value does not necessarily equal to the MPI rank if the 
 * client application uses MPI.
 *
 * @return  Rank of current process.
 */
int dspaces_rank(void);

/**
 * @brief Return the number of peers in current application.
 *  
 * @return  Number of application peers.
 */
int dspaces_peers(void);

/**
 * @brief Blocks until all processes in the client application have reached
 *    this routine.
 *
 * @return  Void.
 */
void dspaces_barrier(void);

/**
 * @brief Acquire the read lock "lock_name".
 *
 * Note: this is a collective routine to be invoked by a group of application 
 * processes. When input communicator is NULL, all applications peers need
 * to call this routine collectively. When input communicator is a valid 
 * MPI communicator, only the application peers that belong to the communicator
 * group need to call this routine.
 * 
 * @param[in] lock_name:    Name of the lock.       
 * @param[in] comm:     Pointer to the MPI communicator.
 *
 * @return  Void.       
 */
void dspaces_lock_on_read(const char *lock_name, void *comm);

/**
 * @brief Release the read lock.
 *
 * Note: this is a collective routine to be invoked by a group of application 
 * processes. When input communicator is NULL, all applications peers need
 * to call this routine collectively. When input communicator is a valid 
 * MPI communicator, only the application peers that belong to the communicator
 * group need to call this routine.
 * 
 * @param[in] lock_name:    Name of the lock.       
 * @param[in] comm:     Pointer to the MPI communicator.
 *
 * @return  Void.       
 */
void dspaces_unlock_on_read(const char *lock_name, void *comm);

/**
 * @brief Acquire the write lock.
 *
 * Note: this is a collective routine to be invoked by a group of application 
 * processes. When input communicator is NULL, all applications peers need
 * to call this routine collectively. When input communicator is a valid 
 * MPI communicator, only the application peers that belong to the communicator
 * group need to call this routine.
 * 
 * @param[in] lock_name:    Name of the lock.       
 * @param[in] comm:     Pointer to the MPI communicator.
 *
 * @return  Void.       
 */
void dspaces_lock_on_write(const char *lock_name, void *comm);

/**
 * @brief Release the write lock.
 * 
 * Note: this is a collective routine to be invoked by a group of application 
 * processes. When input communicator is NULL, all applications peers need
 * to call this routine collectively. When input communicator is a valid 
 * MPI communicator, only the application peers that belong to the communicator
 * group need to call this routine.
 *
 * @param[in] lock_name:    Name of the lock.       
 * @param[in] comm:     Pointer to the MPI communicator.
 *
 * @return  Void.       
 */
void dspaces_unlock_on_write(const char *lock_name,void *comm);

/**
 * @brief Query the space to insert data specified by a geometric
 *    descriptor.
 * 
 * Memory buffer pointed by pointer "data" is a sub-region of the
 * global n-dimensional array in user application, which is described
 * by the local bounding box {(lb[0],lb[1],..,lb[n-1]), (ub[0],ub[1],..,ub[n-1])}.
 *
 * This routine is non-blocking, and successful return of the routine does not 
 * guarantee the completion of data transfer from client process to dataspaces 
 * staging server. User applications need to call dspaces_put_sync to check if
 * the most recent dspaces_put is complete or not.
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
int dspaces_put (const char *var_name,
        unsigned int ver, int size,
        int ndim, uint64_t *lb, uint64_t *ub, 
        const void *data); 

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
int dspaces_get (const char *var_name,
        unsigned int ver, int size,
        int ndim, uint64_t *lb, uint64_t *ub,
        void *data);

/**
 * Remove a particular version of a variable (whole domain)
 * @param[in] var_name:     Name of the variable.
 * @param[in] ver:      Version of the variable.
**/
int dspaces_remove (const char *var_name,
	unsigned int ver);

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
void dspaces_define_gdim (const char *var_name,
        int ndim, uint64_t *gdim);

/**
 * @brief Block till the completion of most recent data insert query.
 *
 * Note: it is recommended to invoke dspaces_put_sync after each call of 
 * dspaces_put, in order to ensure correct insertion of data into dataspaces.
 *
 * @return  0 indicates success.
 */
int dspaces_put_sync(void);

/**
 * @brief Get number of DataSpaces servers.
 * @return Number of space server.
 */
int dspaces_get_num_space_server(void);

#ifdef __cplusplus
}
#endif
#endif

