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
*
*/

#ifndef __DATASPACES_H_
#define __DATASPACES_H_

/**
 * @file dataspaces.h
 * @brief DataSpaces APIs.
 */

/**
 * @brief Initialize dataspaces client library.
 * 
 * @param[in] num_peers:	Number of peers in the client application.
 * @param[in] appid:		Unique id for client application.
 *
 * @return	0 indicates success.
 */
int dspaces_init(int num_peers, int appid);

/**
 * @brief Finalize dataspaces client library.
 *
 * @return	Void.
 */
void dspaces_finalize (void);

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
void dspaces_set_storage_type (int fst);

/**
 * @brief Return the rank for current process.
 *
 * For client application with num_peers processes, the returned rank
 * values range from 0~(num_peers-1).
 *
 * Note: this rank value does not necessarily equal to the MPI rank if the 
 * client application uses MPI.
 *
 * @return	Rank of current process.
 */
int dspaces_rank(void);

/**
 * @brief Return the number of peers in current application.
 *	
 * @return	Number of application peers.
 */
int dspaces_peers(void);

/**
 * @brief Blocks until all processes in the client application have reached
 * 	  this routine.
 *
 * @return	Void.
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
 * @param[in] lock_name:	Name of the lock.		
 * @param[in] comm:		Pointer to the MPI communicator.
 *
 * @return	Void.		
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
 * @param[in] lock_name:	Name of the lock.		
 * @param[in] comm:		Pointer to the MPI communicator.
 *
 * @return	Void.		
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
 * @param[in] lock_name:	Name of the lock.		
 * @param[in] comm:		Pointer to the MPI communicator.
 *
 * @return	Void.		
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
 * @param[in] lock_name:	Name of the lock.		
 * @param[in] comm:		Pointer to the MPI communicator.
 *
 * @return	Void.		
 */
void dspaces_unlock_on_write(const char *lock_name,void *comm);

/**
 * @brief Query the space to insert data specified by a geometric
 * 	  descriptor.
 * 
 * Memory buffer pointed by pointer "data" is a sub-region of the
 * global n-dimensional array (n <= 3) in user application, which is described
 * by the local bounding box {(xl, yl, zl), (xu, yu, zu)}.
 *
 * This routine is non-blocking, and successful return of the routine does not 
 * guarantee the completion of data transfer from client process to dataspaces 
 * staging server. User applications need to call dspaces_put_sync to check if
 * the most recent dspaces_put is complete or not. 
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
int dspaces_put (const char *var_name, 
        unsigned int ver, int size,
        int xl, int yl, int zl,
        int xu, int yu, int zu, 
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
int dspaces_get (const char *var_name, 
        unsigned int ver, int size,
        int xl, int yl, int zl, 
        int xu, int yu, int zu, 
        void *data);

/**
 * @brief Block till the completion of most recent data insert query.
 *
 * Note: it is recommended to invoke dspaces_put_sync after each call of 
 * dspaces_put, in order to ensure correct insertion of data into dataspaces.
 *
 * @return	0 indicates success.
 */
int dspaces_put_sync(void);

void dspaces_set_mpi_rank(int rank);

#endif

