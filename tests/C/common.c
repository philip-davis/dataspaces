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
*  Fan Zhang (2013)  TASSL Rutgers University
*  zhangfan@cac.rutgers.edu
*  Qian Sun (2014) TASSL Rutgers University
*  qiansun@cac.rutgers.edu
*/

#include <stdio.h>
#include <stdlib.h>
#include "common.h"
#include "mpi.h"

int common_init(int num_peers, int appid, void* comm, const char* parameters) {
        return dspaces_init(num_peers, appid, comm, parameters);
}

int common_rank() {
        return dspaces_rank();
}

int common_peers() {
        return dspaces_peers();
}

void common_barrier() {
        dspaces_barrier();
}

void common_finalize() {
        dspaces_finalize();
}

void common_kill() {
        dspaces_kill();
}
void common_lock_on_read(const char *lock_name, void *gcomm) {
        dspaces_lock_on_read(lock_name, gcomm);
}

void common_unlock_on_read(const char *lock_name, void *gcomm) {
        dspaces_unlock_on_read(lock_name, gcomm);
}

void common_lock_on_write(const char *lock_name, void *gcomm) {
        dspaces_lock_on_write(lock_name, gcomm);
}

void common_unlock_on_write(const char *lock_name, void *gcomm) {
        dspaces_unlock_on_write(lock_name, gcomm);
}

int common_put(const char *var_name, 
	unsigned int ver, int size,
	int ndim,
	uint64_t *lb, uint64_t *ub,
	void *data, enum transport_type type)
{
	if ( type == USE_DSPACES ) {
		return dspaces_put(var_name, ver, size,
                        ndim,lb, ub,data);
    } else if (type == USE_DIMES) {
#ifdef DS_HAVE_DIMES
		return dimes_put(var_name, ver, size, 
			ndim, lb, ub, data);
#else
		uloga("%s(): DataSpaces DIMES is not enabled!\n", __func__);
		return -1;
#endif
	}
    return 0;
}

int common_get(const char *var_name,
	unsigned int ver, int size,
	int ndim,
	uint64_t *lb, uint64_t *ub,
	void *data, enum transport_type type)
{
	if ( type == USE_DSPACES ) {
		return dspaces_get(var_name, ver, size,
                        ndim,lb, ub,data);
	} else if (type == USE_DIMES) {
#ifdef DS_HAVE_DIMES
		return dimes_get(var_name, ver, size, 
			ndim, lb, ub, data);
#else
		uloga("%s(): DataSpaces DIMES is not enabled!\n", __func__);
		return -1;
#endif
	}
    return 0;
}

int common_put_sync(enum transport_type type) {
        if (type == USE_DSPACES ) {
                return dspaces_put_sync();
        } else if (type == USE_DIMES) {
#ifdef DS_HAVE_DIMES
                return dimes_put_sync_all();
#else
                uloga("%s(): DataSpaces DIMES is not enabled!\n", __func__);
                return -1;
#endif
        }
        return 0;
}

int common_run_server(int num_sp, int num_cp, enum transport_type type, void* gcomm) {
        int err;
        if (type == USE_DSPACES) {
                struct ds_gspace *dsg;
                dsg = dsg_alloc(num_sp, num_cp, "dataspaces.conf", gcomm);
                if (!dsg)
                        return -1;

                while (!dsg_complete(dsg)){
                        err = dsg_process(dsg);
                        if(err<0)
                                break;
                }

                //dsg_barrier(dsg);
		MPI_Barrier(*(MPI_Comm*)gcomm);
                dsg_free(dsg);

                if (err == 0)
                        uloga("All ok.\n");
                return 0;
        } else if (type == USE_DIMES) {
#ifdef DS_HAVE_DIMES
                struct dimes_server *dsg;
                dsg = dimes_server_alloc(num_sp, num_cp, "dataspaces.conf", gcomm);
                if (!dsg)
                        return -1;

                while (!dimes_server_complete(dsg)){
                        err = dimes_server_process(dsg);
                        if(err<0)
                                break;
                }

                //dimes_server_barrier(dsg);
		MPI_Barrier(*(MPI_Comm*)gcomm);
                dimes_server_free(dsg);

                if (err == 0)
                        uloga("All ok.\n");
                return 0;
#else
                uloga("%s(): Dataspaces DIMES is not enabled!\n", __func__);
                return -1;
#endif
        }
        return 0;
}
