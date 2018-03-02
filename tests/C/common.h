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
*  Qian Sun (2014)  TASSL Rutgers University
*  qiansun@cac.rutgers.edu
*/
#ifndef __TEST_COMMON_H_
#define __TEST_COMMON_H_

#include "debug.h"
#include "timer.h"
#include "dataspaces.h"
#include "ds_gspace.h"
#ifdef DS_HAVE_DIMES
#include "dimes_interface.h"
#include "dimes_server.h"
#endif

enum transport_type {
	USE_DSPACES = 0,
	USE_DIMES = 1
};

// wrapper functions of DataSpaces/DIMES APIs
int common_init(int num_peers, int appid, void* comm, const char* parameters);
int common_rank(); 
int common_peers();
void common_barrier();
void common_finalize(); 
void common_lock_on_read(const char *lock_name, void *gcomm);
void common_unlock_on_read(const char *lock_name, void *gcomm);
void common_lock_on_write(const char *lock_name, void *gcomm); 
void common_unlock_on_write(const char *lock_name, void *gcomm); 
int common_put (const char *var_name,
        unsigned int ver, int size,
	int ndim,
        uint64_t *lb, uint64_t *ub,
	void *data, enum transport_type type);
int common_get (const char *var_name,
        unsigned int ver, int size,
	int ndim,
	uint64_t *lb, uint64_t *ub,
        void *data, enum transport_type type); 
int common_put_sync(enum transport_type type); 
int common_run_server(int num_sp, int num_cp, enum transport_type type, void* gcomm);

void check_data(const char *var_name, double *buf, int num_elem, int rank, int ts);

int write_data_file(const char* fname, void *data, size_t size);
int read_data_file(const char* fname);

int commom_get_transport_type_str(enum transport_type type, char* str);
#endif //end of __TEST_COMMON_H_
