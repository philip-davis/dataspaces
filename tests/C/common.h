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
	USE_DIMES
};

inline int common_init(int num_peers, int appid) {
	return dspaces_init(num_peers, appid); 
}

inline void common_set_storage_type(int fst) {
	dspaces_set_storage_type(fst);
} 

inline int common_rank() {
	return dspaces_rank();
}

inline int common_peers() {
	return dspaces_peers();
}

inline void common_barrier() {
	dspaces_barrier();
}

inline void common_finalize() {
	dspaces_finalize();
}

inline void common_lock_on_read(const char *lock_name, void *gcomm) {
	dspaces_lock_on_read(lock_name, gcomm);
}

inline void common_unlock_on_read(const char *lock_name, void *gcomm) {
	dspaces_unlock_on_read(lock_name, gcomm);
}

inline void common_lock_on_write(const char *lock_name, void *gcomm) {
	dspaces_lock_on_write(lock_name, gcomm);
}

inline void common_unlock_on_write(const char *lock_name, void *gcomm) {
	dspaces_unlock_on_write(lock_name, gcomm);
}

inline int common_put (const char *var_name,
        unsigned int ver, int size,
        int xl, int yl, int zl,
        int xu, int yu, int zu,
        void *data, enum transport_type type) {
	if ( type == USE_DSPACES ) {
		return dspaces_put(var_name, ver, size,
			xl, yl, zl, xu, yu, zu,
			data);
	} else if (type == USE_DIMES) {
#ifdef DS_HAVE_DIMES
		return dimes_put(var_name, ver, size,
			xl, yl, zl, xu, yu, zu,
			data);
#else
		uloga("%s(): DataSpaces DIMES is not enabled!\n", __func__);
		return -1;
#endif
	}
}

inline int common_get (const char *var_name,
        unsigned int ver, int size,
        int xl, int yl, int zl,
        int xu, int yu, int zu,
        void *data, enum transport_type type) {
	if ( type == USE_DSPACES ) {
		return dspaces_get(var_name, ver, size,
			xl, yl, zl, xu, yu, zu,
			data);
	} else if (type == USE_DIMES) {
#ifdef DS_HAVE_DIMES
		return dimes_get(var_name, ver, size,
			xl, yl, zl, xu, yu, zu,
			data);
#else
		uloga("%s(): DataSpaces DIMES is not enabled!\n", __func__);
		return -1;
#endif
	}
}

inline int common_put_sync(enum transport_type type) {
	if (type == USE_DSPACES) {
		return dspaces_put_sync();
	} else if (type == USE_DIMES) {
#ifdef DS_HAVE_DIMES
		return dimes_put_sync_all();
#else
		uloga("%s(): DataSpaces DIMES is not enabled!\n", __func__);
		return -1;
#endif
	}
}

int common_run_server(int num_sp, int num_cp, enum transport_type type);

void check_data(const char *var_name, double *buf, int num_elem, int rank, int ts);

#endif //end of __TEST_COMMON_H_
