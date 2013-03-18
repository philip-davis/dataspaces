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
#ifndef __DIMES_CLIENT_H__
#define __DIMES_CLIENT_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "debug.h"
#include "dart.h"
#include "dc_gspace.h"
#include "ss_data.h"
#include "timer.h"

struct query_tran_d {
        struct list_head        q_list;
        int                     num_ent;
};

struct dimes_client {
        struct dcg_space *dcg;
        struct sspace *ssd; //only used for hashing
        struct bbox domain;
        struct query_tran_d qt;
	
	// Local memory storage
	struct ss_storage *storage;
	// List of pending object data requests (from peers of 
	// other coupled apps)
	struct list_head req_list;

        int    f_ss_info;
};

struct dimes_client* dimes_alloc(void *);
void dimes_free(void);

#ifdef __cplusplus
}
#endif

#endif
