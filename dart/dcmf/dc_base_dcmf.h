/* Copyright (c) 2009, NSF Cloud and Autonomic Computing Center, Rutgers University
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*
* - Redistributions of source code must retain the above copyright notice,
* this list of conditions and the following disclaimer.
* - Redistributions in binary form must reproduce the above copyright notice,
* this list of conditions and the following disclaimer in the documentation
* and/or other materials provided with the distribution.
* - Neither the name of the NSF Cloud and Autonomic Computing Center, Rutgers
* University, nor the names of its contributors may be used to endorse or
* promote products derived from this software without specific prior written
* permission.
*
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
* POSSIBILITY OF SUCH DAMAGE.
*
*/

/*
*  Fan Zhang (2011) TASSL Rutgers University
*  zhangfan@cac.rutgers.edu
*/

#ifndef __DC_BASE_DCMF_H_
#define __DC_BASE_DCMF_H_

#ifdef __cplusplus
extern "C" {
#endif

#include "dart_rpc_dcmf.h"

struct dart_client{
	struct rpc_server	*rpc_s;
	
	int		peer_size;
	struct node_id	*peer_tab;
	struct node_id	*cn_peers;
	
	int	num_sp,num_cp;
	
	struct node_id	*self;
	
	/*Number of compute peers in the app job*/
	int	cp_in_job;
	/*Rank of the master peer in the app job*/
	int	cp_min_rank;
	
	/*??*/
	int	cp_barrier_req;
	
	int	f_reg:1; //Registration flag
	int	f_bar:1;
	
	void	*dart_ref;
	int	num_posted;

	//For testing DART DCMF performance
	int 	read_complete;
};

#define dc_barrier(dc) rpc_barrier(dc->rpc_s)

static inline struct node_id * dc_get_peer(struct dart_client *dc, int n)
{
	return dc->peer_tab + n;
}

static inline struct dart_client *dc_ref_from_rpc(struct rpc_server *rpc_s)
{
	return rpc_s->dart_ref;
}

struct dart_client *dc_alloc(int,int,void*,void*);
void dc_free(struct dart_client*);
int dc_process(struct dart_client*);

//For testing DART DCMF performance
int dc_send_test(struct dart_client *dc, size_t size);
int dc_read_test(struct dart_client *dc, size_t size);

#ifdef __cplusplus
}
#endif

#endif
