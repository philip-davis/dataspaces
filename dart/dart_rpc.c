/*
 * Copyright (c) 2017, NSF Cloud and Autonomic Computing Center, Rutgers University
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

#include "dart_rpc.h"
#include "debug.h"
#include "list.h"

#define RPC_DEFAULT_CMD_NS 0

/* globally defined rpc_server, for backwards compatibility and non-threadsafe operation */
static struct rpc_server *g_rpc_s = NULL;

rpc_return_t rpc_add_service(unsigned char rpc_cmd, rpc_service rpc_func)
{
    return rpc_add_service_with_namespace(RPC_DEFAULT_CMD_NS, rpc_cmd, rpc_func);
}

rpc_return_t rpc_add_service_with_namespace(rpc_cmd_ns_t rpc_ns, rpc_cmd_t rpc_cmd, rpc_service rpc_func)
{
	return rpc_add_service_with_namespace_r(g_rpc_s, rpc_ns, rpc_cmd, rpc_func);
}

/* allocate a new rpc namespace with the given id (rpc_ns) */
static struct rpc_cmd_ns *alloc_rpc_ns(struct rpc_server *rpc_s, rpc_cmd_ns_t rpc_ns)
{
	struct rpc_cmd_ns *ns = NULL;

	if(!rpc_s) {
		uloga("%s(): rpc_s is NULL.\n", __func__);
		goto err_out;
	}

	ns = malloc(sizeof(ns));
	if(!ns) {
		uloga("%s(): allocating ns failed.\n", __func__);
		goto err_out;
	}

	ns->ns = rpc_ns;
	ns->num_rpc_funcs = rpc_s->ns_rpc_count;

	ns->rpc_func = malloc(sizeof(*(ns->rpc_func)) * ns->num_rpc_funcs);
	if(!ns->rpc_func) {
		uloga("%s(): allocating ns->rpc_func failed.\n", __func__);
		goto err_out;
	}

	return(ns);

err_out:
	uloga("%s(): failed.\n", __func__);
	return(NULL);

}

/* Find pointer to rpc namespace with given id (rpc_ns) in rpc_s. If one doesn't exist, create it. */
static struct rpc_cmd_ns *find_rpc_ns(struct rpc_server *rpc_s, rpc_cmd_ns_t rpc_ns)
{
	struct rpc_cmd_ns *ns_temp, *ns = NULL;

	if(!rpc_s) {
		uloga("%s(): rpc_s is NULL.\n", __func__);
		goto err_out;
	}

	list_for_each_entry(ns_temp, &rpc_s->rpc_ns, struct rpc_cmd_ns, rpc_ns_entry) {
		if(ns_temp->ns == rpc_ns) {
			ns = ns_temp;
			break;
		}
	}

	if(!ns) {
		ns = alloc_rpc_ns(rpc_s, rpc_ns);
		if(!ns) {
			goto err_out;
		}
		list_add_tail(&ns->rpc_ns_entry, &rpc_s->rpc_ns);
	}

	return(ns);

err_out:
	uloga("%s(): failed.\n", __func__);
	return(NULL);
}

rpc_return_t rpc_add_service_with_namespace_r(struct rpc_server *rpc_s,
												rpc_cmd_ns_t rpc_ns,
												rpc_cmd_t rpc_cmd,
												rpc_service rpc_func)
{

	struct rpc_cmd_ns *ns;
	rpc_return_t err = RPC_RC_SUCCESS;

	if(!rpc_s) {
		uloga("%s(): rpc_s is NULL.\n", __func__);
		err = RPC_RC_INVALID_PARAM;
		goto err_out;
	}

	ns = find_rpc_ns(rpc_s, rpc_ns);
	if(!ns) {
		err = GNI_RC_ERROR_NOMEM;
		goto err_out;
	}

	if(rpc_cmd >= ns->num_rpc_funcs) {
		uloga("'%s': rcp_cmd (%d) is out of range. Max rpc_cmd is %lu\n",
			__func__, rpc_cmd, ns->num_rpc_funcs - 1);
		err = RPC_RC_INVALID_PARAM;
		goto err_out;
	} else {
		ns->rpc_func[rpc_cmd] = rpc_func;
	}

err_out:
	uloga("%s(): failed.\n", __func__);
	return(err);

}
