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
*/
/* 
 * RPC service implementation. 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
// #include <pthread.h>

// #include "config.h"
// #ifdef HAVE_CRAY_PORTALS
// #include <portals/portals3.h>
// #endif

#include "dart_rpc_portals.h"
#include "debug.h"

#define SYS_WAIT_COMPLETION(x)					\
	while (!(x)) {						\
		err = sys_process_event(rpc_s);			\
		if (err < 0)					\
			goto err_out;				\
	}


#define MD_USE_INC(rpc_s)	rpc_s->num_md_posted++
#define MD_USE_DEC(rpc_s)	rpc_s->num_md_unlinked++

static struct rpc_server *rpc_s_instance;

static int log2_ceil(int n)
{
        unsigned int i;
        int k = -1;

        i = ~(~0U >> 1);
        while (i && !(i & n))
                i = i >> 1;
        if (i != n)
                i = i << 1;

        while (i) {
                k++;
                i = i >> 1;
        }

        return k;
}

static void show_limits(ptl_ni_limits_t *nil)
{
	uloga("'%s()': {.me = %d, .md = %d, .eq = %d, .iov = %d, "
		".melist = %d}",
		__func__, nil->max_mes, nil->max_mds, nil->max_eqs,
		nil->max_md_iovecs, nil->max_me_list);
}

/* Routine to initialize the portals system. */
static int init_portals(struct rpc_server *rpc_s)
{
	int max_ifaces, err;
        ptl_ni_limits_t actual;
	ptl_interface_t ni;

#ifndef HAVE_CRAY_PORTALS
	if (getenv( "PTL_IFACE" ) == NULL)
		setenv( "PTL_IFACE", "eth0", 0 );
#endif /* HAVE_CRAY_PORTALS */

        err = PtlInit(&max_ifaces);
        if (err != PTL_OK)
                goto err_out;

#ifdef HAVE_CRAY_PORTALS
        // jaguar service node
        // ni = IFACE_FROM_BRIDGE_AND_NALID( PTL_BRIDGE_UK, PTL_IFACE_SS );
        ni = CRAY_USER_NAL;
        // printf( "using CRAY_USER_NAL\n" );
#else
        /* 
           This should give a compile error, HAVE_CRAY_PORTALS is not
           defined, and so is j.
        */
	int j = 20;
        // generic (linux) node
        ni = PTL_IFACE_DEFAULT;
#endif /* HAVE_CRAY_PORTALS */

	// get a handle to the network interface
        err = PtlNIInit(ni, PTL_PID_ANY, NULL, &actual, &rpc_s->nih); 
        if (err != PTL_OK && err != PTL_IFACE_DUP) 
                goto err_out;

	// show_limits(&actual);

        err = PtlGetId(rpc_s->nih, &rpc_s->ptlmap.ptlid);
        if (err != PTL_OK)
                goto err_out;

        return 0;
 err_out:
        uloga("'%s()': failed with %s.\n", __func__, ptl_err_str[err]);
        return -EIO;
}

/*
  Initialize the resources for system messages.
*/
static int sys_init(struct rpc_server * rpc_s)
{
        ptl_handle_me_t meh;
        ptl_process_id_t ptl_any = {.nid = PTL_NID_ANY, .pid = PTL_PID_ANY};
        ptl_md_t md;
        int err;

        /* Init resources for system messages. */
        err = PtlEQAlloc(rpc_s->nih, 65535, PTL_EQ_HANDLER_NONE, &rpc_s->sys_eqh);
        if (err != PTL_OK)
                goto err_out;

        /* Setup the system send MD. */
        memset(&md, 0, sizeof(md));
        md.start = NULL;
        md.length = 0;
        md.threshold = PTL_MD_THRESH_INF;
        md.options = PTL_MD_EVENT_START_DISABLE | PTL_MD_EVENT_END_DISABLE | 
                        PTL_MD_TRUNCATE | PTL_MD_OP_PUT;
        md.eq_handle = rpc_s->sys_eqh;

        err = PtlMDBind(rpc_s->nih, md, PTL_RETAIN, &rpc_s->sys_snd_mdh);
        if (err != PTL_OK)
                goto err_out;

        /* Setup the system receive MD. */
        memset(&md, 0, sizeof(md));
        md.start = NULL;
        md.length = 0;
        md.threshold = PTL_MD_THRESH_INF;
        md.options = PTL_MD_EVENT_START_DISABLE | PTL_MD_TRUNCATE | PTL_MD_OP_PUT;
        md.eq_handle = rpc_s->sys_eqh;

        err = PtlMEAttach(rpc_s->nih, 0, ptl_any, MB_SYS_MSG, 0, 
                                PTL_UNLINK, PTL_INS_AFTER, &meh);
        if (err != PTL_OK)
                goto err_out;

        err = PtlMDAttach(meh, md, PTL_RETAIN, &rpc_s->sys_rcv_mdh);
        if (err == 0)
                return 0;
 err_out:
        uloga("'%s()': failed with %s (%d).\n", __func__, ptl_err_str[err], err);
        return err;
}

static void sys_cleanup(struct rpc_server *rpc_s)
{
        int err;

        err = PtlMDUnlink(rpc_s->sys_rcv_mdh);
        if (err != PTL_OK)
                uloga("'%s()' problem %s (%d).\n", __func__, ptl_err_str[err], err);

        err = PtlMDUnlink(rpc_s->sys_snd_mdh);
        if (err != PTL_OK)
                uloga("'%s()' problem %s (%d).\n", __func__, ptl_err_str[err], err);

        err = PtlEQFree(rpc_s->sys_eqh);
        if (err != PTL_OK)
                uloga("'%s()' problem %s (%d).\n", __func__, ptl_err_str[err], err);
}

static int clean_portals(struct rpc_server *rpc_s)
{
	int err;

	err = PtlNIFini(rpc_s->nih);
	if (err != PTL_OK) {
		uloga("'%s()': failed with %s.\n", __func__, ptl_err_str[err]);
		return -EIO;
	}

	// uloga("'%s()' successfull.\n", __func__);
	return 0;
}

/* 
   Publish   buffers  for  communication   with  the   Portals  system
   library. All "service" messages will go into these buffers.
*/
static int rpc_post_buffer(struct rpc_server *rpc_s, struct rpc_request *rr)
{
        ptl_handle_me_t meh;
        ptl_handle_md_t mdh;
        ptl_process_id_t any = {.nid = PTL_NID_ANY, .pid = PTL_PID_ANY};
        ptl_md_t md = {.start = rr->msg->msg_data, 
                .length = rr->msg->size, 
                .threshold = rpc_s->num_rpc_per_buff, // rpc_s->num_peers,
                .max_size = 0, 
                .options = PTL_MD_OP_PUT | PTL_MD_EVENT_START_DISABLE | 
                                PTL_MD_EVENT_AUTO_UNLINK_ENABLE,
                .user_ptr = rr,
                .eq_handle = rpc_s->eqh};
        int err;

        err = PtlMEAttach(rpc_s->nih, 0, any, MB_RPC_MSG, 0, PTL_UNLINK, \
                PTL_INS_AFTER, &meh);
        if (err)
                goto err_out;

        err = PtlMDAttach(meh, md, PTL_UNLINK, &mdh);
        if (err)
                goto err_free_me;

        rr->mdh_rpc = mdh;
        // uloga("'%s()': buffer posted successfuly\n", __func__);

	MD_USE_INC(rpc_s);

        return 0;
 err_free_me:
        PtlMEUnlink(meh);
 err_out:
        uloga("'%s()': failed with %s.\n", __func__, ptl_err_str[err]);
        return -EIO;
}

/* 
   Generic routine to post, i.e., to send, a request message to a peer
   node.
*/
static int rpc_post_request(struct rpc_server *rpc_s, const struct node_id *peer, 
                            struct rpc_request *rr, const struct hdr_sys *hs)
{
        ptl_handle_md_t mdh;
        ptl_md_t md = {.start = rr->data,
                .length = rr->size, 
                .threshold = 1,
                .max_size = 0, 
                .options = PTL_MD_OP_PUT | PTL_MD_EVENT_START_DISABLE | 
                                PTL_MD_EVENT_AUTO_UNLINK_ENABLE,
                .user_ptr = rr,
                .eq_handle = rpc_s->eqh};
        int err = 0;

        ptl_hdr_data_t hdr_data = hs ? * (ptl_hdr_data_t *) hs : 0;

	if (rr->f_vec == set) {
		md.options = md.options | PTL_MD_IOVEC;
	}

        err = PtlMDBind(rpc_s->nih, md, PTL_UNLINK, &mdh);
        if (err != PTL_OK)
                goto err_out;

        err = PtlPut(mdh, PTL_NO_ACK_REQ, peer->ptlmap.ptlid, 
                     rpc_s->pi, 0, peer->mb, 0, hdr_data);
        if (err != PTL_OK)
                goto err_free_md;

        rr->mdh_rpc = mdh;
        rr->msg->refcont++;

	MD_USE_INC(rpc_s);
	// REMOVE:        rpc_server_inc_reply(rpc_s);

        return 0;
 err_free_md:
        PtlMDUnlink(mdh);
 err_out:
        uloga("'%s()': failed with %s.\n", __func__, ptl_err_str[err]);
        uloga("'%s()': data is at address %p.\n", __func__, rr->data);
        return -EIO;
}

static int rpc_fetch_request(struct rpc_server *rpc_s, 
                             const struct node_id *peer, struct rpc_request *rr)
{
        ptl_handle_md_t mdh;
        ptl_md_t md = { .start = rr->data,
                .length = rr->size,
                .threshold = 2,
                .max_size = 0, 
                .options = PTL_MD_OP_GET | PTL_MD_EVENT_START_DISABLE | 
                                PTL_MD_EVENT_AUTO_UNLINK_ENABLE,
                .user_ptr = rr,
                .eq_handle = rpc_s->eqh};
        int err;

        err = PtlMDBind(rpc_s->nih, md, PTL_UNLINK, &mdh);
        if (err != PTL_OK)
                goto err_out;

        err = PtlGet(mdh, peer->ptlmap.ptlid, rpc_s->pi, 0, peer->mb, 0);
        if (err != PTL_OK)
                goto err_free_md;

        rr->mdh_data = mdh;
        rr->msg->refcont++;

	MD_USE_INC(rpc_s);

        return 0;
 err_free_md:
        PtlMDUnlink(mdh);
 err_out:
        uloga("'%s': failed with %s.\n", __func__, ptl_err_str[err]);
        return -EIO;
}

static int rpc_prepare_buffers(
                struct rpc_server *rpc_s, const struct node_id *peer, 
                struct rpc_request *rr, enum io_dir dir)
{
        ptl_handle_me_t meh;
        ptl_handle_md_t mdh;
        ptl_md_t md = { .start = rr->msg->msg_data,
                .length = rr->msg->size,
                .threshold = 1,
                .max_size = 0, 
                // .options = PTL_MD_OP_GET | PTL_MD_EVENT_START_DISABLE | 
                .options = PTL_MD_EVENT_START_DISABLE | 
                           PTL_MD_EVENT_AUTO_UNLINK_ENABLE | 
                           ((dir == io_send) ? PTL_MD_OP_GET : PTL_MD_OP_PUT),
                .user_ptr = rr,
                .eq_handle = rpc_s->eqh};
        int err;

	if (rr->f_vec == set) {
		md.options = md.options | PTL_MD_IOVEC;
	}

        // peer->mb++;
        rr->msg->msg_rpc->mbits = rpc_s->mbits; // peer->mb;

        /* Yes, we would accept a GET only from 'peer'. */
        err = PtlMEAttach(rpc_s->nih, 0, peer->ptlmap.ptlid, rpc_s->mbits, // peer->mb, 
                          0, PTL_UNLINK, PTL_INS_BEFORE, &meh);
        if (err != PTL_OK)
                goto err_out;

        err = PtlMDAttach(meh, md, PTL_UNLINK, &mdh);
        if (err != PTL_OK)
                goto err_out_free;

        rpc_s->mbits++;
        if (rpc_s->mbits <= MB_MIN_RESERVED)
                rpc_s->mbits = MB_MIN_RESERVED + 1;

        rr->mdh_data = mdh;
        rr->msg->refcont++;

	MD_USE_INC(rpc_s);

        return 0;
 err_out_free:
        PtlMEUnlink(meh);
 err_out:
        uloga("'%s()' failed with %s.\n", __func__, ptl_err_str[err]);
        return -EIO;
}

static struct {
	enum cmd_type		rpc_cmd;
	rpc_service		rpc_func;
} rpc_commands[_CMD_COUNT];

static int num_service = 0;

/*
  Local routine to poll on the rpc event queue; it is implemented as a
  separate routine  because it is  called from multiple  places. TODO:
  find a better name for it.
*/
static int __rpc_process_event(struct rpc_server *rpc_s)
{
        ptl_event_t ev;
        int err, n;

        err = PtlEQPoll(&rpc_s->eqh, 1, 300, &ev, &n);
        if (err == PTL_EQ_EMPTY)
                return 0;

        if (err == PTL_EQ_DROPPED)
                uloga("Error: at least one event has been dropped from queue!.\n");
        else if (err != PTL_OK) {
                uloga("'%s()' failed with: %s\n", __func__, ptl_err_str[err]);
                return -EIO;
        }

        /* OK. All good. */
        struct rpc_request *rr = (struct rpc_request *) ev.md.user_ptr;
        err = (*rr->cb)(rpc_s, &ev);
        if (err == 0)
                return 0;

        uloga("'%s()': failed on rpc callback with %d.\n", __func__, err);
        return err;
}

/*
  Generic routine to send a system message.
*/
static int sys_send(struct rpc_server *rpc_s, struct node_id *to, struct hdr_sys *hs)
{
        ptl_hdr_data_t hdrdat = * (ptl_hdr_data_t *) hs;
        int err;

        err = PtlPutRegion(rpc_s->sys_snd_mdh, 0, 0, PTL_NO_ACK_REQ, 
                        to->ptlmap.ptlid, 0, 0, to->mb, 0, hdrdat);
        if (err != PTL_OK) {
                uloga("'%s()': failed with %s (%d).\n", 
                        __func__, ptl_err_str[err], err);
                return -EIO;
        }

        return 0;
}

/*
  Routine to send  a request for credits to a  remote node; routine is
  called by the local process.
*/
static int sys_credits_request(struct rpc_server *rpc_s, struct node_id *to)
{
        struct hdr_sys req_hs;
        int err;

        to->mb = MB_SYS_MSG;
        memset(&req_hs, 0, sizeof(req_hs));
        req_hs.sys_cmd = sys_msg_req;
        req_hs.sys_id = rpc_s->ptlmap.id;

        err = sys_send(rpc_s, to, &req_hs);
        to->mb = MB_RPC_MSG;
        if (err == 0)
                return 0;

        return err;
}

/*
  Routine  to return  credits  to  a remote  node;  routine called  in
  response to a credits request.
*/
static int sys_credits_return(struct rpc_server *rpc_s, struct hdr_sys *hs)
{
        struct node_id *to, tmp;
        struct hdr_sys ret_hs;
        int err;

        to = &rpc_s->peer_tab[hs->sys_id];
        if (to->num_msg_ret == 0) {
                /* No redits to return right now. */
                to->f_need_msg = 1;
                return 0;
        }

        tmp.mb = to->mb;
        to->mb = MB_SYS_MSG;

	// BUG: 
	// uloga("returnS %d\n", to->num_msg_ret);

        memset(&ret_hs, 0, sizeof(ret_hs));
        ret_hs.sys_cmd = sys_msg_ret;
        ret_hs.sys_msg = to->num_msg_ret;
        ret_hs.sys_id = rpc_s->ptlmap.id;
        to->num_msg_ret = 0;

        err = sys_send(rpc_s, to, &ret_hs);
        to->mb = tmp.mb;
        if (err == 0) 
                return 0;

        to->num_msg_ret = ret_hs.sys_msg;
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}

#if 0 /* Here is the old routine ... */
static int sys_credits_return(struct rpc_server *rpc_s, struct hdr_sys *hs)
{
        struct node_id *to, tmp;
        struct hdr_sys ret_hs;
        int err;

        to = &rpc_s->peer_tab[hs->sys_id];
        if (to->num_msg_ret == 0) {
                /* No redits to return right now. */
                to->f_need_msg = 1;
                return 0;
        }

        tmp.mb = to->mb;
        to->mb = MB_SYS_MSG;

        memset(&ret_hs, 0, sizeof(ret_hs));
        ret_hs.sys_cmd = sys_msg_ret;
        ret_hs.sys_msg = to->num_msg_ret;
        ret_hs.sys_id = rpc_s->ptlmap.id;
        to->num_msg_ret = 0;

        err = sys_send(rpc_s, to, &ret_hs);
        to->mb = tmp.mb;
        if (err == 0) 
                return 0;

        to->num_msg_ret = ret_hs.sys_msg;
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}
#endif

/* Forward definition. */
static int peer_process_send_list(struct rpc_server *, struct node_id *);

static int 
sys_update_credits_only(struct rpc_server *rpc_s, struct hdr_sys *hs, int num_return)
{
	struct node_id *peer;
	int err = 0;

	static int num_rec = 0;

	num_rec++;

	// BUG:
	// uloga("receiveD%d %d\n", num_rec, hs->sys_msg);

	if (hs->sys_cmd != sys_msg_ret || !rpc_s->peer_tab)
		return 0;

	peer = rpc_s->peer_tab + hs->sys_id;
	peer->num_msg_at_peer += hs->sys_msg;
	peer->num_msg_ret += num_return;

	if (peer->f_need_msg && peer->num_msg_ret) {
		err = sys_credits_return(rpc_s, hs);
		peer->f_need_msg = 0;
	}

	return err;
}

static int sys_update_credits_process_send_list(struct rpc_server *rpc_s, int id)
{
	struct node_id *peer = rpc_s->peer_tab + id;

	if (peer->f_req_msg && peer->num_msg_at_peer) {
		peer->f_req_msg = 0;
		return peer_process_send_list(rpc_s, peer);
	}

	return 0;
}

/*
  Routine  to update  the credits;  routine  called in  response to  a
  remote return credits.
  TODO:	split this in two: credit updated & peer process send queue !!!
*/
static int sys_update_credits(struct rpc_server *rpc_s, struct hdr_sys *hs, int num_return)
{
        struct node_id *peer;

	// BUG:
	// uloga("receiveS %d\n", hs->sys_msg);

        if (rpc_s->peer_tab && hs->sys_cmd == sys_msg_ret) {
                peer = &rpc_s->peer_tab[hs->sys_id];
                peer->num_msg_at_peer += hs->sys_msg;
                peer->num_msg_ret += num_return;

                if (peer->f_need_msg && peer->num_msg_ret) {
                        /* Send back some credits. */
                        sys_credits_return(rpc_s, hs);
                        peer->f_need_msg = 0;
                }

                if (peer->f_req_msg && peer->num_msg_at_peer) {
                        peer->f_req_msg = 0;

                        return peer_process_send_list(rpc_s, peer);
                }
        }

        return 0;
}

#define myid(rpc_s)		(rpc_s->ptlmap.id)
#define rank2id(rpc_s, rank)	((rank) + (rpc_s->app_minid))
#define id2rank(rpc_s, id)	((id) - (rpc_s->app_minid))
#define myrank(rpc_s)		id2rank(rpc_s, myid(rpc_s))

/*
  A remote peer entered the barrier.
*/
static int sys_bar_arrive(struct rpc_server *rpc_s, struct hdr_sys *hs)
{
	//        struct node_id *from;
	//        from = &rpc_s->peer_tab[hs->sys_id];
#if 0
        if (rpc_s->bar_id != hs->sys_pad1) {
                /* NOTE: I  should return an error code  here, the job
                   will block if enters this state !!! */
                uloga("'%s()': node %d, is not synchronized properly: bid = %d\n",
                        __func__, from->ptlmap.id, hs->sys_pad1);
                return 0;
        }
#endif
	/* NOTE: use the rank to  index the 'bat_tab' array. This is a
	   safe way  as the table is  allocated at init  time, and can
	   accomodate differences due to start-up times. */
	rpc_s->bar_tab[hs->sys_id] = hs->sys_pad1;

        return 0;
}

static int sys_bar_send(struct rpc_server *rpc_s, int peerid)
{
	struct node_id *peer = &rpc_s->peer_tab[peerid];
	struct hdr_sys hs;
	int err;

	memset(&hs, 0, sizeof(hs));
	hs.sys_cmd = sys_bar_enter;
	hs.sys_pad1 = rpc_s->bar_num;
	hs.sys_id = myrank(rpc_s);

	peer->mb = MB_SYS_MSG;
	err = sys_send(rpc_s, peer, &hs);
	peer->mb = MB_RPC_MSG;

	return err;
}

/*
  This is the  entry point for processing system  messages.  It can be
  called  from sys_process_event()  or  rpc_process_event() to  handle
  system events. System messages are not subject to flow control.
*/
static int sys_dispatch_event(struct rpc_server *rpc_s, struct hdr_sys *hs)
{
        int err = 0;

        switch (hs->sys_cmd) {
        case sys_none:
                break;

        case sys_msg_ret:
                err = sys_update_credits(rpc_s, hs, 0);
                break;

        case sys_msg_req:
                err = sys_credits_return(rpc_s, hs);
                break;

        case sys_bar_enter:
                err = sys_bar_arrive(rpc_s, hs);
                break;
        }

        return err;
}

static int sys_process_event(struct rpc_server *rpc_s)
{
        ptl_event_t ev;
        struct hdr_sys *hs;
        int err, n;

        int teqh = rpc_s->sys_eqh;

        // err = PtlEQGet(rpc_s->sys_eqh, &ev);
        err = PtlEQPoll(&rpc_s->sys_eqh, 1, 300, &ev, &n);

        if (teqh != rpc_s->sys_eqh)
                uloga("'%s()' fatal eq has changed, initial %d, final %d.\n",
                        __func__, teqh, rpc_s->sys_eqh);

        if (err == PTL_EQ_EMPTY)
                return 0;

        if (err == PTL_EQ_DROPPED) {
                uloga("'%s()': at least one event was dropped from queue.\n", 
                        __func__);
        }
        else if (err != PTL_OK) {
                uloga("'%s()': failed with %s(%d).\n", 
                        __func__, ptl_err_str[err], err);
                return -EIO;
        }

        hs = (struct hdr_sys *) &ev.hdr_data;

        return sys_dispatch_event(rpc_s, hs);
}

static int peer_process_send_list(struct rpc_server *rpc_s, struct node_id *peer)
{
        struct rpc_request *rr;
        struct hdr_sys hs;
        int err;

	// static int num_ret = 0;

        while (! list_empty(&peer->req_list)) {

                if (peer->num_msg_at_peer == 0) {
			/* Try to drain the queue first */
			err = rpc_process_event_with_timeout(rpc_s, 1);
			if (rpc_s->cmp_type == DART_CLIENT && err == 0)
				/* We  extracted sucessfully  an event
				   from  the queue,  try to  drain the
				   queue more. */
				continue;

			if (err < 0 && err != -ETIME)
				goto err_out;


                        if (!peer->f_req_msg) {
				err = sys_credits_request(rpc_s, peer);
                                if (err < 0)
                                        goto err_out;
                                peer->f_req_msg = 1;
                        }

			// TODO: check this path ... performance ?!
                        break;
                }

                /* Number of send credits is good, we will send the message */

                rr = list_entry(peer->req_list.next, struct rpc_request, req_entry);
                if (rr->msg->msg_data) {
                        err = rpc_prepare_buffers(rpc_s, peer, rr, rr->iodir);
                        if (err < 0)
                                goto err_out;

			if (rr->f_vec)
				rr->f_vec = unset;
                }

                /* Return the credits we have for peer. */
                memset(&hs, 0, sizeof(hs));
                hs.sys_cmd = sys_msg_ret;
                hs.sys_msg = peer->num_msg_ret;
                hs.sys_id = rpc_s->ptlmap.id; // msg->msg_rpc->id;

		// BUG: 
		// num_ret++;
		// uloga("returnP%d %d\n", num_ret, peer->num_msg_ret);

                err = rpc_post_request(rpc_s, peer, rr, &hs);
                if (err < 0)
                        goto err_out;

                /* Message was sent, consume one credit. */
                peer->num_msg_at_peer--;
                peer->num_msg_ret -= hs.sys_msg;

                list_del(&rr->req_entry);
		peer->num_req--;
        }

        return 0;
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}

/* Forward definition. */
static int rpc_cb_decode(struct rpc_server *, ptl_event_t *);

/*
  Allocate   an  RPC  structure   for  communication   buffers,  which
  accomodates 'num_rpc' incomming requests.
*/
static struct rpc_request *rr_comm_alloc(int num_rpc)
{
	struct rpc_request *rr;
	size_t size;

	size = sizeof(*rr) + sizeof(*rr->msg) + sizeof(struct rpc_cmd) * num_rpc + 7;
	rr = malloc(size);
	if (!rr)
		return 0;

	memset(rr, 0, size);
	rr->cb = rpc_cb_decode;
	rr->mdh_rpc = rr->mdh_data = PTL_INVALID_HANDLE;
	rr->msg = (struct msg_buf *) (rr+1);
	rr->msg->msg_data =  rr->msg + 1;
	ALIGN_ADDR_QUAD_BYTES(rr->msg->msg_data);
	rr->msg->size = sizeof(struct rpc_cmd) * num_rpc;

	return rr;
}

struct rpc_request *check_buffer(struct rpc_server *rpc_s, struct rpc_request *rr)
{
        int num_rpc_per_buff;

        num_rpc_per_buff = rr->msg->size / sizeof(struct rpc_cmd);
        if (num_rpc_per_buff == rpc_s->num_rpc_per_buff) 
                return rr;

        list_del(&rr->req_entry);
        free(rr);

        rr = rr_comm_alloc(rpc_s->num_rpc_per_buff);
        if (!rr) {
                errno = ENOMEM;
                return 0;
        }

        list_add(&rr->req_entry, &rpc_s->rpc_list);

        return rr;
}

/* 
   Decode  and   service  a  command  message  received   in  the  rpc
   buffer. This routine is called by 'rpc_process_event()' in response
   to new incomming rpc request or to unlink and free some buffers.
*/
static int rpc_cb_decode(struct rpc_server *rpc_s, ptl_event_t *ev)
{
        struct rpc_request *rr = (struct rpc_request *) ev->md.user_ptr;
        struct rpc_cmd *cmd;
        int err, i;

        // DEBUG: Incomming request is not properly aligned !
        if ((ev->offset % sizeof(struct rpc_cmd)) != 0) {
                uloga("'%s()': error request not properly aligned, offset = %llu\n",
                        __func__, ev->offset);
        }

        if (ev->type == PTL_EVENT_UNLINK) {
		MD_USE_DEC(rpc_s);

                rr = check_buffer(rpc_s, rr);
                if (!rr)
                        return -ENOMEM;

		// DEBUG: TODO: revisit this ... 
		memset(rr->msg->msg_data, 0, rr->msg->size);
                err = rpc_post_buffer(rpc_s, rr);
                return err;
        }

        /* Use this in debugger to see the content of the structure. */
        struct hdr_sys *hs = (struct hdr_sys *) &ev->hdr_data;
	sys_update_credits_only(rpc_s, hs, 1);

        cmd = (struct rpc_cmd *) ((char *) rr->msg->msg_data + ev->offset);

        for (i = 0; i < num_service; i++) {
                if (cmd->cmd == rpc_commands[i].rpc_cmd) {
                        err = rpc_commands[i].rpc_func(rpc_s, cmd);
			break;
                }
        }

	if (i == num_service) {
	        uloga("Network command unknown %d!\n", cmd->cmd);
		err = -EINVAL;
	}

	if (err == 0)
		err = sys_update_credits_process_send_list(rpc_s, hs->sys_id);

        return err;
}

/* Forward declaration. */
int rpc_process_event(struct rpc_server *);

/*
  Default  completion  routine for  rpc  messages  we initiate.   This
  routine is called by  'rpc_process_event()' to complete rpc requests
  which were locally initiated.
*/
static int rpc_cb_req_completion(struct rpc_server *rpc_s, ptl_event_t *ev)
{
        struct rpc_request *rr = (struct rpc_request *) ev->md.user_ptr;
        int err;

        if (ev->type == PTL_EVENT_SEND_END || ev->type == PTL_EVENT_PUT_END ||
            ev->type == PTL_EVENT_REPLY_END || ev->type == PTL_EVENT_GET_END) {
                err = rpc_process_event(rpc_s);
                if (err < 0) {
                        uloga("'%s()': failed with %d.\n", __func__, err);
                        return err;
                }

                return 0;
        }

        if (ev->type == PTL_EVENT_UNLINK) {
                rr->msg->refcont--;

		MD_USE_DEC(rpc_s);
	}

        if (rr->msg->refcont == 0) {
                (*rr->msg->cb)(rpc_s, rr->msg);
                free(rr);
                // REMOVE: rpc_server_dec_reply(rpc_s);
        }

        return 0;
}

/*
  Check if we have enough credits to send a new message at peer or
  send a request for credits update from peer.

  DELETE unused region ... 

*/
static int rpc_check_num_msg(struct rpc_server *rpc_s, struct node_id *to)
{
        int err;

        if (to->num_msg_at_peer > 0) {
                to->num_msg_at_peer--;
                return 0;
        }

        err = sys_credits_request(rpc_s, to);
        if (err < 0)
                goto err_out;

        /* Request for credits was successfull, now wait for peer to reply. */
        while (to->num_msg_at_peer == 0) {
                err = sys_process_event(rpc_s);
                if (err < 0)
                        goto err_out;
        }
        to->num_msg_at_peer--;

        return 0;
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}

/* 
   Public API starts here. 
*/

/*
  Determine portals address of master node, i.e., {nid, pid}. First
  try to read the values from environment vars, then try the 'conf'
  file.
*/
int rpc_read_config(ptl_process_id_t *ptlid)
{
        char *nid, *pid;
        FILE *f;
        int err;

        nid = getenv("P2TNID");
        pid = getenv("P2TPID");

        if (nid && pid) {
                ptlid->nid = atoi(nid);
                ptlid->pid = atoi(pid);

                return 0;
        }

        f = fopen("conf", "rt");
        if (!f) {
		err = -ENOENT;
		goto err_out;
        }

        err = fscanf(f, "P2TNID=%u\nP2TPID=%hu\n", 
                        &ptlid->nid, &ptlid->pid);

        fclose(f);
        if (err == 2)
                return 0;

	err = -EIO;

 err_out:
	ERROR_TRACE();
}

int rpc_write_config(struct rpc_server *rpc_s)
{
        FILE *f;
        int err;

        f = fopen("conf", "wt");
        if (!f)
                goto err_out;

        err = fprintf(f, "P2TNID=%u\nP2TPID=%u\n", 
                        rpc_s->ptlmap.ptlid.nid, rpc_s->ptlmap.ptlid.pid);
        if (err < 0)
                goto err_out_close;
        fclose(f);

        return 0;
 err_out_close:
        fclose(f);
 err_out:
        ulog_err("'%s()' failed with %d.", __func__, err);
        return -EIO;
}

/*
  Util routine to poll and process the event queues for 'timeout' ms.
*/
inline static int __process_event(struct rpc_server *rpc_s, int timeout)
{
	ptl_handle_eq_t eq_tab[] = {rpc_s->eqh, rpc_s->sys_eqh};
	ptl_event_t ev;
	int err, n;

	err = PtlEQPoll(eq_tab, 2, timeout, &ev, &n);
	if (err == PTL_EQ_EMPTY)
		return -ETIME;
	/*
	err = PtlEQWait(rpc_s->eqh, &ev);
	*/
	if (err == PTL_EQ_DROPPED)
		uloga("'%s()': error event has been dropped from queue %d !.\n",
			__func__, n);
	else if (err != PTL_OK) {
		uloga("'%s()' failed with: %s\n", __func__, ptl_err_str[err]);
                uloga("'%s()' eq = {%d, %d}\n", __func__, eq_tab[0], eq_tab[1]);
		return -EIO;
	}

	if (n == 0) {
		/* RPC queue ... */
		struct rpc_request *rr = (struct rpc_request *) ev.md.user_ptr;
		err = (*rr->cb)(rpc_s, &ev);
	}
	else {
		/* System queue ... */
		struct hdr_sys *hs;

		hs = (struct hdr_sys *) &ev.hdr_data;
		err = sys_dispatch_event(rpc_s, hs);
	}

	if (err == 0)
		return 0;

	ERROR_TRACE();
}

int rpc_process_event(struct rpc_server *rpc_s)
{
	int err;

	err = __process_event(rpc_s, 300);
	if (err == 0 || err == -ETIME)
		return 0;

	ERROR_TRACE();
}

int rpc_process_event_with_timeout(struct rpc_server *rpc_s, int timeout)
{
	int err;

	err = __process_event(rpc_s, timeout);
	if (err == 0 || err == -ETIME)
		return err;

	ERROR_TRACE();
}


/*
struct rpc_server *rpc_server_init(int master, int num_peers, 
                void *dart_ref, struct node_id peers_tab_ref[])
*/
struct rpc_server *
rpc_server_init(int num_buff, int num_rpc_per_buff, 
		void *dart_ref, enum rpc_component cmp_type)
{
        struct rpc_server *rpc_s = 0;
        struct rpc_request *rr;
	//        struct msg_buf *msg;
        int i, err = -ENOMEM;

	if (rpc_s_instance)
		return rpc_s_instance;

        rpc_s = calloc(1, sizeof(*rpc_s));
        if (!rpc_s)
		goto err_out_free;

        rpc_s->dart_ref = dart_ref;
        rpc_s->num_buff = num_buff;
        rpc_s->num_rpc_per_buff = num_rpc_per_buff;
        rpc_s->max_num_msg = num_buff;
	rpc_s->cmp_type = cmp_type;

        err = init_portals(rpc_s);
        if (err < 0)
                goto err_out_free;
        rpc_s->mbits = MB_MIN_RESERVED + 1;

        err = PtlEQAlloc(rpc_s->nih, 65535, PTL_EQ_HANDLER_NONE, &rpc_s->eqh);
        if (err != PTL_OK)
                goto err_out_free;

        err = sys_init(rpc_s);
        if (err != PTL_OK)
                goto err_out_free;

        INIT_LIST_HEAD(&rpc_s->rpc_list);
        for (i = 0; i < rpc_s->num_buff+1; i++) {
		rr = rr_comm_alloc(num_rpc_per_buff);
                if (!rr) {
			err = -ENOMEM;
                        goto err_out_free;
		}

                err = rpc_post_buffer(rpc_s, rr);
                if (err < 0) 
                        goto err_out_free;

                list_add(&rr->req_entry, &rpc_s->rpc_list);
        }

	rpc_s->bar_tab = malloc(sizeof(*rpc_s->bar_tab) * num_rpc_per_buff);
	if (!rpc_s->bar_tab) {
		err = -ENOMEM;
		goto err_out_free;
	}
	memset(rpc_s->bar_tab, 0, sizeof(*rpc_s->bar_tab) * num_rpc_per_buff);

	/* Init succeeded, set the instance reference here. */
	rpc_s_instance = rpc_s;

        return rpc_s;
 err_out_free:
        free(rpc_s);
	if (err < 0)
		uloga("'%s()': failed with %d.\n", __func__, err);
	else if (err > 0)
                uloga("'%s()': failed with %s.\n", __func__, ptl_err_str[err]);
        return 0;
}

static void list_credits(struct rpc_server *rpc_s)
{
        struct node_id *peer;
        int i;

        for (i = 0; i < rpc_s->num_peers; i++) {
                peer = rpc_s->peer_tab + i;
                uloga("P%2d : credits for remote P%2d: {send = %d, return %d}\n",
                        rpc_s->ptlmap.id, peer->ptlmap.id,
                        peer->num_msg_at_peer, peer->num_msg_ret);
        }
}

static int rpc_server_finish(struct rpc_server *rpc_s)
{
	struct node_id *peer;
	int i, err;

	peer = rpc_s->peer_tab;
	for (i = 0; i < rpc_s->num_peers; i++, peer++) {
		while (peer->num_req) {
			err = peer_process_send_list(rpc_s, peer);
			if (err < 0) {
				uloga("'%s()': encountered an error %d, skipping.\n", 
					__func__, err);
			}
		}
	}

	return 0;
}

void rpc_server_free(struct rpc_server *rpc_s)
{
        struct rpc_request *rr, *tmp;
	struct node_id *peer;
        int err, n = 0;

	rpc_server_finish(rpc_s);

	/* TODO:  From this  point on,  we should  not accept  any new
	   incomming  requests; so,  if they  still arrive,  we should
	   just  drop them and  drain the  event queue  to be  able to
	   release resources. */

	/* Process any remaining events from the event queue. */
        err = rpc_process_event_with_timeout(rpc_s, 100);
	while (err == 0 || err == -EINVAL) {
		n++;
	        err = rpc_process_event_with_timeout(rpc_s, 100);
	}

#ifdef DEBUG
	if (n > 0)
		uloga("'%s()': flushed %d events from the queue.\n",
			__func__, n);
#endif
	if (err != -ETIME)
		uloga("'%s()': error at flushing the event queue %d!\n", 
			__func__, err);
#ifdef DEBUG
	peer = rpc_s->peer_tab;
	for (n = 0; n < rpc_s->num_peers; n++, peer++)
		uloga("'%s()': for peer %d, there are %d reqs not processed.\n",
			__func__, peer->ptlmap.id, peer->num_req);
#endif
	n = 0;
        /* Unlink RPC communication buffers posted for incomming requests. */
        list_for_each_entry_safe(rr, tmp, &rpc_s->rpc_list, struct rpc_request, req_entry) {
                n++;
                list_del(&rr->req_entry);

                if (rr->mdh_rpc != PTL_INVALID_HANDLE) {
                        err = PtlMDUnlink(rr->mdh_rpc);
                        if (err != PTL_OK)
                                uloga("'%s()' unlink rpc MD error %s.\n", 
                                        __func__, ptl_err_str[err]);
			MD_USE_DEC(rpc_s);
                }

                if (rr->mdh_data != PTL_INVALID_HANDLE) {
                        err = PtlMDUnlink(rr->mdh_data);
                        if (err != PTL_OK)
                                uloga("'%s()' unlink data MD error %s.\n",
                                        __func__, ptl_err_str[err]);
			MD_USE_DEC(rpc_s);
                }
                free(rr);
        }

#ifdef DEBUG
        uloga("'%s()': number of buffers unlinked = %d.\n", __func__, n);
#endif
        err = PtlEQFree(rpc_s->eqh);
        if (err != PTL_OK)
                uloga("'%s()' free event queue error %s.\n", 
                        __func__, ptl_err_str[err]);
#ifdef DEBUG
	rpc_report_md_usage(rpc_s);
        // list_credits(rpc_s);
#endif
        sys_cleanup(rpc_s);

        clean_portals(rpc_s);

        free(rpc_s);
	// uloga("'%s()' OK. bye !\n", __func__);
}

struct rpc_server *rpc_server_get_instance(void)
{
	/* Blindly  get  the  rpc_server   reference;  if  it  is  not
	   initialized, should call rpc_server_init() */
	return rpc_s_instance;
}

/*
  Return the id of the rpc server.
*/
int rpc_server_get_id(void)
{
	// TODO: if server is not initialized, should return -1.
	return rpc_s_instance->ptlmap.id;
}

void rpc_server_set_peer_ref(struct rpc_server *rpc_s, 
                             struct node_id peer_tab[], int num_peers)
{
        rpc_s->num_peers = num_peers;
        rpc_s->peer_tab = peer_tab;
}

void rpc_server_set_rpc_per_buff(struct rpc_server *rpc_s, int num_rpc_per_buff)
{
        rpc_s->num_rpc_per_buff = num_rpc_per_buff;
}

/*
  System barrier implementation.
*/
int rpc_barrier(struct rpc_server *rpc_s)
{
	//	struct node_id *peer;
	int round, np;
	int next, prev;
        int err;

	np = log2_ceil(rpc_s->app_num_peers);
	round = -1;

	rpc_s->bar_num = (rpc_s->bar_num + 1) & 0xFF;
	// uloga("'%s()': barrier number %d.\n", __func__, rpc_s->bar_num);

	while (round < np-1) {

		round = round + 1;

		next = (myrank(rpc_s) + (1 << round)) % rpc_s->app_num_peers;
		prev = (rpc_s->app_num_peers + 
			myrank(rpc_s) - (1 << round)) % rpc_s->app_num_peers;

		// uloga("'%s()': round %d, next %d, prev %d.\n",
		//	__func__, round, next, prev);

		err = sys_bar_send(rpc_s, rank2id(rpc_s, next));
		if (err < 0)
			goto err_out;

		SYS_WAIT_COMPLETION(rpc_s->bar_tab[prev] == rpc_s->bar_num || 
			rpc_s->bar_tab[prev] == ((rpc_s->bar_num+1) & 0xFF));
	}

	return 0;
 err_out:
        ERROR_TRACE();
}

/* 
   Generic interface to send an rpc message to a remote node; peer is
   subject to flow control.
*/
int rpc_send(struct rpc_server *rpc_s, struct node_id *peer, 
             struct msg_buf *msg)
{
        struct rpc_request *rr;
        int err = -ENOMEM;

        rr = calloc(1, sizeof(struct rpc_request));
        if (!rr)
                goto err_out;

        rr->msg = msg;
        rr->iodir = io_send;
        rr->cb = rpc_cb_req_completion;
        rr->mdh_rpc = rr->mdh_data = PTL_INVALID_HANDLE;
        rr->data = msg->msg_rpc;
        rr->size = sizeof(*msg->msg_rpc);

        list_add_tail(&rr->req_entry, &peer->req_list);
	// DEBUG:
	peer->num_req++;

        err = peer_process_send_list(rpc_s, peer);
        if (err == 0)
                return 0;

 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}

inline static int __send_direct(
	struct rpc_server *rpc_s, const struct node_id *peer, 
	struct msg_buf *msg, flag_t f_vec)
{
        struct rpc_request *rr;
        int err = -ENOMEM;

        rr = calloc(1, sizeof(*rr));
        if (!rr)
                return err;

        rr->msg = msg;
        rr->cb = rpc_cb_req_completion;
        rr->mdh_data = rr->mdh_rpc = PTL_INVALID_HANDLE;
        rr->data = msg->msg_data;
        rr->size = msg->size;
	rr->f_vec = f_vec;

        err = rpc_post_request(rpc_s, peer, rr, 0);
	if (err < 0)
		free(rr);

	return err;
}

/*
  Generic interface to send a direct message to a remote node. It
  assumes the memory buffers are allready posted and available at the
  other end; these messages are not subject to flow control and they
  do not consumes credits.
*/
int rpc_send_direct(struct rpc_server *rpc_s, const struct node_id *peer, 
                    struct msg_buf *msg)
{
        int err;

	err = __send_direct(rpc_s, peer, msg, unset);
        if (err == 0)
                return 0;

	ERROR_TRACE();
}

int rpc_send_directv(struct rpc_server *rpc_s, const struct node_id *peer,
			struct msg_buf *msg)
{
	int err;

	err = __send_direct(rpc_s, peer, msg, set);
	if (err == 0)
		return 0;

	ERROR_TRACE();
}

/*
  Generic interface to receive a message from a remote node. 
*/
int rpc_receive_direct(struct rpc_server *rpc_s, const struct node_id *peer, 
                       struct msg_buf *msg)
{
        struct rpc_request *rr;
        int err = -ENOMEM;

        rr = calloc(1, sizeof(struct rpc_request));
        if (!rr)
                goto err_out;

        rr->msg = msg;
        rr->cb = rpc_cb_req_completion;
        rr->mdh_rpc = rr->mdh_data = PTL_INVALID_HANDLE;
        rr->data = msg->msg_data;
        rr->size = msg->size;

        err = rpc_fetch_request(rpc_s, peer, rr);
        if (err == 0) {
                // REMOVE: rpc_server_inc_reply(rpc_s);
                return 0;
        }

        free(rr);
 err_out:
        uloga("'%s'(): failed with %d.\n", __func__, err);
        return err;
}

inline static int __receive(struct rpc_server *rpc_s, 
	struct node_id *peer, struct msg_buf *msg, flag_t f_vec)
{
        struct rpc_request *rr;
	//        struct hdr_sys hs;
        int err = -ENOMEM;

        rr = calloc(1, sizeof(*rr));
        if (!rr)
		return err;

        rr->msg = msg;
        rr->iodir = io_receive;
        rr->cb = rpc_cb_req_completion;
        rr->mdh_rpc = rr->mdh_data = PTL_INVALID_HANDLE;
        rr->data = msg->msg_rpc;
        rr->size = sizeof(*msg->msg_rpc);
	rr->f_vec = f_vec;

        list_add_tail(&rr->req_entry, &peer->req_list);
	// DEBUG:
	peer->num_req++;

        err = peer_process_send_list(rpc_s, peer);
	return err;
	/*
        if (err == 0)
                return 0;
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
	*/
}

/*
  Generic routine to indirectly receive a message from a remote node.
*/
int rpc_receive(struct rpc_server *rpc_s, struct node_id *peer, 
                       struct msg_buf *msg)
{
	int err;

	err = __receive(rpc_s, peer, msg, unset);
	if (err == 0)
		return 0;

	ERROR_TRACE();
}

int rpc_receivev(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg)
{
	int err;

	err = __receive(rpc_s, peer, msg, set);
	if (err == 0)
		return 0;

	ERROR_TRACE();
}

void rpc_add_service(enum cmd_type rpc_cmd, rpc_service rpc_func)
{
        rpc_commands[num_service].rpc_cmd = rpc_cmd;
        rpc_commands[num_service].rpc_func = rpc_func;
        num_service++;
}

void rpc_report_md_usage(struct rpc_server *rpc_s)
{
	uloga("'%s()': MD posted %d, MD released %d, MD in use %d.\n",
	      __func__, rpc_s->num_md_posted, rpc_s->num_md_unlinked, 
	      rpc_s->num_md_unlinked - rpc_s->num_md_posted);
}

#if 0
void* rpc_server_run(void *arg)
{
        struct rpc_server *rpc_s = arg;
        int err;

        while (rpc_s->shoud_stop == 0) {
                err =  rpc_process_event(rpc_s);
                if (err < 0) {
                        fprintf(stderr, "Error on the processing loop.\n");
                        break;
                }
        }
        uloga("RPC posted: %d, RPC freed %d.\n", 
                rpc_s->num_rep_posted, rpc_s->num_rep_freed);

        rpc_server_clean(rpc_s);

        return NULL;
}

/* Routine to start the RPC server. */
int rpc_server_start(struct rpc_server *rpc_s)
{
        int err;

        err = pthread_create(&rpc_s->thread_id, 0, rpc_server_run, rpc_s);
        if (err < 0) {
                uloga("'%s()': failed can not create the thread.\n", __func__);
                return -1;
        }

        uloga("'%s()': thread started successfully.\n", __func__);
        // Should create and start a thread for the server
        // rpc_server_run(rpc_s);

        return 0;
}

void rpc_server_stop(struct rpc_server *rpc_s)
{
        rpc_s->shoud_stop = 1;

        if (rpc_s->thread_id > 0)
                pthread_join(rpc_s->thread_id, 0);
}

void rpc_server_wait(struct rpc_server *rpc_s)
{
        if (rpc_s->thread_id > 0) {
                pthread_join(rpc_s->thread_id, 0);

        }
}

int __main(void)
{
        struct rpc_server *rpc_s;
        int err;

        // printf("struct rsfhdr = %d bytes\n", sizeof(struct rfshdr));

        rpc_s = rpc_server_init(8, NULL);
        if (!rpc_s)
                return -1;

        rpc_server_run(rpc_s);

        rpc_server_clean(rpc_s);

        return 0;
}

#endif /* 0 */

struct msg_buf* msg_buf_alloc(struct rpc_server *rpc_s, 
        const struct node_id *peer, int num_rpcs)
{
        struct msg_buf *msg;
        size_t size;

        size = sizeof(struct msg_buf) + sizeof(struct rpc_cmd) * num_rpcs + 7;
        msg = calloc(1, size);
        if (!msg)
                return NULL;

        msg->peer = peer;
        msg->cb = default_completion_callback;
        if (num_rpcs > 0) {
                msg->msg_rpc = (struct rpc_cmd *) (msg+1);
                ALIGN_ADDR_QUAD_BYTES(msg->msg_rpc);
                msg->msg_rpc->dstnid = peer->ptlmap.ptlid.nid;
                msg->msg_rpc->dstpid = peer->ptlmap.ptlid.pid;
                msg->msg_rpc->srcnid = rpc_s->ptlmap.ptlid.nid;
                msg->msg_rpc->srcpid = rpc_s->ptlmap.ptlid.pid;
        }

        return msg;
}

void rpc_mem_info_cache(struct node_id *peer, struct msg_buf *msg, struct rpc_cmd *cmd)
{
	peer->mb = cmd->mbits;
}

void rpc_mem_info_reset(struct node_id *peer, struct msg_buf *msg, struct rpc_cmd *cmd)
{
	peer->mb = MB_RPC_MSG;
}
