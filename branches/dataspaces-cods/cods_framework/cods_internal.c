#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "cods_internal.h"
#include "cods_strutil.h"

#ifdef HAVE_UGNI
int process_event(struct dcg_space *dcg)
{
    int err;
    err = rpc_process_event_with_timeout(dcg->dc->rpc_s, 1);
    if (err < 0)
        goto err_out;

    return 0;
err_out:
    ERROR_TRACE();
}
#endif

#ifdef HAVE_DCMF
int process_event(struct dcg_space *dcg)
{
    int err;
    err = rpc_process_event(dcg->dc->rpc_s);
    if (err < 0)
        goto err_out;

    return 0;
err_out:
    ERROR_TRACE();
}
#endif

static int client_rpc_send_completion_callback(struct rpc_server *rpc_s, struct msg_buf *msg)
{
    struct client_rpc_send_state *p = (struct client_rpc_send_state *)msg->private;
    p->f_done = 1;
    return 0;
}

int client_rpc_send(struct dcg_space *dcg, struct node_id *peer, struct msg_buf *msg, struct client_rpc_send_state *state)
{
    int err;
    state->f_done = 0;
    msg->cb = client_rpc_send_completion_callback;
    msg->private = state;

    err = rpc_send(dcg->dc->rpc_s, peer, msg);
    if (err < 0) {
        goto err_out;
    }

    // Wait/block for the message delivery 
    while (!state->f_done) {
        err = process_event(dcg);
        if (err < 0) {
            goto err_out;
        }
    }

    return 0;
 err_out:
    uloga("ERROR %s: err= %d\n", __func__, err);
    return err;
}

int write_meta_data(const char* var_name, size_t size, void *send_buf)
{
    int err;
    int ver = 0, ndim = 1;
    uint64_t lb[1], ub[1], gdim[1];
    gdim[0] = dspaces_get_num_space_server();
    lb[0] = ub[0] = 0;
    dspaces_define_gdim(var_name, ndim, gdim);
    err = dspaces_put(var_name, ver, size, ndim, lb, ub, send_buf);
    if (err < 0) {
        goto err_out;
    }
    dspaces_put_sync();
    return 0;
 err_out:
    return err;
}

int read_meta_data(const char* var_name, size_t size, void *recv_buf)
{
    int err;
    int ver = 0, ndim = 1;
    uint64_t lb[1], ub[1], gdim[1];
    gdim[0] = dspaces_get_num_space_server();
    lb[0] = ub[0] = 0;
    dspaces_define_gdim(var_name, ndim, gdim);
    err = dspaces_get(var_name, ver, size, ndim, lb, ub, recv_buf);
    if (err < 0) {
        goto err_out;
    }
    return 0;
 err_out:
    return err;    
}
