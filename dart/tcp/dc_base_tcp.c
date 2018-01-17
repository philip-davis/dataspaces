#include "mpi.h"
#include "dc_base_tcp.h"
#include "debug.h"

static int rpc_cb_recv_cn_register(struct rpc_server *rpc_s, struct msg_buf *msg) {
    struct dart_client *dc = dc_ref_from_rpc(rpc_s);
    struct ptlid_map *p = (struct ptlid_map *)msg->msg_data;

    int i;
    for (i = 0; i < dc->peer_size; ++i) {
        struct node_id *peer = &dc->peer_tab[i];
        peer->ptlmap = p[i];
        // printf("[%s]: Client %d received ptlid_map data of peer %d (%s:%d).\n", __func__, rpc_s->ptlmap.id,
        //     peer->ptlmap.id, inet_ntoa(peer->ptlmap.address.sin_addr), (int)ntohs(peer->ptlmap.address.sin_port));
    }

    rpc_s->app_minid = dc->cp_min_rank;
    rpc_s->app_num_peers = dc->cp_in_job;
    dc->f_reg = 1;

    dc_barrier(dc);

    free(msg->msg_data);
    free(msg);
    return 0;
}

static int rpc_handler_cn_register(struct rpc_server *rpc_s, struct rpc_cmd *cmd) {
    struct dart_client *dc = dc_ref_from_rpc(rpc_s);

    struct payload_client_registration_reply *pl = (struct payload_client_registration_reply *)cmd->pad;
    dc->num_sp = pl->size_sp;
    dc->num_cp = pl->size_cp;
    dc->peer_size = dc->num_sp + dc->num_cp;
    dc->cp_min_rank = pl->id_cp_min;

    size_t size = sizeof(struct node_id) * (size_t)dc->peer_size;
    struct node_id *peer_tab = (struct node_id *)malloc(size);
    if (peer_tab == NULL) {
        goto err_out;
    }
    memset(peer_tab, 0, size);
    if (dc->peer_tab[0].f_connected) {
        peer_tab[dc->peer_tab[0].ptlmap.id] = dc->peer_tab[0];
    }
    if (dc->peer_tab[1].f_connected) {
        peer_tab[dc->peer_tab[1].ptlmap.id] = dc->peer_tab[1];
    }
    dc->peer_tab = peer_tab;
    dc->cn_peers = dc->peer_tab + dc->num_sp;

    int i;
    for (i = 0; i < dc->peer_size; ++i) {
        struct node_id *peer = &dc->peer_tab[i];
        INIT_LIST_HEAD(&peer->req_list);
    }

    rpc_server_set_peer_ref(rpc_s, dc->peer_tab, dc->peer_size);
    rpc_s->ptlmap.id = pl->id_cp;
    dc->self = &dc->peer_tab[rpc_s->ptlmap.id];
    dc->self->ptlmap = rpc_s->ptlmap;

    struct node_id *peer = &dc->peer_tab[pl->id_sp];
    struct msg_buf *msg = msg_buf_alloc(rpc_s, peer, 0);
    if (msg == NULL) {
        goto err_out;
    }

    size = sizeof(struct ptlid_map) * (size_t)dc->peer_size;
    msg->msg_data = malloc(size);
    if (msg->msg_data == NULL) {
        goto err_out;
    }
    msg->size = size;
    msg->cb = rpc_cb_recv_cn_register;

    if (rpc_receive_direct(rpc_s, peer, msg) < 0) {
        printf("[%s]: recv ptlid_map data of all peers from server %d failed!\n", __func__, pl->id_sp);
        goto err_out;
    }
    msg = NULL;

    return 0;

    err_out:
    /* TODO: may need to free msg */
    return -1;
}

static void *dc_listen(void *client) {
    struct dart_client *dc = (struct dart_client *)client;

    while (dc->rpc_s->thread_alive) {
        int sockfd_c = accept(dc->rpc_s->sockfd_s, NULL, NULL);
        if (sockfd_c < 0) {
            printf("[%s]: accept new connection failed, skip!\n", __func__);
            continue;
        }
        struct connection_info info;
        if (rpc_recv_connection_info(sockfd_c, &info) < 0) {
            printf("[%s]: recv connection info from newly connected peer failed, skip!\n", __func__);
            close(sockfd_c);
            continue;
        }
        struct node_id *peer = NULL;
        if (info.cmp_type == DART_CLIENT) {
            printf("[%s]: accept connection from a client, this should not happen, skip!\n", __func__);
            close(sockfd_c);
            continue;
        }
        if (!dc->f_reg) {
            peer = &dc->peer_tab[1];
            if (peer->f_connected) {
                printf("[%s]: accept connection from a server, but another server has been accepted before, skip!\n",
                    __func__);
                continue;
            }
            peer->ptlmap.id = info.id;
        } else {
            printf("[%s]: accept connection from a server, but it's not in registration phase, skip!\n", __func__);
            close(sockfd_c);
            continue;
        }
        peer->sockfd = sockfd_c;
        peer->f_connected = 1;
    }
}

static int dc_register_at_master(struct dart_client *dc, int appid) {
    struct msg_buf *msg = NULL;

    dc->peer_size = 2;
    size_t size = sizeof(struct node_id) * (size_t)dc->peer_size;
    dc->peer_tab = (struct node_id *)malloc(size);
    memset(dc->peer_tab, 0, size);
    rpc_server_set_peer_ref(dc->rpc_s, dc->peer_tab, dc->peer_size);

    const char *filename_conf = "conf";

    struct node_id *peer_master = &dc->peer_tab[0];
    INIT_LIST_HEAD(&peer_master->req_list);
    if (rpc_read_config(&peer_master->ptlmap.address, filename_conf) < 0) {
        printf("[%s]: read RPC config file failed!\n", __func__);
        goto err_out;
    }

    if (dc->comm != NULL) {
        /* If MPI_Comm exists, let the clients connect to server one by one, to optimize process layout */
        int proc_rank, proc_size, flag;
        MPI_Comm_rank(*(MPI_Comm *)dc->comm, &proc_rank);
        MPI_Comm_size(*(MPI_Comm *)dc->comm, &proc_size);
        if (proc_rank != 0) {
            MPI_Recv(&flag, 1, MPI_INT, proc_rank - 1, proc_rank - 1, *(MPI_Comm *)dc->comm, MPI_STATUS_IGNORE);
        }
        if (rpc_connect(dc->rpc_s, peer_master) < 0) {
            printf("[%s]: connect to master server failed!\n", __func__);
            goto err_out;
        }
        if (proc_rank + 1 != proc_size) {
            MPI_Send(&flag, 1, MPI_INT, proc_rank + 1, proc_rank, *(MPI_Comm *)dc->comm);
        }
    } else {
        if (rpc_connect(dc->rpc_s, peer_master) < 0) {
            printf("[%s]: connect to master server failed!\n", __func__);
            goto err_out;
        }
    }
    ulog("[%s]: Connected to the master server (%s:%d).\n", __func__,
        inet_ntoa(peer_master->ptlmap.address.sin_addr), (int)ntohs(peer_master->ptlmap.address.sin_port));

    msg = msg_buf_alloc(dc->rpc_s, peer_master, 1);
    if (msg == NULL) {
        printf("[%s]: allocate message failed!\n", __func__);
        goto err_out;
    }
    msg->msg_rpc->cmd = cn_register;
    *(struct ptlid_map *)msg->msg_rpc->pad = dc->rpc_s->ptlmap;
    if (rpc_send(dc->rpc_s, peer_master, msg) < 0) {
        printf("[%s]: send ptlid_map data to master server failed!\n", __func__);
        msg = NULL;
        goto err_out;
    }
    msg = NULL;

    while (!dc->f_reg) {
        rpc_process_event(dc->rpc_s);
    }
    dc_barrier(dc);
    return 0;

    err_out:
    if (msg != NULL) {
        free(msg);
    }
    return -1;
}

struct dart_client *dc_alloc(int num_peers, int appid, void *dart_ref, void *comm) {

    struct dart_client *dc = (struct dart_client *)malloc(sizeof(struct dart_client));
    if (dc == NULL) {
        printf("[%s]: allocate DART client failed!\n", __func__);
        goto err_out;
    }

    memset(dc, 0, sizeof(*dc));
    dc->dart_ref = dart_ref;

    if(comm) {
        dc->comm = malloc(sizeof(*dc->comm));
        MPI_Comm_dup(*(MPI_Comm *)comm, dc->comm);
    } else {
        dc->comm = NULL;
    }
    dc->cp_in_job = num_peers;

    char *interface = getenv("DATASPACES_TCP_INTERFACE");
    dc->rpc_s = rpc_server_init(interface, dc->cp_in_job, dc, DART_CLIENT);
    dc->rpc_s->ptlmap.appid = appid;
    if (dc->rpc_s == NULL) {
        printf("[%s]: initialize RPC server failed!\n", __func__);
        goto err_out;
    }

    dc->rpc_s->thread_alive = 1;
    if (pthread_create(&dc->rpc_s->comm_thread, NULL, dc_listen, (void *)dc) != 0) {
        printf("[%s]: create pthread failed!\n", __func__);
        goto err_out;
    }

    rpc_add_service(cn_register, rpc_handler_cn_register);

    if (dc_register_at_master(dc, appid) < 0) {
        printf("[%s]: register DART client failed!\n", __func__);
        goto err_out;
    }
    return dc;

    err_out:
    if (dc != NULL) {
        if (dc->rpc_s != NULL) {
            rpc_server_free(dc->rpc_s);
        }
        free(dc);
    }
    return NULL;
}

int dc_process(struct dart_client *dc) {
    return rpc_process_event(dc->rpc_s);
}

static int dc_unregister(struct dart_client *dc) {
    struct msg_buf *msg = NULL;

    int sp_index = dc->self->ptlmap.id % dc->num_sp;
    struct node_id *peer = &dc->peer_tab[sp_index];

    msg = msg_buf_alloc(dc->rpc_s, peer, 1);
    if (msg == NULL) {
        printf("[%s]: allocate message failed!\n", __func__);
        goto err_out;
    }
    msg->msg_rpc->cmd = cn_unregister;
    struct payload_unregistration_info *info = (struct payload_unregistration_info *)msg->msg_rpc->pad;
    info->num_cp = 1;

    if (rpc_send(dc->rpc_s, peer, msg) < 0) {
        printf("[%s]: send unregistration info to server %d failed!\n", __func__, peer->ptlmap.id);
        msg = NULL;
        goto err_out;
    }
    return 0;

    err_out:
    if (msg != NULL) {
        free(msg);
    }
    return -1;
}

void dc_free(struct dart_client *dc) {
    dc_unregister(dc);

    dc->rpc_s->thread_alive = 0;
    pthread_cancel(dc->rpc_s->comm_thread);

    if (rpc_server_free(dc->rpc_s) < 0) {
        printf("[%s]: free RPC server for peer %d (client) failed, skip!\n", __func__, dc->self->ptlmap.id);
    }

    if(dc->peer_tab != NULL) {
        free(dc->peer_tab);
    }

    if(dc->comm) {
        free(dc->comm);
    }

    free(dc);
}
