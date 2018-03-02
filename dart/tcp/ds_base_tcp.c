#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include "ds_base_tcp.h"
#include "debug.h"

static struct app_info *app_alloc() {
    struct app_info *app = (struct app_info *)malloc(sizeof(struct app_info));
    if (app != NULL) {
        memset(app, 0, sizeof(*app));
    }
    return app;
}

static struct app_info *app_find(struct dart_server *ds, int app_id) {
    struct app_info *app;
    list_for_each_entry(app, &ds->app_list, struct app_info, app_entry) {
        if (app->app_id == app_id) {
            return app;
        }
    }
    return NULL;
}

static int rpc_handler_cn_unregister(struct rpc_server *rpc_s, struct rpc_cmd *cmd) {
    struct dart_server *ds = ds_ref_from_rpc(rpc_s);
    struct msg_buf *msg = NULL;

    if (ds->f_stop) {
        printf("[%s]: server received unregistration request, but server had already stopped, skip!\n", __func__);
        return 0;
    }

    static int num_cn_unreg = 0;

    struct payload_unregistration_info *info = (struct payload_unregistration_info *)cmd->pad;

    num_cn_unreg += info->num_cp;
    if (num_cn_unreg == ds->size_cp) {
        ds->f_stop = 1;
    }

    if (cmd->id >= ds->size_sp && --ds->num_charge == 0) {
        int i;
        for (i = 0; i < ds->size_sp; ++i) {
            if (ds->self->ptlmap.id == i) {
                continue;
            }
            struct node_id *peer = &ds->peer_tab[i];
            info->num_cp = ds->num_charge_cp;

            msg = msg_buf_alloc(ds->rpc_s, peer, 1);
            if (msg == NULL) {
                printf("[%s]: allocate message failed!\n", __func__);
                goto err_out;
            }
            memcpy(msg->msg_rpc, cmd, sizeof(*cmd));

            if (rpc_send(ds->rpc_s, peer, msg) < 0) {
                printf("[%s]: send unregistration info to server %d failed!\n", __func__, peer->ptlmap.id);
                msg = NULL;
                goto err_out;
            }
            msg = NULL;
        }
    }
    return 0;

    err_out:
    if (msg != NULL) {
        free(msg);
    }
    return -1;
}

static int ds_register_cp(struct dart_server *ds) {
     //uloga("%s(Yubo) I am here ds_register_cp\n", __func__);
    struct msg_buf *msg = NULL;

    int i;
    for (i = ds->self->ptlmap.id; i < ds->num_cp; i += ds->size_sp) {
        struct node_id *peer = &ds->cn_peers[i];
        //ulog("[%s]: server %d will send ptlid_map data of all peers to client %d.\n", __func__,
        //   ds->self->ptlmap.id, peer->ptlmap.id);
        msg = msg_buf_alloc(ds->rpc_s, peer, 1);
        if (msg == NULL) {
            printf("[%s]: allocate message failed!\n", __func__);
            goto err_out;
        }
        msg->size = sizeof(struct ptlid_map) * (size_t)ds->peer_size;
        msg->msg_data = malloc(msg->size);
        if (msg->msg_data == NULL) {
            printf("[%s]: allocate ptlid_map data failed!\n", __func__);
            goto err_out;
        }

        struct ptlid_map *p = (struct ptlid_map *)msg->msg_data;
        int j;
        for (j = 0; j < ds->peer_size; ++j) {
            p[j] = ds->peer_tab[j].ptlmap;
        }

        msg->msg_rpc->cmd = cn_register;

        struct payload_client_registration_reply *pl = (struct payload_client_registration_reply *)msg->msg_rpc->pad;
        pl->id_sp = ds->self->ptlmap.id;
        pl->id_cp = peer->ptlmap.id;
        pl->size_sp = ds->size_sp;
        pl->size_cp = ds->size_cp;
        pl->id_cp_min = -1;
        struct app_info *app;
        list_for_each_entry(app, &ds->app_list, struct app_info, app_entry) {
            if (app->app_id == peer->ptlmap.appid) {
                pl->id_cp_min = app->app_peer_tab[0].ptlmap.id;
            }
        }
        if (pl->id_cp_min == -1) {
            goto err_out;
        }

        if (rpc_send(ds->rpc_s, peer, msg) < 0) {
            printf("[%s]: send ptlid_map data of all peers to client %d failed!\n", __func__, peer->ptlmap.id);
            msg = NULL;
            goto err_out;
        }
        msg = NULL;
    }

    ds_barrier(ds);
    return 0;

    err_out:
    if (msg != NULL) {
        if (msg->msg_data != NULL) {
            free(msg->msg_data);
        }
        free(msg);
    }
    return -1;
}

int rpc_cb_recv_sp_announce_cp(struct rpc_server *rpc_s, struct msg_buf *msg) {
    struct dart_server *ds = ds_ref_from_rpc(rpc_s);
    struct app_info *app = msg->private;
    struct ptlid_map *p = (struct ptlid_map *)msg->msg_data;

    int i;
    for (i = 0; i < app->app_num_peers; ++i) {
        struct node_id *peer = &app->app_peer_tab[i];
        peer->ptlmap = p[i];
        // printf("[%s]: Server %d received ptlid_map data of client %d (%s:%d).\n", __func__, rpc_s->ptlmap.id,
        //     peer->ptlmap.id, inet_ntoa(peer->ptlmap.address.sin_addr), (int)ntohs(peer->ptlmap.address.sin_port));
    }

    if (ds->num_cp == ds->size_cp) {
        ds->f_reg = 1;
    }

    free(msg->msg_data);
    free(msg);
    return 0;
}

int rpc_handler_sp_announce_cp(struct rpc_server *rpc_s, struct rpc_cmd *cmd) {
    struct dart_server *ds = ds_ref_from_rpc(rpc_s);
    struct payload_app_info *info = (struct payload_app_info *)cmd->pad;

    struct app_info *app = app_alloc();
    if (app == NULL) {
        printf("[%s]: allocate new application failed!\n", __func__);
        goto err_out;
    }
    app->app_id = info->app_id;
    app->app_num_peers = info->app_size;
    app->app_cnt_peers = info->app_size;
    app->app_peer_tab = ds->peer_tab + info->id;
    app->cnt_registered_peers = info->app_size;
    list_add(&app->app_entry, &ds->app_list);

    ds->num_cp += info->app_size;

    struct node_id *peer = &ds->peer_tab[cmd->id];
    struct msg_buf *msg = msg_buf_alloc(rpc_s, peer, 0);
    if (msg == NULL) {
        printf("[%s]: allocate message failed!\n", __func__);
        goto err_out;
    }
    msg->size = sizeof(struct ptlid_map) * (size_t)info->app_size;
    msg->msg_data = malloc(msg->size);
    if (msg->msg_data == NULL) {
        printf("[%s]: allocate ptlid_map data failed!\n", __func__);
        goto err_out;
    }
    msg->private = app;
    msg->cb = rpc_cb_recv_sp_announce_cp;

    if (rpc_receive_direct(rpc_s, peer, msg) < 0) {
        printf("[%s]: recv ptlid_map data for application %d from master server failed!\n", __func__, info->app_id);
        goto err_out;
    }
    msg = NULL;
    return 0;

    err_out:
    /* TODO: may double free msg */
    if (msg != NULL) {
        if (msg->msg_data != NULL) {
            free(msg->msg_data);
        }
        free(msg);
    }
    return -1;
}

int ds_announce_cp(struct dart_server *ds, struct app_info *app) {
    struct msg_buf *msg = NULL;

    int i;
    for (i = 1; i < ds->size_sp; ++i) {
        struct node_id *peer = &ds->peer_tab[i];
        msg = msg_buf_alloc(ds->rpc_s, peer, 1);
        if (msg == NULL) {
            printf("[%s]: allocate message failed!\n", __func__);
            goto err_out;
        }
        msg->size = sizeof(struct ptlid_map) * (size_t)app->app_num_peers;
        msg->msg_data = malloc(msg->size);
        if (msg->msg_data == NULL) {
            printf("[%s]: allocate ptlid_map data failed!\n", __func__);
            goto err_out;
        }

        struct ptlid_map *p = (struct ptlid_map *)msg->msg_data;
        int j;
        for (j = 0; j < app->app_num_peers; ++j) {
            p[j] = app->app_peer_tab[j].ptlmap;
        }

        msg->msg_rpc->cmd = sp_announce_cp;
        struct payload_app_info *info = (struct payload_app_info *)msg->msg_rpc->pad;
        info->id = p[0].id;
        info->app_id = app->app_id;
        info->app_size = app->app_num_peers;

        if (rpc_send(ds->rpc_s, peer, msg) < 0) {
            printf("[%s]: send ptlid_map data for application %d to slave server %d failed!\n", __func__,
                app->app_id, peer->ptlmap.id);
            msg = NULL;
            goto err_out;
        }
        msg = NULL;
    }

    return 0;

    err_out:
    if (msg != NULL) {
        if (msg->msg_data != NULL) {
            free(msg->msg_data);
        }
        free(msg);
    }
    return -1;
}

int rpc_handler_cn_register(struct rpc_server *rpc_s, struct rpc_cmd *cmd) {
    
    struct dart_server *ds = ds_ref_from_rpc(rpc_s);

    struct ptlid_map *ptlmap = (struct ptlid_map *)cmd->pad;
    // printf("[%s]: client registration request from client %d (%s:%d) received.\n", __func__,
    //     (int)cmd->id, inet_ntoa(ptlmap->address.sin_addr), (int)ntohs(ptlmap->address.sin_port));

    struct node_id *peer = &ds->peer_tab[cmd->id];
    peer->ptlmap.appid = ptlmap->appid;
    peer->ptlmap.address = ptlmap->address;

    struct app_info *app = app_find(ds, peer->ptlmap.appid);
    if (app == NULL) {
        printf("[%s]: find application %d failed!\n", __func__, peer->ptlmap.appid);
        return 0;
    }

    if (++app->cnt_registered_peers != app->app_num_peers) {
        return 0;
    }

    if (ds_announce_cp(ds, app) < 0) {
        printf("[%s]: announce client data for application %d failed!\n", __func__, peer->ptlmap.appid);
        goto err_out;
    }

    if (ds->num_cp == ds->size_cp) {
        list_for_each_entry(app, &ds->app_list, struct app_info, app_entry) {
            if (app->cnt_registered_peers != app->app_num_peers) {
                return 0;
            }
        }
        ds->f_reg = 1;
    }

    return 0;

    err_out:
    return -1;
}

int rpc_cb_recv_sp_reg_reply(struct rpc_server *rpc_s, struct msg_buf *msg) {
    struct dart_server *ds = ds_ref_from_rpc(rpc_s);
    struct ptlid_map *p = (struct ptlid_map *)msg->msg_data;

    int i;
    for (i = 0; i < ds->size_sp; ++i) {
        struct node_id *peer = &ds->peer_tab[i];
        peer->ptlmap = p[i];
        // printf("[%s]: Server %d received ptlid_map data of server %d (%s:%d).\n", __func__, rpc_s->ptlmap.id,
        //     peer->ptlmap.id, inet_ntoa(peer->ptlmap.address.sin_addr), (int)ntohs(peer->ptlmap.address.sin_port));
    }

    ds_barrier(ds);
    for (i = 0; i < ds->size_sp; ++i) {
        struct node_id *peer = &ds->peer_tab[i];
        /* This is a simple way to avoid connection corruption,
            i.e. it's bad when two servers want to connect to each other simultaneously */
        if (i != 0 && i < rpc_s->ptlmap.id) {
            rpc_connect(rpc_s, peer);
        }
    }

    free(msg->msg_data);
    free(msg);
    return 0;
}

int rpc_handler_sp_reg_reply(struct rpc_server *rpc_s, struct rpc_cmd *cmd) {
    struct dart_server *ds = ds_ref_from_rpc(rpc_s);
    struct ptlid_map *p = (struct ptlid_map *)cmd->pad;

    rpc_s->ptlmap.id = p->id;
    ulog("[%s]: Now I know I'm server %d.\n", __func__, rpc_s->ptlmap.id);

    ds->self = &ds->peer_tab[p->id];
    ds->num_sp = ds->size_sp;

    struct node_id *peer = &ds->peer_tab[cmd->id];
    struct msg_buf *msg = msg_buf_alloc(rpc_s, peer, 0);
    if (msg == NULL) {
        printf("[%s]: allocate message failed!\n", __func__);
        goto err_out;
    }
    msg->size = sizeof(struct ptlid_map) * (size_t)ds->size_sp;
    msg->msg_data = malloc(msg->size);
    if (msg->msg_data == NULL) {
        printf("[%s]: allocate ptlid_map data failed!\n", __func__);
        goto err_out;
    }
    msg->cb = rpc_cb_recv_sp_reg_reply;

    if (rpc_receive_direct(rpc_s, peer, msg) < 0) {
        printf("[%s]: recv ptlid_map data from master server failed!\n", __func__);
        goto err_out;
    }
    msg = NULL;
    return 0;

    err_out:
    /* TODO: may double free msg */
    if (msg != NULL) {
        if (msg->msg_data != NULL) {
            free(msg->msg_data);
        }
        free(msg);
    }
    return -1;
}

int rpc_handler_sp_reg_request(struct rpc_server *rpc_s, struct rpc_cmd *cmd) {
    /* Store the number of the registered servers */
    static int num_sp_reg = 1;

    struct dart_server *ds = ds_ref_from_rpc(rpc_s);

    struct ptlid_map *ptlmap = (struct ptlid_map *)cmd->pad;
    // printf("[%s]: server registration request from server %d (%s:%d) received.\n", __func__,
    //     (int)cmd->id, inet_ntoa(ptlmap->address.sin_addr), (int)ntohs(ptlmap->address.sin_port));

    struct node_id *peer = &ds->peer_tab[cmd->id];
    peer->ptlmap.appid = ptlmap->appid;
    peer->ptlmap.address = ptlmap->address;

    if (++num_sp_reg != ds->size_sp) {
        return 0;
    }

    struct msg_buf *msg = NULL;
    int i;
    ulog("[%s]: all servers are registered.\n", __func__);
    for (i = 0; i < ds->size_sp; ++i) {
        struct node_id *peer = &ds->peer_tab[i];
        ulog("[%s]: server %d is (%s:%d).\n", __func__, peer->ptlmap.id,
            inet_ntoa(peer->ptlmap.address.sin_addr), (int)ntohs(peer->ptlmap.address.sin_port));
    }
    for (i = 1; i < ds->size_sp; ++i) {
        struct node_id *peer = &ds->peer_tab[i];
        msg = msg_buf_alloc(rpc_s, peer, 1);
        if (msg == NULL) {
            printf("[%s]: allocate message failed!\n", __func__);
            goto err_out;
        }
        msg->size = sizeof(struct ptlid_map) * (size_t)ds->size_sp;
        msg->msg_data = malloc(msg->size);
        if (msg->msg_data == NULL) {
            printf("[%s]: allocate ptlid_map data failed!\n", __func__);
            goto err_out;
        }

        struct ptlid_map *p = (struct ptlid_map *)msg->msg_data;
        int j;
        for (j = 0; j < ds->size_sp; ++j) {
            p[j] = ds->peer_tab[j].ptlmap;
        }

        msg->msg_rpc->cmd = sp_reg_reply;
        *(struct ptlid_map *)msg->msg_rpc->pad = peer->ptlmap;

        if (rpc_send(rpc_s, peer, msg) < 0) {
            printf("[%s]: send ptlid_map data to slave server %d failed!\n", __func__, peer->ptlmap.id);
            msg = NULL;
            goto err_out;
        }
        msg = NULL;
    }
    /* Corresponding to ds_barrier in rpc_cb_recv_sp_reg_reply */
    ds_barrier(ds);

    return 0;

    err_out:
    if (msg != NULL) {
        if (msg->msg_data != NULL) {
            free(msg->msg_data);
        }
        free(msg);
    }
    return -1;
}

static void *ds_listen(void *server) {
    struct dart_server *ds = (struct dart_server *)server;

    while (ds->rpc_s->thread_alive) {
        int sockfd_c = accept(ds->rpc_s->sockfd_s, NULL, NULL);
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
        if (info.id == -1) {
            /* Still in registration phase, it should be the master server */
            if (info.cmp_type == DART_SERVER) {
                peer = &ds->peer_tab[ds->num_sp];
                /* Other information of the peer will be filled in registration callback function */
                peer->ptlmap.id = ds->num_sp;
                ++ds->num_sp;
                ulog("[%s]: server %d accepts connection from server %d.\n", __func__,
                    ds->self->ptlmap.id, peer->ptlmap.id);
            } else {
                struct app_info *app = app_find(ds, info.app_id);
                if (app == NULL) {
                    app = app_alloc();
                    if (app == NULL) {
                        printf("[%s]: allocate new application failed, skip!\n", __func__);
                        continue;
                    }
                    app->app_id = info.app_id;
                    app->app_num_peers = info.app_size;
                    app->app_peer_tab = ds->cn_peers + ds->num_cp;
                    ds->num_cp += info.app_size;
                    list_add(&app->app_entry, &ds->app_list);
                }
                peer = app->app_peer_tab + app->app_cnt_peers;
                ++app->app_cnt_peers;
                /* Other information of the peer will be filled in registration callback function */
                peer->ptlmap.id = (int)(peer - ds->peer_tab);
                ulog("[%s]: server %d accepts connection from client %d (app %d).\n", __func__,
                    ds->self->ptlmap.id, peer->ptlmap.id, app->app_id);
            }
        } else {
            /* Not in registration phase, should come from a data query */
            peer = &ds->peer_tab[info.id];
            ulog("[%s]: server %d accepts connection from peer %d.\n", __func__, ds->self->ptlmap.id, peer->ptlmap.id);
            if (peer->f_connected) {
                printf("[%s]: accept connection from peer %d, but it has been connected, skip!\n", __func__, info.id);
                close(sockfd_c);
                continue;
            }
        }
        peer->sockfd = sockfd_c;
        peer->f_connected = 1;
    }
}

static int file_lock(int fd, int op) {
    if(op) {
        /* Lock file */
        while (lockf(fd, F_TLOCK, (off_t)1) != 0) {
            /* Will it be better to sleep for a while? */
        }
        return 0;
    } else {
        /* Unlock file */
        return lockf(fd, F_ULOCK, (off_t) 1);
    }
}

int ds_boot_master(struct dart_server *ds) {
    ds->rpc_s->ptlmap.id = 0;
    ds->self = &ds->peer_tab[0];
    ds->self->ptlmap = ds->rpc_s->ptlmap;
    while (!ds->f_reg) {
        rpc_process_event(ds->rpc_s);
    }
    int i;
    for (i = 0; i < ds->peer_size; ++i) {
        struct node_id *peer = &ds->peer_tab[i];
        ulog("[%s]: master server knows peer %d is (%s:%d).\n", __func__, peer->ptlmap.id,
            inet_ntoa(peer->ptlmap.address.sin_addr), (int)ntohs(peer->ptlmap.address.sin_port));
    }
    return 0;
}

int ds_boot_slave(struct dart_server *ds) {
    struct node_id *peer_master = &ds->peer_tab[0];
    peer_master->ptlmap.id = 0;
    peer_master->ptlmap.appid = 0;
    if (rpc_connect(ds->rpc_s, peer_master) < 0) {
        printf("[%s]: connect to master server failed!\n", __func__);
        goto err_out;
    }

    struct msg_buf *msg = msg_buf_alloc(ds->rpc_s, peer_master, 1);
    if (msg == NULL) {
        printf("[%s]: allocate message failed!\n", __func__);
        goto err_out;
    }
    msg->msg_rpc->cmd = sp_reg_request;
    *(struct ptlid_map *)msg->msg_rpc->pad = ds->rpc_s->ptlmap;
    if (rpc_send(ds->rpc_s, peer_master, msg) < 0) {
        printf("[%s]: send ptlid_map data to master server failed!\n", __func__);
        msg = NULL;
        goto err_out;
    }
    msg = NULL;

    while (!ds->f_reg) {
        rpc_process_event(ds->rpc_s);
    }
    int i;
    for (i = 0; i < ds->peer_size; ++i) {
        struct node_id *peer = &ds->peer_tab[i];
        ulog("[%s]: server %d knows peer %d is (%s:%d).\n", __func__, ds->self->ptlmap.id, peer->ptlmap.id,
            inet_ntoa(peer->ptlmap.address.sin_addr), (int)ntohs(peer->ptlmap.address.sin_port));
    }
    return 0;

    err_out:
    if (msg != NULL) {
        free(msg);
    }
    return -1;
}

int thread_boot(struct dart_server *ds){
        //create thread_handle
    
    if (pthread_create(&ds->rpc_s->task_thread, NULL, thread_handle_new, (void*)ds->rpc_s) != 0){
        printf("[%s]: create pthread_handle failed!\n", __func__);
        return -1;
    }
    //uloga("%s(Yubo), create thread_handle, id=%lu, the rpc_s->ptlmap.id=%d\n", __func__,ds->rpc_s->task_thread, ds->rpc_s->ptlmap.id);
    return 0;
}


static int ds_boot(struct dart_server *ds)
{

	const char *filename_lock = "srv.lck";
	const char *filename_conf = "conf";
	int fd;
	int is_master = 0;
	int rank;
	struct stat stat_buf;
	int ret;

    ds->rpc_s->thread_alive = 1;
    if (pthread_create(&ds->rpc_s->comm_thread, NULL, ds_listen, (void *)ds) != 0) {
        printf("[%s]: create pthread failed!\n", __func__);
        goto err_out;
    }
    if(ds->comm) {
    	MPI_Comm_rank(*ds->comm, &rank);
    	if(rank == 0) {
    		is_master = 1;
    	}
    } else {
    	uloga("No MPI Comm\n");
    	fd = open(filename_lock, O_WRONLY | O_CREAT, 0644);
    	if (fd < 0) {
    		printf("[%s]: open file %s failed!\n", __func__, filename_lock);
    		goto err_out;
    	}
    	
    	file_lock(fd, 1);

    	ret = stat(filename_conf, &stat_buf);
    	if (ret < 0 && errno != ENOENT) {
    		goto err_out;
    	} else if(stat_buf.st_size == 0) {
    		is_master = 1;
    	}
    }

    if (is_master) {
        if (rpc_write_config(ds->rpc_s, filename_conf) < 0) {
            printf("[%s]: write RPC config file failed!\n", __func__);
            goto err_out;
        }
        if(!ds->comm) {
        	file_lock(fd, 0);
        } else {
        	MPI_Barrier(*ds->comm);
        }
        if (ds_boot_master(ds) < 0) {
            printf("[%s]: boot master server failed!\n", __func__);
            goto err_out;
        }
    } else {
    	if(!ds->comm) {
    		file_lock(fd, 0);
    	} else {
    		MPI_Barrier(*ds->comm);
    	}
        if (rpc_read_config(&ds->peer_tab[0].ptlmap.address, filename_conf) < 0) {
            printf("[%s]: read RPC config file failed!\n", __func__);
            goto err_out;
        }
        if (ds_boot_slave(ds) < 0) {
            printf("[%s]: boot slave server failed!\n", __func__);
            goto err_out;
        }
    }
    if(is_master && !ds->comm) {
    	close(fd);
    	remove(filename_lock);
    }

    return 0;

err_out:
    if (fd >= 0 && !ds->comm) {
        file_lock(fd, 0);
        close(fd);
    }
    return -1;
}




struct dart_server *ds_alloc(int num_sp, int num_cp, void *dart_ref, void *comm) {
    size_t size = sizeof(struct dart_server) + sizeof(struct node_id) * (size_t)(num_sp + num_cp);
    struct dart_server *ds = (struct dart_server *)malloc(size);
    //struct queue tasks_queue;

    //queue_init(&tasks_queue);
    //uloga("%s(YUbo) server queue_init\n", __func__);

    if (ds == NULL) {
        printf("[%s]: allocate DART server failed!\n", __func__);
        goto err_out;
    }

    memset(ds, 0, size);
    ds->peer_size = num_sp + num_cp;
    ds->peer_tab = (struct node_id *)(ds + 1);
    ds->cn_peers = ds->peer_tab + num_sp;
    ds->num_cp = 0;
    ds->num_sp = 1;
    ds->size_cp = num_cp;
    ds->size_sp = num_sp;
    ds->dart_ref = dart_ref;
    INIT_LIST_HEAD(&ds->app_list);

    if(comm) {
        ds->comm = malloc(sizeof(*ds->comm));
        MPI_Comm_dup(*(MPI_Comm *)comm, ds->comm);
    } else {
        ds->comm = NULL;    
    }

    char *interface = getenv("DATASPACES_TCP_INTERFACE");
    ds->rpc_s = rpc_server_init(interface, ds->size_sp, ds, DART_SERVER);
    if (ds->rpc_s == NULL) {
        printf("[%s]: initialize RPC server failed!\n", __func__);
        goto err_out;
    }
    rpc_server_set_peer_ref(ds->rpc_s, ds->peer_tab, ds->peer_size);

    int i;
    for (i = 0; i < ds->peer_size; ++i) {
        struct node_id *peer = &ds->peer_tab[i];
        INIT_LIST_HEAD(&peer->req_list);
        /* TODO: what are the following two lines of code for? */
        // peer->num_msg_at_peer = ds->rpc_s->max_num_msg;
        // peer->num_msg_ret = 0;
    }

    rpc_add_service(sp_reg_request, rpc_handler_sp_reg_request);
    rpc_add_service(sp_reg_reply, rpc_handler_sp_reg_reply);
    rpc_add_service(cn_register, rpc_handler_cn_register);
    rpc_add_service(sp_announce_cp, rpc_handler_sp_announce_cp);
    rpc_add_service(cn_unregister, rpc_handler_cn_unregister);

    //uloga("%s(Yubo), #1 before create thread_handle, the rpc_s->ptlmap.id=%d\n", __func__, ds->rpc_s->ptlmap.id);
    


    if (ds_boot(ds) < 0) {
        printf("[%s]: boot DART server failed!\n", __func__);
        goto err_out;
    }
    ds_register_cp(ds);

    //uloga("%s(Yubo), #2 before create thread_handle, the rpc_s->ptlmap.id=%d\n", __func__, ds->rpc_s->ptlmap.id);
    thread_boot(ds);

    int id = ds->self->ptlmap.id;
    ds->num_charge_cp = (num_cp - 1 - id + num_sp) / num_sp;
    ds->num_charge = ds->num_charge_cp;

    

    //uloga("%s(Yubo), #3 before create thread_handle, the rpc_s->ptlmap.id=%d\n", __func__, ds->rpc_s->ptlmap.id);
    

    return ds;

    err_out:
    if (ds != NULL) {
        free(ds);
    }
    return NULL;
}

void ds_free(struct dart_server* ds) {
    //int ds_id = ds->rpc_s->ptlmap.id;
    int i=0;
    ds->rpc_s->thread_alive = 0;
    pthread_cancel(ds->rpc_s->comm_thread);
    pthread_join(ds->rpc_s->comm_thread, NULL);
    
    //Finalize worker threads
    finalize_threads(ds->rpc_s);

    if (rpc_server_free(ds->rpc_s) < 0) {
        printf("[%s]: free RPC server for peer %d (server) failed, skip!\n", __func__, ds->self->ptlmap.id);
    }

    struct app_info *app, *t;
    list_for_each_entry_safe(app, t, &ds->app_list, struct app_info, app_entry) {
        //uloga("%s(Yubo) call list_del\n",__func__);
        list_del(&app->app_entry);
        free(app);
    }

    if(ds->comm) {
        free(ds->comm);
    }

    /*
    //Print content in tasks_list for debuging purpose
    struct tasks_request *tmp_tr;
    list_for_each_entry(tmp_tr, &ds->rpc_s->tasks_list, struct tasks_request, tasks_entry)
    {
        uloga("%s(Yubo) server %d has rpc_cmd=%d\n",__func__,ds_id, tmp_tr->cmd->cmd);
    }
    */

    
    free(ds);
}

int ds_process(struct dart_server* ds) {
    return rpc_process_event_mt(ds->rpc_s);
}
