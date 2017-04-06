#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>

#include <sys/stat.h>

#include "ds_base_pami.h"
#include "debug.h"
#include "list.h"

static int ds_register_cp(struct dart_server *ds, struct app_info *app);

static int obj_put_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	rpc_s->flag--;
	printf("all the data has been acquited by the server\n");
}

static int cn_read_transfer_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
        struct dart_server *ds = ds_ref_from_rpc(rpc_s);

        if(msg && msg->msg_data){
                free(msg->msg_data);
                free(msg);
        }

	//uloga("all the data has been send to the remote node\n");
        return 0;
}

static struct app_info *app_alloc()
{
        struct app_info *app = 0;

        app = malloc(sizeof(*app));
        if (!app)
                return 0;
        memset(app, 0, sizeof(*app));

        return app;
}


static struct app_info *app_find(struct dart_server *ds, int appid)
{
        struct app_info *app;

        list_for_each_entry(app, &ds->app_list, struct app_info, app_entry) {
                if (app->app_id == appid)
                        return app;
        }

        return NULL;
}

static int announce_cp_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
        struct dart_server *ds = ds_ref_from_rpc(rpc_s);
        struct app_info *app = msg->private;
        struct ptlid_map *pptlmap = msg->msg_data;
        struct node_id *peer;
        int i;

        peer = app->app_peer_tab;
        //Copy the received data
        for (i = 0; i < app->app_num_peers; i++) {
                (peer++)->ptlmap = *pptlmap++;
        }
        free(msg->msg_data);
        free(msg);

        if(ds->num_cp == ds->size_cp){
                ds_register_cp(ds, app);}

	return 0;
}

/*
 *RPC routine to service compute peer joins "sp_announce_cp"
 */

static int dsrpc_announce_cp(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	//printf("get into dsrpc_announce_cp\n");
        struct dart_server *ds = ds_ref_from_rpc(rpc_s);
        struct hdr_register *hreg = (struct hdr_register *)cmd->pad;
        struct node_id *peer;
        struct msg_buf *msg;
        struct app_info *app;
        int err = -ENOMEM;

        app = app_alloc();//new app obj
        if (!app)
                goto err_out;

        app->app_id = hreg->pm_cp.appid;
        app->app_num_peers = hreg->num_cp;
        app->app_cnt_peers = hreg->num_cp;
        app->app_peer_tab = ds->peer_tab + hreg->pm_cp.id;

        ds->num_cp += hreg->num_cp;
        list_add(&app->app_entry, &ds->app_list);//add app obj into list

        peer = ds_get_peer(ds, cmd->id);
        msg = msg_buf_alloc(rpc_s, peer, 0);
        if (!msg)
                goto err_out;

        msg->size = cmd->mem_size;
        msg->msg_data = malloc(msg->size);
        if (!msg->msg_data) {
                free(msg);
                goto err_out;
        }
        msg->cb = announce_cp_completion;
        msg->private = app;

        rpc_mem_info_cache(peer, msg, cmd);	//TODO
        err = rpc_receive_direct(rpc_s, peer, msg);
        if(err<0){
                free(msg->msg_data);
                goto err_out_free;
        }

        return 0;
err_out_free:
        if(msg)
                free(msg);
err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;

}

/*
 *   Propagate compute peer information to other server peers in the space.
 */
static int ds_announce_cp(struct dart_server *ds, struct app_info *app)
{
	//printf("get into ds_announce_cp\n");
        struct msg_buf *msg;
        struct hdr_register *hreg;
        struct node_id *peer, *cpeer;
        struct ptlid_map *pptlmap;
        int i, k, err;

        cpeer = app->app_peer_tab;
        //cpeer = ds->cn_peers;

        for(i = 1; i< ds->num_sp; i++){
                peer = ds_get_peer(ds, i);

                err = -ENOMEM;
                msg = msg_buf_alloc(ds->rpc_s, peer, 1);
                if (!msg)
                        goto err_out;

                msg->cb = default_completion_with_data_callback;

                msg->size = sizeof(*pptlmap) * app->app_num_peers;
                //msg->size = sizeof(*pptlmap) * ds->size_cp ;

                pptlmap = msg->msg_data = calloc(1, msg->size);
                if(!msg->msg_data)
                        goto err_out_free;

                for(k=0; k<app->app_num_peers; k++){
                //for(k=0; k<ds->size_cp; k++){
                        *pptlmap++ = (cpeer++)->ptlmap;
                }

                cpeer = app->app_peer_tab;
                //cpeer = ds->cn_peers;         
                msg->msg_rpc->cmd = sp_announce_cp;
                msg->msg_rpc->id = ds->self->ptlmap.id;

                hreg = (struct hdr_register *) msg->msg_rpc->pad;
                hreg->pm_cp = cpeer->ptlmap;

                hreg->num_cp = app->app_num_peers;
                //hreg->num_cp = ds->size_cp;

		//printf("ds_announce_cp send to %d\n", peer->ptlmap.rank_pami);
                err = rpc_send(ds->rpc_s, peer, msg);
                if(err<0){
                        free(msg->msg_data);
                        goto err_out_free;
                }
        }
        return 0;
err_out_free:
        if(msg)
                free(msg);
err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}

static int ds_register_cp(struct dart_server *ds, struct app_info *app)
{
	//uloga("get into ds_register_cp\n");
        struct hdr_register *hreg;
        struct msg_buf *msg;
        struct node_id *peer;
        struct ptlid_map *pptlmap;
        int err, i, k, id_min;

        i = ds->self->ptlmap.id;

        //while(i < app->app_num_peers){
        //peer = app->app_peer_tab + i;
        while(i < ds->num_cp){
                peer = ds->cn_peers + i;

                err = -ENOMEM;

                //Construct reply msg for remote computer node
                msg = msg_buf_alloc(ds->rpc_s, peer, 1);
                if(!msg)
                        break;

                msg->msg_rpc->cmd = cn_register;
                msg->msg_rpc->id = ds->self->ptlmap.id;

                msg->cb = default_completion_with_data_callback;
                msg->size = sizeof(*pptlmap) * ds->peer_size;

                pptlmap = msg->msg_data = malloc(msg->size);
                if(!msg->msg_data){
                        free(msg);
                        break;
                }

                //Copy the data
                for(k=0; k<ds->peer_size; k++)
                        *pptlmap++ = ds->peer_tab[k].ptlmap;
                        hreg = (struct hdr_register *) msg->msg_rpc->pad;
                        hreg->pm_cp = peer->ptlmap;
                        hreg->pm_sp = ds->self->ptlmap;
                        hreg->roff = 0;
                        hreg->maxsize = 4 * 10124 * 1024;
                        hreg->num_sp = ds->size_sp;
                        hreg->num_cp = ds->size_cp;

                        //hreg->id_min = id_min;
                        list_for_each_entry(app, &ds->app_list, struct app_info, app_entry){
                                if(app->app_id == peer->ptlmap.appid) { //find which app this dc belongs to
                                        hreg->id_min = app->app_peer_tab[0].ptlmap.id;
                                        //break;
                                }
                        }

                        err = rpc_send(ds->rpc_s, peer, msg);

                        if(err < 0){
                                free(msg->msg_data);
                                free(msg);
                                break;
                        }

                        //Step forward with a stride as ds->num_sp
                        i = i + ds->num_sp;
        }

        //if(i < app->app_num_peers)
        if(i < ds->num_cp)
                goto err_out;
        return 0;
err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;

}

/*
 *RPC routine to serve a compute node registration request
 */
static int dsrpc_cn_register(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	//uloga("get into dsrpc_cn_register\n");
        struct dart_server *ds = ds_ref_from_rpc(rpc_s);
        struct node_id *cn_peer;
        struct hdr_register *hreg = (struct hdr_register *) cmd->pad;
        struct app_info *app;
        int err = -ENOMEM;

        app = app_find(ds, hreg->pm_cp.appid);
        if(!app){
                //Create new app obj
                app = app_alloc();
                if(!app)
                        goto err_out;

                        app->app_id = hreg->pm_cp.appid;
                        app->app_num_peers = hreg->num_cp;
                        app->app_peer_tab = ds->cn_peers + ds->num_cp;

                        /* Reserve # of peers for the new application. */
                        ds->num_cp += hreg->num_cp;

                        list_add(&app->app_entry, &ds->app_list);
        }

        cn_peer = app->app_peer_tab + app->app_cnt_peers;
        cn_peer->ptlmap = hreg->pm_cp;//Important copy
        cn_peer->ptlmap.id = cn_peer - ds->peer_tab; //global dart id?

//        cn_peer->num_msg_ret++;
//        ds->self->num_msg_ret--;

        /* One more node for this application has joined. */
        app->app_cnt_peers++;

        /* Wait for all of the peers to join in. */
        if (app->app_cnt_peers != app->app_num_peers)
                return 0;
#ifdef DEBUG
        uloga("%s(): all %d compute peers for app %d have joined.\n",
                __func__, app->app_num_peers, app->app_id);
#endif

        err = ds_announce_cp(ds, app);
        if(err < 0)
                goto err_out;

        if (ds->num_cp == ds->size_cp){ //all apps have joined
#ifdef DEBUG
                uloga("All apps has registered in the master server!\n");
#endif
                list_for_each_entry(app, &ds->app_list, struct app_info, app_entry){
                        if(app->app_cnt_peers != app->app_num_peers)
                                return 0;
                }

                err = ds_register_cp(ds, app);
                if(err < 0)
                        goto err_out;
        }
        return 0;
err_out:
        uloga("'%s()' failed with err %d\n", __func__, err);
        return err;
}



/*
 PC routine to unregister a compute peer.
*/
static int dsrpc_cn_unregister(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        //uloga("get into dsrpc_cn_unregister\n");
        struct dart_server *ds = ds_ref_from_rpc(rpc_s);
        struct hdr_register *hreg = (struct hdr_register *) cmd->pad;
        struct msg_buf *msg;
        struct node_id *peer;
        int err = -ENOMEM;
	int i = 0;

        static int num_unreg = 0;

	if(ds->f_stop != 1){
		num_unreg += hreg->num_cp;
		//uloga("rank%d num_unreg=%d\n", ds->rpc_s->ptlmap.id, num_unreg);
		if(num_unreg == ds->num_cp){
			ds->f_stop = 1;
		}
		ds->f_nacc = 1;
	
		//sent from client
		if(hreg->num_cp && cmd->id >= ds->num_sp){
			//TODO: need to send back to client?
			/*hreg->num_cp = 0;
			peer = ds_get_peer(ds, cmd->id);
			
			//TODO: need peer->f_unreg != 1?
			msg = msg_buf_alloc(rpc_s, peer, 1);
			if(!msg)
				goto err_out;

			msg->msg_rpc->id = ds->self->ptlmap.id;
			msg->msg_rpc->cmd = cn_unregister;
			
			err = rpc_send(rpc_s, peer, msg);
			if(err < 0){
				free(msg);
				goto err_out;
			}*/

			ds->num_charge--;
		}
		//uloga("rank%d, num_charge=%d\n", ds->self->ptlmap.id, ds->num_charge);
		//only send to other server at the time received last msg from client
		if(ds->num_charge == 0 && cmd->id >= ds->num_sp){
			
			for(i = 0; i < ds->num_sp; i++){
				if(ds->self->ptlmap.id == i)
					continue;
				hreg->num_cp = ds->num_charge_cp;
				peer = ds_get_peer(ds, i);
				
				msg = msg_buf_alloc(rpc_s, peer, 1);
				if(!msg)
					goto err_out;
				
				memcpy(msg->msg_rpc, cmd, sizeof(*cmd));
				msg->msg_rpc->id = ds->self->ptlmap.id;
				err = rpc_send(rpc_s, peer, msg);
				if(err < 0){
					free(msg);
					goto err_out;
				}	
			}
		}
	}
	return 0;
err_out:
	return err;
}


static int dsrpc_cn_unregister_old(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	//printf("get into dsrpc_cn_unregister\n");
        struct dart_server *ds = ds_ref_from_rpc(rpc_s);
        struct hdr_register *hreg = (struct hdr_register *) cmd->pad;
        struct msg_buf *msg;
        struct node_id *peer;
        int err = -ENOMEM;

	//check msg from client or server
	static int unreg_count=0;
	if(hreg->pm_cp.id%ds->num_sp == ds->self->ptlmap.id){
		unreg_count++;
		//printf("rank%d unreg_count=%d\n", ds->self->ptlmap.rank_pami, unreg_count);
	}

        static int num_unreg = 0;

        if(num_unreg == ds->num_cp)
                return 0;

        if((++num_unreg) == ds->num_cp){
                ds->f_stop = 1;
                //printf("%s(): #%u ds->f_stop flag is set as %d\n", __func__, ds->self->ptlmap.rank_pami, ds->f_stop);
#ifdef DEBUG
                uloga("%s(): #%u ds->f_stop flag is set as %d\n", __func__, ds->self->ptlmap.rank_pami, ds->f_stop);
#endif
        }

	//printf("rank%d ds->num_sp=%d ds->num_cp=%d, num_unreg=%d\n", rpc_s->ptlmap.rank_pami, ds->num_sp, ds->num_cp, num_unreg);
        /* 
 *            After  the first  compute peer  'unregister'  request, stop
 *                       accepting new requests and terminate pending ones.
 *                               */
        ds->f_nacc = 1;

        /* Forwarding the unregister rpc message in a ring manner*/
        hreg->num_sp--; //the rest number of forwarding
#ifdef DEBUG
        uloga("%s(): #%u(dart_id=%d) get cn_unregister of #%u(dart_id=%d) ds->num_cp=%d, num_unreg=%d, num_to_forward_sp=%d\n",
                __func__,ds->self->ptlmap.rank_pami,ds->self->ptlmap.id,
                hreg->pm_cp.rank_pami,hreg->pm_cp.id,ds->num_cp,num_unreg,hreg->num_sp);
#endif
        if(hreg->num_sp){
                peer = ds_get_peer(ds, (ds->self->ptlmap.id+1)%ds->num_sp);
                msg = msg_buf_alloc(rpc_s, peer, 1);
                if(!msg)
                        goto err_out;

                memcpy(msg->msg_rpc, cmd, sizeof(*cmd));
                msg->msg_rpc->id = ds->self->ptlmap.id;

                err = rpc_send(rpc_s, peer, msg);
                if(err < 0){
                        free(msg);
                        goto err_out;
                }
        }
        return 0;
err_out:
        ERROR_TRACE();
}

/*
 *This is invoked only by the master node.
 */
int dsrpc_sp_register(struct rpc_server *rpc_s, struct rpc_cmd *cmd)	//TODO functionshould be static
{
	//printf("get into dsrpc_sp_register\n");

	int i, k, err;
	struct dart_server *ds = ds_ref_from_rpc(rpc_s);
//	printf("master server=%d\n", ds->rpc_s->ptlmap.rank_pami);
        struct hdr_register *hreg = (struct hdr_register *)cmd->pad;
        struct node_id *peer;
        struct msg_buf *msg;

        peer = ds_get_peer(ds, ds->num_sp);
        peer->ptlmap = hreg->pm_cp;
        peer->ptlmap.id = ds->num_sp; //Assign application level id(DART id)
        peer->num_msg_ret++;

        //ds->self->num_msg_ret--; TODO

        //Incremental
        ds->num_sp = ds->num_sp + 1;

        //Wait for all nodes to join in before sending back registration info
        if(ds->num_sp < ds->size_sp)
                return 0;
#ifdef DEBUG    
        uloga("'%s()': all space peers joined.\n", __func__);
#endif
        //Send back registration info to space peers(excluding itself)
        struct ptlid_map *pptlmap;
        for(i=1; i<ds->size_sp; i++){
                err = -ENOMEM;
                peer = ds_get_peer(ds, i);

                msg = msg_buf_alloc(rpc_s, peer, 1);
                if(!msg)
                        goto err_out;

                //Send back data space servers basic info
                msg->cb = default_completion_with_data_callback;
                msg->size = sizeof(struct ptlid_map)*ds->num_sp;
                pptlmap = msg->msg_data = calloc(1,msg->size);
                if(!msg->msg_data)
                        goto err_out_free;
                for(k=0; k<ds->num_sp; k++){
                        *pptlmap++ = ds->peer_tab[k].ptlmap;
                }

                msg->msg_rpc->cmd = sp_reg_reply;
                msg->msg_rpc->id = ds->self->ptlmap.id;

                hreg = (struct hdr_register *)msg->msg_rpc->pad;
                hreg->pm_cp = peer->ptlmap;
                hreg->pm_sp = ds->self->ptlmap;
                hreg->num_sp = ds->num_sp;

                err = rpc_send(rpc_s, peer, msg);
                if(err < 0){
                        if(msg->msg_data)
                                free(msg->msg_data);
                        goto err_out_free;
                }
        }
	ds->f_reg = 1;
        err = rename("conf.srv", "conf");
        if (err < 0) {
                uloga("%s(): failed to rename the config file!\n", __func__);
                goto err_out;
        }

        return 0;
err_out_free:
        free(msg);
err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;     
}

static int sp_ack_register_completion(struct rpc_server *rpc_s, struct msg_buf *msg)
{
	//printf("get into sp_ack_register_completion\n");
        struct dart_server *ds = ds_ref_from_rpc(rpc_s);
        struct ptlid_map *pptlmap = msg->msg_data;
        struct node_id *peer;
        int i;

        //Copy the data space server nodes info
        peer = ds->peer_tab;
        for(i = 0; i<ds->size_sp; i++){
                peer->ptlmap = *pptlmap++;
                peer++;
        }

        ds->f_reg = 1;

        //Free the memory
        free(msg->msg_data);
        free(msg);

	return 0;

}

static int dsrpc_sp_ack_register(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
        struct dart_server *ds = ds_ref_from_rpc(rpc_s);
        struct hdr_register *hreg = (struct hdr_register *)cmd->pad;
        struct node_id *peer;
        struct msg_buf *msg;
        int err = -ENOMEM;

        ds->self = ds_get_peer(ds, hreg->pm_cp.id);//Locate myself
        ds->num_sp = hreg->num_sp;

        rpc_s->ptlmap = hreg->pm_cp;

        peer = ds_get_peer(ds, cmd->id);
        peer->ptlmap = hreg->pm_sp; //Update the ptlmap info for remote node

        msg = msg_buf_alloc(rpc_s, peer, 0);
        if(!msg)
                goto err_out;

        msg->size = cmd->mem_size;
        msg->msg_data = malloc(msg->size);
        if(!msg->msg_data)
                goto err_out;
        msg->cb = sp_ack_register_completion;

        rpc_mem_info_cache(peer, msg, cmd);
        err = rpc_receive_direct(rpc_s, peer, msg);
        if(err<0)
                goto err_out_free;

        return 0;
err_out_free:
        free(msg->msg_data);
        free(msg);
err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;

}

static int file_exist_nonempty(const char *path)
{
        int err;
        struct stat st_buf;
        err = stat(path, &st_buf);
        if (err != 0)
                return 0;

        if (st_buf.st_size == 0)
                return 0;

        return 1;
}

static int ds_register_at_master(struct dart_server *ds)
{
	//printf("get into ds_register_at_master\n");
        struct node_id *peer = ds_get_peer(ds,0);
        struct msg_buf *msg;
        struct hdr_register *hreg;
	char conf_file[] = "conf.srv";
        int err;

	while(!file_exist_nonempty(conf_file)){
		usleep(1000);
	}

        err = rpc_read_config(&peer->ptlmap.rank_pami, "conf.srv");
        if (err<0)
                goto err_out;
//	printf("read_config rank=%llu\n",peer->ptlmap.rank_pami);

        err = -ENOMEM;
        msg = msg_buf_alloc(ds->rpc_s, peer, 1);
        if (!msg)
                goto err_out;

        msg->msg_rpc->cmd = sp_reg_request;

        hreg = (struct hdr_register *)msg->msg_rpc->pad;
        hreg->pm_cp = ds->rpc_s->ptlmap;
        hreg->pm_sp = peer->ptlmap;

        err = rpc_send(ds->rpc_s, peer, msg);
        if (err<0)
                goto err_out_free;
        while (!ds->f_reg){
                err = rpc_process_event(ds->rpc_s);
                if (err<0)
                        goto err_out;
        }

        return 0;
err_out_free:
        if (msg)
                free(msg);
err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);

}

static int file_lock(int fd, int op)
{
        int err;
        struct flock fl = {
                        .l_type = (op != 0)? F_WRLCK : F_UNLCK,
                        .l_whence = SEEK_SET,
                        .l_start = 0,
                        .l_len = 0,
                        .l_pid = getpid()
        };


        err = fcntl(fd, F_SETLKW, &fl);
        if (err == 0)
                        return 0;

        ulog_err("'%s()' failed", __func__);
        return err;

}

static int ds_boot(struct dart_server *ds)
{
	//printf("get into ds_boot\n");
        struct stat st_buff;
        char lck_file[] = "srv.lck";
        char conf_file[] = "conf.srv";
        int fd, err;

        memset(&st_buff, 0, sizeof(st_buff));
/*
        err = fd = open(lck_file, O_WRONLY | O_CREAT, 0644);
        if (err < 0)
                goto err;

        err = file_lock(fd, 1);
        if (err < 0)
                goto err_fd;

        err = stat(conf_file, &st_buff);
        if (err < 0 && errno != ENOENT)
                goto err_flock;
*/
        //if (st_buff.st_size == 0) {	
        //TODO file_lock doesn't work, then assume rank0 to be the master server
        if(ds->rpc_s->ptlmap.rank_pami == 0) {
                ds->self = ds->peer_tab;
                ds->self->ptlmap = ds->rpc_s->ptlmap;
                err = rpc_write_config(ds->rpc_s, conf_file);

                if (err < 0)
                        goto err_flock;
        //        file_lock(fd, 0);
        	
		if(ds->size_sp == 1){
			printf("only one server\n");
			rename(conf_file, "conf");
		}
        }
        else {
        //        file_lock(fd, 0);
                err = ds_register_at_master(ds);
                if (err < 0)
                        goto err_flock;
        }

        close(fd);
//        remove(lck_file);

        return 0;
err_flock:
        uloga("%s(): failed with err_flock.\n");
//        file_lock(fd, 0);
err_fd:
        uloga("%s(): failed with err_fd.\n");
        close(fd);
//        remove(lck_file);
err:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}


//****************************************************
//Public APIs
//****************************************************

struct dart_server *ds_alloc(int num_sp, int num_cp, void *dart_ref, void *comm)
{
	//printf("get into ds alloc\n");
	struct dart_server *ds = NULL;
	struct node_id *peer;
	size_t size;
	int i, err;

	size = sizeof(struct dart_server) + 
			(num_sp + num_cp) * sizeof(struct node_id);
	ds = calloc(1, size);
	if(!ds)
		goto err_out;


	ds->dart_ref = dart_ref;
	ds->peer_tab = (struct node_id*)(ds+1);
	ds->cn_peers = ds->peer_tab + num_sp;
	ds->peer_size = num_sp + num_cp;
	ds->size_cp = num_cp;
	ds->size_sp = num_sp;
	ds->num_sp = 1;
	
	INIT_LIST_HEAD(&ds->app_list);
	ds->rpc_s = rpc_server_init(10, ds->peer_size, ds, DART_SERVER);
	if(!ds->rpc_s)
		goto err_free_dsrv;
	
	rpc_server_set_peer_ref(ds->rpc_s, ds->peer_tab, ds->peer_size);
	rpc_server_set_rpc_per_buff(ds->rpc_s, ds->peer_size);
	ds->rpc_s->app_num_peers = num_sp;

	peer = ds->peer_tab;
	for(i = 0; i < ds->peer_size; i++){
		INIT_LIST_HEAD(&peer->req_list);
		peer->num_msg_at_peer = ds->rpc_s->max_num_msg;
		peer->num_msg_ret = 0;
		peer++;
	}

	//Add RPC service routines for corresponding messages
        rpc_add_service(cn_register, dsrpc_cn_register);
        rpc_add_service(cn_unregister, dsrpc_cn_unregister);
	rpc_add_service(sp_reg_request, dsrpc_sp_register);
	rpc_add_service(sp_reg_reply, dsrpc_sp_ack_register);
	rpc_add_service(sp_announce_cp, dsrpc_announce_cp);

	//for test
	rpc_add_service(cn_read, dsrpc_cn_read);

	err = ds_boot(ds);
	if(err < 0)
		goto err_free_dsrv;

	//set ds->num_charge for unreg
	for(i = num_sp; i < num_sp + num_cp; i++){
		if((i % num_sp) == ds->self->ptlmap.id)
			ds->num_charge++;
	}
	ds->num_charge_cp = ds->num_charge;
	//printf("rank%d, ds->num_charge_cp=%d\n", ds->rpc_s->ptlmap.id, ds->num_charge_cp);

	return ds;
	
err_free_dsrv:
	if(ds)
		free(ds);
err_out:
	return NULL;
}


//****************************************************
//		for TEST
//****************************************************
int dsrpc_cn_data(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	//printf("get into dsrpc_cn_data\n");
	//struct node_id *peer;
	struct msg_buf *msg;
	int err;
	int *des_array;
	des_array = (int*)malloc(sizeof(int)*200 + 7);
	ALIGN_ADDR_QUAD_BYTES(des_array);

	//TODO peer
        struct node_id peer;
        memset(&peer, 0, sizeof(struct node_id));
        //int cur_rank = rpc_s->ptlmap.rank_pami;
        //int target = (cur_rank>0 ? cur_rank-1 : num_peers-1);
        peer.ptlmap.rank_pami = cmd->id;	
	INIT_LIST_HEAD(&peer.req_list);
	
	msg = msg_buf_alloc(rpc_s, &peer, 0);
	msg->size = cmd->mem_size;
	msg->msg_data = des_array;
	msg->cb = obj_put_completion;

	rpc_mem_info_cache(&peer, msg, cmd);	
	err = rpc_receive_direct(rpc_s, &peer, msg);

	return 0;	
}

int dsrpc_cn_read(struct rpc_server *rpc_s, struct rpc_cmd *cmd)
{
	//uloga("get into dsrpc_cn_read\n");
        struct dart_server *ds = ds_ref_from_rpc(rpc_s);
        struct node_id *peer;
        struct msg_buf *msg;
        int err;

        peer = ds_get_peer(ds, cmd->id);

        msg = msg_buf_alloc(rpc_s, peer, 0);
        //if(!msg)
        //        goto err_out;
        msg->size = cmd->mem_size;
        msg->msg_data = calloc(1, msg->size);
        /*if(!msg->msg_data){
                goto err_out_free;
        }*/
        msg->cb = cn_read_transfer_completion;

	int i;
        //int len = 'Z' - 'A' + 1;
        int num_elem = msg->size/sizeof(double);
        for(i=0; i < num_elem; i++){
                //*((char*)msg->msg_data + i) = 'A' + (i%len);
                *((double*)msg->msg_data + i) = 1;
		//uloga("%f\n", *((double*)msg->msg_data+i));
        }

#ifdef DEBUG
        uloga("%s(): #%u(dart_id=%d), get cn_read(msg->size=%d) from compute node #%u(dart_id=%d)\n",
                __func__,rpc_s->ptlmap.rank_pami,rpc_s->ptlmap.id,msg->size,peer->ptlmap.rank_pami,peer->ptlmap.id);
#endif

	rpc_mem_info_cache(peer, msg, cmd);

	//rpc_send_direct(rpc_s, &peer, msg);
	//peer->cached_remote_memregion = &cmd->mem_region;
	//rpc_send_direct(rpc_s, peer, msg, &cmd->mem_region);
	rpc_send_direct(rpc_s, peer, msg);

	return 0;
}

void ds_free(struct dart_server *ds)
{
	//uloga("get into ds_free\n");
	struct app_info *app, *t;
	rpc_server_free(ds->rpc_s);

	list_for_each_entry_safe(app, t, &ds->app_list, struct app_info, app_entry){
		list_del(&app->app_entry);
		free(app);
	}
	free(ds);
	uloga("%s(): OK\n", __func__);
}

int ds_process(struct dart_server *ds)
{
	return rpc_process_event(ds->rpc_s);
}
