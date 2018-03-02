#include "mpi.h"
#include <arpa/inet.h>
#include <ifaddrs.h>
#include <net/if.h>
#include <sys/ioctl.h>
#include <sys/socket.h>

#include "dart_rpc_tcp.h"
#include "debug.h"
//#include "queue.h"
#include <pthread.h>

/* It may be better to store these values in rpc_server struct */
/* Best size of bytes to be written in a single socket write call */
static uint64_t socket_best_write_size = 16384;
/* Best size of bytes to be read in a single socket read call */
static uint64_t socket_best_read_size = 87380;

pthread_mutex_t task_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t task_cond = PTHREAD_COND_INITIALIZER;

//Yubo use for profilling
double timer_timestamp_2(void)
{
        double ret;

#ifdef XT3
        ret = dclock();
#else
        struct timeval tv;

        gettimeofday( &tv, 0 );
        ret = (double) tv.tv_usec + tv.tv_sec * 1.e6;
#endif
        return ret;
}


static uint64_t str_to_uint64(const char *s) {
    uint64_t res = 0;
    while (*s != '\0') {
        if (!(*s >= '0' && *s <= '9')) {
            return (uint64_t)0;
        }
        res = res * 10 + (*s - '0');
        ++s;
    }
    return res;
}

/* It's really bad to use global variables to store RPC services (and 64 at most), but I have no choice */
static int num_service = 0;
static struct {
    enum cmd_type rpc_cmd;
    rpc_service rpc_func;
} rpc_commands[64];

void rpc_add_service(enum cmd_type rpc_cmd, rpc_service rpc_func) {
    rpc_commands[num_service].rpc_cmd = rpc_cmd;
    rpc_commands[num_service].rpc_func = rpc_func;
    ++num_service;
}

/* Search the address for a specific interface, or the first valid address if the interface is NULL */
static struct sockaddr_in search_ip_address(const char *interface) {
    struct sockaddr_in address;
    memset(&address, 0, sizeof(address));

    struct ifaddrs *addrs;
    getifaddrs(&addrs);
    struct ifaddrs *head = addrs;
    for (; head != NULL; head = head->ifa_next) {
        if (head->ifa_addr == NULL || head->ifa_addr->sa_family != AF_INET) {
            continue;
        }
        if ((head->ifa_flags & IFF_LOOPBACK) != 0) {
            continue;
        }
        if (interface != NULL && (head->ifa_name == NULL || strcmp(interface, head->ifa_name) != 0)) {
            continue;
        }
        address = *(struct sockaddr_in *)head->ifa_addr;
        break;
    }
    freeifaddrs(addrs);

    return address;
}

static int socket_send_bytes(int sockfd, char *buffer, uint64_t size) {
    while (size > 0) {
        ssize_t n = send(sockfd, buffer, (size_t)(socket_best_write_size < size ? socket_best_write_size : size), 0);
        if (n < 0) {
            printf("[%s]: send bytes through socket failed!\n", __func__);
            goto err_out;
        }
        buffer += n;
        size -= n;
    }

    return 0;

    err_out:
    return -1;
}

static int socket_recv_bytes(int sockfd, char *buffer, uint64_t size, int f_blocking) {
    if (!f_blocking) {
        /* Check if there is no data to read, return immediately. */
        int count = 0;
        ioctl(sockfd, FIONREAD, &count);
        if (count == 0) {
            return 1;
        }
    }

    while (size > 0) {
        ssize_t n = recv(sockfd, buffer, (size_t)(socket_best_read_size < size ? socket_best_read_size : size), 0);
        if (n < 0) {
            printf("[%s]: receive bytes through socket failed!\n", __func__);
            goto err_out;
        }
        if (n == 0) {
            printf("[%s]: connection has already closed, skip!\n", __func__);
            goto err_out;
        }
        buffer += n;
        size -= n;
    }
    return 0;

    err_out:
    return -1;
}

static int socket_recv_rpc_cmd(int sockfd, struct rpc_cmd *cmd) {
    /* TODO: should deserialize data */
    int ret = socket_recv_bytes(sockfd, (char *)cmd, (uint64_t)sizeof(*cmd), 0);
    if (ret < 0) {
        printf("[%s]: receive RPC command through socket failed!\n", __func__);
        goto err_out;
    }
    if (ret == 1) {
        /* No RPC command available yet */
        return 1;
    }
    return 0;

    err_out:
    return -1;
}

/* It will send the component type, id and appid */
int rpc_send_connection_info(struct rpc_server *rpc_s, struct node_id *peer) {
    struct connection_info info;
    info.cmp_type = rpc_s->cmp_type;
    info.id = rpc_s->ptlmap.id;
    info.app_id = rpc_s->ptlmap.appid;
    info.app_size = rpc_s->app_num_peers;

    /* TODO: should serialize data */
    if (socket_send_bytes(peer->sockfd, (char *)&info, (uint64_t)sizeof(info)) < 0) {
        printf("[%s]: send my connection info (%d, %d, %d) failed!\n", __func__, (int)info.cmp_type, info.id, info.app_id);
        goto err_out;
    }
    return 0;

    err_out:
    return -1;
}

int rpc_recv_connection_info(int sockfd, struct connection_info *info) {
    if (socket_recv_bytes(sockfd, (char *)info, (uint64_t)sizeof(*info), 1) < 0) {
        printf("[%s]: recv connection info failed!\n", __func__);
        goto err_out;
    }
    return 0;

    err_out:
    return -1;
}

static int rpc_cb_request_posted(struct rpc_server *rpc_s, struct rpc_request *request) {
    if (request->msg != NULL && request->msg->cb != NULL) {
        if ((*request->msg->cb)(rpc_s, request->msg) < 0) {
            printf("[%s]: call message callback function failed!\n", __func__);
            goto err_out;
        }
    }
    free(request);
    request = NULL;
    return 0;

    err_out:
    if (request != NULL) {
        free(request);
    }
    return -1;
}

static int rpc_server_init_socket(struct rpc_server *rpc_s) {
    rpc_s->sockfd_s = socket(AF_INET, SOCK_STREAM, 0);
    if (rpc_s->sockfd_s < 0) {
        printf("[%s]: create socket failed!\n", __func__);
        goto err_out;
    }

    struct sockaddr_in serv_addr;
    socklen_t serv_addr_len = sizeof(serv_addr);
    memset(&serv_addr, 0, serv_addr_len);
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_addr = rpc_s->ptlmap.address.sin_addr;
    serv_addr.sin_port = htons(0);
    if (bind(rpc_s->sockfd_s, (struct sockaddr *)&serv_addr, serv_addr_len) < 0) {
        printf("[%s]: bind server socket failed!\n", __func__);
        goto err_out;
    }

    listen(rpc_s->sockfd_s, 0xFFFF);
    if (getsockname(rpc_s->sockfd_s, (struct sockaddr *)&serv_addr, &serv_addr_len) < 0) {
        printf("[%s]: get socket name failed!\n", __func__);
        goto err_out;
    }
    rpc_s->ptlmap.address.sin_port = serv_addr.sin_port;
    return 0;

    err_out:
    if (rpc_s->sockfd_s >= 0) {
        close(rpc_s->sockfd_s);
    }
    return -1;
}

struct rpc_server* rpc_server_init(const char *interface, int app_num_peers, void *dart_ref, enum rpc_component cmp_type) {
    struct rpc_server *rpc_s = (struct rpc_server *)malloc(sizeof(struct rpc_server));
    if (rpc_s == NULL) {
        printf("[%s]: allocate RPC server failed!\n", __func__);
        goto err_out;
    }
   // struct queue tasks_queue;
    //tasks_queue = malloc(sizeof(struct queue));
    //memset(tasks_queue, 0, sizeof(struct queue));
   // queue_init(&tasks_queue);
   // uloga("%s(Yubo) after queue initiate, tasks_q->num_elem=%d\n",__func__, tasks_queue.num_elem);

    memset(rpc_s, 0, sizeof(*rpc_s));
    rpc_s->cmp_type = cmp_type;
    rpc_s->ptlmap.id = -1;
    rpc_s->ptlmap.appid = (cmp_type == DART_SERVER ? 0 : -1);
    rpc_s->ptlmap.address = search_ip_address(interface);
    rpc_s->num_peers = -1;
    rpc_s->peer_tab = NULL;
    rpc_s->app_minid = (cmp_type == DART_SERVER ? 0 : -1);
    rpc_s->app_num_peers = app_num_peers;
    rpc_s->thread_alive = 0; /* Should be set to 1 before creating the thread */
    rpc_s->dart_ref = dart_ref;
    //rpc_s->tasks_q = &tasks_queue; //tasks queue
    rpc_s->tasks_counter = 0;

    INIT_LIST_HEAD(&rpc_s->tasks_list);

    if (rpc_server_init_socket(rpc_s) < 0) {
        printf("[%s]: initialize socket for RPC server failed!\n", __func__);
        goto err_out;
    }

    char *write_size = getenv("DATASPACES_TCP_WRITE_SIZE");
    if (write_size != NULL) {
        socket_best_write_size = str_to_uint64(write_size);
    }
    char *read_size = getenv("DATASPACES_TCP_READ_SIZE");
    if (read_size != NULL) {
        socket_best_read_size = str_to_uint64(read_size);
    }

    return rpc_s;

    err_out:
    if (rpc_s != NULL) {
        free(rpc_s);
    }
    return NULL;
}

//Return rpc server task list size
int task_list_size(struct rpc_server *rpc_s){
    return rpc_s->tasks_counter;
}

void rpc_server_set_peer_ref(struct rpc_server *rpc_s, struct node_id *peer_tab, int num_peers) {
    rpc_s->num_peers = num_peers;
    rpc_s->peer_tab = peer_tab;
}

int rpc_write_config(struct rpc_server *rpc_s, const char *filename) {
    FILE *f = fopen(filename, "wt");
    if(f == NULL) {
        printf("[%s]: open config file failed!\n", __func__);
        goto err_out;
    }

    const char *ip = inet_ntoa(rpc_s->ptlmap.address.sin_addr);
    int port = (int)ntohs(rpc_s->ptlmap.address.sin_port);
    if (fprintf(f, "P2TNID=%s\nP2TPID=%d\n", ip, port) < 0) {
        printf("[%s]: write config file failed!\n", __func__);
        goto err_out;
    }

    fclose(f);
    f = NULL;
    return 0;

    err_out:
    if (f != NULL) {
        fclose(f);
    }
    return -1;
}

int rpc_read_config(struct sockaddr_in *address, const char *filename) {
    char ip_buff[32], port_buff[32];

    char *ip = getenv("P2TNID");
    char *port = getenv("P2TPID");

    FILE *f = NULL;
    if (ip == NULL || port == NULL) {
        f = fopen(filename, "rt");
        if (f == NULL) {
            printf("[%s]: open config file failed!\n", __func__);
            goto err_out;
        }
        if (fscanf(f, "P2TNID=%32s\nP2TPID=%32s\n", ip_buff, port_buff) != 2) {
            printf("[%s]: read config file failed!\n", __func__);
            goto err_out;
        }
        fclose(f);
        f = NULL;
        ip = ip_buff;
        port = port_buff;
    }

    memset(address, 0, sizeof(*address));
    address->sin_family = AF_INET;
    address->sin_addr.s_addr = inet_addr(ip);
    address->sin_port = htons(atoi(port));
    return 0;

    err_out:
    if (f != NULL) {
        fclose(f);
    }
    return -1;
}

/* Connect to a peer */
int rpc_connect(struct rpc_server *rpc_s, struct node_id *peer) {
    if (peer->f_connected) {
        return 0;
    }

    peer->sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (peer->sockfd < 0) {
        printf("[%s]: create socket failed!\n", __func__);
        goto err_out;
    }

    struct sockaddr_in local_addr;
    memset(&local_addr, 0, sizeof(local_addr));
    local_addr.sin_family = AF_INET;
    local_addr.sin_addr = rpc_s->ptlmap.address.sin_addr;
    local_addr.sin_port = htons(0);
    if (bind(peer->sockfd, (struct sockaddr *)&local_addr, (socklen_t)sizeof(local_addr)) < 0) {
        printf("[%s]: bind local socket failed!\n", __func__);
        goto err_out;
    }

    if (connect(peer->sockfd, (struct sockaddr *)&peer->ptlmap.address, sizeof(peer->ptlmap.address)) < 0) {
        printf("[%s]: connect to peer %d failed!\n", __func__, peer->ptlmap.id);
        goto err_out;
    }
    if (rpc_send_connection_info(rpc_s, peer) < 0) {
        printf("[%s]: send connection info failed!\n", __func__);
        goto err_out;
    }
    peer->f_connected = 1;
    return 0;

    err_out:
    if (peer->sockfd >= 0) {
        close(peer->sockfd);
    }
    return -1;
}

static int rpc_process_cmd(struct rpc_server *rpc_s, struct rpc_cmd *cmd) {
    uloga("[%s]: peer %d (%s) will process RPC command %d from %d.\n", __func__,
        rpc_s->ptlmap.id, rpc_s->cmp_type == DART_SERVER ? "server" : "client", (int)cmd->cmd, cmd->id);
    int i;
            
    for (i = 0; i < num_service; ++i) {
        if (cmd->cmd == rpc_commands[i].rpc_cmd) {
            uloga("%s(Yubo) DEBUG Found rpc function\n",__func__);

            if (rpc_commands[i].rpc_func(rpc_s, cmd) < 0) {
                printf("[%s]: call RPC command function failed!\n", __func__);
                goto err_out;
            }
            uloga("%s(Yubo) DEBUG Finish rpc function\n",__func__);

            break;
        }
    }
    if (i == num_service) {
        printf("[%s]: unknown RPC command %d!\n", __func__, (int)cmd->cmd);
        goto err_out;
    }
    return 0;

    err_out:
    return -1;
}

/* Process the RPC requests from a specific peer */
static int rpc_process_event_peer(struct rpc_server *rpc_s, struct node_id *peer) {
    while (1) {
        struct rpc_cmd cmd;
        /*
        //Allocate space for rpc_cmd and will store it to the tasks queue
        struct rpc_cmd *cmd = (struct rpc_cmd *)malloc(sizeof(struct rpc_cmd));
        memset(cmd, 0, sizeof(struct rpc_cmd));
        struct tasks_request *tasks_req = (struct tasks_request *)malloc(sizeof(struct tasks_request));
        memset(tasks_req, 0, sizeof(struct tasks_request));

        INIT_LIST_HEAD(&tasks_req->tasks_entry);
        */

        int ret = socket_recv_rpc_cmd(peer->sockfd, &cmd);
        if (ret < 0) {
            printf("[%s]: receive RPC command from peer %d failed!\n", __func__, peer->ptlmap.id);
            goto err_out;
        }
        if (ret == 1) {
            /* No event to process */
            break;
        }

        /* It is more convenient to set id here */
        cmd.id = peer->ptlmap.id;
        /*
        tasks_req->rpc_s = rpc_s;
        tasks_req->cmd = cmd;
        list_add_tail(&tasks_req->tasks_entry, &rpc_s->tasks_list);
        */
        uloga("%s(Yubo) before go into rpc_process_cmd\n",__func__);
        if (rpc_process_cmd(rpc_s, &cmd) < 0) {
            printf("[%s]: process RPC command failed!\n", __func__);
            goto err_out;
        }
         uloga("%s(Yubo) after get out of rpc_process_cmd\n",__func__);
    }
    return 0;

    err_out:
    return -1;
}

int rpc_process_event(struct rpc_server *rpc_s) {
    int i;
    for (i = 0; i < rpc_s->num_peers; ++i) {
        struct node_id *peer = &rpc_s->peer_tab[i];
        if (!peer->f_connected) {
            /* Not connected yet, no need for processing event */
            uloga("%s(Yubo) peer not connected \n", __func__);
            continue;
        }
        uloga("%s(Yubo) before rpc_process_event_peer \n", __func__);
        if (rpc_process_event_peer(rpc_s, peer) < 0) {
            printf("[%s]: process event for peer %d failed, skip!\n", __func__, peer->ptlmap.id);
            continue;
        }
        uloga("%s(Yubo) after rpc_process_event_peer \n", __func__);
    }
    return 0;
}

//Thread process RPC cmd
void* rpc_process_cmd_mt(void *tasks_request)
{
    struct tasks_request *local_tr = (struct tasks_request*)tasks_request;
    struct rpc_server *rpc_s = local_tr->rpc_s;
    struct rpc_cmd *cmd = local_tr->cmd;
    //uloga("[%s]: MT peer %d (%s) will process RPC command %d from %d.\n", __func__,
    //    rpc_s->ptlmap.id, rpc_s->cmp_type == DART_SERVER ? "server" : "client", (int)cmd->cmd, cmd->id);
    int i;
            
    for (i = 0; i < num_service; ++i) {
        if (cmd->cmd == rpc_commands[i].rpc_cmd) {

            if (rpc_commands[i].rpc_func(rpc_s, cmd) < 0) {
                printf("[%s]: call RPC command function failed!\n", __func__);
                goto err_out;
            }
            break;
        }
    }
    if (i == num_service) {
        printf("[%s]: unknown RPC command %d!\n", __func__, (int)cmd->cmd);
        goto err_out;
    }
    exit(0);

    err_out:
    exit(-1);

}

static int rpc_process_cmd_mt_2(struct rpc_server *rpc_s, struct rpc_cmd *cmd) {
    uloga("[%s]: peer %d (%s) will process RPC command %d from %d.\n", __func__,
        rpc_s->ptlmap.id, rpc_s->cmp_type == DART_SERVER ? "server" : "client", (int)cmd->cmd, cmd->id);
    int i;
            
    for (i = 0; i < num_service; ++i) {
        if (cmd->cmd == rpc_commands[i].rpc_cmd) {
            uloga("%s(Yubo) DEBUG Found rpc function\n",__func__);

            if (rpc_commands[i].rpc_func(rpc_s, cmd) < 0) {
                printf("[%s]: call RPC command function failed!\n", __func__);
                goto err_out;
            }
            uloga("%s(Yubo) DEBUG Finish rpc function\n",__func__);

            break;
        }
    }
    if (i == num_service) {
        printf("[%s]: unknown RPC command %d!\n", __func__, (int)cmd->cmd);
        goto err_out;
    }
    return 0;

    err_out:
    return -1;
}

void* thread_process_rpc_cmd(void *tasks_request){

    struct tasks_request *tmp_tr = (struct tasks_request*) tasks_request;
    struct rpc_server *local_rpc_s = tmp_tr->rpc_s;
    struct tasks_request *local_tasks_req = (struct tasks_request *)malloc(sizeof(struct tasks_request));;

    //uloga("%s(Yubo) Debug 3, I am pthread %d\n",__func__, pthread_self());

    pthread_mutex_lock(&task_mutex);

    uloga("%s(Yubo) Debug I am pthread %u, my rpc.id=%d \n",__func__, pthread_self(), local_rpc_s->ptlmap.id);

    /*
    list_for_each_entry(local_tasks_req, &local_rpc_s->tasks_list, struct tasks_request, tasks_entry)
            {
                uloga("%s(Yubo) client %d sent rpc_cmd=%d\n",__func__,local_tasks_req->cmd->id, local_tasks_req->cmd->cmd);
            }   
    */ 

    if(list_empty(&local_rpc_s->tasks_list)){
        uloga("%s(Yubo) Debug 4\n",__func__);
        pthread_cond_wait(&task_cond, &task_mutex);
        uloga("%s(Yubo) Debug 5\n",__func__);
    }
    else{
        uloga("%s(Yubo) Debug 6\n",__func__);
        local_tasks_req = list_entry(local_rpc_s->tasks_list.next, struct tasks_request, tasks_entry);
        uloga("%s(Yubo) Debug 7, will process cmd=%d, from rpc %d\n",__func__,local_tasks_req->cmd->cmd, local_tasks_req->rpc_s->ptlmap.id );

        if (rpc_process_cmd(local_tasks_req->rpc_s, local_tasks_req->cmd) < 0){
            uloga("%s process RPC command failed!\n", __func__);
            exit(-1);
            }
        uloga("%s(Yubo) Debug 8\n",__func__);

        //tmp put at here, move up after this function work
        list_del(&local_tasks_req->tasks_entry);
        uloga("%s(Yubo) Debug 9\n",__func__);
        pthread_mutex_unlock(&task_mutex);
    }


}


void* test_thread_process_rpc_cmd(void *rpc_s){

    //struct tasks_request *tmp_tr = (struct tasks_request*) tasks_request;
    struct rpc_server *local_rpc_s = (struct rpc_server*) rpc_s;
    struct tasks_request *local_tasks_req = (struct tasks_request *)malloc(sizeof(struct tasks_request));;

    //uloga("%s(Yubo) Debug , I am pthread %d\n",__func__, pthread_self());
     uloga("%s(Yubo) Debug I am pthread %lu, my rpc.id=%d at time=%f \n",__func__, pthread_self(), local_rpc_s->ptlmap.id, timer_timestamp_2());
    pthread_mutex_lock(&task_mutex);

    //uloga("%s(Yubo) Debug I am pthread %d, my rpc.id=%d \n",__func__, pthread_self(), local_rpc_s->ptlmap.id);

        local_tasks_req = list_entry(local_rpc_s->tasks_list.next, struct tasks_request, tasks_entry);
        //uloga("%s(Yubo) Debug 7\n",__func__);

        if (rpc_process_cmd(local_tasks_req->rpc_s, local_tasks_req->cmd) < 0){
            uloga("%s process RPC command failed!\n", __func__);
            exit(-1);
            }
        uloga("%s(Yubo) finish executing RPC %d cmd at time=%f\n",__func__,local_tasks_req->cmd->cmd, timer_timestamp_2());

        //tmp put at here, move up after this function work
        list_del(&local_tasks_req->tasks_entry);
        local_rpc_s->tasks_counter--;
        //uloga("%s(Yubo) Debug 9\n",__func__);
        pthread_mutex_unlock(&task_mutex);

}




void* thread_handle(void* attr){ //attr here is dart_server

    int err = 0;
    int i;
    struct rpc_server *local_rpc_s = (struct rpc_server*)attr;
    struct tasks_request *local_tasks_req = (struct tasks_request *)malloc(sizeof(struct tasks_request));
    struct tasks_request *tmp_tr;

    local_tasks_req->rpc_s = local_rpc_s;
    local_tasks_req->cmd = NULL;

    
    pthread_t threadid[2]; //tmp create two worker threads
    uloga("%s(Yubo) Debug 1, my rpc_s->ptlmap.id=%d, thread id=%lu\n",__func__, local_rpc_s->ptlmap.id, pthread_self());
    //create worker threads
    /*
    for (i = 0; i<2; i++){ //Need to predefine this number of threads here
        //uloga("%s(Yubo) Debug 1\n",__func__);
        err = pthread_create(&threadid[i], NULL, thread_process_rpc_cmd, (void*) local_tasks_req);
        //uloga("%s(Yubo) Debug 2\n",__func__);
        
    }
    

    if(err < 0){
        uloga("%s() Error in create worker threads\n", __func__);
        exit(-1);
    }
    */
   // uloga("%s(Yubo) Debug 2\n",__func__);
    

    //Need to pass rpc_s
    //while (!local_rpc_s->dart_ref->f_stop){ //using rpc_s find its dart_server, f_stop=1 when dart server stop
    while(1){
       // uloga("%s(Yubo) Debug 3\n",__func__);
       // pthread_mutex_lock(&task_mutex);
        //if(!list_empty(&local_rpc_s->tasks_list))
        if(local_rpc_s->tasks_counter > 0)
        {
            //uloga("%s(Yubo) Debug before thread create\n",__func__);
        //There is task request in the task list, wake up all workers to check it out
            /*
            list_for_each_entry(tmp_tr, &local_rpc_s->tasks_list, struct tasks_request, tasks_entry)
            {
                uloga("%s(Yubo) client %d sent rpc_cmd=%d\n",__func__,tmp_tr->cmd->id, tmp_tr->cmd->cmd);
            }    
            */
            uloga("%s(Yubo) Debug before thread create at time=%f\n",__func__,timer_timestamp_2());
            err = pthread_create(&threadid[0], NULL, test_thread_process_rpc_cmd, (void*) local_rpc_s); //for debuging
            uloga("%s(Yubo) Debug after thread create\n",__func__);
            pthread_join(threadid[0], NULL);
            uloga("%s(Yubo) Debug after thread join at time=%f\n",__func__,timer_timestamp_2());
            //pthread_cond_signal(&task_cond);
            //uloga("%s(Yubo) pthread_cond_broadcast\n",__func__);
        }


            
        

        //uloga("%s(Yubo) tasks list empty\n",__func__);
       // pthread_mutex_unlock(&task_mutex);

    }
   // uloga("%s(Yubo) Debug 5\n",__func__);
    //Finalizae worker threads
    /*
    for (i=0; i<2; i++){
        pthread_cancel(threadid[i]);
        pthread_join(threadid[i], NULL);
    }
    */
   // pthread_mutex_destroy(&task_mutex);
    //pthread_cond_destroy(&task_cond);
  //  uloga("%s(Yubo) Debug 6\n",__func__);

}

void* test_thread_process_rpc_cmd_2(void *tasks_request){

    struct tasks_request *tmp_tr = (struct tasks_request*) tasks_request;
    struct rpc_server *local_rpc_s = tmp_tr->rpc_s;
    //struct rpc_cmd *local_cmd = tmp_tr->cmd;
    struct tasks_request *local_tasks_req = (struct tasks_request *)malloc(sizeof(struct tasks_request));
    int task_flag = 0;


    //uloga("%s(Yubo) Debug , I am pthread %d\n",__func__, pthread_self());
    // uloga("%s(Yubo) Debug I am pthread %lu, my rpc.id=%d at time=%f \n",__func__, pthread_self(), local_rpc_s->ptlmap.id, timer_timestamp_2());


    while(1){

        pthread_mutex_lock(&task_mutex);
        if(local_rpc_s->tasks_counter == 0){
            pthread_cond_wait(&task_cond, &task_mutex);
        }
        else{
            local_tasks_req = list_entry(local_rpc_s->tasks_list.next, struct tasks_request, tasks_entry);
            local_rpc_s->tasks_counter--;
            list_del(&local_tasks_req->tasks_entry);
            task_flag = 1;
        }
        pthread_mutex_unlock(&task_mutex);


        if(task_flag){
            uloga("%s start process RPC command\n", __func__);
            //if (rpc_process_cmd(local_tasks_req->rpc_s, local_tasks_req->cmd) < 0){
              if(rpc_process_cmd_mt(local_tasks_req) < 0){
                uloga("%s process RPC command failed!\n", __func__);
                exit(-1);
            }
            task_flag = 0;
        }

    }


}







/* Process the RPC requests from a specific peer */
static int rpc_process_event_peer_mt(struct rpc_server *rpc_s, struct node_id *peer) {
    pthread_t threadid;
    while (1) {
        //struct rpc_cmd cmd;
        //Allocate space for rpc_cmd and will store it to the tasks queue
        struct rpc_cmd *cmd = (struct rpc_cmd *)malloc(sizeof(struct rpc_cmd));
        memset(cmd, 0, sizeof(struct rpc_cmd));
        struct tasks_request *tasks_req = (struct tasks_request *)malloc(sizeof(struct tasks_request));
        memset(tasks_req, 0, sizeof(struct tasks_request));

        

        int ret = socket_recv_rpc_cmd(peer->sockfd, cmd);
        if (ret < 0) {
            printf("[%s]: receive RPC command from peer %d failed!\n", __func__, peer->ptlmap.id);
            goto err_out;
        }
        if (ret == 1) {
            /* No event to process */
            break;
        }

        /* It is more convenient to set id here */
        cmd->id = peer->ptlmap.id;


        //only put obj_put to the tasks
        if (cmd->cmd == 16){

             //tasks_req->rpc_s = rpc_s;
             //tasks_req->cmd = cmd;

            //pthread_create(&threadid, NULL, test_thread_process_rpc_cmd_2, (void*) tasks_req);
            //pthread_join(threadid, NULL);

        
        INIT_LIST_HEAD(&tasks_req->tasks_entry);
        tasks_req->rpc_s = rpc_s;
        tasks_req->cmd = cmd;
        pthread_mutex_lock(&task_mutex);
        list_add_tail(&tasks_req->tasks_entry, &rpc_s->tasks_list);
        rpc_s->tasks_counter++;
        //uloga("%s(Yubo) Debug added tasks cmd %d into list at time=%f, my rpc_s->ptlmap,id=%d, tasks_counter=%d, current task list size=%d\n",__func__, \
            cmd->cmd, timer_timestamp_2(), rpc_s->ptlmap.id,rpc_s->tasks_counter, task_list_size(rpc_s));
        
        pthread_cond_signal(&task_cond);
        pthread_mutex_unlock(&task_mutex);
        
        }
        /*
        else if(cmd->cmd <=0 || cmd->cmd >= 40){
            break;

        } 
        */
        else{
            if (rpc_process_cmd(rpc_s, cmd) < 0) {
            printf("[%s]: process RPC command failed!\n", __func__);
            goto err_out;
             }
         }


   
/*
        if (rpc_process_cmd(rpc_s, cmd) < 0) {
            printf("[%s]: process RPC command failed!\n", __func__);
            goto err_out;
        }
 */
    }
    return 0;

    err_out:
    return -1;
}

/* Process the RPC requests from a specific peer */
void* rpc_process_event_peer_mt_2(struct tasks_request * tasks_request) {
    struct tasks_request *tasks_req = (struct tasks_request*) tasks_request;
    struct rpc_server *local_rpc_s = tasks_req->rpc_s;
    struct node_id *local_peer;
    struct tasks_request *local_tasks_req;
    struct rpc_cmd cmd;


    while (1) {

        pthread_mutex_lock(&task_mutex);
        uloga("%s(Yubo) mutex locked\n", __func__);
        if(local_rpc_s->tasks_counter == 0){
            uloga("%s(Yubo) before pthread_cond_wait\n", __func__);
            pthread_cond_wait(&task_cond, &task_mutex);
            uloga("%s(Yubo) after pthread_cond_wait\n", __func__);
            //continue;
        }
        else{
            local_tasks_req = list_entry(local_rpc_s->tasks_list.next, struct tasks_request, tasks_entry);
            local_rpc_s->tasks_counter--;
            
            int ret = socket_recv_rpc_cmd(local_tasks_req->peer->sockfd, &cmd);
            uloga("%s(Yubo) socket_recv_rpc_cmd return value=%d\n", __func__,ret);
            if (ret < 0) {
                printf("[%s]: receive RPC command from peer %d failed!\n", __func__, local_tasks_req->peer->ptlmap.id);
                //goto err_out;
            }
            if (ret == 1) {
            /* No event to process */
                //continue;
                uloga("%s(Yubo) before break \n", __func__);
               // pthread_mutex_unlock(&task_mutex);
                break;
                uloga("%s(Yubo) after break\n", __func__);
            }

        /* It is more convenient to set id here */
            cmd.id = local_tasks_req->peer->ptlmap.id;
            uloga("%s(Yubo) will process RPC cmd %d\n", __func__, cmd.cmd);
            if (rpc_process_cmd_mt_2(local_rpc_s, &cmd) < 0) {
                printf("[%s]: process RPC command failed!\n", __func__);
                //goto err_out;
            }
            list_del(&local_tasks_req->tasks_entry);

        }
        uloga("%s(Yubo) mutex ready to unlocked \n", __func__);
         pthread_mutex_unlock(&task_mutex);
         uloga("%s(Yubo) mutex unlocked \n", __func__);
        
    }

    uloga("%s(Yubo) out of while loop\n", __func__);
        
    //err_out:

}


//for DS server only, using multithreading
int rpc_process_event_mt(struct rpc_server *rpc_s) {
    int i;
    struct tasks_request *tasks_req = (struct tasks_request *)malloc(sizeof(struct tasks_request));
    memset(tasks_req, 0, sizeof(struct tasks_request));

    for (i = 0; i < rpc_s->num_peers; ++i) {
        struct node_id *peer = &rpc_s->peer_tab[i];
        
        if (!peer->f_connected) {
            /* Not connected yet, no need for processing event */
            uloga("%s(Yubo) peer not connected \n", __func__);
            continue;
        }
        else{
            uloga("%s(Yubo) before put tasks_request to list \n", __func__);
        INIT_LIST_HEAD(&tasks_req->tasks_entry);
        tasks_req->rpc_s = rpc_s;
        tasks_req->peer = peer;
        pthread_mutex_lock(&task_mutex);
        list_add_tail(&tasks_req->tasks_entry, &rpc_s->tasks_list);
        rpc_s->tasks_counter++;
        
        pthread_cond_signal(&task_cond);
        pthread_mutex_unlock(&task_mutex);
        uloga("%s(Yubo) after put tasks_request to list and signal cond\n", __func__);

        }

/*
        if (rpc_process_event_peer_mt(rpc_s, peer) < 0) {
            printf("[%s]: process event for peer %d failed, skip!\n", __func__, peer->ptlmap.id);
            continue;
        }
 */ 
    }
    return 0;
}

void* thread_handle_new(void *attr){
    int err = 0;
    int i;
    struct rpc_server *local_rpc_s = (struct rpc_server*)attr;
    struct tasks_request *local_tasks_req = (struct tasks_request *)malloc(sizeof(struct tasks_request));
    struct tasks_request *tmp_tr;
    //pthread_t threadid[2];


    local_tasks_req->rpc_s = local_rpc_s;
    //local_tasks_req->cmd = NULL;

    //create two worker threads
    for(i=0; i<1; i++){
        pthread_create(&local_rpc_s->worker_thread[i], NULL, rpc_process_event_peer_mt_2, (void*)local_tasks_req);
    }


}

int rpc_barrier(struct rpc_server *rpc_s, void *comm) {
    /* TODO: should use a better way */
    if (comm == NULL) {
        MPI_Barrier(MPI_COMM_WORLD);
    } else {
        MPI_Barrier(*(MPI_Comm*)comm);
    }
    return 0;
}

static int rpc_post_request(struct rpc_server *rpc_s, struct node_id *peer, struct rpc_request *request) {
    if (socket_send_bytes(peer->sockfd, (char *)request->data, (uint64_t)request->size) < 0) {
        printf("[%s]: send RPC request to peer %d failed!\n", __func__, peer->ptlmap.id);
        goto err_out;
    }


    if (request->iodir == io_send) {
        if (socket_send_bytes(peer->sockfd, (char *)request->msg->msg_data, (uint64_t)request->msg->size) < 0) {
            printf("[%s]: send to peer %d directly failed!\n", __func__, peer->ptlmap.id);
            goto err_out;
        }
    } else if (request->iodir == io_receive) {
        if (socket_recv_bytes(peer->sockfd, (char *)request->msg->msg_data, (uint64_t)request->msg->size, 1) < 0) {
            printf("[%s]: receive from peer %d directly failed!\n", __func__, peer->ptlmap.id);
            goto err_out;
        }
    }

    if (request->cb == NULL) {
        printf("[%s]: request doesn't have a callback function, may cause memory leak!\n", __func__);
        goto err_out;
    }

    if ((*request->cb)(rpc_s, request) < 0) {
        printf("[%s]: call request callback function failed!\n", __func__);
        return -1;
    }
    return 0;

    err_out:
    if (request->msg != NULL) {
        if (request->msg->msg_data != NULL) {
            free(request->msg->msg_data);
        }
        free(request->msg);
    }
    free(request);
    return -1;
}

static int peer_process_send_list(struct rpc_server *rpc_s, struct node_id *peer) {
    while (!list_empty(&peer->req_list)) {
        struct rpc_request *request = list_entry(peer->req_list.next, struct rpc_request, req_entry);
        request->msg->msg_rpc->id = rpc_s->ptlmap.id;
        list_del(&request->req_entry);

        if (rpc_post_request(rpc_s, peer, request) < 0) {
            printf("[%s]: post RPC request for peer %d failed!\n", __func__, peer->ptlmap.id);
            goto err_out;
        }
    }
    return 0;

    err_out:
    return -1;
}

int rpc_send(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg) {
    if (!peer->f_connected) {
        rpc_connect(rpc_s, peer);
    }

    struct rpc_request *request = (struct rpc_request *)malloc(sizeof(struct rpc_request));
    if (request == NULL) {
        printf("[%s]: allocate request failed!\n", __func__);
        goto err_out;
    }

    /* TODO: should serialize data */
    request->msg = msg;
    request->iodir = io_send;
    request->data = msg->msg_rpc;
    request->size = sizeof(*msg->msg_rpc);
    request->cb = (request_callback)rpc_cb_request_posted;
    list_add(&request->req_entry, &peer->req_list);
    if (peer_process_send_list(rpc_s, peer) < 0) {
        printf("[%s]: process send list for peer %d failed!\n", __func__, peer->ptlmap.id);
        goto err_out;
    }
    return 0;

    err_out:
    /* Request will be free in callback function */
    return -1;
}

int rpc_send_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg) {
    if (!peer->f_connected) {
        printf("[%s]: cannot send to an unconnected peer directly!\n", __func__);
        goto err_out;
    }

    /* TODO: should serialize data */
    if (socket_send_bytes(peer->sockfd, (char *)msg->msg_data, (uint64_t)msg->size) < 0) {
        printf("[%s]: send to peer %d directly failed!\n", __func__, peer->ptlmap.id);
        goto err_out;
    }

    if (msg->cb != NULL) {
        if ((*msg->cb)(rpc_s, msg) < 0) {
            printf("[%s]: call message callback function failed!\n", __func__);
            goto err_out;
        }
    }
    return 0;

    err_out:
    return -1;
}

// added by Tong
int rpc_send_directv(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg) {

  return 0;
}

int rpc_receive(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg) {
    if (!peer->f_connected) {
        rpc_connect(rpc_s, peer);
    }

    struct rpc_request *request = (struct rpc_request *)malloc(sizeof(struct rpc_request));
    if (request == NULL) {
        printf("[%s]: allocate request failed!\n", __func__);
        goto err_out;
    }

    /* TODO: should serialize data */
    request->msg = msg;
    request->iodir = io_receive;
    request->data = msg->msg_rpc;
    request->size = sizeof(*msg->msg_rpc);
    request->cb = (request_callback)rpc_cb_request_posted;
    list_add(&request->req_entry, &peer->req_list);
    if (peer_process_send_list(rpc_s, peer) < 0) {
        printf("[%s]: process send list for peer %d failed!\n", __func__, peer->ptlmap.id);
        goto err_out;
    }
    return 0;

    err_out:
    /* Request will be free in callback function */
    return -1;
}

int rpc_receive_direct(struct rpc_server *rpc_s, struct node_id *peer, struct msg_buf *msg) {
    if (!peer->f_connected) {
        printf("[%s]: cannot receive from an unconnected peer directly!\n", __func__);
        goto err_out;
    }

    /* TODO: should deserialize data */
    if (socket_recv_bytes(peer->sockfd, (char *)msg->msg_data, (uint64_t)msg->size, 1) < 0) {
        printf("[%s]: receive from peer %d directly failed!\n", __func__, peer->ptlmap.id);
        goto err_out;
    }

    if (msg->cb != NULL) {
        if ((*msg->cb)(rpc_s, msg) < 0) {
            printf("[%s]: call message callback function failed!\n", __func__);
            goto err_out;
        }
    }

    return 0;

    err_out:
    return -1;
}

/* TODO: */
int rpc_server_free(struct rpc_server *rpc_s) {
    if(rpc_s != NULL) {
        free(rpc_s);
    }
    return 0;
}

struct msg_buf* msg_buf_alloc(struct rpc_server *rpc_s, const struct node_id *peer, int num_rpcs) {
    size_t size = sizeof(struct msg_buf) + sizeof(struct rpc_cmd) * num_rpcs + 7; /* 7 is for alignment padding */
    struct msg_buf *msg = (struct msg_buf *)malloc(size);
    if (msg == NULL) {
        printf("[%s]: allocate message failed!\n", __func__);
        goto err_out;
    }

    memset(msg, 0, size);
    msg->peer = peer;
    msg->cb = default_completion_with_data_callback;
    if (num_rpcs > 0) {
        msg->msg_rpc = (struct rpc_cmd *)(msg + 1);
        ALIGN_ADDR_QUAD_BYTES(msg->msg_rpc);
    }
    return msg;

    err_out:
    if (msg != NULL) { 
        free(msg);
    }
    return NULL;
}

void rpc_mem_info_cache(struct node_id *peer, struct msg_buf *msg, struct rpc_cmd *cmd) {
}

void rpc_mem_info_reset(struct node_id *peer, struct msg_buf *msg, struct rpc_cmd *cmd) {
}

void rpc_report_md_usage(struct rpc_server *rpc_s) {
}

void finalize_threads(struct rpc_server* rpc_s_ptr)
{
    struct rpc_server* rpc_s = (struct rpc_server*) rpc_s_ptr;
    int i=0;
    pthread_cancel(rpc_s->task_thread);
    pthread_join(rpc_s->task_thread, NULL);


    //tmp put worker thread to rpc_s
    for(i=0; i<2; i++){
        pthread_cancel(rpc_s->worker_thread[i]);
        pthread_join(rpc_s->worker_thread[i], NULL);
    }


    pthread_mutex_destroy(&task_mutex);
    pthread_cond_destroy(&task_cond);

}
