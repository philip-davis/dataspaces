#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "common.h"
#include "cods_api.h"
#include "mpi.h"

/*
#include "hpgv.h"
#include "carraysearch.h"
*/

// Forward declaration
static void log_write_var(int rank, const char *var_name, int ndim, int elem_size, int version, uint64_t *lb, uint64_t *ub, uint64_t *gdim);
static void log_read_var(int rank, const char *var_name, int ndim, int elem_size, int version, uint64_t *lb, uint64_t *ub, uint64_t *gdim);

struct viz_task_info {
    int is_viz_init;
    double *viz_data;
    // int num_viz_step;
};
// Information kept for rendering task
struct viz_task_info viz_info;

struct parallel_communicator {
    MPI_Comm comm;
};

typedef int (*task_function)(struct cods_task *t, struct parallel_communicator *comm);
struct parallel_task {
	int appid;
	task_function func_ptr;
};
static struct parallel_task ptasks[MAX_NUM_TASKS+1];
static int num_tasks = 0;
static MPI_Comm origin_mpi_comm; 

static void viz_task_info_init(struct viz_task_info *p)
{
    p->is_viz_init = 0;
    p->viz_data = NULL;
}

static void viz_task_info_free(struct viz_task_info *p)
{
    if (p->is_viz_init && p->viz_data) {
        free(p->viz_data);
    }
}

static int communicator_init(MPI_Comm comm)
{
    viz_task_info_init(&viz_info);
    origin_mpi_comm = comm;

    return 0;    
}

static int communicator_free()
{
    viz_task_info_free(&viz_info);
    return 0;    
}

static struct parallel_communicator* create_parallel_comm(struct cods_task *t)
{
    struct parallel_communicator *comm = malloc(sizeof(*comm));
    MPI_Group origin_mpi_comm_group;
    MPI_Group new_group;

    MPI_Comm_group(origin_mpi_comm, &origin_mpi_comm_group);
    MPI_Group_incl(origin_mpi_comm_group, t->nproc, t->bk_mpi_rank_tab, &new_group);
    MPI_Comm_create_group(origin_mpi_comm, new_group, 0, &comm->comm); 

    return comm;
}

static void free_parallel_comm(struct parallel_communicator *comm) {
    if (comm) {
        free(comm);
    }
}

static void task_done(struct cods_task *t, struct parallel_communicator *comm)
{
    if (comm) {
        MPI_Barrier(comm->comm);
        if (t->rank == 0) { 
            cods_set_task_finished(t);
        }
    } else {
        uloga("%s(): error comm == NULL\n", __func__);
    }
}

static int register_task_function(int appid, task_function func_ptr)
{
	ptasks[num_tasks].appid = appid;
	ptasks[num_tasks].func_ptr = func_ptr;
	num_tasks++;
}

static int exec_task_function(struct cods_task *t)
{
	int i, err;
	for (i = 0; i < num_tasks; i++) {
		if (t->appid == ptasks[i].appid) {
            //double t1, t2;
            //t1 = MPI_Wtime();
            struct parallel_communicator *comm = create_parallel_comm(t);	
            err = (ptasks[i].func_ptr)(t, comm);
            task_done(t, comm); 
            free_parallel_comm(comm); 
            //t2 = MPI_Wtime();
            //uloga("%s(): task appid= %d execution time %lf\n", __func__, t->appid, t2-t1);

            if (t->bk_mpi_rank_tab) free(t->bk_mpi_rank_tab);
            if (t->vars) free(t->vars);
            return err;
		}
	}

	uloga("%s(): unknown task appid= %d\n", __func__, t->appid);
}

// Note: called by the master process (or executor) of the task
static int update_var(const char *var_name, int version, size_t elem_size,
    int ndim, uint64_t *gdim)
{
    int err;
    struct cods_var var_desc;
    // TODO: add filling var_desc
    err = cods_update_var(&var_desc, OP_PUT);
    return err;
}  

static struct cods_var* lookup_task_var(struct cods_task *t, const char *var_name)
{
    int i;
    for (i = 0; i < t->num_vars; i++) {
        if (0 == strcmp(t->vars[i].name, var_name))
            return &(t->vars[i]);
    }
    return NULL;
}

static int check_var_dimension(struct cods_var *var) {
    if (!var) return -1;

    if (var->gdim.ndim != var->dist_hint.ndim) {
        uloga("ERROR %s: number of dimension mismatched for gdim and dist_hint!\n",
            __func__);
        return -1;
    }

    int ndim = var->gdim.ndim;
    if (ndim > BBOX_MAX_NDIM || ndim < 0) {
        uloga("ERROR %s: wrong ndim= %d\n", __func__, ndim);
        return -1;
    }
    return 0;
}

static int generate_sp(struct cods_var *var, uint64_t *sp)
{
    if (check_var_dimension(var) < 0) return -1;

    int i;
    for (i = 0; i < var->gdim.ndim; i++) {
        if (var->dist_hint.sizes.c[i] != 0) {
            sp[i] = var->gdim.sizes.c[i]/var->dist_hint.sizes.c[i];
        } else {
            uloga("ERROR %s: var->dist_hint.sizes.c[%d]= %u\n", __func__,
                i, var->dist_hint.sizes.c[i]);
            return -1;
        }
    }
    return 0;
}

static int generate_np(struct cods_var *var, uint64_t *np)
{
    if (check_var_dimension(var) < 0) return -1;

    int i;
    for (i = 0; i < var->dist_hint.ndim; i++) {
        np[i] = var->dist_hint.sizes.c[i];
        if (np[i] == 0) {
            uloga("ERROR %s: np[%d] is 0!\n", __func__, i);
            return -1;
        }
    }
    return 0;
}

static void* allocate_data(struct cods_var *var)
{
    void* buf = NULL;
    uint64_t sp[BBOX_MAX_NDIM];
    uint64_t num_elem = 1;
    int i;

    if (generate_sp(var, sp) < 0) return buf;
    for (i = 0; i < var->gdim.ndim; i++) {
        num_elem *= sp[i];
    }
    buf = malloc(var->elem_size * num_elem);
    return buf;        
}

static int generate_data_value(struct cods_var *var, void *data, int version)
{
    double value = version;
    int i;
    uint64_t sp[BBOX_MAX_NDIM];
    uint64_t num_elem = 1;

    if (var->elem_size != sizeof(double)) {
        uloga("ERROR %s: var->elem_size= %u should equal to sizeof(double)= %u\n",
            __func__, var->elem_size, sizeof(double));
        return -1;
    } 
    
    if (generate_sp(var,sp) < 0) return -1;
    for (i = 0; i < var->gdim.ndim; i++) {
        num_elem *= sp[i];
    }
    double *array = (double*)data;
    for (i = 0; i < num_elem; i++) {
        array[i] = value;
    } 
    return 0;
}

static void set_ndim(struct cods_var *var, int *ndim) {
    *ndim = var->gdim.ndim;
}

static void set_gdim(struct cods_var *var, uint64_t *gdim) {
    int i;
    for (i = 0; i < var->gdim.ndim; i++) {
        gdim[i] = var->gdim.sizes.c[i];
    }
}

static void set_bbox(struct cods_task *t, struct cods_var *var,
    uint64_t *lb, uint64_t *ub)
{
    int i, j;
    uint64_t np[BBOX_MAX_NDIM], sp[BBOX_MAX_NDIM], offset[BBOX_MAX_NDIM];
    if (generate_sp(var, sp) < 0) return;
    if (generate_np(var, np) < 0) return;

    for (i = 0; i < var->gdim.ndim; i++) {
        int temp = t->rank;
        for (j = 0; j < i; j++)
            temp /= np[j];
        offset[i] = temp % np[i] * sp[i]; 
    }
    for (i = 0; i < var->gdim.ndim; i++) {
        lb[i] = offset[i];
        ub[i] = offset[i] + sp[i] - 1;
    }
}

static int read_data(struct cods_task *t, struct cods_var *var, int version, struct parallel_communicator *comm, int use_lock)
{
    int err;
    double *data = NULL;
    int ndim;
    size_t elem_size;
    uint64_t lb[BBOX_MAX_NDIM], ub[BBOX_MAX_NDIM], gdim[BBOX_MAX_NDIM];
    char lock_name[256], group_name[256];
    sprintf(lock_name, "%s_lock", var->name);
    if (t->rank == 0) {
        uloga("%s: t->rank= %d lock_name= '%s'\n", __func__,
            t->rank, lock_name, group_name);
    }

    ndim = var->gdim.ndim;
    elem_size = var->elem_size;
    // Allocates data
    data = allocate_data(var);
    if (!data) return -1;
    set_ndim(var, &ndim);
    set_gdim(var, gdim);
    set_bbox(t, var, lb, ub);

    // Read data starts ...
    if (use_lock) {
        dspaces_lock_on_read(lock_name, &comm->comm);
    }

    log_read_var(t->rank, var->name, ndim, elem_size, version,
                lb, ub, gdim);

    dimes_define_gdim(var->name, ndim, gdim);
    err = dimes_get(var->name, version, elem_size, ndim, lb, ub, data);
    if (err < 0) {
        uloga("ERROR %s: dimes_get() failed!\n", __func__);
        goto err_out;
    }

    if (use_lock) {
        dspaces_unlock_on_read(lock_name, &comm->comm);
    }
    // Read data ends ...

    if (data) free(data);
    return err;
 err_out:
    if (data) free(data);
    return err;
}

static int write_data(struct cods_task *t, struct cods_var *var, int version, struct parallel_communicator *comm, int use_lock, int use_group) {
    int err;
    double *data = NULL;
    int ndim;
    size_t elem_size;
    uint64_t lb[BBOX_MAX_NDIM], ub[BBOX_MAX_NDIM], gdim[BBOX_MAX_NDIM];
    char lock_name[256], group_name[256];
    sprintf(lock_name, "%s_lock", var->name);
    sprintf(group_name, "%s_group", var->name);
    if (t->rank == 0) {
        uloga("%s: t->rank= %d lock_name= '%s' group_name= '%s'\n", __func__,
            t->rank, lock_name, group_name);
    }

    ndim = var->gdim.ndim;
    elem_size = var->elem_size;
    // Allocates data
    data = allocate_data(var);
    if (!data) return -1;
    generate_data_value(var, data, version);
    set_ndim(var, &ndim);
    set_gdim(var, gdim);
    set_bbox(t, var, lb, ub);

    // Write data starts ...
    if (use_lock) {
        dspaces_lock_on_write(lock_name, &comm->comm);
    }

    if (use_group) {
        dimes_put_sync_group(group_name, version);
        dimes_put_set_group(group_name, version);
    } else {
        dimes_put_sync_all();
    }

    log_write_var(t->rank, var->name, ndim, elem_size, version,
                lb, ub, gdim);

    dimes_define_gdim(var->name, ndim, gdim);
    err = dimes_put(var->name, version, elem_size, ndim, lb, ub, data);
    if (err < 0) {
        uloga("ERROR %s: dimes_put() failed!\n", __func__);
        goto err_out;
    }

    if (use_group) {
        dimes_put_unset_group(group_name, version);
    }

    if (use_lock) {
        dspaces_unlock_on_write(lock_name, &comm->comm);
    }
    // Write data ends ...

    if (data) free(data);
    return err;
 err_out:
    if (data) free(data);
    return err;
} 

static void print_task_info(const struct cods_task *t)
{
    uloga("%s(): task tid= %u appid= %d rank= %d nproc= %d num_vars= %d\n",
        __func__, t->tid, t->appid, t->rank, t->nproc, t->num_vars);

    uint64_t gdim[BBOX_MAX_NDIM], dist[BBOX_MAX_NDIM];
    char gdim_str[256], dist_str[256];
    int i, j;
    for (i = 0; i < t->num_vars; i++) {
        for (j = 0; j < t->vars[i].gdim.ndim; j++) {
            gdim[j] = t->vars[i].gdim.sizes.c[j];
        }
        for (j = 0; j < t->vars[i].dist_hint.ndim; j++) {
            dist[j] = t->vars[i].dist_hint.sizes.c[j];
        }
        int64s_to_str(t->vars[i].gdim.ndim, gdim, gdim_str);
        int64s_to_str(t->vars[i].dist_hint.ndim, dist, dist_str);

        uloga("%s(): task tid= %u appid= %d var '%s' version %d elem_size %u "
            "gdim (%s) dist_hint= (%s)\n",
            __func__, t->tid, t->appid, t->vars[i].name,
            t->vars[i].version, t->vars[i].elem_size,
            gdim_str, dist_str);
    } 
}

static void log_write_var(int rank, const char *var_name, int ndim, int elem_size, int version, uint64_t *lb, uint64_t *ub, uint64_t *gdim) {
    char lb_str[256], ub_str[256], gdim_str[256];
    int64s_to_str(ndim, lb, lb_str);
    int64s_to_str(ndim, ub, ub_str);
    int64s_to_str(ndim, gdim, gdim_str);
    uloga("rank= %d write var= '%s' ndim= %d elem_size= %u version= %d "
        "lb= (%s) ub= (%s) gdim= (%s)\n",
        rank, var_name, ndim, elem_size,
        version, lb_str, ub_str, gdim_str);
}

static void log_read_var(int rank, const char *var_name, int ndim, int elem_size, int version, uint64_t *lb, uint64_t *ub, uint64_t *gdim) {
    char lb_str[256], ub_str[256], gdim_str[256];
    int64s_to_str(ndim, lb, lb_str);
    int64s_to_str(ndim, ub, ub_str);
    int64s_to_str(ndim, gdim, gdim_str);
    uloga("rank= %d read var= '%s' ndim= %d elem_size= %u version= %d "
        "lb= (%s) ub= (%s) gdim= (%s)\n",
        rank, var_name, ndim, elem_size,
        version, lb_str, ub_str, gdim_str);
}

int dummy_dag_task(struct cods_task *t, struct parallel_communicator *comm)
{
    int comm_size, comm_rank;
    MPI_Barrier(comm->comm);
    MPI_Comm_size(comm->comm, &comm_size);
    MPI_Comm_rank(comm->comm, &comm_rank);

    // Print input variable information
    if (t->rank == 0) {
        print_task_info(t);
    }

    unsigned int seconds = t->appid; // TODO: fixme
    sleep(seconds);

    MPI_Barrier(comm->comm);

    if (t->rank == 0) {
        char var_name[MAX_VAR_NAME_LEN];
        sprintf(var_name, "task%d_output_var", t->appid);
    }
        
    return 0;
}

static int sml_mstep = 10;
int dummy_xgc1_task(struct cods_task *t, struct parallel_communicator *comm)
{
    double t1, t2;
    int comm_size, comm_rank;
    MPI_Barrier(comm->comm);
    MPI_Comm_size(comm->comm, &comm_size);
    MPI_Comm_rank(comm->comm, &comm_rank);
    t1 = MPI_Wtime();

    // print variable information
    if (t->rank == 0) {
        print_task_info(t);
    }
    
    // read particle data
    if (comm_rank == 0) {
        uloga("%s(): read particle data\n", __func__);
    }

    // sleep for 1 second
    unsigned int seconds = 1;
    int ts;
    for (ts = 1; ts <= sml_mstep; ts++) {
        sleep(seconds);
        // write turbulence data
        if (comm_rank == 0) {
            uloga("%s(): ts %d write turbulence data\n", __func__, ts);
        }
    }

    // write particle data
    if (comm_rank == 0) {
        uloga("%s(): write particle data\n", __func__);
    }

    MPI_Barrier(comm->comm);
    t2 = MPI_Wtime();

    if (t->rank == 0) {
        uloga("%s(): execution time %lf\n", __func__, t2-t1);
    }
    return 0;
}

int dummy_xgca_task(struct cods_task *t, struct parallel_communicator *comm)
{
    double t1, t2;
    int comm_size, comm_rank;
    MPI_Barrier(comm->comm);
    MPI_Comm_size(comm->comm, &comm_size);
    MPI_Comm_rank(comm->comm, &comm_rank);
    t1 = MPI_Wtime();

    // print variable information
    if (t->rank == 0) {
        print_task_info(t);
    }

    // read particle data
    if (comm_rank == 0) {
        uloga("%s(): read particle data\n", __func__);
    }

    // sleep for 1 second
    unsigned int seconds = 1;
    int ts;
    for (ts = 1; ts <= sml_mstep; ts++) {
        // read turbulence data
        if (comm_rank == 0) {
            uloga("%s(): ts %d read turbulence data\n", __func__, ts);
        }
        sleep(seconds);
    }

    // write particle data
    if (comm_rank == 0) {
        uloga("%s(): write particle data\n", __func__);
    }

    MPI_Barrier(comm->comm);
    t2 = MPI_Wtime();

    if (t->rank == 0) {
        uloga("%s(): execution time %lf\n", __func__, t2-t1);
    }
    return 0;
}

static int s3d_num_ts = 10;
int dummy_s3d_task(struct cods_task *t,
    struct parallel_communicator *comm)
{
    double t1, t2;
    int comm_size, comm_rank;
    MPI_Barrier(comm->comm);
    MPI_Comm_size(comm->comm, &comm_size);
    MPI_Comm_rank(comm->comm, &comm_rank);
    t1 = MPI_Wtime();

    // print variable information
    if (t->rank == 0) {
        print_task_info(t);
    }

    // sleep for 1 second
    unsigned int seconds = 3;
    int ts;
    uint32_t tid = 2;
    for (ts = 1; ts <= s3d_num_ts; ts++) {
        if (comm_rank == 0) {
            uloga("%s(): ts %d\n", __func__, ts);
        }
        sleep(seconds);

        if (comm_rank == 0) {
            // submit analyis operation
            // create task descriptors
            struct task_descriptor task1, task2, task3, task4;
            init_task_descriptor(&task1, tid++, "s3d_viz.conf");
            init_task_descriptor(&task2, tid++, "s3d_stat.conf");
            init_task_descriptor(&task3, tid++, "s3d_topo.conf");
            init_task_descriptor(&task4, tid++, "s3d_indexing.conf");

            // provide placement location hint
            task1.location_hint = intransit_executor; 
            task2.location_hint = insitu_colocated_executor; 
            task3.location_hint = intransit_executor; 
            task4.location_hint = insitu_colocated_executor; 

            // execute tasks and wait for completions
            cods_exec_task(&task1);
            cods_exec_task(&task2);
            cods_exec_task(&task3);
            cods_exec_task(&task4);
            cods_wait_task_completion(task1.tid);
            cods_wait_task_completion(task2.tid);
            cods_wait_task_completion(task3.tid);
            cods_wait_task_completion(task4.tid);
        }
        MPI_Barrier(comm->comm);
    }

    MPI_Barrier(comm->comm);
    t2 = MPI_Wtime();

    if (t->rank == 0) {
        uloga("%s(): execution time %lf\n", __func__, t2-t1);
    }
    return 0;
}

int dummy_s3d_viz_task(struct cods_task *t,
    struct parallel_communicator *comm)
{
    double t1, t2;
    int comm_size, comm_rank;
    MPI_Barrier(comm->comm);
    MPI_Comm_size(comm->comm, &comm_size);
    MPI_Comm_rank(comm->comm, &comm_rank);
    t1 = MPI_Wtime();

    // print input variable information
    if (t->rank == 0) {
        print_task_info(t);
    }

    // sleep for 1 second
    unsigned int seconds = 1;
    sleep(seconds);

    MPI_Barrier(comm->comm);
    t2 = MPI_Wtime();
    if (comm_rank == 0) {
        uloga("%s(): execution time %lf\n", __func__, t2-t1);
    }
    return 0;
}

int dummy_s3d_stat_task(struct cods_task *t,
    struct parallel_communicator *comm)
{
    double t1, t2;
    int comm_size, comm_rank;
    MPI_Barrier(comm->comm);
    MPI_Comm_size(comm->comm, &comm_size);
    MPI_Comm_rank(comm->comm, &comm_rank);
    t1 = MPI_Wtime();

    // print input variable information
    if (t->rank == 0) {
        print_task_info(t);
    }

    // sleep for 1 second
    unsigned int seconds = 1;
    sleep(seconds);

    MPI_Barrier(comm->comm);
    t2 = MPI_Wtime();
    if (comm_rank == 0) {
        uloga("%s(): execution time %lf\n", __func__, t2-t1);
    }
    return 0;
}

int dummy_s3d_topo_task(struct cods_task *t,
    struct parallel_communicator *comm)
{
    double t1, t2;
    int comm_size, comm_rank;
    MPI_Barrier(comm->comm);
    MPI_Comm_size(comm->comm, &comm_size);
    MPI_Comm_rank(comm->comm, &comm_rank);
    t1 = MPI_Wtime();

    // print input variable information
    if (t->rank == 0) {
        print_task_info(t);
    }

    // sleep for 1 second
    unsigned int seconds = 1;
    sleep(seconds);

    MPI_Barrier(comm->comm);
    t2 = MPI_Wtime();
    if (comm_rank == 0) {
        uloga("%s(): execution time %lf\n", __func__, t2-t1);
    }
    return 0;
}

int dummy_s3d_indexing_task(struct cods_task *t,
    struct parallel_communicator *comm)
{
    double t1, t2;
    int comm_size, comm_rank;
    MPI_Barrier(comm->comm);
    MPI_Comm_size(comm->comm, &comm_size);
    MPI_Comm_rank(comm->comm, &comm_rank);
    t1 = MPI_Wtime();

    // print input variable information
    if (t->rank == 0) {
        print_task_info(t);
    }

    // sleep for 1 second
    unsigned int seconds = 1;
    sleep(seconds);

    MPI_Barrier(comm->comm);
    t2 = MPI_Wtime();
    if (comm_rank == 0) {
        uloga("%s(): execution time %lf\n", __func__, t2-t1);
    }
    return 0;
}

static int dns_les_num_ts = 10;
static int dns_les_num_sub_step = 6;
int dummy_dns_task(struct cods_task *t,
    struct parallel_communicator *comm)
{
    double t1, t2;
    int comm_size, comm_rank;
    MPI_Barrier(comm->comm);
    MPI_Comm_size(comm->comm, &comm_size);
    MPI_Comm_rank(comm->comm, &comm_rank);
    t1 = MPI_Wtime();

    // print input variable information
    if (t->rank == 0) {
        print_task_info(t);
    }

    // sleep for 1 second
    unsigned int seconds = 1;
    int i, ts;
    for (ts = 1; ts <= dns_les_num_ts; ts++) {
        for (i = 1; i <= dns_les_num_sub_step; i++) {
            if (comm_rank == 0) {
                uloga("%s(): step %d substep %d\n", __func__, ts, i);
            }

            // write variable            
            char var_name[256];
            sprintf(var_name, "var%d", i);
            struct cods_var *var = lookup_task_var(t, var_name);
            if (var) {
                write_data(t, var, ts, comm, 1, 1);
            }

            sleep(seconds);
        }
        MPI_Barrier(comm->comm);
    }

    t2 = MPI_Wtime();
    
    if (t->rank == 0) {
        uloga("%s(): execution time %lf\n", __func__, t2-t1);
    }
    return 0;
}

int dummy_les_task(struct cods_task *t,
    struct parallel_communicator *comm)
{
    double t1, t2;
    int comm_size, comm_rank;
    MPI_Barrier(comm->comm);
    MPI_Comm_size(comm->comm, &comm_size);
    MPI_Comm_rank(comm->comm, &comm_rank);
    t1 = MPI_Wtime();

    // print input variable information
    if (t->rank == 0) {
        print_task_info(t);
    }

    // sleep for 1 second
    unsigned int seconds = 1;
    int i, ts;
    for (ts = 1; ts <= dns_les_num_ts; ts++) {
        for (i = 1; i <= dns_les_num_sub_step; i++) {
            if (comm_rank == 0) {
                uloga("%s(): step %d substep %d\n", __func__, ts, i);
            }

            // read variable
            char var_name[256];
            sprintf(var_name, "var%d", i);
            struct cods_var *var = lookup_task_var(t, var_name);
            if (var) {
                read_data(t, var, ts, comm, 1);
            }

            sleep(seconds);
        }
        MPI_Barrier(comm->comm);
    }

    t2 = MPI_Wtime();

    if (t->rank == 0) {
        uloga("%s(): execution time %lf\n", __func__, t2-t1);
    }
    return 0;
}

#define RENDER_VAR 3
float my_data_quantize(float value, int varname)
{
    float min, max;

    if (3 == RENDER_VAR) {
        //min = 2.5;
        //max = 15;
        min = 1.22;
        max = 7.2;
    }

    if (4 == RENDER_VAR) {
        min = 1.22;
        max = 7.2;
    }

    //min = -5.5e-04;
    //max = 5.5e-04;

    if( max == min) {
        return value;
    }

    float v = (value - min) / (max - min);

    if (v < 0) {
        v = 0;
    }
    if (v > 1) {
        v = 1;
    }
    return v;
}

// TODO: this task routine is highly hard-coded...
/*
int task_viz_render(struct cods_task *t, struct parallel_communicator *comm)
{
    int err;
    int npx, npy, npz, mypx, mypy, mypz;
    int domain_grid_size[3];
    int render_root = 0;
    double tm_st, tm_end;

    MPI_Comm_rank(comm->comm, &t->rank);
    MPI_Barrier(comm->comm);
    tm_st = MPI_Wtime();

    // read input data
    double *data = NULL;
    int xl, yl, zl, xu, yu, zu;
    size_t elem_size = sizeof(double);
    struct cods_var *var_desc;

    var_desc = &(t->vars[0]);
    set_data_decomposition(t, var_desc);
    if (!viz_info.is_viz_init) {
        data = allocate_data(g.spx*g.spy*g.spz);
        viz_info.viz_data = data;
    } else {
        data = viz_info.viz_data;
    }
    generate_bbox(&g, &xl, &yl, &zl, &xu, &yu, &zu);
    err = cods_get_var(var_desc->name, var_desc->version, elem_size,
            xl, yl, zl, xu, yu, zu, data, NULL);
    if (err < 0) {
        uloga("%s(): failed to get var\n", __func__);
        return -1;
    }

    MPI_Barrier(comm->comm);
    tm_end = MPI_Wtime();
    if (t->rank == 0) {
        uloga("%s(): fetch data time %lf\n", __func__, tm_end-tm_st);
    }

    tm_st = tm_end;
    // perform viz. render
    npx = g.npx;
    npy = g.npy;
    npz = g.npz;
    domain_grid_size[0] = g.npx * g.spx;
    domain_grid_size[1] = g.npy * g.spy;
    domain_grid_size[2] = g.npz * g.spz;
    mypx = g.rank % g.npx;
    mypy = g.rank / g.npx % g.npy;
    mypz = g.rank / g.npx / g.npy;

    //uloga("%s(): step %d rank %d npx %d npy %d npz %d "
    //    "domain_grid_size[] = {%d, %d, %d} "
    //    "mypx %d mypy %d mypz %d\n", __func__,
    //    t->step, g.rank, npx, npy, npz, 
    //    domain_grid_size[0], domain_grid_size[1], domain_grid_size[2],
    //    mypx, mypy, mypz);

    // compute_stats(var_desc->var_name, data, g.spx*g.spy*g.spz, t->rank);
    if (!viz_info.is_viz_init) {
        hpgv_insituvis_init_(g.rank, comm->comm, render_root,
                    npx, npy, npz, mypx, mypy, mypz,
                    domain_grid_size, data);
        viz_info.is_viz_init = 1;
    }

    hpgv_insitu_render_tstep_(g.rank, comm->comm, var_desc->version, my_data_quantize);

    MPI_Barrier(comm->comm);
    tm_end = MPI_Wtime();
    if (t->rank == 0) {
        uloga("%s(): render data time %lf\n", __func__, tm_end-tm_st);
    }

    return 0; 
}
*/

/*
int task_fb_indexing(struct cods_task *t, struct parallel_communicator *comm)
{
    int err;
    double tm_st, tm_end, tm_end1;

    MPI_Comm_rank(comm->comm, &t->rank);

    // read input data
    MPI_Barrier(comm->comm);
    tm_st = MPI_Wtime();

    double *data = NULL;
    int xl, yl, zl, xu, yu, zu;
    struct cods_var *var_desc = &(t->vars[0]);
    size_t elem_size = var_desc->elem_size;
    int num_double_elem = 0;

    set_data_decomposition(t, var_desc);
    num_double_elem = (g.spx*g.spy*g.spz)*elem_size/sizeof(double);
    data = allocate_data(num_double_elem);
    generate_bbox(&g, &xl, &yl, &zl, &xu, &yu, &zu);
    err = cods_get_var(var_desc->name, var_desc->version, elem_size,
            xl, yl, zl, xu, yu, zu, data, NULL);
    if (err < 0) {
        uloga("%s(): failed to get var\n", __func__);
        return -1;
    }

    void* cas;
    const char *indopt = "<binning start=0 end=1000 nbins=100 scale=simple/>";
    char *colname = "var";
    int size_index = 0;
    cas = fb_build_index_double(data, num_double_elem, indopt, (void*)colname, &size_index); 
*/
    // perform I/O
/**
    char index_fname[256];
    sprintf(index_fname, "fb_index_process%d.dat", t->rank);
    FILE *f = fopen(index_fname, "a");
    if (f == NULL) {
        uloga("%s(): failed to create file %s\n", __func__, index_fname);
    } else {
        char *data_buf = (char*)malloc(size_index);
        memset(data_buf, 0, size_index);
        size_t size = size_index;
        size_t offset = 0;
        size_t block_size = 512*1024; // 512KB
        size_t bytes;
        while (offset < size) {
            if ((size-offset) >= block_size) {
                bytes = block_size;
            } else {
                bytes = (size-offset);
            }
            fwrite(data_buf+offset, 1, bytes, f);
            offset += bytes;
        }
        fclose(f); 
        free(data_buf);
    } 
**/
/*
    tm_end = MPI_Wtime();
    if (cas == 0) {
        uloga("%s(): fb_build_index_double failed\n", __func__);
    }
  
    MPI_Barrier(comm->comm);
    tm_end1 = MPI_Wtime();
 
    uloga("%s(): rank %d size_index %d build_index_time %lf\n", __func__,
        t->rank, size_index, tm_end-tm_st);
    if (t->rank == 0) {
        uloga("%s(): indexing data time %lf\n", __func__, tm_end1-tm_st);
    }

    free(data);
    return 0;
}
*/

/*
int dummy_sample_dag_workflow(MPI_Comm comm)
{
    int err;
    int mpi_rank, mpi_nproc;
    MPI_Comm_size(comm, &mpi_nproc);
    MPI_Comm_rank(comm, &mpi_rank);

    communicator_init(comm);

    int appid;
    for (appid = 1; appid < MAX_NUM_TASKS; appid++) {
        register_task_function(appid, dag_task);
    }
    int pool_id = 1;
    cods_register_executor(pool_id, mpi_nproc, mpi_rank);
    MPI_Barrier(comm);

	struct cods_task t;
	while (!cods_request_task(&t)) {
		cods_put_sync_all();
		err = exec_task_function(&t);	
		if (err < 0) {
			return err;
		}
	}

    cods_put_sync_all();
    communicator_free();

	return 0;
 err_out:
	return -1;
}
*/

/*
int s3d_viz_render_task(struct cods_task *t,
    struct parallel_communicator *comm)
{
    double t1, t2;
    int comm_size, comm_rank;
    MPI_Barrier(comm->comm);
    MPI_Comm_size(comm->comm, &comm_size);
    MPI_Comm_rank(comm->comm, &comm_rank);
    t1 = MPI_Wtime();

    // print input variable information
    if (t->rank == 0) {
        print_task_info(t);
    }

    // lookup input data var
    struct cods_var *var = lookup_task_var(t, "s3d_data_viz");
    if (!var) {
        uloga("ERROR %s: failed to lookup task var 's3d_data_viz'\n", __func__);
        return 0;
    }

    // read input data
    int err;
    uint64_t lb[BBOX_MAX_NDIM], ub[BBOX_MAX_NDIM], gdim[BBOX_MAX_NDIM];
    double *data = NULL;
    int version = 0;
    int ndim = var->gdim.ndim;    
    size_t elem_size = var->elem_size;
    char lock_name[256];
    sprintf(lock_name, "%s_lock", var->name);
    
    data = allocate_data(var);
    set_ndim(var, &ndim);
    set_gdim(var, gdim);
    set_bbox(t, var, lb, ub);

    dspaces_lock_on_read(lock_name, &comm->comm);
    log_read_var(t->rank, var->name, ndim, elem_size, version,
            lb, ub, gdim); 
    dimes_define_gdim(var->name, ndim, gdim);
    err = dimes_get(var->name, version, elem_size, ndim, lb, ub, data);
    if (err < 0) {
        uloga("ERROR %s: dimes_get() failed\n", __func__);
    }
    dspaces_unlock_on_read(lock_name, &comm->comm);

    // check data values
    uint64_t sp[BBOX_MAX_NDIM];
    uint64_t num_elem = 1;
    int i;
    generate_sp(var, sp);
    for (i = 0; i < var->gdim.ndim; i++) {
        num_elem *= sp[i];
    }
    compute_stats(var->name, data, (num_elem*var->elem_size)/sizeof(double), t->rank);
    if (data) free(data);

    MPI_Barrier(comm->comm);
    t2 = MPI_Wtime();
    if (comm_rank == 0) {
        uloga("%s(): execution time %lf\n", __func__, t2-t1);
    }
    return 0;
}
*/

/*
int s3d_fb_indexing_task(struct cods_task *t,
    struct parallel_communicator *comm)
{
    double t1, t2;
    double read_data_time = 0, build_index_time = 0;
    int comm_size, comm_rank;

    t1 = MPI_Wtime();
    read_data_time = t1;
    MPI_Barrier(comm->comm);
    MPI_Comm_size(comm->comm, &comm_size);
    MPI_Comm_rank(comm->comm, &comm_rank);

    // print input variable information
    if (t->rank == 0) {
        print_task_info(t);
    }

    // lookup input data var
    struct cods_var *var = lookup_task_var(t, "s3d_data_fb");
    if (!var) {
        uloga("ERROR %s: failed to lookup task var 's3d_data_fb'\n", __func__);
        return 0;
    }

    // read input data
    int err;
    uint64_t lb[BBOX_MAX_NDIM], ub[BBOX_MAX_NDIM], gdim[BBOX_MAX_NDIM];
    double *data = NULL;
    int version = 0;
    int ndim = var->gdim.ndim;    
    size_t elem_size = var->elem_size;
    char lock_name[256];
    sprintf(lock_name, "%s_lock", var->name);
    
    data = allocate_data(var);
    set_ndim(var, &ndim);
    set_gdim(var, gdim);
    set_bbox(t, var, lb, ub);

    dspaces_lock_on_read(lock_name, &comm->comm);
    // log_read_var(t->rank, var->name, ndim, elem_size, version,
    //        lb, ub, gdim); 
    dimes_define_gdim(var->name, ndim, gdim);
    err = dimes_get(var->name, version, elem_size, ndim, lb, ub, data);
    if (err < 0) {
        uloga("ERROR %s: dimes_get() failed\n", __func__);
    }
    dspaces_unlock_on_read(lock_name, &comm->comm);
    t2 = MPI_Wtime();
    read_data_time = (t2-read_data_time);
    build_index_time = t2;

    // check data values
    uint64_t sp[BBOX_MAX_NDIM];
    uint64_t num_elem = 1;
    int i;
    generate_sp(var, sp);
    for (i = 0; i < var->gdim.ndim; i++) {
        num_elem *= sp[i];
    }
    //compute_stats(var->name, data, (num_elem*var->elem_size)/sizeof(double), t->rank);

    // build fb indexing
    void *cas;
    const char *indopt = "<binning start=0 end=1000 nbins=100 scale=simple/>";
    char *col_name = "var";
    int num_double_elem = ((int)num_elem*var->elem_size)/sizeof(double);
    int size_index = 0;
    cas = fb_build_index_double(data, num_double_elem, indopt, (void*)col_name,
            &size_index);
    if (cas == 0) {
        printf("%s failed\n", __func__);
    }
    build_index_time = (MPI_Wtime()-build_index_time);    

    if (data) free(data);
    MPI_Barrier(comm->comm);
    t2 = MPI_Wtime();
    uloga("%s(): task tid= %u rank %d read_data_time %lf\n", __func__,
        t->tid, comm_rank, read_data_time);
    uloga("%s(): task tid= %u rank %d build_index_time %lf\n", __func__,
        t->tid, comm_rank, build_index_time);
    if (comm_rank == 0) {
        uloga("%s(): task tid= %u execution time %lf\n", __func__,
            t->tid, t2-t1);
    }
    return 0;
}
*/

int dummy_epsi_coupling_workflow(MPI_Comm comm)
{
    int err;
    int mpi_rank, mpi_nproc;
    MPI_Comm_size(comm, &mpi_nproc);
    MPI_Comm_rank(comm, &mpi_rank);

    communicator_init(comm);

    register_task_function(1, dummy_xgc1_task);
    register_task_function(2, dummy_xgca_task);

    int pool_id = 1;
    cods_register_executor(pool_id, mpi_nproc, comm);
    MPI_Barrier(comm);

    struct cods_task t;
    while (!cods_request_task(&t)) {
        err = exec_task_function(&t);
        if (err < 0) {
            return err;
        }
    }
    communicator_free();

    return 0;
}

int dummy_s3d_analysis_workflow(MPI_Comm comm)
{
    int err;
    int mpi_rank, mpi_nproc;
    MPI_Comm_size(comm, &mpi_nproc);
    MPI_Comm_rank(comm, &mpi_rank);

    communicator_init(comm);

    register_task_function(1, dummy_s3d_task);
    register_task_function(2, dummy_s3d_viz_task);
    register_task_function(3, dummy_s3d_stat_task); 
    register_task_function(4, dummy_s3d_topo_task);
    register_task_function(5, dummy_s3d_indexing_task);

    int pool_id = 1;
    cods_register_executor(pool_id, mpi_nproc, comm);
    MPI_Barrier(comm);

    struct cods_task t;
    while (!cods_request_task(&t)) {
        err = exec_task_function(&t);
        if (err < 0) {
            return err;
        }
    }
    communicator_free();

    return 0;
}

int dummy_dns_les_workflow(MPI_Comm comm)
{
    int err;
    int mpi_rank, mpi_nproc;
    MPI_Comm_size(comm, &mpi_nproc);
    MPI_Comm_rank(comm, &mpi_rank);

    communicator_init(comm);

    register_task_function(1, dummy_dns_task);
    register_task_function(2, dummy_les_task);

    int pool_id = 1;
    cods_register_executor(pool_id, mpi_nproc, comm);
    MPI_Barrier(comm);

    struct cods_task t;
    while (!cods_request_task(&t)) {
        err = exec_task_function(&t);
        if (err < 0) {
            return err;
        }
    }
    communicator_free();

    return 0;
}

/*
int s3d_analysis_workflow(MPI_Comm comm)
{
    int err;
    int mpi_rank, mpi_nproc;
    MPI_Comm_size(comm, &mpi_nproc);
    MPI_Comm_rank(comm, &mpi_rank);

    communicator_init(comm);

    register_task_function(1, s3d_viz_render_task);
    register_task_function(2, s3d_fb_indexing_task);

    int pool_id = 1;
    cods_register_executor(pool_id, mpi_nproc, comm);
    MPI_Barrier(comm);

    struct cods_task t;
    while (!cods_request_task(&t)) {
        err = exec_task_function(&t);
        if (err < 0) {
            return err;
        }
    }
    communicator_free();

    return 0;
}
*/
