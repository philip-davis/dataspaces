#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "common.h"
#include "hybrid_staging_api.h"
#include "mpi.h"

struct parallel_communicator {
    int level;
    int color;
    MPI_Comm comm;
};
static struct parallel_communicator comms[MAX_NUM_SPLIT_LEVEL];

typedef int (*task_function)(struct task_descriptor *t);
struct parallel_task {
	int tid;
	task_function func_ptr;
};
static struct parallel_task ptasks[64];
static int num_tasks_ = 0;

static enum hstaging_location_type loctype_;
static struct g_info g;

static struct parallel_communicator* parallel_comm_lookup(int color)
{
    int i;
    for (i = 0; i < MAX_NUM_SPLIT_LEVEL; i++) {
        if (comms[i].color == color) {
            return &(comms[i]);
        }
    }

    return NULL; 
}

static int parallel_comm_init()
{
    int i;
    for (i = 0; i < MAX_NUM_SPLIT_LEVEL; i++) {
        comms[i].level = -1;
        comms[i].color = -1;
    }

    return 0;    
}

void recursive_split_mpi_comm(int current_level, int color)
{
    int rank, nproc;
    int l = current_level;
    MPI_Comm_rank(comms[l].comm, &rank);
    MPI_Comm_size(comms[l].comm, &nproc);
    if (rank == 0) {
        uloga("%s(): loctype %d level %d color %d nproc %d\n",
            __func__, loctype_, current_level, color, nproc);
    } 

    if (nproc <= BK_GROUP_BASIC_SIZE) return; // Done!
    if (0 != (nproc % BK_GROUP_BASIC_SIZE)) {
        uloga("%s(): nproc is not multiply of %d\n",
            __func__, nproc, BK_GROUP_BASIC_SIZE);
        return;
    }
    if (current_level+1 >= MAX_NUM_SPLIT_LEVEL) return;

    int left_child_size, right_child_size;
    int left_child_color = color * 2;
    int right_child_color = color * 2 + 1;

    left_child_size = nproc / 2;
    if ((left_child_size % BK_GROUP_BASIC_SIZE) != 0) {
        int t = left_child_size / BK_GROUP_BASIC_SIZE;
        left_child_size = (t+1) * BK_GROUP_BASIC_SIZE;
    }     
    right_child_size = nproc - left_child_size;

    if (rank < left_child_size) {
        MPI_Comm_split(comms[l].comm, left_child_color, rank, &(comms[l+1].comm));
        comms[l+1].level = l+1;
        comms[l+1].color = left_child_color;
        recursive_split_mpi_comm(l+1, left_child_color);
    } else {
        MPI_Comm_split(comms[l].comm, right_child_color, rank, &(comms[l+1].comm));
        comms[l+1].level = l+1;
        comms[l+1].color = right_child_color;
        recursive_split_mpi_comm(l+1, right_child_color);
    }
}

static void task_done(struct task_descriptor *t)
{
    struct parallel_communicator *comm;
    comm = parallel_comm_lookup(t->color);
    if (comm) {
        MPI_Barrier(comm->comm);
        if (t->rank == 0) { 
            uloga("%s(): send task_done msg\n", __func__);
            hstaging_set_task_done(t);
        }
    } else {
        uloga("%s(): error comm == NULL\n", __func__);
    }
}

static int add_task_function(int tid, task_function func_ptr)
{
	ptasks[num_tasks_].tid = tid;
	ptasks[num_tasks_].func_ptr = func_ptr;
	num_tasks_++;
}

static int exec_task_function(struct task_descriptor *t)
{
	int i, err;
	for (i = 0; i < num_tasks_; i++) {
		if (t->tid == ptasks[i].tid) {
			err = (ptasks[i].func_ptr)(t);
            task_done(t); 
            return err;
		}
	}

	uloga("%s(): unknown tid= %d", __func__, t->tid);
	return -1;
}

static int update_var(const char *var_name, int ts)
{
    int err;
    struct var_descriptor var_desc;
    int *c = NULL;
    size_t elem_size = sizeof(double);

    strcpy(var_desc.var_name, var_name);
    var_desc.step = ts;
    var_desc.bb.num_dims = g.dims;
    var_desc.size = elem_size;
    c = var_desc.bb.lb.c;
    c[0] = 0;
    c[1] = 0;
    c[2] = 0;
    c = var_desc.bb.ub.c;
    c[0] = (g.npx * g.spx) - 1;
    c[1] = (g.npy * g.spy) - 1;
    c[2] = (g.npz * g.spz) - 1;
    err = hstaging_update_var(&var_desc, OP_PUT);
    return err;
}

static void set_data_decomposition(struct task_descriptor *t, struct var_descriptor *var_desc)
{
	g.rank = t->rank;
	g.dims = var_desc->bb.num_dims;

	if (var_desc->bb.num_dims == 2) {
		g.npx = t->nproc;
		g.npy = 1;
		g.npz = 0;
		g.spx = (var_desc->bb.ub.c[0]+1)/g.npx;
		g.spy = (var_desc->bb.ub.c[1]+1)/g.npy;
		g.spz = 0;
	} else if (var_desc->bb.num_dims == 3) {
		g.npx = t->nproc;
		g.npy = 1;
		g.npz = 1;
		g.spx = (var_desc->bb.ub.c[0]+1)/g.npx;
		g.spy = (var_desc->bb.ub.c[1]+1)/g.npy;
		g.spz = (var_desc->bb.ub.c[2]+1)/g.npz;
	}

    /*
	uloga("%s(): g.npx= %d g.npy= %d g.npz= %d g.spx= %d g.spy= %d g.spz= %d\n",
		__func__,g.npx,g.npy,g.npz,g.spx,g.spy,g.spz);
    */
}

static int read_input_data(struct task_descriptor *t)
{
	int i;
	int err;
    struct parallel_communicator *comm;
    comm = parallel_comm_lookup(t->color);

	for (i = 0; i < t->num_input_vars; i++) {
		double *databuf = NULL;
		int xl, yl, zl, xu, yu, zu;
		size_t elem_size = sizeof(double);
		struct var_descriptor *var_desc;

		var_desc = &(t->input_vars[i]);
		set_data_decomposition(t, var_desc);
		if (var_desc->bb.num_dims == 2) {
			databuf = allocate_data(g.spx*g.spy);					
		} else if (var_desc->bb.num_dims == 3) {
			databuf = allocate_data(g.spx*g.spy*g.spz);
		}		
		generate_bbox(&g, &xl, &yl, &zl, &xu, &yu, &zu);
        err = hstaging_get_var(var_desc->var_name, t->step, elem_size,
                xl, yl, zl, xu, yu, zu, databuf, NULL);	
		if (err < 0) {
			uloga("%s(): failed to get var\n", __func__);
			return -1;
		}

		if (var_desc->bb.num_dims == 2) {
			//err = validate_data(databuf, t->step, g.spx*g.spy);
			compute_stats(var_desc->var_name, databuf, g.spx*g.spy, t->rank);
		} else if (var_desc->bb.num_dims == 3) {
			//err = validate_data(databuf, t->step, g.spx*g.spy*g.spz);
			compute_stats(var_desc->var_name, databuf, g.spx*g.spy*g.spz,t->rank);
		}

		free(databuf);
	}

	uloga("%s(): task color= %d tid= %d step= %d rank= %d nproc= %d "
		"num_input_vars= %d\n", __func__, t->color, t->tid, t->step, 
        t->rank, t->nproc, t->num_input_vars);
	
	return 0;
}

static int write_output_data(struct task_descriptor *t, const char *var_name)
{
	double *databuf = NULL;
	int elem_size, num_elem, xl, yl, zl, xu, yu, zu;
	int err;

	int num_peer, dimx, dimy, dimz;
	char fname[128];
	sprintf(fname, "task%d.conf", t->tid);
	err = read_task_info(fname, &num_peer, &g.npx, &g.npy, &g.npz,
						&g.dims, &dimx, &dimy, &dimz);
	if (err < 0 ) {
		uloga("Failed to read %s\n", fname);
		return err;
	}

	if (t->nproc != num_peer) {
		uloga("Warning: t->nproc= %d but num_peer= %d\n", t->nproc, num_peer);
	}

	if (g.dims == 2) {
		g.spx = dimx / g.npx;
		g.spy = dimy / g.npy;
		num_elem = g.spx * g.spy;
	} else if (g.dims == 3) {
		g.spx = dimx / g.npx;
		g.spy = dimy / g.npy;
		g.spz = dimz / g.npz;
		num_elem = g.spx * g.spy * g.spz;
	}
	databuf = allocate_data(num_elem);
	generate_data(databuf, t->step, num_elem);
	elem_size = sizeof(double);
	generate_bbox(&g, &xl, &yl, &zl, &xu, &yu, &zu);

	err = hstaging_put_var(var_name, t->step, elem_size,
			xl, yl, zl, xu, yu, zu, databuf, NULL);
	if (err < 0) {
		free(databuf);
		return err;
	}
	
	if (t->rank == 0) {
		update_var(var_name, t->step);
	}

	free(databuf);
	return err;
} 

int task1(struct task_descriptor *t)
{
	read_input_data(t);
	// Do some computation
	// write_output_data(t, "topology_var2");
	return 0;
}

int task2(struct task_descriptor *t)
{
	read_input_data(t);
	// Do some computation
	// write_output_data(t, "stat_var2");
	return 0;
} 

int task3(struct task_descriptor *t)
{
	read_input_data(t);
	// Do some computation
	// write_output_data(t, "viz_var2");
	return 0;
} 

int task4(struct task_descriptor *t)
{
	read_input_data(t);
	// Do some computation
	return 0;
} 

int task5(struct task_descriptor *t)
{
	read_input_data(t);
	// Do some computation
	return 0;
} 

int task6(struct task_descriptor *t)
{
	read_input_data(t);
	// Do some computation
	return 0;
} 

int dummy_s3d_staging_parallel_job(MPI_Comm comm, enum hstaging_location_type loc_type)
{
    int err;
    int level, color;
    int mpi_rank, mpi_nproc;
    MPI_Comm_size(comm, &mpi_nproc);
    MPI_Comm_rank(comm, &mpi_rank);
    if ((mpi_nproc % BK_GROUP_BASIC_SIZE) != 0) {
        uloga("%s(): error size needs to be multiply of %d\n",
            __func__, BK_GROUP_BASIC_SIZE);
        return -1;
    }

    parallel_comm_init();
	loctype_ = loc_type;	
    level = 0;
    color = 1;
    comms[level].level = level;
    comms[level].color = color;
    comms[level].comm = comm;
    recursive_split_mpi_comm(level, color);

	add_task_function(1, task1);
	add_task_function(2, task2);
	add_task_function(3, task3);	
	add_task_function(4, task4);	
	add_task_function(5, task5);	
	add_task_function(6, task6);	

    hstaging_register_bucket_resource(loctype_, mpi_nproc, mpi_rank);
    MPI_Barrier(comm);

	struct task_descriptor t;
	while (!hstaging_request_task(&t)) {
		hstaging_put_sync_all();
		err = exec_task_function(&t);	
		if (t.num_input_vars > 0 && t.input_vars) {
			free(t.input_vars);
		}
		if (err < 0) {
			return err;
		}
	}

	return 0;
 err_out:
	return -1;
}