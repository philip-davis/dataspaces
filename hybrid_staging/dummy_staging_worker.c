#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "common.h"
#include "hybrid_staging_api.h"
#include "mpi.h"

typedef int (*task_function)(struct task_descriptor *t);

static struct {
	int tid;
	task_function func_ptr;
} parallel_tasks[64];
static int num_tasks = 0;

static struct g_info g;

static int add_task_function(int tid, task_function func_ptr)
{
	parallel_tasks[num_tasks].tid = tid;
	parallel_tasks[num_tasks].func_ptr = func_ptr;
	num_tasks++;
}

static int exec_task_function(struct task_descriptor *t)
{
	int i;
	for (i = 0; i < num_tasks; i++) {
		if (t->tid == parallel_tasks[i].tid) {
			return (parallel_tasks[i].func_ptr)(t);
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

	uloga("%s(): g.npx= %d g.npy= %d g.npz= %d g.spx= %d g.spy= %d g.spz= %d\n",
		__func__,g.npx,g.npy,g.npz,g.spx,g.spy,g.spz);
}

static int read_input_data(struct task_descriptor *t)
{
	int i;
	int err;

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
			err = validate_data(databuf, t->step, g.spx*g.spy);
		} else if (var_desc->bb.num_dims == 3) {
			err = validate_data(databuf, t->step, g.spx*g.spy*g.spz);
		}

		if (!err) {
			uloga("%s(): tid= %d step= %d rank= %d nproc= %d "
				"var '%s' validate_data() failed\n", __func__,
				t->tid, t->step, t->rank, t->nproc, var_desc->var_name);
		}

		free(databuf);
	}

	uloga("%s(): task tid= %d step= %d rank= %d nproc= %d "
		"num_input_vars= %d\n",
		__func__, t->tid, t->step, t->rank, t->nproc, t->num_input_vars);
	
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
	write_output_data(t, "topology_var2");
	return 0;
}

int task2(struct task_descriptor *t)
{
	read_input_data(t);
	// Do some computation
	write_output_data(t, "stat_var2");
	return 0;
} 

int task3(struct task_descriptor *t)
{
	read_input_data(t);
	// Do some computation
	write_output_data(t, "viz_var2");
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

int dummy_s3d_staging_parallel_job(MPI_Comm comm)
{
	int i, err;
	int nprocs, mpi_rank;
	
	MPI_Comm_size(comm, &nprocs);
	MPI_Comm_rank(comm, &mpi_rank);
	if (mpi_rank == 0) {
		uloga("Dummy S3D staging: total %d workers\n", nprocs);
	}

	add_task_function(1, task1);
	add_task_function(2, task2);
	add_task_function(3, task3);	
	add_task_function(4, task4);	
	add_task_function(5, task5);	
	add_task_function(6, task6);	

	struct task_descriptor t;
	while ( !hstaging_request_task(&t)) {
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
