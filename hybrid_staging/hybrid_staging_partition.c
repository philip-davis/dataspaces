//#include <sched.h>

#include "hybrid_staging_partition.h"

#include "common.h"
#include "debug.h"

#include "assert.h"
#include "pmi.h"
#include "gni_pub.h"
#include "utility_functions.h"

static int nprocs_, rank_;

static char name_[MPI_MAX_PROCESSOR_NAME];
static unsigned int nid_;
static int pid_;
static int pid_local_rank_; // rank of pid values on local node

// Will perform 2-level split of mpi communicator
static MPI_Comm l1_src_comm_, l1_new_comm_;
static MPI_Comm l2_src_comm_, l2_new_comm_;
static int l1_color_, l2_color_;

struct worker_info {
	unsigned int nid;
	unsigned int pid;
};

#define CORES_PER_NODE 16
#define INSITU_STAGING_CORES_PER_NODE 2 
static struct worker_info local_worker_tab_[CORES_PER_NODE];

int hs_comm_init(int argc, char **argv)
{
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs_);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank_);
    MPI_Barrier(MPI_COMM_WORLD);

	int resultlen = 0;
	int device_id = 0;
	nid_ = get_gni_nic_address(device_id);
	pid_ = getpid();
	MPI_Get_processor_name(name_, &resultlen);
	name_[resultlen] = 0;

	return 0;
}

int hs_comm_fin()
{
	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Finalize();
}

int hs_comm_get_rank()
{
	return rank_;
}

int hs_comm_get_nprocs()
{
	return nprocs_;
}

static enum worker_type get_worker_type(MPI_Comm comm, enum execution_mode mode, enum location_type loc)
{
	int num_workers;
	MPI_Comm_size(comm, &num_workers);
	unsigned int sendbuf[2];
	unsigned int *recvbuf = (unsigned int*)
			malloc(sizeof(unsigned int)*2*num_workers);

	sendbuf[0] = nid_;
	sendbuf[1] = pid_;
	int send_cnt = 2;
	int recv_cnt = 2;

	MPI_Allgather(sendbuf, send_cnt, MPI_UNSIGNED, recvbuf, recv_cnt,
			MPI_UNSIGNED, comm);	

	int i, cnt = 0;
	for (i = 0; i < num_workers; i++) {
		if (recvbuf[i*recv_cnt] == nid_) {
			local_worker_tab_[cnt].nid = recvbuf[i*recv_cnt];
			local_worker_tab_[cnt].pid = recvbuf[i*recv_cnt+1];
			if (local_worker_tab_[cnt].pid < pid_) {
				pid_local_rank_++;
			}
			cnt++;
		}
	}

	uloga("nid= %u pid= %u pid_local_rank= %d local_worker_tab %d entries!\n",
		nid_, pid_, pid_local_rank_, cnt);

	if (loc == intransit) {
		return staging_worker;
	}

	if (mode == staging_mode && loc == insitu) {
		return simulation_worker;
	}

	if (mode == hybrid_staging_mode && loc == insitu) {
		if (pid_local_rank_ < INSITU_STAGING_CORES_PER_NODE)
			return staging_worker;
		else return simulation_worker;	
	} 
}

int hs_comm_perform_split(enum execution_mode mode, enum core_type coretype, enum location_type loc, enum worker_type* workertype)
{
	// Perform level 1 split
	l1_src_comm_ = MPI_COMM_WORLD;
	l1_color_ = coretype;
	MPI_Comm_split(l1_src_comm_, l1_color_, rank_, &l1_new_comm_);

	// Decide the worker type
	if (coretype == worker_core) {
		*workertype = get_worker_type(l1_new_comm_, mode, loc);
		// Perform level 2 split
		l2_src_comm_ = l1_new_comm_;
		l2_color_ = *workertype;
		MPI_Comm_split(l2_src_comm_, l2_color_, rank_, &l2_new_comm_);

		int num_worker = 0;
		MPI_Comm_size(l2_new_comm_, &num_worker);
		switch (*workertype) {
		case simulation_worker:
			uloga("simulation: num_worker= %d location= %u nid= %u, pid= %d\n",
				num_worker, loc, nid_, pid_);
			break;
		case staging_worker:
			uloga("staging: num_worker=%d location= %u nid= %u, pid= %d\n",
				num_worker, loc, nid_, pid_);
			break;
		default:
			uloga("wrong workertype as %u\n", *workertype);
			return -1;
		}

	} else if (coretype == manager_core) {
		*workertype = 0;
	}

	MPI_Barrier(MPI_COMM_WORLD);
	return 0;
}

MPI_Comm hs_get_communicator_l1() {
	return l1_new_comm_;
}

MPI_Comm hs_get_communicator_l2() {
	return l2_new_comm_;
}
