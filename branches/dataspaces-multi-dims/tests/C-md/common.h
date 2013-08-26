#ifndef __TEST_COMMON_H_
#define __TEST_COMMON_H_

#include "debug.h"
#include "timer.h"
#include "dataspaces.h"
#include "ds_gspace.h"
#ifdef DS_HAVE_DIMES
#include "dimes_server.h"
#endif

//typedef unsigned long long __u64;

enum transport_type {
	USE_DSPACES = 0,
	USE_DIMES
};

int common_init(int num_peers, int appid);

void common_set_storage_type(int fst); 

int common_rank();

int common_peers();

void common_barrier();

void common_finalize();

void common_lock_on_read(const char *lock_name, void *gcomm);

void common_unlock_on_read(const char *lock_name, void *gcomm);

void common_lock_on_write(const char *lock_name, void *gcomm);

void common_unlock_on_write(const char *lock_name, void *gcomm);

int common_put (const char *var_name,
        unsigned int ver, int size,
	int ndim,
        //int xl, int yl, int zl, int l4,
	int *lb, int *ub,
        //int xu, int yu, int zu, int u4,
        void *data, enum transport_type type);

int common_get (const char *var_name,
        unsigned int ver, int size,
        //int xl, int yl, int zl, int l4,
        //int xu, int yu, int zu, int u4,
	int ndim,
	int *lb, int *ub,
        void *data, enum transport_type type);

int common_put_sync(enum transport_type type);

int common_run_server(int num_sp, int num_cp, enum transport_type type);

//void check_data(double *buf, int num_elem, int rank);
void check_data(double *buf, __u64 num_elem, int rank);

#endif //end of __TEST_COMMON_H_
