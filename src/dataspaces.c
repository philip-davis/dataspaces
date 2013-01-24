/*
 * Copyright (c) 2009, NSF Cloud and Autonomic Computing Center, Rutgers University
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided
 * that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, this list of conditions and
 * the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
 * the following disclaimer in the documentation and/or other materials provided with the distribution.
 * - Neither the name of the NSF Cloud and Autonomic Computing Center, Rutgers University, nor the names of its
 * contributors may be used to endorse or promote products derived from this software without specific prior
 * written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

/*
*  Ciprian Docan (2009)  TASSL Rutgers University
*  docan@cac.rutgers.edu
*  Tong Jin (2011) TASSL Rutgers University
*  tjin@cac.rutgers.edu
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#include "debug.h"
#include "dc_gspace.h"
#include "ss_data.h"
#include "timer.h"
#include "dataspaces.h"

/* Name mangling for C functions to adapt Fortran compiler */
#define FC_FUNC(name,NAME) name ## _

static struct dcg_space *dcg;
static struct timer timer;
static int sync_op_id;
static int cq_id = -1;
static enum storage_type st = column_major;
// TODO: 'num_dims' is hardcoded to 2.
static int num_dims = 2;

static void lib_exit(void)
{
        dcg_free(dcg);
        exit(EXIT_FAILURE);
}

#define ERROR_TRACE_AND_EXIT()					\
do {								\
	uloga("'%s()': failed with %d.\n", __func__, err);	\
	lib_exit();						\
} while (0)


char *fstrncpy(char *cstr, const char *fstr, size_t len, size_t maxlen)
{
        if (!maxlen)
                return 0;

        while (len > 0 && fstr[len-1] == ' ') 
                len--;

        if (len > maxlen-1)
                len = maxlen-1;

        strncpy(cstr, fstr, len);
        cstr[len] = '\0';

        return cstr;
}

/* 
   C interface for DART.
*/

int dspaces_init(int num_peers, int appid)
{
	int err = -ENOMEM;

	if (dcg) {
		/* Library already initialized. */
		return 0;
	}

	dcg = dcg_alloc(num_peers, appid);
	if (!dcg) {
                uloga("'%s()': failed to initialize.\n", __func__);
		return err;
	}

	err = dcg_ss_info(dcg, &num_dims); 
	if (err < 0) {
		uloga("'%s()': failed to obtain space info, err %d.\n", 
			__func__, err);
		return err;
	}

#ifdef DS_HAVE_DIMES
	err = dimes_alloc(dcg);
	if (err < 0) {
		uloga("%s(): failed to init DIMES.\n", __func__);
		return err;
	}
#endif

	return 0;
}

void dspaces_set_storage_type(int fst)
{
	if (fst == 0)
		st = row_major;
	else if (fst == 1)
		st = column_major;
}

int dspaces_rank(void)
{
	if (dcg)
		return dcg_get_rank(dcg);
	else return -1;
}

int dspaces_peers(void)
{
	if (dcg)
		return dcg_get_num_peers(dcg);
	else return -1;
}

int dspaces_get_num_space_peers(void)
{
	if (dcg)
		return dcg_get_num_space_peers(dcg);
	else return -1;
}

void dspaces_barrier(void)
{
	int err;

	if (!dcg) {
		uloga("'%s()': library was not properly initialized!\n",
			 __func__);
		return;
	}

	err = dcg_barrier(dcg);
	if (err < 0) 
		ERROR_TRACE_AND_EXIT();
}

void dspaces_lock_on_read(const char *lock_name, void *comm)
{
	int err;

	if (!dcg) {
		uloga("'%s()': library was not properly initialized!\n",
			 __func__);
		return;
	}

	err = dcg_lock_on_read(lock_name, comm);
	if (err < 0) 
		ERROR_TRACE_AND_EXIT();
}

void dspaces_unlock_on_read(const char *lock_name, void *comm)
{
	int err;

	if (!dcg) {
		uloga("'%s()': library was not properly initialized!\n",
			 __func__);
		return;
	}

	err = dcg_unlock_on_read(lock_name, comm);
	if (err < 0) 
		ERROR_TRACE_AND_EXIT();
}

void dspaces_lock_on_write(const char *lock_name, void *comm)
{
	int err;

	if (!dcg) {
		uloga("'%s()': library was not properly initialized!\n",
			 __func__);
		return;
	}

	err = dcg_lock_on_write(lock_name, comm);
	if (err < 0)
		ERROR_TRACE_AND_EXIT();
}

void dspaces_unlock_on_write(const char *lock_name, void *comm)
{
	int err;

	if (!dcg) {
		uloga("'%s()': library was not properly initialized!\n",
			 __func__);
		return;
	}

	err = dcg_unlock_on_write(lock_name, comm);
	if (err < 0)
		ERROR_TRACE_AND_EXIT();
}

int dspaces_get(const char *var_name,
	unsigned int ver, int size,
	int xl, int yl, int zl,
	int xu, int yu, int zu,
	void *data)
{
        struct obj_descriptor odsc = {
                .version = ver, .owner = -1, 
                .st = st,
                .size = size,
                .bb = {.num_dims = num_dims, 
                       .lb.c = {xl, yl, zl}, 
                       .ub.c = {xu, yu, zu}}};
        struct obj_data *od;
        int err = -ENOMEM;

	if (!dcg) {
		uloga("'%s()': library was not properly initialized!\n",
			 __func__);
		return -EINVAL;
	}

	strncpy(odsc.name, var_name, sizeof(odsc.name)-1);
	odsc.name[sizeof(odsc.name)-1] = '\0';

        od = obj_data_alloc_no_data(&odsc, data);
        if (!od) {
		uloga("'%s()': failed, can not allocate data object.\n", 
			__func__);
                return -ENOMEM;
	}

        err = dcg_obj_get(od);
	obj_data_free(od);
        if (err < 0 && err != -EAGAIN) 
		uloga("'%s()': failed with %d, can not get data object.\n",
			__func__, err);

        return err;
}

int dspaces_get_versions(int **p_versions)
{
	return dcg_get_versions(p_versions);
}

int dspaces_put(const char *var_name, 
        unsigned int ver, int size,
        int xl, int yl, int zl,
        int xu, int yu, int zu, 
        void *data)
{
        struct obj_descriptor odsc = {
                .version = ver, .owner = -1, 
                .st = st,
                .size = size,
                .bb = {.num_dims = num_dims, 
                       .lb.c = {xl, yl, zl}, 
                       .ub.c = {xu, yu, zu}}};
        struct obj_data *od;
        int err = -ENOMEM;

	if (!dcg) {
		uloga("'%s()': library was not properly initialized!\n",
			 __func__);
		return -EINVAL;
	}

	strncpy(odsc.name, var_name, sizeof(odsc.name)-1);
	odsc.name[sizeof(odsc.name)-1] = '\0';

        od = obj_data_alloc_with_data(&odsc, data);
        if (!od) {
		uloga("'%s()': failed, can not allocate data object.\n", 
			__func__);
                return -ENOMEM;
	}

        err = dcg_obj_put(od);
        if (err < 0) {
                obj_data_free(od);

		uloga("'%s()': failed with %d, can not put data object.\n", 
			__func__, err);
		return err;
        }
        sync_op_id = err;

        return 0;
}

int dspaces_select(char *var_name, unsigned int vers,
        int xl, int yl, int zl,
        int xu, int yu, int zu, 
        void *data)
{
        // TODO: 'size' is hardcoded to 8 !!!
        struct obj_descriptor odsc = {
                .version = vers, .owner = -1,
                .st = st,
                .size = 8,
                .bb = {.num_dims = num_dims,
                       .lb.c = {xl, yl, zl},
                       .ub.c = {xu, yu, zu}
                },
        };
        struct obj_data *od;
        int err = -ENOMEM;


	if (!dcg) {
		uloga("'%s()': library was not properly initialized!\n",
			 __func__);
		return -EINVAL;
	}

	strncpy(odsc.name, var_name, sizeof(odsc.name)-1);
	odsc.name[sizeof(odsc.name)-1] = '\0';

        od = obj_data_alloc_no_data(&odsc, data);
        if (!od) {
		uloga("'%s()': failed, can not allocate data object.\n",
			__func__);
		return -ENOMEM;
	}

        err = dcg_obj_filter(od);
        free(od);
        if (err < 0) 
		uloga("'%s()': failed with %d, can not complete filter.\n",
			__func__, err);

	return err;
}

int dspaces_cq_register(char *var_name,
        int xl, int yl, int zl,
        int xu, int yu, int zu, 
        void *data)
{
        // TODO: 'size' is hardcoded to 8 !!!
        struct obj_descriptor odsc = {
                .version = 0, .owner = -1,
                .st = st,
                .size = 8,
                .bb = {.num_dims = num_dims,
                       .lb.c = {xl, yl, zl},
                       .ub.c = {xu, yu, zu}
                },
        };
        struct obj_data *od; // , *odt;
        int err = -ENOMEM;

	if (!dcg) {
		uloga("'%s()': library was not properly initialized!\n",
			 __func__);
		return -EINVAL;
	}

	strncpy(odsc.name, var_name, sizeof(odsc.name)-1);
	odsc.name[sizeof(odsc.name)-1] = '\0';

	od = obj_data_alloc_no_data(&odsc, data);
	// od = obj_data_allocv(&odsc);
        if (!od) {
		uloga("'%s()': failed, can not allocate data object.\n",
			__func__);
		return -ENOMEM;
	}

	// ssd_copyv(od, odt);
	// obj_data_free(odt);

        err =  dcg_obj_cq_register(od);
        free(od);
        if (err < 0)
		uloga("'%s()': failed with %d, can not complere CQ register.\n",
			__func__, err);
        cq_id = err;

        return err;
}

int dspaces_cq_update(void)
{
        int err;

	if (!dcg) {
		uloga("'%s()': library was not properly initialized!\n",
			 __func__);
		return -EINVAL;
	}

        if (cq_id < 0)
                return cq_id;

        err = dcg_obj_cq_update(cq_id);
        if (err < 0)
                uloga("'%s()': failed with %d, can not complete CQ update.\n",
			 __func__, err);

	return err;
}

int dspaces_put_sync(void)
{
        int err;

	if (!dcg) {
		uloga("'%s()': library was not properly initialized!\n",
			 __func__);
		return -EINVAL;
	}

        err = dcg_obj_sync(sync_op_id);
        if (err < 0)
	        uloga("'%s()': failed with %d, can not complete put_sync.\n", 
			__func__, err);

        return err;
}

#ifdef DS_HAVE_ACTIVESPACE
int dspaces_code_load(void *fnaddr, // int off, int size_code, 
	const char *var_name, unsigned int version, int size_elem,
	int xl, int yl, int zl,
	int xu, int yu, int zu,
	void *data)
{
        struct obj_descriptor odsc = {
                .version = version, .owner = -1, 
                .st = st,
                .size = size_elem,
                .bb = {.num_dims = num_dims, 
                       .lb.c = {xl, yl, zl}, 
                       .ub.c = {xu, yu, zu}}};
        struct obj_data *od;
        int err = -ENOMEM;

	if (!dcg) {
		uloga("'%s()': library was not properly initialized!\n",
			 __func__);
		return -EINVAL;
	}

	strncpy(odsc.name, var_name, sizeof(odsc.name)-1);
	odsc.name[sizeof(odsc.name)-1] = '\0';

	od = obj_data_alloc_no_data(&odsc, data);
	if (!od)
		goto err_out;

	err = dcg_code_send(fnaddr, /*off, size_code,*/ od);
	obj_data_free(od);
	if (err >= 0)
		return err;

 err_out:
	ERROR_TRACE();
}
#endif // end of #ifdef DS_HAVE_ACTIVESPACE

void dspaces_finalize(void)
{
	if (!dcg) {
		uloga("'%s()': library was not properly initialized!\n",
			 __func__);
		return;
	}

        dcg_free(dcg);
        dcg = 0;
#ifdef DS_HAVE_DIMES
	dimes_free();
#endif
}

int dspaces_collect_timing(double time, double *sum_ptr)
{

	if (!dcg) {
		uloga("'%s()': library was not properly initialized!\n",
			 __func__);
		return -EINVAL;
	}

	return dcg_collect_timing(time, sum_ptr);
}

int dspaces_num_space_srv(void)
{
	if (!dcg) {
		uloga("'%s()': library was not properly initialized!\n",
			 __func__);
		return -EINVAL;
	}

	return dcg_get_num_space_srv(dcg);	
}


/* 
   Fortran interface to DART.
*/

void FC_FUNC(dspaces_init, DSPACES_INIT)(int *num_peers, int *appid, int *err)
{
	*err = dspaces_init(*num_peers, *appid);
}

void FC_FUNC(dspaces_set_storage_type, DSPACES_SET_STORAGE_TYPE) (int *fst)
{
	dspaces_set_storage_type(*fst);
}

void FC_FUNC(dspaces_rank, DSPACES_RANK)(int *rank)
{
	*rank = dspaces_rank();
}

void FC_FUNC(dspaces_peers, DSPACES_PEERS)(int *peers)
{
	*peers = dspaces_peers();
}

void FC_FUNC(dspaces_get_num_space_peers, DSPACES_GET_NUM_SPACE_PEERS)(int *peers)
{
	*peers = dspaces_get_num_space_peers();
}

void FC_FUNC(dspaces_barrier, DSPACES_BARRIER)(void)
{
	dspaces_barrier();
}

void FC_FUNC(dspaces_lock_on_read, DSPACES_LOCK_ON_READ)(const char *lock_name, void *comm, int len)
{
	char c_lock_name[64];

	if (!fstrncpy(c_lock_name, lock_name, (size_t) len, sizeof(c_lock_name)))
		strcpy(c_lock_name, "default");

	dspaces_lock_on_read(c_lock_name, comm);
}

void FC_FUNC(dspaces_unlock_on_read, DSPACES_UNLOCK_ON_READ)(const char *lock_name, void *comm, int len)
{
	char c_lock_name[64];

	if (!fstrncpy(c_lock_name, lock_name, (size_t) len, sizeof(c_lock_name)))
		strcpy(c_lock_name, "default");

	dspaces_unlock_on_read(c_lock_name, comm);
}

void FC_FUNC(dspaces_lock_on_write, DSPACES_LOCK_ON_WRITE)(const char *lock_name, void *comm, int len)
{
	char c_lock_name[64];

	if (!fstrncpy(c_lock_name, lock_name, (size_t) len, sizeof(c_lock_name)))
		strcpy(c_lock_name, "default");
	
	dspaces_lock_on_write(c_lock_name, comm);
}

void FC_FUNC(dspaces_unlock_on_write, DSPACES_UNLOCK_ON_WRITE)(const char *lock_name, void *comm, int len)
{
	char c_lock_name[64];

	if (!fstrncpy(c_lock_name, lock_name, (size_t) len, sizeof(c_lock_name)))
		strcpy(c_lock_name, "default");

	dspaces_unlock_on_write(c_lock_name, comm);
}

void FC_FUNC(dspaces_get, DSPACES_GET) (const char *var_name, 
        unsigned int *ver, int *size,
        int *xl, int *yl, int *zl, 
        int *xu, int *yu, int *zu, 
        void *data, int *err, int len)
{
	char vname[256];

        if (!fstrncpy(vname, var_name, (size_t) len, sizeof(vname))) {
		uloga("'%s()': failed, can not copy Fortran var of len %d.\n", 
			__func__, len);
		*err = -ENOMEM;
	}

	*err = dspaces_get(vname, *ver, *size, *xl, *yl, *zl, *xu, *yu, *zu, data);
}

void FC_FUNC(dspaces_get_versions, DSPACES_GET_VERSIONS)(int *num_vers, int *versions, int *err)
{
	int n, *vers_tab;

	n = dspaces_get_versions(&vers_tab);
	memcpy(versions, vers_tab, n * sizeof(int));
}

void FC_FUNC(dspaces_put, DSPACES_PUT) (const char *var_name, 
        unsigned int *ver, int *size,
        int *xl, int *yl, int *zl,
        int *xu, int *yu, int *zu, 
        void *data, int *err, int len)
{
	char vname[256];

        if (!fstrncpy(vname, var_name, (size_t) len, sizeof(vname))) {
		uloga("'%s': failed, can not copy Fortran var of len %d.\n",
			__func__, len);
		*err = -ENOMEM;
	}

	*err =  dspaces_put(vname, *ver, *size, *xl, *yl, *zl, *xu, *yu, *zu, data);
}

void FC_FUNC(dspaces_select, DSPACES_SELECT)(char *var_name, unsigned int *vers,
        int *xl, int *yl, int *zl,
        int *xu, int *yu, int *zu, 
        void *data, int *err, int len)
{
	char vname[256];

        if (!fstrncpy(vname, var_name, (size_t) len, sizeof(vname))) {
		uloga("'%s': failed, can not copy Fortran var of len %d.\n",
			__func__, len);
		*err = -ENOMEM;
	}

	*err = dspaces_select(vname, *vers, *xl, *yl, *zl, *xu, *yu, *zu, data);
}

void FC_FUNC(dspaces_cq_register, DSPACES_CQ_REGISTER)(char *var_name,
        int *xl, int *yl, int *zl,
        int *xu, int *yu, int *zu, 
        void *data, int *err, int len)
{
	char vname[256];

        if (!fstrncpy(vname, var_name, (size_t) len, sizeof(vname))) {
		uloga("'%s': failed, can not copy Fortran var of len %d.\n",
			__func__, len);
		*err = -ENOMEM;
	}

	*err = dspaces_cq_register(vname, *xl, *yl, *zl, *xu, *yu, *zu, data);
}

void FC_FUNC(dspaces_cq_update, DSPACES_CQ_UPDATE) (int *err)
{
	*err = dspaces_cq_update();
}

void FC_FUNC(dspaces_put_sync, DSPACES_PUT_SYNC)(int *err)
{
	*err = dspaces_put_sync();
}

#ifdef DS_HAVE_ACTIVESPACE
/* This will never be directly used from Fortran code !!! */
void FC_FUNC(dspaces_code_load, DSPACES_CODE_LOAD)(void *fnaddr, // int *off, int *size_code, 
	const char *var_name, 
        unsigned int *ver, int *size_elem,
        int *xl, int *yl, int *zl, 
        int *xu, int *yu, int *zu, 
        void *data, int *err, int len)
{
	char vname[len+1];

	fstrncpy(vname, var_name, (size_t) len, sizeof(vname));

	*err = dspaces_code_load(fnaddr, vname, *ver, *size_elem, 
			*xl, *yl, *zl, *xu, *yu, *zu, data);
}
#endif

void FC_FUNC(dspaces_finalize, DSPACES_FINALIZE) (void)
{
	dspaces_finalize();
}

void FC_FUNC(dspaces_collect_timing, DSPACES_COLLECT_TIMING)(double *time, double *sum_ptr, int *err)
{
	int lerr;

	lerr = dspaces_collect_timing(*time, sum_ptr);
	if (err)
		*err = lerr;
}

void FC_FUNC(dspaces_num_space_srv, DSPACES_NUM_SPACE_SRV)(int *numsrv)
{
	*numsrv = dspaces_num_space_srv();
}

/* 
   Timer interface for Fortran language.
*/
void FC_FUNC(timer_init, TIMER_INIT) (void)
{
        timer_init(&timer, 0);
}

void FC_FUNC(timer_start, TIMER_START) (void)
{
        timer_start(&timer);
}

void FC_FUNC(timer_stop, TIMER_STOP) (void)
{
        timer_stop(&timer);
}

void FC_FUNC(timer_reset, TIMER_RESET) (void)
{
        timer_reset(&timer);
}

/* 
   Return type does not work properly under C-Fortran interface, so
   use an output partameter to return a value. 
*/
void FC_FUNC(timer_read, TIMER_READ) (double *dp)
{
        if (dp)
                *dp = timer_read(&timer);
}

/*
  Log the time.
*/
void FC_FUNC(timer_log, TIMER_LOG) (double time_tab[], int *n)
{
        dcg_time_log(time_tab, *n);
}


void FC_FUNC(timer_sleep, TIMER_SLEEP)(int *msec)
{
	usleep(*msec * 1000);
}
