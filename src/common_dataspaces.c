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
*  Hoang Bui (2012-2013) TASSL Rutgers University
*  hbui@cac.rutgers.edu
*  Fan Zhang (2013) TASSL Rutgers University
*  zhangfan@cac.rutgers.edu
*
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

#ifdef DS_HAVE_DIMES
#include "dimes_client.h"
#endif

static struct dcg_space *dcg;
static struct timer timer;
static int sync_op_id;
static int cq_id = -1;
static enum storage_type st = column_major;
static int num_dims = 2;
#ifdef DS_HAVE_DIMES
static struct dimes_client *dimes_c;
#endif

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

/* 
   Common interface for DataSpaces.
*/

int common_dspaces_init(int num_peers, int appid)
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
        dimes_c = dimes_client_alloc(dcg);
        if (dimes_c == NULL) {
                uloga("%s(): failed to init DIMES.\n", __func__);
                return err;
        }
#endif

	return 0;
}

void common_dspaces_set_storage_type(int fst)
{
	if (fst == 0)
		st = row_major;
	else if (fst == 1)
		st = column_major;
}

int common_dspaces_rank(void)
{
	if (dcg)
		return dcg_get_rank(dcg);
	else return -1;
}

int common_dspaces_peers(void)
{
	if (dcg)
		return dcg_get_num_peers(dcg);
	else return -1;
}

int common_dspaces_get_num_space_peers(void)
{
	if (dcg)
		return dcg_get_num_space_peers(dcg);
	else return -1;
}

void common_dspaces_barrier(void)
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

void common_dspaces_lock_on_read(const char *lock_name, void *comm)
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

void common_dspaces_unlock_on_read(const char *lock_name, void *comm)
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

void common_dspaces_lock_on_write(const char *lock_name, void *comm)
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

void common_dspaces_unlock_on_write(const char *lock_name, void *comm)
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

int common_dspaces_get(const char *var_name,
	unsigned int ver, int size,
	int ndim,
	uint64_t *lb,
	uint64_t *ub,
    uint64_t *gdim,
	void *data)
{
        struct obj_descriptor odsc = {
                .version = ver, .owner = -1, 
                .st = st,
                .size = size,
                .bb = {.num_dims = num_dims, 
		}
        };
        memset(odsc.bb.lb.c, 0, sizeof(uint64_t)*num_dims);
        memset(odsc.bb.ub.c, 0, sizeof(uint64_t)*num_dims);

        memcpy(odsc.bb.lb.c, lb, sizeof(uint64_t)*ndim);
        memcpy(odsc.bb.ub.c, ub, sizeof(uint64_t)*ndim);

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

        set_global_dimension(&od->gdim, ndim, gdim);

        err = dcg_obj_get(od);
        obj_data_free(od);
        if (err < 0 && err != -EAGAIN) 
            uloga("'%s()': failed with %d, can not get data object.\n",
                __func__, err);

        return err;
}

int common_dspaces_get_versions(int **p_versions)
{
	return dcg_get_versions(p_versions);
}

int common_dspaces_put(const char *var_name, 
        unsigned int ver, int size,
        int ndim,
        uint64_t *lb,
        uint64_t *ub,
        uint64_t *gdim,
        void *data)
{
        struct obj_descriptor odsc = {
                .version = ver, .owner = -1, 
                .st = st,
                .size = size,
                .bb = {.num_dims = num_dims,
		}
        };

        memset(odsc.bb.lb.c, 0, sizeof(uint64_t)*num_dims);
        memset(odsc.bb.ub.c, 0, sizeof(uint64_t)*num_dims);

        memcpy(odsc.bb.lb.c, lb, sizeof(uint64_t)*ndim);
        memcpy(odsc.bb.ub.c, ub, sizeof(uint64_t)*ndim);

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

        set_global_dimension(&od->gdim, ndim, gdim);

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

int common_dspaces_select(char *var_name, unsigned int vers,
	int ndim,
    uint64_t *lb, //int xl, int yl, int zl,
    uint64_t *ub, //int xu, int yu, int zu, 
    void *data)
{
        // TODO: 'size' is hardcoded to 8 !!!
        struct obj_descriptor odsc = {
                .version = vers, .owner = -1,
                .st = st,
                .size = 8,
                .bb = {.num_dims = ndim, //num_dims,
                       //.lb.c = {xl, yl, zl},
                       //.ub.c = {xu, yu, zu}
                },
        };
        memcpy(odsc.bb.lb.c, lb, sizeof(uint64_t)*odsc.bb.num_dims);
        memcpy(odsc.bb.ub.c, ub, sizeof(uint64_t)*odsc.bb.num_dims);

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

int common_dspaces_cq_register(char *var_name,
	int ndim,
    uint64_t *lb, //int xl, int yl, int zl,
    uint64_t *ub, //int xu, int yu, int zu, 
    void *data)
{
        // TODO: 'size' is hardcoded to 8 !!!
        struct obj_descriptor odsc = {
                .version = 0, .owner = -1,
                .st = st,
                .size = 8,
                .bb = {.num_dims = ndim, //num_dims,
                       //.lb.c = {xl, yl, zl},
                       //.ub.c = {xu, yu, zu}
                },
        };
        memcpy(odsc.bb.lb.c, lb, sizeof(uint64_t)*odsc.bb.num_dims);
        memcpy(odsc.bb.ub.c, ub, sizeof(uint64_t)*odsc.bb.num_dims);

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

int common_dspaces_cq_update(void)
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

int common_dspaces_put_sync(void)
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
int common_dspaces_code_load(void *fnaddr, // int off, int size_code, 
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

void common_dspaces_finalize(void)
{
	if (!dcg) {
		uloga("'%s()': library was not properly initialized!\n",
			 __func__);
		return;
	}

#ifdef DS_HAVE_DIMES
        dimes_client_free();
#endif

        dcg_free(dcg);
        dcg = 0;
}

int common_dspaces_collect_timing(double time, double *sum_ptr)
{

	if (!dcg) {
		uloga("'%s()': library was not properly initialized!\n",
			 __func__);
		return -EINVAL;
	}

	return dcg_collect_timing(time, sum_ptr);
}

int common_dspaces_num_space_srv(void)
{
	if (!dcg) {
		uloga("'%s()': library was not properly initialized!\n",
			 __func__);
		return -EINVAL;
	}

	return dcg_get_num_space_srv(dcg);	
}

#ifdef DS_HAVE_DIMES
void common_dimes_set_storage_type(int fst)
{
    return dimes_client_set_storage_type(fst);
}

int common_dimes_get(const char *var_name,
        unsigned int ver, int size,
        int ndim,
        uint64_t *lb, //int xl, int yl, int zl,
        uint64_t *ub, //int xu, int yu, int zu, 
        uint64_t *gdim,
        void *data)
{
    return dimes_client_get(var_name, ver, size,
                ndim, lb, ub, gdim, data);
}

int common_dimes_put(const char *var_name,
        unsigned int ver, int size,
        int ndim,
        uint64_t *lb, //int xl, int yl, int zl,
        uint64_t *ub, //int xu, int yu, int zu, 
        uint64_t *gdim,
        void *data)
{
    return dimes_client_put(var_name, ver, size,
                ndim, lb, ub, gdim, data);
}

int common_dimes_put_sync_all(void)
{
    return dimes_client_put_sync_all();
}

int common_dimes_put_set_group(const char *group_name, int step)
{
    return dimes_client_put_set_group(group_name, step);
}

int common_dimes_put_unset_group()
{
    return dimes_client_put_unset_group();
}

int common_dimes_put_sync_group(const char *group_name, int step)
{
    return dimes_client_put_sync_group(group_name, step);
}
#endif

void common_dspaces_set_mpi_rank(int rank)
{
    dcg_set_mpi_rank(rank);
}
