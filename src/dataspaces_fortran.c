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
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#include "debug.h"
#include "timer.h"
#include "common_dataspaces.h"
#include "dc_gspace.h"

/* Name mangling for C functions to adapt Fortran compiler */
#ifdef HAVE_DCMF
#define FC_FUNC(name,NAME) name  
#else
#define FC_FUNC(name,NAME) name ## _
#endif

static struct timer timer;

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
   Fortran interface to DataSpaces.
*/

void FC_FUNC(dspaces_init, DSPACES_INIT)(int *num_peers, int *appid, int *err)
{
	// TODO: to remove uloga
	//uloga("%s(): num_peers= %d, appid= %d\n",
	//	__func__, *num_peers, *appid);

	*err = common_dspaces_init(*num_peers, *appid);
}

void FC_FUNC(dspaces_set_storage_type, DSPACES_SET_STORAGE_TYPE) (int *fst)
{
	common_dspaces_set_storage_type(*fst);
}

void FC_FUNC(dspaces_rank, DSPACES_RANK)(int *rank)
{
	*rank = common_dspaces_rank();
}

void FC_FUNC(dspaces_peers, DSPACES_PEERS)(int *peers)
{
	*peers = common_dspaces_peers();
}

void FC_FUNC(dspaces_get_num_space_peers, DSPACES_GET_NUM_SPACE_PEERS)(int *peers)
{
	*peers = common_dspaces_get_num_space_peers();
}

void FC_FUNC(dspaces_barrier, DSPACES_BARRIER)(void)
{
	common_dspaces_barrier();
}

void FC_FUNC(dspaces_lock_on_read, DSPACES_LOCK_ON_READ)(const char *lock_name, void *comm, int len)
{
        char c_lock_name[64];

        if (!fstrncpy(c_lock_name, lock_name, (size_t) len, sizeof(c_lock_name)))
                strcpy(c_lock_name, "default");

        MPI_Comm c_comm;
        c_comm = MPI_Comm_f2c(*(MPI_Fint*)comm);
        common_dspaces_lock_on_read(c_lock_name, &c_comm);
}

void FC_FUNC(dspaces_unlock_on_read, DSPACES_UNLOCK_ON_READ)(const char *lock_name, void *comm, int len)
{
        char c_lock_name[64];

        if (!fstrncpy(c_lock_name, lock_name, (size_t) len, sizeof(c_lock_name)))
                strcpy(c_lock_name, "default");

        MPI_Comm c_comm;
        c_comm = MPI_Comm_f2c(*(MPI_Fint*)comm);
        common_dspaces_unlock_on_read(c_lock_name, &c_comm);
}

void FC_FUNC(dspaces_lock_on_write, DSPACES_LOCK_ON_WRITE)(const char *lock_name, void *comm, int len)
{
        char c_lock_name[64];

        if (!fstrncpy(c_lock_name, lock_name, (size_t) len, sizeof(c_lock_name)))
                strcpy(c_lock_name, "default");

        MPI_Comm c_comm;
        c_comm = MPI_Comm_f2c(*(MPI_Fint*)comm);
        common_dspaces_lock_on_write(c_lock_name, &c_comm);
}

void FC_FUNC(dspaces_unlock_on_write, DSPACES_UNLOCK_ON_WRITE)(const char *lock_name, void *comm, int len)
{
        char c_lock_name[64];

        if (!fstrncpy(c_lock_name, lock_name, (size_t) len, sizeof(c_lock_name)))
                strcpy(c_lock_name, "default");

        MPI_Comm c_comm;
        c_comm = MPI_Comm_f2c(*(MPI_Fint*)comm);
        common_dspaces_unlock_on_write(c_lock_name, &c_comm);
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

    int ndim = 3;
    uint64_t lb[3] = {*xl, *yl, *zl};
    uint64_t ub[3] = {*xu, *yu, *zu};
	*err = common_dspaces_get(vname, *ver, *size, ndim, lb, ub, data);
}

/*
void FC_FUNC(dspaces_get_versions, DSPACES_GET_VERSIONS)(int *num_vers, int *versions, int *err)
{
	int n, *vers_tab;

	n = common_dspaces_get_versions(&vers_tab);
	memcpy(versions, vers_tab, n * sizeof(int));
}
*/

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


	int ndim = 3;
    uint64_t lb[3] = {*xl, *yl, *zl};
    uint64_t ub[3] = {*xu, *yu, *zu};
	*err =  common_dspaces_put(vname, *ver, *size, ndim, lb, ub, data);
}

/*
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

	*err = common_dspaces_select(vname, *vers, *xl, *yl, *zl, *xu, *yu, *zu, data);
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

	*err = common_dspaces_cq_register(vname, *xl, *yl, *zl, *xu, *yu, *zu, data);
}

void FC_FUNC(dspaces_cq_update, DSPACES_CQ_UPDATE) (int *err)
{
	*err = common_dspaces_cq_update();
}
*/

void FC_FUNC(dspaces_put_sync, DSPACES_PUT_SYNC)(int *err)
{
	*err = common_dspaces_put_sync();
}

#ifdef DS_HAVE_ACTIVESPACE
/* This will never be directly used from Fortran code !!! */
/*
void FC_FUNC(dspaces_code_load, DSPACES_CODE_LOAD)(void *fnaddr, // int *off, int *size_code, 
	const char *var_name, 
        unsigned int *ver, int *size_elem,
        int *xl, int *yl, int *zl, 
        int *xu, int *yu, int *zu, 
        void *data, int *err, int len)
{
	char vname[len+1];

	fstrncpy(vname, var_name, (size_t) len, sizeof(vname));

	*err = common_dspaces_code_load(fnaddr, vname, *ver, *size_elem, 
			*xl, *yl, *zl, *xu, *yu, *zu, data);
}
*/
#endif

void FC_FUNC(dspaces_finalize, DSPACES_FINALIZE) (void)
{
	common_dspaces_finalize();
}

/* 
   Timer interface for Fortran language.
*/
void FC_FUNC(ftimer_init, FTIMER_INIT) (void)
{
        timer_init(&timer, 0);
}

void FC_FUNC(ftimer_start, FTIMER_START) (void)
{
        timer_start(&timer);
}

void FC_FUNC(ftimer_stop, FTIMER_STOP) (void)
{
        timer_stop(&timer);
}

void FC_FUNC(ftimer_reset, FTIMER_RESET) (void)
{
        timer_reset(&timer);
}

/* 
   Return type does not work properly under C-Fortran interface, so
   use an output partameter to return a value. 
*/
void FC_FUNC(ftimer_read, FTIMER_READ) (double *dp)
{
        if (dp)
                *dp = timer_read(&timer);
}

/*
  Log the time.
*/
void FC_FUNC(ftimer_log, FTIMER_LOG) (double time_tab[], int *n)
{
        dcg_time_log(time_tab, *n);
}


void FC_FUNC(ftimer_sleep, FTIMER_SLEEP)(int *msec)
{
	usleep(*msec * 1000);
}

/*
  Fortran interface to DIMES
*/
#ifdef DS_HAVE_DIMES
void FC_FUNC(dimes_set_storage_type, DIMES_SET_STORAGE_TYPE) (int *fst)
{
        common_dimes_set_storage_type(*fst);
}

void FC_FUNC(dimes_get, DIMES_GET) (const char *var_name,
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

    int ndim = 3;
    uint64_t lb[3] = {*xl, *yl, *zl};
    uint64_t ub[3] = {*xu, *yu, *zu};
    *err = common_dimes_get(vname, *ver, *size, ndim, lb, ub, data);
}

void FC_FUNC(dimes_put, DIMES_PUT) (const char *var_name,
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

    int ndim = 3;
    uint64_t lb[3] = {*xl, *yl, *zl};
    uint64_t ub[3] = {*xu, *yu, *zu};
    *err = common_dimes_put(vname, *ver, *size, ndim, lb, ub, data);
}

void FC_FUNC(dimes_put_sync_all, DIMES_PUT_SYNC_ALL)(int *err)
{
        *err = common_dimes_put_sync_all();
}
#endif
