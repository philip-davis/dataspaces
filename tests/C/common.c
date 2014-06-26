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
*  Fan Zhang (2013)  TASSL Rutgers University
*  zhangfan@cac.rutgers.edu
*  Qian Sun (2014) TASSL Rutgers University
*  qiansun@cac.rutgers.edu
*/

#include <stdio.h>
#include <stdlib.h>
#include "common.h"
#include "mpi.h"

int read_config_file(const char* fname,
	int *num_sp, int *num_cp, int *iter,
	int *num_writer, int *w,
	int *num_reader, int *r,
	int *dims, int *dim)
{
	FILE *f = NULL;
        f = fopen(fname,"rt");
        if(!f){
                goto err_out;
        }

        int num_items = 0;
        num_items = fscanf(f, "num_sp=%d;num_cp=%d;iter=%d\n",
                        num_sp, num_cp, iter);
        if(num_items != 3)
                goto err_out;

        int i = 0;
        num_items = fscanf(f, "dims=%d\n",dims);
        if(num_items != 1)
                goto err_out;
        for(i = 0; i < *dims; i++)
                fscanf(f, "%d", &dim[i]);
        fscanf(f, "\n");

        num_items = fscanf(f, "num_writer=%d\n", num_writer);
        if(num_items != 1)
                goto err_out;
        for(i = 0; i < *dims; i++)
                fscanf(f, "%d", &w[i]);
        fscanf(f, "\n");

        num_items = fscanf(f, "num_reader=%d\n", num_reader);
        if(num_items != 1)
                goto err_out;
        for(i = 0; i < *dims; i++)
                fscanf(f, "%d", &r[i]);

        fclose(f);
        return 0;
err_out:
        if (f) {
                fclose(f);
        }
        return -1;
}

int parse_args(int argc, char** argv, enum transport_type *type, int *npapp, 
	int *dims, int* npdim, uint64_t* spdim, int *timestep, int *appid, 
	size_t *elem_size, int *num_vars)
{
	int i = 0, j = 0, count = 0;
	*type = USE_DSPACES;
	if(0 == strcmp(argv[1], "DIMES")){
		*type = USE_DIMES;
	}
	*npapp = atoi(argv[2]);
	*dims = atoi(argv[3]);
	count = 3;

	if(argc < 3 + (*dims)*2 + 2 + 1){
		uloga("Wrong number of arguments!\n");
		return -1;
	}

	for(i = count + 1, j = 0; j < *dims; i++, j++){
		*(npdim+j) = atoi(argv[i]);
	}
	count += *dims;

	for(i = count + 1, j = 0; j < *dims; i++, j++){
		*(spdim+j) = strtoull(argv[i], NULL, 10); 
	}
	count += *dims;

	*timestep = atoi(argv[++count]);
	*appid = atoi(argv[++count]);

	if(argc >= ++count + 1)
		*elem_size = atoi(argv[count]);
	else
		*elem_size = sizeof(double);

	if(argc >= ++count + 1)
		*num_vars = atoi(argv[count]);
	else
		*num_vars = 1;

	return 0;
}

int common_init(int num_peers, int appid, void* comm, const char* parameters) {
        return dspaces_init(num_peers, appid, comm, parameters);
}

int common_rank() {
        return dspaces_rank();
}

int common_peers() {
        return dspaces_peers();
}

void common_barrier() {
        dspaces_barrier();
}

void common_finalize() {
        dspaces_finalize();
}

void common_lock_on_read(const char *lock_name, void *gcomm) {
        dspaces_lock_on_read(lock_name, gcomm);
}

void common_unlock_on_read(const char *lock_name, void *gcomm) {
        dspaces_unlock_on_read(lock_name, gcomm);
}

void common_lock_on_write(const char *lock_name, void *gcomm) {
        dspaces_lock_on_write(lock_name, gcomm);
}

void common_unlock_on_write(const char *lock_name, void *gcomm) {
        dspaces_unlock_on_write(lock_name, gcomm);
}

int common_put(const char *var_name, 
	unsigned int ver, int size,
	int ndim,
	uint64_t *lb, uint64_t *ub,
	void *data, enum transport_type type)
{
	if ( type == USE_DSPACES ) {
		return dspaces_put(var_name, ver, size,
                        ndim,lb, ub,data);
	} else if (type == USE_DIMES) {
#ifdef DS_HAVE_DIMES
		return dimes_put(var_name, ver, size, 
			ndim, lb, ub, data);
#else
		uloga("%s(): DataSpaces DIMES is not enabled!\n", __func__);
		return -1;
#endif
	}
    return 0;
}

int common_get(const char *var_name,
	unsigned int ver, int size,
	int ndim,
	uint64_t *lb, uint64_t *ub,
	void *data, enum transport_type type)
{
	if ( type == USE_DSPACES ) {
		return dspaces_get(var_name, ver, size,
                        ndim,lb, ub,data);
	} else if (type == USE_DIMES) {
#ifdef DS_HAVE_DIMES
		return dimes_get(var_name, ver, size, 
			ndim, lb, ub, data);
#else
		uloga("%s(): DataSpaces DIMES is not enabled!\n", __func__);
		return -1;
#endif
	}
    return 0;
}

int common_put_sync(enum transport_type type) {
        if (type == USE_DSPACES) {
                return dspaces_put_sync();
        } else if (type == USE_DIMES) {
#ifdef DS_HAVE_DIMES
                return dimes_put_sync_all();
#else
                uloga("%s(): DataSpaces DIMES is not enabled!\n", __func__);
                return -1;
#endif
        }
        return 0;
}

int common_run_server(int num_sp, int num_cp, enum transport_type type, void* gcomm) {
        int err;
        if (type == USE_DSPACES) {
                struct ds_gspace *dsg;
                dsg = dsg_alloc(num_sp, num_cp, "dataspaces.conf", gcomm);
                if (!dsg)
                        return -1;

                while (!dsg_complete(dsg)){
                        err = dsg_process(dsg);
                        if(err<0)
                                break;
                }

                //dsg_barrier(dsg);
		MPI_Barrier(*(MPI_Comm*)gcomm);
                dsg_free(dsg);

                if (err == 0)
                        uloga("All ok.\n");
                return 0;
        } else if (type == USE_DIMES) {
#ifdef DS_HAVE_DIMES
                struct dimes_server *dsg;
                dsg = dimes_server_alloc(num_sp, num_cp, "dataspaces.conf", gcomm);
                if (!dsg)
                        return -1;

                while (!dimes_server_complete(dsg)){
                        err = dimes_server_process(dsg);
                        if(err<0)
                                break;
                }

                //dimes_server_barrier(dsg);
		MPI_Barrier(*(MPI_Comm*)gcomm);
                dimes_server_free(dsg);

                if (err == 0)
                        uloga("All ok.\n");
                return 0;
#else
                uloga("%s(): Dataspaces DIMES is not enabled!\n", __func__);
                return -1;
#endif
        }
        return 0;
}

void check_data(const char *var_name, double *buf, int num_elem, int rank, int ts)
{
        double max, min, sum, avg;
        int i;
        int cnt = 0;

        if (num_elem <= 0) {
                return;
        }

        max = min = sum = buf[0];
        for (i = 1; i < num_elem; i++) {
                if (max < buf[i])
                        max = buf[i];
                if (min > buf [i])
                        min = buf[i];
                sum += buf[i];
                if (buf[i] != ts) {
                        cnt++;
                }
        }
        avg = sum / num_elem;
#ifdef DEBUG
          uloga("%s(): var= %s, rank= %d, max= %f, min= %f, avg= %f\n",
                          __func__, var_name, rank, max, min, avg);
#endif

        if (cnt > 0) {
                uloga("%s(): var= %s, rank= %d, ts= %d, "
                "error elem cnt= %d, total elem= %d\n",
                        __func__, var_name, rank, ts, cnt, num_elem);
        }

        return;
}

int write_data_file(const char* fname, void *data, size_t size)
{
        FILE *f = fopen(fname, "w");
        if (f == NULL) {
                uloga("%s(): failed to create %s\n", __func__, fname);
                return -1;
        }

        size_t offset = 0;
        size_t block_size = 512*1024; // 512KB
        size_t bytes;
        while (offset < size) {
                if ((size-offset) >= block_size) {
                        bytes = block_size;
                } else {
                        bytes = (size-offset);
                }

                fwrite(data+offset, 1, bytes, f);
                offset += bytes;
        }

        fclose(f);
        return 0;
}

int read_data_file(const char* fname)
{
        FILE *f = fopen(fname, "r");
        if (f == NULL) {
                uloga("%s(): failed to open %s\n", __func__, fname);
                return -1;
        }

        size_t block_size = 512*1024; // 512KB
        void *data = malloc(block_size);
        do {
                fread(data, 1, block_size, f);
        } while (!feof(f));

        free(data);
        fclose(f);
        return 0;
}

int common_get_transport_type_str(enum transport_type type, char* str)
{
    if (type == USE_DSPACES) {
        sprintf(str, "DATASPACES");
    } else if (type == USE_DIMES) {
        sprintf(str, "DIMES");
    } else sprintf(str, "UNKNOWN");

    return 0;
}
