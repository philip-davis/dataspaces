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
*/
#include <stdio.h>
#include "common.h"

int read_config_file(const char *fname,
	int *num_sp, int *num_cp, int *iter,
	int *num_writer, int *writer_x, int *writer_y, int *writer_z,
	int *num_reader, int *reader_x, int *reader_y, int *reader_z,
	int *dims, int *dim_x, int *dim_y, int *dim_z) {
	// Read config file.
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

	num_items = fscanf(
		f, "num_writer=%d;writer_x=%d;writer_y=%d;writer_z=%d\n",
		num_writer, writer_x, writer_y, writer_z);
	if(num_items != 4)
		goto err_out;

	num_items = fscanf(
		f, "num_reader=%d;reader_x=%d;reader_y=%d;reader_z=%d\n",
		num_reader, reader_x, reader_y, reader_z);
	if(num_items != 4)
		goto err_out;

	num_items = fscanf(
		f, "dims=%d;dim_x=%d;dim_y=%d;dim_z=%d\n",
		dims, dim_x, dim_y, dim_z);
	if(num_items != 4)
		goto err_out;

	fclose(f);
	return 0;
err_out:
	if (f) {
		fclose(f);
	}
	return -1;
}

int parse_args_dns_les(int argc, char **argv, int *npapp, int *npx, int *npy, int *npz, uint64_t *spx, uint64_t *spy, uint64_t *spz, int *timestep, int *appid)
{
	if (argc < 10) {
		uloga("Wrong number of arguments!\n");
		return -1;
	}

	*npapp = atoi(argv[1]);
	*npx = atoi(argv[2]);	
	*npy = atoi(argv[3]);	
	*npz = atoi(argv[4]);	
	*spx = atoll(argv[5]);	
	*spy = atoll(argv[6]);	
	*spz = atoll(argv[7]);	
	*timestep = atoi(argv[8]);	
	*appid = atoi(argv[9]);

	return 0;
}

int parse_args(int argc, char **argv, enum transport_type *type, int *npapp, int *npx, int *npy, int *npz, uint64_t *spx, uint64_t *spy, uint64_t *spz, int *timestep, int *appid, int *dims, size_t * elem_size, int *num_vars)
{
    if(argc < 12) {
        uloga("Wrong number of arguments!\n");
        return -1;
    }

    *type = USE_DSPACES;
    if(0 == strcmp(argv[1], "DIMES")) {
        *type = USE_DIMES;
    }
    *npapp = atoi(argv[2]);
    *npx = atoi(argv[3]);
    *npy = atoi(argv[4]);
    *npz = atoi(argv[5]);
    *spx = atoll(argv[6]);
    *spy = atoll(argv[7]);
    *spz = atoll(argv[8]);
    *timestep = atoi(argv[9]);
    *appid = atoi(argv[10]);
    *dims = atoi(argv[11]);

    if(argc >= 13) {
        *elem_size = atoi(argv[12]);
    } else
        *elem_size = sizeof(double);

    if(argc >= 14) {
        *num_vars = atoi(argv[13]);
    } else
        *num_vars = 1;

    return 0;
}

int common_init(int num_peers, int appid) {
        return dspaces_init(num_peers, appid);
}

void common_set_storage_type(int fst, enum transport_type type) {
        if ( type == USE_DSPACES ) {
            dspaces_set_storage_type(fst);
        } else if (type == USE_DIMES) {
#ifdef DS_HAVE_DIMES
            dimes_set_storage_type(fst);
#else
            uloga("%s(): DataSpaces DIMES is not enabled!\n", __func__);
#endif
        }
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

int common_put (const char *var_name,
        unsigned int ver, int ndim, int size,
        uint64_t xl, uint64_t yl, uint64_t zl,
        uint64_t xu, uint64_t yu, uint64_t zu,
        void *data, enum transport_type type)
{
        uint64_t lb[3] = {xl, yl, zl};
        uint64_t ub[3] = {xu, yu, zu};

        if ( type == USE_DSPACES ) {
                return dspaces_put(var_name, ver, size,
                        ndim, lb, ub, data);
        } else if (type == USE_DIMES) {
#ifdef DS_HAVE_DIMES
                return dimes_put(var_name, ver, size,
                        ndim, lb, ub, data);
#else
                uloga("%s(): DataSpaces DIMES is not enabled!\n", __func__);
                return -1;
#endif
        }
}

int common_get (const char *var_name,
        unsigned int ver, int ndim, int size,
        uint64_t xl, uint64_t yl, uint64_t zl,
        uint64_t xu, uint64_t yu, uint64_t zu,
        void *data, enum transport_type type) {
        uint64_t lb[3] = {xl, yl, zl};
        uint64_t ub[3] = {xu, yu, zu};

        if ( type == USE_DSPACES ) {
                return dspaces_get(var_name, ver, size,
                        ndim, lb, ub, data);
        } else if (type == USE_DIMES) {
#ifdef DS_HAVE_DIMES
                return dimes_get(var_name, ver, size,
                        ndim, lb, ub, data);
#else
                uloga("%s(): DataSpaces DIMES is not enabled!\n", __func__);
                return -1;
#endif
        }
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
}

int common_run_server(int num_sp, int num_cp, enum transport_type type) {
	int err;
	if (type == USE_DSPACES) {
		struct ds_gspace *dsg;
		dsg = dsg_alloc(num_sp, num_cp, "dataspaces.conf");
		if (!dsg)
			return -1;

		while (!dsg_complete(dsg)){
			err = dsg_process(dsg);
			if(err<0)
				break;
		}

		dsg_barrier(dsg);
		dsg_free(dsg); 

		if (err == 0)
			uloga("All ok.\n");
		return 0;
	} else if (type == USE_DIMES) {
#ifdef DS_HAVE_DIMES
		struct dimes_server *dsg;
		dsg = dimes_server_alloc(num_sp, num_cp, "dataspaces.conf");
		if (!dsg)
			return -1;

		while (!dimes_server_complete(dsg)){
			err = dimes_server_process(dsg);
			if(err<0)
				break;
		}

		dimes_server_barrier(dsg);
		dimes_server_free(dsg); 

		if (err == 0)
			uloga("All ok.\n");
		return 0;
#else
		uloga("%s(): Dataspaces DIMES is not enabled!\n", __func__);
		return -1;
#endif
	}
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
/*
        uloga("%s(): var= %s, rank= %d, max= %f, min= %f, avg= %f\n",
                __func__, var_name, rank, max, min, avg);
*/
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
