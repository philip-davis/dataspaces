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

int parse_args(int argc, char **argv, int *npapp, int *npx, int *npy, int *npz,
	int *spx, int *spy, int *spz, int *timestep)
{
	if (argc < 9) {
		uloga("Wrong number of arguments!\n");
		return -1;
	}

	*npapp = atoi(argv[1]);
	*npx = atoi(argv[2]);	
	*npy = atoi(argv[3]);	
	*npz = atoi(argv[4]);	
	*spx = atoi(argv[5]);	
	*spy = atoi(argv[6]);	
	*spz = atoi(argv[7]);	
	*timestep = atoi(argv[8]);	

	return 0;
}

int common_init(int num_peers, int appid) {
        return dspaces_init(num_peers, appid);
}

void common_set_storage_type(int fst) {
        dspaces_set_storage_type(fst);
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
        unsigned int ver, int size,
        int xl, int yl, int zl,
        int xu, int yu, int zu,
        void *data, enum transport_type type) {
        if ( type == USE_DSPACES ) {
               /* return dspaces_put(var_name, ver, size,
                        xl, yl, zl, xu, yu, zu,
                        data);
		*/
		int lb[3] = {xl, yl, zl};
		int ub[3] = {xu, yu, zu};
		return dspaces_put(var_name, ver, size,
			3, lb, ub,
			data);
        } else if (type == USE_DIMES) {
#ifdef DS_HAVE_DIMES
                return dimes_put(var_name, ver, size,
                        xl, yl, zl, xu, yu, zu,
                        data);
#else
                uloga("%s(): DataSpaces DIMES is not enabled!\n", __func__);
                return -1;
#endif
        }
}

int common_get (const char *var_name,
        unsigned int ver, int size,
        int xl, int yl, int zl,
        int xu, int yu, int zu,
        void *data, enum transport_type type) {
        if ( type == USE_DSPACES ) {
                /*return dspaces_get(var_name, ver, size,
                        xl, yl, zl, xu, yu, zu,
                        data);
		*/
                int lb[3] = {xl, yl, zl};
                int ub[3] = {xu, yu, zu};
		return dspaces_get(var_name, ver, size,
			3, lb, ub,
			data);		
        } else if (type == USE_DIMES) {
#ifdef DS_HAVE_DIMES
                return dimes_get(var_name, ver, size,
                        xl, yl, zl, xu, yu, zu,
                        data);
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
