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

		if (err = 0)
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

		if (err = 0)
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
			uloga("%s(): var= %s, rank= %d, check data error buf[i]=%f, ts=%d\n",
				__func__, var_name, rank, buf[i], ts);
		}
        }
        avg = sum / num_elem;
        uloga("%s(): var= %s, rank= %d, max= %f, min= %f, avg= %f\n",
                __func__, var_name, rank, max, min, avg);
        return;
}
