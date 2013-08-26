#include <stdio.h>
#include "common.h"

int read_config_file(const char *fname,
	int *num_sp, int *num_cp, int *iter,
	int *num_writer, int *w,
	int *num_reader, int *r,
	int *dims, int *dim//int *dim_x, int *dim_y, int *dim_z, int *dim4
	) {
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

int parse_args(int argc, char **argv, int *ndim, int *npapp, int *np, int*sp, int *timestep)
{
        //if (argc < 9) {
        //        uloga("Wrong number of arguments!\n");
        //        return -1;
        //}
	int i = 0, j = 0;
	int count = 0;
	*ndim = atoi(argv[1]);
        *npapp = atoi(argv[2]);
	count = 2;
	for(i = count + 1, j = 0; j < *ndim; i++, j++){
		*(np+j) = atoi(argv[i]);
	//	printf("np[j]=%d", *(np+j));
	}
	count += *ndim;
	//printf("\n");
       // *npx = atoi(argv[2]);
       // *npy = atoi(argv[3]);
       // *npz = atoi(argv[4]);
	       
	for(i = count + 1, j = 0; j < *ndim; i++, j++){
                *(sp+j) = atoi(argv[i]);
	//	printf("sp[j]=%d", *(sp+j));
        }
	count += *ndim;
	//printf("\n");
        //*spx = atoi(argv[5]);
        //*spy = atoi(argv[6]);
        //*spz = atoi(argv[7]);


        *timestep = atoi(argv[count+1]);

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
        int ndim,
        //int xl, int yl, int zl, int l4,
        int *lb, int *ub,
        //int xu, int yu, int zu, int u4,
        void *data, enum transport_type type) {
        if ( type == USE_DSPACES ) {
                //int lb[4] = {xl, yl, zl, l4};
                //int ub[4] = {xu, yu, zu, u4};
                return dspaces_put(var_name, ver, size,
                        ndim,
                        lb, ub,//xl, yl, zl, xu, yu, zu,
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
        //int xl, int yl, int zl, int l4,
        //int xu, int yu, int zu, int u4,
        int ndim,
        int *lb, int *ub,
        void *data, enum transport_type type) {
        if ( type == USE_DSPACES ) {
                //int lb[4] = {xl, yl, zl, l4};
                //int ub[4] = {xu, yu, zu, u4};
		return dspaces_get(var_name, ver, size,
                        ndim,
                        lb, ub,//xl, yl, zl, xu, yu, zu,
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
                return dimes_put_sync();
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

//void check_data(double *buf, int num_elem, int rank)
void check_data(double *buf, __u64 num_elem, int rank)
{
        double max, min, sum, avg;
        //int i;
	__u64 i;

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
        }
        avg = sum / num_elem;
        uloga("%s(): rank= %d, max= %f, min= %f, avg= %f\n",
                __func__, rank, max, min, avg);
        return;
}
