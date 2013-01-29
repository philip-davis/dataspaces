#include <stdio.h>

#include "common.h"

#include "debug.h"
#include "ds_gspace.h"

extern int read_config_file(const char *fname,
        int *num_sp, int *num_cp, int *iter,
        int *num_writer, int *writer_x, int *writer_y, int *writer_z,
        int *num_reader, int *reader_x, int *reader_y, int *reader_z,
        int *dims, int *dim_x, int *dim_y, int *dim_z);

int main(int argc, char **argv)
{
	int err;

	int num_sp, num_cp, iter;
	int num_writer,writer_x,writer_y,writer_z; 
	int num_reader,reader_x,reader_y,reader_z;
	int dims, dim_x, dim_y, dim_z;
	if(read_config_file("computenode.conf",
		&num_sp, &num_cp, &iter,
		&num_writer, &writer_x, &writer_y, &writer_z,
		&num_reader, &reader_x, &reader_y, &reader_z,
		&dims, &dim_x, &dim_y, &dim_z) != 0) {
		goto err_out;
	}
	
	//run as data space servers
	err = common_run_server(num_sp, num_cp);
	if(err = 0)
		uloga("All ok.\n");

	return 0;
err_out:
	uloga("error out!\n");
	return -1;	
}
