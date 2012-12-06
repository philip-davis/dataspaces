#include "dataspaces.h"

int main(int argc, char *argv[])
{
	int appid = 1;
	check_dc_alloc();
	check_dc_process();
	check_dc_free();

	check_ds_alloc();
	check_ds_process();
	check_ds_free();

	return 0;
}
