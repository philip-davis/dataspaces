#include <stdio.h>
#include <getopt.h>
#include "unistd.h"

#include "common.h"

#include "mpi.h"

static int num_sp;
static int num_cp;
static char *conf;

static void usage(void)
{
        printf("Usage: server OPTIONS\n"
                "OPTIONS: \n"
                "--server, -s    Number of server instance/staging nodes\n"
                "--cnodes, -c    Number of compute nodes\n"
                "--conf, -f      Define configuration file\n");
}

static int parse_args(int argc, char *argv[])
{
        const char opt_short[] = "s:c:f:";
        const struct option opt_long[] = {
                {"server",      1,      NULL,   's'},
                {"cnodes",      1,      NULL,   'c'},
                {"conf",        1,      NULL,   'f'},
                {NULL,          0,      NULL,   0}
        };

        int opt;

        while ((opt = getopt_long(argc, argv, opt_short, opt_long, NULL)) != -1) {
                switch (opt) {
                case 's':
                        num_sp = (optarg) ? atoi(optarg) : -1;
                        break;

                case 'c':
                        num_cp = (optarg) ? atoi(optarg) : -1;
                        break;

                case 'f':
                        conf = (optarg) ? optarg : NULL;
                        break;

                default:
                        printf("Unknown argument \n");
                }
        }

        if (num_sp <= 0)
                num_sp = 1;
        if (num_cp == 0)
                num_cp = 0;
        if (!conf)
                conf = "dataspaces.conf";
        return 0;
}

int main(int argc, char **argv)
{
        int err;
        int mpi_nprocs, mpi_rank;
        MPI_Comm gcomm;

        if (parse_args(argc, argv) < 0) {
                usage();
                return -1;
        }

        MPI_Init(&argc, &argv);
        MPI_Comm_size(MPI_COMM_WORLD, &mpi_nprocs);
        MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
        MPI_Barrier(MPI_COMM_WORLD);

        if (mpi_rank < num_sp) {
                MPI_Comm_split(MPI_COMM_WORLD, 0, mpi_rank, &gcomm);
                // Run as data space servers
                common_run_server(num_sp, num_cp, USE_DSPACES);
        } else {
                uloga("Error: wrong number of processes for test_space\n");
        }

        MPI_Barrier(MPI_COMM_WORLD);
        MPI_Finalize();

        return 0;
err_out:
        uloga("error out!\n");
        return -1;
}
