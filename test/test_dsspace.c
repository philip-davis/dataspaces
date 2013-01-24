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
*/

#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "arpa/inet.h"
//#include "config.h"
#include "list.h"
#include <getopt.h>
#include "unistd.h"
#include <stdio.h>
//#include "dart_rpc_gni.h"
#include "ds_gspace.h"

#ifdef USE_MPI
#include "mpi.h"
#endif

static int num_sp;
static int num_cp;
static char *conf;

static struct ds_gspace *dsg;

struct {
	int		a, b, c;
	double		x, y, z;
} f_point_;


static void usage(void)
{
        printf("Usage: server OPTIONS\n"
		"OPTIONS: \n"
		"--server, -s    Number of server instance/staging nodes\n"
	        "--cnodes, -c    Number of compute nodes\n"
		"--conf, -f	 Define configuration file\n");
}

static int parse_args(int argc, char *argv[])
{
        const char opt_short[] = "s:c:f:";
        const struct option opt_long[] = {
		{"server",	1,	NULL,	's'},
		{"cnodes",	1,	NULL,	'c'},
		{"conf",	1,	NULL,	'f'},
		{NULL,		0,	NULL,	0}
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

static int srv_init(void)
{
	dsg = dsg_alloc(num_sp, num_cp, conf);
	if (!dsg) {
		return -1;
	}
	return 0;
}

static int srv_run(void)
{
	int err;

        while (!dsg_complete(dsg)) {
                err = dsg_process(dsg);
                if (err < 0) {
			/* If there is an error on the execution path,
			   I should stop the server. */

			dsg_free(dsg);

			/* TODO:  implement an  exit method  to signal
			   other servers to stop. */

			printf("Server exits due to error %d.\n", err);

			return err;
		}
        }

	return 0;
}

static int srv_finish(void)
{
        dsg_barrier(dsg);

        dsg_free(dsg);

	return 0;
}

int main(int argc, char *argv[])
{
	if (parse_args(argc, argv) < 0) {
		usage();
                return -1;
        }

#ifdef USE_MPI
        MPI_Comm comm_new;
        int nprocs, mpi_rank;

        MPI_Init(&argc, &argv);
        MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
        MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

#endif

	if (srv_init() < 0) {
		printf("DART server init failed!\n");
		return -1;
	}

	if (srv_run() < 0) {
		printf("DART server got an error at runtime!\n");
		return -1;
	}
	//printf("all finished, ready for finish.\n");
	srv_finish();

	printf("All ok.\n");

#ifdef USE_MPI
        MPI_Barrier(MPI_COMM_WORLD);
        MPI_Finalize();
#endif

        return 0;
}
