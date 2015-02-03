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
	int nprocs, rank;
	MPI_Comm gcomm;
	int color;

	if (parse_args(argc, argv) < 0) {
		usage();
		return -1;
	}

	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Barrier(MPI_COMM_WORLD);

#ifdef DEBUG
	uloga("dataspaces server starts...\n");
#endif

	color = 0;
	MPI_Comm_split(MPI_COMM_WORLD, 0, rank, &gcomm);

#ifdef DS_HAVE_DIMES
	common_run_server(num_sp, num_cp, USE_DIMES, &gcomm);
#else
	common_run_server(num_sp, num_cp, USE_DSPACES, &gcomm);
#endif

	MPI_Barrier(gcomm);
	MPI_Finalize();

	return 0;
err_out:
	uloga("error out!\n");
	return -1;
}
