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
#include <getopt.h>
#include "unistd.h"
#include "mpi.h"

#include "hstaging_scheduler_serial_job.h"

#include "common.h"
#include "hstaging_partition.h"

//static enum execution_mode exemode = hybrid_staging_mode;
static enum execution_mode exemode = staging_mode;
static enum core_type coretype = manager_core;
static enum worker_type workertype;
static enum location_type loctype = 0;

int main(int argc, char **argv)
{
	int err;
	int color;

	hs_comm_init(argc, argv);

	err = hs_comm_perform_split(exemode, coretype, loctype, &workertype);
	if (err < 0) {
		goto err_out;
	}

#ifdef DEBUG
	uloga("dataspaces server starts...\n");
#endif

	if (hstaging_scheduler_serial_parse_args(argc, argv) < 0) {
		hstaging_scheduler_serial_usage();
		return -1;
	}

	if (hstaging_scheduler_serial_init() < 0) {
		printf("DART server init failed!\n");
		return -1;
	}

	if (hstaging_scheduler_serial_run() < 0) {
		printf("DART server got an error at runtime!\n");
		return -1;
	}

	hstaging_scheduler_serial_finish();

	uloga("All ok.\n");	

	hs_comm_fin();

	return 0;
err_out:
	uloga("error out!\n");
	return -1;
}
