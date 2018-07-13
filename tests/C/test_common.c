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
#include "test_common.h"
#include "mpi.h"

int parse_args(int argc, char** argv, enum transport_type *type, int *npapp, 
	int *dims, int* npdim, uint64_t* spdim, int *timestep, int *appid, 
	size_t *elem_size, int *num_vars)
{
	int i = 0, j = 0, count = 0;
	*type = USE_DSPACES;
	if(0 == strcmp(argv[1], "DIMES")){
		*type = USE_DIMES;
	} 
	*npapp = atoi(argv[2]);
	*dims = atoi(argv[3]);
	count = 3;

	if(argc < 3 + (*dims)*2 + 2 + 1){
		uloga("Wrong number of arguments!\n");
		return -1;
	}

	for(i = count + 1, j = 0; j < *dims; i++, j++){
		*(npdim+j) = atoi(argv[i]);
	}
	count += *dims;

	for(i = count + 1, j = 0; j < *dims; i++, j++){
		*(spdim+j) = strtoull(argv[i], NULL, 10); 
	}
	count += *dims;

	*timestep = atoi(argv[++count]);
	*appid = atoi(argv[++count]);

	if(argc >= ++count + 1)
		*elem_size = atoi(argv[count]);
	else
		*elem_size = sizeof(double);

	if(argc >= ++count + 1)
		*num_vars = atoi(argv[count]);
	else
		*num_vars = 1;

	return 0;
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

int common_get_transport_type_str(enum transport_type type, char* str)
{
    if (type == USE_DSPACES) {
        sprintf(str, "DATASPACES");
    } else if (type == USE_DIMES) {
        sprintf(str, "DIMES");
    } else sprintf(str, "UNKNOWN");

    return 0;
}
