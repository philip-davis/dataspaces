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
*  Qian Sun (2014)  TASSL Rutgers University
*  qiansun@cac.rutgers.edu
*/
#ifndef __TEST_DS_COMMON_H_
#define __TEST_DS_COMMON_H_

#include "debug.h"
#include "timer.h"
#include "dataspaces.h"
#include "ds_gspace.h"
#ifdef DS_HAVE_DIMES
#include "dimes_interface.h"
#include "dimes_server.h"
#endif

enum transport_type {
	USE_DSPACES = 0,
	USE_DIMES
};
// ----------------------------------------------------------- test trace on Caliburn -----------------------------------------------------------
//list						          0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50		// duan "0": no action; "1": node #1 fail; "-1": node #1 recovery.
  #define FAILURE_RECOVERY_SERIES	{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan failure free.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 1 failure ; degraded; MTBF = 150.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 1 failure ; lazy; MTBF = 150.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 2 failures; degraded; MTBF = 75.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0,-1, 0, 2, 0,-2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 2 failures; lazy; MTBF = 75.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 3 failures; degraded; MTBF = 50.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0,-1, 0, 2, 0,-2, 0, 3, 0,-3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 3 failures; lazy; MTBF = 50.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 4 failures; degraded; MTBF = 35.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0,-1, 0, 2, 0,-2, 0, 3, 0,-3, 4,-4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 4 failures; lazy; MTBF = 35.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 4, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 5 failures; degraded; MTBF = 30.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0,-1, 0, 2, 0,-2, 0, 3, 0,-3, 4,-4, 5,-5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 5 failures; lazy; MTBF = 30.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 4, 0, 5, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 6 failures; degraded; MTBF = 25.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0,-1, 0, 2, 0,-2, 0, 3, 0,-3, 4,-4, 5,-5, 6,-6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 6 failures; lazy; MTBF = 25.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 4, 0, 5, 0, 6, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 7 failures; degraded; MTBF = 20.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0,-1, 0, 2, 0,-2, 0, 3, 0,-3, 4,-4, 5,-5, 6,-6, 7,-7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 7 failures; lazy; MTBF = 20.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 4, 0, 5, 0, 6, 0, 7, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 8 failures; degraded; MTBF = 18.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0,-1, 0, 2, 0,-2, 0, 3, 0,-3, 4,-4, 5,-5, 6,-6, 7,-7, 0, 8, 0,-8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 8 failures; lazy; MTBF = 18.
//#define FAILURE_RECOVERY_SERIES	{ 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 0}		// duan test failure.
// ----------------------------------------------------------- test trace on Titan -----------------------------------------------------------
//list						      0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50		// duan "0": no action; "1": fail; "2": recovery.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan failure free.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 1 failure; MTBF = 150.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0, 2, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 2 failures; MTBF = 75.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0, 2, 0, 1, 0, 2, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 3 failures; MTBF = 50.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0, 2, 0, 1, 0, 2, 0, 1, 0, 2, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 4 failures; MTBF = 35.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0, 2, 0, 1, 0, 2, 0, 1, 0, 2, 1, 2, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 5 failures; MTBF = 30.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0, 2, 0, 1, 0, 2, 0, 1, 0, 2, 1, 2, 1, 2, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 6 failures; MTBF = 25.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0, 2, 0, 1, 0, 2, 0, 1, 0, 2, 1, 2, 1, 2, 1, 2, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 7 failures; MTBF = 20.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 0, 0, 2, 0, 1, 0, 2, 0, 1, 0, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 10 failures; MTBF = 15.
//#define FAILURE_RECOVERY_SERIES	{ 0, 0, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}		// duan 15 failures; MTBF = 10.
//#define FAILURE_RECOVERY_SERIES	{ 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 0, 0, 0}		// duan 30 failures; MTBF = 05.
//#define FAILURE_RECOVERY_SERIES	{ 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 0}		// duan test failure.
// wrapper functions of DataSpaces/DIMES APIs
int common_init(int num_peers, int appid, void* comm, const char* parameters);
int common_rank(); 
int common_peers();
void common_barrier();
void common_finalize();
void common_kill();
void common_lock_on_read(const char *lock_name, void *gcomm);
void common_unlock_on_read(const char *lock_name, void *gcomm);
void common_lock_on_write(const char *lock_name, void *gcomm); 
void common_unlock_on_write(const char *lock_name, void *gcomm); 
int common_put (const char *var_name,
        unsigned int ver, int size,
	int ndim,
        uint64_t *lb, uint64_t *ub,
	void *data, enum transport_type type);
int common_get (const char *var_name,
        unsigned int ver, int size,
	int ndim,
	uint64_t *lb, uint64_t *ub,
        void *data, enum transport_type type); 
int common_put_sync(enum transport_type type); 
int common_run_server(int num_sp, int num_cp, enum transport_type type, void* gcomm);

double get_curr_time(void);//duan
#endif /* __TEST_DS_COMMON_H_ */
