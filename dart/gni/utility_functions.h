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
*  Tong Jin (2011) TASSL Rutgers University
*  tjin@cac.rutgers.edu
*/

/*
 * This header file contains the common utility functions.
 */
#ifndef __UTILITY_FUNC_H__
#define __UTILITY_FUNC_H__

#include <stdlib.h>
#include <sched.h>
#ifdef CRAY_CONFIG_GHAL_ARIES
#include "aries/misc/exceptions.h"
#endif
#include <sys/utsname.h>
#include <mpi.h>

static int             compare_data_failed = 0;
static int             rank_id;
struct utsname  uts_info;
static int             v_option = 0;

static int             aborted = 0;
char           *command_name;
static int             expected_passed = 0;
static int             failed = 0;
static int             passed = 0;

#define INCREMENT_ABORTED aborted++
#define INCREMENT_FAILED  failed++
#define INCREMENT_PASSED  passed++


/*
 * allgather gather the requested information from all of the ranks.
 */
static void
allgather(void *in, void *out, int len, void *comm)
{
    static int      already_called = 0;
    int             i;
    static int     *ivec_ptr = NULL;
    static int      job_size = 0;
    static int      my_rank;
    int         my_app_rank;
    int         app_job_size;
    int         appnum;
    char           *out_ptr;
    int             rc;
    char           *tmp_buf;

    if (!already_called) {
        if(comm) {
            rc = MPI_Comm_size(*((MPI_Comm *)comm), &job_size);
            assert(rc == MPI_SUCCESS);

            rc = MPI_Comm_rank(*((MPI_Comm *)comm), &my_rank);
            assert(rc == MPI_SUCCESS);

            ivec_ptr = (int *) malloc(sizeof(int) * job_size);
            assert(ivec_ptr != NULL);
            rc = MPI_Allgather(&my_rank, 1, MPI_INT, ivec_ptr, 1, MPI_INT, *((MPI_Comm *)comm));
            assert(rc == MPI_SUCCESS);

        } else {
            rc = PMI_Get_size(&job_size);
            assert(rc == PMI_SUCCESS);

            rc = PMI_Get_rank(&my_rank);
            assert(rc == PMI_SUCCESS);

            ivec_ptr = (int *) malloc(sizeof(int) * job_size);
            assert(ivec_ptr != NULL);

            rc = PMI_Allgather(&my_rank, ivec_ptr, sizeof(int));
            assert(rc == PMI_SUCCESS);
        }
        already_called = 1;
    }

    tmp_buf = (char *) malloc(job_size * len);
    assert(tmp_buf);

    if(comm) {
    rc = MPI_Allgather(in, len, MPI_BYTE, tmp_buf, len, MPI_BYTE, *((MPI_Comm *)comm));
    assert(rc == MPI_SUCCESS);
    } else {
        rc = PMI_Allgather(in, tmp_buf, len);
        assert(rc == PMI_SUCCESS);
    }

    out_ptr = out;

    for (i = 0; i < job_size; i++) {
        memcpy(&out_ptr[len * ivec_ptr[i]], &tmp_buf[i * len], len);
    }
    free(tmp_buf);
}

/*
 * get_gni_nic_address get the nic address for the specified device.
 *
 *   Returns: the nic address for the specified device.
 */

static unsigned int
get_gni_nic_address(int device_id)
{
    int             alps_address = -1;
    int             alps_dev_id = -1;
    unsigned int    address,
                    cpu_id;
    gni_return_t    status;
    int             i;
    char           *token,
                   *p_ptr;

    //p_ptr = getenv("PMI_GNI_DEV_ID");
    p_ptr = NULL;
    if (!p_ptr) {

        /*
         * Get the nic address for the specified device.
         */

        status = GNI_CdmGetNicAddress(device_id, &address, &cpu_id);
        if (status != GNI_RC_SUCCESS) {
            fprintf(stdout,
                    "GNI_CdmGetNicAddress ERROR status: %d\n", status);
            abort();
        }
    } else {

        /*
         * Get the ALPS device id from the PMI_GNI_DEV_ID environment
         * variable.
         */

        while ((token = strtok(p_ptr, ":")) != NULL) {
            alps_dev_id = atoi(token);
            if (alps_dev_id == device_id) {
                break;
            }

            p_ptr = NULL;
        }

        assert(alps_dev_id != -1);

        p_ptr = getenv("PMI_GNI_LOC_ADDR");
        assert(p_ptr != NULL);

        i = 0;

        /*
         * Get the nic address for the ALPS device.
         */

        while ((token = strtok(p_ptr, ":")) != NULL) {
            if (i == alps_dev_id) {
                alps_address = atoi(token);
                break;
            }

            p_ptr = NULL;
            ++i;
        }

        assert(alps_address != -1);
        address = alps_address;
    }

    return address;
}

/*
 * gather_nic_addresses gather all of the nic addresses for all of the
 *                      other ranks.
 *
 *   Returns: an array of addresses for all of the nics from all of the
 *            other ranks.
 */

static void    *
gather_nic_addresses(void)
{
    size_t          addr_len;
    unsigned int   *all_addrs;
    unsigned int    local_addr;
    int             rc;
    int             size;

    /*
     * Get the size of the process group.
     */

    rc = PMI_Get_size(&size);
    assert(rc == PMI_SUCCESS);

    /*
     * Assuming a single gemini device.
     */

    local_addr = get_gni_nic_address(0);

    addr_len = sizeof(unsigned int);

    /*
     * Allocate a buffer to hold the nic address from all of the other
     * ranks.
     */

    all_addrs = (unsigned int *) malloc(addr_len * size);
    assert(all_addrs != NULL);

    /*
     * Get the nic addresses from all of the other ranks.
     */

    allgather(&local_addr, all_addrs, sizeof(int), NULL);

    return (void *) all_addrs;
}

/*
 * get cookie and ptag from shared communication domain information
 * for SPMD model
 */

static int get_named_dom(const char *pname, uint8_t *_ptag, uint32_t *_cookie)
{
  char buffer[255];
  FILE *fdom = NULL;
  char name[16];
  char *returnb;
  uint8_t ptag;
  uint32_t cookie;

  //  system("apstat -P > apstat.out");
  fdom = popen("apstat -P", "r");
  if(fdom == NULL)
    {
      perror("fopen");
      exit(-1);
    }

    do
      {
	returnb = fgets(buffer, 255, fdom);
        if(returnb == NULL)
	  break;
        sscanf(buffer, "%s %*s %*d %hhu %x",
	       &name, &ptag, &cookie);
        if(!strcasecmp(name, pname))
	  {
            *_ptag = ptag;
            *_cookie = cookie;
	    pclose(fdom);
            return 0;
	  }

      }while(1);

    pclose(fdom);
    return -1;

}

/*
 * get cookie and ptag from system shared communication domain ADIOS information
 * for SPMD model
 */

static int get_adios_dom(uint8_t *_ptag, uint32_t *_cookie)
{
  return get_named_dom("ADIOS", _ptag, _cookie);
}

/*                                                                                                                                            
 * get cookie and ptag from shared communication domain information                                                                           
 * for SPMD model at Aries based system, e.g. EOS                                                                                             
 */
static int get_named_dom_aries(const char *pname, uint32_t *_cookie, uint32_t *_cookie2)
{
  char buffer[255];
  FILE *fdom = NULL;
  char name[16];
  char *returnb;
  uint32_t cookie;
  uint32_t cookie2;

  //  system("apstat -P > apstat.out");                                                                                                       
  fdom = popen("apstat -P", "r");
  if(fdom == NULL)
    {
      perror("fopen");
      exit(-1);
    }

    do
      {
       	returnb = fgets(buffer, 255, fdom);
        if(returnb == NULL)
          break;
        sscanf(buffer, "%s %*s %*d %x %x",
               &name, &cookie, &cookie2);
        //printf("Buffer info: name (%s), cookie(%x), cookie2(%x).\n", name, cookie, cookie2);                                                
        if(!strcasecmp(name, pname))
          {
            *_cookie = cookie;
            *_cookie2 = cookie2;
            pclose(fdom);
            return 0;
          }

      }while(1);

    pclose(fdom);
    return -1;
}


/*
 * get_cookie will get the cookie value associated with this process.
 *
 * Returns: the cookie value.
 */

static          uint32_t
get_cookie(void)
{
    uint32_t        cookie;
    char           *p_ptr;
    char           *token;

    p_ptr = getenv("PMI_GNI_COOKIE");
    assert(p_ptr != NULL);

    token = strtok(p_ptr, ":");
    cookie = (uint32_t) atoi(token);

    return cookie;
}

/*
 * get_cookie_env will get the cookie value from environment variable DSPACES_GNI_COOKIE
 * it expects a hexadecimal value
 *
 * Returns: the cookie value.
 */
#include "debug.h"
static          uint32_t
get_cookie_env(char * envvar)
{
    uint32_t        cookie;
    char           *p_ptr;
    char           *ep;

    p_ptr = getenv(envvar);

    if (p_ptr) {
        // Hexa convert 0x... numbers
        cookie = (uint32_t) strtol(p_ptr,&ep,16);
        //uloga("---- cookieenv: ptr=%u str=%s, ep=%u, epstr=%s, cookie=%d, cookiex=%x\n", 
        //    p_ptr, p_ptr, ep, ep, cookie, cookie);
        if (ep != NULL && *ep != '\0')
            cookie = 0;
    } else {
        cookie = 0;
    }

    return cookie;
}


/*
 * get_cq_event will process events from the completion queue.
 *
 *   cq_handle is the completion queue handle.
 *   uts_info contains the node name.
 *   rank_id is the rank of this process.
 *   source_cq determines if the CQ is a source or a
 *       destination completion queue. 
 *
 *   Returns:  gni_cq_entry_t for success
 *             0 on error
 */

static gni_cq_entry_t
get_cq_event(gni_cq_handle_t cq_handle, struct utsname uts_info,
             int rank_id, unsigned int source_cq)
{
    gni_cq_entry_t  event_data = 0;
    uint64_t        event_type;
    gni_return_t    status = GNI_RC_SUCCESS;
    int             wait_count = 0;

    status = GNI_RC_NOT_DONE;
    while (status == GNI_RC_NOT_DONE) {

        /*
         * Get the next event from the specified completion queue handle.
         */

        status = GNI_CqGetEvent(cq_handle, &event_data);
        if (status == GNI_RC_SUCCESS) {

            /*
             * Processed event succesfully.
             */

            if (v_option > 1) {
                event_type = GNI_CQ_GET_TYPE(event_data);

                if (event_type == GNI_CQ_EVENT_TYPE_POST) {
                    if (source_cq == 1) {
                        fprintf(stdout,
                                "[%s] Rank: %4i GNI_CqGetEvent    source      type: POST(%lu) inst_id: %lu tid: %lu event: 0x%16.16lx\n",
                                uts_info.nodename, rank_id,
                                event_type,
                                GNI_CQ_GET_INST_ID(event_data),
                                GNI_CQ_GET_TID(event_data),
                                event_data);
                    } else {
                        fprintf(stdout,
                                "[%s] Rank: %4i GNI_CqGetEvent    destination type: POST(%lu) inst_id: %lu event: 0x%16.16lx\n",
                                uts_info.nodename, rank_id,
                                event_type,
                                GNI_CQ_GET_INST_ID(event_data),
                                event_data);
                    }
                } else if (event_type == GNI_CQ_EVENT_TYPE_SMSG) {
                    if (source_cq == 1) {
                        fprintf(stdout,
                                "[%s] Rank: %4i GNI_CqGetEvent    source      type: SMSG(%lu) msg_id: 0x%8.8x event: 0x%16.16lx\n",
                                uts_info.nodename, rank_id,
                                event_type,
                                (unsigned int) GNI_CQ_GET_MSG_ID(event_data),
                                event_data);
                    } else {
                        fprintf(stdout,
                                "[%s] Rank: %4i GNI_CqGetEvent    destination type: SMSG(%lu) data: 0x%16.16lx event: 0x%16.16lx\n",
                                uts_info.nodename, rank_id,
                                event_type,
                                GNI_CQ_GET_DATA(event_data),
                                event_data);
                    }
                } else if (event_type == GNI_CQ_EVENT_TYPE_MSGQ) {
                    if (source_cq == 1) {
                        fprintf(stdout,
                                "[%s] Rank: %4i GNI_CqGetEvent    source      type: MSGQ(%lu) msg_id: 0x%8.8x event: 0x%16.16lx\n",
                                uts_info.nodename, rank_id,
                                event_type,
                                (unsigned int) GNI_CQ_GET_MSG_ID(event_data),
                                event_data);
                    } else {
                        fprintf(stdout,
                                "[%s] Rank: %4i GNI_CqGetEvent    destination type: MSGQ(%lu) data: 0x%16.16lx event: 0x%16.16lx\n",
                                uts_info.nodename, rank_id,
                                event_type,
                                GNI_CQ_GET_DATA(event_data),
                                event_data);
                    }
                } else {
                    if (source_cq == 1) {
                        fprintf(stdout,
                                "[%s] Rank: %4i GNI_CqGetEvent    source      type: %lu inst_id: %lu event: 0x%16.16lx\n",
                                uts_info.nodename, rank_id,
                                event_type,
                                GNI_CQ_GET_DATA(event_data),
                                event_data);
                    } else {
                        fprintf(stdout,
                                "[%s] Rank: %4i GNI_CqGetEvent    destination type: %lu data: 0x%16.16lx event: 0x%16.16lx\n",
                                uts_info.nodename, rank_id,
                                event_type,
                                GNI_CQ_GET_DATA(event_data),
                                event_data);
                    }
                }
            }

            return event_data;
        } else if (status != GNI_RC_NOT_DONE) {

            /*
             * An error occurred getting the event.
             */

            char           *cqErrorStr;
            char           *cqOverrunErrorStr = "";
            gni_return_t    tmp_status = GNI_RC_SUCCESS;
#ifdef CRAY_CONFIG_GHAL_ARIES
            uint32_t        status_code;

            status_code = GNI_CQ_GET_STATUS(event_data);
            if (status_code == A_STATUS_AT_PROTECTION_ERR) {
                return 0;
            }
#endif

            /*
             * Did the event queue overrun condition occurred?
             * This means that all of the event queue entries were used up
             * and another event occurred, i.e. there was no entry available
             * to put the new event into.
             */

            if (GNI_CQ_OVERRUN(event_data)) {
                cqOverrunErrorStr = "CQ_OVERRUN detected ";

                if (v_option > 2) {
                    fprintf(stdout,
                            "[%s] Rank: %4i ERROR CQ_OVERRUN detected\n",
                            uts_info.nodename, rank_id);
                }
            }

            cqErrorStr = (char *) malloc(256);
            if (cqErrorStr != NULL) {

                /*
                 * Print a user understandable error message.
                 */

                tmp_status = GNI_CqErrorStr(event_data, cqErrorStr, 256);
                if (tmp_status == GNI_RC_SUCCESS) {
                    fprintf(stdout,
                            "[%s] Rank: %4i GNI_CqGetEvent    ERROR %sstatus: %d inst_id: %lu event: 0x%16.16lx GNI_CqErrorStr: %s\n",
                            uts_info.nodename, rank_id, cqOverrunErrorStr, status,
                            GNI_CQ_GET_INST_ID(event_data),
                            event_data,
                            cqErrorStr);
                } else {

                    /*
                     * Print the error number.
                     */

                    fprintf(stdout,
                            "[%s] Rank: %4i GNI_CqGetEvent    ERROR %sstatus: %d inst_id: %lu event: 0x%16.16lx\n",
                            uts_info.nodename, rank_id, cqOverrunErrorStr, status,
                            GNI_CQ_GET_INST_ID(event_data),
                            event_data);
                }

                free(cqErrorStr);
            } else {

                /*
                 * Print the error number.
                 */

                fprintf(stdout,
                        "[%s] Rank: %4i GNI_CqGetEvent    ERROR %sstatus: %d inst_id: %lu event: 0x%16.16lx\n",
                        uts_info.nodename, rank_id, cqOverrunErrorStr, status,
                        GNI_CQ_GET_INST_ID(event_data),
                        event_data);
            }
            return 0;
        } else {

            /*
             * An event has not been received yet.
             *
             * Release the cpu to allow the event to be received.
             * This is basically a sleep, if other processes need to do some work.
             */

            sched_yield();

            wait_count++;

            if (wait_count >= 200000) {
                /*
                 * This prevents an indefinite retry, which could hang the
                 * application.
                 */

                fprintf(stdout,
                        "[%s] Rank: %4i GNI_CqGetEvent    ERROR no event was received status: %d retry count: %d\n",
                        uts_info.nodename, rank_id, status, wait_count);
                return 0;
            }
        }
    }

    return event_data;
}

/*
 * get_ptag will get the ptag value associated with this process.
 *
 * Returns: the ptag value.
 */

static          uint8_t
get_ptag(void)
{
    char           *p_ptr;
    uint8_t         ptag;
    char           *token;

    p_ptr = getenv("PMI_GNI_PTAG");
    assert(p_ptr != NULL);

    token = strtok(p_ptr, ":");
    ptag = (uint8_t) atoi(token);

    return ptag;
}

/*
 * get_ptag_env will get the ptag value from environment variable DSPACES_GNI_PTAG
 *
 * Returns: the ptag value.
 */

static          uint8_t
get_ptag_env(char * envvar)
{
    char           *p_ptr;
    uint8_t         ptag;
    char           *ep;

    p_ptr = getenv(envvar);

    if (p_ptr) { 
        ptag = (uint8_t) strtol(p_ptr, &ep, 10);
        if (ep == NULL)
            ptag = 0;
    } else {
        ptag = 0;
    }

    return ptag;
}
/*
 * print_results will determine if the test was successful or not
 *               and then print a message according to this result.
 *
 *   Returns:  0 for a success
 *            -1 for a failure
 *            -2 for an abort
 */

static inline int
print_results(void)
{
    char            abort_string[256];
    char           *exit_status;
    int             rc;

    if (aborted > 0) {

        /*
         * Abort the application's other ranks.
         */

        sprintf(abort_string, "%s called abort", command_name);
        PMI_Abort(-1, abort_string);
        exit_status = "Aborted      ";
        rc = -2;
    } else {

        /*
         * Wait for the other ranks to get here.
         */

        rc = PMI_Barrier();
        assert(rc == PMI_SUCCESS);

        if (failed > 0) {

            /*
             * This test failed.
             */

            exit_status = "Failed       ";
            rc = -1;
        } else if (passed != expected_passed) {

            /*
             * This test did not have the correct number of passes.
             */

            exit_status = "Indeterminate";
            rc = 0;
        } else {
            /*
             * This test executed successfully.
             */

            exit_status = "Passed       ";
            rc = 0;
        }
    }

    /*
     * Print the results from this test.
     */

    fprintf(stdout, "[%s] Rank: %4i %s:    %s    Test Results    Passed: %i/%i Failed: %i Aborted: %i\n",
            uts_info.nodename, rank_id, command_name, exit_status,
            passed, expected_passed, failed, aborted);

    return rc;
}

#endif
