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

#ifndef __DS_GSPACE_H_
#define __DS_GSPACE_H_

#include "dart.h"
#include "ss_data.h"

struct ds_gspace {
        struct dart_server      *ds;

        /* Shared space data structure. */
        struct sspace           *ssd;
        /* Local in-memory storage. */
        struct ss_storage       *ls;
        /* Default global data domain dimension */
        struct global_dimension default_gdim;

        /* List of dynamically added shared space. */ 
        struct list_head        sspace_list;

        /* Continuous query list. */
        struct list_head        cq_list;
        int                     cq_num;

        /* Pending object descriptors request list. */
        struct list_head        obj_desc_req_list;

        /* Pending object data request list. */
        struct list_head        obj_data_req_list;

        /* List of allocated locks. */
        struct list_head        locks_list;
};

struct ds_gspace *dsg_alloc(int, int, char *, void *);
void dsg_free(struct ds_gspace *);
int dsg_process(struct ds_gspace *);
int dsg_complete(struct ds_gspace *);

int dsghlp_obj_put(struct ds_gspace *, struct obj_data *);
int dsghlp_get_rank(struct ds_gspace *);
int dsghlp_all_sp_joined(struct ds_gspace *);

int dsg_barrier(struct ds_gspace *);

#endif /* __DS_GSPACE_H_ */
