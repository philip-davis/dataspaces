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
*/

#ifndef __SS_DATA_H_
#define __SS_DATA_H_

#include <stdlib.h>

#include "bbox.h"
#include "list.h"

typedef struct {
	void			*iov_base;
	size_t			iov_len;
} iovec_t;

enum storage_type {row_major, column_major};

struct obj_descriptor {
        char                    name[154];

        enum storage_type       st;

        int                     owner;
        unsigned int            version;

        /* Global bounding box descriptor. */
        struct bbox             bb;

        /* Size of one element of a data object. */
        size_t                  size;

        char pad[2];//added by Tong Jin for 4 byte aligned in GEMINI
} __attribute__((__packed__));

struct global_dimension {
        int ndim;
        struct coord sizes;
} __attribute__((__packed__));

struct gdim_list_entry {
        struct list_head    entry;
        char *var_name;       
        struct global_dimension gdim; 
};

struct obj_data {
        struct list_head        obj_entry;

        struct obj_descriptor   obj_desc;
        struct global_dimension gdim;

        void			*_data;		/* Unaligned pointer */
        void                    *data;		/* Aligned pointer */

        /* Reference to the parent object; used only for sub-objects. */
        struct obj_data         *obj_ref;

        /* Count how many references are to this data object. */
        int                     refcnt;

        /* Flag to mark if we should free this data object. */
        unsigned int            f_free:1;
};

struct ss_storage {
        int                     num_obj;
        int                     size_hash;
        /* List of data objects. */
        struct list_head        obj_hash[1];
};

struct obj_desc_list {
	struct list_head	odsc_entry;
	struct obj_descriptor	odsc;
};

struct dht_entry {
        /* Global info. */
        struct sspace           *ss;
        struct bbox             bb;

        int                     rank;

        struct intv             i_virt;

        int                     num_intv;
        struct intv             *i_tab;

        // for v2 
        int num_bbox;
        int size_bb_tab;
        struct bbox             *bb_tab;

        int			odsc_size, odsc_num;
        struct list_head	odsc_hash[1];
};

struct dht {
        struct bbox             bb_glb_domain;

        int                     num_entries;
        struct dht_entry        *ent_tab[1];
};

enum sspace_hash_version {
    ssd_hash_version_v1 = 1, // (default) decompose the global data domain
                             //  using hilbert SFC
    ssd_hash_version_v2, // decompose the global data domain using
                         // recursive bisection of the longest dimension   
    _ssd_hash_version_count,
};

/*
  Shared space structure.
*/
struct sspace {
        uint64_t                   max_dim;
        unsigned int            bpd;

        struct dht              *dht;

        int                     rank;
        /* Pointer into "dht.ent_tab" corresponding to this node. */
        struct dht_entry        *ent_self;

        // for v2 
        int total_num_bbox;
        enum sspace_hash_version    hash_version;
};

struct sspace_list_entry {
        struct list_head    entry;
        struct global_dimension gdim;
        struct sspace   *ssd;
};

// Header for space info.
struct hdr_ss_info {
    int     num_dims;
    struct  coord dims;
    int     num_space_srv;
    unsigned char hash_version;
    int max_versions;
} __attribute__ ((__packed__));

/* Header structure for obj_get requests. */
struct hdr_obj_get {
    int                     qid;
    int                     rank;
    int                     rc;
    union {
        struct {
            /* Number of directory entries. */
            int                     num_de;
            struct obj_descriptor   odsc;
        } o;
        struct {
            /* Number of versions available. */
            int			num_vers;
            int			versions[1];
        } v;
	} u;
    struct global_dimension gdim;
} __attribute__((__packed__));

/* Header structure for obj_put requests. */
struct hdr_obj_put {
    struct obj_descriptor odsc;
    struct global_dimension gdim;
} __attribute__((__packed__));

/* Header structure for obj_filter requests. */
struct hdr_obj_filter {
        int                     qid;
        int                     rc;
        double                  res;
        struct obj_descriptor   odsc;
} __attribute__((__packed__));

/*  Header structure for sending binary codes. */
struct hdr_bin_code {
	/* Offset from the start of the code to the first loading
	   instruction for the PLT. */
	int			offset;

	/* Size of the code to be send and executed remotely. */
	int			size;

	int			qid;

	struct obj_descriptor	odsc;
} __attribute__((__packed__));

/* Header structure for returning the  result of the code execution on
   the space. */
struct hdr_bin_result {
	int			qid;
	int			rc;

	unsigned char		pad[210]; // max is sizeof(struct rpc_cmd.pad == 218)
} __attribute__((__packed__));

struct sspace* ssd_alloc(const struct bbox *, int, int, enum sspace_hash_version);
int ssd_init(struct sspace *, int);
void ssd_free(struct sspace *);
int ssd_copy(struct obj_data *, struct obj_data *);
// TODO: ssd_copyv is not supported yet
int ssd_copyv(struct obj_data *, struct obj_data *);
int ssd_copy_list(struct obj_data *, struct list_head *);
int ssd_filter(struct obj_data *, struct obj_descriptor *, double *);
int ssd_hash(struct sspace *, const struct bbox *, struct dht_entry *[]);

int dht_add_entry(struct dht_entry *, const struct obj_descriptor *);
const struct obj_descriptor * dht_find_entry(struct dht_entry *, const struct obj_descriptor *);
int dht_find_entry_all(struct dht_entry *, struct obj_descriptor *, 
                       const struct obj_descriptor *[]);
int dht_find_versions(struct dht_entry *, struct obj_descriptor *, int []);

struct ss_storage *ls_alloc(int max_versions);
void ls_free(struct ss_storage *);
void ls_add_obj(struct ss_storage *, struct obj_data *);
struct obj_data* ls_lookup(struct ss_storage *, char *);
void ls_remove(struct ss_storage *, struct obj_data *);
void ls_try_remove_free(struct ss_storage *, struct obj_data *);
struct obj_data * ls_find(struct ss_storage *, const struct obj_descriptor *);
struct obj_data * ls_find_no_version(struct ss_storage *, struct obj_descriptor *);

struct obj_data *obj_data_alloc(struct obj_descriptor *);
struct obj_data *obj_data_allocv(struct obj_descriptor *);
struct obj_data *obj_data_alloc_no_data(struct obj_descriptor *, void *);
struct obj_data *obj_data_alloc_with_data(struct obj_descriptor *, const void *);

void obj_data_free(struct obj_data *od);
void obj_data_free_with_data(struct obj_data *);
uint64_t obj_data_size(struct obj_descriptor *);
uint64_t obj_data_sizev(struct obj_descriptor *);

int obj_desc_equals(const struct obj_descriptor *, const struct obj_descriptor *);
int obj_desc_equals_no_owner(const struct obj_descriptor *, const struct obj_descriptor *);

int obj_desc_equals_intersect(const struct obj_descriptor *odsc1,
                const struct obj_descriptor *odsc2);

int obj_desc_by_name_intersect(const struct obj_descriptor *odsc1,
                const struct obj_descriptor *odsc2);

void copy_global_dimension(struct global_dimension *l, int ndim, const uint64_t *gdim);
int global_dimension_equal(const struct global_dimension* gdim1,
                const struct global_dimension* gdim2);
void init_gdim_list(struct list_head *gdim_list);
void update_gdim_list(struct list_head *gdim_list,
                const char *var_name, int ndim, uint64_t *gdim);
struct gdim_list_entry* lookup_gdim_list(struct list_head *gdim_list, const char *var_name);
void free_gdim_list(struct list_head *gdim_list);
void set_global_dimension(struct list_head *gdim_list, const char *var_name,
            const struct global_dimension *default_gdim, struct global_dimension *gdim);
#endif /* __SS_DATA_H_ */
