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
#include <limits.h>

#include "bbox.h"
#include "list.h"
#include <sys/mman.h>
#include <fcntl.h>
#include <sys/shm.h>
#include <sys/stat.h>

#define ODSC_NAME_SIZE                 118 //duan original 154, 100 for 3D, 154 for 2D in IB, all 126 in Gimni, 118 for recovery in Gimni
#define ODSC_PAD_SIZE                  2  //duan


#define DSG_FT_LEVEL			2				//duan
#define NUM_DATA_DEVICE			3				//duan
#define NUM_CODE_DEVICE			1				//duan
#define MAX_OB_IN_BD            1000            //duan
#define INIT_THRESHOLD			4				//duan 100, 4, 0
#define REP_PERCENT				0.5				//duan
#define CODE_TECH				Reed_Sol_Van	//duan
#define WORD_SIZE				8				//word size 8, 16, 32 duan


typedef struct {
	void			*iov_base;
	size_t			iov_len;
} iovec_t;

enum code_technique { No_Coding, Reed_Sol_Van, Reed_Sol_R6_Op, Cauchy_Orig, Cauchy_Good, Liberation, Blaum_Roth, Liber8tion };//duan
enum block_type { data_block, code_block };//duan
enum obj_status { replicate, encode, replicating, encoding, deleting };//delete, lock duan
enum encode_token { token_off, token_on};//delete, loc duan

enum storage_type { row_major, column_major };

struct obj_descriptor {
	char                    name[ODSC_NAME_SIZE];

	enum storage_type       st;
	enum obj_status         ob_status; //duan
	int                     owner;
	int                     code_owner;// there is limitaion for array size duan
	double					time_stamp;
	unsigned int            version;

        /* Global bounding box descriptor. */
        struct bbox             bb;

        /* Size of one element of a data object. */
        size_t                  size;

		char pad[ODSC_PAD_SIZE];//added by Tong Jin for 4 byte aligned in GEMINI
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
		uint64_t                obj_size;//duan
		struct block_data		*block_ref;//duan
};

//duan
struct block_data {
	struct list_head        obj_entry;

	struct obj_descriptor   *obj_desc;
	struct global_dimension gdim;

	void					*_data;		/* Unaligned pointer */
	void                    *data;		/* Aligned pointer */

	/* Reference to the parent object; used only for sub-objects. */
	struct obj_data         *obj_ref;

	/* Count how many references are to this data object. */
	int                     refcnt;

	/* Flag to mark if we should free this data object. */
	unsigned int            f_free : 1;

	/* Flag to mark if we should free this data object. */
	uint64_t                block_size;//block data size except for obj_desc duan
	int						block_index;
	int						num_data_device;
	int						num_code_device;
	int						word_size;
	enum code_technique     code_tech;
	enum block_type			bd_type;
};

struct ss_storage {
        int                     num_obj;
		uint64_t                size_obj;//duan
		enum encode_token		token; //duan
        int                     size_hash;
        /* List of data objects. */
        struct list_head        obj_hash[1];
};

///duan
struct ssb_storage {
	int                     num_block;
	uint64_t                size_block;//duan
	int                     size_hash;
	/* List of data objects. */
	struct list_head        block_hash[1];
};
///

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
#ifdef DS_SYNC_MSG
    int* sync_op_id_ptr; //synchronization lock pointer
#endif
} __attribute__((__packed__));

/* Header structure for obj_get requests. duan */
struct hdr_obj_syn {
	int						num_odsc;
	enum encode_token		token;//only for lock
	struct obj_descriptor odsc;
	struct global_dimension gdim;
} __attribute__((__packed__));

/* Header structure for block_get, block_delete requests. duan*/
struct hdr_block_get {
	/* block_id = DSG_ID + current time. */
	struct				obj_descriptor odsc;
	int					block_index;
} __attribute__((__packed__));

/* Header structure for block_get requests. duan*/
struct hdr_block_put {

	struct					global_dimension gdim;
	uint64_t                block_size;//duan
	int						block_index;
	enum block_type			bd_type;
	/* size of each odsc, each odsc has the same size. */
	int                     num_odsc;

} __attribute__((__packed__));

/* Header structure for peer_status_update requests. duan*/
struct hdr_peer_update {
	int                     peer_id;
	int                     status;
} __attribute__((__packed__));

/* Header structure for meta_get requests. */
struct hdr_nvars_get {
    int current_version;
    char f_name[128];
} __attribute__((__packed__));

/* Header structure for meta_get requests. */
struct hdr_var_meta_get {
    int current_version;
    int length;
    char f_name[128];
} __attribute__((__packed__));

/* Header structure for obj_filter requests. */
struct hdr_obj_filter {
        int                     qid;
        int                     rc;
        double                  res;
        struct obj_descriptor   odsc;
} __attribute__((__packed__));

/* Header structure for killing dataspace server. */
struct hdr_dsg_kill {
        int				kill_flag;
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
int ssd_partition(struct obj_data *to_obj, struct obj_data *from_obj, uint64_t size);//duan
int ssd_copy(struct obj_data *, struct obj_data *);
int ssd_copy_local(struct obj_data *, struct obj_data *);
// TODO: ssd_copyv is not supported yet
int ssd_copyv(struct obj_data *, struct obj_data *);
int ssd_copy_list(struct obj_data *, struct list_head *);
int ssd_copy_list_shmem(struct obj_data *, struct list_head *, int);
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
struct obj_data * ls_find_next(struct ss_storage *, const struct obj_descriptor *);
struct obj_data * ls_find_latest(struct ss_storage *, const struct obj_descriptor *);
struct obj_data * ls_find_no_version(struct ss_storage *, struct obj_descriptor *);

///////////////////////////////////////////duan
struct ssb_storage *lsb_alloc(int max_versions);
void lsb_free(struct ssb_storage *);
void lsb_add_block(struct ssb_storage *, struct block_data *);
struct obj_data* ls_cold_data_lookup(struct ss_storage *ls, int offset);//duan
void lsb_remove(struct ssb_storage *, struct block_data *);
void lsb_try_remove_free(struct ssb_storage *, struct block_data *);
int lsb_find_all(struct ssb_storage *lsb, const struct obj_descriptor *odsc, struct block_data * bd_data[]);//duan
struct block_data * lsb_find(struct ssb_storage *, struct obj_descriptor *);
struct block_data * lsb_find_no_version(struct ssb_storage *lsb, struct obj_descriptor *odsc);//duan
struct block_data *block_data_alloc_with_size(struct obj_descriptor *odsc, uint64_t size);//duan
struct block_data *block_data_alloc_copy(struct block_data *bd_from);//duan
struct block_data *block_data_alloc_copy_no_data(struct block_data *bd_from, void *data);//duan
int block_data_alloc(struct obj_data * od[], int num_od, uint64_t size, struct block_data * bd_data[], struct block_data * bd_code[]);
void block_data_encode(struct block_data * bd_data[], struct block_data * bd_code[]);//duan
void block_data_decode(struct block_data * bd_data[], struct block_data * bd_code[]);//duan

void block_data_free(struct block_data *bd); //duan
void block_data_free_without_odsc(struct block_data *bd);//duan
int block_obj_intersect(int *ob_index_begin, int * num_bd_in_od, const struct block_data * bd_data);//duan
////////////////////////////////////////////duan

struct obj_data *obj_data_alloc(struct obj_descriptor *);
struct obj_data *shmem_obj_data_alloc(struct obj_descriptor *, int);
struct obj_data *obj_data_allocv(struct obj_descriptor *);
struct obj_data *obj_data_alloc_no_data(struct obj_descriptor *, void *);
struct obj_data *obj_data_alloc_with_data(struct obj_descriptor *, const void *);
struct obj_data *obj_data_alloc_with_size(struct obj_descriptor *odsc, uint64_t  data_size);//duan

void obj_data_free(struct obj_data *od);
void shmem_obj_data_free(struct obj_data *od);
void obj_data_free_with_data(struct obj_data *);
struct obj_data *obj_data_alloc_with_data_split(struct obj_descriptor *odsc, const void *data, struct obj_descriptor *odsc_big);
uint64_t obj_data_size(struct obj_descriptor *);
uint64_t obj_data_sizev(struct obj_descriptor *);

int obj_desc_equals(const struct obj_descriptor *, const struct obj_descriptor *);
int obj_desc_equals_no_owner(const struct obj_descriptor *, const struct obj_descriptor *);

int obj_desc_equals_intersect(const struct obj_descriptor *odsc1,
                const struct obj_descriptor *odsc2);

int copy_obj_desc(struct obj_descriptor *odsc1, const struct obj_descriptor *odsc2);//duan

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
void convert_to_string(struct obj_descriptor *odsc, char *name);
void set_global_dimension(struct list_head *gdim_list, const char *var_name,
            const struct global_dimension *default_gdim, struct global_dimension *gdim);
#endif /* __SS_DATA_H_ */
