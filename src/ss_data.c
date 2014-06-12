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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <errno.h>

#include "debug.h"
#include "ss_data.h"
#include "queue.h"

#ifdef TIMING_SSD
#include "timer.h"
#endif

// TODO: I should  import the header file with  the definition for the
// iovec_t data type.

/*
  A view in  the matrix allows to extract any subset  of values from a
  matrix.
*/
struct matrix_view {
        __u64   lb[BBOX_MAX_NDIM];
        __u64   ub[BBOX_MAX_NDIM];	
};

/* Generic matrix representation. */
struct matrix {
        __u64   dist[BBOX_MAX_NDIM];
        int 			        num_dims;
        size_t                  size_elem;
        enum storage_type       mat_storage;
        struct matrix_view      mat_view;
        void                    *pdata;
};

/*
  Cache structure to "map" a bounding box to corresponding nodes in
  the space.
*/
struct sfc_hash_cache {
        struct list_head                sh_entry;

        struct bbox                     sh_bb;

        struct dht_entry                **sh_de_tab;
        int                             sh_nodes;
};

static LIST_HEAD(sfc_hash_list);
static int is_sfc_hash_list_free = 0;

static uint64_t next_pow_2_v2(uint64_t n)
{
        uint64_t i;

        i = 1;
        while (i < n) {
            i = i << 1;
        }

        return i;
}

static int compute_bits_v2(__u64 n)
{
        int nr_bits = 0;

        while (n>1) {
                n = n >> 1;
                nr_bits++;
        }

        return nr_bits;
}

static int compute_bits(__u64 n)
{
        int nr_bits = 0;

        while (n) {
                n = n >> 1;
                nr_bits++;
        }

        return nr_bits;
}

int sh_add(const struct bbox *bb, struct dht_entry *de_tab[], int n)
{
        struct sfc_hash_cache *shc;
        int i, err = -ENOMEM;

        shc = malloc(sizeof(*shc) + sizeof(de_tab[0]) * n);
        if (!shc)
                goto err_out;

        shc->sh_bb = *bb;
        shc->sh_nodes = n;

        shc->sh_de_tab = (struct dht_entry **) (shc+1);
        for (i = 0; i < n; i++)
                shc->sh_de_tab[i] = de_tab[i];

        list_add_tail(&shc->sh_entry, &sfc_hash_list);

        return 0;
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
}

int sh_find(const struct bbox *bb, struct dht_entry *de_tab[])
{
        struct sfc_hash_cache *shc;
        int i;

        list_for_each_entry(shc, &sfc_hash_list, struct sfc_hash_cache, sh_entry) {
                if (bbox_equals(bb, &shc->sh_bb)) {
                        for (i = 0; i < shc->sh_nodes; i++)
                                de_tab[i] = shc->sh_de_tab[i];
                        return shc->sh_nodes;
                }
        }

        return -1;
}

void sh_free(void)
{
        if (is_sfc_hash_list_free) return;

        struct sfc_hash_cache *l, *t;
        int n = 0;

        list_for_each_entry_safe(l, t, &sfc_hash_list, struct sfc_hash_cache, sh_entry) {
                free(l);
                n++;
        }
#ifdef DEBUG
        uloga("'%s()': SFC cached %d object descriptors, size = %zu.\n", 
            __func__, n, sizeof(*l) *n);
#endif
        is_sfc_hash_list_free = 1;
}

static void matrix_init(struct matrix *mat, enum storage_type st,
                        struct bbox *bb_glb, struct bbox *bb_loc, 
                        void *pdata, size_t se)
{
    int i;
    int ndims = bb_glb->num_dims;
	
    memset(mat, 0, sizeof(struct matrix));

    for(i = 0; i < ndims; i++){
        mat->dist[i] = bbox_dist(bb_glb, i);
        mat->mat_view.lb[i] = bb_loc->lb.c[i] - bb_glb->lb.c[i];
        mat->mat_view.ub[i] = bb_loc->ub.c[i] - bb_glb->lb.c[i];
    }

    mat->num_dims = ndims;
    mat->mat_storage = st;
    mat->pdata = pdata;
    mat->size_elem = se;
}

static void matrix_copy(struct matrix *a, struct matrix *b)
{
        char *A = a->pdata;
        char *B = b->pdata;

        __u64 a0, a1, a2, a3, a4, a5, a6, a7, a8, a9;
        __u64 aloc=0, aloc1=0, aloc2=0, aloc3=0, aloc4=0, aloc5=0, aloc6=0, aloc7=0, aloc8=0, aloc9=0;
        __u64 b0, b1, b2, b3, b4, b5, b6, b7, b8, b9;
        __u64 bloc=0, bloc1=0, bloc2=0, bloc3=0, bloc4=0, bloc5=0, bloc6=0, bloc7=0, bloc8=0, bloc9=0;

    switch(a->num_dims){
        case(1):    
            goto dim1;
            break;
        case(2):
            goto dim2;
            break;
        case(3):
            goto dim3;
            break;
        case(4):
            goto dim4;
            break;
        case(5):
            goto dim5;
            break;
        case(6):
            goto dim6;
            break;
        case(7):
            goto dim7;
            break;
        case(8):
            goto dim8;
            break;
        case(9):
            goto dim9;
            break;
        case(10):
            goto dim10;
            break;
        default:
            break;
    }
    
dim10:        for(a9 = a->mat_view.lb[9], b9 = b->mat_view.lb[9];   //TODO-Q
            a9 <= a->mat_view.ub[9]; a9++, b9++){
            aloc9 = a9 * a->dist[8];
            bloc9 = a9 * b->dist[8];
dim9:        for(a8 = a->mat_view.lb[8], b8 = b->mat_view.lb[8];    //TODO-Q
            a8 <= a->mat_view.ub[8]; a8++, b8++){
            aloc8 = (aloc9 + a8) * a->dist[7];
            bloc8 = (bloc9 + b8) * b->dist[7];
dim8:        for(a7 = a->mat_view.lb[7], b7 = b->mat_view.lb[7];    //TODO-Q
            a7 <= a->mat_view.ub[7]; a7++, b7++){
            aloc7 = (aloc8 + a7) * a->dist[6];
            bloc7 = (bloc8 + b7) * b->dist[6];
dim7:        for(a6 = a->mat_view.lb[6], b6 = b->mat_view.lb[6];    //TODO-Q
            a6 <= a->mat_view.ub[6]; a6++, b6++){
            aloc6 = (aloc7 + a6) * a->dist[5];
            bloc6 = (bloc7 + b6) * b->dist[5];
dim6:        for(a5 = a->mat_view.lb[5], b5 = b->mat_view.lb[5];    //TODO-Q
            a5 <= a->mat_view.ub[5]; a5++, b5++){
            aloc5 = (aloc6 + a5) * a->dist[4];
            bloc5 = (bloc6 + b5) * b->dist[4];
dim5:        for(a4 = a->mat_view.lb[4], b4 = b->mat_view.lb[4];
            a4 <= a->mat_view.ub[4]; a4++, b4++){
            aloc4 = (aloc5 + a4) * a->dist[3];
            bloc4 = (bloc5 + b4) * b->dist[3];
dim4:        for(a3 = a->mat_view.lb[3], b3 = b->mat_view.lb[3];
            a3 <= a->mat_view.ub[3]; a3++, b3++){
            aloc3 = (aloc4 + a3) * a->dist[2];
            bloc3 = (bloc4 + b3) * b->dist[2];
dim3:            for(a2 = a->mat_view.lb[2], b2 = b->mat_view.lb[2];
                a2 <= a->mat_view.ub[2]; a2++, b2++){
                aloc2 = (aloc3 + a2) * a->dist[1];
                bloc2 = (bloc3 + b2) * b->dist[1];
dim2:                for(a1 = a->mat_view.lb[1], b1 = b->mat_view.lb[1];
                    a1 <= a->mat_view.ub[1]; a1++, b1++){
                    aloc1 = (aloc2 + a1) * a->dist[0];
                    bloc1 = (bloc2 + b1) * b->dist[0];
dim1:                    for(a0 = a->mat_view.lb[0], b0 = b->mat_view.lb[0];
                        a0 <= a->mat_view.ub[0]; a0++, b0++){
                        aloc = aloc1 + a0;
                        bloc = bloc1 + b0;
                        //memcpy(&(*A)[aloc], &(*B)[bloc], a->size_elem);
                        memcpy(&A[aloc*a->size_elem], &B[bloc*a->size_elem], a->size_elem);
                    }
            if(a->num_dims == 1)    return;
                }
        if(a->num_dims == 2)    return;
            }
        if(a->num_dims == 3)    return; 
        }
    if(a->num_dims == 4)    return;
    }
    if(a->num_dims == 5)    return;
    }
    if(a->num_dims == 6)    return;
    }
    if(a->num_dims == 7)    return;
    }
    if(a->num_dims == 8)    return;
    }
    if(a->num_dims == 9)    return;
    }
}

/* a = destination, b = source. Destination uses iovec_t format. */
static void matrix_copyv(struct matrix *a, struct matrix *b)
{
/*
        typedef char matrix_elem_generic_t[a->size_elem];

        int ai, aj, bi, bj, bk; // , ak;
        int n;

        if (a->mat_storage == column_major && b->mat_storage == column_major) {

		// matrix_elem_generic_t (*A)[a->dimz][a->dimx][a->dimy] = a->pdata;
                matrix_elem_generic_t (*B)[b->dimz][b->dimx][b->dimy] = b->pdata;
		iovec_t *A = a->pdata;

                // Column major data representation (Fortran style) 
                n = a->mat_view.ub[bb_y] - a->mat_view.lb[bb_y] + 1;
		// ak = a->mat_view.lb[bb_y];
                bk = b->mat_view.lb[bb_y];
                for (ai = a->mat_view.lb[bb_z], bi = b->mat_view.lb[bb_z]; 
                     ai <= a->mat_view.ub[bb_z]; ai++, bi++) {
                        for (aj = a->mat_view.lb[bb_x], bj = b->mat_view.lb[bb_x]; 
                             aj <= a->mat_view.ub[bb_x]; aj++, bj++) {
				A->iov_base = &(*B)[bi][bj][bk];
				A->iov_len = a->size_elem * n;
				A++;
			}
                }
        }
        else if (a->mat_storage == row_major && b->mat_storage == row_major) {

                // matrix_elem_generic_t (*A)[a->dimz][a->dimy][a->dimx] = a->pdata;
                matrix_elem_generic_t (*B)[b->dimz][b->dimy][b->dimx] = b->pdata;
		iovec_t *A = a->pdata;

                n = a->mat_view.ub[bb_x] - a->mat_view.lb[bb_x] + 1;
		// ak = a->mat_view.lb[bb_x];
                bk = b->mat_view.lb[bb_x];
                for (ai = a->mat_view.lb[bb_z], bi = b->mat_view.lb[bb_z];
                     ai <= a->mat_view.ub[bb_z]; ai++, bi++) {
                        for (aj = a->mat_view.lb[bb_y], bj = b->mat_view.lb[bb_y];
                             aj <= a->mat_view.ub[bb_y]; aj++, bj++) {
				A->iov_base = &(*B)[bi][bj][bk];
				A->iov_len = a->size_elem * n;
				A++;
			}
                }
        }
*/
	/*
        else if (a->mat_storage == column_major && b->mat_storage == row_major) {

                matrix_elem_generic_t (*A)[a->dimz][a->dimx][a->dimy] = a->pdata;
                matrix_elem_generic_t (*B)[b->dimz][b->dimy][b->dimx] = b->pdata;

                for (ai = a->mat_view.lb[bb_z], bi = b->mat_view.lb[bb_z];
                     ai <= a->mat_view.ub[bb_z]; ai++, bi++) {
                        for (aj = a->mat_view.lb[bb_x], bj = b->mat_view.lb[bb_y];
                             aj <= a->mat_view.ub[bb_x]; aj++, bj++) {
                                for (ak = a->mat_view.lb[bb_y], bk = b->mat_view.lb[bb_y];
                                     ak <= a->mat_view.ub[bb_y]; ak++, bk++) 
                                        memcpy(&(*A)[ai][aj][ak], 
                                               &(*B)[bi][bk][bj],
                                               a->size_elem);
                                // (*A)[ai][aj][ak] = (*B)[bi][bk][bj];
                        }
                }
        }
        else if (a->mat_storage == row_major && b->mat_storage == column_major) {

                matrix_elem_generic_t (*A)[a->dimz][a->dimy][a->dimx] = a->pdata;
                matrix_elem_generic_t (*B)[b->dimz][b->dimx][b->dimy] = b->pdata;

                for (ai = a->mat_view.lb[bb_z], bi = b->mat_view.lb[bb_z];
                     ai <= a->mat_view.ub[bb_z]; ai++, bi++) {
                        for (aj = a->mat_view.lb[bb_y], bj = b->mat_view.lb[bb_y];
                             aj <= a->mat_view.ub[bb_y]; aj++, bj++) {
                                for (ak = a->mat_view.lb[bb_x], bk = b->mat_view.lb[bb_x];
                                     ak <= a->mat_view.ub[bb_x]; ak++, bk++)
                                        memcpy(&(*A)[ai][aj][ak], 
                                               &(*B)[bi][bk][bj],
                                               a->size_elem);
                                // (*A)[ai][aj][ak] = (*B)[bi][bk][bj];
                        }
                }
        }
	*/
}

static void get_bbox_max_dim(const struct bbox *bb, uint64_t *out_max_dim,
    int *out_dim)
{
    int i;
    uint64_t max_dim = 0;
    for (i = 0; i < bb->num_dims; i++) {
        if (max_dim < bb->ub.c[i]) {
            max_dim = bb->ub.c[i];
            *out_dim = i;
        }
    }
    *out_max_dim = max_dim;
}

static void get_bbox_max_dim_size(const struct bbox *bb, uint64_t *out_max_dim_size,
    int *out_dim)
{
    int i;
    uint64_t max_dim_size = 0;
    for (i = 0; i < bb->num_dims; i++) {
        uint64_t size = bb->ub.c[i]-bb->lb.c[i]+1;
        if (max_dim_size < size) {
            max_dim_size = size;
            *out_dim = i;
        }
    }
    *out_max_dim_size = max_dim_size;    
    
}

static struct dht_entry * dht_entry_alloc(struct sspace *ssd, int size_hash)
{
	struct dht_entry *de;
	int i;

	de = malloc(sizeof(*de) + sizeof(struct obj_desc_list)*(size_hash-1));
	if (!de) {
		errno = ENOMEM;
		return de;
	}
	memset(de, 0, sizeof(*de));

	de->ss = ssd;
	de->odsc_size = size_hash;

	for (i = 0; i < size_hash; i++)
		INIT_LIST_HEAD(&de->odsc_hash[i]);

    de->num_bbox = 0;
    de->size_bb_tab = 0;
    de->bb_tab = NULL;
	return de;
}

static void dht_entry_free(struct dht_entry *de)
{
	struct obj_desc_list *l, *t;
	int i;

	//TODO: free the *intv and other resources.
	free(de->i_tab);
	for (i = 0; i < de->odsc_size; i++) {
		list_for_each_entry_safe(l, t, &de->odsc_hash[i], struct obj_desc_list, odsc_entry) 
			free(l);
	}

	free(de);
}

static struct dht * 
dht_alloc(struct sspace *ssd, struct bbox *bb_domain, int num_nodes, int size_hash)
{
	struct dht *dht;
	int i;

	dht = malloc(sizeof(*dht) + sizeof(struct dht_entry)*(num_nodes-1));
	if (!dht) {
		errno = ENOMEM;
		return dht;
	}
	memset(dht, 0, sizeof(*dht));

    dht->bb_glb_domain = *bb_domain;
    dht->num_entries = num_nodes;

	for (i = 0; i < num_nodes; i++) {
		dht->ent_tab[i] = dht_entry_alloc(ssd, size_hash);
		if (!dht->ent_tab[i])
			break;
	}

	if (i != num_nodes) {
		errno = ENOMEM;
		while (--i > 0)
			free(dht->ent_tab[i]);
		free(dht);
		dht = 0;
	}

	return dht;
}

static void dht_free(struct dht *dht)
{
	int i;

	for (i = 0; i < dht->num_entries; i++)
		free(dht->ent_tab[i]);

	free(dht);
}

static void dht_free_v2(struct dht *dht)
{
    int i;

    for (i = 0; i < dht->num_entries; i++) {
        if (dht->ent_tab[i]->bb_tab) {
            free(dht->ent_tab[i]->bb_tab);
        }
        free(dht->ent_tab[i]);
    }

    free(dht);
}

static int dht_intersect(struct dht_entry *de, struct intv *itv)
{
        int i;

        if (de->i_virt.lb > itv->ub || de->i_virt.ub < itv->lb)
                return 0;

        for (i = 0; i < de->num_intv; i++)
                if (intv_do_intersect(&de->i_tab[i], itv))
                        return 1;
        return 0;
}

static __u64 ssd_get_max_dim(struct sspace *ss)
{
        return ss->max_dim;
}

static int ssd_get_bpd(struct sspace *ss)
{
        return ss->bpd;
}

/*
  Hash the global geometric domain space to 1d index, and map a piece
  to each entry in the dht->ent_tab.
*/
static int dht_construct_hash(struct dht *dht, struct sspace *ssd)
{
        const __u64 sn = bbox_volume(&dht->bb_glb_domain) / dht->num_entries;
        struct intv *i_tab, intv;
        struct dht_entry *de;
        __u64 len;
        int num_intv, i, j;
        int err = -ENOMEM;

        bbox_to_intv(&dht->bb_glb_domain, ssd->max_dim, ssd->bpd, 
                     &i_tab, &num_intv);

        /*
        printf("Global domain decomposes into: ");
        for (i = 0; i < num_intv; i++)
                printf("{%llu,%llu} ", i_tab[i].lb, i_tab[i].ub);
        printf("\n");
        */

        for (i = 0, j = 0; i < dht->num_entries; i++) {
                len = sn;

                de = dht->ent_tab[i];
                de->rank = i;
                de->i_tab = malloc(sizeof(struct intv) * num_intv);
                if (!de->i_tab)
                        break;

                // printf("Node rank %d interval cut: ", i);
                while (len > 0) {
                        if (intv_size(&i_tab[j]) > len) {
                                intv.lb = i_tab[j].lb;
                                intv.ub = intv.lb + len - 1;
                                i_tab[j].lb += len;
                        }
                        else { 
                                intv = i_tab[j++];
                        }
                        len -= intv_size(&intv);
                        de->i_tab[de->num_intv++] = intv;
                        // printf("{%llu,%llu} ", intv.lb, intv.ub);
                }

                de->i_virt.lb = de->i_tab[0].lb;
                de->i_virt.ub = de->i_tab[de->num_intv-1].ub;
                de->i_tab = realloc(de->i_tab, sizeof(intv) * de->num_intv);
                if (!de->i_tab)
                        break;

                //printf("\n");
        }

        free(i_tab);

        if (i == dht->num_entries)
                return 0;

        uloga("'%s()': failed at entry %d.\n", __func__, i);
        return err;
}

static struct sspace *ssd_alloc_v1(struct bbox *bb_domain, int num_nodes, int max_versions)
{
        struct sspace *ssd;
        __u64 max_dim;
        int err = -ENOMEM;

        ssd = malloc(sizeof(*ssd));
        if (!ssd)
                goto err_out;
        memset(ssd, 0, sizeof(*ssd));

        ssd->dht = dht_alloc(ssd, bb_domain, num_nodes, max_versions);
        if (!ssd->dht) {
            free(ssd);
            goto err_out;
        }

        int i;
        max_dim = bb_domain->ub.c[0];
        for(i = 1; i < bb_domain->num_dims; i++){
                max_dim = max(bb_domain->ub.c[i], max_dim);
        }

        ssd->max_dim = next_pow_2(max_dim);
        ssd->bpd = compute_bits(ssd->max_dim);

        err = dht_construct_hash(ssd->dht, ssd);
        if (err < 0) {
                dht_free(ssd->dht);
                free(ssd);
                goto err_out;
        }

        ssd->hash_version = ssd_hash_version_v1;
        return ssd;
 err_out:
        uloga("'%s()': failed with %d\n", __func__, err);
        return NULL;
}

static void ssd_free_v1(struct sspace *ssd)
{
        dht_free(ssd->dht);
        free(ssd);
        sh_free();
}

static int ssd_hash_v1(struct sspace *ss, const struct bbox *bb, struct dht_entry *de_tab[])
{
        struct intv *i_tab;
        int i, k, n, num_nodes;

        num_nodes = sh_find(bb, de_tab);
        if (num_nodes > 0)
                /* This is great, I hit the cache. */
                return num_nodes;

        num_nodes = 0;

        // bbox_to_intv(bb, ss->max_dim, ss->bpd, &i_tab, &n);
        bbox_to_intv2(bb, ss->max_dim, ss->bpd, &i_tab, &n);
        //  printf("After bbox_to_intv2 num_intv = %d\n", n);
        //  printf("ss->dht->num_entries = %d\n", ss->dht->num_entries);

        for (k = 0; k < ss->dht->num_entries; k++){
                for (i = 0; i < n; i++) {
                        if (dht_intersect(ss->dht->ent_tab[k], &i_tab[i])) {
                                de_tab[num_nodes++] = ss->dht->ent_tab[k];
                                break;
                        }
                }
        }

        //  printf("num_nodes = %d\n", num_nodes);

        /* Cache the results for later use. */
        sh_add(bb, de_tab, num_nodes);

        free(i_tab);
        return num_nodes;
}

struct sspace *ssd_alloc_v2(const struct bbox *bb_domain, int num_nodes, int max_versions)
{
        struct sspace *ssd = NULL;
        int err = -ENOMEM;
        int i, j, k;
        int dim;
        int nbits_max_dim = 0;
        uint64_t max_dim = 0;
        get_bbox_max_dim(bb_domain, &max_dim, &dim);
        max_dim = next_pow_2_v2(max_dim);
        nbits_max_dim = compute_bits_v2(max_dim);
        //printf("%s(): max_dim= %llu nbits_max_dim= %d\n",
        //    __func__, max_dim, nbits_max_dim);

        // decompose the global bbox       
        int num_divide_iteration = compute_bits_v2(next_pow_2_v2(num_nodes));
        struct bbox *bb, *b1, *b2;
        struct queue q1, q2;
        queue_init(&q1);
        queue_init(&q2);

        //printf("%s(): num_nodes= %d num_divide_iteration= %d\n", __func__,
        //    num_nodes, num_divide_iteration);

        bb = malloc(sizeof(struct bbox));
        memcpy(bb, bb_domain, sizeof(struct bbox));
        queue_enqueue(&q1, bb);
        for (i = 0; i < num_divide_iteration; i++) {
            struct queue *src_q, *dst_q;
            if (!queue_is_empty(&q1) && queue_is_empty(&q2)) {
                src_q = &q1;
                dst_q = &q2;
            } else if (!queue_is_empty(&q2) && queue_is_empty(&q1)) {
                src_q = &q2;
                dst_q = &q1;
            } else {
                printf("%s(): error, both q1 and q2 is (non)empty.\n", __func__);
            }
            while (!queue_is_empty(src_q)) {
                uint64_t max_dim_size;
                struct bbox b_tab[2];

                bb = queue_dequeue(src_q);
                get_bbox_max_dim_size(bb, &max_dim_size, &dim);
                bbox_divide_in2_ondim(bb, b_tab, dim);
                free(bb); bb = NULL;
                b1 = malloc(sizeof(struct bbox));
                b2 = malloc(sizeof(struct bbox));
                *b1 = b_tab[0];
                *b2 = b_tab[1];
                queue_enqueue(dst_q, b1);
                queue_enqueue(dst_q, b2);
            }
        }

        struct queue *q;
        if (!queue_is_empty(&q1)) q = &q1;
        else if (!queue_is_empty(&q2)) q = &q2;

        ssd = malloc(sizeof(*ssd));
        if (!ssd)
                goto err_out;
        memset(ssd, 0, sizeof(*ssd));

        ssd->max_dim = max_dim;
        ssd->bpd = nbits_max_dim;

        ssd->total_num_bbox = queue_size(q);
        ssd->dht = dht_alloc(ssd, bb_domain, num_nodes, max_versions);
        if (!ssd->dht) {
            free(ssd);
            goto err_out;
        }
        for (i = 0; i < num_nodes; i++) {
            ssd->dht->ent_tab[i]->rank = i;
        }

        int n = ceil(ssd->total_num_bbox*1.0 / ssd->dht->num_entries);
        for (i = 0; i < ssd->dht->num_entries; i++) {
            ssd->dht->ent_tab[i]->size_bb_tab = n;
            ssd->dht->ent_tab[i]->bb_tab = malloc(sizeof(struct bbox)*n);
        }
        //printf("%s(): ssd->total_num_bbox= %d ssd->dht->num_entries= %d"
        //    " max_num_bbox_per_dht_entry= %d\n",
        //    __func__, ssd->total_num_bbox, ssd->dht->num_entries, n);

        // simple round-robing mapping of decomposed bbox to dht entries
        i = 0;
        while (! queue_is_empty(q)) {
            bb = queue_dequeue(q);
            j = i++ % ssd->dht->num_entries;
            k = ssd->dht->ent_tab[j]->num_bbox++;
            ssd->dht->ent_tab[j]->bb_tab[k] = *bb;
            free(bb);
        }

/*
        for (i = 0; i < ssd->dht->num_entries; i++) {
            printf("dht entry %d size_bb_tab= %d num_bbox= %d\n", i,
                ssd->dht->ent_tab[i]->size_bb_tab,
                ssd->dht->ent_tab[i]->num_bbox);
            for (j = 0; j < ssd->dht->ent_tab[i]->num_bbox; j++) {
                bbox_print(&ssd->dht->ent_tab[i]->bb_tab[j]);
            }        
            printf("\n");
        }
*/

        ssd->hash_version = ssd_hash_version_v2;        
        return ssd;
 err_out:
        uloga("'%s()': failed with %d\n", __func__, err);
        return NULL;
}

void ssd_free_v2(struct sspace *ssd)
{
        dht_free_v2(ssd->dht);
        free(ssd);
        sh_free();
}

int ssd_hash_v2(struct sspace *ss, const struct bbox *bb, struct dht_entry *de_tab[])
{
        int i, j, num_nodes;

        num_nodes = sh_find(bb, de_tab);
        if (num_nodes > 0)
                /* This is great, I hit the cache. */
                return num_nodes;

        num_nodes = 0;
        for (i = 0; i < ss->dht->num_entries; i++) {
            for (j = 0; j < ss->dht->ent_tab[i]->num_bbox; j++) {
                if (bbox_does_intersect(bb, &ss->dht->ent_tab[i]->bb_tab[j])) {
                    //bbox_print(bb);
                    //bbox_print(&ss->dht->ent_tab[i]->bb_tab[j]);
                    //printf(" i= %d rank= %d\n", i, ss->dht->ent_tab[i]->rank);
                    de_tab[num_nodes++] = ss->dht->ent_tab[i];
                    break;
                }
            }
        }

        /* Cache the results for later use. */
        sh_add(bb, de_tab, num_nodes);

        return num_nodes;
}

/*
  Public API starts here.
*/

/*
 ssd hashing function v1: uses Hilbert SFC to linearize the global data domain
    and bounding box passed by put()/get().
 ssd hashing function v2: NOT use Hilbert SFC for linearization.
*/

/*
  Allocate the shared space structure.
*/
struct sspace *ssd_alloc(struct bbox *bb_domain, int num_nodes, int max_versions,
    enum sspace_hash_version hash_version)
{
    struct sspace *ss = NULL;

#ifdef TIMING_SSD
    struct timer tm;
    double tm_st, tm_end;
    timer_init(&tm, 1);
    timer_start(&tm);
    tm_st = timer_read(&tm);
#endif

    switch (hash_version) {
    case ssd_hash_version_v1:
        ss = ssd_alloc_v1(bb_domain, num_nodes, max_versions);
        break;
    case ssd_hash_version_v2:
        ss = ssd_alloc_v2(bb_domain, num_nodes, max_versions);
        break;
    default:
        uloga("%s(): ERROR unknown shared space hash version %u\n",
            __func__, hash_version);
        break;
    }

#ifdef TIMING_SSD 
    tm_end = timer_read(&tm);
    uloga("%s(): hash_version v%u time %lf seconds\n", __func__, hash_version, tm_end-tm_st);
#endif

    return ss;
}

void ssd_free(struct sspace *ss)
{
    switch (ss->hash_version) {
    case ssd_hash_version_v1:
        ssd_free_v1(ss);
        break;
    case ssd_hash_version_v2:
        ssd_free_v2(ss);
        break;
    default:
        uloga("%s(): ERROR unknown shared space hash version %u\n",
            __func__, ss->hash_version);
        break;
    }
}

/*
  Hash a bounding box 'bb' to the hash entries in dht; fill in the
  entries in the de_tab and return the number of entries.
*/
int ssd_hash(struct sspace *ss, const struct bbox *bb, struct dht_entry *de_tab[])
{
#ifdef TIMING_SSD
    struct timer tm;
    double tm_st, tm_end;
    timer_init(&tm, 1);
    timer_start(&tm);
    tm_st = timer_read(&tm);
#endif

    int ret;
    switch (ss->hash_version) {
    case ssd_hash_version_v1:
        ret = ssd_hash_v1(ss, bb, de_tab);
        break;
    case ssd_hash_version_v2:
        ret = ssd_hash_v2(ss, bb, de_tab);
        break;
    default:
        uloga("%s(): ERROR unknown shared space hash version %u\n",
            __func__, ss->hash_version);
        ret = 0;
        break;
    }

#ifdef TIMING_SSD
    tm_end = timer_read(&tm);
    uloga("%s(): hash_version v%u time %lf seconds\n", __func__, ss->hash_version, tm_end-tm_st);
#endif
    return ret;
}

/*
  Initialize the dht structure.
*/
int ssd_init(struct sspace *ssd, int rank)
{
    ssd->rank = rank;
    ssd->ent_self = ssd->dht->ent_tab[rank];

    return 0;
}

/*
*/
int ssd_copy(struct obj_data *to_obj, struct obj_data *from_obj)
{
        struct matrix to_mat, from_mat;
        struct bbox bbcom;

        bbox_intersect(&to_obj->obj_desc.bb, &from_obj->obj_desc.bb, &bbcom);

        matrix_init(&from_mat, from_obj->obj_desc.st,
                    &from_obj->obj_desc.bb, &bbcom, 
                    from_obj->data, from_obj->obj_desc.size);

        matrix_init(&to_mat, to_obj->obj_desc.st, 
                    &to_obj->obj_desc.bb, &bbcom,
                    to_obj->data, to_obj->obj_desc.size);

        matrix_copy(&to_mat, &from_mat);
        return 0;
}

int ssd_copyv(struct obj_data *obj_dest, struct obj_data *obj_src)
{
	struct matrix mat_dest, mat_src;
	struct bbox bbcom;

	bbox_intersect(&obj_dest->obj_desc.bb, &obj_src->obj_desc.bb, &bbcom);

	matrix_init(&mat_dest, obj_dest->obj_desc.st,
			&obj_dest->obj_desc.bb, &bbcom,
			obj_dest->data, obj_dest->obj_desc.size);

	matrix_init(&mat_src, obj_src->obj_desc.st,
			&obj_src->obj_desc.bb, &bbcom,
			obj_src->data, obj_src->obj_desc.size);

	matrix_copyv(&mat_dest, &mat_src);

	return 0;
}

/*
*/
int ssd_copy_list(struct obj_data *to, struct list_head *od_list)
{
        struct obj_data *from;
        struct matrix to_mat, from_mat;
        struct bbox bbcom;

        list_for_each_entry(from, od_list, struct obj_data, obj_entry) {

                bbox_intersect(&to->obj_desc.bb, &from->obj_desc.bb, &bbcom);

                matrix_init(&from_mat, from->obj_desc.st,
                            &from->obj_desc.bb, &bbcom, 
                            from->data, from->obj_desc.size);

                matrix_init(&to_mat, to->obj_desc.st, 
                            &to->obj_desc.bb, &bbcom, 
                            to->data, to->obj_desc.size);

                matrix_copy(&to_mat, &from_mat);
        }

        return 0;
}

int ssd_filter(struct obj_data *from, struct obj_descriptor *odsc, double *dval)
{
        //TODO: search the matrix to find the min
        static int n = 1;

        *dval = 2.0 * n;
        n++;

        return 0;
}

/*
  Allocate and init the local storage structure.
*/
struct ss_storage *ls_alloc(int max_versions)
{
        struct ss_storage *ls = 0;
        int i;

        ls = malloc(sizeof(*ls) + sizeof(struct list_head) * max_versions);
        if (!ls) {
                errno = ENOMEM;
                return ls;
        }

        memset(ls, 0, sizeof(*ls));
        for (i = 0; i < max_versions; i++)
                INIT_LIST_HEAD(&ls->obj_hash[i]);
        ls->size_hash = max_versions;

        return ls;
}

void ls_free(struct ss_storage *ls)
{
    if (!ls) return;

    struct obj_data *od, *t;
    struct list_head *list;
    int i;

    for (i = 0; i < ls->size_hash; i++) {
        list = &ls->obj_hash[i];
        list_for_each_entry_safe(od, t, list, struct obj_data, obj_entry ) {
            ls_remove(ls, od);
            obj_data_free(od);
        }
    }

    if (ls->num_obj != 0) {
        uloga("%s(): ERROR ls->num_obj is %d not 0\n", __func__, ls->num_obj);
    }
    free(ls);
}

/*
  Add an object to the local storage.
*/
void ls_add_obj(struct ss_storage *ls, struct obj_data *od)
{
        int index;
        struct list_head *bin;
        struct obj_data *od_existing;

        od_existing = ls_find_no_version(ls, &od->obj_desc);
        if (od_existing) {
                od_existing->f_free = 1;
                if (od_existing->refcnt == 0) {
                        ls_remove(ls, od_existing);
                        obj_data_free(od_existing);
                }
                else {
                        uloga("'%s()': object eviction delayed.\n", __func__);
                }
        }

        index = od->obj_desc.version % ls->size_hash;
        bin = &ls->obj_hash[index];

        /* NOTE: new object comes first in the list. */
        list_add(&od->obj_entry, bin);
        ls->num_obj++;
}

struct obj_data* ls_lookup(struct ss_storage *ls, char *name)
{
        struct obj_data *od;
        struct list_head *list;
        int i;

        for (i = 0; i < ls->size_hash; i++) {
                list = &ls->obj_hash[i];

                list_for_each_entry(od, list, struct obj_data, obj_entry ) {
                        if (strcmp(od->obj_desc.name, name) == 0)
                                return od;
                }
        }

        return NULL;
}

void ls_remove(struct ss_storage *ls, struct obj_data *od)
{
        list_del(&od->obj_entry);
        ls->num_obj--;
}

void ls_try_remove_free(struct ss_storage *ls, struct obj_data *od)
{
        /* Note:  we   assume  the  object  data   is  allocated  with
           obj_data_alloc(), i.e., the data follows the structure.  */
        if (od->refcnt == 0) {
                ls_remove(ls, od);
                if (od->data != od+1) {
                        uloga("'%s()': we are about to free an object " 
                              "with external allocation.\n", __func__);
                }
                obj_data_free(od);
        }
}

/*
  Find  an object  in the  local storage  that has  the same  name and
  version with the object descriptor 'odsc'.
*/
struct obj_data *ls_find(struct ss_storage *ls, const struct obj_descriptor *odsc)
{
        struct obj_data *od;
        struct list_head *list;
        int index;

        index = odsc->version % ls->size_hash;
        list = &ls->obj_hash[index];

        list_for_each_entry(od, list, struct obj_data, obj_entry) {
                if (obj_desc_equals_intersect(odsc, &od->obj_desc))
                        return od;
        }

        return NULL;
}

/*
  Search for an object in the local storage that is mapped to the same
  bin, and that has the same  name and object descriptor, but may have
  different version.
*/
struct obj_data *
ls_find_no_version(struct ss_storage *ls, struct obj_descriptor *odsc)
{
        struct obj_data *od;
        struct list_head *list;
        int index;

        index = odsc->version % ls->size_hash; 
        list = &ls->obj_hash[index];

        list_for_each_entry(od, list, struct obj_data, obj_entry) {
                if (obj_desc_by_name_intersect(odsc, &od->obj_desc))
                        return od;
        }

        return NULL;
}

/*
  Test if the  'odsc' matches any object descriptor in  a DHT entry by
  name and coordinates, but not version, and return the matching index.
*/
static struct obj_desc_list * 
dht_find_match(const struct dht_entry *de, const struct obj_descriptor *odsc)
{
	struct obj_desc_list *odscl;
        int n;

	// TODO: delete this (just an assertion for proper behaviour).
	if (odsc->version == (unsigned int) -1) {
		uloga("'%s()': version on object descriptor is not set!!!\n", 
			__func__);
		return 0;
	}

	n = odsc->version % de->odsc_size;
	list_for_each_entry(odscl, &de->odsc_hash[n], struct obj_desc_list, odsc_entry) {
		if(obj_desc_by_name_intersect(odsc, &odscl->odsc))
			return odscl;
	}

	return 0;
}

#define array_resize(a, n) a = realloc(a, sizeof(*a) * (n))

int dht_add_entry(struct dht_entry *de, const struct obj_descriptor *odsc)
{
	struct obj_desc_list *odscl;
        int n, err = -ENOMEM;

        odscl = dht_find_match(de, odsc);
        if (odscl) {
                /* There  is allready  a descriptor  with  a different
		   version in the DHT, so I will overwrite it. */
                memcpy(&odscl->odsc, odsc, sizeof(*odsc));
                return 0;
        }

	n = odsc->version % de->odsc_size;
	odscl = malloc(sizeof(*odscl));
	if (!odscl)
		return err;
	memcpy(&odscl->odsc, odsc, sizeof(*odsc));

	list_add(&odscl->odsc_entry, &de->odsc_hash[n]);
	de->odsc_num++;

        return 0;
}

/*
  Search the dht entry 'de' for an object that intersects object
  descriptor 'odsc' and return a reference to it.
*/
const struct obj_descriptor * 
dht_find_entry(struct dht_entry *de, const struct obj_descriptor *odsc) 
// __attribute__((__unused__))
{
	/*
        int i;

        for (i = 0; i < de->size_objs; i++) {
                if (obj_desc_equals_intersect(&de->od_tab[i], odsc))
                        return &de->od_tab[i];
        }
	*/
        return NULL;
}

/*
  Object descriptor 'q_odsc' can intersect multiple object descriptors
  from dht entry 'de'; find all descriptor from 'de' and return their
  number and references .
*/
int dht_find_entry_all(struct dht_entry *de, struct obj_descriptor *q_odsc, 
                const struct obj_descriptor *odsc_tab[])
{
        int n, num_odsc = 0;
	struct obj_desc_list *odscl;

	n = q_odsc->version % de->odsc_size;
	list_for_each_entry(odscl, &de->odsc_hash[n], struct obj_desc_list, odsc_entry) {
		if (obj_desc_equals_intersect(&odscl->odsc, q_odsc))
			odsc_tab[num_odsc++] = &odscl->odsc;
	}

        return num_odsc;
}

/*
  List the available versions of a data object.
*/
int dht_find_versions(struct dht_entry *de, struct obj_descriptor *q_odsc, int odsc_vers[])
{
	struct obj_desc_list *odscl;
	int i, n = 0;

	for (i = 0; i < de->odsc_size; i++) {
		list_for_each_entry(odscl, &de->odsc_hash[i], struct obj_desc_list, odsc_entry)
			if (obj_desc_by_name_intersect(&odscl->odsc, q_odsc)) {
				odsc_vers[n++] = odscl->odsc.version;
				break;	/* Break the list_for_each_entry loop. */
			}
	}

	return n;
}

#define ALIGN_ADDR_QUAD_BYTES(a)                                \
        unsigned long _a = (unsigned long) (a);                 \
        _a = (_a + 7) & ~7;                                     \
        (a) = (void *) _a;
/*
  Allocate space for an obj_data structure and the data.
*/
struct obj_data *obj_data_alloc(struct obj_descriptor *odsc)
{
    struct obj_data *od = 0;

	od = malloc(sizeof(*od));
	if (!od)
		return NULL;
	memset(od, 0, sizeof(*od));

	od->_data = od->data = malloc(obj_data_size(odsc) + 7);
	if (!od->_data) {
		free(od);
		return NULL;
	}
	ALIGN_ADDR_QUAD_BYTES(od->data);
	od->obj_desc = *odsc;

    return od;
}

/*
  Allocate  space  for obj_data  structure  and  references for  data.
*/
struct obj_data *obj_data_allocv(struct obj_descriptor *odsc)
{
	struct obj_data *od;

	od = malloc(sizeof(*od));
	if (!od)
		return NULL;
	memset(od, 0, sizeof(*od));

	od->_data = od->data = malloc(obj_data_sizev(odsc) + 7);
	if (!od->_data) {
		free(od);
		return NULL;
	}
	ALIGN_ADDR_QUAD_BYTES(od->data);
	od->obj_desc = *odsc;

	return od;
}

/* 
  Allocate space for the obj_data struct only; space for data is
  externally allocated.
*/
struct obj_data * obj_data_alloc_no_data(struct obj_descriptor *odsc, void *data)
{
        struct obj_data *od;

        od = malloc(sizeof(*od));
        if (!od)
                return NULL;
        memset(od, 0, sizeof(*od));

        od->obj_desc = *odsc;
        od->data = data;

        return od;
}

struct obj_data *obj_data_alloc_with_data(struct obj_descriptor *odsc, void *data)
{
        struct obj_data *od = obj_data_alloc(odsc);
        if (!od)
                return NULL;

        memcpy(od->data, data, obj_data_size(odsc));
        //TODO: what about the descriptor ?

        return od;
}

void obj_data_free_with_data(struct obj_data *od)
{
	if (od->_data) {
		uloga("'%s()': explicit data free on descriptor %s.\n", 
			__func__, od->obj_desc.name);
		free(od->_data);
	}
	else    free(od->data);
        free(od);
}

void obj_data_free(struct obj_data *od)
{
	if (od->_data)
		free(od->_data);
	free(od);
}

__u64 obj_data_size(struct obj_descriptor *obj_desc)
{
    return obj_desc->size * bbox_volume(&obj_desc->bb);
}

__u64 obj_data_sizev(struct obj_descriptor *odsc)
{
	__u64 size = 1; // sizeof(iovec_t);

	if (odsc->bb.num_dims == 2) {
		if (odsc->st == row_major)
			size = size * bbox_dist(&odsc->bb, bb_y);
		else	size = size * bbox_dist(&odsc->bb, bb_x);
	}
	else if (odsc->bb.num_dims == 3) {
		size = size * bbox_dist(&odsc->bb, bb_z);
		if (odsc->st == row_major)
			size = size * bbox_dist(&odsc->bb, bb_y);
		else	size = size * bbox_dist(&odsc->bb, bb_x);
	}

	return size;
}

int obj_desc_equals_no_owner(const struct obj_descriptor *odsc1,
                 const struct obj_descriptor *odsc2)
{
        /* Note: object distribution should not change with
           version. */
        if (// odsc1->version == odsc2->version && 
            strcmp(odsc1->name, odsc2->name) == 0 &&
            bbox_equals(&odsc1->bb, &odsc2->bb))
                return 1;
        return 0;
}

int obj_desc_equals(const struct obj_descriptor *odsc1, 
                const struct obj_descriptor *odsc2)
{
        if (odsc1->owner == odsc2->owner && 
            bbox_equals(&odsc1->bb, &odsc2->bb))
                return 1;
        else    return 0;
}

/*
 *   Test if two object descriptors have the same name and versions and
 *     their bounding boxes intersect.
 *     */
int obj_desc_equals_intersect(const struct obj_descriptor *odsc1,
                const struct obj_descriptor *odsc2)
{
        if (strcmp(odsc1->name, odsc2->name) == 0 &&
            odsc1->version == odsc2->version &&
            bbox_does_intersect(&odsc1->bb, &odsc2->bb))
                return 1;
        return 0;
}

/*
 *   Test if two object descriptors have the same name and their bounding
 *     boxes intersect.
 *     */
int obj_desc_by_name_intersect(const struct obj_descriptor *odsc1,
                const struct obj_descriptor *odsc2)
{
        if (strcmp(odsc1->name, odsc2->name) == 0 &&
            bbox_does_intersect(&odsc1->bb, &odsc2->bb))
                return 1;
        return 0;
}

void copy_global_dimension(struct global_dimension *l, int ndim,
                        const uint64_t *gdim)
{
    int i;
    l->ndim = ndim;
    memset(&l->sizes, 0, sizeof(struct coord));
    for (i = 0; i < ndim; i++) {
        l->sizes.c[i] = gdim[i];
    } 
}

void init_gdim_list(struct list_head *gdim_list)
{
    if (!gdim_list) return;
    INIT_LIST_HEAD(gdim_list);
}

void free_gdim_list(struct list_head *gdim_list) {
    if (!gdim_list) return;
    int cnt = 0;
    struct gdim_list_entry *e, *t;
    list_for_each_entry_safe(e, t, gdim_list, struct gdim_list_entry, entry)
    {
        list_del(&e->entry);
        free(e->var_name);
        free(e);
        cnt++;
    }

#ifdef DEBUG
    uloga("%s(): number of user-defined global dimension is %d\n", __func__, cnt);
#endif
} 

struct gdim_list_entry* lookup_gdim_list(struct list_head *gdim_list,
        const char *var_name)
{
    if (!gdim_list) return NULL;
    struct gdim_list_entry *e;
    list_for_each_entry(e, gdim_list, struct gdim_list_entry, entry)
    {
        if (0==strcmp(e->var_name, var_name)) return e;
    }
    return NULL;
}

void update_gdim_list(struct list_head *gdim_list, const char *var_name,
        int ndim, uint64_t *gdim)
{
    struct gdim_list_entry *e = lookup_gdim_list(gdim_list, var_name);
    if (!e) {
        // add new entry
        e = (struct gdim_list_entry*)malloc(sizeof(*e));
        e->var_name = malloc(strlen(var_name)+1);
        strcpy(e->var_name, var_name);
        list_add(&e->entry, gdim_list); 
    }

    // update entry
    copy_global_dimension(&e->gdim, ndim, gdim);
}

void set_global_dimension(struct list_head *gdim_list, const char *var_name,
            const struct global_dimension *default_gdim, struct global_dimension *gdim)
{
    struct gdim_list_entry *e = lookup_gdim_list(gdim_list, var_name);
    if (e) {
        memcpy(gdim, &e->gdim, sizeof(struct global_dimension));
    } else {
        memcpy(gdim, default_gdim, sizeof(struct global_dimension));
    }
}

int global_dimension_equal(const struct global_dimension* gdim1,
    const struct global_dimension* gdim2)
{
    int i;
    for (i = 0; i < gdim1->ndim; i++) {
        if (gdim1->sizes.c[i] != gdim2->sizes.c[i])
            return 0;
    }

    return 1;
}

/*
int main(void)
{
        double *d1 = malloc(sizeof(double) * 100 * 100);
        double *d2 = malloc(sizeof(double) * 10 * 10);
        struct bbox b1 = {.num_dims = 2, .lb.c = {0, 0, 0}, .ub.c = {99, 99, 0}};
        struct bbox b2 = {.num_dims = 2, .lb.c = {3, 3, 0}, .ub.c = {9, 9, 0}};
        struct bbox b3;
        struct matrix *mat, *matl;
        int i, j;

        mat = matrix_alloc(&b1, &b2, d1);
        matl = matrix_alloc(&b2, &b2, d2);

        for (i = 0; i < 100 * 100; i++)
                d1[i] = 1.0 * (i + 1);

        matrix_copy(matl, mat);

        for (i = 0; i < matl->y_dim; i++) {
                for (j = 0; j < matl->x_dim; j++)
                        printf("%8.1f", matl->m[0][i][j]);
                printf("\n");
        }

        return 0;
}
*/
