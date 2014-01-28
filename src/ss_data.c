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
#include <errno.h>

#include "debug.h"
#include "ss_data.h"

// TODO: I should  import the header file with  the definition for the
// iovec_t data type.

/*
  A view in  the matrix allows to extract any subset  of values from a
  matrix.
*/
struct matrix_view {
        uint64_t                     lb[3];
        uint64_t                     ub[3];
};

/* Generic matrix representation. */
struct matrix {
        uint64_t                     dimx, dimy, dimz;
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

static int compute_bits(uint64_t n)
{
        int nr_bits = 0;

        while (n>1) {
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

#if 0 /* DELETE this unused part. */
/*
  Allocate and initialize a matrix "view". It allows you to easily
  access the elements of sub-matrix described by bounding box bb_loc
  from the matrix described by bb_glb. When accessing the sub-matrix,
  the indices should be reversed, i.e., A(k, j, i).
*/
static struct matrix * 
matrix_alloc(struct bbox *bb_glb, struct bbox *bb_loc, void *data, size_t se)
{
        struct matrix *mat;
        size_t size;
        int dimx, dimy, dimz;
        int shx, shy, shz;
        int i, j;

        dimx = bbox_dist(bb_glb, 0);
        dimy = bbox_dist(bb_glb, 1);
        dimz = bbox_dist(bb_glb, 2);

        size = sizeof(*mat) + sizeof(void ***) * dimz +
                sizeof(void **) * dimx * dimz;
        mat = malloc(size);
        if (!mat)
                return NULL;

        mat->data = data;
        mat->se = se;
        /* X is columns. */
        mat->dimx = bbox_dist(bb_loc, 0);
        /* Y is rows. */
        mat->dimy = bbox_dist(bb_loc, 1);
        /* Z is slices of 2d matrices of XxY. */
        mat->dimz = bbox_dist(bb_loc, 2);
        mat->m = (void ***) (mat+1);

        /*
        for (i = 0; i < mat->z_dim; i++) {
                mat->m[i] = (double **) (mat->m + mat->z_dim + i * mat->y_dim);
                for (j = 0; j < mat->y_dim; j++) 
                        mat->m[i][j] = data + 
                                (i + bb_loc->lb.c[2]) * x_dim * y_dim + 
                                (j + bb_loc->lb.c[1]) * x_dim + 
                                bb_loc->lb.c[0];
        }
        */

        /* Shift amounts from the initial matrix. */
        shx = bb_loc->lb.c[0] - bb_glb->lb.c[0];
        shy = bb_loc->lb.c[1] - bb_glb->lb.c[1];
        shz = bb_loc->lb.c[2] - bb_glb->lb.c[2];

        /* Column major matrix representation. */
        for (i = 0; i < mat->dimz; i++) {
                mat->m[i] = (void **) (mat->m + dimz + i * dimx);
                for (j = 0; j < mat->dimx; j++) 
                        mat->m[i][j] = data + 
                                /* Skip 'i' 2d slices. */
                                ((i + shz) * dimx * dimy + 
                                /* Skip 'j' 1d columns. */
                                (j + shx) * dimy + 
                                /* Select the line. */
                                shy) * se;
        }

        return mat;
}

static void matrix_copy(struct matrix *mat_to, struct matrix *mat_from)
{
        int i, j, k;
        size_t size;
        /*
        for (i = 0; i < mat_to->z_dim; i++) {
                for (j = 0; j < mat_to->y_dim; j++)
                        memcpy(mat_to->m[i][j], 
                               mat_from->m[i][j], 
                               sizeof(double) * mat_to->x_dim);
        }
        */

        size = mat_to->se * mat_to->dimy;
        for (i = 0; i < mat_to->dimz; i++) {
                for (j = 0; j < mat_to->dimx; j++)
                        memcpy(mat_to->m[i][j], 
                               mat_from->m[i][j], 
                               size);
        }
}

static void matrix_free(struct matrix *mat)
{
        free(mat);
}
#endif /* 0 */

static void matrix_init(struct matrix *mat, enum storage_type st,
                        struct bbox *bb_glb, struct bbox *bb_loc, 
                        void *pdata, size_t se)
{
        int i;

        mat->dimx = bbox_dist(bb_glb, bb_x);
        mat->dimy = bbox_dist(bb_glb, bb_y);
        mat->dimz = bbox_dist(bb_glb, bb_z);

        mat->mat_storage = st;

        for (i = bb_x; i <= bb_z; i++) {
                mat->mat_view.lb[i] = bb_loc->lb.c[i] - bb_glb->lb.c[i];
                mat->mat_view.ub[i] = bb_loc->ub.c[i] - bb_glb->lb.c[i];
        }

        mat->pdata = pdata;
        mat->size_elem = se;
}

static void matrix_copy(struct matrix *a, struct matrix *b)
{
        /*
        typedef struct {
                char size_on_mem[a->size_elem];
        } matrix_elem_generic_t;
        */

        typedef char matrix_elem_generic_t[a->size_elem];

        uint64_t ai, aj, ak, bi, bj, bk;
        uint64_t n;

        if (a->mat_storage == column_major && b->mat_storage == column_major) {

                matrix_elem_generic_t (*A)[a->dimz][a->dimx][a->dimy] = a->pdata;
                matrix_elem_generic_t (*B)[b->dimz][b->dimx][b->dimy] = b->pdata;

                /* Column major data representation (Fortran style) */
                n = a->mat_view.ub[bb_y] - a->mat_view.lb[bb_y] + 1;
                ak = a->mat_view.lb[bb_y];
                bk = b->mat_view.lb[bb_y];
                for (ai = a->mat_view.lb[bb_z], bi = b->mat_view.lb[bb_z]; 
                     ai <= a->mat_view.ub[bb_z]; ai++, bi++) {
                        for (aj = a->mat_view.lb[bb_x], bj = b->mat_view.lb[bb_x]; 
                             aj <= a->mat_view.ub[bb_x]; aj++, bj++)
                                memcpy(&(*A)[ai][aj][ak], 
                                       &(*B)[bi][bj][bk],
                                       a->size_elem * n);
                }
        }
        else if (a->mat_storage == column_major && b->mat_storage == row_major) {

                matrix_elem_generic_t (*A)[a->dimz][a->dimx][a->dimy] = a->pdata;
                matrix_elem_generic_t (*B)[b->dimz][b->dimy][b->dimx] = b->pdata;

                /* ... */
                for (ai = a->mat_view.lb[bb_z], bi = b->mat_view.lb[bb_z];
                     ai <= a->mat_view.ub[bb_z]; ai++, bi++) {
                        for (aj = a->mat_view.lb[bb_x], bj = b->mat_view.lb[bb_x];
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

                /* ... */
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
        else if (a->mat_storage == row_major && b->mat_storage == row_major) {

                matrix_elem_generic_t (*A)[a->dimz][a->dimy][a->dimx] = a->pdata;
                matrix_elem_generic_t (*B)[b->dimz][b->dimy][b->dimx] = b->pdata;

                n = a->mat_view.ub[bb_x] - a->mat_view.lb[bb_x] + 1;
                ak = a->mat_view.lb[bb_x];
                bk = b->mat_view.lb[bb_x];
                for (ai = a->mat_view.lb[bb_z], bi = b->mat_view.lb[bb_z];
                     ai <= a->mat_view.ub[bb_z]; ai++, bi++) {
                        for (aj = a->mat_view.lb[bb_y], bj = b->mat_view.lb[bb_y];
                             aj <= a->mat_view.ub[bb_y]; aj++, bj++)
                                memcpy(&(*A)[ai][aj][ak],
                                       &(*B)[bi][bj][bk],
                                       a->size_elem * n);
                }
        }
}

/* a = destination, b = source. Destination uses iovec_t format. */
static void matrix_copyv(struct matrix *a, struct matrix *b)
{
        /*
        typedef struct {
                char size_on_mem[a->size_elem];
        } matrix_elem_generic_t;
        */

        typedef char matrix_elem_generic_t[a->size_elem];

        uint64_t ai, aj, bi, bj, bk; // , ak;
        uint64_t n;

        if (a->mat_storage == column_major && b->mat_storage == column_major) {

		// matrix_elem_generic_t (*A)[a->dimz][a->dimx][a->dimy] = a->pdata;
                matrix_elem_generic_t (*B)[b->dimz][b->dimx][b->dimy] = b->pdata;
		iovec_t *A = a->pdata;

                /* Column major data representation (Fortran style) */
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
			/*
                                memcpy(&(*A)[ai][aj][ak], 
                                       &(*B)[bi][bj][bk],
                                       a->size_elem * n);
			*/
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
			/*
                                memcpy(&(*A)[ai][aj][ak],
                                       &(*B)[bi][bj][bk],
                                       a->size_elem * n);
			*/
                }
        }
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

/*
  Allocate and init the local storage structure.
*/
static struct ss_storage *ls_alloc(int max_versions)
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

static int ssd_get_max_dim(struct sspace *ss)
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
        const uint64_t sn = bbox_volume(&dht->bb_glb_domain) / dht->num_entries;
        struct intv *i_tab, intv;
        struct dht_entry *de;
        uint64_t len;
        int num_intv, i, j;
        int err = -ENOMEM;

        bbox_to_intv(&dht->bb_glb_domain, ssd->max_dim, ssd->bpd, 
                     &i_tab, &num_intv);

        //printf("%s(): Global domain decomposes into %d intervals: ", __func__, num_intv);
        //for (i = 0; i < num_intv; i++)
        //        printf("{%llu,%llu} ", i_tab[i].lb, i_tab[i].ub);
        //printf("\n");

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
                        //printf("dht_entry %d index interval {%llu,%llu} size %llu \n",
                        //        i, intv.lb, intv.ub, intv.ub-intv.lb+1);
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

/*
  Public API starts here.
*/

/*
  Allocate the shared space structure.
*/
struct sspace *ssd_alloc(struct bbox *bb_domain, int num_nodes, int max_versions)
{
        struct sspace *ssd;
        //        size_t size;
        uint64_t max_dim;
        int err = -ENOMEM;

        // size = sizeof(*ssd); //  + sizeof(struct dht_entry) * num_nodes;
        ssd = malloc(sizeof(*ssd));
        if (!ssd)
                goto err_out;
        memset(ssd, 0, sizeof(*ssd));

        ssd->storage = ls_alloc(max_versions);
        if (!ssd->storage) {
                free(ssd);
                goto err_out;
        }

        ssd->dht = dht_alloc(ssd, bb_domain, num_nodes, max_versions);
        if (!ssd->dht) {
                // TODO: free storage 
                free(ssd->storage);
                free(ssd);
                goto err_out;
        }

        max_dim = max(bb_domain->ub.c[0], bb_domain->ub.c[1]);
        if (bb_domain->num_dims > 2)
                max_dim = max(max_dim, bb_domain->ub.c[2]);

        ssd->max_dim = next_pow_2(max_dim);
        ssd->bpd = compute_bits(ssd->max_dim);

        err = dht_construct_hash(ssd->dht, ssd);
        if (err < 0) {
                //TODO: do I need a ls_free() routine to clean up the
                //      objects in the space ?
                dht_free(ssd->dht);
                free(ssd->storage);
                free(ssd);
                goto err_out;
        }

        return ssd;
 err_out:
        uloga("'%s()': failed with %d\n", __func__, err);
        return NULL;
}

void ssd_free(struct sspace *ssd)
{
        //        int i;

        dht_free(ssd->dht);

        //TODO: do I need a ls_free() routine ?
        free(ssd->storage);
        free(ssd);

        sh_free();
}

/*
  Initialize the dht structure.
*/
int ssd_init(struct sspace *ssd, int rank)
{
	//        int err = -ENOMEM;

        ssd->rank = rank;
        ssd->ent_self = ssd->dht->ent_tab[rank];

        return 0;
}

/*
  Add an object to the local storage.
*/
void ssd_add_obj(struct sspace *ss, struct obj_data *od)
{
        int index;
        struct list_head *bin;
        struct obj_data *od_existing;

        od_existing = ls_find_no_version(ss->storage, &od->obj_desc);
        if (od_existing) {
                od_existing->f_free = 1;
                if (od_existing->refcnt == 0) {
                        ssd_remove(ss, od_existing);
                        obj_data_free(od_existing);
                }
                else {
                        uloga("'%s()': object eviction delayed.\n", __func__);
                }
        }

        index = od->obj_desc.version % ss->storage->size_hash; 
        bin = &ss->storage->obj_hash[index];

        /* NOTE: new object comes first in the list. */
        list_add(&od->obj_entry, bin);
        ss->storage->num_obj++;
}

/*
*/
int ssd_copy(struct obj_data *to_obj, struct obj_data *from_obj)
{
        // struct matrix *to_mat, *from_mat;
        struct matrix to_mat, from_mat;
        struct bbox bbcom;
        // int err = -ENOMEM;

        bbox_intersect(&to_obj->obj_desc.bb, &from_obj->obj_desc.bb, &bbcom);

        matrix_init(&from_mat, from_obj->obj_desc.st,
                    &from_obj->obj_desc.bb, &bbcom, 
                    from_obj->data, from_obj->obj_desc.size);
        /*
        from_mat = matrix_alloc(&from_obj->obj_desc.bb,
                                &bbcom, from_obj->data, from_obj->obj_desc.size);
        if (!from_mat)
                goto err_out;
        */
        matrix_init(&to_mat, to_obj->obj_desc.st, 
                    &to_obj->obj_desc.bb, &bbcom,
                    to_obj->data, to_obj->obj_desc.size);
        /*
        to_mat = matrix_alloc(&to_obj->obj_desc.bb, 
                              &bbcom, to_obj->data, to_obj->obj_desc.size);
        if (!to_mat) {
                free(from_mat);
                goto err_out;
        }
        */
        matrix_copy(&to_mat, &from_mat);
        /*
        matrix_free(to_mat);
        matrix_free(from_mat);
        */
        return 0;
        /*
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
        */
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
        // struct matrix *mto, *mfrom;
        struct matrix to_mat, from_mat;
        struct bbox bbcom;
        // int err = -ENOMEM;

        list_for_each_entry(from, od_list, struct obj_data, obj_entry) {

                bbox_intersect(&to->obj_desc.bb, &from->obj_desc.bb, &bbcom);

                matrix_init(&from_mat, from->obj_desc.st,
                            &from->obj_desc.bb, &bbcom, 
                            from->data, from->obj_desc.size);
                /*
                mfrom = matrix_alloc(&from->obj_desc.bb, 
                                     &bbcom, from->data, from->obj_desc.size);
                if (!mfrom) {
                        matrix_free(mto);
                        goto err_out;
                }
                */

                matrix_init(&to_mat, to->obj_desc.st, 
                            &to->obj_desc.bb, &bbcom, 
                            to->data, to->obj_desc.size);
                /*
                mto = matrix_alloc(&to->obj_desc.bb, 
                                   &bbcom, to->data, to->obj_desc.size);
                if (!mto)
                        goto err_out;
                */
                matrix_copy(&to_mat, &from_mat);
                /*
                matrix_free(mto);
                matrix_free(mfrom);
                */
        }

        return 0;
        /*
 err_out:
        uloga("'%s()': failed with %d.\n", __func__, err);
        return err;
        */
}

int ssd_filter(struct obj_data *from, struct obj_descriptor *odsc, double *dval)
{
        //TODO: search the matrix to find the min
        static int n = 1;

        *dval = 2.0 * n;
        n++;

        return 0;
}

struct obj_data *ssd_lookup(struct sspace *ss, char *name)
{
        struct obj_data *od;
        struct list_head *list;
        int i;

        for (i = 0; i < ss->storage->size_hash; i++) {
                list = &ss->storage->obj_hash[i];

                list_for_each_entry(od, list, struct obj_data, obj_entry ) {
                        if (strcmp(od->obj_desc.name, name) == 0)
                                return od;
                }
        }

        return NULL;
}

void ssd_remove(struct sspace *ss, struct obj_data *od)
{
        list_del(&od->obj_entry);
        ss->storage->num_obj--;
}

void ssd_try_remove_free(struct sspace *ss, struct obj_data *od)
{
        /* Note:  we   assume  the  object  data   is  allocated  with
           obj_data_alloc(), i.e., the data follows the structure.  */
        if (od->refcnt == 0) {
                ssd_remove(ss, od);
                if (od->data != od+1) {
                        uloga("'%s()': we are about to free an object " 
                              "with external allocation.\n", __func__);
                }
                obj_data_free(od);
        }
}

/*
  Hash a bounding box 'bb' to the hash entries in dht; fill in the
  entries in the de_tab and return the number of entries.
*/
int ssd_hash(struct sspace *ss, const struct bbox *bb, struct dht_entry *de_tab[])
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

        for (k = 0; k < ss->dht->num_entries; k++){
                for (i = 0; i < n; i++) {
                        if (dht_intersect(ss->dht->ent_tab[k], &i_tab[i])) {
                                de_tab[num_nodes++] = ss->dht->ent_tab[k];
                                break;
                        }
                }
        }

        /* Cache the results for later use. */
	sh_add(bb, de_tab, num_nodes);

        /*
        for (i = 0, k = 0; i < n && k < NUM_NODES;) {

                if (node_intersect(&node_tab[k], &i_tab[i])) {
                        n_tab[num_nodes++] = &node_tab[k];
                        k++;
                }
                // else if (node_tab[k].i_tab[node_tab[k].num_itv-1].ub < i_tab[i].lb)
                else if (node_tab[k].i_virt.ub < i_tab[i].lb || 
                         node_tab[k].i_virt.lb > i_tab[i].ub)
                        k++;
                else
                        i++;
        }
        */
        free(i_tab);
        return num_nodes;
}

static inline void ls_inc_num_objects(struct ss_storage *ls)
{
        ls->num_obj++;
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
	/*
        od = malloc(sizeof(*od) + obj_data_size(odsc) + 7);
        if (!od)
                return NULL;
        memset(od, 0, sizeof(*od));

        od->data = od + 1;
        ALIGN_ADDR_QUAD_BYTES(od->data);
        od->obj_desc = *odsc;
	*/

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

	/*
	od = malloc(sizeof(*od) + obj_data_sizev(odsc) + 7);
	if (!od)
		return 0;
	memset(od, 0, sizeof(*od));

	od->data = od + 1;
	ALIGN_ADDR_QUAD_BYTES(od->data);
	od->obj_desc = *odsc;
	*/

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
	/*
	else 
		uloga("'%s()': implicit free for descriptor %s with external data\n",
			__func__, od->obj_desc.name);
	*/
	free(od);
}

// size_t obj_data_size(struct obj_data *od)
size_t obj_data_size(struct obj_descriptor *obj_desc)
{
        return obj_desc->size * bbox_volume(&obj_desc->bb);
}

size_t obj_data_sizev(struct obj_descriptor *odsc)
{
	size_t size = 1; // sizeof(iovec_t);
	static int PTL_MAX_IOV = 65535;

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

	/* If we exceed the IOVEC portals limit, should fall back on
	   the copy method.*/
	if (size > PTL_MAX_IOV)
		return (size_t) -1;

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
