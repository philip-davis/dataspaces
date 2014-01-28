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


#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "bbox.h"
#include "sfc.h"
#include "queue.h"

static inline uint64_t 
coord_dist(struct coord *c0, struct coord *c1, int dim)
{
        return (c1->c[dim] - c0->c[dim] + 1);
}

uint64_t bbox_dist(struct bbox *bb, int dim)
{
        return coord_dist(&bb->lb, &bb->ub, dim);
}

/*
  Split the bounding box b0 in two along dimension dim, and store the
  result in b_tab.
*/
static void bbox_divide_in2_ondim(struct bbox *b0, struct bbox *b_tab, int dim)
{
        uint64_t n;

        n = (b0->lb.c[dim] + b0->ub.c[dim]) >> 1;
        b_tab[0] = b0[0];
        b_tab[1] = b0[0];

        b_tab[0].ub.c[dim] = n;
        b_tab[1].lb.c[dim] = n+1;
}

/*
  Split a 2 dimensions bounding box b0 in 4 and store results in
  b_tab.
*/
static void bbox_divide_in4(struct bbox *b0, struct bbox *b_tab)
{
        bbox_divide_in2_ondim(b0, b_tab+2, 0);
        bbox_divide_in2_ondim(b_tab+2, b_tab, 1);
        bbox_divide_in2_ondim(b_tab+3, b_tab+2, 1);
}

/*
  Split a 3 dimensions bounding box b0 in 8 and store results in
  b_tab.
*/
static void bbox_divide_in8(struct bbox *b0, struct bbox *b_tab)
{
        bbox_divide_in4(b0, b_tab+4);
        bbox_divide_in2_ondim(b_tab+4, b_tab, 2);
        bbox_divide_in2_ondim(b_tab+5, b_tab+2, 2);
        bbox_divide_in2_ondim(b_tab+6, b_tab+4, 2);
        bbox_divide_in2_ondim(b_tab+7, b_tab+6, 2);
}

/*
  Generic routine to split a box in 4 or 8 based on the number of
  dimensions.
*/
void bbox_divide(struct bbox *b0, struct bbox *b_tab)
{
        if (b0->num_dims == 2)
                bbox_divide_in4(b0, b_tab);
        else if (b0->num_dims == 3)
                bbox_divide_in8(b0, b_tab);
}

/* 
   Test if bounding box b0 includes b1 along dimension dim. 
*/
static inline int 
bbox_include_ondim(const struct bbox *b0, const struct bbox *b1, int dim)
{
        if ((b0->lb.c[dim] <= b1->lb.c[dim]) && 
            (b0->ub.c[dim] >= b1->ub.c[dim]) )
                return 1;
        else    return 0;
}

/* 
   Test if bounding box b0 includes b1 (test on all dimensions).
*/
int bbox_include(const struct bbox *b0, const struct bbox *b1)
{
        if (bbox_include_ondim(b0, b1, 0) && 
            bbox_include_ondim(b0, b1, 1) &&
            bbox_include_ondim(b0, b1, 2))
                return 1;
        else    return 0;
}

/*
  Test if bounding boxes b0 and b1 intersect along dimension dim.
*/
static int bbox_intersect_ondim(const struct bbox *b0, const struct bbox *b1, int dim)
{
        if ((b0->lb.c[dim] <= b1->lb.c[dim] && 
             b1->lb.c[dim] <= b0->ub.c[dim]) || 
            (b1->lb.c[dim] <= b0->lb.c[dim] &&
             b0->lb.c[dim] <= b1->ub.c[dim]))
                return 1;
        else    return 0;
}

/*
  Test if bounding boxes b0 and b1 intersect (on all dimensions).
*/
int bbox_does_intersect(const struct bbox *b0, const struct bbox *b1)
{
        if (bbox_intersect_ondim(b0, b1, 0) && 
            bbox_intersect_ondim(b0, b1, 1) && 
            bbox_intersect_ondim(b0, b1, 2))
                return 1;
        else    return 0;
}

/*
  Compute the intersection of bounding boxes b0 and b1, and store it on
  b2. Implicit assumption: b0 and b1 intersect.
*/
void bbox_intersect(struct bbox *b0, const struct bbox *b1, struct bbox *b2)
{
        int i;

        b2->num_dims = b0->num_dims;
        for (i = 0; i < 3; i++) {
                b2->lb.c[i] = max(b0->lb.c[i], b1->lb.c[i]);
                b2->ub.c[i] = min(b0->ub.c[i], b1->ub.c[i]);
        }
}

/*
  Test if two bounding boxes are equal.
*/
int bbox_equals(const struct bbox *bb0, const struct bbox *bb1)
{
        if (bb0->num_dims == bb1->num_dims && 
            bb0->lb.c[0] == bb1->lb.c[0] &&
            bb0->lb.c[1] == bb1->lb.c[1] &&
            bb0->ub.c[0] == bb1->ub.c[0] &&
            bb0->ub.c[1] == bb1->ub.c[1]) {
                if (bb0->num_dims > 2) {
                         if (bb0->lb.c[2] == bb1->lb.c[2] &&
                             bb0->ub.c[2] == bb1->ub.c[2])
                                return 1;
                         else   return 0;
                }
                else    return 1;
        }
        else    return 0;
}

uint64_t bbox_volume(struct bbox *bb)
{
        uint64_t n = 1;

        switch (bb->num_dims) {
        case 3:
                n = n * coord_dist(&bb->lb, &bb->ub, 2);
        case 2:
                n = n * coord_dist(&bb->lb, &bb->ub, 1);
        case 1:
                n = n * coord_dist(&bb->lb, &bb->ub, 0);
        }
        return n;
}

static int compute_bits(uint64_t n)
{
        int nr_bits = 0;

        while (n) {
                n = n >> 1;
                nr_bits++;
        }

        return nr_bits;
}

/*
  Tranlate a bounding bb box into a 1D inteval using a SFC.
  Assumption: bb has the same size on all dimensions and the size is a
  power of 2.
*/
static void bbox_flat(struct bbox *bb, struct intv *itv, int bpd)
{
        bitmask_t sfc_coord[3];
        uint64_t ival[2], jval[2], kval[2];
        int i, j, k;
        uint64_t index;

        ival[0] = bb->lb.c[0];
        ival[1] = bb->ub.c[0];
        jval[0] = bb->lb.c[1];
        jval[1] = bb->ub.c[1];

        itv->lb = ~(0UL);
        itv->ub = 0;

        if (bb->num_dims == 2) {
                for (i = 0; i < 2; i++) {
                        sfc_coord[0] = ival[i];
                        for (j = 0; j < 2; j++) {
                                sfc_coord[1] = jval[j];
                                index = hilbert_c2i(2, bpd, sfc_coord);
                                if (index < itv->lb)
                                        itv->lb = index;
                                else if (index > itv->ub)
                                        itv->ub = index;
                        }
                }
        }
        else if (bb->num_dims == 3) {
                kval[0] = bb->lb.c[2];
                kval[1] = bb->ub.c[2];
                for (i = 0; i < 2; i++) {
                        sfc_coord[0] = ival[i];
                        for (j = 0; j < 2; j++) {
                                sfc_coord[1] = jval[j];
                                for (k = 0; k < 2; k++) {
                                        sfc_coord[2] = kval[k];
                                        index = hilbert_c2i(3, bpd, sfc_coord);
                                        if (index < itv->lb)
                                                itv->lb = index;
                                        else if (index > itv->ub)
                                                itv->ub = index;
                                }
                        }
                }
        }
}

static int intv_compar(const void *a, const void *b)
{
        const struct intv *i0 = a, *i1 = b;

        //        return (int) (i0->lb - i1->lb);
        if (i0->lb < i1->lb)
                return -1;
        else if (i0->lb > i1->lb)
                return 1;
        else    return 0;
}

static uint64_t intv_compact(struct intv *i_tab, uint64_t num_itv)
{
        uint64_t i, j;

        for (i = 0, j = 1; j < num_itv; j++) {
                if ((i_tab[i].ub + 1) == i_tab[j].lb)
                        i_tab[i].ub = i_tab[j].ub;
                else  {
                        i = i + 1;
                        i_tab[i] = i_tab[j];
                }
        }

        return (i+1);
}

/*
  Find the equivalence in 1d index space using a SFC for a bounding
  box bb.
*/
void bbox_to_intv(const struct bbox *bb, uint64_t dim_virt, int bpd, 
                  struct intv **intv, int *num_intv)
{
        struct bbox *bb_virt;
        struct bbox b_tab[8];
        struct queue q_can, q_good;
        struct intv *i_tab, *i_tmp;
        uint64_t n;
        uint64_t i;

        n = dim_virt;

        bpd = compute_bits(n);
        bb_virt = malloc(sizeof(struct bbox));
        memset(bb_virt, 0, sizeof(struct bbox));
        bb_virt->num_dims = bb->num_dims;
        bb_virt->ub.c[0] = bb_virt->ub.c[1] = n - 1;
        if (bb->num_dims > 2)
                bb_virt->ub.c[2] = n - 1;

        queue_init(&q_can);
        queue_init(&q_good);
        n = 4;
        if (bb->num_dims == 3)
                n = 8;
        memset(&b_tab, 0, sizeof(b_tab));

        queue_enqueue(&q_can, bb_virt);
        while (! queue_is_empty(&q_can)) {
                bb_virt = queue_dequeue(&q_can);

                if (bbox_include(bb, bb_virt)) {
                        /* 
                         * Bounding box of proper size, can transform
                         * it to 1d index.
                         */
                        i_tmp = malloc(sizeof(struct intv));
                        bbox_flat(bb_virt, i_tmp, bpd);
                        queue_enqueue(&q_good, i_tmp);
                        // bbox_print(bb_virt);
                        // printf(" sfc {%u, %u}\n", i_tmp->lb, i_tmp->ub);
                        free(bb_virt);
                }
                else if (bbox_does_intersect(bb, bb_virt)) {
                        bbox_divide(bb_virt, b_tab);
                        free(bb_virt);

                        for (i = 0; i < n; i++) {
                                bb_virt = malloc(sizeof(struct bbox));
                                *bb_virt = b_tab[i];
                                queue_enqueue(&q_can, bb_virt);
                        }
                }
                else {
                        free(bb_virt);
                }
        }

        n = queue_size(&q_good);
        i_tab = malloc(n * sizeof(struct intv));

#ifdef DEBUG
        printf("total # boxes to decompose is %d.\n", n);
#endif
        n = 0;
        while (! queue_is_empty(&q_good)) {
                i_tmp = queue_dequeue(&q_good);
                i_tab[n++] = *i_tmp;
                free(i_tmp);
        }

        qsort(i_tab, n, sizeof(struct intv), &intv_compar);
        n = intv_compact(i_tab, n);

        /* Reduce the index array size to the used elements only. */
        i_tab = realloc(i_tab, n * sizeof(struct intv));
        *intv = i_tab;
        *num_intv = n;
}

/*
  New test ...
*/
void bbox_to_intv2(const struct bbox *bb, uint64_t dim_virt, int bpd, 
                  struct intv **intv, int *num_intv)
{
        struct bbox *bb_tab, *pbb;
        int64_t bb_size, bb_head, bb_tail;
        struct intv *i_tab;
        int64_t i_num, i_size, i_resize = 0;
        uint64_t n; // , i;

        n = dim_virt;
        bpd = compute_bits(n);

        bb_size = 4000;
        bb_tab = malloc(sizeof(*bb_tab) * bb_size);
        pbb = &bb_tab[bb_size-1];
        bb_head = bb_size-1; 
        bb_tail = 0;

        pbb->num_dims = bb->num_dims;
        pbb->lb.c[0] = pbb->lb.c[1] = pbb->lb.c[2] = 0;
        pbb->ub.c[0] = pbb->ub.c[1] = n - 1;
        if (bb->num_dims > 2) 
                pbb->ub.c[2] = n - 1;
        else
                pbb->ub.c[2] = 0;

        i_size = 4000;
        i_num = 0;
        i_tab = malloc(sizeof(*i_tab) * i_size);

        n = 4;
        if (bb->num_dims == 3)
                n = 8;

        while (bb_head != bb_tail) {
            pbb = &bb_tab[bb_head];

            if (bbox_include(bb, pbb)) {
                /* 
                 * Bounding box of proper size, can transform
                 * it to 1d index.
                 */
                if (i_num == i_size) {
                    i_size = i_size + i_size/2;
                    i_tab = realloc(i_tab, sizeof(*i_tab) * i_size);
                    i_resize++;
                }
                bbox_flat(pbb, &i_tab[i_num], bpd);
                i_num++;
            }
            else if (bbox_does_intersect(bb, pbb)) {
                if ((bb_tail+n)%bb_size == (bb_head - (bb_head%n))) {
                    int64_t bb_nsize = (bb_size + bb_size/2) & (~0x07);
                    struct bbox *bb_ntab;
                    int64_t bb_nhead = bb_head - (bb_head % n);

                    bb_ntab = malloc(sizeof(*bb_ntab) * bb_nsize);
                    if (bb_tail > bb_head) {
                        memcpy(bb_ntab, &bb_tab[bb_nhead], 
                           sizeof(*bb_ntab)*(bb_tail-bb_nhead));
                    }
                    else {
                        memcpy(bb_ntab, &bb_tab[bb_nhead], 
                               sizeof(*bb_ntab)*(bb_size-bb_nhead));
                        memcpy(&bb_ntab[bb_size-bb_nhead], bb_tab, 
                               sizeof(*bb_ntab)*bb_tail);

                    }
                    bb_head = bb_head % n;
                    bb_tail = bb_size - n;
                    bb_size = bb_nsize;

                    free(bb_tab);
                    bb_tab = bb_ntab;
                    pbb = &bb_tab[bb_head];
                }

                bbox_divide(pbb, &bb_tab[bb_tail]);
                bb_tail = (bb_tail + n) % bb_size;
            }

            bb_head = (bb_head + 1) % bb_size;
        }
        free(bb_tab);

        // printf("total # boxes to decompose is %d.\n", i_num);
        // printf("I had to resize the interval array %d times.\n", i_resize);

        qsort(i_tab, i_num, sizeof(*i_tab), &intv_compar);
        n = intv_compact(i_tab, i_num);

        // printf("Compact size is: %d.\n", n);

        /* Reduce the index array size to the used elements only. */
        i_tab = realloc(i_tab, sizeof(*i_tab) * n);
        *intv = i_tab;
        *num_intv = n;
}


/*
  Translates a bounding box coordinates from global space described by
  bb_glb to local space. The bounding box for a local space should
  always start at coordinates (0,0,0).
*/
void bbox_to_origin(struct bbox *bb, const struct bbox *bb_glb)
{
        int i;

        if (bb->num_dims != bb_glb->num_dims)
                printf("'%s()': number of dimensions different, I set "
                      "them here, please check.", __func__);

        bb->num_dims = bb_glb->num_dims;
        for (i = 0; i < bb->num_dims; i++) {
                // if (bb->lb.c[i] > bb_glb->ub.c[i]) {
                bb->lb.c[i] -= bb_glb->lb.c[i];
                bb->ub.c[i] -= bb_glb->lb.c[i];
                // }
        }
}

/*
  Test if 1-d interval i0 intersects i1.
*/
int intv_do_intersect(struct intv *i0, struct intv *i1)
{
        if ((i0->lb <= i1->lb && i1->lb <= i0->ub) || 
            (i1->lb <= i0->lb && i0->lb <= i1->ub))
                return 1;
        else    return 0;
}

uint64_t intv_size(struct intv *intv)
{
        return intv->ub - intv->lb + 1;
}
