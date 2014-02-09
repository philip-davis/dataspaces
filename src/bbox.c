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
#include "debug.h"

//static inline unsigned int 
static inline __u64 
coord_dist(struct coord *c0, struct coord *c1, int dim)
{
        return (c1->c[dim] - c0->c[dim] + 1);
}

//int bbox_dist(struct bbox *bb, int dim)
__u64 bbox_dist(struct bbox *bb, int dim)
{
        return coord_dist(&bb->lb, &bb->ub, dim);
}

/*
  Generic routine to split a box in 4 or 8 based on the number of
  dimensions.
*/
void bbox_divide(struct bbox *b0, struct bbox *b_tab)
{
    int ndims = b0->num_dims;
    int num_subbox = 1 << ndims; //number of sub n-dimensions bbox
    int i, j, n = 0;

    for(i = 0; i < num_subbox; i++){
        j = 0;
        b_tab[i].num_dims = b0->num_dims;
        while(j < ndims){
            n = (b0->lb.c[j] + b0->ub.c[j]) / 2; //the middle point of bounding
            if((i & (1 << j))==0){
                b_tab[i].lb.c[j] = b0->lb.c[j];
                b_tab[i].ub.c[j] = n;
            }
            else{
                b_tab[i].lb.c[j] = n + 1;
                b_tab[i].ub.c[j] = b0->ub.c[j];
            }
            j++;
        }
    }
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
    int i;

    /**FOR TEST**/
    /**printf("b1 is [%d,%d]\n", b1->lb.c[0],b1->lb.c[1]);
    printf("b1 is [%d,%d]\n", b1->ub.c[0],b1->ub.c[1]);
    printf("b0 is [%d,%d]\n", b0->lb.c[0],b0->lb.c[1]);
    printf("b0 is [%d,%d]\n\n", b0->ub.c[0],b0->ub.c[1]);**/
    //printf("b0 is dims[%d], b1 is dims[%d]\n", b0->num_dims, b1->num_dims);

    //if(b0->num_dims == b1->num_dims){
        for(i = 0; i < b0->num_dims; i++){
            if(!bbox_include_ondim(b0, b1, i))
                return 0;
        }
        return 1;
    //}
    //return 0;
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
    int i;
    //printf("b1 dims=%d, b2 dims=%d\n", b0->num_dims, b1->num_dims);
    //if(b0->num_dims == b1->num_dims){
        for(i = 0; i < b0->num_dims; i++){
            if(!bbox_intersect_ondim(b0, b1, i))
                return 0;
        }
        return 1;
    //}
    //return 0;
}

/*
  Compute the intersection of bounding boxes b0 and b1, and store it on
  b2. Implicit assumption: b0 and b1 intersect.
*/
void bbox_intersect(struct bbox *b0, const struct bbox *b1, struct bbox *b2)
{
        int i;

        b2->num_dims = b0->num_dims;
        for (i = 0; i < BBOX_MAX_NDIM; i++) {	//TODO 3 to 10
                b2->lb.c[i] = max(b0->lb.c[i], b1->lb.c[i]);
                b2->ub.c[i] = min(b0->ub.c[i], b1->ub.c[i]);
        }
}

/*
  Test if two bounding boxes are equal.
*/
int bbox_equals(const struct bbox *bb0, const struct bbox *bb1)
{
    int i;
    if(bb0->num_dims == bb1->num_dims){
        for(i = 0; i < bb0->num_dims; i++){
            if((bb0->lb.c[i] != bb1->lb.c[i]) ||
               (bb0->ub.c[i] != bb1->ub.c[i]))
                return 0;
        }
        return 1;
    }
    return 0;
}

//unsigned long bbox_volume(struct bbox *bb)
__u64 bbox_volume(struct bbox *bb)
{
    //unsigned long n = 1;
    __u64 n = 1;
    int ndims = bb->num_dims;
    int i;

    for(i = 0; i < ndims; i++){
        n = n * coord_dist(&bb->lb, &bb->ub, i);
    }
    return n;
}

//static int compute_bits(int n)
static int compute_bits(__u64 n)
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
    int dims = bb->num_dims;
    bitmask_t* sfc_coord;
    int i, j, k;
    //int index;
    __u64 index;

    //sfc_coord = malloc(sizeof(bitmask_t)*dims);
    /*
    bb->lb.c[0], c[1], c[2]...c[dims] 
    bb->ub.c[0], c[1], c[2]...c[dims]
    */
    //itv->lb = ~(0UL);   //TODO for 64 bits
    itv->lb = ~(0ULL);   //TODO for 64 bits
    itv->ub = 0;

    //initialize sfc_coord: all the possible 2-based number with dims bits
    sfc_coord = malloc(sizeof(bitmask_t) * dims); //TODO free resource
    for(i = 0; i < (1<<dims); i++){
        j = 0;
        //memcpy(sfc_coord, bb->lb.c, dims);
        for(k = 0; k < dims; k++){
            sfc_coord[k] = bb->lb.c[k];
        }
        while(j < dims){
            if(i & (1<<j))
                sfc_coord[j] = bb->ub.c[j];
            j++;
        }
        //printf("coord is [%d,%d]\n\n", (int)sfc_coord[0],(int)sfc_coord[1]);
        index = hilbert_c2i(dims, bpd, sfc_coord);
        if (index < itv->lb)
            itv->lb = index;
        else if (index > itv->ub)
            itv->ub = index;
    }
    free(sfc_coord);
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

static int intv_compact(struct intv *i_tab, int num_itv)
{
        int i, j;

        for (i = 0, j = 1; j < num_itv; j++) {
                if ((i_tab[i].ub + 1) == i_tab[j].lb)
                        i_tab[i].ub = i_tab[j].ub;
                else  {
                        i = i + 1;
                        i_tab[i] = i_tab[j];
                }
        }

	printf("in intv_compact, num_itv=%d, i=%d\n", num_itv, i);
        return (i+1);
}

/*
  Find the equivalence in 1d index space using a SFC for a bounding
  box bb.
*/
//void bbox_to_intv(const struct bbox *bb, int dim_virt, int bpd, 
void bbox_to_intv(const struct bbox *bb, __u64 dim_virt, int bpd, 
                  struct intv **intv, int *num_intv)
{
    //const int 1<<(bb->num_dims);
    struct bbox *bb_virt;
    struct bbox *b_tab; //the number of b_tab is 2^n
    struct queue q_can, q_good;
    struct intv *i_tab, *i_tmp;
    //int max;
    __u64 max;
    int i, n;

    //n = dim_virt; //n is the next power of 2 that includes the user's bbox
    //bpd = compute_bits(n); //TODO
    max = dim_virt; //n is the next power of 2 that includes the user's bbox
    bpd = compute_bits(max); //TODO

    bb_virt = malloc(sizeof(struct bbox));
    memset(bb_virt, 0, sizeof(struct bbox));
    bb_virt->num_dims = bb->num_dims;

    printf("bbox_to_intv ndims=%d\n", bb->num_dims);

    for(i = 0; i < bb->num_dims; i++){
        //bb_virt->ub.c[i] = n - 1;
        bb_virt->ub.c[i] = max - 1;
    }

    queue_init(&q_can);
    queue_init(&q_good);

    n = 1 << (bb->num_dims); //number of b_tab
    b_tab = malloc(sizeof(struct bbox) * n);
    memset(b_tab, 0, sizeof(struct bbox) * n); //TODO sizeof???

    queue_enqueue(&q_can, bb_virt);
    while(! queue_is_empty(&q_can)){

        bb_virt = (struct bbox *)queue_dequeue(&q_can);
        if(bbox_include(bb, bb_virt)){
            //printf("include\n");
            i_tmp = malloc(sizeof(struct intv));
            bbox_flat(bb_virt, i_tmp, bpd);
            queue_enqueue(&q_good, i_tmp);
            free(bb_virt);
        }
        else if(bbox_does_intersect(bb, bb_virt)){
            //printf("intersect\n");
            bbox_divide(bb_virt, b_tab);
            free(bb_virt);

            for(i = 0; i < n; i++){
                bb_virt = malloc(sizeof(struct bbox));
                *bb_virt = b_tab[i];
                queue_enqueue(&q_can, bb_virt);

                //printf("b_tab+%d is [%d,%d]\n", i, bb_virt->lb.c[0],bb_virt->lb.c[1]);
                //printf("b_tab+%d is [%d,%d]\n", i, bb_virt->ub.c[0],bb_virt->ub.c[1]);
                //printf("bb_virt is dims[%d]\n", bb_virt->num_dims);
            }
        }
        else
            //printf("finish\n");
            free(bb_virt);
    }
    free(b_tab);
    n = queue_size(&q_good);
    i_tab = malloc(n * sizeof(struct intv));

    //uloga("total # boxes to decompose is %d\n", n);
    n = 0;
    while(!queue_is_empty(&q_good)){
        i_tmp = queue_dequeue(&q_good);
        i_tab[n++] = *i_tmp;
        free(i_tmp);
    }
    qsort(i_tab, n, sizeof(struct intv), &intv_compar);
    n = intv_compact(i_tab, n);

    i_tab = realloc(i_tab, n*sizeof(struct intv));
    *intv = i_tab;
    *num_intv = n;
}


/*
  New test ...
*/
//void bbox_to_intv2(const struct bbox *bb, int dim_virt, int bpd, 
void bbox_to_intv2(const struct bbox *bb, __u64 dim_virt, int bpd, 
                  struct intv **intv, int *num_intv)
{
    struct bbox *bb_tab, *pbb;
    int bb_size, bb_head, bb_tail;
    struct intv *i_tab;
    __u64 n, i;
    int i_num, i_size, i_resize = 0;
    //int n, i;

//printf("dim_virt=%llu,bpd=%d,lb[%d,%d,%d,%d],ub[%d,%d,%d,%d]\n", dim_virt,bpd,bb->lb.c[0],bb->lb.c[1],bb->lb.c[2],bb->lb.c[3],
//					bb->ub.c[0],bb->ub.c[1],bb->ub.c[2],bb->ub.c[3]);

    n = dim_virt;
    bpd = compute_bits(n);

   // bb_size = 4000; //TODO
    bb_size = 4096;
    bb_tab = malloc(sizeof(*bb_tab) * bb_size);
    pbb = &bb_tab[bb_size - 1];
    bb_head = bb_size - 1;
    bb_tail = 0;

    pbb->num_dims = bb->num_dims;
    for(i = 0; i < pbb->num_dims; i++){
        pbb->lb.c[i] = 0;
        pbb->ub.c[i] = n - 1;
    }

    //i_size = 4000;
    i_size = 4096;
    i_num = 0;
    i_tab = malloc(sizeof(*i_tab) * i_size);

    n = 1 << (bb->num_dims);
    while(bb_head != bb_tail){
        pbb = &bb_tab[bb_head];
        if(bbox_include(bb, pbb)){
            if(i_num == i_size){
                i_size = i_size + i_size/2;
                i_tab = realloc(i_tab, sizeof(*i_tab)*i_size);
                i_resize++;
            }
            bbox_flat(pbb, &i_tab[i_num], bpd);
            i_num++;
        }
        else if(bbox_does_intersect(bb, pbb)){
                        if((bb_tail+n)%bb_size == (bb_head - (bb_head%n))){
                               // int bb_nsize = (bb_size + bb_size/2) & (~0x07); /**1 byte is 8 bits**/
				int tmp_size = bb_size + bb_size/2;
				int bb_nsize = tmp_size - tmp_size % (1 << bb->num_dims);
                                struct bbox *bb_ntab;
                                int bb_nhead = bb_head - (bb_head % n);

                                bb_ntab = malloc(sizeof(*bb_ntab) * bb_nsize);
                                if (bb_tail > bb_head) {
                                        memcpy(bb_ntab, &bb_tab[bb_nhead], sizeof(*bb_ntab)*(bb_tail-bb_nhead));
                                }
                                else {
                                        memcpy(bb_ntab, &bb_tab[bb_nhead], sizeof(*bb_ntab)*(bb_size-bb_nhead));
                                        memcpy(&bb_ntab[bb_size-bb_nhead], bb_tab, sizeof(*bb_ntab)*bb_tail);
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
	printf("============bb_size = %d, total_size=%d===========\n", bb_size, bb_size*sizeof(*bb_tab));
        free(bb_tab);
        // printf("I had to resize the interval array %d times.\n", i_resize);


    qsort(i_tab, i_num, sizeof(*i_tab), &intv_compar);
    n = intv_compact(i_tab, i_num);
   //printf("after compact, n = %llu, i_tab[300]=[%llu, %llu]", n, i_tab[300].lb, i_tab[300].ub);
        // printf("Compact size is: %d.\n", n);

    /** Reduce the index array size to the used elements only. **/
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

//unsigned long intv_size(struct intv *intv)
__u64 intv_size(struct intv *intv)
{
        return intv->ub - intv->lb + 1;
}
