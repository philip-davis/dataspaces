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
#include <sys/time.h>
#include "slom.h"
/* Generic matrix representation. */
struct slom_matrix {
	uint64_t	*dist;
	int			num_dims;
	uint64_t	*lbc;
	uint64_t	*ubc;
	double      *pdata;
	double		max_v;
	double		min_v;
};

/*
//>>>>>>>>> decomment above for debugging >>>>>>>>>>
static inline uint64_t coord_dist(struct coord *c0, struct coord *c1, int dim)
{
	return (c1->c[dim] - c0->c[dim] + 1);
}

static uint64_t bbox_dist(struct bbox *bb, int dim)
{
	return coord_dist(&bb->lb, &bb->ub, dim);
}

static uint64_t bbox_volume(struct bbox *bb)
{
	uint64_t n = 1;
	int ndims = bb->num_dims;
	int i;

	for (i = 0; i < ndims; i++){
		n = n * coord_dist(&bb->lb, &bb->ub, i);
	}
	return n;
}
//<<<<<<<<< decomment above for debugging <<<<<<<<<<
*/
static struct slom_matrix *init_slom_matrix(struct bbox *bb, double *pdata)
{
	int i, ndims = bb->num_dims;
	struct slom_matrix *mat = (struct slom_matrix *)malloc(sizeof(struct slom_matrix));
	memset(mat, 0, sizeof(struct slom_matrix));

	uint64_t *dist = (uint64_t *)malloc(sizeof(uint64_t)*BBOX_MAX_NDIM);
	memset(dist, 0, sizeof(uint64_t)*BBOX_MAX_NDIM);

	uint64_t *lbc = (uint64_t *)malloc(sizeof(uint64_t)*BBOX_MAX_NDIM);
	memset(lbc, 0, sizeof(uint64_t)*BBOX_MAX_NDIM);
	
	uint64_t *ubc = (uint64_t *)malloc(sizeof(uint64_t)*BBOX_MAX_NDIM);
	memset(ubc, 0, sizeof(uint64_t)*BBOX_MAX_NDIM);

	mat->dist = dist;
	mat->lbc = lbc;
	mat->ubc = ubc;

	for (i = 0; i < ndims; i++){
		mat->dist[i] = bbox_dist(bb, i);
		mat->lbc[i] = 0;//important
		mat->ubc[i] = bb->ub.c[i] - bb->lb.c[i];
	}
	mat->num_dims = ndims;
	mat->pdata = pdata;
	return mat;
}

static inline void normalize_attri(uint64_t index, struct data_point *point, struct slom_matrix *mat)//duan
{
	if (mat->max_v == mat->min_v){
		point->attri = (mat->pdata[index] - mat->min_v); 
	}else{ 
		point->attri = (mat->pdata[index] - mat->min_v) / (mat->max_v - mat->min_v); 
	}
}

static void print_data_point(const struct data_point *point, const int num_dims)//duan
{
	int i;
	printf("( ");
	for (i = 0; i < num_dims; i++){
		if (i != num_dims - 1) {
			printf("%llu, ", point->c[i]);
		}
		else{
			printf("%llu", point->c[i]);
		}
	}
	printf(" ) attri = %6.4lf, ", point->attri);
	printf("slom_value = %6.4lf \n", point->slom_value);
}

/*
Test if bounding box b0 includes point p (test on all dimensions).
*/
static inline int include_check(const struct data_point *p, struct slom_matrix *mat)
{
	int dim;

	for (dim = 0; dim < mat->num_dims; dim++){
		if ((mat->lbc[dim] > p->c[dim]) || (mat->ubc[dim] < p->c[dim]))
			return 0;
	}
	return 1;
}

static inline uint64_t get_index(struct data_point *point, struct slom_matrix *mat)//duan
{
	uint64_t loc = 0, loc1 = 0, loc2 = 0, loc3 = 0, loc4 = 0, loc5 = 0, loc6 = 0, loc7 = 0, loc8 = 0, loc9 = 0;
	switch (mat->num_dims){
	case(1) :
		goto dim1;
		break;
	case(2) :
		goto dim2;
		break;
	case(3) :
		goto dim3;
		break;
	case(4) :
		goto dim4;
		break;
	case(5) :
		goto dim5;
		break;
	case(6) :
		goto dim6;
		break;
	case(7) :
		goto dim7;
		break;
	case(8) :
		goto dim8;
		break;
	case(9) :
		goto dim9;
		break;
	case(10) :
		goto dim10;
		break;
	default:
		break;
	}

dim10:      loc9 = point->c[9] * mat->dist[8];

dim9:       loc8 = (loc9 + point->c[8]) * mat->dist[7];

dim8:       loc7 = (loc8 + point->c[7]) * mat->dist[6];

dim7:       loc6 = (loc7 + point->c[6]) * mat->dist[5];

dim6:       loc5 = (loc6 + point->c[5]) * mat->dist[4];

dim5:       loc4 = (loc5 + point->c[4]) * mat->dist[3];

dim4:       loc3 = (loc4 + point->c[3]) * mat->dist[2];

dim3:       loc2 = (loc3 + point->c[2]) * mat->dist[1];

dim2:       loc1 = (loc2 + point->c[1]) * mat->dist[0];

dim1:       loc = loc1 + point->c[0];
			//printf(" loc = %llu \n", loc);
			return loc;
}

static inline void init_coord(uint64_t index, struct data_point *point, struct slom_matrix *mat)//duan
{
	uint64_t loc = index;
	int i = 0;
	uint64_t *coord = (uint64_t *)malloc(sizeof(uint64_t) * BBOX_MAX_NDIM);
	memset(coord, 0, sizeof(uint64_t) * BBOX_MAX_NDIM);
	point->c = coord;

	switch (mat->num_dims){
	case(1) :
		goto dim1;
		break;
	case(2) :
		goto dim2;
		break;
	case(3) :
		goto dim3;
		break;
	case(4) :
		goto dim4;
		break;
	case(5) :
		goto dim5;
		break;
	case(6) :
		goto dim6;
		break;
	case(7) :
		goto dim7;
		break;
	case(8) :
		goto dim8;
		break;
	case(9) :
		goto dim9;
		break;
	case(10) :
		goto dim10;
		break;
	default:
		break;
	}

dim10:      point->c[i] = loc % mat->dist[i];
			loc = loc / mat->dist[i];
			i++;
dim9:       point->c[i] = loc % mat->dist[i];
			loc = loc / mat->dist[i];
			i++;
dim8:		point->c[i] = loc % mat->dist[i];
			loc = loc / mat->dist[i];
			i++;
dim7:		point->c[i] = loc % mat->dist[i];
			loc = loc / mat->dist[i];
			i++;
dim6:       point->c[i] = loc % mat->dist[i];
			loc = loc / mat->dist[i];
			i++;
dim5:       point->c[i] = loc % mat->dist[i];
			loc = loc / mat->dist[i];
			i++;
dim4:       point->c[i] = loc % mat->dist[i];
			loc = loc / mat->dist[i];
			i++;
dim3:       point->c[i] = loc % mat->dist[i];
			loc = loc / mat->dist[i];
			i++;
dim2:       point->c[i] = loc % mat->dist[i];
			loc = loc / mat->dist[i];
			i++;
dim1:       point->c[i] = loc;
}

static inline int init_neighbor(struct data_point *table, struct data_point *point, struct slom_matrix *mat, uint64_t num_elem)//duan
{
	int d0, d1, d2, d3, d4, d5, d6, d7, d8, d9;
	int i, j = 0, flag = 0, init_dist, neib_dist = 2;
	init_dist = (0 - neib_dist) / 2;
	//print_data_point(point, mat->num_dims);

	struct data_point *neib_point = (struct data_point *)malloc(sizeof(struct data_point));
	memset(neib_point, 0, sizeof(struct data_point));
	uint64_t *coord = (uint64_t *)malloc(sizeof(uint64_t) * BBOX_MAX_NDIM);
	memset(coord, 0, sizeof(uint64_t) * BBOX_MAX_NDIM);
	neib_point->c = coord;

	uint64_t *dist = (uint64_t *)malloc(sizeof(uint64_t) * BBOX_MAX_NDIM);
	memset(dist, 0, sizeof(uint64_t) * BBOX_MAX_NDIM);

	struct data_point **list = (struct data_point **)malloc(sizeof(struct data_point *) * NUM_MAX_NEIB);
	memset(list, 0, sizeof(struct data_point *) * NUM_MAX_NEIB);
	point->neib_list = list;

	switch (mat->num_dims){
	case(1) :
		goto dim1;
		break;
	case(2) :
		goto dim2;
		break;
	case(3) :
		goto dim3;
		break;
	case(4) :
		goto dim4;
		break;
	case(5) :
		goto dim5;
		break;
	case(6) :
		goto dim6;
		break;
	case(7) :
		goto dim7;
		break;
	case(8) :
		goto dim8;
		break;
	case(9) :
		goto dim9;
		break;
	case(10) :
		goto dim10;
		break;
	default:
		break;
	}

dim10:       for (d9 = init_dist; d9 < neib_dist; d9++){
				dist[9] = d9;
dim9:        for (d8 = init_dist; d8 < neib_dist; d8++){
				dist[8] = d8;
dim8:        for (d7 = init_dist; d7 < neib_dist; d7++){
				dist[7] = d7;
dim7:        for (d6 = init_dist; d6 < neib_dist; d6++){
				dist[6] = d6;
dim6:        for (d5 = init_dist; d5 < neib_dist; d5++){
				dist[5] = d5;
dim5:        for (d4 = init_dist; d4 < neib_dist; d4++){
				dist[4] = d4;
dim4:        for (d3 = init_dist; d3 < neib_dist; d3++){
				dist[3] = d3;
dim3:        for (d2 = init_dist; d2 < neib_dist; d2++){
				dist[2] = d2;
dim2:        for (d1 = init_dist; d1 < neib_dist; d1++){
				dist[1] = d1;
dim1:        for (d0 = init_dist; d0 < neib_dist; d0++){
				dist[0] = d0;
				flag = 0;
				//---------- [i](x, y, z) ------------
				for (i = 0; i < mat->num_dims; i++){
					neib_point->c[i] = point->c[i] + dist[i];
					if (dist[i] != 0){ flag = 1; }
				}
				if (flag == 1 && include_check(neib_point, mat)){
					point->neib_list[j] = table + get_index(neib_point, mat);
					//if (get_index(neib_point, mat) >= num_elem){
					//	printf("Error get_index = %llu >= num_elem = %llu \n", get_index(neib_point, mat), num_elem);
					//}
					//print_neib_point(&neib_point, mat->num_dims);			
					j++;
				}
	}
			 if (mat->num_dims == 1)    { 
				 point->num_neib = j; 
				 free(neib_point->c);
				 free(neib_point);
				 free(dist);
				 return j; 
			 }
	}
			 if (mat->num_dims == 2)    { 
				 point->num_neib = j;
				 free(neib_point->c);
				 free(neib_point);
				 free(dist);
				 return j; 
			 }
	}
			 if (mat->num_dims == 3)    { 
				 point->num_neib = j;
				 free(neib_point->c);
				 free(neib_point);
				 free(dist);
				 return j; 
			 }
	}
			 if (mat->num_dims == 4)    { 
				 point->num_neib = j;
				 free(neib_point->c);
				 free(neib_point);
				 free(dist);
				 return j; 
			 }
	}
			 if (mat->num_dims == 5)    { 
				 point->num_neib = j;
				 free(neib_point->c);
				 free(neib_point);
				 free(dist);
				 return j; 
			 }
	}
			 if (mat->num_dims == 6)    { 
				 point->num_neib = j;
				 free(neib_point->c);
				 free(neib_point);
				 free(dist);
				 return j; 
			 }
	}
			 if (mat->num_dims == 7)    { 
				 point->num_neib = j;
				 free(neib_point->c);
				 free(neib_point);
				 free(dist);
				 return j; 
			 }
	}
			 if (mat->num_dims == 8)    { 
				 point->num_neib = j;
				 free(neib_point->c);
				 free(neib_point);
				 free(dist);
				 return j; 
			 }
	}
			 if (mat->num_dims == 9)    { 
				 point->num_neib = j;
				 free(neib_point->c);
				 free(neib_point);
				 free(dist);
				 return j; 
			 }
	}
			 point->num_neib = j; 
			 free(neib_point->c);
			 free(neib_point);
			 free(dist);
			 return j;
}

static inline void measure_d(struct data_point *point)
{
	double maxd = 0, sum_dist = 0, dist;
	int i;
	for (i = 0; i < point->num_neib; i++){
		dist = point->neib_list[i]->attri - point->attri;
		if (dist < 0){ dist = dist*(-1); }
		sum_dist += dist;
		if (maxd < dist){ maxd = dist; }
	}
	sum_dist = sum_dist - maxd;
	point->measure_d = sum_dist / (point->num_neib - 1);
	//printf("sum_dist = %8.2lf, maxd = %8.2lf, num_neib = %d \n", sum_dist, maxd, num_neib);
}

static inline double measure_beta(const struct data_point *point)
{
	double beta = 0, avg_Nplus = 0, avg_N = 0, max;
	int i;

	for (i = 0; i < point->num_neib; i++){
		avg_Nplus += point->neib_list[i]->measure_d;
	}
	avg_N = avg_Nplus;
	avg_Nplus += point->measure_d;
	avg_Nplus = avg_Nplus / (point->num_neib + 1);
	for (i = 0; i < point->num_neib; i++){
		if (point->neib_list[i]->measure_d > avg_Nplus){
			beta++;
		}
		else if (point->neib_list[i]->measure_d < avg_Nplus){
			beta--;
		}
	}
	if (point->measure_d > avg_Nplus){
		beta++;
	}
	else if (point->measure_d < avg_Nplus){
		beta--;
	}
	if (beta < 0){ beta = beta * (-1); }
	if (beta > 1){
		max = beta; 
	}else{
		max = 1;
	}
	beta = max / abs((point->num_neib + 1) - 2);
	beta = beta / (1 + avg_N);
	return beta;
}

static struct data_point *init_neib_table(struct slom_matrix *slom_mat, uint64_t num_elem)
{
	uint64_t i;
	int mun_neib;
	struct data_point *neib_table = (struct data_point *)malloc(sizeof(struct data_point) * num_elem);
	memset(neib_table, 0, sizeof(struct data_point) * num_elem);
	for (i = 0; i < num_elem; i++) {
		normalize_attri(i, neib_table + i, slom_mat);
		init_coord(i, neib_table + i, slom_mat);//duan
		mun_neib = init_neighbor(neib_table, neib_table + i, slom_mat, num_elem);//duan
		//printf("mun_neib %d \n", mun_neib);
	}
	return neib_table;
}

static void free_slom_matrix(struct slom_matrix *slom_mat)
{
	free(slom_mat->dist);
	free(slom_mat->lbc);
	free(slom_mat->ubc);
	free(slom_mat);
}

static void free_neib_table(struct data_point *neib_table, uint64_t num_elem)
{
	uint64_t i;
	for (i = 0; i < num_elem; i++) {
		free(neib_table[i].c);
		free(neib_table[i].neib_list);
	}
	free(neib_table);
}

uint64_t SLOM(double *buf, struct bbox *bb)
{
	uint64_t i, s_index = 0, num_elem;//s_index = 0 duan important
	double m_b, s_v, max_s_v = 0, max_attri = 1, min_attri = 0;
	struct data_point *neib_table;
	struct slom_matrix *slom_mat;
	/*
	printf("bb->num_dims %d \n", bb->num_dims);
	printf("bb->lb.c[0] %llu \n", bb->lb.c[0]);
	printf("bb->ub.c[0] %llu \n", bb->ub.c[0]);
	*/
	num_elem = bbox_volume(bb);

	for (i = 0; i < num_elem; i++) {
		if (buf[i] > max_attri){
			max_attri = buf[i];
		}
		if (buf[i] < min_attri){
			min_attri = buf[i];
		}
	}
	slom_mat = init_slom_matrix(bb, buf);
	slom_mat->max_v = max_attri;
	slom_mat->min_v = min_attri;
	neib_table = init_neib_table(slom_mat, num_elem);

	struct timeval tv;//duan
	gettimeofday(&tv, 0);
	double time_begin = (double)tv.tv_usec* 1.e-3 + tv.tv_sec * 1.e3;
	for (i = 0; i < num_elem; i++) {
		measure_d(neib_table + i);
	}

	for (i = 0; i < num_elem; i++) {
		m_b = measure_beta(neib_table + i);
		//printf("%8.2lf \n", m_b);
		s_v = neib_table[i].measure_d * m_b;
		//print_data_point(&point, bb->num_dims);//duan
		if (s_v > max_s_v){
			max_s_v = s_v;
			s_index = i;
		}
	}
	
	neib_table[s_index].slom_value = max_s_v;
	//print_data_point(neib_table + s_index, bb->num_dims);//duan
	gettimeofday(&tv, 0);
	double time_end = (double)tv.tv_usec* 1.e-3 + tv.tv_sec * 1.e3;
	//printf("Slom done!!! total time = %lf ms, num elem = %llu, data size = %llu \n", time_end - time_begin, num_elem, sizeof(double)*num_elem);
	free_slom_matrix(slom_mat);
	free_neib_table(neib_table, num_elem);
	return s_index;
}

/*
int main(void)
{
	uint64_t num_elem;
	double *d1;
	
	num_elem = 9;
	double d1[100] = {
	-16, -9, -16,
	-3, -1,   9,
	14,  1,  11,
	};
	bbox b1 = { .num_dims = 2, .lb.c = { 0, 0, 0 }, .ub.c = { 2, 2, 0 } };

	num_elem = 100;
	double d1[100] = {
		-16, -9, -16,   4,   8,  25,  -2,  20, -11,   9,
		 -3, -1,   9, -12,   1,   1,  -1,  -2,  -4,  -2,
		 14,  1,  11,   2, -13,  15,   4,   3,  11,  19,
		-10, 16, -11,  -2, -10, -11, -17,   4,   8, -15,
		 -5, 20, -11,   4,  -5,   8,   6,   6,  -2,  -1,
		 15, 10,  -9,   7,  12,  -9, -18,  16,   8,  -6,
		  0,  0,   0,   0, -21,  -5,  12, -15,  -5,  11,
		  0,  0,   0,   0,   5,   6,   1,   1,  -9,   3,
		  0,  8,   0,   0,  -9,  -8,  -1,  -2,   9,   5,
		  0,  0,   0,   0,  19,  -1,  -2,  -7,  -3, -12,
	};
	b1 = { .num_dims = 2, .lb.c = { 0, 0, 0 }, .ub.c = { 9, 9, 0 } };

	///////////// 8M ////////////////
	num_elem = 64 * 128 * 128;
	d1 = (double *) malloc(sizeof(double) * num_elem);
	memset(d1, 1, sizeof(sizeof(double) * num_elem));
	struct bbox b1 = { .num_dims = 3, .lb.c = { 0, 0, 0 }, .ub.c = { 63, 127, 127 } };//1024*1024

	SLOM(d1, &b1);
	free(d1);
	////////////// 16M ///////////////
	num_elem = 128 * 128 * 128;
	d1 = (double *) malloc(sizeof(double) * num_elem);
	memset(d1, 1, sizeof(sizeof(double) * num_elem));
	struct bbox b2 = { .num_dims = 3, .lb.c = { 0, 0, 0 }, .ub.c = { 127, 127, 127 } };//1024*1024

	SLOM(d1, &b2);
	free(d1);
	////////////// 24M ////////////////
	num_elem = 192 * 128 * 128;
	d1 = malloc(sizeof(double) * num_elem);
	memset(d1, 1, sizeof(sizeof(double) * num_elem));
	struct bbox b3 = { .num_dims = 3, .lb.c = { 0, 0, 0 }, .ub.c = { 191, 127, 127 } };//1024*1024

	SLOM(d1, &b3);
	free(d1);
	/////////////// 32M ///////////////
	num_elem = 256 * 128 * 128;
	d1 = malloc(sizeof(double) * num_elem);
	memset(d1, 1, sizeof(sizeof(double) * num_elem));
	struct bbox b4 = { .num_dims = 3, .lb.c = { 0, 0, 0 }, .ub.c = { 255, 127, 127 } };//1024*1024

	SLOM(d1, &b4);
	free(d1);
	//////////////// 40M //////////////
	num_elem = 320 * 128 * 128;
	d1 = malloc(sizeof(double) * num_elem);
	memset(d1, 1, sizeof(sizeof(double) * num_elem));
	struct bbox b5 = { .num_dims = 3, .lb.c = { 0, 0, 0 }, .ub.c = { 319, 127, 127 } };//1024*1024

	SLOM(d1, &b5);
	free(d1);
	//////////////// 48M //////////////
	num_elem = 384 * 128 * 128;
	d1 = malloc(sizeof(double) * num_elem);
	memset(d1, 1, sizeof(sizeof(double) * num_elem));
	struct bbox b6 = { .num_dims = 3, .lb.c = { 0, 0, 0 }, .ub.c = { 383, 127, 127 } };//1024*1024

	SLOM(d1, &b6);
	free(d1);
	//////////////// 56M //////////////
	num_elem = 448 * 128 * 128;
	d1 = malloc(sizeof(double) * num_elem);
	memset(d1, 1, sizeof(sizeof(double) * num_elem));
	struct bbox b7 = { .num_dims = 3, .lb.c = { 0, 0, 0 }, .ub.c = { 447, 127, 127 } };//1024*1024

	SLOM(d1, &b7);
	free(d1);
	//////////////// 64M //////////////
	num_elem = 256 * 256 * 128;
	d1 = malloc(sizeof(double) * num_elem);
	memset(d1, 1, sizeof(sizeof(double) * num_elem));
	struct bbox b8 = { .num_dims = 3, .lb.c = { 0, 0, 0 }, .ub.c = { 255, 255, 127 } };//1024*1024

	SLOM(d1, &b8);
	free(d1);

	return 0;
}
*/
/*decomment above for debugging*/
