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
*  Pradeep Subedi (2017) RDI2 Rutgers University
*  pradeep.subedi@rutgers.edu
*/
#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <string.h>
#include "unistd.h"

#include "debug.h"
#include "ss_data.h"
#include "common.h"



#include "mpi.h"

static char *transport;
enum transport_type type = USE_DSPACES;

static int npapp_;

static int dims = 1;

static int rank_, nproc_;

static uint64_t lb[10]={0};
static uint64_t ub[10]={0};

static struct timer timer_;

static MPI_Comm gcomm;

static size_t elem_size;

static char transport_type_str_[256] = "DATASPACES";


typedef struct node{
	int time_step;
	char lock[10];
	char req_type[10];
	int offset;
	int req_size;
	char var_name_[10];
	uint64_t lbs[10];
	uint64_t ubs[10];
	struct node *next;
} node;
static void get_data_bounds(int rank, int data_size, int data_offset)
{
	//data size is the size from conf file
	//data_offset is the offset from conf file
	int temp = data_size/npapp_;
	lb[0]=rank*temp+data_offset;
	if(rank==(npapp_-1)){
		ub[0] = data_offset + data_size-1;
	}else{
		ub[0]=data_offset+(rank+1)*temp-1;
	}

}
static double* allocate_nd(uint64_t *lbs, uint64_t *ubs)
{
	double* tmp = NULL;
    int i = 0;
    uint64_t size = 1;
    for(i = 0; i < 3; i++){
            size *= (int)ubs[i]-(int)lbs[i]+1;
    }
    tmp = (double*)malloc(elem_size*size);
    return tmp;
}
static int generate_nd(double *mnd, unsigned int ts, int dims, uint64_t *lbs, uint64_t *ubs)
{
    //double value = 1.0*(rank_) + 0.0001*ts;
	double value = ts;
    int i,j;
	uint64_t mnd_size = 1;
	for(j = 0; j < dims; j++){
            mnd_size *= (int)ubs[j]-(int)lbs[j]+1;
    }
	mnd_size = mnd_size * elem_size/ sizeof(double);
    for(i = 0; i < mnd_size; i++)
        *(mnd+i) = value;
    return 0;
}

int main(int argc, char **argv)
{
	FILE *fin;
	node* list_head;
	node* last_access;
	int i,j;
	char lock[10];
	char buff[2048];
	char var_name[10];
	int lineno = 1, err;
	int ts, app_id, req_offset, data_size = 0;
	char req[10];
	int lbr[10];
	int ubr[10];
	last_access = (node*)malloc(sizeof(node));
	list_head = (node*)malloc(sizeof(node));
	fin = fopen(argv[1], "r");
	if (!fin){
		printf("File args not found\n");
		return -1;
	}
	for (i = 0; i < atoi(argv[2]); ++i)
	{
		while (fgets(buff, sizeof(buff), fin) != NULL) {
			//printf("Start new line\n");
			sscanf(buff, "%s\t%s\t%s\t%d\t%d\t%d\t%d\t%d\t%d", &lock, &req, &var_name, &lbr[0], &lbr[1], &lbr[2], &ubr[0], &ubr[1], &ubr[2]);
			//printf("Finish new line lb and ub are %d, %d, %d %d, %d, %d\n", lbr[0], lbr[1], lbr[2], ubr[0], ubr[1], ubr[2]);
			node *temp;
			temp = (node*) malloc(sizeof(node));
			temp->time_step = ts;
			strcpy(temp->req_type, req);
			strcpy(temp->lock, lock);
			//temp->offset = req_offset;
			//temp->req_size = data_size;
			strcpy(temp->var_name_,var_name);
			//printf("Finish before line\n");
			for (j = 0; j < 3; ++j)
			{
				temp->lbs[j]=(uint64_t)lbr[j];
				temp->ubs[j]=(uint64_t)ubr[j];
			}
			//printf("Finish after assignment line\n");
			temp->next = NULL;
			if(i==0 && lineno ==1){
				list_head = temp;
				last_access = temp;
				lineno++;
			}else{
				last_access->next = temp;
				last_access = temp;
			}
			//printf("Finished all line %d\n", i);

		}
		rewind(fin);
	}
	fclose(fin);
	printf("File loaded into memory \n");
	
	int nprocs, rank;
	int root = 0;
	elem_size = 8;
	// Using SPMD style programming
	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Barrier(MPI_COMM_WORLD);
	gcomm = MPI_COMM_WORLD;
	npapp_ = nprocs;
	app_id = atoi(argv[3]);
	timer_init(&timer_, 1);
	timer_start(&timer_);

	double tm_st, tm_end, tm_diff, tm_max;
	tm_st = timer_read(&timer_);
	//printf("About to common_init\n");
	common_init(nprocs, app_id, &gcomm, NULL);
	//printf("Finished common_init\n");
	tm_end = timer_read(&timer_);

#ifdef TIMING_PERF
	uloga("TIMING_PERF init_dspaces peer %d time %lf\n", common_rank(), tm_end-tm_st);
#endif
	tm_st = MPI_Wtime();
	node *tmp = list_head;
	while(tmp!=NULL){
		char lock_ty[10];
		sprintf(lock_ty, "%s", tmp->lock);
		if (strcmp(tmp->req_type, "low")==0){
			//sprintf(lock_ty, "%d", tmp->time_step);
			//lock on write
			common_lock_on_write(lock_ty, &gcomm);
			//printf("Lock on write issued %d, app id %d\n", rank, app_id);
		}else if(strcmp(tmp->req_type, "lor")==0){
			//lock on read
			common_lock_on_read(lock_ty, &gcomm);
			//printf("Lock on read issued %d, app id %d\n", rank, app_id);
		}else if (strcmp(tmp->req_type, "uow")==0){
			//sprintf(lock_ty, "%d", tmp->time_step);
			//unlock on write
			common_unlock_on_write(lock_ty, &gcomm);
			//printf("Unlock on write issued %d, app_id %d\n", rank, app_id);
		}else if(strcmp(tmp->req_type, "uor")==0){
			//unlock on read
			common_unlock_on_read(lock_ty, &gcomm);
			//printf("Unlock on read issued %d, app id %d\n", rank, app_id);
		}else if(strcmp(tmp->req_type, "r")==0){
			//issue read request
			
			//get_data_bounds(rank, tmp->req_size, tmp->offset);
			//printf("After Data Bounds in read %d, rank %d, lb = %f, ub = %f\n", app_id, rank, (double)lb[0], (double)ub[0]);
			double *data = NULL;
			data = allocate_nd(tmp->lbs, tmp->ubs);
			if(data == NULL){
				uloga("%s(): allocate_2d() failed.\n", __func__);
            		return -1; // TODO: free buffers
            	}
            memset(data, 0, elem_size);

            MPI_Barrier(gcomm);
            printf("Starting dspaces_get. %d %d %d, %d %d %d, rank %d\n", tmp->lbs[0],tmp->lbs[1],tmp->lbs[2],tmp->ubs[0],tmp->ubs[1],tmp->ubs[2], rank);
            dspaces_get(tmp->var_name_, tmp->time_step, elem_size, 3, tmp->lbs, tmp->ubs,
            		data);
            printf("Finished dspaces_get. %d, rank %d\n", app_id, rank);


        } else if(strcmp(tmp->req_type, "w")==0){
			//issue write request
			//printf("Getting into the write req. %d, rank %d\n", app_id, rank);
        	//get_data_bounds(rank, tmp->req_size, tmp->offset);
        	//printf("After Data Bounds in Write %d, rank %d, lb = %f, ub = %f\n", app_id, rank, (double)lb[0], (double)ub[0]);
        	double *data = NULL;
        	data = allocate_nd(tmp->lbs, tmp->ubs);
        	//printf("Starting data generation. %d, rank %d\n", app_id, rank);
        	generate_nd(data, tmp->time_step, 3, tmp->lbs, tmp->ubs);
        	//printf("Finished data generation. %d, rank %d\n", app_id, rank);

        	
        	MPI_Barrier(gcomm);
			//printf("Starting dspaces_put. %d, rank %d, lb = %f, ub = %f\n", app_id, rank, (double)lb[0], (double)ub[0]);
			dspaces_put(tmp->var_name_, tmp->time_step, elem_size, 3, tmp->lbs, tmp->ubs,
				data);
			printf("Finished dspaces_put. %d %d %d, %d %d %d, rank %d\n", tmp->lbs[0],tmp->lbs[1],tmp->lbs[2],tmp->ubs[0],tmp->ubs[1],tmp->ubs[2], rank);
			dspaces_put_sync();
			printf("Finished dspaces_put. %d, rank %d\n", app_id, rank);
			sleep(3);
        }
            tmp = tmp->next;
    }	
    	MPI_Barrier(gcomm);
    	tm_end = MPI_Wtime();
    	tm_diff = tm_end-tm_st;
    	uloga("TIMING_PERF App_ID: %d time %lf\n", app_id, tm_diff);

        if(rank == 0){
        	uloga("%s(): done\n", __func__);
        }

        MPI_Barrier(gcomm);
        common_finalize();
        MPI_Finalize();

        return 0;
    }

