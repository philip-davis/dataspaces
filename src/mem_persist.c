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
*  Shaohua Duan (2016) TASSL Rutgers University
*  sd904@rdi2.rutgers.edu
*/

#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include<string.h>
#include "debug.h"
#include "mem_persist.h"
#include<time.h>
#include <ctype.h>
#include <daos.h>
#include <linux/uuid.h>
#include <assert.h>
#include <mpi.h>

/* using 128G ssd file for this example */
//#define PMEM_SIZE 256*1024*1024*1024L
//#define PMEM_SIZE 128*1024*1024*1024L
//#define PMEM_SIZE 64*1024*1024*1024L
//#define PMEM_SIZE 60*1024*1024*1024L
//#define PMEM_SIZE 56*1024*1024*1024L
//#define PMEM_SIZE 48*1024*1024*1024L
//#define PMEM_SIZE 32*1024*1024*1024L
//#define PMEM_SIZE 16*1024*1024*1024L
//#define PMEM_SIZE 8*1024*1024*1024L
//#define PMEM_SIZE 1*1024*1024*1024L
#define PMEM_SIZE 512*1024*1024L
/* using /ssd1/sd904/ ssd file path for this example */
#define PMEM_PATH "ssd/"
//#define PMEM_PATH "/home1/sd904/"
//#define PMEM_PATH ""

struct PMemEntry{
	struct PMemEntry *succ;
	int isfree;
	uint64_t size;
	void *pmem_ptr;
};

static struct PMemEntry *pmem_header = NULL;
static int fd;
static char *pmem_file;
//static char pmem_file[100];

static daos_handle_t poh, coh, objh;
static daos_obj_id_t oid;
static uuid_t pool_uuid, cont_uuid;

void *pmem_alloc(uint64_t num_bytes)
{
#ifdef DEBUG
	{
	char *str;
	asprintf(&str, "num_bytes %d ", num_bytes);
	uloga("'%s()': %s\n", __func__, str);
	free(str);
	}
#endif

	struct PMemEntry *pmem_iter, *pmem_iter_succ, * pmem_splited;
	if (num_bytes <= 0){
		return NULL;
	}
	pmem_iter = pmem_header;
	while (pmem_iter != NULL ){
		if (pmem_iter->size >= num_bytes && pmem_iter->isfree == 1){ break; }
		pmem_iter = pmem_iter->succ;
	}
	if (pmem_iter == NULL){
		return NULL;
	}
	pmem_iter->isfree = 0;
	
	if (pmem_iter->size > num_bytes){ //split node into two node
		pmem_splited = (struct PMemEntry *)malloc(sizeof(struct PMemEntry));
		pmem_splited->succ = pmem_iter->succ;
		pmem_iter->succ = pmem_splited;
		pmem_splited->size = pmem_iter->size - num_bytes;
		pmem_splited->isfree = 1;
		pmem_splited->pmem_ptr = pmem_iter->pmem_ptr + num_bytes;
		pmem_iter->size = num_bytes;
	}

	return pmem_iter->pmem_ptr;
}

int pmem_free(void *pmem_ptr)
{
	struct PMemEntry *pmem_iter, *pmem_iter_succ, *pmem_iter_prev = NULL;
	if (pmem_ptr == NULL){
		return 0;
	}
	pmem_iter = pmem_header;
	while (pmem_iter != NULL && pmem_iter->pmem_ptr != pmem_ptr){
		pmem_iter_prev = pmem_iter;
		pmem_iter = pmem_iter->succ;
	}

	if (pmem_iter != NULL)/*matched node in the middle and at lest three nodes in the list*/
	{
		pmem_iter->isfree = 1;
		if (pmem_iter->succ != NULL && pmem_iter->succ->isfree == 1){ /*merge node with behind node*/
			pmem_iter->size += pmem_iter->succ->size;
			pmem_iter_succ = pmem_iter->succ;
			pmem_iter->succ = pmem_iter->succ->succ;
			free(pmem_iter_succ);
			
		}
		if (pmem_iter_prev != NULL && pmem_iter_prev->isfree == 1){/*merge node with before node*/
			pmem_iter_prev->succ = pmem_iter->succ;
			pmem_iter_prev->size += pmem_iter->size;
			free(pmem_iter);
		}
	}
	return 1;
}


static size_t pmem_str_len(const char *str)
{
	if (str)
		return strlen(str);
	else
		return 0;
}

static char *pmem_str_append_const(char *str, const char *msg)
{
	int len, fix_str;

	len = pmem_str_len(str) + pmem_str_len(msg) + 1;
	fix_str = (str == 0);
	str = realloc(str, len);
	if (fix_str)
		*str = '\0';
	if (str)
		strcat(str, msg);

	return str;
}

void dspaces_daos_init(MPI_Comm *comm)
{

    daos_pool_info_t pool_info = {0};
    daos_cont_info_t cont_info = {0};
    daos_rank_list_t svc = {0};
    daos_obj_id_t oid;
    int rc;
    daos_epoch_t epoch = 0;
    int rank;
    daos_iov_t iov = {0};

    rc = MPI_Comm_rank(*comm, &rank);
    assert(rc == MPI_SUCCESS);

    printf("rank = %i\n", rank);
    rc = daos_init();
    assert(rc == 0 && "daos_init");

    if(rank == 0) {
        rc = daos_pool_create(0731, geteuid(), getegid(), NULL,
                         NULL, "pmem",  PMEM_SIZE,
                         &svc, pool_uuid, NULL);
        assert(rc == 0 && "daos_pool_create");
    }

    rc = MPI_Bcast(pool_uuid, 16, MPI_CHAR, 0, *comm);
    assert(rc == MPI_SUCCESS && "MPI_Bcast(pool_uuid)");

    rc = MPI_Bcast(&pool_info, sizeof(pool_info), MPI_CHAR, 0, *comm);
    assert(rc == MPI_SUCCESS && "MPI_Bcast(pool_info)");

    if(rank == 0) {
        rc = daos_pool_connect(pool_uuid, NULL, NULL, DAOS_PC_RW,
                            &poh, &pool_info, NULL);
        assert(rc == 0 && "daos_pool_connect");
        
        rc = daos_pool_local2global(poh, &iov);
        assert(rc == 0 && "daos_pool_local2_global");
    }

    rc = MPI_Bcast(&iov.iov_buf_len, 1, MPI_UINT64_T, 0, *comm);
    assert(rc == MPI_SUCCESS && "MPI_Bcast(iov.iov_buf_len)");
    
    iov.iov_buf = malloc(iov.iov_buf_len);
    iov.iov_len = iov.iov_buf_len;

    if(rank == 0) {
        rc = daos_pool_local2global(poh, &iov);
        assert(rc == 0 && "daos_pool_local2global");
    }
    rc = MPI_Bcast(iov.iov_buf, iov.iov_len, MPI_BYTE, 0, *comm);
    assert(rc == MPI_SUCCESS && "MPI_Bcast(iov.iov_buf)");

    if(rank != 0) {
        rc = daos_pool_global2local(iov, &poh);
        assert(rc == 0 && "daos_pool_global2local");
    }

    free(iov.iov_buf);
    iov.iov_buf = NULL;
    iov.iov_len = iov.iov_buf_len = 0;

    if(rank == 0) {
        uuid_generate(cont_uuid);
        rc = daos_cont_create(poh, cont_uuid, NULL);
        assert(rc == 0 && "daos_cont_create");
    }

    rc = MPI_Bcast(cont_uuid, 16, MPI_CHAR, 0, *comm);
    assert(rc == 0 && "MPI_Bcast(cont_uuid)");

    if(rank == 0) {
        rc = daos_cont_open(poh, cont_uuid, DAOS_COO_RW, &coh,
                                        &cont_info, NULL);
        assert(rc == 0 && "daos_cont_open");
    }

    if(rank == 0) {
        rc = daos_cont_local2global(coh, &iov);
        assert(rc == 0 && "daos_cont_local2global");
    }

    rc = MPI_Bcast(&iov.iov_buf_len, 1, MPI_UINT64_T, 0, *comm);
    assert(rc == MPI_SUCCESS && "MPI_Bcast(iov.iov_buf_len)");

    iov.iov_buf = malloc(iov.iov_buf_len);
    iov.iov_len = iov.iov_buf_len;

    if(rank == 0) {
        rc = daos_cont_local2global(coh, &iov);
        assert(rc == 0 && "daos_cont_local2global");
    }

    rc = MPI_Bcast(iov.iov_buf, iov.iov_len, MPI_BYTE, 0, *comm);
    assert(rc == MPI_SUCCESS && "MPI_Bcast(iov.iov_buf)");

    if(rank != 0) {
        rc = daos_cont_global2local(poh, iov, &coh);
        assert(rc == 0 && "daos_cont_global2local");
    }

    free(iov.iov_buf);
    iov.iov_len = iov.iov_buf_len = 0;

    oid.mid = 0;
    oid.lo = rank;
    daos_obj_id_generate(&oid, DAOS_OC_LARGE_RW);
    rc = daos_obj_declare(coh, oid, epoch, NULL, NULL);
    assert(rc == 0 && "daos_obj_declare");

    rc = daos_obj_open(coh, oid, epoch, DAOS_OO_RW, &objh, NULL);
    assert(rc == 0 && "daos_obj_open");

}

void *get_daos_obj_handle()
{

    return(&objh);

}

void pmem_init(const char *file_name)
{
	asprintf(&pmem_file, PMEM_PATH);
	pmem_file = pmem_str_append_const(pmem_file, file_name);

	//#ifdef DEBUG
	{
		char *str;
		asprintf(&str, "pmem_file %s ", pmem_file);
		uloga("'%s()': %s\n", __func__, str);
		free(str);
	}
	//#endif
	
	void * start_ptr = 0, *mem_ptr;
	
	fd = open(pmem_file, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR);
	if (fd == -1)
	{
		uloga("%s(): ERROR pmem file created failed! \n", __func__);
		perror("open");
		exit(1);
	}
	lseek(fd, PMEM_SIZE - 1, SEEK_SET);
	write(fd, "\0", 1);
	lseek(fd, 0, SEEK_SET);

	mem_ptr = mmap(start_ptr, PMEM_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
	if (mem_ptr == MAP_FAILED)
	{
		uloga("%s(): ERROR mmap failed! \n", __func__);
		perror("mmap");
		exit(1);
	}
	if (close(fd) == -1)
	{
		perror("close");
		exit(1);
	}
	
	pmem_header = (struct PMemEntry *)malloc(sizeof(struct PMemEntry));
	pmem_header->succ = NULL;
	pmem_header->isfree = 1;
	pmem_header->size = PMEM_SIZE;
	pmem_header->pmem_ptr = mem_ptr;
	
//#ifdef DEBUG
	{
		char *str;
		asprintf(&str, "init ok. pmem_file %s, pmem_size %llu ", pmem_file, pmem_header->size);
		uloga("'%s()': %s\n", __func__, str);
		free(str);
	}
//#endif

}

void pmem_destroy()
{
	struct PMemEntry *pmem_iter, *pmem_iter_prev;
	pmem_iter = pmem_header;
	while (pmem_iter!=NULL){
		pmem_iter_prev = pmem_iter;
		pmem_iter = pmem_iter->succ;
		free(pmem_iter_prev);

	}
	if (pmem_header != NULL){
		munmap(pmem_header->pmem_ptr, PMEM_SIZE);
	}
	close(fd);
	remove(pmem_file);
	free(pmem_file);

//#ifdef DEBUG
	{
	char *str;
	asprintf(&str, "destroy ok.");
	uloga("'%s()': %s\n", __func__, str);
	free(str);
	}
//#endif
}

void dspaces_daos_destroy(MPI_Comm *comm)
{

    int rank;
    int rc;

    rc = MPI_Comm_rank(*comm, &rank);
    assert(rc == MPI_SUCCESS);

    rc = daos_obj_close(objh, NULL);
    assert(rc == 0 && "doas_obj_close");

    rc = daos_cont_close(coh, NULL);
    assert(rc == 0 && "daos_cont_close");

    MPI_Barrier(*comm);

    if(rank == 0) {
        rc = daos_cont_destroy(poh, cont_uuid, 1, NULL);
        assert(rc == 0 && "daos_cont_destroy");
    }
    rc = daos_pool_disconnect(poh, NULL);
    assert(rc == 0 && "daos_pool_disconnect");

    MPI_Barrier(*comm);

    if(rank == 0) {
        rc = daos_pool_destroy(pool_uuid, NULL, 1, NULL);
        assert(rc == 0 && "daos_pool_destroy");
    }

    rc = daos_fini();
    assert(rc == 0 && "daos_fini");

}

void int_to_char(int n, char s[])
{
	int i, j, sign;
	if ((sign = n)<0)
		n = -n;
	i = 0;
	do{
		s[i++] = n % 10 + '0';
	} while ((n /= 10)>0);
	if (sign<0)
		s[i++] = '-';
	s[i] = '\0';
	for (j = i; j >= 0; j--){}
		//printf("%c", s[j]);
}

/*
int main(int argc, char *argv[]) {
	//-----------------------------------case1---------------------------------------
	printf("case1:add\n");
	char *str_in_mem, *str_in_ssd, *str_in_mem_tmp;
	int str_len;

	int dsg_id = 0;
	char dsg_id_str[100];

	int_to_char(dsg_id, dsg_id_str);

	pmem_init(dsg_id_str);//ssd storage initiate Duan
	dsg_id = 1;
	int_to_char(dsg_id, dsg_id_str);
	pmem_init(dsg_id_str);//ssd storage initiate Duan
	dsg_id = 2;
	int_to_char(dsg_id, dsg_id_str);
	pmem_init(dsg_id_str);//ssd storage initiate Duan
	dsg_id = 3;
	int_to_char(dsg_id, dsg_id_str);
	pmem_init(dsg_id_str);//ssd storage initiate Duan
	return 1;
}

	asprintf(&str_in_mem, "DataSpaces ");
	str_len = strlen(str_in_mem);

	str_in_ssd = pmem_alloc(str_len*sizeof(char));
	printf("pmem_alloc:%s \n", str_in_mem);
	printf("pmem_alloc: str_in_ssd:%p \n", str_in_ssd);
	memcpy(str_in_ssd, str_in_mem, str_len*sizeof(char)); //void *memcpy(void *dest, const void *src, size_t n);
	msync(str_in_ssd, str_len*sizeof(char), MS_ASYNC);//int msync ( void * ptr, size_t len, int flags) flags = MS_ASYNC|MS_SYNC
	
	//-----------------------------------case2---------------------------------------
	printf("case2:add\n");
	char *str_in_mem_2, *str_in_ssd_2;
	int str_len_2;
	asprintf(&str_in_mem_2, "ssd ");
	str_len_2 = strlen(str_in_mem_2);

	str_in_ssd_2 = pmem_alloc(str_len_2*sizeof(char));
	printf("pmem_alloc:%s \n", str_in_mem_2);
	printf("pmem_alloc: str_in_ssd_2:%p \n", str_in_ssd_2);
	memcpy(str_in_ssd_2, str_in_mem_2, str_len_2*sizeof(char)); //void *memcpy(void *dest, const void *src, size_t n);
	msync(str_in_ssd_2, str_len_2*sizeof(char), MS_ASYNC);//int msync ( void * ptr, size_t len, int flags) flags = MS_ASYNC|MS_SYNC
	
	//-----------------------------------case3---------------------------------------
	printf("case3:add\n");
	char *str_in_mem_3, *str_in_ssd_3;
	int str_len_3;
	asprintf(&str_in_mem_3, "data ");
	str_len_3 = strlen(str_in_mem_3);

	str_in_ssd_3 = pmem_alloc(str_len_3*sizeof(char));
	printf("pmem_alloc:%s \n", str_in_mem_3);
	printf("pmem_alloc: str_in_ssd:%p \n", str_in_ssd_3);
	memcpy(str_in_ssd_3, str_in_mem_3, str_len_3*sizeof(char)); //void *memcpy(void *dest, const void *src, size_t n);
	msync(str_in_ssd_3, str_len_3*sizeof(char), MS_ASYNC);//int msync ( void * ptr, size_t len, int flags) flags = MS_ASYNC|MS_SYNC

	//-----------------------------------case4---------------------------------------
	printf("case4:update\n");
	asprintf(&str_in_mem_tmp, "DataSpaceS ");//DataSpaces ssd data stage text
	str_len = strlen(str_in_mem_tmp);
	memcpy(str_in_ssd, str_in_mem_tmp, str_len*sizeof(char));
	printf("str_in_ssd: %s\n", str_in_ssd);
	memcpy(str_in_mem, str_in_ssd, str_len*sizeof(char));
	printf("str_in_mem: %s\n", str_in_mem);
	
	//-----------------------------------case5---------------------------------------
	printf("case5:free\n");
	printf("free 1 \n");
	pmem_free(str_in_ssd);
	printf("free 1 \n");
	pmem_free(str_in_ssd);
	printf("free 2 \n");
	pmem_free(str_in_ssd_2);
	printf("free 3 \n");
	pmem_free(str_in_ssd_3);
	
	pmem_destroy();
	free(str_in_mem);
	free(str_in_mem_2);
	free(str_in_mem_3);
	free(str_in_mem_tmp);
	
	return 0;
}
*/
