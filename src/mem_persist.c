
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

/* using 64M ssd file for this example */
//#define PMEM_SIZE 1*512*1024L
//#define PMEM_SIZE 256*1024*1024*1024L
//#define PMEM_SIZE 128*1024*1024*1024L
#define PMEM_SIZE 64*1024*1024*1024L
//#define PMEM_SIZE 60*1024*1024*1024L
//#define PMEM_SIZE 56*1024*1024*1024L
//#define PMEM_SIZE 48*1024*1024*1024L
//#define PMEM_SIZE 32*1024*1024*1024L
//#define PMEM_SIZE 16*1024*1024*1024L
//#define PMEM_SIZE 8*1024*1024*1024L
//#define PMEM_SIZE 1*1024*1024*1024L

/* using /ssd1/sd904/ ssd file path for this example */
//#define PMEM_PATH "/ssd/users/ps917/"
//#define PMEM_PATH "/home/subedip/"
#define PMEM_PATH "/lustre/atlas/scratch/subedip1/"
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

