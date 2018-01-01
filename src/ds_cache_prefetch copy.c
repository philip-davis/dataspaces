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

#include<unistd.h>
#include<stdio.h>
#include<stdlib.h>
#include<time.h>
#include "debug.h"
#include "ss_data.h"
#include "ds_cache_prefetch.h"

extern struct ss_storage       *ls;

pthread_mutex_t pmutex = PTHREAD_MUTEX_INITIALIZER;//init prefetching pthread function lock Duan
pthread_cond_t  pcond = PTHREAD_COND_INITIALIZER;//init prefetching pthread function cond Duan

int cond_index = 0; //Prefetching thread global condition array index Duan
int cond_num = 0; //Prefetching thread global condition number Duan

/*
Pthread function for prefetching data from SSD to DRAM  when call get function.
*/
void *prefetch_thread(void*attr){
	int j = 0;
	int local_cond_index = 0;
#ifdef DEBUG
	char *str;
	asprintf(&str, "init prefetch_thread: cond_num %d cond_index %d  ",
		cond_num, cond_index);
	uloga("'%s()': %s\n", __func__, str);
	free(str);
#endif
	while (j == 0)
	{
		pthread_mutex_lock(&pmutex);		
		if (cond_num == 0){/*sleep */
#ifdef DEBUG
			char *str;
			asprintf(&str, "wait prefetch_thread: cond_num %d cond_index %d  ",
				cond_num, cond_index);
			uloga("'%s()': %s\n", __func__, str);
			free(str);
#endif
			pthread_cond_wait(&pcond, &pmutex); //wait
#ifdef DEBUG
			asprintf(&str, "wake prefetch_thread: cond_num %d cond_index %d  ",
				cond_num, cond_index);
			uloga("'%s()': %s\n", __func__, str);
			free(str);
#endif
		}else{ /*cond_num > 0 */
			local_cond_index = cond_index;
			do{
				obj_data_copy_to_mem(pod_list.pref_od[local_cond_index]);//copy data from ssd to mem
				pod_list.pref_od[local_cond_index]->sl = in_memory_ssd;
				pod_list.pref_od[local_cond_index]->so = prefetching;
				ls->mem_used += obj_data_size(&pod_list.pref_od[local_cond_index]->obj_desc);
#ifdef DEBUG
				char *str;
				asprintf(&str, "runable prefetch_thread: cond_num %d local_cond_index %d  ",
					cond_num, local_cond_index);
				uloga("'%s()': %s\n", __func__, str);
				free(str);
#endif
				local_cond_index = get_prev(local_cond_index, MAX_PREFETCH);
			} while (pod_list.pref_od[local_cond_index] !=NULL && pod_list.pref_od[local_cond_index]->sl == prefetching && pod_list.pref_od[local_cond_index]->sl == in_ssd);

			cond_num = 0;
		}
		//sleep(1);
		pthread_mutex_unlock(&pmutex);
	}
	return NULL;
}

/*
*  Free memory if it is needed Duan
*/
int cache_replacement(int added_mem_size){
	int index = 0, version = 0;
	struct obj_data *od, *t;
	struct list_head *list;
	
//#ifdef DEBUG
	{
	char *str;
	asprintf(&str, "ls->mem_size %llu ls->mem_used %llu  added_mem_size %d",
		ls->mem_size, ls->mem_used, added_mem_size);
	uloga("'%s()': %s\n", __func__, str);
	free(str);
	}
//#endif

	if (ls->mem_size >= ls->mem_used + added_mem_size){
		return 0;
	}
	while (ls->mem_size < ls->mem_used + added_mem_size && index < ls->size_hash){
		version = index % ls->size_hash;
		list = &ls->obj_hash[index];
		list_for_each_entry_safe(od, t, list, struct obj_data, obj_entry) {

#ifdef DEBUG
			{
				char *str;
				asprintf(&str, "list_for_each_entry_safe: od->sl == %d od->so == %d od->data == %p od->_data == %p od->s_data == %p",
					od->sl, od->so, od->data, od->_data, od->s_data);
				str = str_append(str, bbox_sprint(&od->obj_desc.bb));
				uloga("'%s()': %s\n", __func__, str);
				free(str);
			}
#endif

			if (od->s_data != NULL && (od->data != NULL || od->_data != NULL) && od->so == caching){
			//if (od->sl == in_memory_ssd && (od->data != NULL || od->data != NULL) && od->so != prefetching){

#ifdef DEBUG
				{
					char *str;
					asprintf(&str, "od->s_data != NULL && (od->data != NULL || od->data != NULL) && od->so != prefetching: od->sl == %d od->so == %d od->data == %p od->_data == %p od->s_data == %p",
						od->sl, od->so, od->data, od->_data, od->s_data);
					str = str_append(str, bbox_sprint(&od->obj_desc.bb));
					uloga("'%s()': %s\n", __func__, str);
					free(str);
				}
#endif

				/*unload data in memory Duan*/
				obj_data_free_in_mem(od);
				od->so = normal;
				ls->mem_used -= obj_data_size(&od->obj_desc);			

				if (ls->mem_size > ls->mem_used + added_mem_size){ break; }

			}
			if (od->s_data == NULL && (od->data != NULL || od->_data != NULL) && od->so == caching){
		//if (od->sl == in_memory && (od->data != NULL || od->data != NULL) && od->so != prefetching){

#ifdef DEBUG	
			{
				char *str;
				asprintf(&str, "(od->s_data == NULL && (od->data != NULL || od->data != NULL) && od->so != prefetching: od->sl == %d od->so == %d od->data == %p od->_data == %p od->s_data == %p",
					od->sl, od->so, od->data, od->_data, od->s_data);
				str = str_append(str, bbox_sprint(&od->obj_desc.bb));
				uloga("'%s()': %s\n", __func__, str);
				free(str);
			}
#endif

				/*copy data to ssd and unload data in memory Duan*/
				obj_data_copy_to_ssd(od);
				obj_data_free_in_mem(od);

				od->so = normal;
				ls->mem_used -= obj_data_size(&od->obj_desc);			

				if (ls->mem_size > ls->mem_used + added_mem_size){ break; }
			}

		}
		index++;
	}

	if (ls->mem_size < ls->mem_used + added_mem_size){
		uloga("%s(): ERROR not enough caching space ls->mem_size %d < ls->mem_used + added_mem_size! \n", __func__, ls->mem_size, ls->mem_used, added_mem_size);
		return 0;
	}
	return 1;
}

/*
get next node index in the circle array list. Duan
*/
static int get_next(int current, int array_size){
	current++;
	if (current >= array_size){
		current = 0;
	}
	return current;
}

/*
get previous node index in the circle array list. Duan
*/
int get_prev(int current, int array_size){
	current--;
	if (current < 0){
		current = array_size - 1;
	}
	return current;
}

/*
insert a node into the tail of prefetch circle array list. Duan
*/
int prefetch_insert_tail(struct obj_data * pod, int array_size){
	if (pod_list.length <= 0){
		pod_list.length = 1;
	}
	else if (pod_list.length >= array_size){
		obj_data_free_in_mem(pod_list.pref_od[pod_list.head]);
		pod_list.pref_od[pod_list.head]->so = normal;
		ls->mem_used -= obj_data_size(&pod_list.pref_od[pod_list.head]->obj_desc);

		pod_list.head = get_next(pod_list.head, array_size);
		pod_list.tail = get_next(pod_list.tail, array_size);
	}
	else{
		pod_list.length = pod_list.length + 1;
		pod_list.tail = get_next(pod_list.tail, array_size);
	}
	pod->so = prefetching;
	pod_list.pref_od[pod_list.tail] = pod;

	cond_index = pod_list.tail;
	return pod_list.tail;
}
