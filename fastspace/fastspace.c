#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "debug.h"
#include "carraysearch.h"
#include "fastspace.h"
#include "list.h"

#define NUM_TYPE 8
#define TYPE_MAX 4 //length of a data type name(s(short), i(int), l(long), f(float), d(double))
#define NAME_MAX 10 //length of variable name
char* type_name[NUM_TYPE] = {"SHORT", "USHORT", "INT", "UINT", "LONG", "ULONG", "FLOAT", "DOUBLE"};

static void eat_spaces(char *line)
{
        char *t = line;
        while(t && *t){
                if(*t !=' ' && *t!='\t' && *t!='\n')
                        *line++ = *t;
                t++;
        }
        if(line)
                *line = '\0';
}

int parse_meta(char* meta_conf, struct metaData* mtdata)
{
	/**************
	col_num = 2
	time int	//[attr_name, data_type]
	tempreture float
	**************/
	printf("Start to parse_meta file\n");
	FILE* fin = fopen(meta_conf, "rt");
	if(!fin)
		return -errno;

	int i, j;
	char *err;
	char buff[20];
	fscanf(fin, "num_attr=%d\n", &mtdata->num_attr);
	for(i = 0; i < mtdata->num_attr; i++){
		err = fgets(buff, sizeof(buff), fin); 
		if(err == NULL){
			fclose(fin);
			return -1;	//TODO
		}	
                char *t = strstr(buff, ";");
                t[0] = '\0';
		eat_spaces(buff);
                t++;
		eat_spaces(t);

                //mtdata->attr[i] = buff;
                mtdata->attr[i] = malloc(sizeof(NAME_MAX));
		memcpy(mtdata->attr[i], buff, sizeof(buff));

		mtdata->type[i] = *t;	

		/*for(j = 0; j < NUM_TYPE; j++){
			if(strcmp(t, type_name[j]) == 0){
				mtdata->type[i] = '';
			}
		}*/
		printf("attr=%s\ntype=%c\n", mtdata->attr[i], mtdata->type[i]);
	}
	return 0;
}

void parse_query_str(void *query, char *select, char *from, char *where)
{
	//TODO design query formulation
	const char* str = (char *)query;
	const char* end;

	while(isspace(*str)) ++str;
	if(0 == strncasecmp(str, "select ", 7)) {
		//|| 0 == strncmp(str, "Select ", 7 ||
		//0 == strncmp(str, "SELECT ", 7))){
		str += 7;
		while(isspace(*str)) ++str;

		end = strstr(str, " from ");
		if(end == 0){
			end = strstr(str, " From ");
			if(end == 0)
				end = strstr(str, " FROM ");
		}
		if(end){	//found FROM clause
			int i = 0;
			while(str < end){
				*(select+i) = *str;
				i++;
				++str;
			}
			str = end + 1;
		}
		else{		//no FROM, found WHERE clause
			end = strstr(str, " where ");
			if(end == 0){
				end = strstr(str, " Where ");
				if(end == 0)
					end = strstr(str, " WHERE ");
			}	
			if(end == 0){
				//select = str;
				memcpy(select, str, sizeof(char) * strlen(str));
				str = 0;
			}
			else{
				int i = 0;
				while(str < end){
					*(select+i) = *str;
					i++;
					++str;	
				}
				str = end + 1;
			}				
		}
	}

	if(str != 0 && 0 == strncasecmp(str, "from ", 5)){
		str += 5;
		while(isspace(*str)) ++str;
		end = strstr(str, " where ");
		if(end == 0){
			end = strstr(str, " Where ");
			if(end == 0)
				end = strstr(str, " WHERE ");
		}
		if(end){
			int i = 0;
			while(str < end){
				*(from+i) = *str;
				i++;
				str++;
			}
			str = end + 1;
		}else{
				//from = str;
				memcpy(from, str, sizeof(char) * strlen(str));
				str = 0;
		}
	}

	if(str != 0 || 0 == strncasecmp(str, "where ", 6)){
		str += 6;
		//where = str;
		memcpy(where, str, sizeof(char)* strlen(str));
	}
}

/*
int build_obj_idx(struct obj_data* obj, struct metaData *mtdata)
{
	//idx = fastbit_build_index(obj->data)
	void *cas;
	int nelem = obj_data_size(&obj->obj_desc)/(obj->obj_desc.size);
	char *colname = mtdata->attr;
	const char *indopt = "<binning start=300 end=1500 nbins=100000 scale=simple/>";
	if(mtdata->type[0] == DOUBLE){
		cas = fb_build_index_double((double*)obj->data, nelem, indopt, (void *)colname);
		//cas = fb_build_index_double((double*)obj->data, nelem, 0, (void *)colname);
		if(cas == 0)
			return -2;//TODO
	}
	else if(mtdata->type[0] == INT){
		cas = fb_build_index_int32((double*)obj->data, nelem, 0);
		if(cas == 0)
			return -2;//TODO
	}
	obj->fb_obj = cas; 	//TODO only store index to save memory

	// for test index with query 
	//int num;
	//const char *op = ">";
	//num = fb_count_hits(obj->fb_obj, op, 10);
	//printf("\nbuild_obj_idx, num_hits = %d\n", num);
	
	return 0;
}*/

int build_obj_idx(struct obj_data* obj, struct metaData *mtdata)
{
        int nelem = obj_data_size(&obj->obj_desc)/(obj->obj_desc.size);

	//Follow the struct metaData to split obj->data into multiple variables
        int num_col = mtdata->num_attr;
	void *tmp = NULL; 
        tmp = malloc(obj_data_size(&obj->obj_desc));
        int stride = 0, cur_pt = 0;
        int i, j;	
        for(i = 0; i < num_col; i++){
		mtdata->pointer[i] = (void*)(tmp + cur_pt);
                if(mtdata->type[i] == 'd'){	//DataType is DOUBLE
                        for(j = 0; j < nelem; j++){
                                *(double*)(tmp + cur_pt) = *(double*)(obj->data + j*(obj->obj_desc.size) + stride);
				cur_pt += sizeof(double);
                        }
                        stride += sizeof(double);
                }
        }
	free(obj->_data);
	obj->data = obj->_data = tmp;

	void *cas;
        char **colname = mtdata->attr;
        const char *indopt = NULL; 

	cas = fb_build_index(num_col, mtdata->pointer, mtdata->type, nelem, 0, mtdata->attr);
	if(cas == 0)
		return -2;

	obj->fb_obj = cas;
	return 0;
}

int local_fb_query(struct obj_data* od, char* select, char* from, char* where, int *size_qret, void *qret)
{
	//void *ret;
	int err = -1;
	err = fb_build_result_set(od->fb_obj, select, from, where, size_qret, qret);
	//printf("Query result local_fb_query=%s\n", (char *)ret);
	if(err == 0){
	//	printf("can not get query result\n");
		return 0;
	}
	return err;
}
