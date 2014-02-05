#include<stdio.h>
#include "ss_data.h"
#define MAX 20	//number of attribute for one obj
#define NUM_TYPE 8
enum DATATYPE
{
        SHORT, USHORT, INT, UINT,
        LONG, ULONG, FLOAT, DOUBLE
};

struct metaData{
	int 		num_attr;
        char 		*attr[MAX];
        enum DATATYPE 	type[MAX];
};

int parse_meta(char* meta_conf, struct metaData* mtdata);
int build_obj_idx(struct obj_data* obj, struct metaData *mtdata);
//void* local_fb_query(struct obj_data* od, struct ss_storage *ls, struct obj_descriptor* odsc, void *qstr, int*);
//void* local_fb_query(struct obj_data* od, char* select, char* from, char* where, int *size_qret);
int local_fb_query(struct obj_data* od, char* select, char* from, char* where, int *size_qret, void* qret);
void parse_query_file(FILE* qfile, void *query);

