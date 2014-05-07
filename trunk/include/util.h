#ifndef __STRUTIL_H_
#define __STRUTIL_H_

size_t str_len(const char *str);
char *str_append_const(char *, const char *);
char *str_append(char *, char *);

/*******************************************************
   Processing parameter lists
**********************************************************/
/*
   Process a ;-separated and possibly multi-line text and 
   create a list of name=value pairs from each 
   item which has a "name=value" pattern. Whitespaces are removed. 
   Input is not modified. Space is allocated;
   Also, simple "name" or "name=" patterns are processed and 
   returned with value=NULL. 
*/
struct PairStruct {
    char * name;
    char * value;
    struct PairStruct * next;
};
typedef struct PairStruct PairStruct;

PairStruct * text_to_name_value_pairs (const char * text);
void free_name_value_pairs (PairStruct * pairs);

#endif
