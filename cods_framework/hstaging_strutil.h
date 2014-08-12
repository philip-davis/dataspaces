#ifndef __HSTAGING_STRUTIL_H__
#define __HSTAGING_STRUTIL_H__

#include <stdint.h>
#include <stdio.h>
#include <string.h>

// Trim all characters in delim off of both ends of str
void trim(char *str, const char *delim);
void int64s_to_str(int ndim, uint64_t *values, char *s);

#endif
