/** file: carraysearch.h

    The header for C functions to index and search arrays of values.
 */
#ifndef CARRAYSEARCH_H
#define CARRAYSEARCH_H
/* FastBit ibis C API for the fixed size types and some utility functions */
#include "capi.h"

#ifdef __cplusplus
extern "C" {
#endif
    struct FastBitQuery;
    struct FastBitResultSet;

    void* fb_build_index_int32(int32_t*, size_t, const char*);
    void* fb_build_index_float(float*, size_t, const char*);
    void* fb_build_index_double(double*, size_t, const char*, void*);
    void fb_free_data(void*);

    int fb_count_hits(void*, const char*, double);
    int fb_find_hits(void*, const char*, double, uint32_t*, size_t);

    //void *fb_build_result_set(void*, const char*, const char*, const char*, int *);
    int fb_build_result_set(void*, const char*, const char*, const char*, int *, void*);
#ifdef __cplusplus
}
#endif
#endif
