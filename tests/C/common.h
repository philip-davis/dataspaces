#ifndef __TEST_COMMON_H_
#define __TEST_COMMON_H_

#include "debug.h"
#include "timer.h"
#include "dataspaces.h"
#include "ds_gspace.h"
#ifdef DS_HAVE_DIMES
#include "dimes_server.h"
#endif

enum transport_type {
	USE_DSPACES = 0,
	USE_DIMES
};

inline int common_init(int num_peers, int appid) {
	return dspaces_init(num_peers, appid); 
}

inline void common_set_storage_type(int fst) {
	dspaces_set_storage_type(fst);
} 

inline int common_rank() {
	return dspaces_rank();
}

inline int common_peers() {
	return dspaces_peers();
}

inline void common_barrier() {
	dspaces_barrier();
}

inline void common_finalize() {
	dspaces_finalize();
}

inline void common_lock_on_read(const char *lock_name, void *gcomm) {
	dspaces_lock_on_read(lock_name, gcomm);
}

inline void common_unlock_on_read(const char *lock_name, void *gcomm) {
	dspaces_unlock_on_read(lock_name, gcomm);
}

inline void common_lock_on_write(const char *lock_name, void *gcomm) {
	dspaces_lock_on_write(lock_name, gcomm);
}

inline void common_unlock_on_write(const char *lock_name, void *gcomm) {
	dspaces_unlock_on_write(lock_name, gcomm);
}

inline int common_put (const char *var_name,
        unsigned int ver, int size,
        int xl, int yl, int zl,
        int xu, int yu, int zu,
        void *data, enum transport_type type) {
	if ( type == USE_DSPACES ) {
		return dspaces_put(var_name, ver, size,
			xl, yl, zl, xu, yu, zu,
			data);
	} else if (type == USE_DIMES) {
#ifdef DS_HAVE_DIMES
		return dimes_put(var_name, ver, size,
			xl, yl, zl, xu, yu, zu,
			data);
#else
		uloga("%s(): DataSpaces DIMES is not enabled!\n", __func__);
		return -1;
#endif
	}
}

inline int common_get (const char *var_name,
        unsigned int ver, int size,
        int xl, int yl, int zl,
        int xu, int yu, int zu,
        void *data, enum transport_type type) {
	if ( type == USE_DSPACES ) {
		return dspaces_get(var_name, ver, size,
			xl, yl, zl, xu, yu, zu,
			data);
	} else if (type == USE_DIMES) {
#ifdef DS_HAVE_DIMES
		return dimes_get(var_name, ver, size,
			xl, yl, zl, xu, yu, zu,
			data);
#else
		uloga("%s(): DataSpaces DIMES is not enabled!\n", __func__);
		return -1;
#endif
	}
}

inline int common_put_sync(enum transport_type type) {
	if (type == USE_DSPACES) {
		return dspaces_put_sync();
	} else if (type == USE_DIMES) {
#ifdef DS_HAVE_DIMES
		return dimes_put_sync_all();
#else
		uloga("%s(): DataSpaces DIMES is not enabled!\n", __func__);
		return -1;
#endif
	}
}

int common_run_server(int num_sp, int num_cp, enum transport_type type);

void check_data(const char *var_name, double *buf, int num_elem, int rank, int ts);

#endif //end of __TEST_COMMON_H_
