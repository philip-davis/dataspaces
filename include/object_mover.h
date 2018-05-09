#ifndef __OBJECT_MOVER_H
#define __OBJECT_MOVER_H

#include <stddef.h>
#include <stdint.h>

enum {
  SIRIUS_CEPH_TIER_SSD,
  SIRIUS_CEPH_TIER_HDD,
  SIRIUS_CEPH_TIER_TD,
};

/**
 * Initialize
 */
extern void sirius_ceph_initialize(const char* ceph_conf_file);

/**
 * Create an object in the specified tier asynchronously
 *
 * @param[in] tier the tier in which to create the object
 * @param[in] oid the name of the object
 * @param[in] buf pointer the buffer
 * @param[in] len length of the buffer
 * @param[out] err  0 success
 * @param[out] err <0 failure
 */
extern void sirius_ceph_create_async(int tier, const char *oid, const char *buf, size_t len, int *err);

/**
 * Move an object to the specified tier asynchronously
 *
 * @param[in] tier the tier to which to move the object
 * @param[in] oid the name of the object
 * @param[out] err  0 success
 * @param[out] err <0 failure
 */
extern void sirius_ceph_move_async(int tier, const char *oid, int *err);

/**
 * Read an object asynchronously
 *
 * @param[in] oid the name of the object
 * @param[in] buf where to store the results 
 * @param[in] off the offset to start reading from in the object
 * @param[in] len the number of bytes to read
 * @param[out] ret number of bytes read on success, negative error code on failure
 */
extern void sirius_ceph_read_async(const char *oid, char* buf, uint64_t off, size_t len, int *ret);

/**
 * Delete an object asynchronously
 *
 * @param[in] oid the name of the object
 * @param[out] err  0 success
 * @param[out] err <0 failure
 */
extern void sirius_ceph_delete_async(const char *oid, int *err);

/**
 * Finalize
 */
extern void sirius_ceph_finalize();

#endif