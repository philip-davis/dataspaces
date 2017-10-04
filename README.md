DataSpaces version based on dataspaces as service

Includes new APIs to write to Ceph cluster and in-node SSD.

Define SSD path in src/mem_persist.c as #define PMEM_PATH "/home/ceph-deploy/"

Define #define PMEM_SIZE 1*1024*1024*1024L for a total of 1GB memory-mapped file
for storing data to SSD. This memory mapped file resides in PMEM_PATH.

All writes to SSD is reflected in this file.

Define cluster_name, user_name and path for ceph.conf()  inside ceph_init() in ds_gspace.c

New APIs

int dspaces_put_ssd (const char *var_name,
        unsigned int ver, int size,
        int ndim, uint64_t *lb, uint64_t *ub,
        const void *data)

int dspaces_put_ceph (const char *var_name,
        unsigned int ver, int size,
        int ndim, uint64_t *lb, uint64_t *ub,
        const void *data)

DataSpaces takes care of data movement internally for data reads irrespective of data location, i.e., only writers need to specify where to write data by respective API calls.
  
