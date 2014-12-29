/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.ac by autoheader.  */

/* Maximum number of array dimension */
#define BBOX_MAX_NDIM 3

/* Build for Stampede option */
#define BUILD_FOR_STAMPEDE 0

/* Size of rdma memory that can be used for data writes/reads in DIMES */
#define DIMES_RDMA_BUFFER_SIZE 64

/* Max number of concurrent rdma read operations that can be issued by DIMES
   in the data fetching process */
#define DIMES_RDMA_MAX_NUM_CONCURRENT_READ 4

/* DIMES is enabled */
/* #undef DS_HAVE_DIMES */

/* Fixed Gemini cookie */
/* #undef GNI_COOKIE */

/* Fixed Gemini ptag */
/* #undef GNI_PTAG */

/* Define if you have Cray Portals. */
/* #undef HAVE_CRAY_PORTALS */

/* Define if you have the DCMF. */
/* #undef HAVE_DCMF */

/* Define to 1 if you have the <dcmf.h> header file. */
/* #undef HAVE_DCMF_H */

/* Define to 1 if you have the <gni_pub.h> header file. */
#define HAVE_GNI_PUB_H 1

/* Define to 1 if you have <infiniband/verbs.h>. */
#define HAVE_IBVERBS_H 1

/* Define if you have the Infiniband. */
/* #undef HAVE_INFINIBAND */

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the <p3nal_utcp.h> header file. */
/* #undef HAVE_P3NAL_UTCP_H */

/* Define to 1 if you have the <p3rt/p3rt.h> header file. */
/* #undef HAVE_P3RT_P3RT_H */

/* Define if you have the PAMI. */
/* #undef HAVE_PAMI */

/* Define to 1 if you have the <pami.h> header file. */
/* #undef HAVE_PAMI_H */

/* Define to 1 if you have the <pmi.h> header file. */
#define HAVE_PMI_H 1

/* Define if you have the Portals. */
/* #undef HAVE_PORTALS */

/* Define to 1 if you have <portals3.h>. */
/* #undef HAVE_PORTALS3_H */

/* Define to 1 if you have <portals/portals3.h>. */
/* #undef HAVE_PORTALS_PORTALS3_H */

/* Define if you have POSIX threads libraries and header files. */
#define HAVE_PTHREAD 1

/* Define to 1 if you have the `PtlACEntry' function. */
/* #undef HAVE_PTLACENTRY */

/* Define to 1 if you have the `PtlErrorStr' function. */
/* #undef HAVE_PTLERRORSTR */

/* Define to 1 if you have the `PtlEventKindStr' function. */
/* #undef HAVE_PTLEVENTKINDSTR */

/* Define to 1 if you have the `PtlGetJid' function. */
/* #undef HAVE_PTLGETJID */

/* Define to 1 if you have the `PtlNIFailStr' function. */
/* #undef HAVE_PTLNIFAILSTR */

/* Define to 1 if the system has the type `ptl_eq_handler_t'. */
/* #undef HAVE_PTL_EQ_HANDLER_T */

/* Define if you have PTL_NOACK_REQ. */
/* #undef HAVE_PTL_NOACK_REQ */

/* Define if you have PTL_NO_ACK_REQ. */
/* #undef HAVE_PTL_NO_ACK_REQ */

/* Define to 1 if the system has the type `ptl_time_t'. */
/* #undef HAVE_PTL_TIME_T */

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define if you have the Gemini. */
#define HAVE_UGNI 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Max message queue size for infiniband DataSpaces server */
#define INFINIBAND_MSG_QUEUE_SIZE 32

/* Name of package */
#define PACKAGE "dataspaces"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT ""

/* Define to the full name of this package. */
#define PACKAGE_NAME "dataspaces"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "dataspaces 1.4.0"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "dataspaces"

/* Define to the version of this package. */
#define PACKAGE_VERSION "1.4.0"

/* Define to the necessary symbol if this constant uses a non-standard name on
   your system. */
/* #undef PTHREAD_CREATE_JOINABLE */

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Version number of package */
#define VERSION "1.4.0"
