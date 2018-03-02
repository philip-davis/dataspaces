/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.ac by autoheader.  */

/* Maximum number of array dimension */
#define BBOX_MAX_NDIM 3

/* Size of rdma memory that can be used for data writes/reads in DIMES */
#define DIMES_RDMA_BUFFER_SIZE 64

/* Max number of concurrent rdma read operations that can be issued by DIMES
   in the data fetching process */
#define DIMES_RDMA_MAX_NUM_CONCURRENT_READ 4

/* uGNI-Aries is enabled */
/* #undef DS_HAVE_ARIES */

/* DIMES is enabled */
/* #undef DS_HAVE_DIMES */

/* "uGNI Dynamic Credentials is enabled" */
/* #undef DS_HAVE_DRC */

/* Define to dummy `main' function (if any) required to link to the Fortran
   libraries. */
/* #undef FC_DUMMY_MAIN */

/* Define if F77 and FC dummy `main' functions are identical. */
/* #undef FC_DUMMY_MAIN_EQ_F77 */

/* Define to a macro mangling the given C identifier (in lower and upper
   case), which must not contain underscores, for linking with Fortran. */
#define FC_FUNC(name,NAME) name ## _

/* As FC_FUNC, but for C identifiers containing underscores. */
#define FC_FUNC_(name,NAME) name ## _

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
/* #undef HAVE_GNI_PUB_H */

/* Define to 1 if you have <rdma/rdma_cma.h>. */
#define HAVE_IBCM_H 1

/* Define to 1 if you have <rdma/rdma_verbs.h>. */
#define HAVE_IBVERBS_H 1

/* Define if you have the Infiniband. */
#define HAVE_INFINIBAND 0

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the `m' library (-lm). */
#define HAVE_LIBM 1

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
/* #undef HAVE_PMI_H */

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

/* Define to 1 if you have the <sys/socket.h> header file. */
#define HAVE_SYS_SOCKET_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define if you have the TCP socket. */
#define HAVE_TCP_SOCKET 1

/* Define if you have the Gemini. */
/* #undef HAVE_UGNI */

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the <"rdmacred.h"> header file. */
/* #undef HAVE__RDMACRED_H_ */

/* IB Interface option */
#define IB_INTERFACE "ib0"

/* Max message queue size for infiniband DataSpaces server */
#define INFINIBAND_MSG_QUEUE_SIZE 32

/* Timeout for RDMA function in IB */
#define INFINIBAND_TIMEOUT 300

/* Define to 1 if your C compiler doesn't accept -c and -o together. */
/* #undef NO_MINUS_C_MINUS_O */

/* Name of package */
#define PACKAGE "dataspaces"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT ""

/* Define to the full name of this package. */
#define PACKAGE_NAME "dataspaces"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "dataspaces 1.6.5"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "dataspaces"

/* Define to the version of this package. */
#define PACKAGE_VERSION "1.6.5"

/* Define to the necessary symbol if this constant uses a non-standard name on
   your system. */
/* #undef PTHREAD_CREATE_JOINABLE */

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Version number of package */
#define VERSION "1.6.5"
