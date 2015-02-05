dnl @synopsis AC_DIMES_CONF([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl This macro handles configure options --with-gni-ptag and --with-gni-cookie
dnl For configuring DIMES.
dnl
dnl
AC_DEFUN([AC_DIMES_CONF],[

have_dimes_rdma_buffer_size=no
have_dimes_rdma_max_num_concurrent_read=no
cond_dimes_conf=yes

AC_ARG_WITH(dimes-rdma-buffer-size,
	[AS_HELP_STRING([--with-dimes-rdma-buffer-size=integer value for megabytes],
		[This option is used to specify the size of rdma memory that can be used for data writes/reads in DIMES. Must be used with --enable-dimes. Default value is set as 64MB.])],
	[have_dimes_rdma_buffer_size=yes;
	 DIMES_RDMA_BUFFER_SIZE="$withval";])

AC_ARG_WITH(dimes-rdma-max-num_concurrent-read,
	[AS_HELP_STRING([--with-dimes-rdma-max-num-concurrent-read=integer value],
		[This option is used to specify the max number of concurrent RDMA read operations DIMES can issue for data fetching process. Must be used with --enable-dimes. Default value is set as 3.])],
	[have_dimes_rdma_max_num_concurrent_read=yes;
	 DIMES_RDMA_MAX_NUM_CONCURRENT_READ="$withval";])

if test "x$have_dimes_rdma_buffer_size" = "xyes"; then
    AC_DEFINE_UNQUOTED(DIMES_RDMA_BUFFER_SIZE,$DIMES_RDMA_BUFFER_SIZE,[Size of rdma memory that can be used for data writes/reads in DIMES])
else
    AC_DEFINE_UNQUOTED(DIMES_RDMA_BUFFER_SIZE,64,[Size of rdma memory that can be used for data writes/reads in DIMES])
fi

if test "x$have_dimes_rdma_max_num_concurrent_read" = "xyes"; then
    AC_DEFINE_UNQUOTED(DIMES_RDMA_MAX_NUM_CONCURRENT_READ,$DIMES_RDMA_MAX_NUM_CONCURRENT_READ,[Max number of concurrent rdma read operations that can be issued by DIMES in the data fetching process])
else
    AC_DEFINE_UNQUOTED(DIMES_RDMA_MAX_NUM_CONCURRENT_READ,3,[Max number of concurrent rdma read operations that can be issued by DIMES in the data fetching process])
fi

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test "x$cond_dimes_conf" = "xyes"; then
        ifelse([$1],,,[$1])
        :
else
        $2
        :
fi
])

