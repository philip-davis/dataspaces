dnl @synopsis AC_CONFIGURE_OPTIONS([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
dnl
dnl This macro handles configure options 
dnl For configuring DataSpaces/DIMES.
dnl
dnl
AC_DEFUN([AC_CONFIGURE_OPTIONS],[

have_max_num_array_dimension=no
have_dimes_rdma_buffer_size=no
have_dimes_rdma_max_num_concurrent_read=no
cond_configure_options=yes
have_build_for_stampede=no
have_infiniband_msg_queue_size=no

AC_ARG_WITH(max-num-array-dimension,
	[AS_HELP_STRING([--with-max-num-array-dimension=integer value],
		[This option is used to configure the maximum number of array dimension that can be supported in DataSpaces/DIMES. Default value is set as 3. Note: the value can not be set larger than 10, or smaller than 3.])],
	[have_max_num_array_dimension=yes;
	 MAX_NUM_ARRAY_DIMENSION="$withval";])

AC_ARG_WITH(dimes-rdma-buffer-size,
	[AS_HELP_STRING([--with-dimes-rdma-buffer-size=integer value for megabytes],
		[This option specifies the maximum amount of RDMA memory buffer that can be used by DIMES in each application process. DIMES RDMA memory buffer is used to locally cache the data written by dimes_put, and is also used by dimes_get to fetch data in remote memory buffer. Must be used with --enable-dimes option. Default value is set as 64MB.])],
	[have_dimes_rdma_buffer_size=yes;
	 DIMES_RDMA_BUFFER_SIZE="$withval";])

AC_ARG_WITH(dimes-rdma-max-num-concurrent-read,
	[AS_HELP_STRING([--with-dimes-rdma-max-num-concurrent-read=integer value],
		[This option is used to specify the max number of concurrent RDMA GET
network operations DIMES can issue for data fetching process. Must be used with --enable-dimes. Default value is set as 4.])],
	[have_dimes_rdma_max_num_concurrent_read=yes;
	 DIMES_RDMA_MAX_NUM_CONCURRENT_READ="$withval";])

AC_ARG_WITH(ib-interface,
        [AS_HELP_STRING([--with-ib-interface=string value],
                [This option is used to configure DataSpaces for Stampede cluster. Users must Set this value to 1 while configure. The default vaule is 0.])],
        [have_ib_interface=yes;
         IB_INTERFACE="$withval";])

AC_ARG_WITH(infiniband-msg-queue-size,                                                                                                [AS_HELP_STRING([--with-infiniband-msg-queue-size=integer value],
                [This option is used to specify the size of messaging queue for DataSpaces servers in infiniband
        Default value is set as 32.])],
        [have_infiniband_msg_queue_size=yes;
         INFINIBAND_MSG_QUEUE_SIZE="$withval";])


AC_ARG_WITH(infiniband-timeout,                                                                                                [AS_HELP_STRING([--with-infiniband-timeout=integer value],
                [This option is used to specify the timeout for rdma_resolve_addr and rmda_resolve_route in infiniband
        Default value is set as 300.])],
        [have_infiniband_timeout=yes;
         INFINIBAND_TIMEOUT="$withval";])

AC_ARG_ENABLE(ib-cpp,[AS_HELP_STRING([--enable-ib-cpp], [Enable ib-cpp] ) ],
              [AM_CONDITIONAL(ENABLE_IB_CPP, true) ], [AM_CONDITIONAL(ENABLE_IB_CPP, false) ] )
AC_MSG_CHECKING([whether to enable ib-cpp])
AS_IF([test "x$enableval" = "xyes" ], AC_MSG_RESULT([yes]), AC_MSG_RESULT([no]))

if test "x$have_max_num_array_dimension" = "xyes"; then
    AC_DEFINE_UNQUOTED(BBOX_MAX_NDIM,$MAX_NUM_ARRAY_DIMENSION,[Maximum number of array dimension])
else
    AC_DEFINE_UNQUOTED(BBOX_MAX_NDIM,3,[Maximum number of array dimension])
fi

if test "x$have_dimes_rdma_buffer_size" = "xyes"; then
    AC_DEFINE_UNQUOTED(DIMES_RDMA_BUFFER_SIZE,$DIMES_RDMA_BUFFER_SIZE,[Size of rdma memory that can be used for data writes/reads in DIMES])
else
    AC_DEFINE_UNQUOTED(DIMES_RDMA_BUFFER_SIZE,64,[Size of rdma memory that can be used for data writes/reads in DIMES])
fi

if test "x$have_ib_interface" = "xyes"; then
    AC_DEFINE_UNQUOTED(IB_INTERFACE,"$IB_INTERFACE",[IB Interface option])
else
    AC_DEFINE_UNQUOTED(IB_INTERFACE,"ib0",[IB Interface option])
fi

if test "x$have_infiniband_msg_queue_size" = "xyes"; then
    AC_DEFINE_UNQUOTED(INFINIBAND_MSG_QUEUE_SIZE,$INFINIBAND_MSG_QUEUE_SIZE,[Max message queue size for infiniband DataSpaces server])
else
    AC_DEFINE_UNQUOTED(INFINIBAND_MSG_QUEUE_SIZE,32,[Max message queue size for infiniband DataSpaces server])
fi

if test "x$have_infiniband_timeout" = "xyes"; then
    AC_DEFINE_UNQUOTED(INFINIBAND_TIMEOUT,$INFINIBAND_TIMEOUT,[Timeout for RDMA function in IB])
else
    AC_DEFINE_UNQUOTED(INFINIBAND_TIMEOUT,300,[Timeout for RDMA function in IB])
fi


if test "x$have_dimes_rdma_max_num_concurrent_read" = "xyes"; then
    AC_DEFINE_UNQUOTED(DIMES_RDMA_MAX_NUM_CONCURRENT_READ,$DIMES_RDMA_MAX_NUM_CONCURRENT_READ,[Max number of concurrent rdma read operations that can be issued by DIMES in the data fetching process])
else
    AC_DEFINE_UNQUOTED(DIMES_RDMA_MAX_NUM_CONCURRENT_READ,4,[Max number of concurrent rdma read operations that can be issued by DIMES in the data fetching process])
fi

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test "x$cond_configure_options" = "xyes"; then
        ifelse([$1],,,[$1])
        :
else
        $2
        :
fi
])

