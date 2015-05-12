#ifndef __DART_H_
#define __DART_H_

#include "config.h"

#if HAVE_UGNI

#include "gni/dart_rpc_gni.h"
#include "gni/dc_base_gni.h"
#include "gni/ds_base_gni.h"
#include "gni/dart_rdma_gni.h"

#elif HAVE_PORTALS

#include "portals/dart_rpc_portals.h"
#include "portals/dc_base_portals.h"
#include "portals/ds_base_portals.h"

#elif HAVE_INFINIBAND

#include "ib/dart_rpc_ib.h"
#include "ib/dc_base_ib.h"
#include "ib/ds_base_ib.h"
#include "ib/dart_rdma_ib.h"

#elif HAVE_DCMF

#include "dcmf/dart_rpc_dcmf.h"
#include "dcmf/dc_base_dcmf.h"
#include "dcmf/ds_base_dcmf.h"
#include "dcmf/dart_rdma_dcmf.h"

#elif HAVE_PAMI

#include "pami/dart_rpc_pami.h"
#include "pami/dc_base_pami.h"
#include "pami/ds_base_pami.h"
#include "pami/dart_rdma_pami.h"

#elif HAVE_TCP_SOCKET

#include "tcp/dart_rpc_tcp.h"
#include "tcp/dc_base_tcp.h"
#include "tcp/ds_base_tcp.h"

#endif

#endif
