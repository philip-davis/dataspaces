#!/bin/bash
sudo modprobe rdma_cm
sudo modprobe ib_uverbs
sudo modprobe rdma_ucm
sudo insmod /lib/modules/3.13.0-43-generic/extra/siw.ko
sudo lsmod 
export OMPI_MCA_btl_openib_warn_no_device_params_found=0
export OMPI_MCA_orte_base_help_aggregate=0
export OMPI_MCA_btl_openib_cpc_include=rdmacm
