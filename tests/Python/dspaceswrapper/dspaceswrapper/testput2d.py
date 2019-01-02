from mpi4py import MPI
import numpy as np
import dataspaces 
import ctypes
import os
import time

comm = MPI.COMM_WORLD
rank = comm.Get_rank()

# copy all conf.* file to current dir
serverdir = "/home1/zw241/dataspaces/tests/C"

confpath = serverdir+"/conf*"

copyCommand = "cp "+confpath+" ."

os.system(copyCommand)


# number of clients at clients end to join server
num_peers= 1
appid = 1


var_name = "ex1_sample_data" 
lock_name = "my_test_lock"


ds = dataspaces.dataspaceClient()

ds.dspaces_init(comm,num_peers,appid)


for ver in range (2):

    

    ds.dspaces_lock_on_write(lock_name)

    print("call lock")

    elemsize = ctypes.sizeof(ctypes.c_double)
    data = ([[1.1,2.2,3.3],[4.4,5.5,6.6]])

    dataarray = (ver+1)*np.asarray(data)

    lb = [0+ver*3,0+ver*3]

    ds.dspaces_put_data(var_name,ver,elemsize,lb,dataarray)
    ds.dspaces_unlock_on_write(lock_name)
    time.sleep(1)


ds.dspaces_wrapper_finalize()

MPI.Finalize()

