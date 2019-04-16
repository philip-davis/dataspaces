from mpi4py import MPI
import numpy as np
import dataspaces.dataspaceClient as dataspaces
import ctypes
import os
import time

comm = MPI.COMM_WORLD
rank = comm.Get_rank()

# copy all conf.* file to current dir (if using ib)
serverdir = "/home1/zw241/dataspaces/tests/C"

confpath = serverdir+"/conf*"

copyCommand = "cp "+confpath+" ."

os.system(copyCommand)

# number of clients at clients end to join server
num_peers= 1
appid = 2


var_name = "ex1_sample_data" 
lock_name = "my_test_lock"

ds = dataspaces.dataspaceClient(appid,comm)

for ver in range (2):

    ds.lock_on_read(lock_name)

    lb = [0+ver*3,0+ver*3]
    #ub,ndim = ds.getUpBound(lb,data)
    ub = [0+ver*3+1,0+ver*3+2]

    getdata=ds.get(var_name,ver,lb,ub)
    
    print ("get data")
    print (getdata)

    ds.unlock_on_read(lock_name)

    time.sleep(1)


ds.finalize()
MPI.Finalize()

