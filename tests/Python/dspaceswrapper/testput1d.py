from mpi4py import MPI
import numpy as np
import dspaceswrapper.dataspaces as dataspaces 
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

ds = dataspaces.dataspaceClient(appid,comm)

for ver in range (2):

    ds.lock_on_write(lock_name)
    
    data = [1.1*(ver+1),2.2*(ver+1),3.3*(ver+1)]
    lb = [0+ver*3]

    print ("put data")
    print (data)
    
    ds.put(var_name,ver,lb,data)
    ds.unlock_on_write(lock_name)
    time.sleep(1)



ds.finalize()
MPI.Finalize()

