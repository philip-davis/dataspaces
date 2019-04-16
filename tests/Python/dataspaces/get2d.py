from mpi4py import MPI
import numpy as np
import dataspaces.dataspaceClient as dataspaces

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
appid = 2

var_name = "ex1_sample_data"

ds = dataspaces.dataspaceClient(appid, comm)

for ts in range(10):
    ds.lock_on_read("my_test_lock")
    lb = [rank,0]
    ub = [rank, 3]
    data = ds.get(var_name, ts, lb, ub)
    print("Timestep %d: get data %s" % (ts, data))
    ds.unlock_on_read("my_test_lock")