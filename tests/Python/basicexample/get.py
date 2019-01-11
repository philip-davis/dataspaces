from mpi4py import MPI
import numpy as np
import dataspaces

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
appid = 2

ds = dataspaces(appid, comm)

var_name = "ex1_sample_data"

for ts in range(10):
    ds.lock_on_read("my_test_lock")
    lb = np.array([rank,0])
    ub = np.array([rank, 3])
    data = ds.get(var_name, ts, lb, ub)
    print("Timestep %d: get data %s" % (ts, data))
    ds.unlock_on_read("my_test_lock")
