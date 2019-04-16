from mpi4py import MPI
import numpy as np
import dataspaces

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
appid = 1

var_name = "ex1_sample_data"

ds = dataspaces(appid, comm)
for ts in range(10):
    ds.lock_on_write("my_test_lock")
    data = np.random.randint(0, 99, size=3)
    print("Timestep %d: put data %s" % (ts, data))
    lb = np.array([rank,0])
    ds.put(var_name, ts, data, lb)
    ds.unlock_on_write(i"my_test_lock")
