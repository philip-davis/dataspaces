from mpi4py import MPI
import numpy as np
import dataspaces.dataspaceClient as dataspaces

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
appid = 1

var_name = "ex1_sample_data"

ds = dataspaces.dataspaceClient(appid, comm)
for ts in range(10):
    ds.lock_on_write("my_test_lock")
    data = np.random.randint(0, 99, size=(1,3))
    print("Timestep %d: put data %s" % (ts, data))
    lb = [rank,0]
    ds.put(var_name, ts, lb, data)
    ds.unlock_on_write("my_test_lock")