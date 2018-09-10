from mpi4py import MPI
import example as ex

null = MPI.COMM_NULL
ex.sayhello(null)

comm = MPI.COMM_WORLD
ex.sayhello(comm)