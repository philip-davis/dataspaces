qsub --env PAMI_CLIENTS=MPI,dart --env RUNJOB_MAPPING=mapfile-bgq -A xxx -t 10 -n 4 --mode c2 ./dataspaces_server 
