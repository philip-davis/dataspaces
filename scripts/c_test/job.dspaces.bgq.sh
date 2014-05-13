qsub --env BG_COREDUMPONEXIT=1 --env PAMI_CLIENTS=MPI,dart --env RUNJOB_MAPPING=mapfile-bgq -A SDAV -t 10 -n 4 --mode c2 ./dataspaces_server 
