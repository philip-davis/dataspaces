#!/bin/bash
#SBATCH -p regular
#SBATCH -A ALLOC_GOES_HERE
#SBATCH -N 20
#SBATCH -t 01:30:00
#SBATCH -C haswell

# Clean Up
rm -f conf cred *.log
rm -f dataspaces.conf
rm -f srv.lck

###############################
# CORI PARAMETERS	      #
###############################	
# THERE ARE 128GBs DRAM PER NODE
PHYSICAL_CORES_PER_NODE_HASWELL=32
LOGICAL_CORES_PER_NODE_HASWELL=64
PHYSICAL_CORES_PER_NODE_KNL=68
LOGICAL_CORES_PER_NODE_KNL=272

###############################
# CONFIGURATION PARAMETERS    #
###############################	

APP1_PROC=512
APP1_NPX=32
APP1_NPY=16
#APP1_NPZ=1
APP1_ID=1
let "APP1_NODES=$APP1_PROC/$PHYSICAL_CORES_PER_NODE_HASWELL"

# Number of staging servers
NUM_SP=8
NUM_SP_PER_NODE=4
let "SP_NODES=NUM_SP/NUM_SP_PER_NODE"

APP2_PROC=32
APP2_NPX=8
APP2_NPY=4
#APP2_NPZ=1
APP2_ID=2
let "APP2_NODES=$APP2_PROC/$PHYSICAL_CORES_PER_NODE_HASWELL"

METHOD="DATASPACES"
NUM_TS=100

###############################
# GLOBAL DIMENSIONS OF DS     #
###############################	
# Key Assumption: Data is of type double in C = 4 bytes

# Global dimension (weak scaling) for dataspaces.conf
NDIM_WS=2
DIMX_WS=32768
DIMY_WS=16384
#DIMZ_WS=1
WS_CONF_DIMS="${DIMX_WS},${DIMY_WS}"

# Global dimension (strong scaling) for dataspaces.conf
NDIM_SS=2
DIMX_SS=32768
DIMY_SS=16384
#DIMZ_SS=1
SS_CONF_DIMS="${DIMX_SS},${DIMY_SS}"

###############################
# SRUN PARAMETERS	      #
###############################	

if ! ((LOGICAL_CORES_PER_NODE_HASWELL%NUM_SP_PER_NODE)); then
	#HOLD FOR FLOOR
	let "C_SP=(PHYSICAL_CORES_PER_NODE_HASWELL/NUM_SP_PER_NODE)*2"
else
	let "C_SP=LOGICAL_CORES_PER_NODE_HASWELL/NUM_SP_PER_NODE"
fi

###############################
# SETUP 1		      #
###############################	

## WEAK SCALING ##
rm -f conf cred dataspaces.conf

echo "## Config file for DataSpaces
ndim = $NDIM_WS 
dims = $WS_CONF_DIMS

max_versions = 1
lock_type = 2
" > dataspaces.conf

### SERVER START
let "NUM_CP=APP1_PROC+APP2_PROC"
echo "srun -N $SP_NODES -n $NUM_SP -c $C_SP --cpu_bind=cores ./dataspaces_server -s $NUM_SP -c $NUM_CP"
srun -N $SP_NODES -n $NUM_SP -c $C_SP --cpu_bind=cores ./dataspaces_server -s $NUM_SP -c $NUM_CP >& log.server &

## WAIT FOR SERVER START TO COMPLETE
sleep 1s
while [ ! -f conf ]; do
    sleep 1s
done
sleep 10s  # wait server to fill up the conf file

### WRITER START
let "APP1_SPX=DIMX_WS/APP1_NPX"
let "APP1_SPY=DIMY_WS/APP1_NPY"
#let "APP1_SPZ=DIMZ_WS/APP1_NPZ"
echo "srun -N $APP1_NODES -n $APP1_PROC ./test_writer $METHOD $APP1_PROC $NDIM_WS $APP1_NPX $APP1_NPY $APP1_SPX $APP1_SPY $NUM_TS $APP1_ID"
srun -N $APP1_NODES -n $APP1_PROC ./test_writer $METHOD $APP1_PROC $NDIM_WS $APP1_NPX $APP1_NPY $APP1_SPX $APP1_SPY $NUM_TS $APP1_ID >& log.writer &

## READER START
let "APP2_SPX=DIMX_WS/APP2_NPX"
let "APP2_SPY=DIMY_WS/APP2_NPY"
#let "APP2_SPZ=DIMZ_WS/APP2_NPZ"
echo "srun -N $APP2_NODES -n $APP2_PROC ./test_reader $METHOD $APP2_PROC $NDIM_WS $APP2_NPX $APP2_NPY $APP2_SPX $APP2_SPY $NUM_TS $APP2_ID"
srun -N $APP2_NODES -n $APP2_PROC ./test_reader $METHOD $APP2_PROC $NDIM_WS $APP2_NPX $APP2_NPY $APP2_SPX $APP2_SPY $NUM_TS $APP2_ID >& log.reader &

wait

mkdir logdir_setup1_weak_scaling
mv log.server log.writer log.reader ./logdir_setup1_weak_scaling

## STRONG SCALING ##

rm -f conf cred dataspaces.conf

echo "## Config file for DataSpaces
ndim = $NDIM_SS 
dims = $SS_CONF_DIMS

max_versions = 1
lock_type = 2
" > dataspaces.conf

### SERVER START
let "NUM_CP=APP1_PROC+APP2_PROC"
echo "srun -N $SP_NODES -n $NUM_SP -c $C_SP --cpu_bind=cores ./dataspaces_server -s $NUM_SP -c $NUM_CP"
srun -N $SP_NODES -n $NUM_SP -c $C_SP --cpu_bind=cores ./dataspaces_server -s $NUM_SP -c $NUM_CP >& log.server &

## WAIT FOR SERVER START TO COMPLETE
sleep 1s
while [ ! -f conf ]; do
    sleep 1s
done
sleep 10s  # wait server to fill up the conf file

### WRITER START
let "APP1_SPX=DIMX_SS/APP1_NPX"
let "APP1_SPY=DIMY_SS/APP1_NPY"
#let "APP1_SPZ=DIMZ_SS/APP1_NPZ"
echo "srun -N $APP1_NODES -n $APP1_PROC ./test_writer $METHOD $APP1_PROC $NDIM_SS $APP1_NPX $APP1_NPY $APP1_SPX $APP1_SPY $NUM_TS $APP1_ID"
srun -N $APP1_NODES -n $APP1_PROC ./test_writer $METHOD $APP1_PROC $NDIM_SS $APP1_NPX $APP1_NPY $APP1_SPX $APP1_SPY $NUM_TS $APP1_ID >& log.writer &

## READER START
let "APP2_SPX=DIMX_SS/APP2_NPX"
let "APP2_SPY=DIMY_SS/APP2_NPY"
#let "APP2_SPZ=DIMZ_SS/APP2_NPZ"
echo "srun -N $APP2_NODES -n $APP2_PROC ./test_reader $METHOD $APP2_PROC $NDIM_SS $APP2_NPX $APP2_NPY $APP2_SPX $APP2_SPY $NUM_TS $APP2_ID"
srun -N $APP2_NODES -n $APP2_PROC ./test_reader $METHOD $APP2_PROC $NDIM_SS $APP2_NPX $APP2_NPY $APP2_SPX $APP2_SPY $NUM_TS $APP2_ID >& log.reader &

wait

mkdir logdir_setup1_strong_scaling
mv log.server log.writer log.reader ./logdir_setup1_strong_scaling

###############################
# SETUP 2		      #
###############################	

## WEAK SCALING ##
rm -f conf cred dataspaces.conf

echo "## Config file for DataSpaces
ndim = $NDIM_WS 
dims = $WS_CONF_DIMS

max_versions = 1
lock_type = 2
" > dataspaces.conf

### SERVER START
let "NUM_CP=APP1_PROC+APP2_PROC"
echo "srun -N $SP_NODES -n $NUM_SP -c $C_SP --cpu_bind=cores ./dataspaces_server -s $NUM_SP -c $NUM_CP"
srun -N $SP_NODES -n $NUM_SP -c $C_SP --cpu_bind=cores ./dataspaces_server -s $NUM_SP -c $NUM_CP >& log.server &

## WAIT FOR SERVER START TO COMPLETE
sleep 1s
while [ ! -f conf ]; do
    sleep 1s
done
sleep 10s  # wait server to fill up the conf file

## READER START
let "APP1_SPX=DIMX_WS/APP1_NPX"
let "APP1_SPY=DIMY_WS/APP1_NPY"
echo "srun -N $APP1_NODES -n $APP1_PROC ./test_reader $METHOD $APP1_PROC $NDIM_WS $APP1_NPX $APP1_NPY $APP1_SPX $APP1_SPY $NUM_TS $APP1_ID"
srun -N $APP1_NODES -n $APP1_PROC ./test_reader $METHOD $APP1_PROC $NDIM_WS $APP1_NPX $APP1_NPY $APP1_SPX $APP1_SPY $NUM_TS $APP1_ID >& log.reader &

## WRITER START
let "APP2_SPX=DIMX_WS/APP2_NPX"
let "APP2_SPY=DIMY_WS/APP2_NPY"
#let "APP2_SPZ=DIMZ_WS/APP2_NPZ"
echo "srun -N $APP2_NODES -n $APP2_PROC ./test_writer $METHOD $APP2_PROC $NDIM_WS $APP2_NPX $APP2_NPY $APP2_SPX $APP2_SPY $NUM_TS $APP2_ID"
srun -N $APP2_NODES -n $APP2_PROC ./test_writer $METHOD $APP2_PROC $NDIM_WS $APP2_NPX $APP2_NPY $APP2_SPX $APP2_SPY $NUM_TS $APP2_ID >& log.writer &

wait

mkdir logdir_setup2_weak_scaling
mv log.server log.writer log.reader ./logdir_setup2_weak_scaling

## STRONG SCALING ##

rm -f conf cred dataspaces.conf

echo "## Config file for DataSpaces
ndim = $NDIM_SS 
dims = $SS_CONF_DIMS

max_versions = 1
lock_type = 2
" > dataspaces.conf

### SERVER START
let "NUM_CP=APP1_PROC+APP2_PROC"
echo "srun -N $SP_NODES -n $NUM_SP -c $C_SP --cpu_bind=cores ./dataspaces_server -s $NUM_SP -c $NUM_CP"
srun -N $SP_NODES -n $NUM_SP -c $C_SP --cpu_bind=cores ./dataspaces_server -s $NUM_SP -c $NUM_CP >& log.server &

## WAIT FOR SERVER START TO COMPLETE
sleep 1s
while [ ! -f conf ]; do
    sleep 1s
done
sleep 10s  # wait server to fill up the conf file

### READER START
let "APP1_SPX=DIMX_SS/APP1_NPX"
let "APP1_SPY=DIMY_SS/APP1_NPY"
#let "APP1_SPZ=DIMZ_SS/APP1_NPZ"
echo "srun -N $APP1_NODES -n $APP1_PROC ./test_reader $METHOD $APP1_PROC $NDIM_SS $APP1_NPX $APP1_NPY $APP1_SPX $APP1_SPY $NUM_TS $APP1_ID"
srun -N $APP1_NODES -n $APP1_PROC ./test_reader $METHOD $APP1_PROC $NDIM_SS $APP1_NPX $APP1_NPY $APP1_SPX $APP1_SPY $NUM_TS $APP1_ID >& log.reader &

## WRITER START
let "APP2_SPX=DIMX_SS/APP2_NPX"
let "APP2_SPY=DIMY_SS/APP2_NPY"
#let "APP2_SPZ=DIMZ_SS/APP2_NPZ"
echo "srun -N $APP2_NODES -n $APP2_PROC ./test_writer $METHOD $APP2_PROC $NDIM_SS $APP2_NPX $APP2_NPY $APP2_SPX $APP2_SPY $NUM_TS $APP2_ID"
srun -N $APP2_NODES -n $APP2_PROC ./test_writer $METHOD $APP2_PROC $NDIM_SS $APP2_NPX $APP2_NPY $APP2_SPX $APP2_SPY $NUM_TS $APP2_ID >& log.writer &

wait

mkdir logdir_setup2_strong_scaling
mv log.server log.writer log.reader ./logdir_setup2_strong_scaling
