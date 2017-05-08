#PBS -N S3D
#PBS -A CMB003dns
#PBS -j oe
#PBS -q batch
#PBS -l walltime=3:00:00
#PBS -l size=8

ID=`echo $PBS_JOBID | cut -f 1 -d .`

export MPICH_PTL_UNEX_EVENTS=200000
export MPICH_MSGS_PER_PROC=100000

export MPICH_SMP_SINGLE_COPY_OFF=1
export MPICH_ENV_DISPLAY=1
export MPICH_VERSION_DISPLAY=1

cd $PBS_O_WORKDIR

cp -f ../input/choose_a_s3d.in_here ../input/s3d.in 

aprun -n $PBS_NNODES ./s3d.x 

