# Script to launch multiple instances of the
# comprehensive_parallel_merge_tree_test. This script takes in input arguments
# that are passed to the test application
#
# Created on: July 13, 2015
# author: landge1

num_args=$#

if [ $num_args -lt 14 ]; then
  echo "Too few arguments"
  echo "Usage: multi_comprehensive_test.sh <mpi_exec> <num_processes> <num_experiments> 
  <input_file> <data_dim_x> <data_dim_y> <data_dim_z> 
  <min_test_block_size_x> <min_test_block_size_y> <min_test_block_size_z> 
  <num_proc_x> <num_proc_y> <num_proc_z> <fanin> <threshold(optional)>"
  exit 
fi

MPI_EXEC=$1
num_processes=$2
num_experiments=$3
input_file=$4
data_dim_x=$5
data_dim_y=$6
data_dim_z=$7
min_test_block_size_x=$8
min_test_block_size_y=$9
min_test_block_size_z=${10}
num_proc_x=${11}
num_proc_y=${12}
num_proc_z=${13}
fanin=${14}

if [ $num_args == 15 ]; then
  threshold=${15}
  for ((i=1; i<=$num_experiments; i++))
  do
    $MPI_EXEC -n $num_processes ./comprehensive_parallel_merge_tree_test $input_file $data_dim_x $data_dim_y $data_dim_z $min_test_block_size_x $min_test_block_size_y $min_test_block_size_z $num_proc_x $num_proc_y $num_proc_z $fanin $threshold 2> err.log
    if [[ -s err.log ]] ; then
      cat err.log
      echo "stderr written to err.log"
      echo "!!! TEST FAILED !!!"
      exit
    fi
  done
else 
  for ((i=1; i<=$num_experiments; i++))
  do
    $MPI_EXEC -n $num_processes ./comprehensive_parallel_merge_tree_test $input_file $data_dim_x $data_dim_y $data_dim_z $min_test_block_size_x $min_test_block_size_y $min_test_block_size_z $num_proc_x $num_proc_y $num_proc_z $fanin
  done
fi

rm -f err.log
echo "All test PASSED!!"


