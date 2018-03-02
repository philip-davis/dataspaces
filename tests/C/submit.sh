#!/bin/bash

#sbatch /home1/yq47/code/dataspace/dataspaces_mt/tests/C/run_job.sh
bash /home1/yq47/code/dataspace/ds_hybrid_mt/tests/C/cleanall.sh
sbatch /home1/yq47/code/dataspace/ds_hybrid_mt/tests/C/cal_test.sh
echo "Submited!"
