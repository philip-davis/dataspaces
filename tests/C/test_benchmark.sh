#!/bin/bash

rm -rf *.out
while getopts n:f:r:p: option
do
        case "${option}"
        in
                n) num_of_apps=${OPTARG};;
                f) ior_file=${OPTARG};;
                r) repetitions=${OPTARG};;
				p) procs=$OPTARG;;
        esac
done

awk -F '\t' 'BEGIN{OFS="\t";} {print $1,$3,$4,$5,$6,$7,$8,$9,$10> "file"$2".out";}' $ior_file
x=$((num_of_apps-1))
for (( i = 0; i < $num_of_apps; i++ )); do
	mpirun -np $procs ./issue_reqs_3d "file"$i".out" $repetitions $i >& "app"$i".out" &

done

#mpirun -np $procs ./issue_reqs "file0.out" $repetitions 0 >& app0.out &
#mpirun -np $procs ./issue_reqs "file1.out" $repetitions 1 >& app1.out &
wait
rm -rf file*.out

echo 'done'
