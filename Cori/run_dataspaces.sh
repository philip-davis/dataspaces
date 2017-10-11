#!/bin/bash
#SBATCH -p TBD
#SBATCH -A YOUR_ALLOC
#SBATCH -N TBD
#SBATCH -t TBD
#SBATCH -C TBD

#Cori

###########################
# GET PARAMETER FROM CONF #
###########################

#Array to store input data from file
DATA_SIZE_X_ARR=
DATA_SIZE_Y_ARR=
DATA_SIZE_Z_ARR=

PPN_ARR=  #Processes per node

NUM_SERVER_ARR= 
NUM_WRITER_ARR=
NUM_READER_ARR=

#Global data size
DATA_SIZE_X=
DATA_SIZE_Y=
DATA_SIZE_Z=

NDIM=3
PPN=1 #Processes per node
NUM_SERVER= 
NUM_CLIENT=
NUM_WRITER=
NUM_READER=

NUM_SCALE=1
METHOD="DATASPACES"
ID_WRITER=1
ID_READER=2

#number of process in writer/reader each dimension
NUM_PROC_W_X=
NUM_PROC_W_Y=
NUM_PROC_W_Z=

NUM_PROC_R_X=
NUM_PROC_R_Y=
NUM_PROC_R_Z=

#block size per process in writer/reader in each dimension
BLK_SIZE_W_X=
BLK_SIZE_W_Y=
BLK_SIZE_W_Z=

BLK_SIZE_R_X=
BLK_SIZE_R_Y=
BLK_SIZE_R_Z=

SERVER_NODE=
WRITER_NODE=
READER_NODE=




#############################
# DETECT SYSTEM ENVIRONMENT #
#############################

PHYSICAL_CORES_PER_NODE_HASWELL=32
LOGICAL_CORES_PER_NODE_HASWELL=64
PHYSICAL_CORES_PER_NODE_KNL=68
LOGICAL_CORES_PER_NODE_KNL=272

###################################################
# From http://phodd.net/gnu-bc/bcfaq.html#bashlog #
###################################################
function log { 
	local x=$1 n=2 l=-1;
	if [ "$2" != "" ];
		then n=$x;x=$2;
	fi;

	while((x));
	do 
	let l+=1 x/=n;
	done;

	echo $l; }

#################################
# Ceiling function				#
#################################
function ceiling_divide {
  # Normal integer divide.
  ceiling_result=$(($1/$2))
  # If there is any remainder...
  if [ $(($1%$2)) -gt 0 ]; then
    # rount up to the next integer
    ceiling_result=$((ceiling_result + 1))
  fi
  echo "$ceiling_result"
}

#########################################################################
# Number of writer must equal to 2 ^ #									#
# Otherwise, will be arounded to nearest integer   						#
# First, do log to get the number of power and assign to val1			#
# And then, the number of writer can be parse into these three cases    #
#  |	         |														#
#  || 1 * 2 * 2 || ^ val2												#
#  || 1 * 1 * 2 ||														#
#  || 1 * 1 * 1 ||														#
#  |             |														#
#																		#
# val3 is used to choose which row										#
#########################################################################

function parse_config_writer {	#$1 is NUM_WRITER or NUM_READER
	local Tol_num_core=$1 #get argument
	local val1 val2 val3

	val1=$(log 2 $Tol_num_core);
	let "val2=$val1/3"
	let "val3=$val1%3"

	case "$val3" in
		"0" )
		NUM_PROC_W_X=$((2**$val2))
		NUM_PROC_W_Y=$((2**$val2))
		NUM_PROC_W_Z=$((2**$val2))
		;;

		"1" )
		NUM_PROC_W_X=$((2**$val2))
		NUM_PROC_W_Y=$((2**$val2))
		NUM_PROC_W_Z=$((2**$val2*2))
		;;

		"2" )
		NUM_PROC_W_X=$((2**$val2))
		NUM_PROC_W_Y=$((2**$val2*2))
		NUM_PROC_W_Z=$((2**$val2*2))
		;;
	
		"*" )
		echo "Error in parse_config_writer"
		;;
	esac

	#Calculate block size per process
	BLK_SIZE_W_X=$(($DATA_SIZE_X/NUM_PROC_W_X))
	BLK_SIZE_W_Y=$(($DATA_SIZE_Y/NUM_PROC_W_Y))
	BLK_SIZE_W_Z=$(($DATA_SIZE_Z/NUM_PROC_W_Z))

}



function parse_config_reader {	#$1 is NUM_WRITER or NUM_READER
	local Tol_num_core=$1 #get argument
	local val1 val2 val3

	val1=$(log 2 $Tol_num_core);
	let "val2=$val1/3"
	let "val3=$val1%3"

	case "$val3" in
		"0" )
		NUM_PROC_R_X=$((2**$val2))
		NUM_PROC_R_Y=$((2**$val2))
		NUM_PROC_R_Z=$((2**$val2))
		;;

		"1" )
		NUM_PROC_R_X=$((2**$val2))
		NUM_PROC_R_Y=$((2**$val2))
		NUM_PROC_R_Z=$((2**$val2*2))
		;;

		"2" )
		NUM_PROC_R_X=$((2**$val2))
		NUM_PROC_R_Y=$((2**$val2*2))
		NUM_PROC_R_Z=$((2**$val2*2))
		;;
	
		"*" )
		echo "Error in parse_config_reader"
		;;
	esac

	#Calculate block size per process
	BLK_SIZE_R_X=$(($DATA_SIZE_X/NUM_PROC_R_X))
	BLK_SIZE_R_Y=$(($DATA_SIZE_Y/NUM_PROC_R_Y))
	BLK_SIZE_R_Z=$(($DATA_SIZE_Z/NUM_PROC_R_Z))

}

function write_dataspaces_conf {

	rm -f $SCRIPT_DIR/conf $SCRIPT_DIR/cred $SCRIPT_DIR/dataspaces.conf

	echo "## Config file for DataSpaces
	ndim = $NDIM 
	dims = ${DATA_SIZE_X},${DATA_SIZE_Y},${DATA_SIZE_Z}

	max_versions = 1
	lock_type = 2
	" > $DATASPACES_DIR/dataspaces.conf
}


function srun_config {

######################################################################################################################
# Reference http://www.nersc.gov/users/computational-systems/cori/running-jobs/general-running-jobs-recommendations/ #
# -c Set the value as "number of of logical cores (CPUs) per MPI task"												 #
######################################################################################################################

if ! ((LOGICAL_CORES_PER_NODE_HASWELL%PPN)); then
	#HOLD FOR FLOOR
	let "C_SP=(PHYSICAL_CORES_PER_NODE_HASWELL/PPN)*2"
else
	let "C_SP=LOGICAL_CORES_PER_NODE_HASWELL/PPN"
fi
}

function read_input_to_array {
	#Get data size
	# if found keyword, then from the next line, get and process content, till match then next keyword
DATA_SIZE_X_ARR=($(awk '/DATA_SIZE_START:/{f=1;next}/DATA_SIZE_END/{f=0}f{split($0,ft,",");print ft[1]}' $CONFIG_FILE))
DATA_SIZE_Y_ARR=($(awk '/DATA_SIZE_START:/{f=1;next}/DATA_SIZE_END/{f=0}f{split($0,ft,",");print ft[2]}' $CONFIG_FILE))
DATA_SIZE_Z_ARR=($(awk '/DATA_SIZE_START:/{f=1;next}/DATA_SIZE_END/{f=0}f{split($0,ft,",");print ft[3]}' $CONFIG_FILE))

PPN_ARR=($(awk '/PPN_START:/{f=1;next}/PPN_END/{f=0}f{split($0,ft);print ft[1]}' $CONFIG_FILE))

NUM_SERVER_ARR=($(awk '/RATIO_START:/{f=1;next}/RATIO_END/{f=0}f{split($0,ft,":");print ft[2]}' $CONFIG_FILE))
NUM_WRITER_ARR=($(awk '/RATIO_START:/{f=1;next}/RATIO_END/{f=0}f{split($0,ft,":");print ft[1]}' $CONFIG_FILE))
NUM_READER_ARR=($(awk '/RATIO_START:/{f=1;next}/RATIO_END/{f=0}f{split($0,ft,":");print ft[3]}' $CONFIG_FILE))

#SYSTEM=$(awk -F '=' '/^SYSTEM/{gsub(/ /, "", $2);print $2}' "${CONFIG_FILE}") #gsub to remove space
#TIME_LIMIT=$(awk -F '=' '/^TIME_LIMIT/{gsub(/ /, "", $2);print $2}' "${CONFIG_FILE}")

}


#Run on Cori with srun
function run_dataspaces {
	NUM_CLIENT=$(($NUM_WRITER+$NUM_READER))
	SERVER_NODE=$(ceiling_divide $NUM_SERVER $PPN)
	WRITER_NODE=$(ceiling_divide $NUM_WRITER $PPN)
	READER_NODE=$(ceiling_divide $NUM_READER $PPN)


	#init
	parse_config_writer $NUM_WRITER
	parse_config_reader $NUM_READER
	write_dataspaces_conf
	srun_config

	## Server start
	srun -N $SERVER_NODE -n $NUM_SERVER -c $C_SP --cpu_bind=cores .$DATASPACES_DIR/dataspaces_server -s $NUM_SERVER -c $NUM_CLIENT $>> $SCRIPT_DIR/output/log.server &

	## WAIT FOR SERVER START TO COMPLETE
	sleep 1s
	while [ ! -f conf ]; do
    	sleep 1s
	done
	sleep 10s  # wait server to fill up the conf file

	## Writer start
	srun -N $WRITER_NODE -n $NUM_WRITER -c $C_SP --cpu_bind=cores .$DATASPACES_DIR/test_writer $METHOD $NUM_WRITER $NDIM $NUM_PROC_W_X $NUM_PROC_W_Y $NUM_PROC_W_Z $BLK_SIZE_W_X $BLK_SIZE_W_Y $BLK_SIZE_W_Z $NUM_TS $ID_WRITER &>> $SCRIPT_DIR/output/log.writer &

	## Reader start 
	srun -N $READER_NODE -n $NUM_READER -c $C_SP --cpu_bind=cores .$DATASPACES_DIR/test_reader $METHOD $NUM_READER $NDIM $NUM_PROC_R_X $NUM_PROC_R_Y $NUM_PROC_R_Z $BLK_SIZE_R_X $BLK_SIZE_R_Y $BLK_SIZE_R_Z $NUM_TS $ID_READER &>> $SCRIPT_DIR/output/log.reader &


	wait

	mkdir logdir_setup1_strong_scaling
	mv log.server log.writer log.reader ./logdir_setup1_strong_scaling

}





function dummy_run {

	NUM_CLIENT=$(($NUM_WRITER+$NUM_READER))
	SERVER_NODE=$(ceiling_divide $NUM_SERVER $PPN)
	WRITER_NODE=$(ceiling_divide $NUM_WRITER $PPN)
	READER_NODE=$(ceiling_divide $NUM_READER $PPN)

	
	echo ""
	echo ""
	echo "Inputs:"
	echo "Data size: $DATA_SIZE_X x $DATA_SIZE_Y x $DATA_SIZE_Z"
	echo "PPN: $PPN"
	echo "NUM_SERVER=$NUM_SERVER  NUM_WRITER=$NUM_WRITER  NUM_READER=$NUM_READER"
	echo ""
	echo "Execution configure:"


	#init
	parse_config_writer $NUM_WRITER
	parse_config_reader $NUM_READER
	write_dataspaces_conf
	srun_config

	echo "srun -N $SERVER_NODE -n $NUM_SERVER -c $C_SP --cpu_bind=cores ./dataspaces_server -s $NUM_SERVER -c $NUM_CLIENT"

	## Server start
	#srun -N $SERVER_NODE -n $NUM_SERVER -c $C_SP --cpu_bind=cores .$DATA_DIR/dataspaces_server -s $NUM_SERVER -c $NUM_CLIENT >& $DATA_DIR/log.server &
	echo "srun -N $WRITER_NODE -n $NUM_WRITER -c $C_SP --cpu_bind=cores ./test_writer $METHOD $NUM_WRITER $NDIM $NUM_PROC_W_X $NUM_PROC_W_Y $NUM_PROC_W_Z $BLK_SIZE_W_X $BLK_SIZE_W_Y $BLK_SIZE_W_Z $NUM_TS $ID_WRITER"

	## Writer start
	#srun -N $WRITER_NODE -n $NUM_WRITER -c $C_SP --cpu_bind=cores .$DATA_DIR//test_writer $METHOD $NUM_WRITER $NDIM $NUM_PROC_W_X $NUM_PROC_W_Y $NUM_PROC_W_Z $BLK_SIZE_W_X $BLK_SIZE_W_Y $BLK_SIZE_W_Z $NUM_TS $ID_WRITER >& $DATA_DIR/log.writer &

	echo "srun -N $READER_NODE -n $NUM_READER -c $C_SP --cpu_bind=cores ./test_reader $METHOD $NUM_READER $NDIM $NUM_PROC_R_X $NUM_PROC_R_Y $NUM_PROC_R_Z $BLK_SIZE_R_X $BLK_SIZE_R_Y $BLK_SIZE_R_Z $NUM_TS $ID_READER"
	## Reader start 
	#srun -N $READER_NODE -n $NUM_READER -c $C_SP --cpu_bind=cores .$DATA_DIR//test_reader $METHOD $NUM_READER $NDIM $NUM_PROC_R_X $NUM_PROC_R_Y $NUM_PROC_R_Z $BLK_SIZE_R_X $BLK_SIZE_R_Y $BLK_SIZE_R_Z $NUM_TS $ID_READER >& $DATA_DIR/log.reader &


}




function main {

#let's run simulation from here
	read_input_to_array

	for (( i=0; i<${#DATA_SIZE_X_ARR[@]}; i++)); #Data size
	do
		DATA_SIZE_X=${DATA_SIZE_X_ARR[i]}
		DATA_SIZE_Y=${DATA_SIZE_Y_ARR[i]}
		DATA_SIZE_Z=${DATA_SIZE_Z_ARR[i]}


		for (( j=0; j<${#PPN_ARR[@]}; j++)); #Process per node
		do
			PPN=${PPN_ARR[j]}

			for (( k=0; k<${#NUM_SERVER_ARR[@]}; k++)); #Ratio
			do
				NUM_SERVER=${NUM_SERVER_ARR[k]}
				NUM_WRITER=${NUM_WRITER_ARR[k]}
				NUM_READER=${NUM_READER_ARR[k]}


				#Run DataSpaces Benchmark
				echo "Data Size: $DATA_SIZE_X, $DATA_SIZE_Y, $DATA_SIZE_Z" &>> $SCRIPT_DIR/output/log.writer
				echo "Writer:Server:Reader: $NUM_WRITER:$NUM_SERVER:$NUM_READER" &>> $SCRIPT_DIR/output/log.writer
				echo "Data Size: $DATA_SIZE_X, $DATA_SIZE_Y, $DATA_SIZE_Z" &>> $SCRIPT_DIR/output/log.reader
				echo "Writer:Server:Reader: $NUM_WRITER:$NUM_SERVER:$NUM_READER" &>> $SCRIPT_DIR/output/log.reader

				run_dataspaces
				#dummy_run

			done
		done
	done

}

##################
# Run Test Suite #
##################
main















