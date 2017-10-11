#!/bin/bash
#SBATCH -J DS_test_suite
#SBATCH -o DS_test_suite.%J.stdout
#SBATCH -e DS_test_suite.%J.stderr
#SBATCH -p development
#SBATCH -N 4
#SBATCH -n 30
#SBATCH -t 00:10:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=qybo123@gmail.com

SCRIPT_DIR=/home1/yq47/code/dataspace/test_suite
DATASPACES_DIR=/home1/yq47/code/dataspace/test_suite/DataSpaces

#Configure value files
CONFIG_FILE="$SCRIPT_DIR/input/configure"

SYSTEM=
SYS_NUM=
TIMEOUT_FLAG=
TIME_LIMIT=10

#function print_options {

#echo "Select the number of your system from following options:
#1. Summit
#2. Cori
#3. Theta
#4. Stampede2
#5. Caliburn
#6. Quit"

#echo "Please type the sytem number, followed by [ENTER]: "
#read SYS_NUM

#if [ $SYS_NUM -gt 6 ] || [ $SYS_NUM -lt 1 ]
#	then
#	echo "Wrong number and exit"
#	exit 1
#fi

#echo "Please type the execution time limit in seconds, followed by [ENTER]: "
#read TIME_LIMIT

#}

function read_system_config {
	SYSTEM=$(awk -F '=' '/^SYSTEM/{gsub(/ /, "", $2);print $2}' "${CONFIG_FILE}")
	TIME_LIMIT=$(awk -F '=' '/^TIME_LIMIT/{gsub(/ /, "", $2);print $2}' "${CONFIG_FILE}")
	NUM_TS=$(awk -F '=' '/^NUM_TS/{gsub(/ /, "", $2);print $2}' "${CONFIG_FILE}")
}

function main {

	#print_options
	#Export two directory for other scripts to use
	export SCRIPT_DIR
	export DATASPACES_DIR 
	export CONFIG_FILE
	export NUM_TS

	read_system_config
	
	case $SYSTEM in    #Tolerant lower case
		"Summit" | "summit" )
		if [ $SYSTEM == "summit" ]
			then 
			SYSTEM="Summit"
		fi
		timeout $TIME_LIMIT bash $SCRIPT_DIR/$SYSTEM/run_dataspaces.sh
		;;

		"Cori" | "cori" )
		if [ $SYSTEM == "cori" ]
			then 
			SYSTEM="Cori"
		fi
		timeout $TIME_LIMIT bash $SCRIPT_DIR/$SYSTEM/run_dataspaces.sh
		;;

		"Theta" | "theta" )
		if [ $SYSTEM == "theta" ]
			then 
			SYSTEM="Theta"
		fi
		timeout $TIME_LIMIT bash $SCRIPT_DIR/$SYSTEM/run_dataspaces.sh
		;;

		"Stampede2" | "stampede2" )
		if [ $SYSTEM == "stampede2" ]
			then 
			SYSTEM="Stampede2"
		fi
		timeout $TIME_LIMIT bash $SCRIPT_DIR/$SYSTEM/run_dataspaces.sh
		;;

		"Caliburn" | "caliburn" )
		if [ $SYSTEM == "caliburn" ]
			then 
			SYSTEM="Caliburn"
		fi
		timeout $TIME_LIMIT bash $SCRIPT_DIR/$SYSTEM/run_dataspaces.sh
		;;

		* )
		echo "ERROR: "\'"$SYSTEM"\'" you entered is current not supported"
		exit 1
		;;

	esac

	TIMEOUT_FLAG=$?

	if [ $TIMEOUT_FLAG -eq 124 ]
	then
		echo "ERROR: Timeout after $TIME_LIMIT seconds"
		#Process all we have
		bash $SCRIPT_DIR/postprocess/postprocess.sh
	else
		#Calling postprocess
		bash $SCRIPT_DIR/postprocess/postprocess.sh
		echo "Succeed!"
	fi
}


main







