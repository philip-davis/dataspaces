#!/bin/bash
#SBATCH -p TBD
#SBATCH -A YOUR_ALLOC
#SBATCH -N TBD
#SBATCH -t TBD
#SBATCH -C TBD

SCRIPT_DIR=
DATASPACES_DIR=

SYSTEM=
SYS_NUM=
TIMEOUT_FLAG=
TIME_LIMIT=10

function print_options {

echo "Select the number of your system from following options:
1. Summit
2. Cori
3. Theta
4. Stampede2
5. Caliburn
6. Quit"

echo "Please type the sytem number, followed by [ENTER]: "
read SYS_NUM

if [ $SYS_NUM -gt 6 ] || [ $SYS_NUM -lt 1 ]
	then
	echo "Wrong number and exit"
	exit 1
fi

echo "Please type the execution time limit in seconds, followed by [ENTER]: "
read TIME_LIMIT

}

function main {

	print_options
	#Export two directory for other scripts to use
	export SCRIPT_DIR
	export DATASPACES_DIR 

	
	case $SYS_NUM in
		1)
		SYSTEM="Summit"
		timeout $TIME_LIMIT bash $SCRIPT_DIR/$SYSTEM/run_dataspaces.sh
		;;

		2)
		SYSTEM="Cori"
		timeout $TIME_LIMIT bash $SCRIPT_DIR/$SYSTEM/run_dataspaces.sh
		;;

		3)
		SYSTEM="Theta"
		timeout $TIME_LIMIT bash $SCRIPT_DIR/$SYSTEM/run_dataspaces.sh
		;;

		4)
		SYSTEM="Stampede2"
		timeout $TIME_LIMIT bash $SCRIPT_DIR/$SYSTEM/run_dataspaces.sh
		;;

		5)
		SYSTEM="Caliburn"
		timeout $TIME_LIMIT bash $SCRIPT_DIR/$SYSTEM/run_dataspaces.sh
		;;

		6)
		exit 1
		;;

	esac

	TIMEOUT_FLAG=$?

	if [ $TIMEOUT_FLAG -eq 124 ]
	then
		echo "ERROR: Timeout after $TIME_LIMIT seconds"
	else
		echo "Succeed!"
	fi
}


main







