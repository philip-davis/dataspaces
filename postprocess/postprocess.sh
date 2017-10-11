#!/bin/bash
RESULT_FILE="$SCRIPT_DIR/output/result.csv"
LOG_FILE="$SCRIPT_DIR/output/log.writer" 
TMP_FILE="$SCRIPT_DIR/output/tmp"
TMP_FILE2="$SCRIPT_DIR/output/tmp_2"
TMP_FILE3="$SCRIPT_DIR/output/tmp_3"
CONFIG_FILE="$SCRIPT_DIR/input/configure"


function read_log_file {

	awk '/time=/{print $8}' < $LOG_FILE | xargs -n $NUM_TS > $TMP_FILE  #This xargs convert data to 2d array, Num_Interval * Num_timesteps

}


function read_config {
	#Get data size
	# if found keyword, then from the next line, get and process content, till match then next keyword
DATA_SIZE_X_ARR=($(awk '/DATA_SIZE_START:/{f=1;next}/DATA_SIZE_END/{f=0}f{split($0,ft,",");print ft[1]}' $CONFIG_FILE))
DATA_SIZE_Y_ARR=($(awk '/DATA_SIZE_START:/{f=1;next}/DATA_SIZE_END/{f=0}f{split($0,ft,",");print ft[2]}' $CONFIG_FILE))
DATA_SIZE_Z_ARR=($(awk '/DATA_SIZE_START:/{f=1;next}/DATA_SIZE_END/{f=0}f{split($0,ft,",");print ft[3]}' $CONFIG_FILE))


PPN_ARR=($(awk '/PPN_START:/{f=1;next}/PPN_END/{f=0}f{split($0,ft);print ft[1]}' $CONFIG_FILE))

RATIO_ARR=($(awk '/RATIO_START:/{f=1;next}/RATIO_END/{f=0}f{split($0,ft);print ft[1]}' $CONFIG_FILE))



}

function process_config {

#let's run simulation from here
	read_config

	echo "DATA_SIZE","PPN","RATIO" >> $TMP_FILE2

	for (( i=0; i<${#DATA_SIZE_X_ARR[@]}; i++)); #Data size
	do
		DATA_SIZE="${DATA_SIZE_X_ARR[i]}x${DATA_SIZE_Y_ARR[i]}x${DATA_SIZE_Z_ARR[i]}" #convert data size format
 


		for (( j=0; j<${#PPN_ARR[@]}; j++)); #Process per node
		do
			PPN=${PPN_ARR[j]}

			for (( k=0; k<${#RATIO_ARR[@]}; k++)); #Ratio
			do
				RATIO=${RATIO_ARR[k]}
				
				echo $DATA_SIZE,$PPN,$RATIO >> $TMP_FILE2
			

			done
		done
	done

}


function process_result {
	#Standart deviation formula
	#stdev = sqrt((1/N)*(sum of (value - mean)^2))

	#Here we use the "sum of squares" formula for standard deviation
	#stdev = sqrt((1/N)*((sum of squares) - (((sum)^2)/N)))
	awk '
		{SUM=0
		SUMSQ=0 #sum of squares, used for calc standard deviation
		AVG=0
		MAX=$1
		MIN=$1
		DEV=0 #standard deviation
		for(i=1;i<=NF;i++)
		{
			SUM += $i;
			SUMSQ += ($i)^2;
			if($i>MAX) MAX=$i;
			if($i<MIN) MIN=$i;
		}
		AVG=SUM/NF
		DEV=sqrt((SUMSQ-SUM^2/NF)/NF) 
		
		if (NR == 1){
			OFS=",";
			print "AVG","MAX","MIN","DEV"
			print AVG, MAX, MIN, DEV
		}
		else{
			print AVG, MAX, MIN, DEV
		}
		
		}
	' <$TMP_FILE> $TMP_FILE3

}

function merge2file {

	awk 'NR==FNR{a[FNR]=$0;next} {print a[FNR] "," $0}' $TMP_FILE2 $TMP_FILE3 >> $RESULT_FILE

}

function main {

#print current time to result file
DATE_TIME=`date "+%Y-%m-%d %H:%M:%S"`;
echo -e "Time:", "$DATE_TIME\n" >> $RESULT_FILE

#Process writer data 
LOG_FILE="$SCRIPT_DIR/output/log.writer" 
echo -e "WRITER\n" >> $RESULT_FILE
read_log_file
read_config
process_config
process_result
merge2file

rm -rf $TMP_FILE $TMP_FILE2 $TMP_FILE3

#Process reader data 
LOG_FILE="$SCRIPT_DIR/output/log.reader" 
echo -e "READER\n" >> $RESULT_FILE
read_log_file
read_config
process_config
process_result
merge2file

rm -rf $TMP_FILE $TMP_FILE2 $TMP_FILE3
}

main







