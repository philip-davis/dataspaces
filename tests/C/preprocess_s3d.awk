#! /usr/bin/awk -f

BEGIN {
	FS = " "; 
} 
{ 
	#split($1, aps, "\.");
	if($1=="dspaces_get"){
		#print $9 "\t0\tlor\tA";
		if(get_count==0){
			print $9 "\t0\tlor\t A";
		}
		get_count++;
		print $9 "\t0\tr\tA\t" $3 "\t" $4 "\t" $5 "\t" $6 "\t" $7 "\t" $8;
		if(get_count==64){
			print $9 "\t0\tuor\t A";
			get_count = 0;
		}
		#print $9 "\t0\tuor\tA";
	}

	if($1=="dspaces_put"){
		if(count==0){
			print $9-1 "\t1\tlow\t A";
		}
		count++;
		#print $9 "\t1\tlow\t A";
		print $9-1 "\t1\tw\tA\t" $3 "\t" $4 "\t" $5 "\t" $6 "\t" $7 "\t" $8;
		if(count==64){
			print $9-1 "\t1\tuow\t A";
			count = 0;
		}
		#print $9 "\t1\tuow\tA";
		
	}
}