#! /usr/bin/awk -f

BEGIN {
	FS = " "; 
} 
{ 
	if($1=="dspaces_get"){
		print $9 "_" $2 "\t0\tlor\tA";
		get_count++;
		print $9 "\t0\tr\tA\t" $3 "\t" $4 "\t" $5 "\t" $6 "\t" $7 "\t" $8;
		print $9 "_" $2 "\t0\tuor\tA";
	}

	if($1=="dspaces_put"){
		print $9-1 "_" $2 "\t1\tlow\t A";
		print $9-1 "\t1\tw\tA\t" $3 "\t" $4 "\t" $5 "\t" $6 "\t" $7 "\t" $8;
		print $9-1 "_" $2 "\t1\tuow\tA";
		
	}
}