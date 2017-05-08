#!/usr/bin/perl
# Script by Ramanan Sankaran
# Originally written for the seaborg 
# This will generate the task geometry so as to minimize the communication
# This version will do 2x2x3 subcubes for 12 core jaguarpf node

@np[0..2] = ($npx, $npy, $npz); 
$fctr = 0; 
foreach(0..2){
  $fctr+=1*(4**$_) if ($np[$_]%2==0); 
  $fctr+=2*(4**$_) if ($np[$_]%3==0); 
}
%cmb=undef; 
%cmb = qw(
  22  322
  25  232
  37  223
); 

$div=0; 
foreach(keys %cmb){
  $qtnt = $_ & $fctr; 
  ($div=$_ and last) if (($qtnt&3) and ($qtnt&12) and ($qtnt&48));
}

if($div==0) {
  print "Suitable factor for $npx x $npy x $npz for rank reorder was not found\n"; 
} else {
  foreach (0..2){
    $nd[$_]=2;
    $nd[$_]=3 unless ( ($div & 3*(4**$_))/(4**$_) == "1"); 
    $nb[$_] = $np[$_]/$nd[$_]; 
  }
  print "Reordering ranks in $nb[0]x$nb[1]x$nb[2] $cmb{$div} subcubes for $npx x $npy x $npz\n"; 
}

open(RANKFILE,">rank.$ext") || die "cannot open rankfile: $!"; 
print "writing to rank.$ext\n"; 

#print "{";  #needed for IBM loadleveler
foreach $k (0 .. $nb[2] -1){
  foreach $j (0 .. $nb[1]-1){
    foreach $i (0 .. $nb[0]-1){
      @pidarray = ();
      foreach $n (0 .. $nd[2]-1){
        foreach $m (0 .. $nd[1]-1){
          foreach $l (0 .. $nd[0]-1){
            $pid = 
              ($k*$nd[2]+$n)*$np[0]*$np[1] + ($j*$nd[1]+$m)*$np[0] + $i*$nd[0]+$l;
            push (@pidarray, $pid);
          }
        }
      }
      $pidlist = join(",",@pidarray);
#      print "($pidlist)";   #For IBM loadlever
      print RANKFILE "$pidlist,\n";   #For Cray XT, jaguar, each line sits on a node
    }
  }
}
#print "}\n";  #needed for IBM loadleveler

close(RANKFILE); 

