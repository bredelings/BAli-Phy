#!/usr/bin/perl -w
# This script is for analyzing dynamic programming matrices, to look
#  for diagonals.

my $xsize = shift;
my $ysize = shift;
my $x = 0;
my $y = 0;

while(<>) {
    chomp;
    if (/^\#/) {
	next;
    }
    my @pair = split(/\s+/);
    my $value = $pair[1];
    $function[$x][$y] = $value;

    $y++;
    if ($y == $ysize) {
	$y = 0;
	$x++;
    }
}

for(my $i=0;$i<$xsize;$i++) {
    for(my $j=0;$j<$ysize;$j++) {
	my $largest = $i;
	$largest = $j if ($j>$i);
	my $value = $function[$i][$j];
#	my $value = $function[$i][$j] - $function[$largest][$largest];
	print "$i $j $value\n";
    }
    print "\n";
}
