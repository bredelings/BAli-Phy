#!/usr/bin/perl -w

use strict;


my $skip=0;

if (defined($ARGV[0]) && ($ARGV[0] =~ /--skip=(.+)/))
{
    $skip=$1;
    shift;
}



my $total = 0;
my @counts;
my $subtotal = 0;
while (my $line = <>)
{
    if ($total == 0) {
	for(my $i=0;$i<length($line);$i++) {
	    $counts[$i]=0;
	}
    }

    $total++;

    if ($total >= $skip) {
	$subtotal++;
	for(my $i=0;$i<length($line);$i++) {
	    my $letter = substr($line,$i,1);
	    $counts[$i]++ if ($letter eq "F");
	}
    }

    
}

for(my $i=0;$i<$#counts;$i++) {
    my $Pr_fast =  $counts[$i]/$subtotal;
    print "$i ",$Pr_fast,"\n";

    my $consensus = "S";
    $consensus = "F" if ($Pr_fast > 0.5);
    print STDERR $consensus;
}
print STDERR "\n";
print STDERR "SUBTOTAL = $subtotal\n";
