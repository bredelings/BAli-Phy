#!/usr/bin/perl -w

use strict;

my $skip = shift;
if (!defined($skip)) {$skip = 0;}

my %count = ();

my $i=0;
my $total=0;
print STDERR "Skipping 10\n";
while(<STDIN>) {
    chomp;
    $i++;
    next if ($i < $skip);
    if (!defined($count{$_})) {
	$count{$_}=1;
    } 
    else {
	$count{$_}++;
    }
    $total++;
}

my $sum = 0.0;
my %Pr = ();
my %CDF = ();
my %ECDF = ();

my $max = undef;
foreach my $value (sort {$a <=> $b} keys(%count)) {
    if (!defined($max) || $value > $max) {
	$max = $value;
    }
}


my $csum = 0;
foreach my $value (sort {$a <=> $b} keys(%count)) {
    $Pr{$value} = exp($value-$max);
    $sum += $Pr{$value};
    print STDERR "$value $Pr{$value} $sum\n";
    $CDF{$value} = $sum;
    $csum += $count{$value};
    $ECDF{$value} = $csum; 
}
print STDERR "sum = $sum\n";

foreach my $value (sort {$a <=> $b} keys(%Pr)) {
#    print STDERR "key = $value  sum = $sum   cdf = $CDF{$value}\n";
    $Pr{$value} /= $sum;
    $CDF{$value}  /= $sum;
    $ECDF{$value} /= $total;
}



foreach my $value (sort {$a <=> $b} keys(%Pr)) {
    print "value = $value   p = $Pr{$value}    cdf = $CDF{$value}   ECDF = $ECDF{$value}\n";
}
