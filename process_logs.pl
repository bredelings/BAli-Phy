#!/usr/bin/perl -w

use strict;

my $skip = shift;
if (!defined($skip)) {$skip = 0;}

my %count = ();
my %ll = ();

my $i=0;
my $total=0;
print STDERR "Skipping $skip\n";
while(<STDIN>) {
    chomp;
    $i++;
    my ($value,$id) = split(/ /);
    if (!defined($id)) {$id = "";}
    my $key = "$value:$id";
    $ll{$key} = $value;
    next if ($i < $skip);

    if (!defined($count{$key})) {
	$count{$key}=1;
    } 
    else {
	$count{$key}++;
    }
    $total++;
}

my $sum = 0.0;
my %Pr = ();
my %CDF = ();
my %ECDF = ();

my $max = undef;
my @keys = sort {$ll{$a} <=> $ll{$b}} keys(%count);


foreach my $key (@keys) {
    if (!defined($max) || $ll{$key}> $max) {
	$max = $ll{$key};
    }
}


my $csum = 0;
foreach my $key (@keys) {
    my $value = $ll{$key};

    $Pr{$key} = exp($value-$max);
    $sum += $Pr{$key};
    print STDERR "$value $Pr{$key} $sum\n";
    $CDF{$key} = $sum;
    $csum += $count{$key};
    $ECDF{$key} = $csum; 
}
print STDERR "sum = $sum\n";

foreach my $key (@keys) {
#    print STDERR "key = $value  sum = $sum   cdf = $CDF{$value}\n";
    $Pr{$key} /= $sum;
    $CDF{$key}  /= $sum;
    $ECDF{$key} /= $total;
}



foreach my $key (@keys) {
    print "ll = $ll{$key}   p = $Pr{$key}    cdf = $CDF{$key}   ECDF = $ECDF{$key}\n";
}
