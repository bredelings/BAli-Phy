#!/usr/bin/perl -w 

use strict;

my $filename = shift;
$filename = "out" if (!defined($filename));

my $pwd = get_pwd();
my $data = get_data();
my $smodel = get_smodel();
my $marginal = get_marginal();

print "----- [$pwd] ----\n";
print "data = $data      smodel = $smodel\n\n";

print "P( $data | $smodel )  =  $marginal";

sub get_pwd {
    my $pwd = `pwd`;
    chomp $pwd;
    $pwd =~ s|.*/||g;
    return $pwd;
}


sub get_marginal {
    my $marginal = `cat Work/Pmarg`;
    $marginal =~ s/P\(M\|data\) = //;
    return $marginal;
}
    

sub get_data {
    my $data = get_var("data");
    $data =~ s|Data/||;
    return $data;
}

sub get_smodel {
    my $smodel = get_var("subst model");
    $smodel =~ s|Empirical\(([^)]*)\)|$1|;
    $smodel =~ s|Data/||;
    $smodel =~ s|.dat||;
    $smodel =~ s|rate~||;
    return $smodel;
}

sub get_var {
    my $var_name = shift;
    my $var=undef;
    open(INPUT,$filename);
    while (<INPUT>) {
	if (/^$var_name = (.*)/) {
	    $var = $1;
	    return $var;
	}
    }
    return undef;
}
