#!/usr/bin/perl -w

use strict;

my $file1 = shift;
my $file2 = shift;

my $count1 = count($file1);
my $count2 = count($file2);
my $count3 = {};

subtract($count1,$count2,$count3);

print_count($count3,$count1,$count2);

sub count {
    my $filename = shift;
    open FILE, $filename;

    my %count = ();
    while(<FILE>) {
	chomp;
	my ($x1,$y1,$x2,$y2,$number) = split(/\s+/);

	$count{$x1}{$y1}{$x2}{$y2}=$number;
    }
    close FILE;
    return {%count};
}

sub subtract {
    my $c1 = shift;
    my $c2 = shift;
    my $c3 = shift;
    my $c3p = {};
    my $c3n = {};

    subtract_half($c1,$c2,$c3p);
    subtract_half($c2,$c1,$c3n);

    for my $x1 (sort {$a <=> $b} (keys(%$c3p))) {
	my $hash2 = $c3p->{$x1};
	for my $y1 (keys(%$hash2)) {
	    my $hash3 = $hash2->{$y1};
	    for my $x2 (keys(%$hash3)) {
		my $hash4 = $hash3->{$x2};
		for my $y2 (keys(%$hash4)) {
		    my $temp = $hash4->{$y2};
		    $c3->{$x1}{$y1}{$x2}{$y2} = $temp;
		}
	    }
	}
    }

    for my $x1 (sort {$a <=> $b} (keys(%$c3n))) {
	my $hash2 = $c3n->{$x1};
	for my $y1 (keys(%$hash2)) {
	    my $hash3 = $hash2->{$y1};
	    for my $x2 (keys(%$hash3)) {
		my $hash4 = $hash3->{$x2};
		for my $y2 (keys(%$hash4)) {
		    my $temp = $hash4->{$y2};
		    $c3->{$x1}{$y1}{$x2}{$y2} = -$temp;
		}
	    }
	}
    }
}


sub subtract_half {
    my $c1 = shift;
    my $c2 = shift;
    my $c3 = shift;
    my %count = %$c1;

    for my $x1 (sort {$a <=> $b} (keys(%count))) {
	my $hash2 = $count{$x1};
	for my $y1 (keys(%$hash2)) {
	    my $hash3 = $hash2->{$y1};
	    for my $x2 (keys(%$hash3)) {
		my $hash4 = $hash3->{$x2};
		for my $y2 (keys(%$hash4)) {
		    my $temp = $hash4->{$y2};
		    my $temp2 = $c2->{$x1}{$y1}{$x2}{$y2};
		    if (!defined($temp2)) {
			$temp2 = 0;
		    }
		    if ($temp >= $temp2) {
			$c3->{$x1}{$y1}{$x2}{$y2} = $temp-$temp2;
		    }
		}
	    }
	}
    }
}

sub print_count {
    my $c1 = shift;
    my $c2 = shift;
    my $c3 = shift;
    my %count = %$c1;
    for my $x1 (sort {$a <=> $b} (keys(%count))) {
	my $hash2 = $count{$x1};
	for my $y1 (keys(%$hash2)) {
	    my $hash3 = $hash2->{$y1};
	    for my $x2 (keys(%$hash3)) {
		my $hash4 = $hash3->{$x2};
		for my $y2 (keys(%$hash4)) {
		    my $temp = $hash4->{$y2};
		    my $count1 = $c2->{$x1}{$y1}{$x2}{$y2};
		    my $count2 = $c3->{$x1}{$y1}{$x2}{$y2};
		    $count1 = 0 if (!defined($count1));
		    $count2 = 0 if (!defined($count2));
		    print "$x1 $y1 $x2 $y2  $temp $count1 $count2\n";
		}
	    }
	}
    }
}
