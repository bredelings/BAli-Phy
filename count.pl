#!/usr/bin/perl -w

my $node1 = shift;
my $node2 = shift;

my %count = ();

while(<STDIN>) {
    if (/$node1 $node2 :\s+([0-9].*)$/) {
	my $data = $1;
	my ($x1,$y1,$x2,$y2) = split(/\s+/,$data);

	if (!defined($count{$x1}{$y1}{$x2}{$y2})) {
	    $count{$x1}{$y1}{$x2}{$y2}=1;
	}
	else {
	    $count{$x1}{$y1}{$x2}{$y2}++;
	}
    }
}


for my $x1 (sort {$a <=> $b} (keys(%count))) {
    my $hash2 = $count{$x1};
    for my $y1 (keys(%$hash2)) {
	my $hash3 = $hash2->{$y1};
	for my $x2 (keys(%$hash3)) {
	    my $hash4 = $hash3->{$x2};
	    for my $y2 (keys(%$hash4)) {
		my $temp = $hash4->{$y2};
		print "$x1 $y1 $x2 $y2 $temp\n";
	    }
	}
    }
}
