#!/usr/bin/perl -w

my $prefix = shift;
my $line = <STDIN>;

while($line =~ /($prefix.) = /g) {
    print "$1\n";
}
