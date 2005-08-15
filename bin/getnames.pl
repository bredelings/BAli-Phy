#!/usr/bin/perl -w

my $line = <STDIN>;

while($line =~ /([^* ]+) = /g) {
    print "$1\n";
}
