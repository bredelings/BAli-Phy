#!/usr/bin/perl -w
use strict;

while (my $line = <>)
{
    if ($line =~ /^DEFINITION *([^ ].*)/)
    {
	my $name = $1;
	print ">$name\n";
	next;
    }
    if ($line =~ /^ *[0-9]+ ([^ ].*)$/)
    {
	my $data = $1;
	print "$data\n";
    }
}
