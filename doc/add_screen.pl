#!/usr/bin/perl -w

use strict;

my $screen = "";
while(my $line = <>)
{
    if ($line =~ s|^\% (.*)# (.*)$|<prompt>\%</prompt> <userinput>$1</userinput># $2|)
    {
	$screen = $screen . $line;
	next;
    }
    elsif ($line =~ s|^\%\%(.*)$|$1|)
    {
	$screen = $screen . $line;
	next;
    }
    elsif ($line =~ s|^\% (.*)$|<prompt>\%</prompt> <userinput>$1</userinput>|)
    {
	$screen = $screen . $line;
	next;
    }
    elsif ($line =~ m/^\| (.*)$/)
    {
	$screen = $screen . $line;
	next;
    }
    elsif ($screen ne "")
    {
	print "<screen>$screen</screen>";
	$screen = "";
    }
    print $line;
}
