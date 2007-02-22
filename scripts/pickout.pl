#! /usr/bin/perl -w

$names = shift;
@namelist = split(/,/,$names);

while(<STDIN>) {
    chomp;
    @values = ( );
    for($i=0; $i <= $#namelist; $i++) {
	$value = "";
	$name = $namelist[$i];
	$name =~ s|\\|\\\\|g;

	if (/$name = ([^ ]+)( |$)/) {
	    $value = $1;
	}
	push @values,$value;

    }
    $empty = 0;
    foreach $value (@values) {
	if ($value eq "") { $empty = 1 ;}
    }
    if (!$empty) {
	print join(' ',@values);
	print "\n";
    }
}
