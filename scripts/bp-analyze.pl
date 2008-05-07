#!/usr/bin/perl -w 

use strict;

sub do_init()
{
    mkdir "Work";
    mkdir "Mixing";
    mkdir "Results";
}

sub do_cleanup()
{
    rmdir_recursive("Work");
    rmdir_recursive("Mixing");
    rmdir_recursive("Results");
}

sub rmdir_recursive 
{
    my $dir = shift;
    local *DIR;

    opendir DIR, $dir or die "opendir $dir: $!";
    for (readdir DIR) {
	next if /^\.{1,2}$/;
	my $path = "$dir/$_";
	unlink $path if -f $path;
	cleanup($path) if -d $path;
    }
    closedir DIR;
    rmdir $dir or print "error - $!";
}

sub get_partitions()
{
    local *FILE;

    open FILE, "1.out" or die "Can't open 1.out!";

    my @partitions = ();

    while (my $line = <FILE>) 
    {
	if ($line =~ /data(.+) = (.+)/) {
	    push @partitions,$2;
	}
	last if ($line =~ /iterations/);
    }
    return [@partitions];
}

sub get_n_lines($)
{
    my $filename = shift;

    local *FILE;
  
    open FILE, $filename;

    my $n_lines = 0;

    while (<FILE>) {
	$n_lines++;
    }

    return $n_lines;
}

sub get_n_iterations()
{
    return get_n_lines("1.trees")-1;
}

sub more_recent_than($;$)
{
    my $filename1 = shift;
    my $filename2 = shift;

    die "I can't open '$filename2'" if (! -f $filename2);
    return 0 if (! -f $filename1);

    my $age1 = -M $filename1;
    my $age2 = -M $filename2;

    return 1 if ($age1 <= $age2);
    return 0;
}

#----------------------------- MAIN --------------------------#

my $burnin = 0;
my $max_iter;
my $subsample = 1;
my $min_support;

while ($#ARGV > -1) 
{
    my $arg = shift;
    if ($arg eq "clean") {
	do_cleanup();
	exit;
    }
    elsif ($arg =~ /--burnin=(.+)/) {
	$burnin = $1;
    }
    elsif ($arg =~ /--subsample=(.+)/) {
	$subsample = $1;
    }
    elsif ($arg =~ /--max=(.+)/) {
	$max_iter = $1;
    }
    elsif ($arg =~ /--min-support=(.+)/) {
	$min_support = $1;
    }
}

do_init();

my @partitions = @{ get_partitions() };
my $n_partitions = 1+$#partitions;
my $n_iterations = get_n_iterations();


my @trees = ();
my %tree_name = ();

my @tree_consensus_values = sort(0.5,0.66,0.8,0.9,0.95,0.99,1.0);

# 1. compute consensus trees
my $max_arg = "";
$max_arg = "--max=$max_iter" if (defined($max_iter));
my $min_support_arg = "";
$min_support_arg = "--min-support=$min_support" if (defined($min_support));
my $consensus_arg = "--consensus=".join(',',@tree_consensus_values);
my $size_arg = "";
$size_arg = "--size=$max_iter" if defined($max_iter);

print STDERR "Summarizing topology distribution ... ";
if (! more_recent_than("Results/consensus","1.trees")) {
    `trees-consensus 1.trees --skip=$burnin $max_arg $min_support_arg --sub-partitions $consensus_arg > Results/consensus`;
}
print STDERR "done.\n";

# 2. compute consensus trees

print " Calculating branch lengths for consensus trees:\n   ";
for my $cvalue (@tree_consensus_values)
{
    my $value = $cvalue*100;
    my $tree = "c$value";
    push @trees,$tree;
    $tree_name{$tree} = "$value\% consensus";

    if (! more_recent_than("Results/$tree.topology","Results/consensus")) {
	`pickout $value-consensus -n < Results/consensus > Results/$tree.topology`;
    }
    if (! more_recent_than("Results/$tree.mtree","Results/consensus")) {
	`pickout $value-consensus -n --multi-line < Results/consensus > Results/$tree.mtree`;
    }
    
    print "$tree ";
    if (! more_recent_than("Results/$tree.tree","1.trees")) {
    `tree-mean-lengths Results/$tree.topology --safe --skip=$burnin $max_arg < 1.trees > Results/$tree.tree 2>/dev/null`;
    }
}
print "done.\n";

# 3. compute MAP tree

if (! more_recent_than("Results/MAP.topology","Results/consensus")) {
    `pickout MAP-0 -n < Results/consensus > Results/MAP.topology`;
}
print " Calculating branch lengths for MAP tree... ";
if (! more_recent_than("Results/MAP.tree","1.trees")) {
    `tree-mean-lengths Results/MAP.topology --safe --skip=$burnin $max_arg < 1.trees > Results/MAP.tree 2>/dev/null`;
}
push @trees,"MAP";
$tree_name{"MAP"} = "MAP";
print "done.\n";

# 4. compute images
print " Drawing trees ... ";
for my $tree (@trees) {
    `cd Results ; draw-tree $tree.tree --layout=equal-daylight 2>/dev/null`;
    `cd Results ; draw-tree $tree.tree --layout=equal-daylight --output=svg 2>/dev/null`;
}
print "done.\n";

# 5. Summarize scalar parameters
print "\nSummarizing distribution of numerical parameters... ";
if (! more_recent_than("Results/Report","1.p")) {
    `statreport --skip=$burnin $max_arg < 1.p > Results/Report 2>/dev/null`;
}
print "done.\n";

# 6. Compute initial alignments

print "\nComputing initial alignments... ";
my @alignments = ();
my @AU_alignments = ();
my %alignment_names = ();

for(my $i=0;$i<$n_partitions;$i++)
{
    my $p = $i+1;
    my $name = "P$p-initial";
    push @alignments,$name;
    $alignment_names{$name} = "Initial";

    `alignment-find --first < 1.P$p.fastas > Work/$name-unordered.fasta`;
}
print "done.\n";

# 7. Compute consensus-alignments
my @alignment_consensus_values = sort(0.1,0.25,0.5,0.75);

for(my $i=0;$i<$n_partitions;$i++)
{
    my $p = $i+1;
    my $infile = "1.P$p.fastas";

    print " Partition $p: Computing consensus alignments: \n   ";
    for my $cvalue (@alignment_consensus_values) {
	my $value = $cvalue*100;
	my $name = "P$p-consensus-$value";
	print "c$value ";
	if (! more_recent_than("Work/$name-unordered.fasta",$infile)) {
	    `cut-range --skip=$burnin $size_arg < $infile | alignment-consensus --cutoff=$cvalue> Work/$name-unordered.fasta 2>/dev/null`;
	}
	push @alignments,$name;
	$alignment_names{$name} = "$value% consensus";
    }
    print "done.\n\n";
    push @AU_alignments,"P$p-consensus-10";
}

for my $alignment (@alignments) {
    `alignment-reorder Work/$alignment-unordered.fasta Results/c50.tree > Results/$alignment.fasta 2>/dev/null`;
    `alignment-draw Results/$alignment.fasta --no-legend --show-ruler --color-scheme=AA+contrast > Results/$alignment.html`;
    `alignment-draw Results/$alignment.fasta --no-legend --show-ruler --color-scheme=DNA+contrast > Results/$alignment.html`;
}

# 8. AU plots

for my $alignment (@AU_alignments) 
{
    if ($alignment =~ /^P([^-]+)-.*/) {
	print "Generating AU values for $alignment... ";
	my $p = $1;
	my $infile = "1.P$p.fastas";

	if (!more_recent_than("Results/$alignment-AU.prob",$infile)) {
	`cut-range --skip=$burnin $size_arg < $infile | alignment-gild Results/$alignment.fasta Results/MAP.tree --max-alignments=500 > Results/$alignment-AU.prob 2>/dev/null`;
	}
	print "done.\n";
	`alignment-draw Results/$alignment.fasta --no-legend --show-ruler --AU Results/$alignment-AU.prob --color-scheme=AA+contrast+fade+fade+fade+fade > Results/$alignment-AU.html`;
	`alignment-draw Results/$alignment.fasta --no-legend --show-ruler --AU Results/$alignment-AU.prob --color-scheme=DNA+contrast+fade+fade+fade+fade > Results/$alignment-AU.html`;
    }
}



#------------------------- Print Index -----------------------#

open INDEX,">Results/index.html";

my $title = "Analysis";

print INDEX '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
';
print INDEX "<title>$title</title>\n";
print INDEX 
'    <style type="text/css">
      ol li {padding-bottom:0.5em}

      td {padding: 0.1em;}
      td {padding-left: 0.3em;}
      td {padding-right: 0.3em;}

    </style>';


print INDEX "<h1>$title</h2>\n";
print INDEX "  <p>$n_partitions partitions</p>\n";
print INDEX "<ol>\n";
for my $partition (@partitions) {
    print INDEX "<li>$partition</li>\n";
}
print INDEX "</ol>\n";

print INDEX '<table style="width:100%;text-align:center"><tr>'."\n";
print INDEX "<td>burn-in = $burnin</td>\n";
print INDEX "<td>sub-sample = $subsample</td>\n" if ($subsample != 1);
print INDEX "<td>total iterations = $n_iterations</td>\n";
print INDEX "</table>\n";

print INDEX "<h2>Parameters</h2>\n";
print INDEX "<ul><li><a href=\"Report\">Summary</a></li></ul>\n";

print INDEX "<h2>Phylogeny</h2>\n";

print INDEX "  </head>\n  <body>\n";

print INDEX "<p>Partition support: <a href=\"consensus\">Summary</a></p>\n";

print INDEX "<table>\n";
for my $tree (@trees)
{
    my $name = $tree_name{$tree};
    print INDEX "<tr>";
    print INDEX "<td>$name</td>";
    print INDEX "<td><a href=\"$tree.topology\">topology</a></td>";
    print INDEX "<td><a href=\"$tree.tree\">tree</a></td>";
    if (-f "Results/$tree.mtree") {
	print INDEX "<td><a href=\"$tree.mtree\">mtree</a></td>"     
    }
    else {
	print INDEX "<td></td>"     
    }
    print INDEX "<td><a href=\"$tree-tree.pdf\">PDF</a></td>";
    print INDEX "<td><a href=\"$tree-tree.svg\">SVG</a></td>";
    print INDEX "</tr>";
}
print INDEX "</table>\n";

print INDEX "<h2>Alignments</h2>\n";

for(my $i=0;$i<$n_partitions;$i++) 
{
    my $p = $i+1;
    print INDEX "<h3>Partition $p</h3>\n";
    print INDEX "<table>\n";
    for my $alignment (@alignments) 
    {
	next if ($alignment !~ /^P$p-/);
	my $name = $alignment_names{$alignment};
	print INDEX "<tr>\n";
	print INDEX "<td>$name</td>\n";
	print INDEX "<td><a href=\"$alignment.fasta\">FASTA</a></td>\n";
	if (-f "Results/$alignment.html") {
	    print INDEX "<td><a href=\"$alignment.html\">HTML</a></td>\n";
	}
	else {
	    print INDEX "<td></td>\n";
	}
	if (-f "Results/$alignment-AU.html") {
	    print INDEX "<td><a href=\"$alignment-AU.html\">AU-HTML</a></td>\n";
	}
	else {
	    print INDEX "<td></td>\n";
	}
	print INDEX "</tr>\n";
    }
    print INDEX "</table>\n";
}

print INDEX "  </body>\n";
print INDEX "</html>\n";
