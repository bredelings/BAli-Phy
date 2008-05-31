#!/usr/bin/perl -w 

# TODO
# 1. # of sequences
# 2. alignment properties (#indels, etc)
# 3. alphabet of each partition
# 4. Somehow print posterior distribution of # of indels (really)
#    and substitutions (really - not just parsimony score?)     

use strict;

use POSIX;

my $home = $ENV{'HOME'};

sub do_init()
{
    mkdir "Results";
    mkdir "Results/Work";
}

sub do_cleanup()
{
    rmdir_recursive("Results") if (-e "Results");
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
	rmdir_recursive($path) if -d $path;
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
	    my $filename = $2;
	    $filename =~ s/$home/~/;
	    push @partitions,$filename;
	}
	if ($line =~ /data = (.+)/) {
	    my $filename = $1;
	    $filename =~ s/$home/~/;
	    push @partitions,$filename;
	}
	last if ($line =~ /^iterations = 0/);
    }
    return [@partitions];
}

sub get_header_attribute($)
{
    my $attribute = shift;
    my $value = "Cannot find value \"$attribute\" in output file!";

    local *FILE;

    open FILE, "1.out" or die "Can't open 1.out!";

    my @partitions = ();

    while (my $line = <FILE>) 
    {
	if ($line =~ /$attribute: (.*)$/) {
	    $value = $1;
	    last;
	}
	last if ($line =~ /^iterations = 0/);
    }
    close FILE;

    return $value;
}

#Empirical(/home/bredelings/local/share/bali-phy/Data//wag.dat) 

sub sanitize_smodel($)
{
    my $smodel = shift;

    if ($smodel =~ m|(Empirical\(.*/(.*).dat\))|)
    {
	my $temp1 = $1;
	my $temp2 = $2;
	$temp2 =~ tr/a-z/A-Z/;
	
	$smodel =~ s/\Q$temp1/$temp2/;
    }

    return $smodel;
}

sub get_smodels()
{
    local *FILE;

    open FILE, "1.out" or die "Can't open 1.out!";

    my @smodels = ();

    while (my $line = <FILE>) 
    {
	if ($line =~ /subst model(.+) = (.+)/) {
	    push @smodels,$2;
	}
	if ($line =~ /subst model = (.+)/) {
	    push @smodels,$1;
	}
	last if ($line =~ /^iterations = 0/);
    }
    close FILE;

    return [@smodels];
}

sub get_imodels()
{
    local *FILE;

    open FILE, "1.out" or die "Can't open 1.out!";

    my @imodels = ();
    my $imodel0;

    while (my $line = <FILE>) 
    {
	if ($line =~ /indel model(.+) = (.+)/) {
	    push @imodels,$2;
	}
	if ($line =~ /indel model = (.+)/) {
	    push @imodels,$1;
	}

	last if ($line =~ /^iterations = 0/);
    }
    close FILE;

    push @imodels,$imodel0 if ($#imodels == -1);
    return [@imodels];
}

sub get_smodel_indices()
{
    local *FILE;

    open FILE, "1.out" or die "Can't open 1.out!";

    my @smodel_indices = ();

    while (my $line = <FILE>) 
    {
	if ($line =~ /smodel-index(.+) = (.+)/) {
	    push @smodel_indices,$2;
	}
	last if ($line =~ /^iterations = 0/);
    }
    close FILE;

    return [@smodel_indices];
}

sub get_imodel_indices()
{
    local *FILE;

    open FILE, "1.out" or die "Can't open 1.out!";

    my @imodel_indices = ();

    while (my $line = <FILE>) 
    {
	if ($line =~ /imodel-index(.+) = (.+)/) {
	    push @imodel_indices,$2;
	}
	last if ($line =~ /^iterations = 0/);
    }
    return [@imodel_indices];
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

my $burnin;

sub get_prev_burnin
{
    my $prev_burnin;
    $prev_burnin = `cat Results/burnin` if (-e "Results/burnin");
    return $prev_burnin;
}

sub record_burnin
{
    open BURN,">Results/burnin";
    print BURN $burnin;
    close BURN;
}

#----------------------------- MAIN --------------------------#

my $max_iter;
my $subsample = 1;
my $min_support;
my $muscle = 0;
my $probcons = 0;

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
    elsif ($arg =~ /--muscle/) {
	$muscle = 1;
    }
    elsif ($arg =~ /--probcons/) {
	$probcons = 1;
    }
    else {
	die "I don't recognize option $arg";
    }
}

do_init();

my $command = get_header_attribute("command");
my $directory = get_header_attribute("directory");
my $subdir    = get_header_attribute("subdirectory");


my @partitions = @{ get_partitions() };
my $n_partitions = 1+$#partitions;
my $n_iterations = get_n_iterations();
my @smodels = @{ get_smodels() };
my @imodels = @{ get_imodels() };

my @smodel_indices = @{ get_smodel_indices() };
push @smodel_indices,0 if ($#smodel_indices == -1);

my @imodel_indices = @{ get_imodel_indices() };
if ($#imodel_indices == -1)
{
    if ($#imodels == -1 || $imodels[0] eq "none") {
	push @imodel_indices,-1;
    }
    else {
	push @imodel_indices,0;
    }
}

die "I can't find sample file '1.out' - are you running this in the right directory?" if (! -e '1.out');

`ln -s 1.out 1.P1.fastas` if (! -e '1.P1.fastas' && $n_partitions == 1);


$burnin = int 0.1*$n_iterations if (!defined($burnin));
my $after_burnin = $n_iterations - $burnin +1;

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

print "Summarizing topology distribution ... ";
if (! more_recent_than("Results/consensus","1.trees")) {
    `trees-consensus 1.trees --skip=$burnin $max_arg $min_support_arg --sub-partitions $consensus_arg > Results/consensus`;
}
print "done.\n";

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

    if (! more_recent_than("Results/$tree.dot","Results/$tree.mtree")) {
	`draw-graph Results/$tree.mtree > Results/$tree.dot 2>/dev/null`;
    }
    if (! more_recent_than("Results/$tree-mctree.svg","Results/$tree.dot")) {
	`neato -Tsvg Results/$tree.dot > Results/$tree-mctree.svg`;
    }
    if (! more_recent_than("Results/$tree-mctree.pdf","Results/$tree.dot")) {
	`neato -Tpdf Results/$tree.dot > Results/$tree-mctree.pdf 2>/dev/null`;
	if ($?) {
	    `neato -Tps Results/$tree.dot > Results/$tree-mctree.ps`;
	    `ps2pdf Results/$tree-mctree.ps Results/$tree-mctree.pdf`;
	}
    }
    
    print "$tree ";
    if (! more_recent_than("Results/$tree.ltree","1.trees")) {
    `tree-mean-lengths Results/$tree.topology --safe --show-node-lengths --skip=$burnin $max_arg < 1.trees > Results/$tree.ltree 2>/dev/null`;
    }
    if (! more_recent_than("Results/$tree.tree","Results/$tree.ltree")) {
    `head -n1 Results/$tree.ltree > Results/$tree.tree`;
    }
}
print "done.\n";

# 3. compute MAP tree

if (! more_recent_than("Results/MAP.topology","Results/consensus")) {
    `pickout MAP-0 -n < Results/consensus > Results/MAP.topology`;
}
print " Calculating branch lengths for MAP tree... ";
if (! more_recent_than("Results/MAP.ltree","1.trees")) {
    `tree-mean-lengths Results/MAP.topology --safe --skip=$burnin $max_arg < 1.trees > Results/MAP.ltree 2>/dev/null`;
}
if (! more_recent_than("Results/MAP.tree","Results/MAP.ltree")) {
    `head -n1 Results/MAP.ltree > Results/MAP.tree`;
}
push @trees,"MAP";
$tree_name{"MAP"} = "MAP";
print "done.\n";

# 4. compute images
print " Drawing trees ... ";
for my $tree (@trees) {
    if (! more_recent_than("Results/$tree-tree.pdf","Results/$tree.ltree")) {
	`cd Results ; draw-tree $tree.ltree --layout=equal-daylight 2>/dev/null`;
    }
    if (! more_recent_than("Results/$tree-tree.svg","Results/$tree.ltree")) {
	`cd Results ; draw-tree $tree.ltree --layout=equal-daylight --output=svg 2>/dev/null`;
    }
}
print "done.\n";

# 5. Summarize scalar parameters
print "\nSummarizing distribution of numerical parameters... ";
if (! more_recent_than("Results/Report","1.p")) {
    `statreport 2: --skip=$burnin $max_arg < 1.p > Results/Report 2>/dev/null`;
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

    # These initial alignments should never change!
    if (! -e "Results/Work/$name-unordered.fasta") {
	`alignment-find --first < 1.P$p.fastas > Results/Work/$name-unordered.fasta 2>/dev/null`;
	if ($? && $n_partitions==1) {
	    `alignment-find --first < 1.MAP > Results/Work/$name-unordered.fasta`;
	}
    }
}
print "done.\n";

# 6.5. Compute MUSCLE alignments

if ($muscle) {
print "\nComputing MUSCLE alignments... ";

for(my $i=0;$i<$n_partitions;$i++) {
    my $p = ($i+1);
    my $name = "P$p-muscle";
    if (! more_recent_than("Results/Work/$name-unordered.fasta", "Results/Work/P$p-initial-unordered.fasta")) {
	`muscle -in Results/Work/P$p-initial-unordered.fasta -out Results/Work/$name-unordered.fasta -quiet`;
    }
    push @alignments,$name;
    $alignment_names{$name} = "MUSCLE";

}
print "done.\n";
}

# 6.5. Compute ProbCons alignments

if ($probcons) {
print "\nComputing ProbCons alignments... ";

for(my $i=0;$i<$n_partitions;$i++) {
    my $p = ($i+1);
    my $name = "P$p-probcons";
    if (! more_recent_than("Results/Work/$name-unordered.fasta", "Results/Work/P$p-initial-unordered.fasta")) {
	`probcons Results/Work/P$p-initial-unordered.fasta > Results/Work/$name-unordered.fasta 2>/dev/null`;
    }
    push @alignments,$name;
    $alignment_names{$name} = "ProbCons";

}
print "done.\n";
}

# 6.7 Compute maximum (weighted posterior decoding) alignments

print "\nComputing WPD alignments... ";

for(my $i=0;$i<$n_partitions;$i++) 
{
    next if ($imodel_indices[$i] == -1);

    my $p = $i+1;
    my $infile = "1.P$p.fastas";

    my $name = "P$p-max";
    if (! more_recent_than("Results/Work/$name-unordered.fasta",$infile)) {
	`cut-range --skip=$burnin $size_arg < $infile | alignment-max> Results/Work/$name-unordered.fasta 2>/dev/null`;
    }
    push @alignments,$name;
    $alignment_names{$name} = "Best (WPD)";
    push @AU_alignments,$name;
}

print "done.\n";

# 7. Compute consensus-alignments
my @alignment_consensus_values = sort(0.1,0.25,0.5,0.75);

for(my $i=0;$i<$n_partitions;$i++)
{
    next if ($imodel_indices[$i] == -1);

    my $p = $i+1;
    my $infile = "1.P$p.fastas";

    print " Partition $p: Computing consensus alignments: \n   ";
    for my $cvalue (@alignment_consensus_values) {
	my $value = $cvalue*100;
	my $name = "P$p-consensus-$value";
	print "c$value ";
	if (! more_recent_than("Results/Work/$name-unordered.fasta",$infile)) {
	    `cut-range --skip=$burnin $size_arg < $infile | alignment-consensus --cutoff=$cvalue> Results/Work/$name-unordered.fasta 2>/dev/null`;
	}
	push @alignments,$name;
	$alignment_names{$name} = "$value% consensus";
    }
    print "done.\n\n";
    push @AU_alignments,"P$p-consensus-10";
}

print "Drawing alignments... ";
for my $alignment (@alignments) 
{
    if (! more_recent_than("Results/$alignment.fasta","Results/Work/$alignment-unordered.fasta")) {
    `alignment-reorder Results/Work/$alignment-unordered.fasta Results/c50.tree > Results/$alignment.fasta 2>/dev/null`;
    }

    if (! more_recent_than("Results/$alignment.html","Results/$alignment.fasta")) {
	`alignment-draw Results/$alignment.fasta --no-legend --show-ruler --color-scheme=DNA+contrast > Results/$alignment.html 2>/dev/null`;

	if ($?) {
	    `alignment-draw Results/$alignment.fasta --no-legend --show-ruler --color-scheme=AA+contrast > Results/$alignment.html 2>/dev/null`;
	}
    }

}
print "done.\n";

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
	`alignment-draw Results/$alignment.fasta --no-legend --show-ruler --AU Results/$alignment-AU.prob --color-scheme=DNA+contrast+fade+fade+fade+fade > Results/$alignment-AU.html 2>/dev/null`;
	if ($?) {
	`alignment-draw Results/$alignment.fasta --no-legend --show-ruler --AU Results/$alignment-AU.prob --color-scheme=AA+contrast+fade+fade+fade+fade > Results/$alignment-AU.html`;
	}
    }
}

# 9. Estimate marginal likelihood
print "Calculating marginal likelihood... ";
my $temp = $burnin+2;
if (!more_recent_than("Results/Pmarg","1.p")) {
`stats-select likelihood --no-header < 1.p | tail -n +$temp | model_P > Results/Pmarg 2>/dev/null`;
}
print "done.\n";
my $marginal_prob = `cat Results/Pmarg`;



# 10. Mixing diagnostics -- block bootstrap

if (!more_recent_than("Results/partitions","Results/consensus")) {
    `pickout --no-header --large pi < Results/consensus > Results/partitions`;
}
if (!more_recent_than("Results/partitions.pred","Results/partitions")) {
    `sed "s/\$/\\n/" < Results/partitions > Results/partitions.pred`;
}

if (!more_recent_than("Results/partitions.bs","1.trees")) {
    `trees-bootstrap --skip=$burnin $max_arg 1.trees --pred Results/partitions.pred > Results/partitions.bs`;
}

# 11. c-levels.plot - FIXME!

`pickout --no-header LOD full < Results/consensus > Results/c-levels.plot`;
`gnuplot <<EOF
set terminal svg
set output "Results/c-levels.svg"
set xlabel "Log10 posterior Odds (LOD)"
set ylabel "Supported Partitions"
plot [0:][0:] 'Results/c-levels.plot' with lines
EOF`;

# 12. Mixing diagnostics - SRQ plots
my @SRQ = ();

print "Generate SRQ plot for partitions ... ";
if (!more_recent_than("Results/partitions.SRQ","1.trees")) {
`trees-to-SRQ Results/partitions.pred --skip=$burnin $max_arg < 1.trees > Results/partitions.SRQ`;
}
print "done.\n";

push @SRQ,"partitions";

print "Generate SRQ plot for c50 tree ... ";
if (!more_recent_than("Results/c50.SRQ","1.trees")) {
`trees-to-SRQ Results/c50.topology --skip=$burnin $max_arg < 1.trees > Results/c50.SRQ`;
}
print "done.\n";

push @SRQ,"c50";

for my $srq (@SRQ) {
`gnuplot <<EOF
set terminal png size 800,600
set output "Results/$srq.SRQ.png"
set key right bottom
set xlabel "Regenerations (fraction)"
set ylabel "Time (fraction)"
set title "Scaled Regeneration Quantile (SRQ) plot: $srq"
plot 'Results/$srq.SRQ' title "$srq" with linespoints 1, x title "Goal" lw 2 lt 3
EOF
`;
}

# 13. Get # of topologies sampled

my $n_topologies = `pickout n_topologies -n < Results/consensus`;

my $n_topologies_95;

open CONSENSUS, "Results/consensus";
while(my $line = <CONSENSUS>) {
    if ($line =~ /contains ([^ ]+) topologies/) {
	$n_topologies_95 = $1;
	last;
    }
}

# 14. Traceplots for scalar variables
open VARS, "1.p";
my $header = <VARS>;
chomp $header;
my @var_names = split(/\t/,$header);
close VARS;

open REPORT, "Results/Report";

my %median = ();
my %CI_low = ();
my %CI_high = ();
my %ACT = ();
my %Ne = ();

print "Generating trace-plots ... ";

while (my $line = <REPORT>) {
    chomp $line;
    next if ($line eq "");

    if ($line =~ /\s+(.+) ~ (.+)\s+\((.+),(.+)\)/) 
    {
	my $var = $1;
	$median{$var} = $2;
	$CI_low{$var} = $3;
	$CI_high{$var} = $4;
	$line = <REPORT>;

	$line =~ /t @ (.+)\s+Ne = (.+)/;
	$ACT{$var} = $1;
	$Ne{$var} = $2;
    }
    elsif ($line =~ /\s+(.+) = (.+)/) {
	$median{$1} = $2;
    }
}

my $Nmax = 5000;

for(my $i=1;$i<= $#var_names; $i++) 
{
    next if (more_recent_than("Results/$i.trace.png","1.p"));

    my $var = $var_names[$i];
    next if (!defined($CI_low{$var}));

    my $file1 = "Results/Work/1.p.$i";
    `stats-select iter '$var' --no-header < 1.p > $file1`;

    my $file2 = $file1.".2";

    my $N = $after_burnin;

    $N = 1000 if ($N > 1000);

    my $factor = ceil($after_burnin / $N);

    `subsample --skip=$burnin $factor < $file1 > $file2`;

    `gnuplot <<EOF
set terminal png size 800,600
set output "Results/$i.trace.png"
set key right bottom
set xlabel "Iteration"
set ylabel "$var"
plot '$file2' title '$var' with lines
EOF`;
}

print "done\n";
#------------------------- Print Index -----------------------#

open INDEX,">Results/index.html";

my $title = "MCMC Post-hoc Analysis";

print INDEX '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
';
print INDEX "<title>BAli-Phy: $title</title>\n";
print INDEX 
'    <style type="text/css">
      ol li {padding-bottom:0.5em}

      td {padding: 0.1em;}
      td {padding-left: 0.3em;}
      td {padding-right: 0.3em;}

      .backlit td {background: rgb(220,220,220);}

      h1 {font-size: 150%;}
      h2 {font-size: 130%; margin-bottom: 0.2em;}
      h3 {font-size: 110%; margin-top: 0.3em ; margin-bottom: 0.2em}

      ul {margin-top: 0.4em;}
    </style>';


print INDEX "<h1>$title</h2>\n";

print INDEX "<p>Samples were created by the following command line:";
print INDEX "<p><b>command line:</b> $command</p>\n";
print INDEX "<p><b>directory:</b> $directory</p>\n";
print INDEX "<p><b>subdirectory:</b> $subdir</p>\n";

print INDEX "<h2>Data</h2>\n";
print INDEX "<table class=\"backlit\">\n";
print INDEX "<tr><th>Partition</th><th>Sequences</th><th>Substitution&nbsp;Model</th><th>Indel&nbsp;Model</th></tr>\n";
for(my $p=0;$p<=$#partitions;$p++) 
{
    print INDEX "<tr>\n";
    print INDEX " <td>".($p+1)."</td>\n";
    print INDEX " <td>$partitions[$p]</td>\n";
    my $smodel = sanitize_smodel( $smodels[$smodel_indices[$p]] );
    print INDEX " <td>$smodel</td>\n";
    my $imodel ="none";
    $imodel = $imodels[$imodel_indices[$p]] if ($imodel_indices[$p] != -1);
    print INDEX " <td>$imodel</td>\n";
    print INDEX "</tr>\n";
}

print INDEX "</table>\n";

print INDEX "<h2>Analysis</h2>\n";
print INDEX '<table style="width:100%;"><tr>'."\n";
print INDEX "<td>burn-in = $burnin samples</td>\n";
print INDEX "<td>after burnin = $after_burnin samples</td>\n";
print INDEX "<td>sub-sample = $subsample</td>\n" if ($subsample != 1);
print INDEX "</tr><tr>\n";
print INDEX "<td>$marginal_prob</td>\n";
print INDEX "</tr><tr>\n";
print INDEX "<td>Complete sample: $n_topologies topologies</td>\n";
print INDEX "<td>95% Bayesian credible interval: $n_topologies_95 topologies</td>\n";
print INDEX "</tr></table>\n";



print INDEX "<h2>Parameter Distribution</h2>\n";
print INDEX "<ul><li><a href=\"Report\">Summary</a></li></ul>\n";

print INDEX "<h2>Phylogeny Distribution</h2>\n";

print INDEX "  </head>\n  <body>\n";

print INDEX '<table style="width:100%;"><tr>'."\n";
print INDEX "<td>Partition support: <a href=\"consensus\">Summary</a></td>\n";
print INDEX "<td>Partition support graph: <a href=\"c-levels.svg\">SVG</a></td>\n";
print INDEX "</tr></table>\n";

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
    if (-f "Results/$tree-mctree.pdf") {
	print INDEX "<td><a href=\"$tree-mctree.pdf\">MC Tree (PDF)</a></td>";
    }
    else {
	print INDEX "<td></td>"     
    }
    if (-f "Results/$tree-mctree.svg") {
	print INDEX "<td><a href=\"$tree-mctree.svg\">MC Tree (SVG)</a></td>";
    }
    else {
	print INDEX "<td></td>"     
    }
    print INDEX "</tr>";
}
print INDEX "</table>\n";

print INDEX "<h2>Alignment Distribution</h2>\n";

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

print INDEX "<h2>Mixing: Topologies</h2>\n";

print INDEX "<ol>\n";
print INDEX "<li><a href=\"partitions.bs\">Partition uncertainty</a></li>\n";
for my $srq (@SRQ) {
    print INDEX "<li><a href=\"$srq.SRQ.png\">SRQ plot: $srq</a></li>\n";
}
print INDEX "</ol>\n";

print INDEX "<h2>Scalar variables</h2>\n";

print INDEX "<table>\n";
print INDEX "<tr><th>Statistic</th><th>Median</th><th>95% BCI</th><th>ACT</th><th>Ne</th></tr>\n";

my @sne = sort {$a <=> $b} values(%Ne);
my $min_Ne = $sne[0];
print "min_Ne = $min_Ne\n";

for(my $i=1;$i <= $#var_names; $i++) 
{
    my $var = $var_names[$i];

    next if ($var eq "iter");

    print INDEX "<tr>\n";
    print INDEX "<td>$var</td>\n";
    print INDEX "<td>$median{$var}</td>\n";
    if (defined($CI_low{$var})) {
	print INDEX "<td>($CI_low{$var},$CI_high{$var})</td>\n";
	print INDEX "<td>$ACT{$var}</td>\n";
	my $style = "";
	$style = ' style="color:red"' if ($Ne{$var} < 100);
	$style = ' style="color:red"' if ($Ne{$var} <= $min_Ne);
	print INDEX "<td $style>$Ne{$var}</td>\n";
	print INDEX "<td><a href=\"$i.trace.png\">Trace</a></td>\n";
    }
    else {
	print INDEX "<td></td>";
	print INDEX "<td></td>";
	print INDEX "<td></td>";
	print INDEX "<td></td>";
    }
    print INDEX "</tr>\n";
}
print INDEX "</table>\n";


print INDEX "  </body>\n";
print INDEX "</html>\n";
