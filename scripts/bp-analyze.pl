#!/usr/bin/perl -w 

# TODO

# !! For alignment-diff, invert fg/bg based on AU>0.5 (bg=white,fg=color)
#    Would look cool!
#    Or, us color (DNA/AA) only if AU<0.5

# !! How to select alignments from a given temperature level?
#    Annotate alignments with information... ?


# 0. (a) Instead of making symbolic links, just use a variable to determine
#    the correct files to analyze, so that we can handle the single-chain
#    case without creating a lot of useless files.
#    (b) This would also make it easier to handle a lot of output from older
#    versions without using symbolic links.
# 1. Histogram for distribution of each statistic.
# 2. alignment properties (#indels, etc)
# 3. alphabet of each partition
# 4. Somehow print posterior distribution of # of indels (really)
#    and substitutions (really - not just parsimony score?)     
# 5. Include links from top to sections.

use strict;

use POSIX;

my $home = $ENV{'HOME'};

my $out_file;
my $trees_file;
my $parameters_file;
my $n_chains;
my @out_files;
my @tree_files;
my @partition_samples;
my $MAP_file;

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

    open FILE, $out_file or die "Can't open $out_file!";

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

sub get_cmdline_attribute($)
{
    my $attribute = shift;
    my $value;

    local *FILE;

    open FILE, $out_file or die "Can't open $out_file!";

    my @partitions = ();

    my $line = <FILE>;
    {
	if ($line =~ /--$attribute[ =]([^ ]*)$/) {
	    $value = $1;
	    last;
	}
	last if ($line =~ /^iterations = 0/);
    }
    close FILE;

    return $value;
}

sub get_header_attribute($)
{
    my $attribute = shift;
    my $value;

    local *FILE;

    open FILE, $out_file or die "Can't open $out_file!";

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

    open FILE, $out_file or die "Can't open $out_file!";

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

    open FILE, $out_file or die "Can't open $out_file!";

    my @imodels = ();

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

    push @imodels, "none" if ($#imodels == -1);
    return [@imodels];
}

sub get_smodel_indices()
{
    local *FILE;

    open FILE, $out_file or die "Can't open $out_file!";

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

    open FILE, $out_file or die "Can't open $out_file!";

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

sub get_alphabets()
{
    local *FILE;

    open FILE, $out_file or die "Can't open $out_file!";

    my @alphabets = ();

    while (my $line = <FILE>) 
    {
	if ($line =~ /alphabet(.+) = (.+)/) {
	    push @alphabets,$2;
	}
	if ($line =~ /alphabet = (.+)/) {
	    push @alphabets,$1;
	}

	last if ($line =~ /^iterations = 0/);
    }
    close FILE;

    return @alphabets;
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
    return get_n_lines("C1.trees")-1 if (-e 'C1.trees') ;
    return get_n_lines("1.trees")-1  if (-e '1.trees');
    die "Error: I can't find either 1.trees or C1.trees!";
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

sub more_recent_than_all_of($;$)
{
    my $filename1 = shift;
    my $temp = shift;
    my @filenames2 = @$temp;

    foreach my $filename2 (@filenames2) {
	return 0 if (!more_recent_than($filename1,$filename2));
    }

    return 1;
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

sub get_alignment_info 
{
    my $filename = shift;
    open INFO,"alignment-info $filename |";

    my %features = ();

    my $indels = 0;
    while(my $line=<INFO>) {
	if ($line =~ /Alignment: (.+) columns of (.+) sequences/) 
	{
	    $features{"length"} = $1;
	    $features{"n_sequences"} = $2;
	}
	if ($line =~ /sequence lengths: ([^ ]+)-([^ ]+)/) {
	    $features{"min_length"} = $1;
	    $features{"max_length"} = $2;
	}
	if ($line =~ m|w/  indels|) {
	    $indels = 1;
	}
	next if ($indels == 0);

	if ($line =~ / const.: ([^ ]+) \(([^ ]+)\%\)/) {
	    $features{"n_const"} = $1;
	    $features{"p_const"} = $2;
	}
	if ($line =~ /non-const.: ([^ ]+) \(([^ ]+)\%\)/) {
	    $features{"n_non-const"} = $1;
	    $features{"p_non-const"} = $2;
	}
	if ($line =~ /inform.: ([^ ]+) \(([^ ]+)\%\)/) {
	    $features{"n_inform"} = $1;
	    $features{"p_inform"} = $2;
	}
	if ($line =~ / ([^ ]+)% minimum sequence identity/){
	    $features{"min_p_identity"} = $1;
	}
    }
    return {%features};
}

sub tooltip
{
    my $text = shift;
    return "<a title=\"$text\">?</a>";
}

#----------------------------- SETUP --------------------------#
if (-e 'C1.out') 
{
    $out_file = 'C1.out';
    $n_chains = get_header_attribute("MPI_SIZE");
    for(my $i=0;$i<$n_chains;$i++) {
	push @out_files,"C$i.out" if (-e "C$i.out");
	push @tree_files,"C$i.trees" if (-e "C$i.trees");
    }

    if ($n_chains == 1) {
	$trees_file = 'C1.trees';
	die "error: I can't find 'C1.trees'" if (! -e 'C1.trees');

	$parameters_file = 'C1.p';
	die "error: I can't find 'C1.p'" if (! -e 'C1.p');

	$MAP_file = "C1.MAP";
    }
}
elsif (-e '1.out') {
    $out_file = '1.out';
    $trees_file = '1.trees';
    $parameters_file = '1.p';
    die "error: I can't find '1.trees'" if (! -e '1.trees');

    $n_chains = 1;
    @out_files = ( '1.out' );
    @tree_files = ( '1.trees' );

    $MAP_file = "1.MAP";
}
else {
    die "I can't find file '1.out' or 'C1.out' - are you running this in the right directory?";
}

my $command = get_header_attribute("command");
my $directory = get_header_attribute("directory");
my $subdir    = get_header_attribute("subdirectory");

my $betas = get_cmdline_attribute("beta");
my @beta = (1);
@beta = split(/,/, $betas) if (defined($betas));


my @partitions = @{ get_partitions() };
my $n_partitions = 1+$#partitions;
if ($out_file eq "C1.out") {
    if ($n_chains == 1) {
	for(my $p=1;$p<=$n_partitions;$p++) {
	    push @partition_samples,"C1.P$p.fastas";
	}
    }
    else {
	## FIXME - How do we construct these?
	for(my $p=1;$p<=$n_partitions;$p++) {
	    push @partition_samples,"C1.P$p.fastas";
	}
    }
}
else {
    for(my $p=1;$p<=$n_partitions;$p++) {
	push @partition_samples,"1.P$p.fastas" if (-e "1.P$p.fastas");
    }
    @partition_samples = ("1.out") if ($#partition_samples == -1);
}


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

my @alphabets = get_alphabets();

$burnin = int 0.1*$n_iterations if (!defined($burnin));
my $after_burnin = $n_iterations - $burnin +1;

my @trees = ();
my %tree_name = ();

my @tree_consensus_values = sort(0.5,0.66,0.8,0.9,0.95,0.99,1.0);

#

my $max_iter;
my $subsample = 1;
my $min_support;
my $muscle = 0;
my $probcons = 0;

while ($#ARGV > -1) 
{
    my $arg = shift;
    if ($arg eq "clean" || $arg eq "--clean") {
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

# 0. Compute T1.p and T1.trees

if ($n_chains > 1) 
{
    for(my $i=1;$i<=$n_chains;$i++)
    {
	# Construct C1.pt
	# Construct C1Ti.pt
	# Construct C1Ti.p
	# Construct C1Ti.trees
	next if (! -e "C$i.trees" );

	if (! more_recent_than("Results/C$i.t","C$i.trees"))
	{
	    `echo "tree" > Results/C$i.t`;
	    `cat C$i.trees >> Results/C$i.t`;
	}
	
	if (! more_recent_than_all_of("Results/C$i.pt",["C$i.trees","C$i.p"]))
	{
	    `stats-merge C$i.p Results/C$i.t > Results/C$i.pt 2>/dev/null`;
	}

	if (! more_recent_than("Results/C${i}T1.pt","Results/C$i.pt")) 
	{
	    my $use_header = "";
	    $use_header = "--no-header" if ($i != 1);

	    `subsample --header --skip=$burnin < Results/C$i.pt | stats-select -s beta=1 $use_header > Results/C${i}T1.pt`;
	}
    }

    my $cmd = "cat ";
    my $rerun=0;
    for(my $i=1;$i<=$n_chains;$i++) {
	if (-e "Results/C${i}T1.pt") {
	    $cmd = "$cmd Results/C${i}T1.pt ";
	    $rerun=1 if (! more_recent_than("Results/T1.p","Results/C${i}T1.pt"));
	}
    }
    $cmd = "$cmd > Results/T1.pt";
    `$cmd` if ($rerun);

    if (! more_recent_than("Results/T1.trees","Results/T1.pt")) {
	`stats-select tree --no-header < Results/T1.pt > Results/T1.trees`;
    }
    
    if (! more_recent_than("Results/T1.p","Results/T1.pt")) {
	`stats-select -r tree < Results/T1.pt > Results/T1.p`;

#       This messes up the printing of statistics
#	`stats-select -i -r tree < Results/T1.pt > Results/T1.p`;

    }

    $trees_file = "Results/T1.trees";
    $parameters_file = "Results/T1.p";
}


# 1. compute consensus trees
my $max_arg = "";
$max_arg = "--max=$max_iter" if (defined($max_iter));
my $min_support_arg = "";
$min_support_arg = "--min-support=$min_support" if (defined($min_support));
my $consensus_arg = "--consensus=".join(',',@tree_consensus_values);
my $size_arg = "";
$size_arg = "--size=$max_iter" if defined($max_iter);

print "Summarizing topology distribution ... ";
if (! more_recent_than("Results/consensus",$trees_file)) {
    my $skip="";
    $skip="--skip=$burnin" if ($trees_file eq "Results/T1.trees");
    `trees-consensus $trees_file $max_arg $min_support_arg --sub-partitions $consensus_arg > Results/consensus`;
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

    if (! more_recent_than("Results/$tree-mctree.svg","Results/$tree.mtree")) {
	`draw-tree Results/$tree.mtree --out=Results/$tree-mctree --output=svg`;
    }
    if (! more_recent_than("Results/$tree-mctree.pdf","Results/$tree.mtree")) {
	`draw-tree Results/$tree.mtree --out=Results/$tree-mctree`;
    }
    
    print "$tree ";
    if (! more_recent_than("Results/$tree.ltree",$trees_file)) {
    `tree-mean-lengths Results/$tree.topology --safe --show-node-lengths $max_arg < $trees_file > Results/$tree.ltree`;
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
if (! more_recent_than("Results/MAP.ltree",$trees_file)) {
    `tree-mean-lengths Results/MAP.topology --safe $max_arg < $trees_file > Results/MAP.ltree`;
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
	`cd Results ; draw-tree $tree.ltree --layout=equal-daylight`;
    }
    if (! more_recent_than("Results/$tree-tree.svg","Results/$tree.ltree")) {
	`cd Results ; draw-tree $tree.ltree --layout=equal-daylight --output=svg`;
    }
}
print "done.\n";

# 5. Summarize scalar parameters
print "\nSummarizing distribution of numerical parameters... ";
if (! more_recent_than("Results/Report",$parameters_file)) {
    `statreport 2: $max_arg < $parameters_file > Results/Report`;
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
    if (! -s "Results/Work/$name-unordered.fasta") {
	`alignment-find --first < $partition_samples[$i] > Results/Work/$name-unordered.fasta 2>/dev/null`;
	if ($? && $n_chains==1 && defined($MAP_file)) {
	    `alignment-find --first < $MAP_file > Results/Work/$name-unordered.fasta`;
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
    my $infile = $partition_samples[$i];

    my $name = "P$p-max";
    if (! more_recent_than("Results/Work/$name-unordered.fasta",$infile) ||
	! more_recent_than("Results/Work/$name-unordered.fasta",$infile) ) {
	`cut-range --skip=$burnin $size_arg < $infile | alignment-max> Results/Work/$name-unordered.fasta`;
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
    my $infile = $partition_samples[$i];

    print " Partition $p: Computing consensus alignments: \n   ";
    for my $cvalue (@alignment_consensus_values) {
	my $value = $cvalue*100;
	my $name = "P$p-consensus-$value";
	print "c$value ";
	if (! more_recent_than("Results/Work/$name-unordered.fasta",$infile)) {
	    `cut-range --skip=$burnin $size_arg < $infile | alignment-consensus --cutoff=$cvalue> Results/Work/$name-unordered.fasta`;
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
    if (! more_recent_than("Results/$alignment.fasta","Results/Work/$alignment-unordered.fasta") ||
	! more_recent_than("Results/$alignment.fasta","Results/c50.tree")) {
    `alignment-reorder Results/Work/$alignment-unordered.fasta Results/c50.tree > Results/$alignment.fasta`;
    }

    if (! more_recent_than("Results/$alignment.html","Results/$alignment.fasta")) {
	`alignment-draw Results/$alignment.fasta --show-ruler --color-scheme=DNA+contrast > Results/$alignment.html 2>/dev/null`;

	if ($?) {
	    `alignment-draw Results/$alignment.fasta --show-ruler --color-scheme=AA+contrast > Results/$alignment.html`;
	}
    }
}

for my $alignment (@alignments) 
{
    my $p;
    if ($alignment =~ /^P([^-]+)-/) {
	$p=$1;
	next if ($imodel_indices[$p-1] == -1);
    }
    else {
	next;
    }

    next if ($alignment eq "P$p-max");
    next if (! -e "Results/P$p-max.fasta");

    if (! more_recent_than("Results/$alignment-diff.fasta","Results/$alignment.fasta") || 
	! more_recent_than("Results/$alignment-diff.AU","Results/$alignment.fasta") )
    {
	`alignments-diff Results/$alignment.fasta Results/P$p-max.fasta --merge --fill=unknown -d Results/$alignment-diff.AU > Results/$alignment-diff.fasta`;
    }

    if (! more_recent_than("Results/$alignment-diff.html","Results/$alignment-diff.fasta")) {
	`alignment-draw Results/$alignment-diff.fasta --scale=invert --AU Results/$alignment-diff.AU --show-ruler --color-scheme=Rainbow+fade[1,0]+contrast > Results/$alignment-diff.html`;
    }
}
print "done.\n";

# 8. AU plots

for my $alignment (@AU_alignments) 
{
    if ($alignment =~ /^P([^-]+)-.*/) {
	print "Generating AU values for $alignment... ";
	my $p = $1;
	my $infile = $partition_samples[$p-1];

	if (!more_recent_than("Results/$alignment-AU.prob",$infile)) {
	`cut-range --skip=$burnin $size_arg < $infile | alignment-gild Results/$alignment.fasta Results/MAP.tree --max-alignments=500 > Results/$alignment-AU.prob`;
	}
	print "done.\n";
	`alignment-draw Results/$alignment.fasta --show-ruler --AU Results/$alignment-AU.prob --color-scheme=DNA+contrast+fade+fade+fade+fade > Results/$alignment-AU.html 2>/dev/null`;
	if ($?) {
	`alignment-draw Results/$alignment.fasta --show-ruler --AU Results/$alignment-AU.prob --color-scheme=AA+contrast+fade+fade+fade+fade > Results/$alignment-AU.html`;
	}
    }
}

# 9. Estimate marginal likelihood
print "Calculating marginal likelihood... ";

if (!more_recent_than("Results/Pmarg",$parameters_file)) {
`stats-select likelihood --no-header < $parameters_file | model_P > Results/Pmarg`;
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

if (!more_recent_than("Results/partitions.bs",$trees_file)) {
    `trees-bootstrap $max_arg $trees_file --pred Results/partitions.pred > Results/partitions.bs`;
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
if (!more_recent_than("Results/partitions.SRQ",$trees_file)) {
`trees-to-SRQ Results/partitions.pred $max_arg < $trees_file > Results/partitions.SRQ`;
}
print "done.\n";

push @SRQ,"partitions";

print "Generate SRQ plot for c50 tree ... ";
if (!more_recent_than("Results/c50.SRQ",$trees_file)) {
`trees-to-SRQ Results/c50.topology $max_arg < $trees_file > Results/c50.SRQ`;
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
open VARS, $parameters_file;
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
    next if (more_recent_than("Results/$i.trace.png",$parameters_file));

    my $var = $var_names[$i];
    next if (!defined($CI_low{$var}));

    my $file1 = "Results/Work/T1.p.$i";
    `stats-select iter '$var' --no-header < $parameters_file > $file1`;

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

my $p1_features = get_alignment_info("Results/P1-initial.fasta");

my $n_sequences = ${$p1_features}{"n_sequences"};

open INDEX,">Results/index.html";

my $title = "MCMC Post-hoc Analysis: $n_sequences sequences";

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

      *[title] {cursor:help;}
      a[title] {vertical-align:top;color:#00f;font-size:70%; border-bottom:1px dotted;}
    </style>';


print INDEX "<h1>$title</h2>\n";

print INDEX "<p>Samples were created by the following command line:";
print INDEX "<p><b>command line:</b> $command</p>\n";
print INDEX "<p><b>directory:</b> $directory</p>\n";
print INDEX "<p><b>subdirectory:</b> $subdir</p>\n" if (defined($subdir));


print INDEX "<h2 name=\"data\">Data</h2>\n";
print INDEX "<table class=\"backlit\">\n";
print INDEX "<tr><th>Partition</th><th>Sequences</th><th>Lengths</th><th>Substitution&nbsp;Model</th><th>Indel&nbsp;Model</th></tr>\n";
for(my $p=0;$p<=$#partitions;$p++) 
{
    print INDEX "<tr>\n";
    print INDEX " <td>".($p+1)."</td>\n";
    print INDEX " <td>$partitions[$p]</td>\n";
    my $features = get_alignment_info("Results/P".($p+1)."-initial.fasta");
    my $min = $features->{'min_length'};
    my $max = $features->{'max_length'};
    my $alphabet = $alphabets[$p];
    print INDEX " <td>$min - $max $alphabet</td>\n";
    my $smodel = sanitize_smodel( $smodels[$smodel_indices[$p]] );
    print INDEX " <td>$smodel</td>\n";
    my $imodel ="none";
    $imodel = $imodels[$imodel_indices[$p]] if ($imodel_indices[$p] != -1);
    print INDEX " <td>$imodel</td>\n";
    print INDEX "</tr>\n";
}

print INDEX "</table>\n";

print INDEX "<h2 name=\"analysis\">Analysis</h2>\n";
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

print INDEX "<h2 name=\"topology\">Phylogeny Distribution</h2>\n";

print INDEX "  </head>\n  <body>\n";

print INDEX '<table style="width:100%;"><tr>'."\n";
print INDEX "<td>Partition support: <a href=\"consensus\">Summary</a></td>\n";
print INDEX "<td><span title=\"How many partitions are supported at each level of Posterior Log Odds (LOD)?\">Partition support graph:</span> <a href=\"c-levels.svg\">SVG</a></td>\n";
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

print INDEX "<h2 name=\"alignment\">Alignment Distribution</h2>\n";

for(my $i=0;$i<$n_partitions;$i++) 
{
    my $p = $i+1;
    print INDEX "<h3>Partition $p</h3>\n";
    print INDEX "<table>\n";
    print INDEX "<tr>\n";
    print INDEX "<th></th>\n";
    print INDEX "<th></th>\n";
    print INDEX "<th></th>\n";
    print INDEX "<th title=\"Comparison of this alignment (top) to the WPD alignment (bottom)\">Diff</th>\n";
    print INDEX "<th></th>\n";
    print INDEX "<th style=\"padding-right:0.5em;padding-left:0.5em\" title=\"Percent identity of the most dissimilar sequences\">Min. %identity</th>\n";
    print INDEX "<th style=\"padding-right:0.5em;padding-left:0.5em\" title=\"Number of columns in the alignment\"># Sites</th>\n";
    print INDEX "<th style=\"padding-right:0.5em;padding-left:0.5em\" title=\"Number of invariant columns\">Constant</th>\n";
    print INDEX "<th style=\"padding-right:0.5em;padding-left:0.5em\" title=\"Number of variant columns\">Variable</th>\n";
    print INDEX "<th title=\"Number of parsiomny-informative columns.\">Parsimony-Informative</th>\n";
    print INDEX "</tr>\n";
    for my $alignment (@alignments)
    {
	next if ($alignment !~ /^P$p-/);
	my $name = $alignment_names{$alignment};
	my $features = get_alignment_info("Results/$alignment.fasta");

	print INDEX "<tr>\n";
	print INDEX "<td>$name</td>\n";
	print INDEX "<td><a href=\"$alignment.fasta\">FASTA</a></td>\n";
	if (-f "Results/$alignment.html") {
	    print INDEX "<td><a href=\"$alignment.html\">HTML</a></td>\n";
	}
	else {
	    print INDEX "<td></td>\n";
	}
	if (-f "Results/$alignment-diff.html") {
	    print INDEX "<td><a href=\"$alignment-diff.html\">Diff</a></td>\n";
	}
	else {
	    print INDEX "<td></td>\n";
	}
	if (-f "Results/$alignment-AU.html") {
	    print INDEX "<td><a href=\"$alignment-AU.html\">AU</a></td>\n";
	}
	else {
	    print INDEX "<td></td>\n";
	}
	print INDEX "<td style=\"text-align: center\">${$features}{'min_p_identity'}%</td>\n";
	print INDEX "<td style=\"text-align: center\">${$features}{'length'}</td>\n";
	print INDEX "<td style=\"text-align: center\">${$features}{'n_const'} (${$features}{'p_const'}%)</td>\n";
	print INDEX "<td style=\"text-align: center\">${$features}{'n_non-const'} (${$features}{'p_non-const'}%)</td>\n";
	print INDEX "<td style=\"text-align: center\">${$features}{'n_inform'} (${$features}{'p_inform'}%)</td>\n";
	print INDEX "</tr>\n";
    }
    print INDEX "</table>\n";
}

print INDEX "<h2 name=\"topology-mixing\">Mixing: Topologies</h2>\n";

print INDEX "<ol>\n";
print INDEX "<li><a href=\"partitions.bs\">Partition uncertainty</a></li>\n";
for my $srq (@SRQ) {
    print INDEX "<li><a href=\"$srq.SRQ.png\">SRQ plot: $srq</a></li>\n";
}
print INDEX "</ol>\n";

print INDEX "<h2 name=\"parameters\">Scalar variables</h2>\n";

print INDEX "<table>\n";
print INDEX "<tr><th>Statistic</th><th>Median</th><th title=\"95% Bayesian Credible Interval\">95% BCI</th><th title=\"Auto-Correlation Time\">ACT</th><th title=\"Effective Sample Size\">Ne</th></tr>\n";

my @sne = sort {$a <=> $b} values(%Ne);
my $min_Ne = $sne[0];
print "\nNOTE: min_Ne = $min_Ne\n";

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
