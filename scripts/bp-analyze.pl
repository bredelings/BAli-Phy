#!/usr/bin/perl -w 

# TODO

# !! For alignment-diff, invert fg/bg based on AU>0.5 (bg=white,fg=color)
#    Would look cool!
#    Or, us color (DNA/AA) only if AU<0.5

# !! How to select alignments from a given temperature level?
#    Annotate alignments with information... ?


# 1. Histogram for distribution of each statistic.
# 2. alignment properties (#indels, etc)
# 3. alphabet of each partition
# 4. Somehow print posterior distribution of # of indels (really)
#    and substitutions (really - not just parsimony score?)     
# 5. Include links from top to sections.

# FIX: Handling different temperatures is probably broken:
#      + Treat every chain as a separate run.
#      + However, select out the samples with beta=1
# PROBLEM: Our output format of separate files makes it difficult to 
#            selecting trees & alignments with a given temperature.
#          I suppose we could write out a list of iterations to select, and then
#            select them using that.
#
#          This may be related to the fact that it is hard to subsample alignments
#          because our current format (a giant list) makes it hard to tag them with info.


use strict;

use warnings;

use Carp;

use POSIX;

my $home = $ENV{'HOME'};

my $verbose = 0;

#----------------------------- SETUP 1 --------------------------#

if (! is_in_path("trees-consensus")) {
    print "I can't find the program 'trees-consensus' in your PATH!\n";
    print "See the manual for adding the bali-phy programs to your PATH\n";
    exit(1);
}

my $have_draw_tree = 1;

if (! is_in_path("draw-tree")) {
    print "Program 'draw-tree' not found.  Tree pictures will not be generated.\n\n";
    $have_draw_tree = 0;
}

my $have_gnuplot = 1;

if (! is_in_path("gnuplot")) {
    print "Program 'gnuplot' not found.  Trace plots will not be generated.\n\n";
    $have_gnuplot = 0;
}

my $have_R = 1;

if (! is_in_path("R")) {
    print "Program 'R' not found.  Some mixing graphs will not be generated.\n\n";
    $have_gnuplot = 0;
}

# These things can be different between runs of the MCMC chain
my @subdirectories;      # the name of the directories scanned, as given on the cmd line
my @out_files;
my @tree_files;
my @parameter_files;
my @partition_samples;   # $partition_samples[$chain_number][$partition_number]
my @commands;
my @directories;
my @subdirs;             # the name of the subdirectories, as computed from the runs

# These things are the same between all MCMC chains
my $burnin;
my $MAP_file;
my $personality="";
my @input_file_names;

# These things are option values
my $subsample = 1;
my $muscle = 0;
my $probcons = 0;
my $sub_partitions=0;
my $do_consensus_alignments=0;
my $do_trace_plots=1;
my $prune;
my $speed=1;
my $max_iter;    # maximum number of iterations to consider

# These things are ... ??
my $n_chains=1;
my $min_support;
my $min_Ne;

&parse_command_line();

&determine_personality();

# determine @out_files, @tree_files, @parameter_files
&determine_input_files();

@commands = get_header_attributes("command",@out_files);
@directories = get_header_attributes("directory",@out_files);
@subdirs    = get_header_attributes("subdirectory",@out_files);

my $betas = get_cmdline_attribute("beta");
my @beta = (1);
@beta = split(/,/, $betas) if (defined($betas));
@input_file_names = @{ get_input_file_names() };
my $n_partitions = 1+$#input_file_names;

&determine_alignment_files();

my @n_iterations = get_n_iterations();
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

my %tree_name = ();
# This is necessary to display them in order:
my @trees = (); 

#

my @tree_consensus_values = sort(0.5,0.66,0.8,0.9,0.95,0.99,1.0);

my @alignment_consensus_values;
if ($speed == 0) {
    @alignment_consensus_values = sort(0.1,0.25,0.5,0.75);
}
elsif ($speed == 1) # default
{
    @alignment_consensus_values = (0.5);
}
elsif ($speed == 2) {
    @alignment_consensus_values = ();
}

&determine_burnin();

# create a new directory, decide whether or not to reuse existing directory
&initialize_results_directory();

open LOG, ">>Results/bp-analyze.log";
print LOG "------------------------------------------------\n";

&compute_tree_and_parameter_files_for_heated_chains();

# 5. Summarize scalar parameters
my $max_arg = "";
$max_arg = "--max=$max_iter" if (defined($max_iter));

my $skip="";
$skip="--skip=$burnin" if ($tree_files[0] ne "Results/T1.trees");

my $subsample_string = "--sub-sample=$subsample";
$subsample_string = "" if ($subsample == 1);

if ($personality =~ "bali-phy.*" || $personality eq "beast" || $personality eq "phylobayes") {
    print "Summarizing distribution of numerical parameters...";
    if (! more_recent_than_all_of("Results/Report",[@parameter_files])) {
	exec_show("statreport $subsample_string $max_arg $skip @parameter_files > Results/Report");
    }
    print "done.\n";
}
    
# 1. compute consensus trees
my $min_support_arg = "";
$min_support_arg = "--min-support=$min_support" if (defined($min_support));

my $consensus_no_pp_arg = "--consensus=". get_consensus_arg("tree",\@tree_consensus_values);
my $consensus_pp_arg = "--consensus-PP=". get_consensus_arg("PP.tree",\@tree_consensus_values);
my $e_consensus_arg = "";
$e_consensus_arg = "--extended-consensus=". get_consensus_arg("mtree",\@tree_consensus_values) if ($sub_partitions);
my $el_consensus_arg = "";
$el_consensus_arg = "--extended-consensus-L=". get_consensus_arg("mlengths",\@tree_consensus_values) if ($sub_partitions);
my $consensus_arg = "$consensus_no_pp_arg $consensus_pp_arg $e_consensus_arg $el_consensus_arg";

my $size_arg = "";
$size_arg = "--size=$max_iter" if defined($max_iter);
my $prune_arg = "";
my $prune_arg2 = "";

print "\nSummarizing topology distribution ... ";
if (-z "Results/consensus" || ! more_recent_than_all_of("Results/consensus",[@tree_files])) {
    my $sub_string = "--sub-partitions";
    $sub_string = "" if (!$sub_partitions);
    $prune_arg = "--ignore $prune" if (defined($prune));

    my $select_trees_arg = "$max_arg $skip $subsample_string $prune_arg";
    my $levels_arg = "--support-levels=Results/c-levels.plot";
    $levels_arg = "$levels_arg --extended-support-levels=Results/extended-c-levels.plot" if ($sub_partitions);
    exec_show("trees-consensus @tree_files $select_trees_arg $min_support_arg $sub_string $consensus_arg $levels_arg --map-tree=Results/MAP.tree --greedy-consensus=Results/greedy.tree --report=Results/consensus");
}
print "done.\n";


print "\nComputing mean branch lengths ... ";
    for my $cvalue (@tree_consensus_values)
    {
	my $value = $cvalue*100;
	
	my $tree = "c$value";

	# No node lengths???
	my $filename1 = "Results/$tree.tree";
	my $filename2 = "Results/$tree.mtree";
	
        # Generate trees w/ node lengths.
	if (! more_recent_than("Results/$tree.ltree",$tree_files[0])) 
	{
	    $prune_arg2 = "--prune $prune" if (defined($prune));
	    exec_show("tree-mean-lengths Results/$tree.tree --safe --show-node-lengths $max_arg $skip $subsample_string $prune_arg2 < $tree_files[0] > Results/$tree.ltree");
 	}
    }
print "done.\n";



# 2. Draw trees

for my $cvalue (@tree_consensus_values)
{
    my $value = $cvalue*100;
    my $tree = "c$value";
    $tree_name{$tree} = "$value\% consensus";
    push @trees,$tree;
}
push @trees, "MAP";
$tree_name{"MAP"} = "MAP";

push @trees, "greedy";
$tree_name{"greedy"} = "greedy";

&draw_trees();

&mixing_diagnostics();

my @SRQ = &SRQ_plots();

my @alignments = ();
my %alignment_names = ();
my @AU_alignments = ();

&compute_initial_alignments();

&compute_muscle_alignment();

&compute_probcons_alignment();

&compute_wpd_alignments();

&compute_consensus_alignments() if ($do_consensus_alignments);

&draw_alignments();

&compute_and_draw_AU_plots();

my $marginal_prob = &compute_marginal_likelihood();

# 13. Get # of topologies sampled

my $n_topologies = exec_show("pickout n_topologies -n < Results/consensus");

my $n_topologies_95;

open CONSENSUS, "Results/consensus";
while(my $line = <CONSENSUS>) {
    if ($line =~ /contains ([^ ]+) topologies/) {
	$n_topologies_95 = $1;
	last;
    }
}
close CONSENSUS;

# 14. Traceplots for scalar variables

my @var_names = ();
my %median = ();
my %CI_low = ();
my %CI_high = ();
my %ACT = ();
my %Ne = ();
my %Burnin = ();

&generate_trace_plots();

&print_index_html();

#---------------------------------------- end execution ---------------------------

sub get_n_sequences
{
    if ($personality =~ "bali-phy.*")
    {
	my $p1_features = get_alignment_info("Results/P1-initial.fasta");

	return ${$p1_features}{"n_sequences"};
    }
    elsif ($personality eq "phylobayes")
    {
	return get_value_from_file($out_files[0],"number of taxa :");
    }
    else {
	return undef;
    }
}

sub html_header
{
    my $title = shift;
    my $section = "";
    $section .= '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
';
    $section .= "<title>BAli-Phy: $title</title>\n";
    $section .= 
'    <style type="text/css">
      ol li {padding-bottom:0.5em}

      td {padding: 0.1em;}
      td {padding-left: 0.3em;}
      td {padding-right: 0.3em;}

#topbar {
	background-color: rgb(201,217,233);
	margin: 0;
	width: 99%;
	padding: 0.2em 0.2em 0.5em 0.2em; /* .5 is on the bottom */
	display: table;
	border: none; /* only needed for Netscape 4.x */
        width: 100%;
}

#topbar #menu {
	font-size: 90%;
	display: table-cell;
	text-align: right; 
}
#topbar #path {
	font-weight: bold;
	display: table-cell;
	text-align: left; 
	white-space: nowrap;
/*	whitespace: nowrap; only for Netscape 4.x, but causes invalidation */
}
      .backlit td {background: rgb(220,220,220);}

      h1 {font-size: 150%;}
      h2 {font-size: 130%; margin-bottom: 0.2em;}
      h3 {font-size: 110%; margin-top: 0.3em ; margin-bottom: 0.2em}

      ul {margin-top: 0.4em;}

      *[title] {cursor:help;}
      a[title] {vertical-align:top;color:#00f;font-size:70%; border-bottom:1px dotted;}
      .floating_picture {float:left;padding-left:0.5em;padding-right:0.5em;margin:0.5em;}    
      .r_floating_picture {float:right;padding-left:0.5em;padding-right:0.5em;margin:0.5em;}    
      .clear {clear:both;}    
    </style>
</head>
';
    return $section;
}

sub topbar
{
    return '<p id="topbar"> 
   <span id="path">Sections:</span>
   <span id="menu">
    [<a href="#data">Data+Model</a>]
    [<a href="#analysis">Analysis</a>]
    [<a href="#alignment">Alignment</a>]
    [<a href="#topology">Phylogeny</a>]
    [<a href="#mixing">Mixing</a>]
    [<a href="#parameters">Parameters</a>]
   </span>
</p>
';
}

sub print_data_and_model
{
    my $section = "";
    $section .= "<h2 style=\"clear:both\"><a name=\"data\">Data &amp; Model</a></h2>\n";
    $section .= "<table class=\"backlit\">\n";
    $section .= "<tr><th>Partition</th><th>Sequences</th><th>Lengths</th><th>Substitution&nbsp;Model</th><th>Indel&nbsp;Model</th></tr>\n";
    for(my $p=0;$p<=$#input_file_names;$p++) 
    {
	$section .= "<tr>\n";
	$section .= " <td>".($p+1)."</td>\n";
	$section .= " <td>$input_file_names[$p]</td>\n";

	my $features = get_alignment_info("Results/P".($p+1)."-initial.fasta");
	my $min = $features->{'min_length'};
	my $max = $features->{'max_length'};
	my $alphabet = $alphabets[$p];

	$section .= "<td>";
	$section .= "$min - $max" if defined ($min);
	$section .= "$alphabet" if ($personality =~ "bali-phy.*");
	$section .= "</td>\n";

	if ($personality =~ "bali-phy.*")
	{
	    my $smodel = sanitize_smodel( $smodels[$smodel_indices[$p]] );

	    $section .= " <td>$smodel</td>\n";
	}
	else
	{
	    $section .= " <td></td>\n";
	}

	my $imodel ="none";

	$imodel = $imodels[$imodel_indices[$p]] if ($imodel_indices[$p] != -1);
	$section .= " <td>$imodel</td>\n";
	$section .= "</tr>\n";
    }
    
    $section .= "</table>\n";

    return $section;
}

sub section_analysis
{
    my $section = "";
$section .= "<h2><a name=\"analysis\">Analysis</a></h2>\n";
$section .= '<table style="width:100%">'."\n";

$section .= "<tr>\n";
$section .= "  <td>burn-in = $burnin samples</td>\n";
$section .= "  <td>sub-sample = $subsample</td>\n" if ($subsample != 1);
$section .= "  <td>$marginal_prob</td>\n";
$section .= "</tr>\n";
$section .= "</table>\n";

$section .= '<table style="width:100%">'."\n";
$section .= "<tr>\n";
$section .= "  <td>Complete sample: $n_topologies topologies</td>\n";
$section .= "  <td>95% Bayesian credible interval: $n_topologies_95 topologies</td>\n";
$section .= "</tr>\n";

$section .= "</table>\n";

$section .= '<table class="backlit">'."\n";
$section .= "<tr><th>chain #</th><th>Iterations (after burnin)</th></tr>\n";
    for(my $i=0;$i<=$#out_files;$i++)
    {
$section .= "<tr>\n";
$section .= "  <td>".($i+1)."</td>\n";
my $after_burnin = $n_iterations[$i] - $burnin;
$section .= "  <td>$after_burnin samples</td>\n";
$section .= "</tr>\n";
    }
$section .= "</table>\n";


    return $section;
}

sub html_svg_object
{
    my $url = shift;
    my $width = shift;
    my $height = shift;
    my $classes = shift;
    my $extra = shift;

    $classes = [] if (!defined($classes));
    push @$classes,"svg-image";
    my $class = join(' ',@$classes);

    my $svg = "<object data=\"$url\" type=\"image/svg+xml\" class=\"$class\"";

    $svg = "$svg width=\"$width\"" if (defined($width));
    $svg = "$svg height=\"$height\"" if (defined($height));
    $svg = "$svg $extra" if (defined($extra));
    $svg = "$svg ></object>";

    return $svg;
}

sub html_svg_img
{
    my $url = shift;
    my $width = shift;
    my $height = shift;
    my $classes = shift;
    my $extra = shift;

    $classes = [] if (!defined($classes));
    push @$classes,"svg-image";
    my $class = join(' ',@$classes);

    my $svg = "<img src=\"$url\" class=\"$class\"";

    $svg = "$svg width=\"$width\"" if (defined($width));
    $svg = "$svg height=\"$height\"" if (defined($height));
    $svg = "$svg $extra" if (defined($extra));
    $svg = "$svg />";

    return $svg;
}

sub html_svg
{
    return html_svg_img(@_);
}

sub section_phylogeny_distribution
{
    my $section = "";
    $section .= "<h2><a name=\"topology\">Phylogeny Distribution</a></h2>\n";

    $section .= html_svg("c-levels.svg","400pt","300pt",["r_floating_picture"]);

    $section .= '<table>'."\n";
    $section .= "<tr><td>Partition support: <a href=\"consensus\">Summary</a></td></tr>\n";
    $section .= "<tr><td><span title=\"How many partitions are supported at each level of Posterior Log Odds (LOD)?\">Partition support graph:</span> <a href=\"c-levels.svg\">SVG</a></td></tr>\n";
    $section .= "</table>\n";

    $section .= "<table>\n";
    for my $tree (@trees)
    {
	my $name = $tree_name{$tree};
	$section .= "<tr>";
	$section .= "<td>$name</td>";
#    $section .= "<td><a href=\"$tree.topology\">-L</a></td>";
	$section .= "<td><a href=\"$tree.tree\">+L</a></td>";
	$section .= "<td><a href=\"$tree-tree.pdf\">PDF</a></td>";
	$section .= "<td><a href=\"$tree-tree.svg\">SVG</a></td>";
	
	if ($sub_partitions && (-f "Results/$tree.mtree" || -f "Results/$tree-mctree.svg" ||
				-f "Results/$tree-mctree.pdf" )) 
	{
	    $section .= "<td>MC Tree:</td>";
	}
	else {
	    $section .= "<td></td>"     
	}
	
	if ($sub_partitions && -f "Results/$tree.mtree") {
	    $section .= "<td><a href=\"$tree.mtree\">-L</a></td>"     
	}
	else {
	    $section .= "<td></td>"     
	}
	if ($sub_partitions && -f "Results/$tree-mctree.pdf") {
	    $section .= "<td><a href=\"$tree-mctree.pdf\">PDF</a></td>";
	}
	else {
	    $section .= "<td></td>"     
	}
	if ($sub_partitions && -f "Results/$tree-mctree.svg") {
	    $section .= "<td><a href=\"$tree-mctree.svg\">SVG</a></td>";
	}
	else {
	    $section .= "<td></td>"     
	}
	$section .= "</tr>";
    }
    $section .= "</table>\n";
    return $section;
}

sub section_alignment_distribution
{
    return "" if ($n_partitions == 0);

    my $section .= "<h2 class=\"clear\"><a name=\"alignment\">Alignment Distribution</a></h2>\n";

    for(my $i=0;$i<$n_partitions;$i++) 
    {
	my $p = $i+1;
	$section .= "<h3>Partition $p</h3>\n";
	$section .= "<table>\n";
	$section .= "<tr>\n";
	$section .= "<th></th>\n";
	$section .= "<th></th>\n";
	$section .= "<th></th>\n";
	$section .= "<th title=\"Comparison of this alignment (top) to the WPD alignment (bottom)\">Diff</th>\n";
	$section .= "<th></th>\n";
	$section .= "<th style=\"padding-right:0.5em;padding-left:0.5em\" title=\"Percent identity of the most dissimilar sequences\">Min. %identity</th>\n";
	$section .= "<th style=\"padding-right:0.5em;padding-left:0.5em\" title=\"Number of columns in the alignment\"># Sites</th>\n";
	$section .= "<th style=\"padding-right:0.5em;padding-left:0.5em\" title=\"Number of invariant columns\">Constant</th>\n";
#    $section .= "<th style=\"padding-right:0.5em;padding-left:0.5em\" title=\"Number of variant columns\">Variable</th>\n";
	$section .= "<th title=\"Number of parsiomny-informative columns.\">Informative</th>\n";
	$section .= "</tr>\n";
	for my $alignment (@alignments)
	{
	    next if ($alignment !~ /^P$p-/);
	    my $name = $alignment_names{$alignment};
	    my $features = get_alignment_info("Results/$alignment.fasta");
	    
	    $section .= "<tr>\n";
	    $section .= "<td>$name</td>\n";
	    $section .= "<td><a href=\"$alignment.fasta\">FASTA</a></td>\n";
	    if (-f "Results/$alignment.html") {
		$section .= "<td><a href=\"$alignment.html\">HTML</a></td>\n";
	    }
	    else {
		$section .= "<td></td>\n";
	    }
	    if (-f "Results/$alignment-diff.html") {
		$section .= "<td><a href=\"$alignment-diff.html\">Diff</a></td>\n";
	    }
	    else {
		$section .= "<td></td>\n";
	    }
	    if (-f "Results/$alignment-AU.html") {
		$section .= "<td><a href=\"$alignment-AU.html\">AU</a></td>\n";
	    }
	    else {
		$section .= "<td></td>\n";
	    }
	    $section .= "<td style=\"text-align: center\">${$features}{'min_p_identity'}%</td>\n";
	    $section .= "<td style=\"text-align: center\">${$features}{'length'}</td>\n";
	    $section .= "<td style=\"text-align: center\">${$features}{'n_const'} (${$features}{'p_const'}%)</td>\n";
#	$section .= "<td style=\"text-align: center\">${$features}{'n_non-const'} (${$features}{'p_non-const'}%)</td>\n";
	    $section .= "<td style=\"text-align: center\">${$features}{'n_inform'} (${$features}{'p_inform'}%)</td>\n";
	    $section .= "</tr>\n";
	}
	$section .= "</table>\n";
    }
    return $section;
}




sub section_mixing
{
    my $section = "";

#$section .= '<object class="r_floating_picture" data="partitions.SRQ.svg" type="image/svg+xml" height="200pt"></object>';
$section .= '<img src="partitions.SRQ.png" class="r_floating_picture" alt="SRQ plot for support of each partition."/>';
#$section .= '<object class="r_floating_picture" data="c50.SRQ.svg" type="image/svg+xml" height="200pt"></object>';
#$section .= '<embed class="r_floating_picture" src="c50.SRQ.svg" type="image/svg+xml" height="200" />';
#$section .= '<embed class="r_floating_picture" src="partitions.SRQ.svg" type="image/svg+xml" height="200" />';
$section .= '<img src="c50.SRQ.png" class="r_floating_picture" alt="SRQ plot for supprt of 50% consensus tree."/>';



    $section .= "<h2><a name=\"mixing\">Mixing</a></h2>\n";
    
    $section .= "<ol>\n";
    $section .= "<li><a href=\"partitions.bs\">Partition uncertainty</a></li>\n";
    for my $srq (@SRQ) {
	$section .= "<li><a href=\"$srq.SRQ.png\">SRQ plot: $srq</a></li>\n";
    }
    $section .= "</ol>\n";
    
    my $burnin_before;
    if ($#parameter_files != -1)
    {
	$burnin_before = get_value_from_file('Results/Report','min burnin <=');
	$burnin_before = "Not Converged!" if ($burnin_before eq "Not");
	$section .= "<p><i>burn-in (scalar)</i> = $burnin_before</p>\n" if defined ($burnin_before);
	$section .= "<p><i>min Ne (scalar)</i> = ".get_value_from_file('Results/Report','Ne  >=')."</p\n";
    }
    
    $section .= "<p><i>min Ne (partition)</i> = ".get_value_from_file('Results/partitions.bs','min Ne =')."</p>\n";
    
    my $asdsf = get_value_from_file('Results/partitions.bs','ASDSF\[min=0.100\] =');
    my $msdsf = get_value_from_file('Results/partitions.bs','MSDSF =');
    $section .= "<p><i>ASDSF</i> = $asdsf</p>\n" if defined ($asdsf);
    $section .= "<p><i>MSDSF</i> = $msdsf</p>\n" if defined ($msdsf);
    
    my $psrf_80;
    my $psrf_rcf;
    if ($#parameter_files != -1)
    {
        $psrf_80 = get_value_from_file('Results/Report','PSRF-80%CI <=');
	$psrf_rcf = get_value_from_file('Results/Report','PSRF-RCF <=');
    }
    $section .= "<p><i>PSRF-80%CI</i> = $psrf_80</p>\n" if defined ($asdsf);
    $section .= "<p><i>PSRF-RCF</i> = $psrf_rcf</p>\n" if defined ($msdsf);

    $section .= '<p><a href="convergence-PP.pdf">Variation in split frequency estimates</a></p>'."\n" if (-f "Results/convergence-PP.pdf");

my $tne_string = exec_show("pickout -n Ne < Results/partitions.bs");
my @tne_array = split(/\n/,$tne_string);
@tne_array = sort {$a <=> $b} @tne_array;
#my $min_tNe = $tne_array[0];
my $min_tNe = exec_show("pickout -n 'min Ne' < Results/partitions.bs");

my @sne = sort {$a <=> $b} values(%Ne);
    $min_Ne = $sne[0];

print "\n";
print "NOTE: burnin (scalar) <= $burnin_before\n" if defined($burnin_before);
print "NOTE: min_Ne (scalar)    = $min_Ne\n" if defined($min_Ne);
print "NOTE: min_Ne (partition) = $min_tNe\n" if defined($min_tNe);
print "NOTE: ASDSF = $asdsf\n" if defined($asdsf);
print "NOTE: MSDSF = $msdsf\n" if defined($msdsf);
print "NOTE: PSRF-80%CI = $psrf_80\n" if defined($psrf_80);
print "NOTE: PSRF-RCF = $psrf_rcf\n" if defined($psrf_rcf);

    return $section;
}

sub section_scalar_variables
{
    return "" if ($#var_names == -1);
    
    my $section = "";

    $section .= "<h2 class=\"clear\"><a name=\"parameters\">Scalar variables</a></h2>\n";

    $section .= "<table>\n";
    $section .= "<tr><th>Statistic</th><th>Median</th><th title=\"95% Bayesian Credible Interval\">95% BCI</th><th title=\"Auto-Correlation Time\">ACT</th><th title=\"Effective Sample Size\">Ne</th><th>burnin</th></tr>\n";
    
    for(my $i=1;$i <= $#var_names; $i++) 
    {
	my $var = $var_names[$i];
	
	next if ($var eq "iter");
	next if (($var eq "time") && ($personality eq "phylobayes"));
	next if (($var eq "#treegen") && ($personality eq "phylobayes"));
	
	my $style = "";
	$style = 'style="font-style:italic"' if (!defined($CI_low{$var}));
	$section .= "<tr $style>\n";
	$section .= "<td>$var</td>\n";
	$section .= "<td>$median{$var}</td>\n";
	if (defined($CI_low{$var})) {
	    $section .= "<td>($CI_low{$var}, $CI_high{$var})</td>\n";
	    my $style = "";
	    $style = ' style="color:red"' if ($Ne{$var} <= $min_Ne);
	    $section .= "<td $style>$ACT{$var}</td>\n";
	    $style = "";
	    $style = ' style="color:orange"' if ($Ne{$var} < 300);
	    $style = ' style="color:red"' if ($Ne{$var} < 100);
	    $section .= "<td $style>$Ne{$var}</td>\n";
	    $style = "";
	    $style = ' style="color:red"' if ($Burnin{$var} eq "Not Converged!");
	    $section .= "<td $style>$Burnin{$var}</td>\n";
	    $section .= "<td><a href=\"$i.trace.png\">Trace</a></td>\n";
	}
	else {
	    $section .= "<td></td>";
	    $section .= "<td></td>";
	    $section .= "<td></td>";
	    $section .= "<td></td>";
	}
	$section .= "</tr>\n";
    }
    $section .= "</table>\n";
    return $section;
}

sub print_index_html
{

    my $n_sequences = get_n_sequences();

    open my $index, ">Results/index.html";

    my $title = "MCMC Post-hoc Analysis";
    $title = $title . ": $n_sequences sequences" if defined($n_sequences);

    print $index &html_header($title);

    print $index &topbar();

print $index "<h1>$title</h1>\n";
print $index html_svg("c50-tree.svg","200pt","",["floating_picture"]);
#print $index "<p>Samples were created by the following command line:</p>";
for(my $i=0; $i<= $#subdirs; $i++)
{
    print $index "<p><b>directory:</b> $directories[$i]<br/>\n";
    print $index "<b>subdirectory:</b> $subdirs[$i]<br/>\n";
    print $index "<b>command line:</b> $commands[$i]</p>\n";
}

print $index &print_data_and_model();

print $index &section_analysis();

print $index &section_phylogeny_distribution();

print $index &section_alignment_distribution();

print $index &section_mixing();

if ($#var_names != -1) {
    print $index "<h2 class=\"clear\"><a name=\"parameters\">Scalar variables</a></h2>\n";

    print $index "<table>\n";
    print $index "<tr><th>Statistic</th><th>Median</th><th title=\"95% Bayesian Credible Interval\">95% BCI</th><th title=\"Auto-Correlation Time\">ACT</th><th title=\"Effective Sample Size\">Ne</th><th>burnin</th></tr>\n";
}
    
for(my $i=1;$i <= $#var_names; $i++) 
{
    my $var = $var_names[$i];

    next if ($var eq "iter");
    next if (($var eq "time") && ($personality eq "phylobayes"));
    next if (($var eq "#treegen") && ($personality eq "phylobayes"));

    my $style = "";
    $style = 'style="font-style:italic"' if (!defined($CI_low{$var}));
    print $index "<tr $style>\n";
    print $index "<td>$var</td>\n";
    print $index "<td>$median{$var}</td>\n";
    if (defined($CI_low{$var})) {
	print $index "<td>($CI_low{$var}, $CI_high{$var})</td>\n";
	my $style = "";
	$style = ' style="color:red"' if ($Ne{$var} <= $min_Ne);
	print $index "<td $style>$ACT{$var}</td>\n";
	$style = "";
	$style = ' style="color:orange"' if ($Ne{$var} < 300);
	$style = ' style="color:red"' if ($Ne{$var} < 100);
	print $index "<td $style>$Ne{$var}</td>\n";
	$style = "";
	$style = ' style="color:red"' if ($Burnin{$var} eq "Not Converged!");
	print $index "<td $style>$Burnin{$var}</td>\n";
	print $index "<td><a href=\"$i.trace.png\">Trace</a></td>\n" if ($do_trace_plots);
    }
    else {
	print $index "<td></td>";
	print $index "<td></td>";
	print $index "<td></td>";
	print $index "<td></td>";
    }
    print $index "</tr>\n";
}
print $index "</table>\n";


print $index "  </body>\n";
print $index "</html>\n";
}

sub Rexec
{
    return if (!$have_R);

    my $script = shift;

    my $args = shift;

    exec_show("R --slave --vanilla --args $args < $script");
}

sub parse_command_line
{
    while ($#ARGV > -1) 
    {
	my $arg = shift @ARGV;
	if ($arg eq "clean" || $arg eq "--clean") {
	    do_cleanup();
	    exit;
	}
	elsif ($arg =~ /--burnin=(.+)/) {
	    $burnin = $1;
	}
	elsif ($arg =~ /--verbose/) {
	    $verbose = 1;
	}
	elsif ($arg =~ /--fast/) {
	    $speed = 2;
	}
	elsif ($arg =~ /--slow/) {
	    $speed = 0;
	}
	elsif ($arg =~ /--subsample=(.+)/) {
	    $subsample = $1;
	}
	elsif ($arg =~ /--prune=(.+)/) {
	    $prune = $1;
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
	elsif ($arg =~ /--mc-tree/) {
	    $sub_partitions=1;
	}
	elsif ($arg =~ /--no-trace-plots/) {
	    $do_trace_plots=0;
	}
	elsif ($arg =~ /--alignment-consensus/) {
	    $do_consensus_alignments=1;
	}
	elsif ($arg =~ /--treefile=(.+)/) {
	    $personality = "treefile";
	    my @arg_tree_files = split(/,/,$1);
	    foreach my $tree_file (@arg_tree_files)
	    {
		check_file_exists($tree_file)
	    }
	    @tree_files = (@tree_files,@arg_tree_files);
	}    
	elsif ($arg =~ /^-.*/) {
	    print "Error: I don't recognize option '$arg'\n";
	    exit(1);
	}
	else
	{
	    $arg = translate_cygwin($arg);
	    push @subdirectories, $arg;
	}
    }
    push @subdirectories,"." if ($#subdirectories == -1);
}


#----------------------------- SETUP 2 --------------------------#
sub determine_personality
{
    # quit if personality is already determined
    return if ($personality ne "");

    my $first_dir = $subdirectories[0];
    
    if (-e "$first_dir/C1.out")
    {
	$personality = "bali-phy-2.1";
	$n_chains = get_header_attribute("$first_dir/C1.out","MPI_SIZE");
	$n_chains=1 if (!defined($n_chains));
	$personality = "bali-phy-2.1-heated" if ($n_chains > 1);
    }
    elsif (-e "$first_dir/1.out")
    {
	$personality = "bali-phy-2.0";
    }
    else {
	my @treelists = glob("$first_dir/*.treelist");
	my @beast_trees = glob("$first_dir/*.trees");
	if ($#treelists >= 0) 
	{
	    $personality = "phylobayes";
	}
	elsif ($#beast_trees >= 0) {
	    $personality = "beast";
	}
	else {
	    print "Error: No BAli-Phy, phylobayes, or BEAST files in directory '$first_dir'.\n";
	    exit(1);
	}
    }
}

sub check_file_exists
{
    my $filename = shift;
    if (! -e "$filename") {
	print "Error: I can't find file '$filename'\n";
	exit(1);
    }
    return $filename;
}

sub determine_input_files
{
    my $first_dir = $subdirectories[0];

    if ($personality eq "bali-phy-2.1")
    {
	foreach my $directory (@subdirectories)
	{
	    push @out_files, check_file_exists("$directory/C1.out");
	    push @tree_files, check_file_exists("$directory/C1.trees");
	    push @parameter_files, check_file_exists("$directory/C1.p");
	}
	$MAP_file = "$first_dir/1.MAP";
    }
    elsif ($personality eq "bali-phy-2.1-heated")
    {
	$n_chains = get_header_attribute("MPI_SIZE");
	for(my $i=0;$i<$n_chains;$i++) {
	    push @out_files,"C$i.out" if (-e "C$i.out");
	    push @tree_files,"C$i.trees" if (-e "C$i.trees");
	}
    }
    elsif ($personality eq "bali-phy-2.0")
    {
	foreach my $directory (@subdirectories)
	{
	    push @out_files, check_file_exists("$directory/1.out");
	    push @tree_files, check_file_exists("$directory/1.trees");
	    push @parameter_files, check_file_exists("$directory/1.p");
	}
	$MAP_file = "$first_dir/1.MAP";
    }
    elsif ($personality eq "treefile")
    { }
    elsif ($personality eq "phylobayes")
    {
	print "Summarizing output files from phylobayes:\n";

	foreach my $directory (@subdirectories)
	{
	    my @treelists = glob("$directory/*.treelist");

	    for my $treelist (@treelists)
	    {
		push @tree_files, check_file_exists($treelist);

		my $prefix = get_file_prefix($treelist);
		push @parameter_files, check_file_exists($prefix.".trace");
		push @out_files, check_file_exists($prefix.".log");
	    }
	}
    }
    elsif ($personality eq "beast")
    {
	print "Summarizing output files from BEAST:\n";
	my @tree_files = glob("*.trees");
	foreach my $tree_file (@tree_files)
	{
	    check_file_exists($tree_file);
	    my $prefix = get_file_prefix($tree_file);
	    push @parameter_files,check_file_exists("$prefix.log");
	}
    }
    else {
	print "Error: unrecognized analysis of type '$personality'";
	exit(1);
    }
}

sub draw_trees
{
    return if (! $have_draw_tree);

    print " Drawing trees:  ";
    for my $cvalue (@tree_consensus_values)
    {
	my $value = $cvalue*100;
	
	my $tree = "c$value";

	# No node lengths???
	my $filename1 = "Results/$tree.tree";
	my $filename2 = "Results/$tree.mtree";
	
	if ($speed < 2)
	{
	    if (-e $filename2 && ! more_recent_than("Results/$tree-mctree.svg",$filename2)) {
		exec_show("draw-tree Results/$tree.mlengths --out=Results/$tree-mctree --output=svg --draw-clouds=only");
	    }
	    if (-e $filename2 && ! more_recent_than("Results/$tree-mctree.pdf",$filename2)) {
		exec_show("draw-tree Results/$tree.mlengths --out=Results/$tree-mctree --draw-clouds=only");
	    }
	}
	
	if (! more_recent_than("Results/$tree-tree.pdf",$filename1)) {
	    exec_show("cd Results ; draw-tree $tree.ltree --layout=equal-daylight --no-shade");
	}
	
	if (! more_recent_than("Results/$tree-tree.svg",$filename1)) {
	    exec_show("cd Results ; draw-tree $tree.ltree --layout=equal-daylight --output=svg --no-shade");
	}
	
	print "$tree ";
	my $prune_arg = "";
	$prune_arg = "--prune $prune" if defined($prune);
	
    }

    exec_show("cd Results ; draw-tree greedy.tree --layout=equal-daylight --no-shade") if (-e 'Results/greedy.tree');
    exec_show("cd Results ; draw-tree greedy.tree --layout=equal-daylight --no-shade --output=svg ") if (-e 'Results/greedy.tree');

    exec_show("cd Results ; draw-tree MAP.tree --layout=equal-daylight --no-shade") if (-e 'Results/MAP.tree');
    exec_show("cd Results ; draw-tree MAP.tree --layout=equal-daylight --no-shade --output=svg ") if (-e 'Results/MAP.tree');

    print "done.\n";
}

sub mixing_diagnostics
{
# 10. Mixing diagnostics -- block bootstrap
    print "\nGenerate mixing diagnostics for topologies ... ";

    if (!more_recent_than("Results/partitions","Results/consensus")) {
	exec_show("pickout --no-header --large pi < Results/consensus > Results/partitions");
    }
    if (!more_recent_than("Results/partitions.pred","Results/partitions")) {
	exec_show("perl -e 'while(<>) {s/\$/\\n/;print;}' < Results/partitions > Results/partitions.pred");
    }

    if (!more_recent_than_all_of("Results/partitions.bs",[@tree_files])) {
	exec_show("trees-bootstrap $max_arg @tree_files $skip $subsample_string --pred Results/partitions.pred --LOD-table=Results/LOD-table --pseudocount 1 > Results/partitions.bs");
    }
    print "done.\n";

    if (!more_recent_than_all_of("Results/convergence-PP.pdf",[@tree_files]) and $#tree_files > 0)
    {
	my $script = find_in_path("compare-runs.R");
	die "can't find script!" if (!defined($script));
	Rexec($script,"Results/LOD-table Results/convergence-PP.pdf");
    }
}

sub SRQ_plots
{
    my @SRQ = ();
# 12. Mixing diagnostics - SRQ plots
    print "Generate SRQ plot for partitions ... ";
    if (!more_recent_than_all_of("Results/partitions.SRQ",[@tree_files])) {
	exec_show("trees-to-SRQ Results/partitions.pred $max_arg $skip $subsample_string --max-points=1000 < $tree_files[0] > Results/partitions.SRQ");
    }
    print "done.\n";
    
    push @SRQ,"partitions";

    print "Generate SRQ plot for c50 tree ... ";
    if (!more_recent_than_all_of("Results/c50.SRQ",[@tree_files])) {
	exec_show("trees-to-SRQ Results/c50.tree $max_arg $subsample_string $skip --max-points=1000 < $tree_files[0] > Results/c50.SRQ");
    }
    print "done.\n";
    
    push @SRQ,"c50";
    
    if ($have_gnuplot) {
	for my $srq (@SRQ) {
`gnuplot <<EOF
set terminal png size 300,300
set output "Results/$srq.SRQ.png"
set key right bottom
set xlabel "Regenerations (fraction)"
set ylabel "Time (fraction)"
set title "Scaled Regeneration Quantile (SRQ) plot: $srq"
plot 'Results/$srq.SRQ' title "$srq" with linespoints lw 1 lt 1, x title "Goal" lw 1 lt 3
EOF
`;
	}
    }
    if ($have_gnuplot) {
	if ($sub_partitions) {
`gnuplot <<EOF
set terminal svg
set output "Results/c-levels.svg"
set xlabel "Log10 posterior Odds (LOD)"
set ylabel "Supported Splits"
set style data lines
plot [0:][0:] 'Results/c-levels.plot' title 'Full Splits','Results/extended-c-levels.plot' title 'Partial Splits'
EOF`;
	}
	else {
`gnuplot <<EOF
set terminal svg
set output "Results/c-levels.svg"
set xlabel "Log10 posterior Odds (LOD)"
set ylabel "Supported Splits"
plot [0:][0:] 'Results/c-levels.plot' with lines notitle
EOF`;
	}
    }
    return @SRQ;
}

sub compute_initial_alignments
{
    if ($personality =~ "bali-phy.*") {
	print "\nComputing initial alignments... ";
	for(my $i=0;$i<$n_partitions;$i++)
	{
	    my $p = $i+1;
	    my $name = "P$p-initial";
	    push @alignments,$name;
	    $alignment_names{$name} = "Initial";
	    
	    # These initial alignments should never change!
	    if (! -s "Results/Work/$name-unordered.fasta") 
	    {
		my $initial_name = $partition_samples[0][$i];
		$initial_name =~ s/\.fastas/\.initial\.fasta/;

		# Version > 2.1.1
		if (-e $initial_name)
		{
		    exec_show("cp $initial_name Results/Work/$name-unordered.fasta");
		}
		else
		{
		    exec_show("alignment-find --first < $partition_samples[0][$i] > Results/Work/$name-unordered.fasta 2>/dev/null");
		    if ($? && $n_chains==1 && defined($MAP_file)) {
			exec_show("alignment-find --first < $MAP_file > Results/Work/$name-unordered.fasta");
		    }
		}
	    }
	}
	print "done.\n";
    }
}

sub compute_muscle_alignment
{
    if ($personality =~ "bali-phy.*") {
	if ($muscle) {
	    print "\nComputing MUSCLE alignment... ";
	    
	    for(my $i=0;$i<$n_partitions;$i++) {
		my $p = ($i+1);
		my $name = "P$p-muscle";
		if (! more_recent_than("Results/Work/$name-unordered.fasta", "Results/Work/P$p-initial-unordered.fasta")) {
		    exec_show("muscle -in Results/Work/P$p-initial-unordered.fasta -out Results/Work/$name-unordered.fasta -quiet");
		}
		push @alignments,$name;
		$alignment_names{$name} = "MUSCLE";
		
	    }
	    print "done.\n";
	}
    }
}

sub compute_probcons_alignment
{
# 6.5. Compute ProbCons alignments

    if ($personality =~ "bali-phy.*") {
	if ($probcons) {
	    print "\nComputing ProbCons alignment... ";
	    
	    for(my $i=0;$i<$n_partitions;$i++) {
		my $p = ($i+1);
		my $name = "P$p-probcons";
		
#    my $alignment_info = get_alignment_info("Results/Work/P$p-initial-unordered.fasta");
		my $alphabet = $alphabets[$i];
		
		if ($alphabet =~ /RNA nucleotides/) {
		    print "got here\n";
		    if (! more_recent_than("Results/Work/$name-unordered.fasta", "Results/Work/P$p-initial-unordered.fasta")) {
			exec_show("probcons-RNA Results/Work/P$p-initial-unordered.fasta > Results/Work/$name-unordered.fasta 2>/dev/null");
		    }
		}
		elsif (! more_recent_than("Results/Work/$name-unordered.fasta", "Results/Work/P$p-initial-unordered.fasta")) {
		    exec_show("probcons Results/Work/P$p-initial-unordered.fasta > Results/Work/$name-unordered.fasta 2>/dev/null");
		}
		push @alignments,$name;
		$alignment_names{$name} = "ProbCons";
		
	    }
	    print "done.\n";
	}
    }
}

sub compute_wpd_alignments
{
    if ($personality =~ "bali-phy.*") {
	print "\nComputing WPD alignments... ";

	for(my $i=0;$i<$n_partitions;$i++) 
	{
	    next if ($imodel_indices[$i] == -1);
	
	    my $p = $i+1;
	    my $infile = $partition_samples[0][$i];
	    
	    my $name = "P$p-max";
	    if (! more_recent_than("Results/Work/$name-unordered.fasta",$infile) ||
		! more_recent_than("Results/Work/$name-unordered.fasta",$infile) ) {
		exec_show("cut-range --skip=$burnin $size_arg < $infile | alignment-max> Results/Work/$name-unordered.fasta");
	    }
	    push @alignments,$name;
	    $alignment_names{$name} = "Best (WPD)";
	    push @AU_alignments,$name;
	}
	
	print "done.\n";
    }
}

sub compute_consensus_alignments
{
    print "Computing consensus alignments:\n";
    for(my $i=0;$i<$n_partitions;$i++)
    {
	next if ($imodel_indices[$i] == -1);

	my $p = $i+1;
	my $infile = $partition_samples[0][$i];
	
	print " Partition $p: ";
	for my $cvalue (@alignment_consensus_values) {
	    my $value = $cvalue*100;
	    my $name = "P$p-consensus-$value";
	    print "c$value ";
	    if (! more_recent_than("Results/Work/$name-unordered.fasta",$infile)) {
		exec_show("cut-range --skip=$burnin $size_arg < $infile | alignment-consensus --cutoff=$cvalue> Results/Work/$name-unordered.fasta");
	    }
	    push @alignments,$name;
	    $alignment_names{$name} = "$value% consensus";
	}
	print "done.\n\n";
	push @AU_alignments,"P$p-consensus-10" if ($speed == 0);
    }
}

sub draw_alignments
{
    return if ($#alignments == -1);

    print "Drawing alignments... ";
    for my $alignment (@alignments) 
    {
	if (! more_recent_than("Results/$alignment.fasta","Results/Work/$alignment-unordered.fasta") ||
	    ! more_recent_than("Results/$alignment.fasta","Results/c50.tree")) {
	    exec_show("alignment-cat Results/Work/$alignment-unordered.fasta --reorder-by-tree=Results/c50.tree > Results/$alignment.fasta");
	}
	
	if (! more_recent_than("Results/$alignment.html","Results/$alignment.fasta")) {
	    exec_show("alignment-draw Results/$alignment.fasta --show-ruler --color-scheme=DNA+contrast > Results/$alignment.html 2>/dev/null");
	    
	    if ($?) {
		exec_show("alignment-draw Results/$alignment.fasta --show-ruler --color-scheme=AA+contrast > Results/$alignment.html");
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
	    exec_show("alignments-diff Results/$alignment.fasta Results/P$p-max.fasta --merge --fill=unknown -d Results/$alignment-diff.AU > Results/$alignment-diff.fasta");
	}
	
	if (! more_recent_than("Results/$alignment-diff.html","Results/$alignment-diff.fasta")) {
	    exec_show("alignment-draw Results/$alignment-diff.fasta --scale=invert --AU Results/$alignment-diff.AU --show-ruler --color-scheme=Rainbow+fade[1,0]+contrast > Results/$alignment-diff.html");
	}
    }
    print "done.\n";
}

sub compute_and_draw_AU_plots
{
    for my $alignment (@AU_alignments) 
    {
	if ($alignment =~ /^P([^-]+)-.*/) {
	    print "Generating AU values for $alignment... ";
	    my $p = $1;
	    my $infile = $partition_samples[0][$p-1];
	    
	    if (!more_recent_than("Results/$alignment-AU.prob",$infile)) {
		exec_show("cut-range --skip=$burnin $size_arg < $infile | alignment-gild Results/$alignment.fasta Results/MAP.tree --max-alignments=500 > Results/$alignment-AU.prob");
	    }
	    print "done.\n";
	    exec_show("alignment-draw Results/$alignment.fasta --show-ruler --AU Results/$alignment-AU.prob --color-scheme=DNA+contrast+fade+fade+fade+fade > Results/$alignment-AU.html 2>/dev/null");
	    if ($?) {
		exec_show("alignment-draw Results/$alignment.fasta --show-ruler --AU Results/$alignment-AU.prob --color-scheme=AA+contrast+fade+fade+fade+fade > Results/$alignment-AU.html");
	    }
	}
    }
}

sub compute_marginal_likelihood
{
    my $marginal_prob = "unknown";
    if ($personality ne "treefile") 
    {
	print "Calculating marginal likelihood... ";
	
	if (!more_recent_than_all_of("Results/Pmarg",[@parameter_files])) {
	    my $likelihood = "likelihood";
	    $likelihood = "loglik" if ($personality eq "phylobayes");
	    exec_show("stats-select $likelihood --no-header < $parameter_files[0] | tail -n+$burnin | model_P > Results/Pmarg");
	}
	print "done.\n";
	$marginal_prob = `cat Results/Pmarg`;
    }
    return $marginal_prob;
}


sub generate_trace_plots
{
    
    if ($#parameter_files > -1) 
    {
	open VARS, $parameter_files[0];
	my $header = <VARS>;
	while ($header eq "" or substr($header,0,2) eq "# ") {
	    $header = <VARS>;
	}
	chomp $header;
	@var_names = split(/\t/,$header);
	close VARS;
	
	open REPORT, "Results/Report";
	
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
		
		$line =~ /t @ (.+)\s+Ne = ([^ ]+)\s+burnin = (Not Converged!|[^ ]+)/;
		$ACT{$var} = $1;
		$Ne{$var} = $2;
		$Burnin{$var} = $3;
	    }
	    elsif ($line =~ /\s+(.+) = (.+)/) {
		my $var = $1;
		$median{$var} = $2;
	    }
	}

	return if (!$do_trace_plots);
	
	my $Nmax = 5000;
	
	for(my $i=1;$i<= $#var_names; $i++)
	{
	    next if (more_recent_than("Results/$i.trace.png",$parameter_files[0]));
	    
	    my $var = $var_names[$i];
	    next if (!defined($CI_low{$var}));
	    
	    my $file1 = "Results/Work/T1.p.$i";
	    if ($personality =~ "bali-phy.*") {
		exec_show("stats-select iter '$var' --no-header < $parameter_files[0] > $file1");
	    }
	    elsif ($personality =~ "phylobayes.*") {
		exec_show("stats-select time '$var' --no-header < $parameter_files[0] > $file1");
	    }
	    elsif ($personality =~ "beast.*")
	    {
		exec_show("stats-select state '$var' --no-header < $parameter_files[0] > $file1");
	    }
	    
	    my $file2 = $file1.".2";
	    
	    my $N = $n_iterations[0] - $burnin;
	    
	    $N = 1000 if ($N > 1000);
	    
	    my $factor = ceil(($n_iterations[0] - $burnin) / $N);
	    
	    exec_show("subsample --skip=$burnin $factor < $file1 > $file2");
	
	    `gnuplot <<EOF
set terminal png size 800,600
set output "Results/$i.trace.png"
set key right bottom
set xlabel "Iteration"
set ylabel "$var"
plot '$file2' title '$var' with lines
EOF` if ($have_gnuplot);
	}
	
	print "done\n";
    }
}


sub find_in_path
{
    my $file = shift;
    my $home = $ENV{'HOME'};

    my @dirs = split(':',$ENV{'PATH'});
    
    for my $dir (@dirs) {
	$dir =~ s/^~/$home/;
	if (-x "$dir/$file" ) {
	    return "$dir/$file";
	}
    }

    return undef;
}

sub is_in_path
{
    return 1 if (defined(find_in_path(@_)));
    return 0;
}

sub check_input_file_names()
{
    return 0 if (! -e "Results/input_files");

    open FILE,"Results/input_files";

    my @old_input_file_names;
    while(my $line = <FILE>)
    {
	chomp $line;
	push @old_input_file_names, $line;
    }
    close FILE;

    return compare_arrays( [@old_input_file_names], [@input_file_names]);
}

sub write_input_file_names()
{
    open FILE,">Results/input_files";

    print FILE join("\n",@input_file_names);

    close FILE;
}

sub check_out_file_names()
{
    return 0 if (! -e "Results/out_files");
    open FILE,"Results/out_files";

    my @out_files_old;
    while(my $line = <FILE>)
    {
	chomp $line;
	push @out_files_old, $line;
    }
    close FILE;

    return compare_arrays( [@out_files_old], [@out_files]);
}

sub write_out_file_names()
{
    open FILE,">Results/out_files";

    print FILE join("\n",@out_files);

    close FILE;
}

sub check_burnin()
{
    return 0 if (! -e "Results/burnin");
    open FILE,"Results/burnin";

    my $line = <FILE>;
    chomp $line;

    return ($burnin == $line);
}

sub write_burnin()
{
    open FILE,">Results/burnin";

    print FILE "$burnin\n";

    close FILE;
}

sub get_unused_dir_name
{
     for(my $i=1;;$i++)
    {
	my $unused_name = "Results.".$i;
	return $unused_name if (! -e $unused_name);
    }
}

sub initialize_results_directory
{
    my $reuse = 1;

    # check that this is an analysis of the same MCMC chains as last time
    $reuse = check_out_file_names() if ($reuse);

    # check that the input alignment files are the same as last time
    $reuse = check_input_file_names() if ($reuse);

    # check that the input alignment files are the same as last time
    $reuse = check_burnin() if ($reuse);

    if (!$reuse && -e "Results")
    {
	my $new_dir_name = get_unused_dir_name();
	print "Renaming 'Results/' to '$new_dir_name/'.\n\n" if (-e "Results/");
	rename "Results/", $new_dir_name;
    }

    if (! -e "Results")
    {
	print "Creating new directory Results/ for summary files.\n";
	mkdir "Results";
	mkdir "Results/Work";
    }

    write_input_file_names();
    write_out_file_names();
    write_burnin();
}

sub do_cleanup
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

sub compare_arrays {
    my ($first, $second) = @_;
    no warnings;  # silence spurious -w undef complaints
    return 0 unless @$first == @$second;
    for (my $i = 0; $i < @$first; $i++) {
	return 0 if $first->[$i] ne $second->[$i];
    }
    return 1;
}  

sub get_input_file_names_for_outfile
{
    my $file = shift;
    die "Which file should I get the partitions for?" if (!defined($file));

    if ($personality =~ "bali-phy.*")
    {
	local *FILE;

	open FILE, $file or die "Can't open $file!";

	my @input_file_names = ();
	
	while (my $line = <FILE>) 
	{
	    if ($line =~ /data(.+) = (.+)/) {
		my $filename = $2;
		$filename =~ s/$home/~/;
		push @input_file_names,$filename;
	    }
	    if ($line =~ /data = (.+)/) {
		my $filename = $1;
		$filename =~ s/$home/~/;
		push @input_file_names,$filename;
	    }
	    last if ($line =~ /^iterations = 0/);
	}
	return [@input_file_names];
    }
    elsif ($personality eq "phylobayes")
    {
	my $filename = get_value_from_file($file,"data file :");
	$filename =~ s/$home/~/;
	return [$filename];
    }
    else
    {
	return [];
    }
}

sub show_array
{
    my $spacer = shift;
    my $array = shift;
    foreach my $entry (@$array)
    {
	print $spacer.$entry."\n";
    }
}

sub show_array_differences
{
    my $spacer = shift;
    my $name1 = shift;
    my $array1 = shift;
    my $name2 = shift;
    my $array2 = shift;

    print "  For $name1:\n";
    show_array($spacer,$array1);
    print "  For $name2:\n";
    show_array($spacer,$array2);
}

sub get_input_file_names
{
    my $input_file_names;
    for my $out_file (@out_files)
    {
	my $these_input_file_names = get_input_file_names_for_outfile($out_file);
	if (!defined($input_file_names))
	{
	    $input_file_names = $these_input_file_names;
	}
	else
	{
	    if (!compare_arrays($input_file_names,$these_input_file_names))
	    {
		print "\nError! Different MCMC chains have different input files!\n";
		show_array_differences("    file = ",$out_files[0],$input_file_names,$out_file,$these_input_file_names);
		exit(1);
	    }
	}
    }
    $input_file_names = [] if (!defined($input_file_names));
    return $input_file_names;
}

#FIXME - rewrite to take the cmdline as an argument.
sub get_cmdline_attribute
{
    return "unknown" if ($personality !~ "bali-phy.*");
    my $attribute = shift;
    my $value;

    local *FILE;

    open FILE, $out_files[0] or die "Can't open $out_files[0]!";

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

sub get_header_attribute
{
    return "unknown" if ($personality !~ "bali-phy.*");
    my $filename = shift;
    my $attribute = shift;
    my $value;

    local *FILE;

    open FILE, $filename or die "Can't open $filename!";

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

sub get_value_from_file
{
    my $filename = shift;
    my $attribute = shift;

    my $value;

    local *FILE;

    open FILE, $filename or die "Can't open $filename!";

    my @partitions = ();

    while (my $line = <FILE>) 
    {
	if ($line =~ /$attribute ([^ ]*)($| )/) {
	    $value = $1;
	    last;
	}
    }
    close FILE;

    return $value;
}

sub get_header_attributes
{
    return ("unknown") if ($personality !~ "bali-phy.*");
    my $attribute = shift;
    my @filenames = @_;
    my @values;

    foreach my $filename (@filenames)
    {

	local *FILE;

	open FILE, $filename or die "Can't open $filename!";

	my @partitions = ();

	while (my $line = <FILE>) 
	{
	    if ($line =~ /$attribute: (.*)$/) {
		push @values, $1;
		last;
	    }
	    last if ($line =~ /^iterations = 0/);
	}
	close FILE;
    }

    return @values;
}

#Empirical(/home/bredelings/local/share/bali-phy/Data//wag.dat) 

sub sanitize_smodel
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

sub get_smodels_for_file
{
    my $file = shift;

    return [] if ($personality !~ "bali-phy.*");

    local *FILE;

    open FILE, $file or die "Can't open $file!";

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

sub arrays_all_equal
{
    my @arrays = @_;
    for(my $i=1;$i <= $#arrays ; $i++) {
	return $i if (!compare_arrays($arrays[$i], $arrays[0]));
    }
    return 0;
}

sub get_smodels
{
    return [] if ($#out_files == -1);
    my @smodels;

    foreach my $out_file (@out_files)
    {
	push @smodels, get_smodels_for_file($out_file);
    }

    my $different = arrays_all_equal(@smodels);

    return $smodels[0] if (! $different);

    print "\nError! Different MCMC chains have different substitution models!\n";
    show_array_differences("    subst model = ", $out_files[0], $smodels[0], $out_files[$different], $smodels[$different]);
    exit(1);
}

sub get_imodels_for_file
{
    my $file = shift;

    return [] if ($personality !~ "bali-phy.*");

    local *FILE;

    open FILE, $file or die "Can't open $file!";

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

sub get_imodels
{
    return [] if ($#out_files == -1);
    my @imodels;

    foreach my $out_file (@out_files)
    {
	push @imodels, get_imodels_for_file($out_file);
    }

    my $different = arrays_all_equal(@imodels);

    return $imodels[0] if (! $different);

    print "\nError! Different MCMC chains have different substitution models!\n";
    show_array_differences("    subst model = ", $out_files[0], $imodels[0], $out_files[$different], $imodels[$different]);
    exit(1);
}

sub get_smodel_indices
{
    return [] if ($personality !~ "bali-phy.*");

    local *FILE;

    open FILE, $out_files[0] or die "Can't open $out_files[0]!";

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

sub get_imodel_indices
{
    return [] if ($personality !~ "bali-phy.*");

    local *FILE;

    open FILE, $out_files[0] or die "Can't open $out_files[0]!";

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

sub get_alphabets
{
    return [] if ($personality !~ "bali-phy.*");

    local *FILE;

    open FILE, $out_files[0] or die "Can't open $out_files[0]!";

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

sub get_n_lines
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

sub get_n_iterations
{
    my @n_iterations;
    for my $tree_file (@tree_files)
    {
	my $n = get_n_lines($tree_file)-1;
	if ($n <= 0) {
	    print "Error: Tree file '$tree_file' has no samples!\n";
	    exit(1);
	}
	push @n_iterations,$n;
    }
    return @n_iterations;
}

sub more_recent_than
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

sub more_recent_than_all_of
{
    my $filename1 = shift;
    my $temp = shift;
    my @filenames2 = @$temp;

    foreach my $filename2 (@filenames2) {
	return 0 if (!more_recent_than($filename1,$filename2));
    }

    return 1;
}

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
    return {} if ($personality !~ "bali-phy.*");

    my $filename = shift;
    open INFO,"alignment-info $filename |";

    my %features = ();

    my $indels = 0;
    while(my $line=<INFO>) {
	if ($line =~ /Alphabet: (.*)$/) {
	    $features{"alphabet"} = $1;
	}
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

sub get_consensus_arg
{
    my $suffix = shift;
    my $levels = shift;
    my @pairs = @$levels;
    for my $level (@pairs)
    {
	my $filename = $level*100;
	$filename = "Results/c$filename.".$suffix;
	$level = "$level:$filename";
    }
    return join(',',@pairs);
}

sub get_file_prefix
{
    my $filename = shift;
    if ($filename =~ /(.*)\.([^.\/\\]*)$/)
    {
	return $1;
    }
    return $filename;
}

sub get_file_suffix
{
    my $filename = shift;
    if ($filename =~ /(.*)\.([^.\/\\]*)$/)
    {
	return $2;
    }
    return "";
}

sub get_mcmc_command_line
{
    my $command;
    if ($personality =~ "bali-phy.*") 
    {
	$command = get_header_attribute($out_files[0],"command");
    }
    else
    {
	$command = "unknown";
    }
    return $command;
}

sub min
{
    my $array = shift;
    my $best = $$array[0];
    for my $element (@$array)
    {
	$best = $element if ($element < $best);
    }
    return $best;
}

sub max
{
    my $array = shift;
    my $best = $$array[0];
    for my $element (@$array)
    {
	$best = $element if ($element > $best);
    }
    return $best;
}

sub arg_max
{
    my $array = shift;
    my $best = $$array[0];
    my $best_i = 0;
    for(my $i=1;$i<=$#$array;$i++)
    {
	if ($$array[$i] > $best)
	{
	    $best = $$array[$i];
	    $best_i = $i;
	}
    }
    return $best_i;
}

sub arg_min
{
    my $array = shift;
    my $best = $$array[0];
    my $best_i = 0;
    for(my $i=1;$i<=$#$array;$i++)
    {
	if ($$array[$i] < $best)
	{
	    $best = $$array[$i];
	    $best_i = $i;
	}
    }
    return $best_i;
}

sub determine_burnin
{
    if (defined($burnin))
    {
	for(my $i=0;$i<=$#out_files;$i++)
	{
	    if ($burnin > $n_iterations[$i]) 
            {
		print "Chain #".($i+1)." with file $out_files[$i] has only $n_iterations[$i] iterations.\n";
		print "Error!  The the burnin (specified as $burnin) cannot be higher than this.\n";
		exit(1);
	    }
	}
    }
    return if (defined($burnin));

    my $max_iterations = max(\@n_iterations);
    my $max_i = arg_max(\@n_iterations);

    my $min_iterations = min(\@n_iterations);
    my $min_i = arg_min(\@n_iterations);

    if ($max_iterations > 3*$min_iterations)
    {
	print "The variation in run length between MCMC chains is too great.\n";
	print "  Chain #".($max_i+1)." has $max_iterations iterations.\n";
	print "  Chain #".($min_i+1)." has $min_iterations iterations.\n";
	print "Not going to guess a burnin: please specify one.\n";
	exit(1)
    }

    $burnin = int 0.1*min(\@n_iterations);
}

sub get_the_only_subdirectory
{
    if ($#subdirectories != 0)
    {
	print "Error: There is more than one subdirectory, but other subdirectories are being ignored.";
	exit(1);
    }
    return $subdirectories[0];
}

sub determine_alignment_files
{
    if ($personality =~ "bali-phy-2.1.*") {
	if ($n_chains == 1) {
	    # In this case, we are constructing a list for each DIRECTORY
	    foreach my $directory (@subdirectories)
	    {
		my @samples = ();
		for(my $p=1;$p<=$n_partitions;$p++) {
		    push @samples,"$directory/C1.P$p.fastas";
		}
		push @partition_samples, [@samples];
	    }
	}
	else {
	    # In this case we are constructing the list of files for different temperatures

	    my @samples = ();
	    my $first_dir = get_the_only_subdirectory();
	    for(my $p=1;$p<=$n_partitions;$p++) {
		push @samples,"$first_dir/C1.P$p.fastas";
	    }
	    push @partition_samples, [@samples];
	}
    }
    else {
	my $first_dir = get_the_only_subdirectory();
	my @samples = ();
	for(my $p=1;$p<=$n_partitions;$p++) {
	    push @samples,"$first_dir/1.P$p.fastas" if (-e "$first_dir/1.P$p.fastas");
	}
	@samples = ("$first_dir/1.out") if ($#samples == -1);
	push @partition_samples, [@samples];
    }
}

# 0. Compute T1.p and T1.trees
sub compute_tree_and_parameter_files_for_heated_chains
{
    return if ($n_chains == 1);

    for(my $i=1;$i<=$n_chains;$i++)
    {
	# Construct C1.pt
	# Construct C1Ti.pt
	# Construct C1Ti.p
	# Construct C1Ti.trees
	next if (! -e "C$i.trees" );
	
	if (! more_recent_than_all_of("Results/C$i.t",[@tree_files]))
	{
	    exec_show("echo 'tree' > Results/C$i.t");
	    exec_show("cat C$i.trees >> Results/C$i.t");
	}
	
	if (! more_recent_than_all_of("Results/C$i.pt",[@tree_files, @parameter_files]))
	{
	    exec_show("stats-merge C$i.p Results/C$i.t > Results/C$i.pt 2>/dev/null");
	}
	
	if (! more_recent_than("Results/C${i}T1.pt","Results/C$i.pt")) 
	{
	    my $use_header = "";
	    $use_header = "--no-header" if ($i != 1);
	    
	    exec_show("subsample --header --skip=$burnin < Results/C$i.pt | stats-select -s beta=1 $use_header > Results/C${i}T1.pt");
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
    exec_show("$cmd") if ($rerun);
    
    if (! more_recent_than("Results/T1.trees","Results/T1.pt")) {
	exec_show("stats-select tree --no-header < Results/T1.pt > Results/T1.trees");
    }
    
    if (! more_recent_than("Results/T1.p","Results/T1.pt")) {
	exec_show("stats-select -r tree < Results/T1.pt > Results/T1.p");
	
#       This messes up the printing of statistics
#	exec_show("stats-select -i -r tree < Results/T1.pt > Results/T1.p");
	
    }
    
    @tree_files = ( "Results/T1.trees" );
    @parameter_files = ("Results/T1.p");
}

# Replace /cydrive/LETTER{STUFF} with LETTER:{STUFF}
sub translate_cygwin
{
    my $arg = shift;
    my $new_arg = $arg;
    $new_arg =~ s|^/cygdrive/([^/]*)(.*)$|$1:$2|;

    if ($new_arg ne $arg)
    {
	print "Translating '$arg' to '$new_arg'\n";
	$arg = $new_arg;
    }
    return $arg;
}

sub exec_show
{
    my $cmd = shift;
    print LOG "\ncommand:  $cmd\n\n";
    my $result = `$cmd 2>err`;
    if ($? != 0)
    {
	my $code = $?>>8;
	my $message = `cat err`; 
	print STDERR "Subcommand failed! (code $code)\n";
	print LOG    "Subcommand failed! (code $code)\n";

	print STDERR "\n  command:  $cmd\n";

	print STDERR "\n  message:  $message\n";
	print LOG    "\n  message:  $message\n";
	exit($code);
    }
    elsif ($verbose)
    {
	print STDERR "\n\t$cmd\n\n" if ($verbose);
    }
}
