#!/usr/bin/env python3

import shutil
import argparse
import re
import os
from os import path
import json
import itertools

def get_json_from_file(filename):
    with open(filename, encoding='utf-8') as json_file:
        return json.load(json_file)

def make_extracted(x):
    if not "main" in x:
        return {"main": x, "extracted": []}
    else:
        return x

def all_same(xs):
    if len(xs) < 2:
        return True
    for x in xs:
        if x != xs[0]:
            return False
    return True

def first_all_same(xs):
    if not all_same(xs):
        raise Exception("Not all the same!")
    return xs[0]

def get_unused_dir_name():
    for i in itertools.count(1):
        dirname = "Results.{}".format(i)
        if not path.exists(dirname):
            return dirname

def get_value_from_file(filename,attribute):
    with open(filename,'r',encoding='utf-8') as file:
        for line in file:
            m = re.match('{} ([^ ]*)($| )'.format(attribute), line)
            if m:
                return m.group(1)
    return None

def check_file_exists(filename):
    if not path.exists(filename):
        print("Error: I cannot find file '{}'".format(filename))
        exit(1)
    return filename

def get_alignment_info(filename):
    pass
#
#    my $filename = shift;
#    open INFO,"alignment-info $filename |";
#
#    my %features = ();
#
#    my $indels = 0;
#    while(my $line=<INFO>) {
#	if ($line =~ /Alphabet: (.*)$/) {
#	    $features{"alphabet"} = $1;
#	}
#	if ($line =~ /Alignment: (.+) columns of (.+) sequences/) 
#	{
#	    $features{"length"} = $1;
#	    $features{"n_sequences"} = $2;
#	}
#	if ($line =~ /sequence lengths: ([^ ]+)-([^ ]+)/) {
#	    $features{"min_length"} = $1;
#	    $features{"max_length"} = $2;
#	}
#	if ($line =~ m|w/  indels|) {
#	    $indels = 1;
#	}
#	next if ($indels == 0);
#
#	if ($line =~ / const.: ([^ ]+) \(([^ ]+)\%\)/) {
#	    $features{"n_const"} = $1;
#	    $features{"p_const"} = $2;
#	}
#	if ($line =~ /non-const.: ([^ ]+) \(([^ ]+)\%\)/) {
#	    $features{"n_non-const"} = $1;
#	    $features{"p_non-const"} = $2;
#	}
#	if ($line =~ /inform.: ([^ ]+) \(([^ ]+)\%\)/) {
#	    $features{"n_inform"} = $1;
#	    $features{"p_inform"} = $2;
#	}
#	if ($line =~ / ([^ ]+)% minimum sequence identity/){
#	    $features{"min_p_identity"} = $1;
#	}
#    }
#    return {%features};
#}


class MCMCRun(object):
    def __init__(self, mcmc_output):
        self.mcmc_output = mcmc_output
        self.trees_file = None
        self.log_file = None
        self.alignments_files = None
        self.input_files = None
        self.cmd = None

        if path.isdir(mcmc_output):
            self.dir = mcmc_output
            self.prefix = None
        else:
            self.dir = path.dirname(mcmc_output)
            self.prefix = path.basename(mcmc_output)

    # A log file might then be in "{}/{}.log".format(self.dir,self.prefix)
    def get_dir(self):
        return self.dir

    def get_prefix(self):
        return self.prefix
    
    def get_trees_file(self):
        return self.trees_file

    def get_input_files(self):
        return self.input_files

    def get_alignments_files(self):
        return self.alignments_files

    def get_log_file(self):
        return self.log_file

    def get_cmd(self):
        return self.cmd

    def input_files(self):
        return self.input_files

    def n_partitions(self):
        return 1

    def get_n_sequences(self):
        return None

class BAliPhyRun(MCMCRun):

    def get_n_sequences(self):
        features = get_alignment_info("Results/C1.P1.initial.fasta")
        return features["n_sequences"]

    def get_input_file_names_for_outfile(self,outfile):
        input_filenames = []
        with open(outfile,'r',encoding='utf-8') as outf:
            for line in outf:
                m = re.match(r'data(.+) = (.+)', line)
                if m:
                    input_filenames.append(m.group(2))

        return input_filenames

    def get_alignment_files(self):
        filenames = []
        for p in range(1,self.n_partitions()+1):
            filename = path.join(self.get_dir(),'C1.P{}.fastas'.format(p))
            if not path.exists(filename):
                filename = None
            filenames.append(filename)
        return filenames

    def get_header_attribute(self, attribute):
        with open(self.out_file,'r',encoding='utf-8') as file:
            reg = attribute + ': (.*)$'
            for line in file:
                m = re.match(reg,line)
                if m:
                    return m.group(1)
        return None

    def n_partitions(self):
        return len(self.get_input_files())

    def run(self,p):
        return self.mcmc_runs[p]

    def __init__(self,mcmc_output):
        super().__init__(mcmc_output)
        self.prefix = 'C1'
        self.out_file = check_file_exists(path.join(self.get_dir(),'C1.out'))
        self.input_files = self.get_input_file_names_for_outfile(self.out_file)
        self.trees_file = check_file_exists(path.join(self.get_dir(),'C1.trees'))
        self.alignments_files = self.get_alignment_files()
        self.MAP_file = check_file_exists(path.join(self.get_dir(),'C1.MAP'))
        self.cmd = self.get_header_attribute("command")
        print(self.get_cmd())
        print(self.get_input_files())
        print(self.get_alignments_files())

class BAliPhy2_1Run(BAliPhyRun):
    def __init__(self,mcmc_output):
        super().__init__(mcmc_output)
        self.log_file = check_file_exists(path.join(self.get_dir(),'C1.p'))

    def get_models(self, name1, name2):
        models = []
        with open(self.out_file,encoding='utf-8') as file:
            for line in file:
                m = re.match(name1+"([0-9]+) = (.+)", line)
                if m:
                    models.append(m.group(2))
                m = re.match(name1+"([0-9]+) ~ (.+)", line)
                if m:
                    models.append("~"+m.group(2))
                if re.match("^iterations = 0", line):
                    break
        return [make_extracted(model) for model in models]

class BAliPhy3Run(BAliPhyRun):
    def __init__(self,mcmc_output):
        super().__init__(mcmc_output)
        self.log_file = check_file_exists(path.join(self.get_dir(),'C1.log'))
        self.run_file = path.join(self.get_dir(),'C1.run.json')
        if not path.exists(self.run_file):
            self.run_file = None

    def get_json_from_run_file(self):
        return get_json_from_file(self.run_file)

    def get_models(self, name1, name2):
        j = self.get_json_from_run_file()
        return [make_extracted(model) for model in j[name2]]

class TreeFileRun(MCMCRun):
    def __init__(self,mcmc_output):
        super().__init__(mcmc_output)

class LogFileRun(MCMCRun):
    def __init__(self,mcmc_output):
        super().__init__(mcmc_output)

# Since PhyloBayes doesn't make new run files for each dir, maybe we need to file the different prefixes, and pass them to PhyloBayes?
# A prefix could be like dir/prefix (e.g. dir/prefix.trace) or prefix (e.g. prefix.trace)
class PhyloBayesRun(MCMCRun):
    def __init__(self,mcmc_output):
        super().__init__(mcmc_output)
        for treelist in glob.glob(path.join(self.get_dir(),'*.treelist')):
            self.prefix = get_file_prefix(treelist)
            self.log_file = check_file_exists("{}.trace".format(prefix))
            self.out_file = check_file_exists("{}.log".format(prefix))
    
    def get_input_file_names_for_outfile(self,outfile):
        return get_value_from_file(outfile,"data file :")

    def get_n_sequences(self):
        return get_value_from_file(self.out_file, "number of taxa:")

class BEASTRun(MCMCRun):
    def __init__(self,mcmc_output):
        super().__init__(mcmc_output)
        for tree_file in glob.glob(path.join(self.get_dir(),'*,trees')):
            check_file_exists(tree_file)
            self.prefix = get_file_prefix(tree_file)
            self.log_file = check_file_exists("{}.log".format(prefix))

def ConstructRun(mcmc_output):
    if not path.exists(mcmc_output):
        print("No file or directory '{}'".format(mcmc_output))
        exit(1)
    if not path.isdir(mcmc_output):
        return TreeFileRun(mcmc_output)

    if path.exists(path.join(mcmc_output,"C1.out")):
        if path.exists(path.join(mcmc_output,'C1.p')):
            return BAliPhy2_1Run(mcmc_output)
        if path.exists(path.join(mcmc_output,'C1.log')):
            return BAliPhy3Run(mcmc_output)
        print("Directory {} appears to be a BAli-Phy directory, but doesn't have 'C1.p' or 'C1.log'")
        exit(1)

    if glob.glob(path.join(mcmc_output,'*.treelist')):
        return PhyloBayesRun(mcmc_output)

    if glob.glob(path.join(mcmc_output,'*.trees')):
        return BEASTRun(mcmc_output)

    return None
        


class Analysis(object):

    def find_exe(self,name,message=None):
        exe = shutil.which(name)
        if exe is None:
            if message is not None:
                print("Program '{}' not found: {}".format(name,message))
            else:
                print("Program '{}' not found.".format(name))
        return exe

    def __init__(self,args,mcmc_outputs):
        self.mcmc_runs = [ConstructRun(mcmc_run) for mcmc_run in mcmc_outputs]

        self.trees_consensus_exe = self.find_exe('trees-consensus', message="See the main for adding the bali-phy programs to your PATH.")
        if self.trees_consensus_exe is None:
            exit(1)

        self.draw_tree_exe = self.find_exe('draw-tree', message="Tree pictures will not be generated.\n")
        # FIXME - maybe switch from gnuplot to R?
        self.gnuplot_exe = self.find_exe('gnuplot', message='Some graphs will not be generated.\n')
        self.R_exe = self.find_exe('R', message='Some mixing graphs will not be generated.\n')

        self.subsample = None
        self.sub_partitions = False
        self.prune = None
        self.burnin = None

        for mcmc_run in self.mcmc_runs:
            print(mcmc_run.get_dir())

        self.initialize_results_directory()
        self.log_shell_cmds = open("Results/commands.log",'w',encoding='utf-8')

        for i in range(len(self.mcmc_runs)):
            if self.mcmc_runs[i].get_cmd() != self.mcmc_runs[0].get_cmd():
                print("WARNING: Commands differ!\n   {}\n   {}\n".format(self.mcmc_runs[0].get_cmd(),
                                                                         self.mcmc_runs[i].get_cmd()))
        self.get_input_files()

    def get_input_files(self):
        result = self.mcmc_runs[0].get_input_files()
        for run in self.mcmc_runs:
            result2 = run.get_input_files()
            if result != result2:
                raise Exception("Differences in get_input_files!")
        return result

    def get_alignments_files(self):
        return [run.get_alignments_files() for run in self.mcmc_runs]

    def n_partitions(self):
        return first_all_same([run.n_partitions() for run in self.mcmc_runs])

    def get_models(self, name1, name2):
        return first_all_same([run.get_models(name1,name2) for run in self.mcmc_runs])

    def exec_show(self,cmd):
        print(cmd,file=self.log_shell_cmds)

        result = subprocess.run(cmd,stdout=subprocess.PIPE)
        message = result.stdout.decode('utf-8')
        code = result.returncode
        if code != 0:
            print("Subcommand file! (code {})",file=sys.stderr)
            print("Subcommand file! (code {})",file=self.log_shell_cmds)
            print("command: {}".format(cmd),file=sys.stderr)
            print("message: {}".format(message),file=sys.stderr)
            exit(code)
        elif (self.verbose):
            print("\n\t{}\n".format(cmd))
        return result

    def exec_result(self,cmd):
        print(cmd,file=self.log_shell_cmds)
        if self.verbose:
            print(cmd,file=sys.stderr)
        result = subprocess.run(cmd,stdout=subprocess.PIPE)
        message = result.stdout.decode('utf-8')
        return result.returncode

    def load_analysis_properties(self):
        if not path.exists("Results/properties.json"):
            return dict()
        return get_json_from_file("Results/properties.json")

    def save_analysis_properties(self, properties):
        with open('Results/properties.json', 'w') as outfile:
            json.dump(properties, outfile, indent=2)

    def read_analysis_property(self,key):
        return self.load_analysis_properties()[key]

    def check_analysis_property(self,key,value):
        properties = self.load_analysis_properties()
        if key in properties:
            return value == properties[key]
        else:
            return True

    def write_analysis_property(self,key,value):
        properties = self.load_analysis_properties()
        properties[key] = value
        self.save_analysis_properties(properties)
        return properties
        
    def write_input_file_names(input_file_names):
        with open("Results/input_files",w,encoding='utf-8') as out:
            print(input_file_names.join('\n'),file=out)

    def initialize_results_directory(self):
        reuse = self.check_analysis_property("burnin", self.burnin)
        reuse = reuse and self.check_analysis_property("alignment_file_names",self.get_alignments_files())
        reuse = reuse and self.check_analysis_property("input_files", self.get_input_files())

        if not reuse and path.exists("Results"):
            new_dir_name = get_unused_dir_name()
            if path.exists("Results"):
                print("Renaming 'Results/' to '{}'\n".format(new_dir_name))
                os.rename("Results",new_dir_name)

        if not path.exists("Results"):
            print("Creating new directory Results/ for summary files.")
            os.mkdir("Results")
            os.mkdir("Results/Work")

        self.write_analysis_property("burnin", self.burnin)
        self.write_analysis_property("input_files", self.get_input_files())
        self.write_analysis_property("alignment_file_names", self.get_alignments_files())

#----------------------------- SETUP 1 --------------------------#
if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Generate an HTML report summarizing MCMC runs for BAli-Phy and other software.",
                                     epilog= "Example: bp-analyze analysis-dir-1 analysis-dir-2")

    parser.add_argument("mcmc_outputs", default=['.'], help="Subdirectories with MCMC runs",nargs='*')
    parser.add_argument("--clean", help="Delete generated files")
    parser.add_argument("--verbose", help="Be verbose")
    parser.add_argument("--skip", help="Skip NUM iterations as burnin")
    parser.add_argument("--subsample", help="Keep only every NUM iterations")
    parser.add_argument("--prune", help="Taxa to remove")
    parser.add_argument("--muscle")
    parser.add_argument("--probcons")
    parser.add_argument("--mafft")
    parser.add_argument("--max")
    parser.add_argument("--mc-tree")
    # FIXME: change subdirs to check if we've got a list of tree files instead
    parser.add_argument("--tree-file",nargs='*')
    args = parser.parse_args()

    analysis = Analysis(args,args.mcmc_outputs)

    smodels = analysis.get_models("subst model", "smodels")
    imodels = analysis.get_models("indel model", "imodels")
    scale_models = analysis.get_models("scale model", "scales")
    print(smodels)

#my $topology_prior;
#my $branch_prior;
#&get_tree_prior();
#
#my @smodel_indices = @{ get_smodel_indices() };
#push @smodel_indices,0 if ($#smodel_indices == -1);
#
#my @imodel_indices = @{ get_imodel_indices() };
#if ($#imodel_indices == -1)
#{
#    if ($#imodels == -1 || $imodels[0] eq "none") {
#	push @imodel_indices,-1;
#    }
#    else {
#	push @imodel_indices,0;
#    }
#}
#
#my @scale_model_indices = @{ get_scale_model_indices() };
#push @scale_model_indices,0 if ($#scale_model_indices == -1);
#
#my @alphabets = get_alphabets();
#
#my %tree_name = ();
## This is necessary to display them in order:
#my @trees = (); 
#
##
#
#my @tree_consensus_values = sort(0.5,0.66,0.8,0.9,0.95,0.99,1.0);
#
#my @alignment_consensus_values;
#if ($speed == 0) {
#    @alignment_consensus_values = sort(0.1,0.25,0.5,0.75);
#}
#elsif ($speed == 1) # default
#{
#    @alignment_consensus_values = (0.5);
#}
#elsif ($speed == 2) {
#    @alignment_consensus_values = ();
#}
#
#&determine_burnin();
#
## create a new directory, decide whether or not to reuse existing directory
#&initialize_results_directory();
#
#open my $LOG, ">>Results/bp-analyze.log";
#print $LOG "------------------------------------------------\n";
#
#&compute_tree_and_parameter_files_for_heated_chains();
#
## 5. Summarize scalar parameters
#my $max_arg = "";
#$max_arg = "--max=$max_iter" if (defined($max_iter));
#
#my $skip="";
#$skip="--skip=$burnin" if ($tree_files[0] ne "Results/T1.trees");
#
#my $subsample_string = "--subsample=$subsample";
#$subsample_string = "" if ($subsample == 1);
#
#if ($personality =~ "bali-phy.*" || $personality eq "beast" || $personality eq "phylobayes") {
#    print "Summarizing distribution of numerical parameters...";
#    if (! more_recent_than_all_of("Results/Report",[@parameter_files])) {
#	exec_show("statreport $subsample_string $max_arg $skip @parameter_files > Results/Report");
#    }
#    print "done.\n";
#}
#    
## 1. compute consensus trees
#my $min_support_arg = "";
#$min_support_arg = "--min-support=$min_support" if (defined($min_support));
#
#my $maj_consensus_arg = "--consensus=". get_consensus_arg("PP.tree",\@tree_consensus_values);
#my $e_consensus_arg = "";
#$e_consensus_arg = "--extended-consensus=". get_consensus_arg("mtree",\@tree_consensus_values) if ($sub_partitions);
#my $el_consensus_arg = "";
#$el_consensus_arg = "--extended-consensus-L=". get_consensus_arg("mlengths",\@tree_consensus_values) if ($sub_partitions);
#my $consensus_arg = "$maj_consensus_arg $e_consensus_arg $el_consensus_arg";
#
#my @tree_names = get_consensus_trees("PP.tree",\@tree_consensus_values);
#@tree_names = ("Results/greedy.PP.tree","Results/MAP.PP.tree",@tree_names);
#    
#my $size_arg = "";
#$size_arg = "--size=$max_iter" if defined($max_iter);
#my $prune_arg = "";
#my $prune_arg2 = "";
#
#print "\nSummarizing topology distribution ... ";
#if (-z "Results/consensus" || ! more_recent_than_all_of("Results/consensus",[@tree_files])) {
#    my $sub_string = "--sub-partitions";
#    $sub_string = "" if (!$sub_partitions);
#    $prune_arg = "--ignore $prune" if (defined($prune));
#
#    my $select_trees_arg = "$max_arg $skip $subsample_string $prune_arg";
#    my $levels_arg = "--support-levels=Results/c-levels.plot";
#    $levels_arg = "$levels_arg --extended-support-levels=Results/extended-c-levels.plot" if ($sub_partitions);
#    exec_show("trees-consensus @tree_files $select_trees_arg $min_support_arg $sub_string $consensus_arg $levels_arg --map-tree=Results/MAP.PP.tree --greedy-consensus=Results/greedy.PP.tree --report=Results/consensus");
#    for my $tree (@tree_names)
#    {
#	my $tree2 = $tree;
#	$tree2 =~ s/.PP.tree$/.tree/;
#	exec_show("tree-tool $tree --strip-internal-names --name-all-nodes > $tree2")
#    }
#}
#print "done.\n";
#
#
#print "\nComputing mean branch lengths ... ";
#    for my $cvalue (@tree_consensus_values)
#    {
#	my $value = $cvalue*100;
#	
#	my $tree = "c$value";
#
#	# No node lengths???
#	my $filename1 = "Results/$tree.tree";
#	my $filename2 = "Results/$tree.mtree";
#	
#        # Generate trees w/ node lengths.
#	if (! more_recent_than("Results/$tree.ltree",$tree_files[0])) 
#	{
#	    $prune_arg2 = "--prune $prune" if (defined($prune));
#	    exec_show("tree-mean-lengths Results/$tree.tree --safe --show-node-lengths $max_arg $skip $subsample_string $prune_arg2 < $tree_files[0] > Results/$tree.ltree");
# 	}
#    }
#print "done.\n";
#
#
#
## 2. Draw trees
#
#for my $cvalue (@tree_consensus_values)
#{
#    my $value = $cvalue*100;
#    my $tree = "c$value";
#    $tree_name{$tree} = "$value\% consensus";
#    push @trees,$tree;
#}
#push @trees, "MAP";
#$tree_name{"MAP"} = "MAP";
#
#push @trees, "greedy";
#$tree_name{"greedy"} = "greedy";
#
#&draw_trees();
#
#&mixing_diagnostics();
#
#my @SRQ = &SRQ_plots();
#
#my $tree_MDS = &tree_MDS();
#
#my @alignments = ();
#my %alignment_names = ();
#my @AU_alignments = ();
#
#&compute_initial_alignments();
#
#&compute_muscle_alignment();
#
#&compute_probcons_alignment();
#
#&compute_wpd_alignments();
#
#&compute_consensus_alignments() if ($do_consensus_alignments);
#
#&draw_alignments();
#
#&compute_ancestral_states(); # This needs to be after draw_alignments(), which reorders the alignments.
#
#&compute_and_draw_AU_plots();
#
#my $marginal_prob = &compute_marginal_likelihood();
#
## 13. Get # of topologies sampled
#
#my $n_topologies = exec_show("pickout n_topologies -n < Results/consensus");
#
#my $n_topologies_95;
#
#open my $CONSENSUS, "Results/consensus";
#while(my $line = portable_readline($CONSENSUS)) {
#    if ($line =~ /contains ([^ ]+) topologies/) {
#	$n_topologies_95 = $1;
#	last;
#    }
#}
#close $CONSENSUS;
#
## 14. Traceplots for scalar variables
#
#my @var_names = ();
#my %median = ();
#my %CI_low = ();
#my %CI_high = ();
#my %PSRF_CI80 = ();
#my %PSRF_RCF = ();
#my %ACT = ();
#my %ESS = ();
#my %Burnin = ();
#
#&generate_trace_plots();
#
#&print_index_html();
#
#print "\nReport written to 'Results/index.html'\n";
#
##---------------------------------------- end execution ---------------------------
#
#
#sub html_header
#{
#    my $title = shift;
#    my $section = "";
#    $section .= '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
#<html xmlns="http://www.w3.org/1999/xhtml">
#  <head>
#';
#    $section .= "<title>BAli-Phy: $title</title>\n";
#    $section .= 
#'    <style type="text/css">
#      body {margin:0; padding:0}
#      ol li {padding-bottom:0.5em}
#
#      th {padding-left: 0.5em; padding-right:0.5em}
#      td {padding: 0.1em;}
#      .backlit2 td {padding-left: 0.5em;}
#      .backlit2 td {padding-right: 1.0em;}
#
##topbar {
#	background-color: rgb(201,217,233);
#	margin: 0;
#	padding: 0.5em;
#	display: table;
#        width: 100%;
#        position: fixed;
#        top: 0;
#}
#
##topbar #menu {
#//	font-size: 90%;
#	display: table-cell;
#	text-align: right; 
#        // Leave space for the menubar
#        padding-right: 0.75em;
#}
##topbar #path {
#	font-weight: bold;
#	display: table-cell;
#	text-align: left; 
#	white-space: nowrap;
#}
#
#      .content {margin:1em; margin-top: 3em}
#      .backlit td {background: rgb(220,220,220);}
#
#      :target:before {
#        content:"";
#        display:block;
#        height:2em; /* fixed header height*/
#        margin:-2em 0 0; /* negative fixed header height */
#      }
#
#      h1 {font-size: 150%;}
#      h2 {font-size: 130%; margin-top:2.5em; margin-bottom: 1em}
#      h3 {font-size: 110%; margin-bottom: 0.2em}// margin-top: 0.3em}
#
#      ul {margin-top: 0.4em;}
#
#      .center td {text-align: center};
#
#      *[title] {cursor:help;}
#      a[title] {vertical-align:top;color:#00f;font-size:70%; border-bottom:1px dotted;}
#      .floating_picture {float:left;padding-left:0.5em;padding-right:0.5em;margin:0.5em;}    
#      .r_floating_picture {float:right;padding-left:0.5em;padding-right:0.5em;margin:0.5em;}    
#      table.backlit2 {border-collapse: collapse; }
#      table.backlit2 tr:nth-child(odd) td { background-color: #F0F0F0; margin:0}
#      .clear {clear:both;}    
#
#      .modelname {font-weight: bold; color: blue}
#      table.model {margin-left: 2em}
#      table.model td {vertical-align: top}
#      table.models td {vertical-align: top}
#
#    </style>
#</head>
#<body>
#';
#    return $section;
#}
#
#sub topbar
#{
#    return '<p id="topbar"> 
#   <span id="path">Sections:</span>
#   <span id="menu">
#    [<a href="#data">Data+Model</a>]
#    [<a href="#parameters">Parameters</a>]
#    [<a href="#topology">Phylogeny</a>]
#    [<a href="#alignment">Alignment</a>]
#    [<a href="#ancestors">Ancestors</a>]
#    [<a href="#mixing">Mixing</a>]
#    [<a href="#analysis">Analysis</a>]
#    [<a href="#models">Models+Priors</a>]
#   </span>
#</p>
#';
#}
#
#sub print_model
#{
#    my $model = shift;
#
#    my $main= ${${model}}{'main'};
#
#    my $assign = '=';
#    if (substr($main,0,1) eq '~')
#    {
#	$main = substr($main,1);
#	$assign = "~";
#    }
#
#    my $section = $main . "\n";
#
#    my @extracted = @{${${model}}{'extracted'}};
#    if (scalar @extracted != 0)
#    {
#	$section .= "<table class=\"model\">\n";
#	foreach my $arg (@extracted)
#	{
#	    my $name = ${$arg}[0];
#	    my $submodel = ${$arg}[1];
#	    (my $subassign,$submodel) = print_model($submodel);
#	    $section .= "<tr><td>$name</td><td>$subassign</td><td>$submodel</td></tr>\n";
#	}
#	$section .= "</table>";
#    }
#    return ($assign,$section);
#}
#
#sub print_models
#{
#    my $name = shift;
#    my $models = shift;
#    my $section = "<table class=\"models\">\n";
#    my $i=1;
#    foreach my $model (@$models)
#    {
#	my $model_name = $name.$i;
#	my ($assign, $model_html) = print_model($model);
#	$section .= "<tr><td><a name=\"$model_name\" class=\"anchor\"></a><span class=\"modelname\">$model_name</span></td><td>$assign</td><td>\n";
#	$section .= $model_html;
#	$section .= "</td></tr>\n";
#	$i = $i + 1;
#    }
#    $section .= "</table>\n";
#    return $section;
#}
#
#
#sub print_data_and_model
#{
#    my $section = "";
#    $section .= "<h2 style=\"clear:both\"><a class=\"anchor\" name=\"data\"></a>Data &amp; Model</h2>\n";
#    $section .= "<table class=\"backlit2\">\n";
#    $section .= "<tr><th>Partition</th><th>Sequences</th><th>Lengths</th><th>Alphabet</th><th>Substitution&nbsp;Model</th><th>Indel&nbsp;Model</th><th>Scale&nbsp;Model</th></tr>\n";
#    for(my $p=0;$p<=$#input_file_names;$p++) 
#    {
#	$section .= "<tr>\n";
#	$section .= " <td>".($p+1)."</td>\n";
#	$section .= " <td>$input_file_names[$p]</td>\n";
#
#	my $features = get_alignment_info("Results/C1.P".($p+1).".initial.fasta");
#	my $min = $features->{'min_length'};
#	my $max = $features->{'max_length'};
#	my $alphabet = $alphabets[$p];
#
#	$section .= "<td>";
#	$section .= "$min - $max" if defined ($min);
#	$section .= "</td>\n";
#
#	$section .= "<td>$alphabet</td>" if ($personality =~ "bali-phy.*");
#
#	if ($personality =~ "bali-phy.*")
#	{
#	    my $smodel = $smodels[$smodel_indices[$p]];
#	    $smodel = ${${smodel}}{'main'};
#	    my $index = $smodel_indices[$p]+1;
#	    my $target = "S".$index;
#
#	    my $link = "<a href=\"#$target\">$target</a>";
#
#	    $section .= "<td>$link = $smodel</td>\n";
#	}
#	else
#	{
#	    $section .= " <td></td>\n";
#	}
#
#	my $imodel ="none";
#
#	if ($imodel_indices[$p] ne "--")
#	{
#	    $imodel = $imodels[$imodel_indices[$p]];
#	    $imodel = ${${imodel}}{'main'};
#
#	    my $index = $imodel_indices[$p]+1;
#	    my $target = "I".$index;
#
#	    my $link = "<a href=\"#$target\">$target</a>";
#	    $imodel = "$link = $imodel";
#	}
#	$section .= " <td>$imodel</td>\n";
#
#	if ($personality =~ "bali-phy.*")
#	{
#	    my $scale_model = $scale_models[$scale_model_indices[$p]];
#	    $scale_model = ${${scale_model}}{'main'};
#
#	    my $index = $scale_model_indices[$p]+1;
#	    my $target = "scale".$index;
#
#	    my $link = "<a href=\"#$target\">$target</a>";
#
#	    my $assign = '=';
#	    if (substr($scale_model,0,1) eq "~")
#	    {
#		$scale_model = substr($scale_model,1);
#		$assign = "~";
#	    }
#	    $section .= " <td>$link $assign $scale_model</td>\n";
#	}
#	else
#	{
#	    $section .= " <td></td>\n";
#	}
#	$section .= "</tr>\n";
#    }
#    
#    $section .= "</table>\n";
#    return $section;
#}
#
#sub print_model_section
#{
#
#    my $section = "<h2 style=\"clear:both\"><a class=\"anchor\" name=\"models\"></a>Model and priors</h2>\n";
#    
#    $section .= "<h3 style=\"clear:both\"><a class=\"anchor\" name=\"tree\"></a>Tree (+priors)</h3>\n";
##    $section .= "<table class=\"backlit2\">\n";
#    $section .= "<table>\n";
#    $section .= "<tr><td class=\"modelname\">topology</td><td>$topology_prior</td></tr>" if (defined($topology_prior));
#    $section .= "<tr><td class=\"modelname\">branch lengths</td><td>$branch_prior</td></tr>" if (defined($branch_prior));
#    $section .= "</table>\n";
#    
#    $section .= "<h3 style=\"clear:both\"><a class=\"anchor\" name=\"smodel\"></a>Substitution model (+priors)</h3>\n";
#    $section .= print_models("S",\@smodels);
#
#    $section .= "<h3 style=\"clear:both\"><a class=\"anchor\" name=\"imodel\"></a>Indel model (+priors)</h3>\n";
#    $section .= print_models("I",\@imodels);
#
#    $section .= "<h3 style=\"clear:both\"><a class=\"anchor\" name=\"scales\"></a>Scales (+priors)</h3>\n";
#    $section .= print_models("scale",\@scale_models);
#
#    return $section;
#}
#
#sub section_analysis
#{
#    my $section = "";
#    $section .= "<br/><hr/><br/>\n";
#    $section .= "<h2><a class=\"anchor\" name=\"analysis\"></a>Analysis</h2>\n";
#    $section .= "<p>" if (!$commands_differ || !$directories_differ || !$versions_differ);
#    $section .= "<b>command line</b>: $commands[0]</br>\n" if (!$commands_differ);
#    $section .= "<b>directory</b>: $directories[0]</br>\n" if (!$directories_differ);
#    $section .= "<b>version</b>: $versions[0]\n" if (!$versions_differ);
#    $section .= "</p>" if (!$commands_differ || !$directories_differ || !$versions_differ);
#    #$section .= '<table style="width:100%">'."\n";
#
#    #$section .= '<table class="backlit2 center" style="width:100%">'."\n";
#    $section .= '<table class="backlit2 center">'."\n";
#    $section .= "<tr><th>chain #</th>";
#    $section .= "<th>version</th>" if ($versions_differ);
#    $section .= "<th>burnin</th><th>subsample</th><th>Iterations (after burnin)</th>";
#    $section .= "<th>command line</th>" if ($commands_differ);
#    $section .= "<th>subdirectory</th>";
#    $section .= "<th>directory</th>" if ($directories_differ);
#    $section .= "</tr>\n";
#    for(my $i=0;$i<=$#out_files;$i++)
#    {
#$section .= "<tr>\n";
#
#$section .= "  <td>".($i+1)."</td>\n";
#$section .= "  <td>$versions[$i]</td>\n" if ($versions_differ);
#
#$section .= "  <td>$burnin</td>\n";
#$section .= "  <td>$subsample</td>\n";
#
#my $remaining = ($n_iterations[$i] - $burnin)/$subsample;
#$section .= "  <td>$remaining</td>\n";
#
#$section .= "  <td>$commands[$i]</td>\n" if ($commands_differ);
#$section .= "  <td>$subdirs[$i]</td>\n";
#$section .= "  <td>$directories[$i]</td>\n" if ($directories_differ);
#
#
#$section .= "</tr>\n";
#    }
#$section .= "</table>\n";
#
#$section .= "<br/>\n";
#$section .= '<table style="width:100%">'."\n";
#$section .= "<tr>\n";
#$section .= "  <td>$marginal_prob</td>\n";
#$section .= "  <td>Complete sample: $n_topologies topologies</td>\n";
#$section .= "  <td>95% Bayesian credible interval: $n_topologies_95 topologies</td>\n";
#$section .= "</tr>\n";
#$section .= "</table>\n";
#
#
#    return $section;
#}
#
#sub html_svg_object
#{
#    my $url = shift;
#    my $width = shift;
#    my $height = shift;
#    my $classes = shift;
#    my $extra = shift;
#
#    $classes = [] if (!defined($classes));
#    push @$classes,"svg-image";
#    my $class = join(' ',@$classes);
#
#    my $svg = "<object data=\"$url\" type=\"image/svg+xml\" class=\"$class\"";
#
#    $svg = "$svg width=\"$width\"" if (defined($width));
#    $svg = "$svg height=\"$height\"" if (defined($height));
#    $svg = "$svg $extra" if (defined($extra));
#    $svg = "$svg ></object>";
#
#    return $svg;
#}
#
#sub html_svg_img
#{
#    my $url = shift;
#    my $width = shift;
#    my $height = shift;
#    my $classes = shift;
#    my $extra = shift;
#
#    $classes = [] if (!defined($classes));
#    push @$classes,"svg-image";
#    my $class = join(' ',@$classes);
#
#    my $svg = "<img src=\"$url\" class=\"$class\"";
#
#    $svg = "$svg width=\"$width\"" if (defined($width) and ($width ne ""));
#    $svg = "$svg height=\"$height\"" if (defined($height) and ($height ne ""));
#    $svg = "$svg $extra" if (defined($extra));
#    $svg = "$svg />";
#
#    return $svg;
#}
#
#sub html_svg
#{
#    return html_svg_img(@_);
#}
#
#sub section_phylogeny_distribution
#{
#    my $section = "";
#    $section .= "<h2><a class=\"anchor\" name=\"topology\"></a>Phylogeny Distribution</h2>\n";
#    $section .= html_svg("c-levels.svg","35%","",["r_floating_picture"]);
#
#
#    $section .= html_svg("c50-tree.svg","25%","",["floating_picture"]);
#    $section .= '<table>'."\n";
#    $section .= "<tr><td>Partition support: <a href=\"consensus\">Summary</a></td><td><a href=\"partitions.bs\">Across chains</a></td></tr>\n";
##    $section .= "<tr><td><span title=\"How many partitions are supported at each level of Posterior Log Odds (LOD)?\">Partition support graph:</span> <a href=\"c-levels.svg\">SVG</a></td></tr>\n";
#    $section .= "</table>\n";
#
#    $section .= "<table>\n";
#    for my $tree (@trees)
#    {
#	my $name = $tree_name{$tree};
#	$section .= "<tr>";
#	$section .= "<td>$name</td>";
##    $section .= "<td><a href=\"$tree.topology\">-L</a></td>";
#	$section .= "<td><a href=\"$tree.tree\">Newick</a> (<a href=\"$tree.PP.tree\">+PP</a>)</td>";
#	$section .= "<td><a href=\"$tree-tree.pdf\">PDF</a></td>";
#	$section .= "<td><a href=\"$tree-tree.svg\">SVG</a></td>";
#	
#	if ($sub_partitions && (-f "Results/$tree.mtree" || -f "Results/$tree-mctree.svg" ||
#				-f "Results/$tree-mctree.pdf" )) 
#	{
#	    $section .= "<td>MC Tree:</td>";
#	}
#	else {
#	    $section .= "<td></td>"     
#	}
#	
#	if ($sub_partitions && -f "Results/$tree.mtree") {
#	    $section .= "<td><a href=\"$tree.mtree\">-L</a></td>"     
#	}
#	else {
#	    $section .= "<td></td>"     
#	}
#	if ($sub_partitions && -f "Results/$tree-mctree.pdf") {
#	    $section .= "<td><a href=\"$tree-mctree.pdf\">PDF</a></td>";
#	}
#	else {
#	    $section .= "<td></td>"     
#	}
#	if ($sub_partitions && -f "Results/$tree-mctree.svg") {
#	    $section .= "<td><a href=\"$tree-mctree.svg\">SVG</a></td>";
#	}
#	else {
#	    $section .= "<td></td>"     
#	}
#	$section .= "</tr>";
#    }
#    $section .= "</table>\n";
#    return $section;
#}
#
#sub section_alignment_distribution
#{
#    return "" if ($n_partitions == 0);
#
#    my $section .= "<h2 class=\"clear\"><a class=\"anchor\" name=\"alignment\"></a>Alignment Distribution</h2>\n";
#
#    for(my $i=0;$i<$n_partitions;$i++) 
#    {
#	my $p = $i+1;
#	$section .= "<h3>Partition $p</h3>\n";
#	$section .= "<table>\n";
#	$section .= "<tr>\n";
#	$section .= "<th></th>\n";
#	$section .= "<th></th>\n";
#	$section .= "<th></th>\n";
#	$section .= "<th title=\"Comparison of this alignment (top) to the WPD alignment (bottom)\">Diff</th>\n";
#	$section .= "<th></th>\n";
#	$section .= "<th style=\"padding-right:0.5em;padding-left:0.5em\" title=\"Percent identity of the most dissimilar sequences\">Min. %identity</th>\n";
#	$section .= "<th style=\"padding-right:0.5em;padding-left:0.5em\" title=\"Number of columns in the alignment\"># Sites</th>\n";
#	$section .= "<th style=\"padding-right:0.5em;padding-left:0.5em\" title=\"Number of invariant columns\">Constant</th>\n";
##    $section .= "<th style=\"padding-right:0.5em;padding-left:0.5em\" title=\"Number of variant columns\">Variable</th>\n";
#	$section .= "<th title=\"Number of parsiomny-informative columns.\">Informative</th>\n";
#	$section .= "</tr>\n";
#	for my $alignment (@alignments)
#	{
#	    next if ($alignment !~ /^C1.P$p./ && $alignment !~ /^P$p./);
#	    my $name = $alignment_names{$alignment};
#	    my $features = get_alignment_info("Results/$alignment.fasta");
#	    
#	    $section .= "<tr>\n";
#	    $section .= "<td>$name</td>\n";
#	    $section .= "<td><a href=\"$alignment.fasta\">FASTA</a></td>\n";
#	    if (-f "Results/$alignment.html") {
#		$section .= "<td><a href=\"$alignment.html\">HTML</a></td>\n";
#	    }
#	    else {
#		$section .= "<td></td>\n";
#	    }
#	    if (-f "Results/$alignment-diff.html") {
#		$section .= "<td><a href=\"$alignment-diff.html\">Diff</a></td>\n";
#	    }
#	    else {
#		$section .= "<td></td>\n";
#	    }
#	    if (-f "Results/$alignment-AU.html") {
#		$section .= "<td><a href=\"$alignment-AU.html\">AU</a></td>\n";
#	    }
#	    else {
#		$section .= "<td></td>\n";
#	    }
#	    $section .= "<td style=\"text-align: center\">${$features}{'min_p_identity'}%</td>\n";
#	    $section .= "<td style=\"text-align: center\">${$features}{'length'}</td>\n";
#	    $section .= "<td style=\"text-align: center\">${$features}{'n_const'} (${$features}{'p_const'}%)</td>\n";
##	$section .= "<td style=\"text-align: center\">${$features}{'n_non-const'} (${$features}{'p_non-const'}%)</td>\n";
#	    $section .= "<td style=\"text-align: center\">${$features}{'n_inform'} (${$features}{'p_inform'}%)</td>\n";
#	    $section .= "</tr>\n";
#	}
#	$section .= "</table>\n";
#    }
#    return $section;
#}
#
#
#sub section_ancestral_sequences
#{
#    return "" if ($n_partitions == 0);
#
#    my $section .= "<h2 class=\"clear\"><a class=\"anchor\" name=\"ancestors\"></a>Ancestral sequence estimates</h2>\n";
#
#    $section .= "<ul>\n";
#    for(my $i=0;$i<$n_partitions;$i++)
#    {
#        my $p = $i+1;
#        my $ancestors = "P${p}.ancestors.fasta";
#        next if (! -e "Results/$ancestors");
#
#        $section .= "    <li><a href=\"${ancestors}\">Partition ${p}</li>\n";
#    }
#    $section .= "</ul>\n";
#
#    return $section;
#}
#
#sub section_mixing
#{
#    my $section = "";
#
#    $section .= "<h2><a class=\"anchor\" name=\"mixing\"></a>Mixing</h2>\n";
#
#    $section .= '<table style="width:100%;clear:both"><tr>';
#    $section .= '<td style="vertical-align:top">';
#    
#    $section .= "<ol>\n";
##    for my $srq (@SRQ) {
##	$section .= "<li><a href=\"$srq.SRQ.png\">SRQ plot: $srq</a></li>\n";
##    }
##    $section .= '<li><a href="convergence-PP.pdf">Variation in split frequency estimates</a></li>'."\n" if (-f "Results/convergence-PP.pdf");
#    $section .= "</ol>\n";
#    
#
#    
#    $section .= "<p><b>Statistics:</b></p>";
#    $section .= "<table class=\"backlit2\">";
##    $section .= "<tr><th>burnin (scalar)</th><th>ESS (scalar)</th><th>ESS (partition)</th><th>ASDSF</th><th>MSDSF</th><th>PSRF-CI80%</th><th>PSRF-RCF</th></tr>";
#
#    my $burnin_before = "NA";
#    my $min_NE = "NA";
#    if ($#parameter_files != -1)
#    {
#	$burnin_before = get_value_from_file('Results/Report','min burnin <=');
#	$burnin_before = "Not Converged!" if ($burnin_before eq "Not");
#	$min_NE = get_value_from_file('Results/Report','Ne  >=');
#    }
#    
#    $section .= "<tr><td><b>scalar burnin</b></td><td>$burnin_before</td></tr>";
#    $section .= "<tr><td><b>scalar ESS</b></td><td>$min_NE</td></tr>";
#    my $min_NE_partition = get_value_from_file('Results/partitions.bs','min Ne =');
#    $section .= "<tr><td><b title=\"Effective Sample Size for bit-vectors of partition support (smallest)\">topological ESS</b></td><td>$min_NE_partition</td></tr>";
#
#    my $asdsf = get_value_from_file('Results/partitions.bs','ASDSF\[min=0.100\] =');
#    $asdsf = "NA" if (!defined($asdsf));
#    $section .= "<tr><td><b title=\"Average Standard Deviation of Split Frequencies\">ASDSF</b></td><td>$asdsf</td></tr>";
#
#    my $msdsf = get_value_from_file('Results/partitions.bs','MSDSF =');
#    $msdsf = "NA" if (!defined($msdsf));
#    $section .= "<tr><td><b title=\"Maximum Standard Deviation of Split Frequencies\">MSDSF</b></td><td>$msdsf</td></tr>";
#
#    my $psrf_80 = "NA";
#    my $psrf_rcf = "NA";
#    if ($#parameter_files != 0)
#    {
#        $psrf_80 = get_value_from_file('Results/Report','PSRF-80%CI <=');
#	$psrf_rcf = get_value_from_file('Results/Report','PSRF-RCF <=');
#    }
#    $section .= "<tr><td><b>PSRF CI80%</b></td><td>$psrf_80</td></tr>";
#    $section .= "<tr><td><b>PSRF RCF</b></td><td>$psrf_rcf</td></tr>";
#
#    $section .= "</table>\n";
#
#    ###### What file should we show for MDS?
#    my $MDS_figure;
#    my $MDS_figure_3d;
#    my $MDS_title;
#    if (-e "Results/tree-1-2-3.svg")
#    {
#	$MDS_figure = "tree-1-2-3.svg";
#	$MDS_figure_3d = "tree-1-2-3.points";
#	$MDS_title = "the first 3 chains";
#    }
#    elsif (-e "Results/tree-1-2.svg")
#    {
#	$MDS_figure = "tree-1-2.svg";
#	$MDS_figure_3d = "tree-1-2.points";
#	$MDS_title = "2 chains";
#    }
#    elsif (-e "Results/tree1.svg")
#    {
#	$MDS_figure = "tree1.svg";
#	$MDS_figure_3d = "tree-3D1-1.points";
#	$MDS_title = "1 chain";
#    }
#    else
#    {
#	$MDS_title = "1 chain" if ($n_chains == 1);
#	$MDS_title = "2 chains" if ($n_chains == 2);
#	$MDS_title = "3 chains" if ($n_chains == 3);
#	$MDS_title = "the first 3 chains" if ($n_chains > 3);
#    }
#    $section .= '</td>';
##$section .= '<object class="r_floating_picture" data="partitions.SRQ.svg" type="image/svg+xml" height="200pt"></object>';
#    $section .= '<td>';
##    $section .= '</td>';
##$section .= '<object class="r_floating_picture" data="c50.SRQ.svg" type="image/svg+xml" height="200pt"></object>';
##$section .= '<embed class="r_floating_picture" src="c50.SRQ.svg" type="image/svg+xml" height="200" />';
##$section .= '<embed class="r_floating_picture" src="partitions.SRQ.svg" type="image/svg+xml" height="200" />';
##    $section .= '<td>';
#    $section .= '<img src="c50.SRQ.png" alt="SRQ plot for supprt of 50% consensus tree."/>';
#    $section .= '<img src="partitions.SRQ.png" alt="SRQ plot for support of each partition."/>';
#    $section .= '</td>';
#
#    $section .= '</tr></table>';
#    
#    ###### Table of MDS versus 
#    $section .= '<table style="width:100%;clear:both"><tr>';
#
#    $section .= '<td style="width:40%;vertical-align:top">';
#    $section .= "<h4 style=\"text-align:center\">Projection of RF distances for $MDS_title (<a href=\"https://doi.org/10.1080/10635150590946961\">Hillis et al 2005</a>)</h4>";
#    if (defined($MDS_figure))
#    {
#
#	$section .= html_svg($MDS_figure,"90%","",[]);
#	$section .= "<a href='${MDS_figure_3d}.html'>3D</a>";
#    }
#    elsif (!$have_R)
#    {
#	$section .= "<p>Not generated: can't find R.</p>";
#    }
#    $section .= '</td>';
#    
#    $section .= '<td style="width:40%;vertical-align:top">';
#    $section .= '<h4 style="text-align:center">Variation of split PPs across chains (<a href="https://doi.org/10.1080/10635150600812544">Beiko et al 2006</a>)</h4>';
#    if (-e "Results/convergence1-PP.svg")
#    {
#	$section .= html_svg("convergence1-PP.svg","90%","",[])
#    }
#    else
#    {
#	if (!$have_R)
#	{
#	    $section .= "<p>Not generated: can't find R.</p>";
#	}
#	elsif ($n_chains == 1)
#	{
#	    $section .= "<p>Not generated: multiple chains needed.</p>";
#	}
#	else
#	{
#	    $section .= "<p>Not generated.</p>";
#	}
#    }
#
#    if (-e "Results/convergence2-PP.svg")
#    {
#	$section .= "<br/><br/><br/><br/>";
#	$section .= html_svg("convergence2-PP.svg","90%","",[]);
#    }
#
#    $section .= '</td>';
#    
#
#    $section .= '</tr></table>';
#
#my $tne_string = exec_show("pickout -n Ne < Results/partitions.bs");
#my @tne_array = split(/\n/,$tne_string);
#@tne_array = sort {$a <=> $b} @tne_array;
##my $min_tESS = $tne_array[0];
#my $min_tESS = exec_show("pickout -n 'min Ne' < Results/partitions.bs");
#
#print "\n";
#print "NOTE: burnin (scalar) <= $burnin_before\n" if defined($burnin_before);
#print "NOTE: min_ESS (scalar)    = $min_ESS\n" if defined($min_ESS);
#print "NOTE: min_ESS (partition) = $min_tESS\n" if defined($min_tESS);
#print "NOTE: ASDSF = $asdsf\n" if defined($asdsf);
#print "NOTE: MSDSF = $msdsf\n" if defined($msdsf);
#print "NOTE: PSRF-80%CI = $psrf_80\n" if defined($psrf_80);
#print "NOTE: PSRF-RCF = $psrf_rcf\n" if defined($psrf_rcf);
#
#    return $section;
#}
#
#sub section_scalar_variables
#{
#    my $section = "";
#    if ($#var_names != -1) {
#	$section .= "<h2 class=\"clear\"><a class=\"anchor\" name=\"parameters\"></a>Scalar variables</h2>\n";
#
#	$section .= "<table class=\"backlit2\">\n";
#	$section .= "<tr><th>Statistic</th><th>Median</th><th title=\"95% Bayesian Credible Interval\">95% BCI</th><th title=\"Auto-Correlation Time\">ACT</th><th title=\"Effective Sample Size\">ESS</th><th>burnin</th><th title=\"Potential Scale Reduction Factor based on width of 80% credible interval\">PSRF-CI80%</th><th>PSRF-RCF</th></tr>\n";
#    }
#    
#    for(my $i=1;$i <= $#var_names; $i++) 
#    {
#	my $var = $var_names[$i];
#
#	next if ($var eq "iter");
#	next if (($var eq "time") && ($personality eq "phylobayes"));
#	next if (($var eq "#treegen") && ($personality eq "phylobayes"));
#
#	my $style = "";
#	$style = 'style="font-style:italic"' if (!defined($CI_low{$var}));
#	$section .= "<tr $style>\n";
#	$section .= "<td>$var</td>\n";
#	$section .= "<td>$median{$var}</td>\n";
#	if (defined($CI_low{$var})) {
#	    $section .= "<td>($CI_low{$var}, $CI_high{$var})</td>\n";
#	    my $style = "";
#	    $style = ' style="color:red"' if ($ESS{$var} <= $min_ESS);
#	    $section .= "<td $style>$ACT{$var}</td>\n";
#	    $style = "";
#	    $style = ' style="color:orange"' if ($ESS{$var} < 300);
#	    $style = ' style="color:red"' if ($ESS{$var} < 100);
#	    $section .= "<td $style>$ESS{$var}</td>\n";
#	    $style = "";
#	    $style = ' style="color:red"' if ($Burnin{$var} eq "Not Converged!");
#	    $section .= "<td $style>$Burnin{$var}</td>\n";
#	    if (defined($PSRF_CI80{$var}))
#	    {
#		my $style = "";
#		$style = ' style = "color:orange"' if ($PSRF_CI80{$var} >= 1.05);
#		$style = ' style = "color:red"' if ($PSRF_CI80{$var} >= 1.2);
#		$section .= "<td $style>$PSRF_CI80{$var}</td>";
#	    }
#	    else
#	    {
#		$section .= "<td>NA</td>";
#	    }
#	    if (defined($PSRF_RCF{$var}))
#	    {
#		my $style = "";
#		$style = ' style = "color:orange"' if ($PSRF_RCF{$var} >= 1.05);
#		$style = ' style = "color:red"' if ($PSRF_RCF{$var} >= 1.2);
#		$section .= "<td $style>$PSRF_RCF{$var}</td>";
#	    }
#	    else
#	    {
#		$section .= "<td>NA</td>";
#	    }
#	    $section .= "<td><a href=\"$i.trace.png\">Trace</a></td>\n" if ($do_trace_plots);
#	}
#	else {
#	    $section .= "<td></td>";
#	    $section .= "<td></td>";
#	    $section .= "<td></td>";
#	    $section .= "<td></td>";
#	    $section .= "<td></td>";
#	    $section .= "<td></td>";
#	}
#	$section .= "</tr>\n";
#    }
#    $section .= "</table>\n";
#
#
#    return $section;
#}
#
#
#sub section_end
#{
#    my $section;
#    $section .= "  </div>\n";
#    $section .= "  </body>\n";
#    $section .= "</html>\n";
#    return $section;
#}
#
#
#sub print_index_html
#{
#
#    my $n_sequences = get_n_sequences();
#
#    open my $index, ">Results/index.html";
#
#    my $title = "MCMC Post-hoc Analysis";
#    $title = $title . ": $n_sequences sequences" if defined($n_sequences);
#    
#    print $index &html_header($title);
#
#    print $index &topbar();
#
#    print $index "<div class=\"content\">\n";
#    print $index "<h1>$title</h1>\n";
#
#    print $index &print_data_and_model();
#
#    my @sne = sort {$a <=> $b} values(%ESS);
#    $min_ESS = $sne[0];
#
#    print $index &section_scalar_variables();
#
#    print $index &section_phylogeny_distribution();
#
#    print $index &section_alignment_distribution();
#
#    print $index &section_ancestral_sequences();
#
#    print $index &section_mixing();
#
#    print $index &section_analysis();
#
#    print $index &print_model_section();
#
#    print $index &section_end();
#
#}
#
#sub Rexec
#{
#    return if (!$have_R);
#
#    my $script = shift;
#
#    my $args = shift;
#
#    exec_show("R --slave --vanilla --args $args < $script");
#}
#
#sub show_help()
#{
#    print "Generate an HTML report summarizing bali-phy runs.\n\n";
#    print "Usage: bp-analyze [OPTIONS] <directory1> [<directory2> ... ]\n\n";
#    print "Options:\n";
#    print "  -h [ --help ]             Print usage information.\n";
#    print "  --skip=NUM                Skip NUM iterations as burnin\n";
#    print "  --subsample=NUM           Keep only ever NUM iterations\n\n";
#}
#
#
#    elsif ($personality eq "beast")
#    {
#	print "Summarizing output files from BEAST:\n";
#	my @tree_files = glob("*.trees");
#	foreach my $tree_file (@tree_files)
#	{
#	    check_file_exists($tree_file);
#	    my $prefix = get_file_prefix($tree_file);
#	    push @parameter_files,check_file_exists("$prefix.log");
#	}
#    }
#    else {
#	print "Error: unrecognized analysis of type '$personality'";
#	exit(1);
#    }
#}
#
#sub draw_trees
#{
#    return if (! $have_draw_tree);
#
#    print " Drawing trees:  ";
#    for my $cvalue (@tree_consensus_values)
#    {
#	my $value = $cvalue*100;
#	
#	my $tree = "c$value";
#
#	# No node lengths???
#	my $filename1 = "Results/$tree.tree";
#	my $filename2 = "Results/$tree.mtree";
#	
#	if ($speed < 2)
#	{
#	    if (-e $filename2 && ! more_recent_than("Results/$tree-mctree.svg",$filename2)) {
#		exec_show("draw-tree Results/$tree.mlengths --out=Results/$tree-mctree --output=svg --draw-clouds=only");
#	    }
#	    if (-e $filename2 && ! more_recent_than("Results/$tree-mctree.pdf",$filename2)) {
#		exec_show("draw-tree Results/$tree.mlengths --out=Results/$tree-mctree --draw-clouds=only");
#	    }
#	}
#	
#	if (! more_recent_than("Results/$tree-tree.pdf",$filename1)) {
#	    exec_show("cd Results ; draw-tree $tree.ltree --layout=equal-daylight"); # --no-shade is too slow!
#	}
#	
#	if (! more_recent_than("Results/$tree-tree.svg",$filename1)) {
#	    exec_show("cd Results ; draw-tree $tree.ltree --layout=equal-daylight --output=svg");
#	}
#	
#	print "$tree ";
#	my $prune_arg = "";
#	$prune_arg = "--prune $prune" if defined($prune);
#	
#    }
#
#    exec_show("cd Results ; draw-tree greedy.tree --layout=equal-daylight") if (-e 'Results/greedy.tree');
#    exec_show("cd Results ; draw-tree greedy.tree --layout=equal-daylight --output=svg ") if (-e 'Results/greedy.tree');
#    print "greedy ";
#
#    exec_show("cd Results ; draw-tree MAP.tree --layout=equal-daylight") if (-e 'Results/MAP.tree');
#    exec_show("cd Results ; draw-tree MAP.tree --layout=equal-daylight --output=svg ") if (-e 'Results/MAP.tree');
#    print "MAP ";
#
#    print " ... done.\n";
#}
#
#sub mixing_diagnostics
#{
## 10. Mixing diagnostics -- block bootstrap
#    print "\nGenerate mixing diagnostics for topologies ... ";
#
#    if (!more_recent_than("Results/partitions","Results/consensus")) {
#	exec_show("pickout --no-header --large pi < Results/consensus > Results/partitions");
#    }
#    if (!more_recent_than("Results/partitions.pred","Results/partitions")) {
#	exec_show("perl -e 'while(<>) {s/\$/\\n/;print;}' < Results/partitions > Results/partitions.pred");
#    }
#
#    if (!more_recent_than_all_of("Results/partitions.bs",[@tree_files])) {
#	exec_show("trees-bootstrap $max_arg @tree_files $skip $subsample_string --pred Results/partitions.pred --LOD-table=Results/LOD-table --pseudocount 1 > Results/partitions.bs");
#    }
#    print "done.\n";
#
#    return if ($#tree_files <= 0);
#
#    if (!more_recent_than_all_of("Results/convergence-PP.pdf",[@tree_files]))
#    {
#	my $script = get_libexec_script("compare-runs.R");
#	die "can't find script $script!" if (!defined($script));
#	Rexec($script,"Results/LOD-table Results/convergence-PP.pdf");
#    }
#    if (!more_recent_than_all_of("Results/convergence1-PP.svg",[@tree_files]) or 
#	!more_recent_than_all_of("Results/convergence2-PP.svg",[@tree_files]))
#    {
#	my $script = get_libexec_script("compare-runs2.R");
#	die "can't find script $script!" if (!defined($script));
#	Rexec($script,"Results/LOD-table Results/convergence1-PP.svg Results/convergence2-PP.svg");
#    }
#}
#
#sub SRQ_plots
#{
#    my @SRQ = ();
## 12. Mixing diagnostics - SRQ plots
#    print "Generate SRQ plot for partitions ... ";
#    if (!more_recent_than_all_of("Results/partitions.SRQ",[@tree_files])) {
#	exec_show("trees-to-SRQ Results/partitions.pred $max_arg $skip $subsample_string --max-points=1000 < $tree_files[0] > Results/partitions.SRQ");
#    }
#    print "done.\n";
#    
#    push @SRQ,"partitions";
#
#    print "Generate SRQ plot for c50 tree ... ";
#    if (!more_recent_than_all_of("Results/c50.SRQ",[@tree_files])) {
#	exec_show("trees-to-SRQ Results/c50.tree $max_arg $subsample_string $skip --max-points=1000 < $tree_files[0] > Results/c50.SRQ");
#    }
#    print "done.\n";
#    
#    push @SRQ,"c50";
#    
#    if ($have_gnuplot) {
#	for my $srq (@SRQ) {
#`gnuplot <<EOF
#set terminal png size 300,300
#set output "Results/$srq.SRQ.png"
#set key right bottom
#set xlabel "Regenerations (fraction)"
#set ylabel "Time (fraction)"
#set title "Scaled Regeneration Quantile (SRQ) plot: $srq"
#plot 'Results/$srq.SRQ' title "$srq" with linespoints lw 1 lt 1, x title "Goal" lw 1 lt 3
#EOF
#`;
#	}
#    }
#    if ($have_gnuplot) {
#	if ($sub_partitions) {
#`gnuplot <<EOF
#set terminal svg
#set output "Results/c-levels.svg"
#set xlabel "Log10 posterior Odds (LOD)"
#set ylabel "Supported Splits"
#set style data lines
#plot [0:][0:] 'Results/c-levels.plot' title 'Full Splits','Results/extended-c-levels.plot' title 'Partial Splits'
#EOF`;
#	}
#	else {
#`gnuplot <<EOF
#set terminal svg
#set output "Results/c-levels.svg"
#set xlabel "Log10 posterior Odds (LOD)"
#set ylabel "Supported Splits"
#plot [0:][0:] 'Results/c-levels.plot' with lines notitle
#EOF`;
#	}
#    }
#    return @SRQ;
#}
#
#sub compute_initial_alignments
#{
#    if ($personality =~ "bali-phy.*") {
#	print "\nComputing initial alignments... ";
#	for(my $i=0;$i<$n_partitions;$i++)
#	{
#	    my $p = $i+1;
#	    my $name = "C1.P$p.initial";
#	    push @alignments,$name;
#	    $alignment_names{$name} = "Initial";
#	    
#	    # These initial alignments should never change!
#	    if (! -s "Results/Work/$name-unordered.fasta") 
#	    {
#		my $dir1 = $subdirectories[0];
#		exec_show("cp $dir1/$name.fasta Results/Work/$name-unordered.fasta");
#	    }
#	}
#	print "done.\n";
#    }
#}
#
#sub compute_muscle_alignment
#{
#    if ($personality =~ "bali-phy.*") {
#	if ($muscle) {
#	    print "\nComputing MUSCLE alignment... ";
#	    
#	    for(my $i=0;$i<$n_partitions;$i++) {
#		my $p = ($i+1);
#		my $name = "P$p-muscle";
#		if (! more_recent_than("Results/Work/$name-unordered.fasta", "Results/Work/P$p-initial-unordered.fasta")) {
#		    exec_show("muscle -in Results/Work/P$p-initial-unordered.fasta -out Results/Work/$name-unordered.fasta -quiet");
#		}
#		push @alignments,$name;
#		$alignment_names{$name} = "MUSCLE";
#		
#	    }
#	    print "done.\n";
#	}
#    }
#}
#
#sub compute_probcons_alignment
#{
## 6.5. Compute ProbCons alignments
#
#    if ($personality =~ "bali-phy.*") {
#	if ($probcons) {
#	    print "\nComputing ProbCons alignment... ";
#	    
#	    for(my $i=0;$i<$n_partitions;$i++) {
#		my $p = ($i+1);
#		my $name = "P$p-probcons";
#		
##    my $alignment_info = get_alignment_info("Results/Work/P$p-initial-unordered.fasta");
#		my $alphabet = $alphabets[$i];
#		
#		if ($alphabet =~ /RNA nucleotides/) {
#		    if (! more_recent_than("Results/Work/$name-unordered.fasta", "Results/Work/P$p-initial-unordered.fasta")) {
#			exec_show("probcons-RNA Results/Work/P$p-initial-unordered.fasta > Results/Work/$name-unordered.fasta 2>/dev/null");
#		    }
#		}
#		elsif (! more_recent_than("Results/Work/$name-unordered.fasta", "Results/Work/P$p-initial-unordered.fasta")) {
#		    exec_show("probcons Results/Work/P$p-initial-unordered.fasta > Results/Work/$name-unordered.fasta 2>/dev/null");
#		}
#		push @alignments,$name;
#		$alignment_names{$name} = "ProbCons";
#		
#	    }
#	    print "done.\n";
#	}
#    }
#}
#
#
#sub get_alignments_for_partition
#{
#    my $i=shift;
#    my @args=();
#    foreach my $a (@partition_samples)
#    {
#	push @args,${$a}[$i];
#    }
#    return @args;
#}
#
#sub compute_wpd_alignments
#{
#    if ($personality =~ "bali-phy.*") {
#	print "\nComputing WPD alignments... ";
#
#	for(my $i=0;$i<$n_partitions;$i++) 
#	{
#	    next if ($imodel_indices[$i] eq "--");
#	
#	    my $p = $i+1;
#	    my @afiles = get_alignments_for_partition($i);
#	    
#	    my $name = "P$p-max";
#	    if (! more_recent_than_all_of("Results/Work/$name-unordered.fasta",[@afiles]))
#	    {
#		my $infiles = join(' ',@afiles);
#		exec_show("cut-range $infiles --skip=$burnin $size_arg | alignment-chop-internal --tree=Results/MAP.tree | alignment-max> Results/Work/$name-unordered.fasta");
#	    }
#	    push @alignments,$name;
#	    $alignment_names{$name} = "Best (WPD)";
#	    push @AU_alignments,$name;
#	}
#	
#	print "done.\n";
#    }
#}
#
#sub compute_ancestral_states
#{
#    if ($personality =~ "bali-phy.*")
#    {
#        print "\nComputing ancestral state reconstructions: ";
#	for(my $i=0;$i<$n_partitions;$i++)
#        {
#            my $p = $i+1;
#
#            my @afiles = get_alignments_for_partition($i);
#            my $bad = 0;
#            for my $afile (@afiles)
#            {
#                $bad++ if (! -e $afile);
#            }
#            if ($bad > 0)
#            {
#                print ".";
#                next;
#            }
#
#            die "wrong number of alignment and tree files for partition $i" if ((scalar @afiles) != (scalar @tree_files));
#
#            my $template = "Results/P${p}-max.fasta";
#            $template = "Results/C1.P${p}.initial.fasta" if ($imodel_indices[$i] eq "--");
#            my $tree = "Results/c66.tree";
#
#            my $cmd = "extract-ancestors -a ${template} -n ${tree} -g ${tree}";
#
#            foreach my $directory (@subdirectories)
#            {
#                $cmd = "${cmd} -A ${directory}/C1.P${p}.fastas -T ${directory}/C1.trees";
#            }
#
#            my $output = "Results/P${p}.ancestors.fasta";
#            if (!more_recent_than_all_of($output,[@tree_files]))
#            {
#                exec_show("${cmd} > ${output}");
#            }
#            print "*";
#        }
#	print " done.\n";
#    }
#}
#
#sub compute_consensus_alignments
#{
#    print "Computing consensus alignments:\n";
#    for(my $i=0;$i<$n_partitions;$i++)
#    {
#	next if ($imodel_indices[$i] eq "--");
#
#	my $p = $i+1;
#	my @afiles = get_alignments_for_partition($i);
#	
#	print " Partition $p: ";
#	for my $cvalue (@alignment_consensus_values) {
#	    my $value = $cvalue*100;
#	    my $name = "P$p-consensus-$value";
#	    print "c$value ";
#	    if (! more_recent_than_all_of("Results/Work/$name-unordered.fasta",[@afiles]))
#	    {
#		my $infiles = join(' ',@afiles);
#		exec_show("cut-range $infiles --skip=$burnin $size_arg | alignment-chop-internal --tree=Results/MAP.tree | alignment-consensus --cutoff=$cvalue> Results/Work/$name-unordered.fasta");
#	    }
#	    push @alignments,$name;
#	    $alignment_names{$name} = "$value% consensus";
#	}
#	print "done.\n\n";
#	push @AU_alignments,"P$p-consensus-10" if ($speed == 0);
#    }
#}
#
#sub get_partition_for_alignment
#{
#    my $alignment = shift;
#
#    my $p;
#    if ($alignment =~ /^C[0-9]+\.P([0-9]+)\./)
#    {
#	$p = $1;
#	$p = $p - 1;
#    }
#
#    if ($alignment =~ /^P([0-9]+)-max/)
#    {
#	$p = $1;
#	$p = $p - 1;
#    }
#    return $p;
#}
#
#sub draw_alignments
#{
#    return if ($#alignments == -1);
#
#    print "Drawing alignments: ";
#    for my $alignment (@alignments) 
#    {
#	if (! more_recent_than("Results/$alignment.fasta","Results/Work/$alignment-unordered.fasta") ||
#	    ! more_recent_than("Results/$alignment.fasta","Results/c50.tree")) {
#	    exec_show("alignment-cat Results/Work/$alignment-unordered.fasta --reorder-by-tree=Results/c50.tree > Results/$alignment.fasta");
#	}
#	
#	my $color_scheme="DNA+contrast";
#	my $p = get_partition_for_alignment($alignment);
#	if (defined($p))
#	{
#	    $color_scheme="AA+contrast" if ($alphabets[$p] eq "Amino-Acids");
#	}
#	else
#	{
#	    print $LOG "WARNING: Don't know partition for alignment '$alignment'\n";
#	}
#	
#	if (! more_recent_than("Results/$alignment.html","Results/$alignment.fasta")) {
#	    exec_show("alignment-draw Results/$alignment.fasta --show-ruler --color-scheme=${color_scheme} > Results/$alignment.html");
#	}
#        print "*";
#    }
#    
#    # Generate  alignments-diff
#    for my $alignment (@alignments) 
#    {
#	my $p = get_partition_for_alignment($alignment);
#
#        if (!defined($p) || $imodel_indices[$p] eq "--")
#        {
#            print ".";
#            next;
#        }
#	
#	# Find alignment to compare to.
#	my $p1 = $p+1;
#	my $compare_name = "P$p1-max";
#	my $compare_fasta = "Results/${compare_name}.fasta";
#
#	next if ($alignment eq $compare_name);
#
#	next if (! -e ${compare_fasta});
#	
#	if (! more_recent_than("Results/$alignment-diff.fasta","Results/$alignment.fasta") || 
#	    ! more_recent_than("Results/$alignment-diff.AU","Results/$alignment.fasta") )
#	{
#	    exec_show("alignments-diff Results/$alignment.fasta ${compare_fasta} > Results/$alignment-diff.AU");
#	}
#	
#	if (! more_recent_than("Results/$alignment-diff.html","Results/$alignment-diff.AU")) {
#	    exec_show("alignment-draw Results/$alignment.fasta --scale=identity --AU Results/$alignment-diff.AU --show-ruler --color-scheme=diff[1]+contrast > Results/$alignment-diff.html");
#	}
#        print "*";
#    }
#    print " done.\n";
#}
#
#sub compute_and_draw_AU_plots
#{
#    for my $alignment (@AU_alignments) 
#    {
#	if ($alignment =~ /^P([^-]+)-.*/) {
#	    print "Generating AU values for $alignment... ";
#	    my $p = $1;
#	    my @afiles = get_alignments_for_partition($p-1);
#	    
#	    if (!more_recent_than_all_of("Results/$alignment-AU.prob",[@afiles])) 
#	    {
#		my $infiles = join(' ',@afiles);
#		exec_show("cut-range $infiles --skip=$burnin $size_arg | alignment-chop-internal --tree=Results/MAP.tree | alignment-gild Results/$alignment.fasta Results/MAP.tree --max-alignments=500 > Results/$alignment-AU.prob");
#	    }
#	    print "done.\n";
#	    my $result = exec_result("alignment-draw Results/$alignment.fasta --show-ruler --AU Results/$alignment-AU.prob --color-scheme=DNA+contrast+fade+fade+fade+fade > Results/$alignment-AU.html 2>/dev/null");
#	    if ($result) {
#		exec_show("alignment-draw Results/$alignment.fasta --show-ruler --AU Results/$alignment-AU.prob --color-scheme=AA+contrast+fade+fade+fade+fade > Results/$alignment-AU.html");
#	    }
#	}
#    }
#}
#
#sub compute_marginal_likelihood
#{
#    my $marginal_prob = "unknown";
#    if ($personality ne "treefile") 
#    {
#	print "Calculating marginal likelihood... ";
#	
#	if (!more_recent_than_all_of("Results/Pmarg",[@parameter_files])) {
#	    my $likelihood = "likelihood";
#	    $likelihood = "loglik" if ($personality eq "phylobayes");
#	    exec_show("stats-select $likelihood --no-header < $parameter_files[0] | tail -n+$burnin | model_P > Results/Pmarg");
#	}
#	print "done.\n";
#	$marginal_prob = `cat Results/Pmarg`;
#    }
#    return $marginal_prob;
#}
#
#
#sub generate_trace_plots
#{
#    
#    if ($#parameter_files > -1) 
#    {
#	open my $VARS, $parameter_files[0];
#	my $header = portable_readline($VARS);
#	while ($header eq "" or substr($header,0,2) eq "# ") {
#	    $header = portable_readline($VARS);
#	}
#	chomp $header;
#	@var_names = split(/\t/,$header);
#	close $VARS;
#	
#	print "Analyzing scalar variables ... ";
#
#	open my $REPORT, "Results/Report";
#	
#	while (my $line = portable_readline($REPORT)) {
#	    chomp $line;
#	    next if ($line eq "");
#	    
#	    if ($line =~ /\s+(.+) ~ (.+)\s+\((.+),(.+)\)/)
#	    {
#		my $var = $1;
#		$median{$var} = $2;
#		$CI_low{$var} = $3;
#		$CI_high{$var} = $4;
#
#		$line = portable_readline($REPORT);
#		$line =~ /t @ (.+)\s+Ne = ([^ ]+)\s+burnin = (Not Converged!|[^ ]+)/;
#		$ACT{$var} = $1;
#		$ESS{$var} = $2;
#		$Burnin{$var} = $3;
#
#		$line = portable_readline($REPORT);
#		if ($line =~ /PSRF-80%CI = ([^ ]+)\s+PSRF-RCF = ([^ ]+)/)
#		{
#		    $PSRF_CI80{$var} = $1;
#		    $PSRF_RCF{$var} = $2;
#		}
#	    }
#	    elsif ($line =~ /\s+(.+) = (.+)/) {
#		my $var = $1;
#		$median{$var} = $2;
#	    }
#	}
#
#	print "done\n";
#
#	return if (!$do_trace_plots);
#	
#	print "Generating trace-plots ... ";
#
#	my $Nmax = 5000;
#	
#	for(my $i=1;$i<= $#var_names; $i++)
#	{
#	    next if (more_recent_than("Results/$i.trace.png",$parameter_files[0]));
#	    
#	    my $var = $var_names[$i];
#	    next if (!defined($CI_low{$var}));
#	    
#	    my $file1 = "Results/Work/T1.p.$i";
#	    if ($personality =~ "bali-phy.*") {
#		exec_show("stats-select iter '$var' --no-header < $parameter_files[0] > $file1");
#	    }
#	    elsif ($personality =~ "phylobayes.*") {
#		exec_show("stats-select time '$var' --no-header < $parameter_files[0] > $file1");
#	    }
#	    elsif ($personality =~ "beast.*")
#	    {
#		exec_show("stats-select state '$var' --no-header < $parameter_files[0] > $file1");
#	    }
#	    
#	    my $file2 = $file1.".2";
#	    
#	    my $N = $n_iterations[0] - $burnin;
#	    
#	    $N = 1000 if ($N > 1000);
#	    
#	    my $factor = ceil(($n_iterations[0] - $burnin) / $N);
#	    
#	    exec_show("subsample --skip=$burnin $factor < $file1 > $file2");
#	
#	    `gnuplot <<EOF
#set terminal png size 800,600
#set output "Results/$i.trace.png"
#set key right bottom
#set xlabel "Iteration"
#set ylabel "$var"
#plot '$file2' title '$var' with lines
#EOF` if ($have_gnuplot);
#	}
#	
#	print "done\n";
#    }
#}
#
#
#sub get_libexec_script
#{
#    my $file = shift;
#    my $prefix = dirname(dirname(abs_path(__FILE__)));
#    my $libexecdir = "$prefix/lib/bali-phy/libexec/";
#    die "Missing libexecdir '$libexecdir'!" if (! -e $libexecdir);
#    my $path = "$libexecdir/$file";
#    die "Missing libexec script '$file'!" if (! -e $path);
#    
#    return $path;
#}
#
#sub find_in_path
#{
#    my $file = shift;
#    my $home = $ENV{'HOME'};
#
#    my @dirs = split(':',$ENV{'PATH'});
#    
#    for my $dir (@dirs) {
#	$dir =~ s/^~/$home/;
#	if (-x "$dir/$file" ) {
#	    return "$dir/$file";
#	}
#    }
#
#    return undef;
#}
#
#sub is_in_path
#{
#    return 1 if (defined(find_in_path(@_)));
#    return 0;
#}
#
#sub check_input_file_names()
#{
#    return 0 if (! -e "Results/input_files");
#
#    open my $FILE,"Results/input_files";
#
#    my @old_input_file_names;
#    while(my $line = portable_readline($FILE))
#    {
#	chomp $line;
#	push @old_input_file_names, $line;
#    }
#    close $FILE;
#
#    return compare_arrays( [@old_input_file_names], [@input_file_names]);
#}
#

#sub write_out_file_names()
#{
#    open my $FILE,">Results/out_files";
#
#    print $FILE join("\n",@out_files);
#
#    close $FILE;
#}
#
#sub check_burnin()
#{
#    return 0 if (! -e "Results/burnin");
#    open my $FILE,"Results/burnin";
#
#    my $line = portable_readline($FILE);
#    chomp $line;
#
#    close $FILE;
#
#    return ($burnin == $line);
#}
#
#sub write_burnin()
#{
#    open my $FILE,">Results/burnin";
#
#    print $FILE "$burnin\n";
#
#    close $ FILE;
#}
#
#sub do_cleanup
#{
#    rmdir_recursive("Results") if (-e "Results");
#}
#
#sub rmdir_recursive 
#{
#    my $dir = shift;
#    local *DIR;
#
#    opendir DIR, $dir or die "opendir $dir: $!";
#    for (readdir DIR) {
#	next if /^\.{1,2}$/;
#	my $path = "$dir/$_";
#	unlink $path if -f $path;
#	rmdir_recursive($path) if -d $path;
#    }
#    closedir DIR;
#    rmdir $dir or print "error - $!";
#}
#
#sub compare_arrays {
#    my ($first, $second) = @_;
#    no warnings;  # silence spurious -w undef complaints
#    return 0 unless @$first == @$second;
#    for (my $i = 0; $i < @$first; $i++) {
#	return 0 if $first->[$i] ne $second->[$i];
#    }
#    return 1;
#}  
#

#sub show_array
#{
#    my $spacer = shift;
#    my $array = shift;
#    foreach my $entry (@$array)
#    {
#	print $spacer.$entry."\n";
#    }
#}
#
#sub show_array_differences
#{
#    my $spacer = shift;
#    my $name1 = shift;
#    my $array1 = shift;
#    my $name2 = shift;
#    my $array2 = shift;
#
#    print "  For $name1:\n";
#    show_array($spacer,$array1);
#    print "  For $name2:\n";
#    show_array($spacer,$array2);
#}
#
#sub get_input_file_names
#{
#    my $input_file_names;
#    for my $out_file (@out_files)
#    {
#	my $these_input_file_names = get_input_file_names_for_outfile($out_file);
#	if (!defined($input_file_names))
#	{
#	    $input_file_names = $these_input_file_names;
#	}
#	else
#	{
#	    if (!compare_arrays($input_file_names,$these_input_file_names))
#	    {
#		print "\nError! Different MCMC chains have different input files!\n";
#		show_array_differences("    file = ",$out_files[0],$input_file_names,$out_file,$these_input_file_names);
#		exit(1);
#	    }
#	}
#    }
#    $input_file_names = [] if (!defined($input_file_names));
#    return $input_file_names;
#}
#
##FIXME - rewrite to take the cmdline as an argument.
#sub get_cmdline_attribute
#{
#    return "unknown" if ($personality !~ "bali-phy.*");
#    my $attribute = shift;
#    my $value;
#
#    open my $FILE, $out_files[0] or die "Can't open $out_files[0]!";
#
#    my @partitions = ();
#
#    my $line = portable_readline($FILE);
#    {
#	if ($line =~ /--$attribute[ =]([^ ]*)$/) {
#	    $value = $1;
#	    last;
#	}
#	last if ($line =~ /^iterations = 0/);
#    }
#    close $FILE;
#
#    return $value;
#}
#
#sub get_header_attributes
#{
#    return ("unknown") if ($personality !~ "bali-phy.*");
#    my $attribute = shift;
#    my @filenames = @_;
#    my @values;
#
#    foreach my $filename (@filenames)
#    {
#
#	open my $FILE, $filename or die "Can't open $filename!";
#
#	while (my $line = portable_readline($FILE))
#	{
#	    if ($line =~ /$attribute: (.*)$/) {
#		push @values, $1;
#		last;
#	    }
#	    last if ($line =~ /^iterations = 0/);
#	}
#	close $FILE;
#    }
#
#    return @values;
#}
#
##Empirical(/home/bredelings/local/share/bali-phy/Data//wag.dat) 
#
#sub get_tree_prior
#{
#    return [] if ($personality !~ "bali-phy.*");
#
#    my $file = $out_files[0];
#
#    open my $FILE, $file or die "Can't open $file!";
#
#    while (my $line = portable_readline($FILE))
#    {
#	if ($line =~ /^T:topology (.*)$/)
#	{
#	    $topology_prior = $1;
#	}
#	if ($line =~ /^T:lengths (.*)$/)
#	{
#	    $branch_prior = $1;
#	}
#	last if ($line =~ /^iterations/);
#    }
#
#    close $FILE;
#}
#
#
#sub get_models_for_run_file
#{
#    return [] if ($personality !~ "bali-phy.*");
#
#    my $name = shift;
#    my $filename = shift;
#
#    my $j = get_json_from_file($filename);
#
#    my @scales = ();
#
#    foreach my $scale (@{${$j}{$name}})
#    {
#	push @scales, make_extracted($scale);
#    }
#
#    return [@scales];
#}
#
#sub arrays_all_equal
#{
#    my @arrays = @_;
#    for(my $i=1;$i <= $#arrays ; $i++) {
#	return $i if (!compare_arrays($arrays[$i], $arrays[0]));
#    }
#    return 0;
#}
#
#
#sub get_models_for_file
#{
#    my $name = shift;
#    my $file = shift;
#
#    return [] if ($personality !~ "bali-phy.*");
#
#    open my $FILE, $file or die "Can't open $file!";
#
#    my @models = ();
#
#    while (my $line = portable_readline($FILE))
#    {
#	if ($line =~ /${name}([0-9]+) = (.+)/)
#	{
#	    push @models,make_extracted($2);
#	}
#	elsif ($line =~ /${name}([0-9]+) ~ (.+)/)
#	{
#	    push @models,make_extracted("~".$2);
#	}
#	else
#	{
#	    last if ($line =~ /^iterations = 0/);
#	}
#    }
#    close $FILE;
#
#    return [@models];
#}
#
#sub get_version_for_file
#{
#    my $file = shift;
#    my $version = get_header_attributes("VERSION",$file);
#    $version = (split /\s+/,$version)[0];
#    return $version;
#}
#
#sub get_version_for_run_file
#{
#    my $file = shift;
#    my $j = get_json_from_file($file);
#    return ${$j}{'program'}{'version'};
#}
#
#sub get_all_versions
#{
#    return [] if ($#out_files == -1);
#    my @versions;
#
#    if (!@run_files)
#    {
#	foreach my $out_file (@out_files)
#	{
#	    push @versions, get_version_for_file($out_file);
#	}
#    }
#    else
#    {
#	foreach my $run_file (@run_files)
#	{
#	    push @versions, get_version_for_run_file($run_file);
#	}
#    }
#    return @versions;
#}
#
#sub get_models
#{
#    my $name1 = shift;
#    my $name2 = shift;
#    return [] if ($#out_files == -1);
#    my @models;
#
#    if (!@run_files)
#    {
#	foreach my $out_file (@out_files)
#	{
#	    push @models, get_models_for_file($name1, $out_file);
#	}
#    }
#    else
#    {
#	foreach my $run_file (@run_files)
#	{
#	    push @models, get_models_for_run_file($name2, $run_file);
#	}
#    }
#    return $models[0];
#}
#
#sub get_smodels
#{
#    return [] if ($#out_files == -1);
#    my @smodels;
#
#
#    if (!@run_files)
#    {
#	foreach my $out_file (@out_files)
#	{
#	    push @smodels, get_smodels_for_file($out_file);
#	}
#    }
#    else
#    {
#	foreach my $run_file (@run_files)
#	{
#	    push @smodels, get_smodels_for_run_file($run_file);
#	}
#    }
#
#    my $different = arrays_all_equal(@smodels);
#
#    return $smodels[0] if (! $different);
#
#    print "\nError! Different MCMC chains have different substitution models!\n";
#    show_array_differences("    subst model = ", $out_files[0], $smodels[0], $out_files[$different], $smodels[$different]);
#    exit(1);
#}
#
#sub get_scale_models
#{
#    return [] if ($#out_files == -1);
#    my @scale_models;
#
#    foreach my $out_file (@out_files)
#    {
#	push @scale_models, get_scale_models_for_file($out_file);
#    }
#
#    my $different = arrays_all_equal(@scale_models);
#
#    return $scale_models[0] if (! $different);
#
#    print "\nError! Different MCMC chains have different scale models!\n";
#    show_array_differences("    subst model = ", $out_files[0], $scale_models[0], $out_files[$different], $scale_models[$different]);
#    exit(1);
#}
#
#sub get_imodels_for_file
#{
#    my $file = shift;
#
#    return [] if ($personality !~ "bali-phy.*");
#
#    open my $FILE, $file or die "Can't open $file!";
#
#    my @imodels = ();
#
#    while (my $line = portable_readline($FILE))
#    {
#	if ($line =~ /indel model(.+) = (.+)/) {
#	    push @imodels,$2;
#	}
#	if ($line =~ /indel model = (.+)/) {
#	    push @imodels,$1;
#	}
#
#	last if ($line =~ /^iterations = 0/);
#    }
#    close $FILE;
#
#    push @imodels, "none" if ($#imodels == -1);
#    return [@imodels];
#}
#
#sub get_imodels
#{
#    return [] if ($#out_files == -1);
#    my @imodels;
#
#    foreach my $out_file (@out_files)
#    {
#	push @imodels, get_imodels_for_file($out_file);
#    }
#
#    my $different = arrays_all_equal(@imodels);
#
#    return $imodels[0] if (! $different);
#
#    print "\nError! Different MCMC chains have different substitution models!\n";
#    show_array_differences("    subst model = ", $out_files[0], $imodels[0], $out_files[$different], $imodels[$different]);
#    exit(1);
#}
#
#sub get_smodel_indices
#{
#    return [] if ($personality !~ "bali-phy.*");
#
#    open my $FILE, $out_files[0] or die "Can't open $out_files[0]!";
#
#    my @smodel_indices = ();
#
#    while (my $line = portable_readline($FILE))
#    {
#	if ($line =~ /smodel-index(.+) = (.+)/) {
#	    push @smodel_indices,$2;
#	}
#	last if ($line =~ /^iterations = 0/);
#    }
#    close $FILE;
#
#    return [@smodel_indices];
#}
#
#sub get_scale_model_indices
#{
#    return [] if ($personality !~ "bali-phy.*");
#
#    open my $FILE, $out_files[0] or die "Can't open $out_files[0]!";
#
#    my @scale_model_indices = ();
#
#    while (my $line = portable_readline($FILE))
#    {
#	if ($line =~ /scale-index(.+) = (.+)/) {
#	    push @scale_model_indices,$2;
#	}
#	last if ($line =~ /^iterations = 0/);
#    }
#    close $FILE;
#
#    return [@scale_model_indices];
#}
#
#sub get_imodel_indices
#{
#    return [] if ($personality !~ "bali-phy.*");
#
#    open my $FILE, $out_files[0] or die "Can't open $out_files[0]!";
#
#    my @imodel_indices = ();
#
#    while (my $line = portable_readline($FILE))
#    {
#	if ($line =~ /imodel-index(.+) = (.+)/) {
#	    push @imodel_indices,$2;
#	}
#	last if ($line =~ /^iterations = 0/);
#    }
#    return [@imodel_indices];
#}
#
#sub get_alphabets
#{
#    return [] if ($personality !~ "bali-phy.*");
#
#    open my $FILE, $out_files[0] or die "Can't open $out_files[0]!";
#
#    my @alphabets = ();
#
#    while (my $line = portable_readline($FILE))
#    {
#	if ($line =~ /alphabet(.+) = (.+)/) {
#	    push @alphabets,$2;
#	}
#	if ($line =~ /alphabet = (.+)/) {
#	    push @alphabets,$1;
#	}
#
#	last if ($line =~ /^iterations = 0/);
#    }
#    close FILE;
#
#    return @alphabets;
#}
#
#sub get_n_lines
#{
#    my $filename = shift;
#
#    open FILE, $filename;
#
#    my $n_lines = 0;
#
#    while (<FILE>) {
#	$n_lines++;
#    }
#
#    return $n_lines;
#}
#
#sub get_n_iterations
#{
#    my @n_iterations;
#    for my $tree_file (@tree_files)
#    {
#	my $n = get_n_lines($tree_file)-1;
#	if ($n <= 0) {
#	    print "Error: Tree file '$tree_file' has no samples!\n";
#	    exit(1);
#	}
#	push @n_iterations,$n;
#    }
#    return @n_iterations;
#}
#
#sub more_recent_than
#{
#    my $filename1 = shift;
#    my $filename2 = shift;
#
#    die "I can't open '$filename2'" if (! -f $filename2);
#    return 0 if (! -f $filename1);
#
#    my $age1 = -M $filename1;
#    my $age2 = -M $filename2;
#
#    return 1 if ($age1 <= $age2);
#    return 0;
#}
#
#sub more_recent_than_all_of
#{
#    my $filename1 = shift;
#    my $temp = shift;
#    my @filenames2 = @$temp;
#
#    foreach my $filename2 (@filenames2) {
#	return 0 if (!more_recent_than($filename1,$filename2));
#    }
#
#    return 1;
#}
#
#sub get_prev_burnin
#{
#    my $prev_burnin;
#    $prev_burnin = `cat Results/burnin` if (-e "Results/burnin");
#    return $prev_burnin;
#}
#
#sub record_burnin
#{
#    open BURN,">Results/burnin";
#    print BURN $burnin;
#    close BURN;
#}
#

#sub tooltip
#{
#    my $text = shift;
#    return "<a title=\"$text\">?</a>";
#}
#
#sub get_consensus_arg
#{
#    my $suffix = shift;
#    my $levels = shift;
#    my @pairs = @$levels;
#    for my $level (@pairs)
#    {
#	my $filename = $level*100;
#	$filename = "Results/c$filename.".$suffix;
#	$level = "$level:$filename";
#    }
#    return join(',',@pairs);
#}
#
#sub get_consensus_trees
#{
#    my $suffix = shift;
#    my $levels = shift;
#    my @filenames = ();
#    for my $level (@$levels)
#    {
#	my $filename = $level*100;
#	$filename = "Results/c$filename.".$suffix;
#	push @filenames,$filename;
#    }
#    return @filenames;
#}
#
#sub get_file_prefix
#{
#    my $filename = shift;
#    if ($filename =~ /(.*)\.([^.\/\\]*)$/)
#    {
#	return $1;
#    }
#    return $filename;
#}
#
#sub get_file_suffix
#{
#    my $filename = shift;
#    if ($filename =~ /(.*)\.([^.\/\\]*)$/)
#    {
#	return $2;
#    }
#    return "";
#}
#
#sub get_mcmc_command_line
#{
#    my $command;
#    if ($personality =~ "bali-phy.*") 
#    {
#	$command = get_header_attribute($out_files[0],"command");
#    }
#    else
#    {
#	$command = "unknown";
#    }
#    return $command;
#}
#
#sub min
#{
#    my $array = shift;
#    my $best = $$array[0];
#    for my $element (@$array)
#    {
#	$best = $element if ($element < $best);
#    }
#    return $best;
#}
#
#sub max
#{
#    my $array = shift;
#    my $best = $$array[0];
#    for my $element (@$array)
#    {
#	$best = $element if ($element > $best);
#    }
#    return $best;
#}
#
#sub arg_max
#{
#    my $array = shift;
#    my $best = $$array[0];
#    my $best_i = 0;
#    for(my $i=1;$i<=$#$array;$i++)
#    {
#	if ($$array[$i] > $best)
#	{
#	    $best = $$array[$i];
#	    $best_i = $i;
#	}
#    }
#    return $best_i;
#}
#
#sub arg_min
#{
#    my $array = shift;
#    my $best = $$array[0];
#    my $best_i = 0;
#    for(my $i=1;$i<=$#$array;$i++)
#    {
#	if ($$array[$i] < $best)
#	{
#	    $best = $$array[$i];
#	    $best_i = $i;
#	}
#    }
#    return $best_i;
#}
#
#sub determine_burnin
#{
#    if (defined($burnin))
#    {
#	for(my $i=0;$i<=$#out_files;$i++)
#	{
#	    if ($burnin > $n_iterations[$i]) 
#            {
#		print "Chain #".($i+1)." with file $out_files[$i] has only $n_iterations[$i] iterations.\n";
#		print "Error!  The the burnin (specified as $burnin) cannot be higher than this.\n";
#		exit(1);
#	    }
#	}
#    }
#    return if (defined($burnin));
#
#    my $max_iterations = max(\@n_iterations);
#    my $max_i = arg_max(\@n_iterations);
#
#    my $min_iterations = min(\@n_iterations);
#    my $min_i = arg_min(\@n_iterations);
#
#    if ($max_iterations > 3*$min_iterations)
#    {
#	print "The variation in run length between MCMC chains is too great.\n";
#	print "  Chain #".($max_i+1)." has $max_iterations iterations.\n";
#	print "  Chain #".($min_i+1)." has $min_iterations iterations.\n";
#	print "Not going to guess a burnin: please specify one.\n";
#	exit(1)
#    }
#
#    $burnin = int 0.1*min(\@n_iterations);
#}
#
#sub get_the_only_subdirectory
#{
#    if ($#subdirectories != 0)
#    {
#	print "Error: There is more than one subdirectory, but other subdirectories are being ignored.";
#	exit(1);
#    }
#    return $subdirectories[0];
#}
#
## 0. Compute T1.p and T1.trees
#sub compute_tree_and_parameter_files_for_heated_chains
#{
#    return if ($n_chains == 1);
#
#    for(my $i=1;$i<=$n_chains;$i++)
#    {
#	# Construct C1.pt
#	# Construct C1Ti.pt
#	# Construct C1Ti.p
#	# Construct C1Ti.trees
#	next if (! -e "C$i.trees" );
#	
#	if (! more_recent_than_all_of("Results/C$i.t",[@tree_files]))
#	{
#	    exec_show("echo 'tree' > Results/C$i.t");
#	    exec_show("cat C$i.trees >> Results/C$i.t");
#	}
#	
#	if (! more_recent_than_all_of("Results/C$i.pt",[@tree_files, @parameter_files]))
#	{
#	    exec_show("stats-merge C$i.p Results/C$i.t > Results/C$i.pt 2>/dev/null");
#	}
#	
#	if (! more_recent_than("Results/C${i}T1.pt","Results/C$i.pt")) 
#	{
#	    my $use_header = "";
#	    $use_header = "--no-header" if ($i != 1);
#	    
#	    exec_show("subsample --header --skip=$burnin < Results/C$i.pt | stats-select -s beta=1 $use_header > Results/C${i}T1.pt");
#	}
#    }
#    
#    my $cmd = "cat ";
#    my $rerun=0;
#    for(my $i=1;$i<=$n_chains;$i++) {
#	if (-e "Results/C${i}T1.pt") {
#	    $cmd = "$cmd Results/C${i}T1.pt ";
#	    $rerun=1 if (! more_recent_than("Results/T1.p","Results/C${i}T1.pt"));
#	}
#    }
#    $cmd = "$cmd > Results/T1.pt";
#    exec_show("$cmd") if ($rerun);
#    
#    if (! more_recent_than("Results/T1.trees","Results/T1.pt")) {
#	exec_show("stats-select tree --no-header < Results/T1.pt > Results/T1.trees");
#    }
#    
#    if (! more_recent_than("Results/T1.p","Results/T1.pt")) {
#	exec_show("stats-select -r tree < Results/T1.pt > Results/T1.p");
#	
##       This messes up the printing of statistics
##	exec_show("stats-select -i -r tree < Results/T1.pt > Results/T1.p");
#	
#    }
#    
#    @tree_files = ( "Results/T1.trees" );
#    @parameter_files = ("Results/T1.p");
#}
#
## Replace /cydrive/LETTER{STUFF} with LETTER:{STUFF}
#sub translate_cygwin
#{
#    my $arg = shift;
#    my $new_arg = $arg;
#    $new_arg =~ s|^/cygdrive/([^/]*)(.*)$|$1:$2|;
#
#    if ($new_arg ne $arg)
#    {
#	print "Translating '$arg' to '$new_arg'\n";
#	$arg = $new_arg;
#    }
#    return $arg;
#}
#
#sub tree_MDS
#{
#    return if (!$have_R);
#
#    print "\nGenerate MDS plots of topology burnin ... ";
#
#    if ($#tree_files+1 == 1)
#    {
#	my $script = get_libexec_script("tree-plot1.R");
#	my $tree_file = $tree_files[0];
#	my $N = 400;
#	my $matfile = "Results/tree1.M";
#	my $outfile = "Results/tree1.svg";
#	if (! more_recent_than($outfile, $tree_file))
#	{
#	    exec_show("trees-distances matrix --max=$N --jitter=0.3 $subsample_string $skip $tree_file > $matfile");
#	    Rexec($script,"$matfile $outfile");
#
#	    my $script3d = get_libexec_script("tree-plot1-3D.R");
#
#	    my $point_string = Rexec($script3d, "$matfile");
#	    &write_x3d_file("Results","tree-3D1-1.points", $point_string);
#	}
#    }
#
#    elsif ($#tree_files+1 == 2)
#    {
#	my $script = get_libexec_script("tree-plot2.R");
#	my $tf1 = $tree_files[0];
#	my $tf2 = $tree_files[1];
#	my $N = 400;
#	my $outfile = "Results/tree-1-2.svg";
#	if (! more_recent_than($outfile, $tf1) || ! more_recent_than($outfile, $tf2))
#	{
#	    my $L1 = min([$N, int((get_n_lines($tf1)-$burnin)/$subsample)]);
#	    my $L2 = min([$N, int((get_n_lines($tf2)-$burnin)/$subsample)]);
#	    my $matfile = "Results/tree-1-2.M";
#	    #	print "L1 = $L1  L2 = $L2\n";
#	    exec_show("trees-distances matrix --max=$N --jitter=0.3 $subsample_string  $skip $tf1 $tf2 > $matfile");
#	    Rexec($script,"$L1 $L2 $matfile $outfile");
#
#	    my $script3d = get_libexec_script("tree-plot2-3D.R");
#	    #	print "3d script is at '${script3d}'\n";
#	    my $point_string = Rexec($script3d, "$L1 $L2 $matfile");
#
#	    &write_x3d_file("Results","tree-1-2.points", $point_string);
#	}
#    }
#
#    elsif ($#tree_files+1 >= 3)
#    {
#	my $script = get_libexec_script("tree-plot3.R");
#	my $tf1 = $tree_files[0];
#	my $tf2 = $tree_files[1];
#	my $tf3 = $tree_files[2];
#	my $N = 400;
#	my $L1 = min([$N, int((get_n_lines($tf1)-$burnin)/$subsample)]);
#	my $L2 = min([$N, int((get_n_lines($tf2)-$burnin)/$subsample)]);
#	my $L3 = min([$N, int((get_n_lines($tf3)-$burnin)/$subsample)]);
#	my $matfile = "Results/tree-1-2-3.M";
#	my $outfile = "Results/tree-1-2-3.svg";
##	print "L1 = $L1  L2 = $L2\n";
#	if (! more_recent_than($outfile, $tf1) || ! more_recent_than($outfile, $tf2) || ! more_recent_than($outfile, $tf3))
#	{
#	    exec_show("trees-distances matrix --max=$N --jitter=0.3 $subsample_string  $skip $tf1 $tf2 $tf3 > $matfile");
#	    Rexec($script,"$L1 $L2 $L3 $matfile $outfile");
#
#	    my $script3d = get_libexec_script("tree-plot3-3D.R");
#	    #	print "3d script is at '${script3d}'\n";
#	    my $point_string = Rexec($script3d, "$L1 $L2 $L3 $matfile");
#
#	    &write_x3d_file("Results","tree-1-2-3.points", $point_string);
#	}
#    }
#
#}
#
#sub write_x3d_file
#{
#    my $dir = shift;
#    my $filename = shift;
#    my $point_string = shift;
#
#    $dir = "$dir/" if ("$dir");
#    
#    open FILE, ">${dir}${filename}.html";
#    print FILE 
#"<html>
#  <head>
#    <title>MDS 3D Plot</title>
#    <script type='text/javascript' src='http://www.x3dom.org/download/x3dom.js'> </script>
#    <link rel='stylesheet' type='text/css' href='http://www.x3dom.org/download/x3dom.css'></link>
#    <style>
#      x3d { border:2px solid darkorange; }
#    </style>
#  </head>
#  <body>
#    <x3d width='1000px' height='1000px'>
#      <scene>";
#    print FILE &gen_x3d_of_mds($point_string);
#    print FILE "
#      </scene>
#    </x3d>
#  </body>
#</html>
#";
#    close FILE;
#}
#
#sub hsv_to_rgb
#{
#    my $H = shift;
#    my $S = shift;
#    my $V = shift;
#
#    # decompose color range [0,6) into a discrete color (i) and fraction (f)
#    my $h = ($H * 6);
#    my $i = int($h);
#    my $f = $h - $i;
#    $i = $i % 6;
#
#    my $p = $V*(1.0 - $S);
#    my $q = $V*(1.0 - ($S*$f));
#    my $t = $V*(1.0 - ($S*(1.0-$f)));
#
#    if ($i==0) {
#	return ($V,$t,$p);
#    }
#    elsif ($i==1) {
#	return ($q,$V,$p);
#    }
#    elsif ($i==2) {
#	return ($p,$V,$t);
#    }
#    elsif ($i==3) {
#	return ($p,$q,$V);
#    }
#    elsif ($i==4) {
#	return ($t,$p,$V);
#    }
#    elsif ($i==5) {
#	return ($V,$p,$q);
#    }
#}
#
#sub rgb_to_color
#{
#    my $r = shift;
#    my $g = shift;
#    my $b = shift;
#    return "$r $g $b";
#}
#
#sub interpolate
#{
#    my $i = shift;
#    my $total = shift;
#    my $from = shift;
#    my $to = shift;
#    return $from + ($to-$from)*($i*1.0/($total-1));
#}
#
#sub gen_x3d_of_mds
#{
#    my $point_string = shift;
#
#    my @points;
#    my @x = ();
#    my @y = ();
#    my @z = ();
#
#    my %n = ();
#
#    foreach my $line (split "\n", $point_string)
#    {
#	chomp $line;
#	my $point = [split "\t", $line];
#	push @x, ${$point}[0];
#	push @y, ${$point}[1];
#	push @z, ${$point}[2];
#	my $g = ${$point}[3];
#	$g = 1 if (!defined($g));
#	$n{$g} = 0 if (!defined($n{$g}));
#	$n{$g}++;
#	push @points, $point;
#    }
#
#    my $xmin = min(\@x);
#    my $xmax = max(\@x);
#    my $xw = abs($xmax - $xmin);
#
#    my $ymin = min(\@y);
#    my $ymax = max(\@y);
#    my $yw = abs($ymax - $ymin);
#
#    my $zmin = min(\@z);
#    my $zmax = max(\@z);
#    my $zw = abs($zmax - $zmin);
#    
#    my $x3d = "";
#    my %seen = ();
#    foreach my $point (@points)
#    {
#	my $x = ${$point}[0];
#	$x = ($x-$xmin)/$xw*5.0 - 2.5;
#	my $y = ${$point}[1];
#	$y = ($y-$ymin)/$yw*5.0 - 2.5;
#	my $z = ${$point}[2];
#	$z = ($z-$zmin)/$zw*5.0 - 2.5;
#	my $g = ${$point}[3];
#	$g = 1 if (!defined($g));
#	$seen{$g} = 0 if (!defined($seen{$g}));
#	my $color;
#	my $size = 0.04;
#	my $scale = "$size $size $size";
#	$color = "1 0 0";
#
#	$color = rgb_to_color(hsv_to_rgb(interpolate($seen{$g}, $n{$g}, 0.833333, 1.0),
##	$color = rgb_to_color(hsv_to_rgb(interpolate($seen{$g}, $n{$g}, 0.0,      0.0),
#					 interpolate($seen{$g}, $n{$g}, 0.3,      1.0),
#					 1)
#	                      ) if (defined($g) && $g == 1);
#
#	$color = rgb_to_color(hsv_to_rgb(interpolate($seen{$g}, $n{$g}, 0.5,      0.666666),
##	$color = rgb_to_color(hsv_to_rgb(interpolate($seen{$g}, $n{$g}, 0.666666, 0.666666),
#					 interpolate($seen{$g}, $n{$g}, 0.3,      1.0),
#					 1)
#	                      ) if (defined($g) && $g == 2);
#
#	$color = rgb_to_color(hsv_to_rgb(interpolate($seen{$g}, $n{$g}, 0.166666, 0.333333),
#					 interpolate($seen{$g}, $n{$g}, 0.3,      1.0),
#					 1)
#	    ) if (defined($g) && $g == 3);
#
#	$x3d .= "<transform translation='$x $y $z' scale='$scale'><shape><appearance><material diffuseColor='$color'></material></appearance><sphere></sphere></shape></transform>";
#	$x3d .= "\n";
#	$seen{$g}++;
#    }
#
#    ## FIXME - Chrome can't <include /> local files because it treats them as cross-site scripting, which they are not, I think.
##    $x3d = "<x3d><scene>\n$x3d\n</scene></x3d>\n";
#    return $x3d;
#}
#
#sub portable_readline
#{
#    my $fh = shift;
#    my $line = readline($fh);
#    return $line if (!defined($line));
#    $line =~ s/\015?\012/\n/g;
#    return $line;
#}
