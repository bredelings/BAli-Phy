#!/usr/bin/env python3

import shutil
import argparse
import re
import os
from os import path
import subprocess
import json
import itertools
import glob
import sys

def flatten(l):
    return [item for sublist in l for item in sublist]

def file_is_empty(filename):
    return path.getsize(filename) == 0

def hsv_to_rgb(H,S,V):
    # decompose color range [0,6) into a discrete color (i) and fraction (f)
    h = (H * 6);
    i = int(h);
    f = h - i;
    i = i % 6;

    p = V*(1.0 - S);
    q = V*(1.0 - (S*f));
    t = V*(1.0 - (S*(1.0-f)));

    if i==0:
        return (V,t,p)
    elif i==1:
        return (q,V,p)
    elif i==2:
        return (p,V,t)
    elif i==3:
        return (p,q,V)
    elif i==4:
        return (t,p,V)
    elif i==5:
        return (V,p,q)

def rgb_to_color(rgb):
    (r,g,b) = rgb
    return "{} {} {}".format(r,g,b)

def interpolate(i,total,start,end):
    return start + (end-start)*(i/(total-1))

def get_x3d_of_mds(point_string):
    points = []
    x = []
    y = []
    z = []
    num = dict()

    for line in point_string.strip().split('\n'):
        line.strip()
        point = line.split('\t')
        x.append(float(point[0]))
        y.append(float(point[1]))
        z.append(float(point[2]))
        group = 1
        if len(point) > 3:
            group = int(point[3])
        if group not in num:
            num[group]=0
        num[group] += 1
        points.append((float(point[0]),float(point[1]),float(point[2]),group))

    xmin = min(x)
    xmax = max(x)
    xw = abs(xmax - xmin)

    ymin = min(y)
    ymax = max(y)
    yw = abs(ymax - ymin)

    zmin = min(z)
    zmax = max(z)
    zw = abs(zmax - zmin)

    x3d = ""
    seen = dict()
    for group in num.keys():
        seen[group] = 0

    for (x,y,z,group) in points:
        xx = (x - xmin)/xw*5.0 - 2.5
        yy = (y - ymin)/yw*5.0 - 2.5
        zz = (z - zmin)/zw*5.0 - 2.5

        size = 0.04
        scale = "{s} {s} {s}".format(s=size)

        if group == 1:
            color = rgb_to_color(hsv_to_rgb(interpolate(seen[group], num[group], 0.833333, 1.0),
                                            interpolate(seen[group], num[group], 0.3,      1.0),
                                            1)
                                )
        elif group == 2:
            color = rgb_to_color(hsv_to_rgb(interpolate(seen[group], num[group], 0.5     , 0.666666),
                                            interpolate(seen[group], num[group], 0.3,      1.0),
                                            1)
                                )
        elif group == 3:
            color = rgb_to_color(hsv_to_rgb(interpolate(seen[group], num[group], 0.166666, 0.333333),
                                            interpolate(seen[group], num[group], 0.3,      1.0),
                                            1)
                                )
        else:
            color = "1 0 0"
        obj = "<transform translation='{x} {y} {z}' scale='{scale}'><shape><appearance><material diffuseColor='{color}'></material></appearance><sphere></sphere></shape></transform>"
        x3d += obj.format(x=xx,
                          y=yy,
                          z=zz,
                          scale=scale,
                          color=color)
        x3d += "\n"
        seen[group] += 1

    return x3d

def more_recent_than(f1,f2):
    if not path.exists(f1):
        return False
    return path.getmtime(f1) > path.getmtime(f2)

def more_recent_than_all_of(f1,f2s):
    for f2 in f2s:
        if not more_recent_than(f1,f2):
            return False;
    return True

def find_exe(name,message=None):
    exe = shutil.which(name)
    if exe is None:
        if message is not None:
            print("Program '{}' not found: {}".format(name,message))
        else:
            print("Program '{}' not found.".format(name))
    return exe

def get_n_lines(filename):
    count = 0
    with open(filename) as ifile:
        for line in ifile:
            count += 1
    return count

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

def arg_max(xs):
    arg = None
    val = None
    for i in range(len(xs)):
        x = xs[i]
        if val is None or x > val:
            arg = i
            val = x
    return arg

def arg_min(xs):
    arg = None
    val = None
    for i in range(len(xs)):
        x = xs[i]
        if val is None or x < val:
            arg = i
            val = x
    return arg

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

    def get_alphabets(self):
        return None

    def n_iterations(self):
        return None

    def compute_initial_alignments(self):
        return []

    def get_smodel_indices(self):
        return None

    def get_imodel_indices(self):
        return None

    def get_scale_model_indices(self):
        return None

class BAliPhyRun(MCMCRun):

    def __init__(self,mcmc_output):
        super().__init__(mcmc_output)
        self.prefix = 'C1'
        self.out_file = check_file_exists(path.join(self.get_dir(),'C1.out'))
        self.input_files = self.find_input_file_names_for_outfile(self.out_file)
        self.trees_file = check_file_exists(path.join(self.get_dir(),'C1.trees'))
        self.alignments_files = self.get_alignment_files()
        self.MAP_file = check_file_exists(path.join(self.get_dir(),'C1.MAP'))
        self.cmd = self.find_header_attribute("command")
        self.find_tree_prior()
        self.smodel_indices = self.find_smodel_indices()
        self.imodel_indices = self.find_imodel_indices()
        self.scale_model_indices = self.find_scale_model_indices()
        self.alphabets = self.find_alphabets()

    def get_alphabets(self):
        return self.alphabets

    def get_smodels(self):
        return self.smodels

    def get_smodel_indices(self):
        return self.smodel_indices

    def get_imodels(self):
        return self.imodels

    def get_imodel_indices(self):
        return self.imodel_indices

    def get_scale_models(self):
        return self.scale_models

    def get_scale_imodel_indices(self):
        return self.scale_model_indices

    def get_n_sequences(self):
        features = get_alignment_info("Results/P1.initial.fasta")
        return features["n_sequences"]

    def find_input_file_names_for_outfile(self,outfile):
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

    def find_header_attribute(self, attribute):
        with open(self.out_file,'r',encoding='utf-8') as file:
            reg = attribute + ': (.*)$'
            for line in file:
                m = re.match(reg,line)
                if m:
                    return m.group(1)
        return None

    def n_partitions(self):
        return len(self.get_input_files())

    def find_tree_prior(self):
        with open(self.out_file, encoding='utf-8') as file:
            for line in file:
                m = re.match('^T:topology (.*)$', line)
                if m:
                    self.topology_prior = m.group(1)
                m = re.match('^T:lengths (.*)$', line)
                if m:
                    self.branch_prior = m.group(1)
                if line.startswith("iterations"):
                    break

    def find_smodel_indices(self):
        smodel_indices = []
        with open(self.out_file,encoding='utf-8') as file:
            for line in file:
                m = re.match("smodel-index(.+) = (.+)", line)
                if m:
                    smodel_indices.append(int(m.group(2)))
                if line.startswith("iterations"):
                    break
        return smodel_indices

    def find_imodel_indices(self):
        imodel_indices = []
        with open(self.out_file,encoding='utf-8') as file:
            for line in file:
                m = re.match("imodel-index(.+) = (.+)", line)
                if m:
                    index = m.group(2)
                    if index == "--":
                        index = None
                    else:
                        index = int(index)
                    imodel_indices.append(index)
                if line.startswith("iterations"):
                    break
        return imodel_indices

    def find_scale_model_indices(self):
        scale_model_indices = []
        with open(self.out_file,encoding='utf-8') as file:
            for line in file:
                m = re.match("scale-index(.+) = (.+)", line)
                if m:
                    scale_model_indices.append(int(m.group(2)))
                if line.startswith("iterations"):
                    break
        return scale_model_indices

    def find_alphabets(self):
        alphabets = []
        with open(self.out_file,encoding='utf-8') as file:
            for line in file:
                m = re.match("alphabet.* = (.+)", line)
                if m:
                    alphabets.append(m.group(1))
                if line.startswith("iterations"):
                    break
        return alphabets

    def n_iterations(self):
        n = get_n_lines(self.get_trees_file())-1
        if n <= 0:
            print("Error: Tree file '{}' has no samples!".format(self.get_tree_file()))
            exit(1)
        return n

    def compute_initial_alignments(self):
        print("Computing initial alignments: ",end='',flush=True)
        alignment_names = []
        for i in range(self.n_partitions()):
            name="P{}.initial".format(i+1)
            source = path.join(self.dir,"C1."+name+".fasta")
            dest=path.join("Results","Work",name+"-unordered.fasta")
            if not more_recent_than(dest,source):
                shutil.copyfile(source,dest)
            alignment_names.append(name)
        print(" done.")
        return alignment_names

class BAliPhy2_1Run(BAliPhyRun):
    def __init__(self,mcmc_output):
        super().__init__(mcmc_output)
        self.log_file = check_file_exists(path.join(self.get_dir(),'C1.p'))
        self.version = self.get_header_attributes("VERSION").split('\s+')

        self.smodels = self.get_models("subst models", "smodels")
        self.imodels = self.get_models("indel models", "imodels")
        self.scale_models = self.get_models("scale model", "scales")

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
        self.run_json = get_json_from_file(self.run_file)
        self.version = self.run_json["program"]["version"]
        self.smodels = self.get_models("subst models", "smodels")
        self.imodels = self.get_models("indel models", "imodels")
        self.scale_models = self.get_models("scale model", "scales")

    def get_models(self, name1, name2):
        return [make_extracted(model) for model in self.run_json[name2]]

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

    print("Error: MCMC run '{}' is not a BAli-Phy, BEAST, or PhyloBayes run.".format(mcmc_output),file=sys.stderr)
    exit(3)
        


class Analysis(object):

    def __init__(self,args,mcmc_outputs):
        self.mcmc_runs = [ConstructRun(mcmc_run) for mcmc_run in mcmc_outputs]

        self.trees_consensus_exe = find_exe('trees-consensus', message="See the main for adding the bali-phy programs to your PATH.")
        if self.trees_consensus_exe is None:
            exit(1)

        self.draw_tree_exe = find_exe('draw-tree', message="Tree pictures will not be generated.\n")
        # FIXME - maybe switch from gnuplot to R?
        self.gnuplot_exe = find_exe('gnuplot', message='Some graphs will not be generated.\n')
        self.R_exe = find_exe('R', message='Some mixing graphs will not be generated.\n')

        self.subpartitions = args.subpartitions
        self.subsample = args.subsample
        self.prune = args.prune
        self.burnin = args.skip
        self.until = args.until
        self.verbose = args.verbose
        self.speed = 1

        # map from alignment name to alphabet name
        self.alignments = dict()

        prefix=path.dirname(path.dirname(self.trees_consensus_exe))
        self.libexecdir = path.join(prefix,"lib","bali-phy","libexec")
        if not path.exists(self.libexecdir):
            print("Can't find bali-phy libexec path '{}'".format(self.libexecdir))

        for i in range(len(self.mcmc_runs)):
            if self.mcmc_runs[i].get_cmd() != self.mcmc_runs[0].get_cmd():
                print("WARNING: Commands differ!\n   {}\n   {}\n".format(self.mcmc_runs[0].get_cmd(),
                                                                         self.mcmc_runs[i].get_cmd()))
        self.get_input_files()

        self.tree_consensus_levels = [0.5,0.66,0.8,0.9,0.95,0.99,1.0]
        self.alignment_consensus_values = [0.1,0.25,0.5,0.75]

        self.determine_burnin()
        self.initialize_results_directory()

        self.log_shell_cmds = open("Results/commands.log",'a+',encoding='utf-8')
        print("-----------------------------------------------------------",file=self.log_shell_cmds)

        self.summarize_numerical_parameters()
        self.summarize_topology_distribution()
        self.compute_mean_branch_lengths()
        self.draw_trees()
        self.compute_tree_mixing_diagnostics()
        self.compute_srq_plots()
        self.compute_tree_MDS()
        self.compute_initial_alignments()
        self.compute_wpd_alignments()
        self.compute_ancestral_states()
        self.draw_alignments()
        self.compute_and_draw_AU_plots()
        self.print_index_html()

    def n_chains(self):
        return(len(self.mcmc_runs))

    def run(self,p):
        return self.mcmc_runs[p]

    def get_libexec_script(self,file):
        filepath = path.join(self.libexecdir,file)
        if not path.exists(filepath):
            print("Can't find script '{}'!".format(file))
        return filepath

    def Rexec(self,script,args=[]):
        if not self.R_exe:
            return
        return self.exec_show([self.R_exe,'--slave','--vanilla','--args']+args,infile=script)

    def run_gnuplot(self,script):
        if not self.gnuplot_exe:
            return
        subprocess.Popen(self.gnuplot_exe,stdin=subprocess.PIPE).communicate(script.encode('utf-8'))

    def get_input_files(self):
        return first_all_same([run.get_input_files() for run in self.mcmc_runs])

    def n_partitions(self):
        return first_all_same([run.n_partitions() for run in self.mcmc_runs])

    def get_imodel_indices(self):
        return first_all_same([run.get_imodel_indices() for run in self.mcmc_runs])

    def get_models(self, name1, name2):
        return first_all_same([run.get_models(name1,name2) for run in self.mcmc_runs])

    def get_alphabets(self):
        return first_all_same([run.get_alphabets() for run in self.mcmc_runs])

    def get_alignments_files(self):
        return [run.get_alignments_files() for run in self.mcmc_runs]

    def get_log_files(self):
        return [run.get_log_file() for run in self.mcmc_runs]

    def get_trees_files(self):
        return [run.get_trees_file() for run in self.mcmc_runs]

    def get_alignments_for_partition(self,p):
        return [run.get_alignments_files()[p] for run in self.mcmc_runs]

    def exec_show_result(self,cmd,**kwargs):
        showcmd = ' '.join(["'{}'".format(word) for word in cmd])

        subargs = dict()
        if "infile" in kwargs:
            infile = kwargs["infile"]
            subargs["stdin"] = open(infile,encoding='utf-8')
            showcmd += " < '{}'".format(infile)
        elif "stdin" in kwargs:
            subargs["stdin"] = kwargs["stdin"]

        if "outfile" in kwargs:
            outfile = kwargs["outfile"]
            subargs["stdout"] = open(outfile,'w+',encoding='utf-8')
            showcmd += " > '{}'".format(outfile)
        elif "stdout" in kwargs:
            subargs["stdout"] = kwargs["stdout"]
        else:
            subargs["stdout"] = subprocess.PIPE

        if "stderr" in kwargs:
            subargs["stderr"] = kwargs["stderr"]
        else:
            subargs["stderr"] = subprocess.PIPE

        if "cwd" in kwargs:
            subargs["cwd"] = kwargs["cwd"]

        print(showcmd,file=self.log_shell_cmds)
        result = subprocess.run(cmd,**subargs)
        if result.returncode != 0:
            print("command: {}".format(showcmd),file=sys.stderr)
            if "outfile" in kwargs and path.exists(kwargs["outfile"]):
                os.remove(kwargs["outfile"])
            if "handler" in kwargs:
                handler = kwargs["handler"]
                handler(result.returncode)
        elif self.verbose:
            print("\n\t{}\n".format(showcmd))
        return result

    def exec_show(self,cmd,**kwargs):
        result = self.exec_show_result(cmd,**kwargs)

        out_message = None
        if "stdout" not in kwargs and "outfile" not in kwargs:
            out_message = result.stdout.decode('utf-8')

        err_message = None
        if "stderr" not in kwargs:
            err_message = result.stderr.decode('utf-8')

        # Always record error messages in the log file.
        if err_message:
            print("  err: {}".format(err_message),file=self.log_shell_cmds)
        if self.verbose and out_message:
            print("  out: {}".format(out_message),file=self.log_shell_cmds)

        code = result.returncode
        if code != 0:
            print(" exit: {}".format(code),file=sys.stderr)

            print(" exit: {}".format(code),file=self.log_shell_cmds)
            if out_message:
                print("  out: {}".format(out_message),file=self.log_shell_cmds)
                print("  out: {}".format(out_message),file=sys.stdout)
            if err_message:
                print("  err: {}".format(err_message),file=self.log_shell_cmds)

            exit(code)
        return out_message

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
        return key in properties and value == properties[key]

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
        reuse = reuse and self.check_analysis_property("subsample", self.subsample)
        reuse = reuse and self.check_analysis_property("until", self.until)
        reuse = reuse and self.check_analysis_property("prune", self.prune)
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
        self.write_analysis_property("subsample", self.subsample)
        self.write_analysis_property("until", self.until)
        self.write_analysis_property("prune", self.prune)
        self.write_analysis_property("input_files", self.get_input_files())
        self.write_analysis_property("alignment_file_names", self.get_alignments_files())

    def determine_burnin(self):
        if self.burnin is not None:
            for run in self.mcmc_runs:
                if self.burnin > run.n_iterations():
                    print("MCMC run {} has only {} iterations.".format(run.mcmc_output, run.n_iterations()))
                    print("Error!  The burnin (specified as {}) cannot be higher than this.".format(self.burnin))
                    exit(1)
            return

        iterations = [run.n_iterations() for run in self.mcmc_runs]

        max_iterations = max(iterations)
        max_i = arg_max(iterations)

        min_iterations = min(iterations)
        min_i = arg_min(iterations)

        if max_iterations > 3*min_iterations:
            print("The variation in run length between MCMC chains is too great.\n")
            print("  Chain #{} has {} iterations.".format(max_iterations))
            print("  Chain #{} has {} iterations.".format(min_iterations))
            print("Not going to guess a burnin: please specify one.")
            exit(1)

        self.burnin = int(0.1*min_iterations)

    def summarize_numerical_parameters(self):
        print("Summarizing distribution of numerical parameters: ",end='')
        if not more_recent_than_all_of("Results/Report",self.get_log_files()):
            cmd = ["statreport"] + self.get_log_files()
            if self.subsample is not None and self.subsample != 1:
                cmd.append("--subsample={}".format(self.subsample))
            if self.burnin is not None:
                cmd.append("--skip={}".format(self.burnin))
            if self.until is not None:
                cmd.append("--until={}".format(self.until))
            self.exec_show(cmd,outfile="Results/Report")
        print("done.")

    def summarize_topology_distribution(self):
        print("\nSummarizing topology distribution: ",end='')
        cmd = ['trees-consensus','--map-tree=Results/MAP.PP.tree','--greedy-consensus=Results/greedy.PP.tree','--report=Results/consensus']+self.get_trees_files()
        cmd.append("--support-levels=Results/c-levels.plot")
        if self.subpartitions:
            cmd.append("--sub-partitions")
            cmd.append("--extended-support-levels=Results/extended-c-levels.plot")
        if self.prune is not None:
            cmd.append("--ignore={}".format(self.prune))
        if self.subsample is not None and self.subsample != 1:
            cmd.append("--subsample={}".format(self.subsample))
        if self.burnin is not None:
            cmd.append("--skip={}".format(self.burnin))
        if self.until is not None:
            cmd.append("--until={}".format(self.until))

        tree_names=['Results/greedy.PP.tree','Results/MAP.PP.tree']
        consensus_trees=[]
        for level in self.tree_consensus_levels:
            filename = "Results/c{}.PP.tree".format(int(level*100))
            tree_names.append(filename)
            consensus_trees.append("{}:{}".format(level,filename))
        cmd.append("--consensus={}".format(','.join(consensus_trees)))

        extended_consensus_trees=[]
        for level in self.tree_consensus_levels:
            filename = "Results/c{}.mtree".format(int(level*100))
            extended_consensus_trees.append("{}:{}".format(level,filename))
        if self.subpartitions:
            cmd.append("--extended-consensus={}".format(','.join(extended_consensus_trees)))

        extended_consensus_L=[]
        for level in self.tree_consensus_levels:
            filename = "Results/c{}.mlengths".format(int(level*100))
            extended_consensus_L.append("{}:{}".format(level,filename))
        if self.subpartitions:
            cmd.append("--extended-consensus-L={}".format(','.join(extended_consensus_L)))

        if not more_recent_than_all_of("Results/consensus", self.get_trees_files()):
            self.exec_show(cmd)
        for tree in tree_names:
            if not path.exists(tree) or file_is_empty(tree):
                raise Exception("Tree '{}' not found!".format(tree))
            assert(tree.endswith('.PP.tree'))
            tree2 = tree[0:-8]+'.tree'
            if not more_recent_than(tree2,tree):
                self.exec_show(['tree-tool',tree,'--strip-internal-names','--name-all-nodes'],outfile=tree2)
        print(" done.")

    # This routine computes mean branch lengths WITH NODE CIRCLES (*.ltree)
    def compute_mean_branch_lengths(self):
        print("\nComputing mean branch lengths: ",end='')
        for level in self.tree_consensus_levels:
            value = int(level*100)
            prefix = "Results/c{}".format(value)
            tree = prefix+".tree"

            outfile=prefix+".ltree"
            if more_recent_than_all_of(outfile, self.get_trees_files()):
                break

            cmd = ['tree-mean-lengths','--tree',tree,'--safe','--show-node-lengths']
            cmd += self.get_trees_files()

            if self.prune is not None:
                cmd.append("--ignore={}".format(self.prune))
            if self.subsample is not None and self.subsample != 1:
                cmd.append("--subsample={}".format(self.subsample))
            if self.burnin is not None:
                cmd.append("--skip={}".format(self.burnin))
            if self.until is not None:
                cmd.append("--until={}".format(self.until))

            self.exec_show(cmd,outfile=outfile)
        print(" done.")

    def draw_trees(self):
        self.trees = []
        self.tree_name = dict()
        for level in self.tree_consensus_levels:
            value = int(level*100)
            tree = "c{}".format(value)
            self.trees.append(tree)
            self.tree_name[tree] = "{}% consensus".format(value)
        self.trees.append("MAP")
        self.tree_name["MAP"] = "MAP"
        self.trees.append("greedy")
        self.tree_name["greedy"] = "greedy"
        if not self.draw_tree_exe:
            return
        print("Drawing trees: ",end='')
        for level in self.tree_consensus_levels:
            value = int(level*100)
            tree = "c{}".format(value)

            filename1 = "Results/{}.tree".format(tree)
            filename2 = "Results/{}.mtree".format(tree)

            if self.speed < 2 and path.exists(filename2):
                cmd = ['draw-tree', 'Results/{}.mlengths'.format(tree), '--out=Results/{}-mctree'.format(tree), '--draw-clouds=only']
                if not more_recent_than("Results/{}-mctree.svg".format(tree),filename2):
                    self.exec_show(cmd + ['--output=svg'])
                if not more_recent_than("Results/{}-mctree.pdf".format(tree),filename2):
                    self.exec_show(cmd + ['--output=pdf'])

            if not more_recent_than("Results/{}-tree.pdf".format(tree), filename1):
                cmd = ['draw-tree',tree+".ltree",'--layout=equal-daylight']
                self.exec_show(cmd,cwd="Results")
            if not more_recent_than("Results/{}-tree.svg".format(tree), filename1):
                cmd = ['draw-tree',tree+".ltree",'--layout=equal-daylight','--output=svg']
                self.exec_show(cmd,cwd="Results")
            print(tree+' ',end='',flush=True)

        for tree in ['greedy','MAP']:
            filename = "Results/{}.tree".format(tree)
            if path.exists(filename):
                outname = "Results/{}-tree".format(tree)
                if not more_recent_than(outname+".pdf", filename):
                    self.exec_show(['draw-tree', tree+'.tree','--layout=equal-daylight'],cwd="Results")
                if not more_recent_than(outname+".svg", filename):
                    self.exec_show(['draw-tree', tree+'.tree','--layout=equal-daylight','--output=svg'],cwd="Results")
            print('{} '.format(tree), end='')

        print(". done.")

    def compute_tree_mixing_diagnostics(self):
        print("\nGenerate mixing diagnostics for topologies ...",end='')

        if not more_recent_than("Results/partitions","Results/consensus"):
            self.exec_show(['pickout','--no-header','--large','pi'],
                           infile="Results/consensus",
                           outfile="Results/partitions")

        # This just adds blank lines between the partitions.
        if not more_recent_than("Results/partitions.pred","Results/partitions"):
            with open("Results/partitions",encoding='utf-8') as infile:
                with open("Results/partitions.pred","w+",encoding='utf-8') as outfile:
                    for line in infile:
                        print(line,file=outfile)

        if not more_recent_than_all_of("Results/partitions.bs",self.get_trees_files()):
            cmd = ['trees-bootstrap',
                   '--pred=Results/partitions.pred',
                   '--LOD-table=Results/LOD-table',
                   '--pseudocount=1']
            cmd += self.get_trees_files()
            if self.prune is not None:
                cmd.append("--ignore={}".format(self.prune))
            if self.subsample is not None and self.subsample != 1:
                cmd.append("--subsample={}".format(self.subsample))
            if self.burnin is not None:
                cmd.append("--skip={}".format(self.burnin))
            if self.until is not None:
                cmd.append("--until={}".format(self.until))
            self.exec_show(cmd,outfile="Results/partitions.bs")

        if self.n_chains() < 2:
            print(" done.")
            return

        if not more_recent_than_all_of("Results/convergence-PP.pdf",self.get_trees_files()):
            script = self.get_libexec_script("compare-runs.R")
            self.Rexec(script,["Results/LOD-table","Results/convergence-PP.pdf"])

        if (not more_recent_than_all_of("Results/convergence1-PP.svg",self.get_trees_files()) or
            not more_recent_than_all_of("Results/convergence2-PP.svg",self.get_trees_files())):
            script = self.get_libexec_script("compare-runs2.R")
            self.Rexec(script,["Results/LOD-table","Results/convergence1-PP.svg","Results/convergence2-PP.svg"])
        print(" done.")


    def compute_srq_plots(self):
        self.srq = []
        print("Generate SRQ plot for partitions: ",end='')

        if not more_recent_than_all_of("Results/partitions.SRQ",self.get_trees_files()):
            cmd = ['trees-to-SRQ','Results/partitions.pred','--max-points=1000']
            if self.subsample is not None and self.subsample != 1:
                cmd.append("--subsample={}".format(self.subsample))
            if self.burnin is not None:
                cmd.append("--skip={}".format(self.burnin))
            if self.until is not None:
                cmd.append("--until={}".format(self.until))
            cmd += self.get_trees_files()
            self.exec_show(cmd,outfile="Results/partitions.SRQ")
        print("done.");
        self.srq.append("partitions")

        print("Generate SRQ plot for c50 tree: ", end='')
        if not more_recent_than_all_of("Results/c50.SRQ", self.get_trees_files()):
            cmd = ['trees-to-SRQ','Results/c50.tree','--max-points=1000']
            if self.subsample is not None and self.subsample != 1:
                cmd.append("--subsample={}".format(self.subsample))
            if self.burnin is not None:
                cmd.append("--skip={}".format(self.burnin))
            if self.until is not None:
                cmd.append("--until={}".format(self.until))
            cmd += self.get_trees_files()
            self.exec_show(cmd,outfile="Results/c50.SRQ")
        print("done.");
        self.srq.append("c50")

        for srq in self.srq:
            cmd = ['gnuplot']
            self.run_gnuplot("""\
set terminal png size 300,300
set output "Results/{srq}.SRQ.png"
set key right bottom
set xlabel "Regenerations (fraction)"
set ylabel "Time (fraction)"
set title "Scaled Regeneration Quantile (SRQ) plot: $srq"
plot "Results/{srq}.SRQ" title "{srq}" with linespoints lw 1 lt 1, x title "Goal" lw 1 lt 3
""".format(srq=srq))
        if self.subpartitions:
            self.run_gnuplot("""\
set terminal svg
set output "Results/c-levels.svg"
set xlabel "Log10 posterior Odds (LOD)"
set ylabel "Supported Splits"
set style data lines
plot [0:][0:] 'Results/c-levels.plot' title 'Full Splits','Results/extended-c-levels.plot' title 'Partial Splits'""")
        else:
            self.run_gnuplot("""\
set terminal svg
set output "Results/c-levels.svg"
set xlabel "Log10 posterior Odds (LOD)"
set ylabel "Supported Splits"
plot [0:][0:] 'Results/c-levels.plot' with lines notitle
""")
    def compute_tree_MDS(self):
        if not self.R_exe:
            return
        print ("\nGenerate MDS plots of topology burnin: ", end='',flush=True)

        N = 400
        dist_cmd = ['trees-distances','matrix','--max={}'.format(N),'--jitter=0.3']
        if self.subsample is not None and self.subsample != 1:
            dist_cmd.append("--subsample={}".format(self.subsample))
        if self.burnin is not None:
            dist_cmd.append("--skip={}".format(self.burnin))
        if self.until is not None:
            dist_cmd.append("--until={}".format(self.until))

        if self.n_chains() == 1:
            tree_file = self.get_trees_files()[0]
            matfile = "Results/tree1.M"
            outfile = "Results/tree1.svg"
            if not more_recent_than(matfile, tree_file):
                self.exec_show(dist_cmd+[tree_file], outfile=matfile)

            if not more_recent_than(outfile, matfile):
                script = self.get_libexec_script("tree-plot1.R")
                self.Rexec(script,[matfile,outfile])

            if not more_recent_than("Results/tree-3D-1.points", matfile):
                script3d = self.get_libexec_script("tree-plot1-3D.R")
                point_string = self.Rexec(script3d,[matfile])
                self.write_x3d_file("Results","tree-3D-1.points", point_string)
        elif self.n_chains() == 2:
            outfile = "Results/tree-1-2.svg"
            [tf1,tf2] = self.get_trees_files()
            L1 = min([N,int((get_n_lines(tf1)-self.burnin)/self.subsample)])
            L2 = min([N,int((get_n_lines(tf2)-self.burnin)/self.subsample)])
            matfile = "Results/tree-1-2.M"
            outfile3d = "Results/tree-1-2.points.html"

            if not more_recent_than_all_of(matfile,self.get_trees_files()):
                self.exec_show(dist_cmd+[tf1,tf2],outfile=matfile)

            if not more_recent_than_all_of(outfile,self.get_trees_files()):
                script = self.get_libexec_script("tree-plot2.R")
                self.Rexec(script,[str(L1),str(L2),matfile,outfile])

            if not more_recent_than_all_of(outfile3d,self.get_trees_files()):
                script3d = self.get_libexec_script("tree-plot2-3D.R")
                point_string = self.Rexec(script3d, [str(L1),str(L2),matfile])
                self.write_x3d_file("Results","tree-1-2.points",point_string)

        elif self.n_chains() >= 3:
            script = self.get_libexec_script("tree-plot3.R")
            tree_files = self.get_trees_files()[0:3]
            [tf1,tf2,tf3] = tree_files
            L1 = min([N,int((get_n_lines(tf1)-self.burnin)/self.subsample)])
            L2 = min([N,int((get_n_lines(tf2)-self.burnin)/self.subsample)])
            L3 = min([N,int((get_n_lines(tf3)-self.burnin)/self.subsample)])
            matfile = "Results/tree-1-2-3.M"
            outfile = "Results/tree-1-2-3.svg"
            outfile3d = "Results/tree-1-2-3.points.html"
            if not more_recent_than_all_of(matfile,tree_files):
                self.exec_show(dist_cmd + tree_files,outfile=matfile)

            if not more_recent_than_all_of(outfile,tree_files):
                self.Rexec(script, [str(L1), str(L2), str(L3), matfile, outfile])

            if not more_recent_than_all_of(outfile3d,tree_files):
                script3d = self.get_libexec_script("tree-plot3-3D.R")
                point_string = self.Rexec(script3d, [str(L1), str(L2), str(L3), matfile])
                self.write_x3d_file("Results", "tree-1-2-3.points", point_string)
        print(" done.")

    def write_x3d_file(self,dir,filename,point_string):
        assert(point_string is not None)

        if dir:
            filename = path.join(dir,filename+".html")
        with open(filename,"w+",encoding='utf-8') as x3d_file:
            print("""\
<html>
 <head>
   <title>MDS 3D Plot</title>
   <script type='text/javascript' src='http://www.x3dom.org/download/x3dom.js'> </script>
   <link rel='stylesheet' type='text/css' href='http://www.x3dom.org/download/x3dom.css'></link>
    <style>
      x3d { border:2px solid darkorange; }
    </style>
  </head>
  <body>
    <x3d width='1000px' height='1000px'>
      <scene>""",file=x3d_file)
            print(get_x3d_of_mds(point_string),file=x3d_file)
            print("""\
      </scene>
    </x3d>
  </body>
</html>""",file=x3d_file)

    def compute_initial_alignments(self):
        names = self.run(0).compute_initial_alignments()
        for i in range(len(names)):
            name = names[i]
            self.alignments[name] = self.get_alphabets()[i]
            self.make_ordered_alignment(name)


    def compute_wpd_alignments(self):
        print("\nComputing WPD alignments: ", end='',flush=True)
        for i in range(self.n_partitions()):
            if self.get_imodel_indices()[i] is None:
                continue
            afiles = self.get_alignments_for_partition(i)
            name = "P{}.max".format(i+1)
            self.alignments[name] = self.get_alphabets()[i]
            if not more_recent_than_all_of("Results/Work/{}-unordered.fasta".format(name),afiles):
                cut_cmd=['cut-range']+afiles
                if self.burnin is not None:
                    cut_cmd.append("--skip={}".format(self.burnin))
                if self.until is not None:
                    cut_cmd.append("--until={}".format(self.until))
                p1 = subprocess.Popen(cut_cmd,  stdout=subprocess.PIPE)

                chop_cmd=['alignment-chop-internal','--tree=Results/MAP.tree']
                p2 = subprocess.Popen(chop_cmd, stdout=subprocess.PIPE, stdin=p1.stdout)

                max_cmd=['alignment-max']
                self.exec_show(max_cmd,stdin=p2.stdout, outfile="Results/Work/{}-unordered.fasta".format(name))

                p1.wait()
                p2.wait()
                self.make_ordered_alignment(name)
        print(" done.")

    def reorder_alignment_by_tree(self,alignment,tree,outfile):
        if not path.exists(tree):
            print("Can't reorder alignment by tree '{}': tree file does not exist!".format(tree))
        if file_is_empty(tree):
            print("Can't reorder alignment by tree '{}': tree file is empty!".format(tree))
        if not path.exists(alignment):
            print("Can't reorder alignment '{}' by tree: alignment file does not exist!".format(tree))
        if file_is_empty(alignment):
            print("Can't reorder alignment '{}' by tree: alignment file is empty!".format(tree))
        if not more_recent_than_all_of(outfile,[tree,alignment]):
            cmd = ['alignment-cat', alignment, '--reorder-by-tree={}'.format(tree)]
            self.exec_show(cmd, outfile=outfile)
        assert(path.exists(outfile))

    def make_ordered_alignment(self,alignment):
        ufilename = "Results/Work/{}-unordered.fasta".format(alignment)
        filename = "Results/{}.fasta".format(alignment)
        if not more_recent_than_all_of(filename, [ufilename,"Results/c50.tree"] ):
            self.reorder_alignment_by_tree(ufilename,"Results/c50.tree",filename)
        assert(path.exists(filename))
        return filename

    def color_scheme_for_alphabet(self,alphabet):
        if alphabet == "Amino-Acids":
            return "AA+contrast"
        else:
            return "DNA+contrast"

    def draw_alignment(self,filename,**kwargs):
        cmd = ['alignment-draw',filename]

        if "color_scheme" in kwargs:
            color_scheme = kwargs["color_scheme"]
            if color_scheme is not None:
                cmd += ['--color-scheme',color_scheme]

        if "ruler" in kwargs:
            if kwargs["ruler"]:
                cmd += ['--show-ruler']

        if "AU" in kwargs:
            AU = kwargs["AU"]
            if AU is not None:
                cmd += ['--AU',AU]

        if "outfile" in kwargs:
            outfile = kwargs["outfile"]
        else:
            outfile = None
        if outfile is None:
            outfile = os.path.splitext(filename)[0]+'.html'

        self.exec_show(cmd,outfile=outfile)
        return outfile

    def draw_alignments(self):
        if not self.alignments:
            return

        print("Drawing alignments: ", end='', flush=True)
        for (alignment,alphabet) in self.alignments.items():
            filename = self.make_ordered_alignment(alignment)
            color_scheme = self.color_scheme_for_alphabet(alphabet)
            self.draw_alignment(filename, color_scheme=color_scheme, ruler=True)
            print('*',end='',flush=True)

        for i in range(self.n_partitions()):
            if self.get_imodel_indices()[i] is None:
                continue

            outfile = "Results/P{}.initial-diff.AU".format(i+1)
            initial = "Results/P{}.initial.fasta".format(i+1)
            wpd = "Results/P{}.max.fasta".format(i+1)
            self.exec_show(['alignments-diff',initial,wpd],outfile=outfile)

            outhtml = "Results/P{}.initial-diff.html".format(i+1)
            self.exec_show(['alignment-draw',initial,'--scale=identity','--AU',outfile,'--show-ruler','--color-scheme=diff[1]+contrast'],outfile=outhtml)
            print("*",end='',flush=True);

        print(" done.")


    def compute_ancestral_states(self):
        print("Computing ancestral state alignment: ",end='',flush=True)
        for i in range(self.n_partitions()):
            afiles = self.get_alignments_for_partition(i)
            name = 'P{}.ancestors'.format(i+1)
#           We can't do AU on this, since it has too many rows!
#           We should be able to draw it though.
#            self.alignments[name] = self.get_alphabets()[i]
            if self.get_imodel_indices()[i] is None:
                bad = 0
                for afile in afiles:
                    if afile is None or not path.exists(afile):
                        bad += 1
                if bad > 0:
                    continue
            assert(len(afiles) == self.n_chains())

            template = "Results/P{}.max.fasta".format(i+1)
            if self.get_imodel_indices()[i] is None:
                template = "Results/P{}.initial.fasta".format(i+1)
            tree = "Results/c50.tree"
            cmd = ['extract-ancestors','-a',template,'-n',tree,'-g',tree]
            for dir in [run.dir for run in self.mcmc_runs]:
                cmd += ['-A',path.join(dir,'C1.P{}.fastas'.format(i+1)),
                        '-T',path.join(dir,'C1.trees')]
            output = "Results/P{}.ancestors.fasta".format(i+1)
            if not more_recent_than_all_of(output,self.get_trees_files()+list(filter(lambda x:x is not None,flatten(self.get_alignments_files())))):
                self.exec_show(cmd,outfile=output)
        print(" done.")

    def compute_and_draw_AU_plots(self):
        for (alignment,alphabet) in self.alignments.items():
            m = re.match('^P([0-9]+).*',alignment)
            if m:
                i = int(m.group(1))-1
                afile = "Results/{}.fasta".format(alignment)
                afiles = self.get_alignments_for_partition(i)
                if None in afiles:
                    continue
                print("Generating AU values for '{}'...".format(alignment),end='',flush=True)
                AUfile = "Results/{}-AU.prob".format(alignment)
                if not more_recent_than_all_of(AUfile, afiles):
                    cut_cmd=['cut-range']+afiles
                    if self.burnin is not None:
                        cut_cmd.append("--skip={}".format(self.burnin))
                    if self.until is not None:
                        cut_cmd.append("--until={}".format(self.until))
                    p1 = subprocess.Popen(cut_cmd,  stdout=subprocess.PIPE)

                    chop_cmd=['alignment-chop-internal','--tree=Results/MAP.tree']
                    p2 = subprocess.Popen(chop_cmd, stdout=subprocess.PIPE, stdin=p1.stdout)

                    gild_cmd = ['alignment-gild',afile,'Results/MAP.tree','--max-alignments=500']
                    self.exec_show(gild_cmd, stdin=p2.stdout, outfile=AUfile)

                    p1.wait()
                    p2.wait()

                html_file = "Results/{}-AU.html".format(alignment)
                if not more_recent_than_all_of(html_file,[afile,AUfile]):
                    color_scheme=self.color_scheme_for_alphabet(alphabet)
                    color_scheme += "+fade+fade+fade+fade"
                    self.draw_alignment(afile,ruler=True,AU=AUfile,color_scheme=color_scheme,outfile=html_file)
                print(' done.')



    def print_index_html(self):
        print("\nReport written to 'Results/index.html");

#----------------------------- SETUP 1 --------------------------#
if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Generate an HTML report summarizing MCMC runs for BAli-Phy and other software.",
                                     epilog= "Example: bp-analyze analysis-dir-1 analysis-dir-2")

    parser.add_argument("mcmc_outputs", default=['.'], help="Subdirectories with MCMC runs",nargs='*')
    parser.add_argument("--clean", default=False,help="Delete generated files",action='store_true')
    parser.add_argument("--verbose",default=0, help="Be verbose",action='store_true')
    parser.add_argument("--skip", metavar='NUM',type=int,default=None,help="Skip NUM iterations as burnin")
    parser.add_argument("--subsample",metavar='NUM',type=int,default=1,help="Keep only every NUM iterations")
    parser.add_argument("--until",type=int,default=None)
    parser.add_argument("--prune", default=None,help="Taxa to remove")
    parser.add_argument("--muscle")
    parser.add_argument("--probcons")
    parser.add_argument("--mafft")
    parser.add_argument("--subpartitions",default=False,action='store_true')
    # FIXME: change subdirs to check if we've got a list of tree files instead
    parser.add_argument("--tree-file",nargs='*')
    args = parser.parse_args()

    analysis = Analysis(args,args.mcmc_outputs)

#    smodels = analysis.get_models("subst model", "smodels")
#    imodels = analysis.get_models("indel model", "imodels")
#    scale_models = analysis.get_models("scale model", "scales")



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
