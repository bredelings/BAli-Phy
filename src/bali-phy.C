/*
   Copyright (C) 2004-2010 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

///
/// \file bali-phy.C
///
/// \brief This file contains routines to parse and check input and initiate
///        the bali-phy program.
///

/// \mainpage
/// \section main Main
/// The program is initiated from the file bali-phy.C
///
/// \section subst Substitution
/// The substitution likelihood is calculated in the file
/// substitution.C.  In order to speed up likelihood calculations, we
/// cache conditional likelihoods (substitution-cache.C).  However,
/// because the alignment is changing, column numbers may change and
/// cannot be relied on.  Therefore, we compute indices for a branch
/// that do not change if the alignment of the subtree behind the
/// branch does not change (substitution-index.C).  Exponentials are
/// computed in exponential.C .
/// 
/// \section mcmc MCMC
/// The Markov Chain Monte Carlo (MCMC) routines are all called from
/// the file mcmc.C.  Markov chains are constructed in mcmc/setup.C.
/// Proposals for Metropolis-Hastings moves are defined in
/// proposals.C . Slice sampling routines are defined in
/// slice-sampling.C. 
///
/// \section objects Objects
/// Trees are defined in tree.C and tree-branchnode.H. Leaf-labelled
/// trees are defined in sequencetree.C.  Sequences are defined in
/// sequence.C. The types of sequence are defined in alphabet.C, which
/// contains classes that relate letters like A, T, G, and C to
/// integers.  Alignments are defined in alignment.C and place 
/// collection of sequences in a matrix to depict homology.  Methods
/// for reading FASTA and PHYLIP files are defined in
/// sequence-format.C.
///
/// \section models Models
/// In BAli-Phy, models are built from class Model.  Model objects
///   - depend on some number of parameters (all of type double)
///   - implement prior distributions on their parameters.
///
/// A Model object that implements the SuperModel interface can
/// contain other Model objects as parts - child Model parameters are
/// mapped to parameters in the parent Model.  However, two child
/// Models cannot (easily) share a parameter, because each Model
/// manages and 'owns' its own parameters.  This ownership 
/// means that a Model specifies:
/// - a prior distribution on its parameter vector
/// - a name (a string) for each parameter
/// - an attribute (a boolean) that determinies whether each parameter is fixed or variable
///
/// \section DP Dynamic Programming
/// Many of the sampling routines rely on dynamic programming.
/// Dynamic programming in turn relies on Hidden Markov Models
/// (HMMs).  A general HMM class is defined in hmm.C.  Specific HMMs
/// are define in 2way.C (pairwise alignments), 3way.C (three adjacent
/// pairwise alignments) and 5way.C (pairwise alignments on a branch
/// and its 4 adjacent branches).  Objects for performing 1-D dynamic
/// programming are defined in dp-array.C.  Objects for performing 2-D 
/// dynamic programming are defined in dp-matrix.C. 
///
/// There are a number of dynamic programming problems, and each of
/// them involves both (a) summing over all the alignments in some
/// set, and (b) sampling from the posterior distribution of
/// alignments in that set.  Each problem is distinguished by the
/// different set of alignments.  The file sample-alignment.C
/// implements summing/sampling the alignment on one branch (2D) using
/// the HMM in 2way.C.  The file sample-node.C implements resampling
/// the +/- state in each column for a specific internal node
/// sequence (1D) using the HMM in 3way.C. The file sample-tri.C
/// implements resampling the alignment along a branch, and also
/// resampling the +/- state in each column for a sequence at an
/// internal node on one end of the branch (2D) using the HMM in
/// 3way.C.  The file sample-two-nodes.C implements resample the +/-
/// state for sequence at two internal nodes connected by a branch
/// (1D) using the HMM in 5way.C.  The interface for these routes and
/// other related utility routines is in alignment-sums.H.
/// 
/// \section intro_sec Introduction
///
/// BAli-Phy is an MCMC sampler to jointly estimate alignments and a phylogeny.
///
///  

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_SYS_RESOURCE_H
extern "C" {
#include <sys/resource.h>
}
#endif

#ifdef HAVE_FENV_H
extern "C" {
#include "fenv.h"
}
#endif

#include "timer_stack.H"

#ifdef HAVE_MPI
#include <mpi.h>
#include <boost/mpi.hpp>
namespace mpi = boost::mpi;
#endif

#include <cmath>
#include <ctime>
#include <iostream>
#include <fstream>
#include <sstream>
#include <new>
#include <signal.h>
#include <map>

#include <boost/program_options.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/chrono.hpp>

#include "substitution/substitution.H"
#include "myexception.H"
#include "tree/sequencetree.H"
#include "alignment/alignment.H"
#include "rng.H"
#include "models/parameters.H"
#include "mcmc/mcmc.H"
#include "likelihood.H"
#include "util.H"
#include "setup.H"
#include "smodel/setup.H"
#include "imodel/setup.H"
#include "alignment/alignment-constraint.H"
#include "alignment/alignment-util.H"
#include "substitution/substitution-index.H"
#include "monitor.H"
#include "math/pow2.H"
#include "tree/tree-util.H" //extends
#include "version.H"
#include "mcmc/setup.H"
#include "io.H"
#include "tools/parsimony.H"
#include "parser/desugar.H"
#include "computation/module.H"

namespace fs = boost::filesystem;
namespace chrono = boost::chrono;

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::clog;
using std::endl;
using std::ifstream;
using std::ofstream;
using std::ostream;
using std::string;
using std::vector;
using std::map;
using std::set;

using boost::dynamic_bitset;
using boost::shared_ptr;

#ifdef DEBUG_MEMORY
void * operator new(size_t sz) throw(std::bad_alloc) {
  printf("new called, sz = %d\n",sz);
  return malloc(sz); 
}

void operator delete(void * p) throw() {
  printf("delete called, content = %d\n",(*(int*)p));
  free(p); 
}
#endif

// How to record that the user said e.g. "fix the alignment"?  Or, fix parameter X?  Should we?

const string trailing_args_separator = "---";

vector<string> drop_trailing_args(int argc, char* argv[], const string& separator)
{
  vector<string> args;
  for(int i=1;i<argc;i++)
  {
    string arg = argv[i];
    if (arg == separator) break;
    args.push_back(arg);
  }
  return args;
}

vector<string> trailing_args(int argc, char* argv[], const string& separator)
{
  vector<string> args;
  int i = 1;
  for(;i<argc;i++)
  {
    string arg = argv[i];
    if (arg == separator) break;
  }
  for(i++;i<argc;i++)
  {
    string arg = argv[i];
    args.push_back(arg);
  }
  return args;
}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  options_description advanced("Advanced options");
  advanced.add_options()
    ("beta",value<string>(),"MCMCMC temperature")
    ("dbeta",value<string>(),"MCMCMC temperature changes")
    ("internal",value<string>(),"If set to '+', then make all internal node entries wildcards")
    ("partition-weights",value<string>(),"File containing tree with partition weights")
    ("t-constraint",value<string>(),"File with m.f. tree representing topology and branch-length constraints.")
    ("a-constraint",value<string>(),"File with groups of leaf taxa whose alignment is constrained.")
    ("verbose","Print extra output in case of error.")
    ("subA-index",value<string>()->default_value("internal"),"What kind of subA index to use?")
    ;

  // named options
  options_description general("General options");
  general.add_options()
    ("help,h", "Print usage information.")
    ("version,v", "Print version information.")
    ("config,c", value<string>(),"Config file to read.")
    ("show-only","Analyze the initial values and exit.")
    ("seed", value<unsigned long>(),"Random seed")
    ("name", value<string>(),"Name for the analysis directory to create.")
    ("traditional,t","Fix the alignment and don't model indels.")
    ;
  
  options_description mcmc("MCMC options");
  mcmc.add_options()
    ("iterations,i",value<long int>()->default_value(100000),"The number of iterations to run.")
    ("pre-burnin",value<int>()->default_value(3),"Iterations to refine initial tree.")
    ("subsample",value<int>()->default_value(1),"Factor by which to subsample.")
    ("enable",value<string>(),"Comma-separated list of kernels to enable.")
    ("disable",value<string>(),"Comma-separated list of kernels to disable.")
    ;
    
  options_description parameters("Parameter options");
  parameters.add_options()
    ("align", value<vector<string> >()->composing(),"Files with sequences and initial alignment.")
    ("randomize-alignment","Randomly realign the sequences before use.")
    ("unalign-all","Unalign all sequences sequences before use.")
    ("tree",value<string>(),"File with initial tree")
    ("initial-value",value<vector<string> >()->composing(),"Set parameter=<initial value>")
    ("set",value<vector<string> >()->composing(),"Set key=<value>")
    ("frequencies",value<string>(),"Initial frequencies: 'uniform','nucleotides', or a comma-separated vector.")
    ("model",value<string>(),"File containing hierarchical model description.")
    ("Rao-Blackwellize",value<string>(),"Parameter names to print Rao-Blackwell averages for.")
    ;

  options_description model("Model options");
  model.add_options()
    ("alphabet",value<vector<string> >()->composing(),"The alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.")
    ("smodel",value<vector<string> >()->composing(),"Substitution model.")
    ("imodel",value<vector<string> >()->composing(),"Indel model: none, RS05, RS07-no-T, or RS07.")
    ("branch-prior",value<string>()->default_value("Gamma"),"Exponential, Gamma, or Dirichlet.")
    ("same-scale",value<vector<string> >()->composing(),"Which partitions have the same scale?")
    ("align-constraint",value<string>(),"File with alignment constraints.")
    ("modules-path",value<string>(),"Directories to search for modules (: separated)")
    ("builtins-path",value<string>(),"Directories to search for modules (: separated)")
    ;
  options_description all("All options");
  all.add(general).add(mcmc).add(parameters).add(model).add(advanced);
  options_description some("All options");
  some.add(general).add(mcmc).add(parameters).add(model);

  // positional options
  positional_options_description p;
  p.add("align", -1);
  

  vector<string> cargs = drop_trailing_args(argc, argv, trailing_args_separator);
  variables_map args;
  store(command_line_parser(cargs).options(all).positional(p).run(), args);
  notify(args);    

  if (args.count("version")) {
    print_version_info(cout);
    exit(0);
  }

  if (args.count("verbose"))
    log_verbose = 1;

  if (args.count("help")) {
    cout<<"Usage: bali-phy <sequence-file1> [<sequence-file2> [OPTIONS]]\n";
    cout<<some<<"\n";
    exit(0);
  }

  if (args.count("config")) 
  {
    string filename = args["config"].as<string>();
    checked_ifstream file(filename,"config file");

    store(parse_config_file(file, all), args);
    notify(args);
  }

  load_bali_phy_rc(args,all);

  if (not args.count("align")) 
    throw myexception()<<"No sequence files given.\n\nTry `"<<argv[0]<<" --help' for more information.";

  if (not args.count("iterations"))
    throw myexception()<<"The number of iterations was not specified.\n\nTry `"<<argv[0]<<" --help' for more information.";

  return args;
}

int parameter_with_extension(const Model& M, const string& name)
{
  vector<int> indices = parameters_with_extension(M, name);
  if (not indices.size())
    return -1;

  if (indices.size() == 1)
    return indices.back();

  myexception e;
  e<<"Multiple parameter names fit the pattern '"<<name<<"':\n";
  for(int i=0;i<indices.size();i++)
    e<<"   * "<<M.parameter_name(indices[i])<<"\n";

  throw e;
}

void set_key_values(Parameters& P, const variables_map& args)
{
  if (not args.count("set")) return;

  vector<string> key_value_pairs = args["set"].as<vector<string> >();

  for(const auto& key_value_pair: key_value_pairs)
  {
    vector<string> parse = split(key_value_pair,'=');
    if (parse.size() != 2)
      throw myexception()<<"Ill-formed key-value pair '"<<key_value_pair<<"'.";

    string key = parse[0];

    double value = convertTo<double>(parse[1]);
    
    (*P.keys.modify())[key] = value;
  }
}

/// Parse command line arguments of the form --fix X=x or --unfix X=x or --set X=x and modify P
void set_initial_parameter_values(Parameters& P, const variables_map& args) 
{
  //-------------- Specify fixed parameters ----------------//
  vector<string> doset;
  if (args.count("initial-value"))
    doset = args["initial-value"].as<vector<string> >();

  vector<string> short_names = short_parameter_names(P);

  // set parameters
  for(const auto& arg: doset)
  {
    //parse
    vector<string> parse = split(arg,'=');
    if (parse.size() != 2)
      throw myexception()<<"Ill-formed initial condition '"<<arg<<"'.";

    string name = parse[0];
    object_ref value;
    try {
      value = parse_object(parse[1]);
    }
    catch (myexception& e)
    {
      std::ostringstream o;
      o<<"Setting parameter '"<<name<<"': ";
      e.prepend(o.str());
      throw e;
    }

    int p_index = P.find_parameter(name);
    if (p_index == -1)
      p_index = find_index(short_names,name);
    if (p_index == -1)
      throw myexception()<<"Can't find parameter '"<<name<<"' to set value '"<<parse[1]<<"'";

    P.set_parameter_value(p_index,value);
  }
}

/// Close the files.
void close_files(vector<shared_ptr<ofstream>>& files)
{
  for(int i=0;i<files.size();i++)
    files[i]->close();
  files.clear();
}

/// Delete the files specified by 'filenames'
void delete_files(vector<string>& filenames)
{
  for(int i=0;i<filenames.size();i++)
    fs::remove(filenames[i]);
  filenames.clear();
}

vector<shared_ptr<ofstream>> open_files(int proc_id, const string& name, vector<string>& names)
{
  vector<shared_ptr<ofstream>> files;
  vector<string> filenames;

  for(int j=0;j<names.size();j++) 
  {
    string filename = name + "C" + convertToString(proc_id+1)+"."+names[j];
      
    if (fs::exists(filename)) {
      close_files(files);
      delete_files(filenames);
      throw myexception()<<"Trying to open '"<<filename<<"' but it already exists!";
    }
    else {
      files.push_back(shared_ptr<ofstream>(new ofstream(filename.c_str())));
      filenames.push_back(filename);
    }
  }

  names = filenames;

  return files;
}

string open_dir(const string& dirbase)
{
  for(int i=1;;i++) {
    string dirname = dirbase + "-" + convertToString(i);

    if (not fs::exists(dirname)) {
      fs::create_directories(dirname);
      return dirname;
    }
  }
}

#if defined _MSC_VER || defined __MINGW32__
#include <windows.h>
#include <errno.h>
#include <process.h>

string hostname() 
{
  // We have to use MAX_COMPUTERNAME_LENGTH+1 so it doesn't fail in Win9x
  char temp[MAX_COMPUTERNAME_LENGTH + 1];
  DWORD size =  sizeof (temp);

  if (!GetComputerName (temp, &size))
    return "unknown";

  return string(temp);
}
#else
string hostname()
{
  string hostname="";
  char temp[256];
  if (not gethostname(temp,256))
    hostname = temp;
  return hostname;
}
#endif

/// Create the directory for output files and return the name
string init_dir(const variables_map& args)
{
  vector<string> alignment_filenames = args["align"].as<vector<string> >();
  for(int i=0;i<alignment_filenames.size();i++)
    alignment_filenames[i] = remove_extension( fs::path( alignment_filenames[i] ).leaf().string() );

  string name = join(alignment_filenames,'-');
  if (args.count("name"))
    name = args["name"].as<string>();
    
  string dirname = open_dir(name);
  cerr<<"Created directory '"<<dirname<<"/' for output files."<<endl;
  return dirname;
}

/// Create output files for thread 'proc_id' in directory 'dirname'
vector<shared_ptr<ostream>> init_files(int proc_id, const string& dirname,
			    int argc,char* argv[])
{
  vector<shared_ptr<ostream>> files;

  vector<string> filenames;
  filenames.push_back("out");
  filenames.push_back("err");

  vector<shared_ptr<ofstream>> files2 = open_files(proc_id, dirname+"/",filenames);
  files.clear();
  for(int i=0;i<files2.size();i++)
    files.push_back(files2[i]);

  ostream& s_out = *files[0];
    
  s_out<<"command: ";
  for(int i=0;i<argc;i++) {
    s_out<<argv[i];
    if (i != argc-1) s_out<<" ";
  }
  s_out<<endl;
  {
    time_t now = time(NULL);
    s_out<<"start time: "<<ctime(&now)<<endl;
  }
  print_version_info(s_out);
  s_out<<"directory: "<<fs::initial_path().string()<<endl;
  s_out<<"subdirectory: "<<dirname<<endl;
  if (getenv("SLURM_JOBID"))
    s_out<<"SLURM_JOBID: "<<getenv("SLURM_JOBID")<<endl;
  if (getenv("JOB_ID"))
    s_out<<"JOB_ID: "<<getenv("JOB_ID")<<endl;
  if (getenv("LSB_JOBID"))
    s_out<<"LSB_JOBID: "<<getenv("LSB_JOBID")<<endl;
  s_out<<"hostname: "<<hostname()<<endl;
  s_out<<"PID: "<<getpid()<<endl;
#ifdef HAVE_MPI
  mpi::communicator world;
  s_out<<"MPI_RANK: "<<world.rank()<<endl;
  s_out<<"MPI_SIZE: "<<world.size()<<endl;
#endif
  s_out<<endl;

  //  files[0]->precision(10);
  //  cerr.precision(10);

  return files;
}

/// Determine the parameters of model \a M that must be sorted in order to enforce identifiability.
vector< vector< vector<int> > > get_un_identifiable_indices(const Model& M, const vector<string>& names)
{
  vector< vector< vector<int> > > indices;

  int n_smodels = dynamic_cast<const Parameters&>(M).n_smodels();

  for(int i=0;i<n_smodels+1;i++) 
  {
    string prefix = "^";
    if (i>0)
      prefix = string("S")+convertToString(i) + ".";

    vector< vector<int> > DP;
    if (parameters_with_extension(names, prefix + "DP.rate*").size()  )
    {
      DP.push_back( parameters_with_extension(names, prefix + "DP.rate*") );
      DP.push_back( parameters_with_extension(names, prefix + "DP.f*") );
      indices.push_back( DP );
    }

    vector< vector<int> > M3;
    if (parameters_with_extension(names, prefix + "M3.omega*").size() )
    {
      M3.push_back( parameters_with_extension(names, prefix + "M3.omega*") );
      M3.push_back( parameters_with_extension(names, prefix + "M3.f*") );
      indices.push_back( M3 );
    }
  }

  return indices;
}

void find_sub_loggers(Parameters& P, int& index, const string& name, vector<int>& logged_computations, vector<string>& logged_names)
{
  assert(index != -1);
  object_ref result = P.evaluate(index);
  if ((bool)dynamic_pointer_cast<const Double>(result) or (bool)dynamic_pointer_cast<const Int>(result))
  {
    logged_computations.push_back(index);
    logged_names.push_back(name);
    index = -1;
    return;
  }

  if (auto c = dynamic_pointer_cast<const constructor>(result))
  {
    if (c->f_name == "Prelude.True" or c->f_name == "Prelude.False")
    {
      logged_computations.push_back(index);
      logged_names.push_back(name);
      index = -1;
      return;
    }

    if (c->f_name == "[]")
      return;

    if (c->f_name == ":")
    {
      expression_ref L = P.get_expression(index);
      expression_ref E = (identifier("Prelude.length"),L);
      int length = *convert<const Int>(P.evaluate_expression(E));
      int index2 = -1;
      for(int i=0;i<length;i++)
      {
	expression_ref E2 = (identifier("Prelude.!!"),L,i) ;
	if (index2 == -1)
	  index2 = P.add_compute_expression(E2);
	else
	  P.set_compute_expression(index2, E2);

	find_sub_loggers(P, index2, name+"!!"+convertToString(i), logged_computations, logged_names);
      }
    }
  }
}




owned_ptr<MCMC::TableFunction<string> > construct_table_function(Parameters& P, const vector<string>& Rao_Blackwellize)
{
  using namespace MCMC;
  owned_ptr<TableGroupFunction<string> > TL = claim(new TableGroupFunction<string>);
  
  TL->add_field("iter", ConvertToStringFunction<long>( IterationsFunction() ) );
  TL->add_field("prior", GetPriorFunction() );
  for(int i=0;i<P.n_data_partitions();i++)
    if (P[i].variable_alignment())
      TL->add_field("prior_A"+convertToString(i+1), GetAlignmentPriorFunction(i) );
  TL->add_field("likelihood", GetLikelihoodFunction() );
  TL->add_field("logp", GetProbabilityFunction() );
  
  {
    vector<int> logged_computations;
    vector<string> logged_names;

    vector<string> names_ = parameter_names(P);
    set<string> names(names_.begin(), names_.end());

    // FIXME: Using short_parameter_names should be nice... but
    //          we are now logging EXPRESSIONS as well as actual parameters
    //        This makes such simplification difficult.

    for(int i=0;i<P.n_parameters();i++)
    {
      string name = P.parameter_name(i);
      if (name.size() and name[0] == '*' and not log_verbose) continue;

      int index = P.add_compute_expression(parameter(name));

      find_sub_loggers(P, index, name, logged_computations, logged_names);
    }

    TableGroupFunction<object_ref> T1;
    for(int i=0;i<logged_computations.size();i++)
    {
      int index = logged_computations[i];
      string name = logged_names[i];
      T1.add_field(name, GetComputationFunction(index) );
    }

    SortedTableFunction T2(T1, get_un_identifiable_indices(P, logged_names));

    TL->add_fields( ConvertTableToStringFunction<object_ref>( T2 ) );
  }

  
  for(int i=0;i<P.n_data_partitions();i++)
    {
      if (P[i].variable_alignment())
	{
	  TL->add_field("|A"+convertToString(i+1)+"|", Get_Alignment_Length_Function(i) );
	  TL->add_field("#indels"+convertToString(i+1), Get_Num_Indels_Function(i) );
	  TL->add_field("|indels"+convertToString(i+1)+"|", Get_Total_Length_Indels_Function(i) );
	}
      const alphabet& a = P[i].get_alphabet();
      TL->add_field("#substs"+convertToString(i+1), Get_Num_Substitutions_Function(i, unit_cost_matrix(a)) );
      if (const Triplets* Tr = dynamic_cast<const Triplets*>(&a))
	TL->add_field("#substs(nuc)"+convertToString(i+1), Get_Num_Substitutions_Function(i, nucleotide_cost_matrix(*Tr)) );
      if (const Codons* C = dynamic_cast<const Codons*>(&a))
	TL->add_field("#substs(aa)"+convertToString(i+1), Get_Num_Substitutions_Function(i, amino_acid_cost_matrix(*C)) );
    }
  
  if (P.variable_alignment()) {
    TL->add_field("|A|", Get_Total_Alignment_Length_Function() );
    TL->add_field("#indels", Get_Total_Num_Indels_Function() );
    TL->add_field("|indels|", Get_Total_Total_Length_Indels_Function() );
  }
  TL->add_field("#substs", Get_Total_Num_Substitutions_Function() );
  
  TL->add_field("|T|", Get_Tree_Length_Function() );

  for(const auto& p: Rao_Blackwellize)
  {
    int p_index = P.find_parameter(p);
    if (p_index == -1)
      throw myexception()<<"No such parameter '"<<p<<"' to Rao-Blackwellize";

    vector<object_ref> values = {Int(0),Int(1)};
    TL->add_field("RB-"+p, Get_Rao_Blackwellized_Parameter_Function(p_index, values));
  }

  return TL;
}

vector<owned_ptr<MCMC::Logger> > construct_loggers(Parameters& P, const vector<string>& Rao_Blackwellize, int proc_id, const string& dir_name)
{
  using namespace MCMC;
  vector<owned_ptr<Logger> > loggers;

  string base = dir_name + "/" + "C" + convertToString(proc_id+1);

  owned_ptr<TableFunction<string> > TF = construct_table_function(P, Rao_Blackwellize);

  // Write out scalar numerical variables (and functions of them) to C<>.p
  loggers.push_back( TableLogger(base +".p", TF) );
  
  // Write out the (scaled) tree each iteration to C<>.trees
  loggers.push_back( FunctionLogger(base + ".trees", TreeFunction()<<"\n" ) );
  
  // Write out the MAP point to C<>.MAP - later change to a dump format that could be reloaded?
  {
    ConcatFunction F; 
    F<<TableViewerFunction(TF)<<"\n";
    F<<Show_SModels_Function()<<"\n";
    for(int i=0;i<P.n_data_partitions();i++)
      if (P[i].variable_alignment())
	F<<AlignmentFunction(i)<<"\n\n";
    F<<TreeFunction()<<"\n\n";
    loggers.push_back( FunctionLogger(base + ".MAP", MAP_Function(F)) );
  }

  // Write out the probability that each column is in a particular substitution component to C<>.P<>.CAT
  if (P.contains_key("log-categories"))
    for(int i=0;i<P.n_data_partitions();i++)
      loggers.push_back( FunctionLogger(base + ".P" + convertToString(i+1)+".CAT", 
					Mixture_Components_Function(i) ) );

  // Write out ancestral sequences
  if (P.contains_key("log-ancestral"))
    for(int i=0;i<P.n_data_partitions();i++)
      loggers.push_back( FunctionLogger(base + ".P" + convertToString(i+1)+".ancestral.fastas", 
					Ancestral_Sequences_Function(i) ) );

  // Write out the alignments for each (variable) partition to C<>.P<>.fastas
  for(int i=0;i<P.n_data_partitions();i++)
    if (P[i].variable_alignment()) 
    {
      string filename = base + ".P" + convertToString(i+1)+".fastas";

      ConcatFunction F;
      F<<"iterations = "<<ConvertToStringFunction<long> ( IterationsFunction() )<<"\n\n";
      F<<AlignmentFunction(i);

      loggers.push_back( FunctionLogger(filename, Subsample_Function(F,10) ) );
    }
  return loggers;
}

/// A stringbuf that write to 2 streambufs
class teebuf: public std::stringbuf
{
protected:
  std::streambuf* sb1;
  std::streambuf* sb2;

public:
  
  int sync() {
    string s = str();
    sb1->sputn(s.c_str(), s.length());
    sb2->sputn(s.c_str(), s.length());
    int rc = sb1->pubsync();
    rc = sb2->pubsync();
    str(string());
    return rc;
  } 

  std::streambuf* rdbuf1() {return sb1;}
  std::streambuf* rdbuf2() {return sb2;}

  void setbuf1(std::streambuf* sb) {sb1 = sb;}
  void setbuf2(std::streambuf* sb) {sb2 = sb;}

  teebuf(std::streambuf* s1, std::streambuf* s2):
    sb1(s1),
    sb2(s2)
  {}

  ~teebuf() {sync();}
};

// return the list of constrained branches
vector<int> load_alignment_branch_constraints(const string& filename, const SequenceTree& TC)
{
  // open file
  checked_ifstream file(filename,"alignment-branch constraint file");

  // read file
  string line;
  vector<vector<string> > name_groups;
  while(portable_getline(file,line)) {
    vector<string> names = split(line,' ');
    for(int i=names.size()-1;i>=0;i--)
      if (names[i].size() == 0)
	names.erase(names.begin()+i);

    if (names.size() == 0) 
      continue;
    else if (names.size() == 1)
      throw myexception()<<"In alignment constraint file: you must specify more than one sequence per group.";
    
    name_groups.push_back(names);
  }

  // parse the groups into mask_groups;
  vector< dynamic_bitset<> > mask_groups(name_groups.size());
  for(int i=0;i<mask_groups.size();i++) 
  {
    mask_groups[i].resize(TC.n_leaves());
    mask_groups[i].reset();

    for(int j=0;j<name_groups[i].size();j++) 
    {
      int index = find_index(TC.get_leaf_labels(), name_groups[i][j]);

      if (index == -1)
	throw myexception()<<"Reading alignment constraint file '"<<filename<<"':\n"
			   <<"   Can't find leaf taxon '"<<name_groups[i][j]<<"' in the tree.";
      else
	mask_groups[i][index] = true;
    }
  }

  // 1. check that each group is a fully resolved clade in the constraint tree (no polytomies)
  // 2. construct the list of constrained branches
  // FIXME - what if the user specifies nested clades?  Won't we get branches twice, then?
  //       - SOLUTION: use a bitmask.
  vector<int> branches;
  for(int i=0;i<mask_groups.size();i++) 
  {
    // find the branch that corresponds to a mask
    boost::dynamic_bitset<> mask(TC.n_leaves());
    int found = -1;
    for(int b=0;b<2*TC.n_branches() and found == -1;b++) 
    {
      mask = TC.partition(b);

      if (mask_groups[i] == mask)
	found = b;
    }

    // complain if we can't find it
    if (found == -1) 
      throw myexception()<<"Alignment constraint: clade '"
			 <<join(name_groups[i],' ')
			 <<"' not found in topology constraint tree.";
    
    // mark branch and child branches as constrained
    vector<const_branchview> b2 = branches_after_inclusive(TC,found); 
    for(int j=0;j<b2.size();j++) {
      if (b2[j].target().degree() > 3)
	throw myexception()<<"Alignment constraint: clade '"
			   <<join(name_groups[i],' ')
			   <<"' has a polytomy in the topology constraint tree.";
      branches.push_back(b2[j].undirected_name());
    }
  }


  return branches;
}


/// Initialize the default random number generator and return the seed
unsigned long init_rng_and_get_seed(const variables_map& args)
{
  unsigned long seed = 0;
  if (args.count("seed")) {
    seed = args["seed"].as<unsigned long>();
    myrand_init(seed);
  }
  else
    seed = myrand_init();

  return seed;
}

/// Replace negative or zero branch lengths with saner values.
void sanitize_branch_lengths(SequenceTree& T)
{
  double min_branch = 0.000001;
  for(int i=0;i<T.n_branches();i++)
  {
    if (not T.branch(i).has_length())
      T.branch(i).set_length(3.0/T.n_branches());
    if (T.branch(i).length() > 0)
      min_branch = std::min(min_branch,T.branch(i).length());
  }
  
  for(int i=0;i<T.n_branches();i++) {
    if (T.branch(i).length() == 0)
      T.branch(i).set_length(min_branch);
    if (T.branch(i).length() < 0)
      T.branch(i).set_length( - T.branch(i).length() );
  }
}

vector<double> get_geometric_heating_levels(const string& s)
{
  vector<double> levels;

  vector<string> parse = split(s,'/');

  if (parse.size() != 2) return levels;

  try
  {
    int n_levels = convertTo<int>(parse[1]);
    levels.resize(n_levels);
    
    parse = split(parse[0],'-');
    levels[0] = convertTo<double>(parse[0]);
    levels.back() = convertTo<double>(parse[1]);
    double factor = pow(levels.back()/levels[0], 1.0/(n_levels-1));
    
    for(int i=1;i<levels.size()-1;i++)
      levels[i] = levels[i-1]*factor;
    
    return levels;
  }
  catch (...)
  {
    throw myexception()<<"I don't understand beta level string '"<<s<<"'";
  }
}


void setup_heating(int proc_id, const variables_map& args, Parameters& P) 
{
  if (args.count("beta")) 
  {
    string beta_s = args["beta"].as<string>();

    vector<double> beta = get_geometric_heating_levels(beta_s);
    if (not beta.size())
      beta = split<double>(beta_s,',');

    P.all_betas = beta;

    if (proc_id >= beta.size())
      throw myexception()<<"not enough temperatures given: only got "<<beta.size()<<", wanted at least "<<proc_id+1;

    P.beta_index = proc_id;

    P.set_beta(beta[proc_id]);

    P.beta_series.push_back(beta[proc_id]);
  }

  if (args.count("dbeta")) {
    vector<string> deltas = split(args["dbeta"].as<string>(),',');
    for(int i=0;i<deltas.size();i++) {
      vector<double> D = split<double>(deltas[i],'*');
      if (D.size() != 2)
	throw myexception()<<"Couldn't parse beta increment '"<<deltas[i]<<"'";
      int D1 = (int)D[0];
      double D2 = D[1];
      for(int i=0;i<D1;i++) {
	double next = P.beta_series.back() + D2;
	next = std::max(0.0,next);
	P.beta_series.push_back(next);
      }
    }
  }
  for(double b:P.beta_series)
    std::cout<<b<<"\n";
}

void setup_partition_weights(const variables_map& args, Parameters& P) 
{
  if (args.count("partition-weights")) {

    string filename = args["partition-weights"].as<string>();

    const double n = 0.6;

    checked_ifstream partitions(filename,"partition weights file");
    string line;
    while(portable_getline(partitions,line)) {
      Partition p(P.T().get_leaf_labels(),line);
      portable_getline(partitions,line);
      double o = convertTo<double>(line);
      
      cerr<<p<<"      P = "<<o<<endl;
      if (o > n) {
	double w = n/(1-n)*(1-o)/o;
	log_double_t w2 = w;
	
	P.partitions.push_back(p);
	P.partition_weights.push_back(w2);
	
	cerr<<P.partitions.back()<<"      weight = "<<w<<endl;
      }
    }
  }
}

vector<expression_ref>
get_smodels(const module_loader& L,const variables_map& args, const vector<alignment>& A,
	    shared_items<string>& smodel_names_mapping)
{
  vector<expression_ref> smodels;
  for(int i=0;i<smodel_names_mapping.n_unique_items();i++) 
  {
    vector<alignment> alignments;
    for(int j=0;j<smodel_names_mapping.n_partitions_for_item(i);j++)
      alignments.push_back(A[smodel_names_mapping.partitions_for_item[i][j]]);

    if (smodel_names_mapping.unique(i) == "")
    {
      const alphabet& a = alignments[0].get_alphabet();
      smodel_names_mapping.unique(i) = default_markov_model(a);

      if (smodel_names_mapping.unique(i) == "")
	throw myexception()<<"You must specify a substitution model - there is no default substitution model for alphabet '"<<a.name<<"'";
    }

    expression_ref full_smodel = get_smodel(L,
						    args,
						    smodel_names_mapping.unique(i),
						    alignments);
    smodels.push_back(full_smodel);
    //    cout<<"SModel "<<i+1<<": prior = "<<log(smodels.back()->prior())<<"\n";
  }
  return smodels;
}

vector<expression_ref> 
get_imodels(const shared_items<string>& imodel_names_mapping, const SequenceTree& T)
{
  vector<expression_ref> imodels;
  for(int i=0;i<imodel_names_mapping.n_unique_items();i++) 
    imodels.push_back( get_imodel(imodel_names_mapping.unique(i), T) );
  return imodels;
}

#if defined(HAVE_SYS_RESOURCE_H)
string rlim_minutes(rlim_t val)
{
  if (val == RLIM_INFINITY)
    return "unlimited";
  else
    return convertToString<>(val/60) + " minutes";
}

void raise_cpu_limit(ostream& o)
{
  rlimit limits;

  getrlimit(RLIMIT_CPU,&limits);

  if (log_verbose) {
    o<<endl;
    o<<"OLD cpu time limits = "<<rlim_minutes(limits.rlim_cur)<<" / "<<rlim_minutes(limits.rlim_max)<<endl;
  }

  limits.rlim_cur = RLIM_INFINITY;

  setrlimit(RLIMIT_CPU,&limits);
  getrlimit(RLIMIT_CPU,&limits);

  if (log_verbose)
    o<<"NEW cpu time limits = "<<rlim_minutes(limits.rlim_cur)<<" / "<<rlim_minutes(limits.rlim_max)<<endl;
}
#else
void raise_cpu_limit(ostream& o) 
{
  o<<"Not checking CPU time limits..."<<endl;
}
#endif

void check_alignment_names(const alignment& A)
{
  const string forbidden = "();:\"'[]&,";

  for(int i=0;i<A.n_sequences();i++) {
    const string& name = A.seq(i).name;
    for(int j=0;j<name.size();j++)
      for(int c=0;c<forbidden.size();c++)
	for(int pos=0;pos<name.size();pos++)
	  if (name[pos] == forbidden[c])
	    throw myexception()<<"Sequence name '"<<name<<"' contains illegal character '"<<forbidden[c]<<"'";
  }
}

void check_alignment_values(const alignment& A,const string& filename)
{
  const alphabet& a = A.get_alphabet();

  for(int i=0;i<A.n_sequences();i++)
  {
    string name = A.seq(i).name;

    for(int j=0;j<A.length();j++) 
      if (A.unknown(j,i))
	throw myexception()<<"Alignment file '"<<filename<<"' has a '"<<a.unknown_letter<<"' in sequence '"<<name<<"'.\n (Please replace with gap character '"<<a.gap_letter<<"' or wildcard '"<<a.wildcard<<"'.)";
  }
}

chrono::system_clock::time_point start_time = chrono::system_clock::now();

string ctime(const chrono::system_clock::time_point& t)
{
  time_t t2 = chrono::system_clock::to_time_t(t);
  char* c = ctime(&t2);
  return c;
}

void show_ending_messages()
{
  using namespace chrono;

  system_clock::time_point end_time = system_clock::now();
  
  if (end_time - start_time > seconds(2)) 
  {
    cout<<endl;
    cout<<"start time: "<<ctime(start_time)<<endl;
    cout<<"  end time: "<<ctime(end_time)<<endl;
    cout<<"total (elapsed) time: "<<duration_string( duration_cast<seconds>(end_time-start_time) )<<endl;
    cout<<"total (CPU) time: "<<duration_string( duration_cast<seconds>(total_cpu_time()) )<<endl;
  }
  if (substitution::total_likelihood > 1) {
    cout<<endl;
    cout<<"total likelihood evals = "<<substitution::total_likelihood<<endl;
    cout<<"total calc_root_prob evals = "<<substitution::total_calc_root_prob<<endl;
    cout<<"total branches peeled = "<<substitution::total_peel_branches<<endl;
  }
}

void die_on_signal(int sig)
{
  // Throwing exceptions from signal handlers is not allowed.  Bummer.
  cout<<"received signal "<<sig<<".  Dying."<<endl;
  cerr<<"received signal "<<sig<<".  Dying."<<endl;

  show_ending_messages();

  exit(3);
}

void log_summary(ostream& out_cache, ostream& out_screen,ostream& out_both,
		 const shared_items<string>& imodels, const shared_items<string>& smodels,
		 const Parameters& P,const variables_map& args)
{
  //-------- Log some stuff -----------//
  vector<string> filenames = args["align"].as<vector<string> >();
  for(int i=0;i<filenames.size();i++) {
    out_cache<<"data"<<i+1<<" = "<<filenames[i]<<endl<<endl;
    out_cache<<"alphabet"<<i+1<<" = "<<P[i].get_alphabet().name<<endl<<endl;
  }

  for(int i=0;i<P.n_data_partitions();i++) {
    out_cache<<"smodel-index"<<i+1<<" = "<<P.get_smodel_index_for_partition(i)<<endl;
    out_cache<<"imodel-index"<<i+1<<" = "<<P.get_imodel_index_for_partition(i)<<endl;
  }
  out_cache<<endl;

  for(int i=0;i<P.n_smodels();i++)
    //    out_cache<<"subst model"<<i+1<<" = "<<P.SModel(i).name()<<endl<<endl;
    out_cache<<"subst model"<<i+1<<" = "<<smodels.unique(i)<<endl<<endl;

  for(int i=0;i<P.n_imodels();i++)
    out_cache<<"indel model"<<i+1<<" = "<<imodels.unique(i)<<endl<<endl;

  out_screen<<"\n";
  for(int i=0;i<P.n_data_partitions();i++) {
    int s_index = P.get_smodel_index_for_partition(i);
    //    out_screen<<"#"<<i+1<<": subst ~ "<<P.SModel(s_index).name()<<" ("<<s_index+1<<")    ";
    out_screen<<"#"<<i+1<<": subst ~ "<<smodels[i]<<" ("<<s_index+1<<")    ";

    int i_index = P.get_imodel_index_for_partition(i);
    string i_name = "none";
    if (i_index != -1)
      i_name = imodels[i];
    out_screen<<" indel ~ "<<i_name<<" ("<<i_index+1<<")"<<endl;;
  }
  out_screen<<"\n";

  out_both<<"Prior on branch lengths T[b]:\n";
  if (P.branch_prior_type == 0)
    out_both<<" T[b] ~ Exponential(Main.mu<i>)   [mean=Main.mu<i>, variance=Main.mu<i>^2]"<<endl;
  else if (P.branch_prior_type == 1)
    out_both<<" T[b] ~ Gamma(alpha=0.5, beta=2*Main.mu<i>)   [mean=Main.mu<i>, variance=2*Main.mu<i>^2]"<<endl;
  else if (P.branch_prior_type == 2)
  {
    out_both<<" T[b]/Tree length ~ Dirichlet(alpha=0.5)"<<endl;
    out_both<<" Tree length ~ Gamma(alpha=0.5, beta=2*Main.mu<i>)   [mean=Main.mu<i>, variance=2*Main,mu<i>^2]"<<endl;
  }
  if (P.n_data_partitions() > 1)
    out_both<<"(Each partition i has a separate 'Main.mu<i>' except where specified by --same-scale.)"<<endl;
}

void write_initial_alignments(const vector<alignment>& A, int proc_id, string dir_name)
{
  string base = dir_name + "/" + "C" + convertToString(proc_id+1);
  for(int i=0;i<A.size();i++)
  {
    checked_ofstream file(base+".P"+convertToString(i+1)+".initial.fasta");
    file<<A[i]<<endl;
  }
}

/// If the tree has any foreground branch attributes, then set the corresponding branch to foreground, here.
void set_foreground_branches(Parameters& P)
{
  const SequenceTree& T = P.T();

  if (T.find_undirected_branch_attribute_index_by_name("foreground") != -1)
  {
    int attribute_index = T.find_undirected_branch_attribute_index_by_name("foreground");

    for(int b=0;b<T.n_branches();b++)
    {
      boost::any value = T.branch(b).undirected_attribute(attribute_index);
      if (value.empty()) continue;

      int foreground_level = convertTo<int>( boost::any_cast<string>( value) );

      P.set_parameter_value( P.find_parameter("*Main.branchCat"+convertToString(b+1)), object_ref(Int(foreground_level)));
      std::cerr<<"Setting branch '"<<b<<"' to foreground level "<<foreground_level<<"\n";;
    }
  }
}

void write_branch_numbers(ostream& o, SequenceTree T)
{
  // Write out a tree 
  auto flags = o.flags();
  o.unsetf(std::ios::floatfield);
  for(int b=0;b<T.n_branches();b++)
    T.branch(b).set_length(b);
  o<<"branch numbers = "<<T<<"\n\n";
  o.flags(flags);
}

fs::path find_exe_path(const fs::path& argv0)
{
  /*
    Linux: readlink /proc/self/exe
    FreeBSD: sysctl CTL_KERN KERN_PROC KERN_PROC_PATHNAME -1

    Mac OS X: _NSGetExecutablePath() (man 3 dyld)
    Solaris: getexecname()
    BSD with procfs: readlink /proc/curproc/file
    Windows: GetModuleFileName() with hModule = NULL
  */

  /* For Mac
     char path[1024];
     uint32_t size = sizeof(path);
     if (_NSGetExecutablePath(path, &size) == 0)
       printf("executable path is %s\n", path);
     else
       printf("buffer too small; need size %u\n", size);
  */
  fs::path program_location;

  // This only works on Linux.
  if (fs::exists("/proc/self/exe"))
    program_location = "/proc/self/exe";
  // This only works on BSD with procfs.
  else if (fs::exists("/proc/curproc/file"))
    program_location = "/proc/curproc/file";
  // Try argv[0] - This *PROBABLY* works on windows.
  else if (fs::exists(argv0))
    program_location = argv0;
  // Search $PATH for argv[0]
  else if (not argv0.is_absolute() and getenv("PATH"))
  {
    string PATH = getenv("PATH");
    vector<string> paths = split(PATH,':');
    for(const string& prefix: paths)
    {
      fs::path p = prefix / argv0;
      if (fs::exists(p))
	program_location = p;
    }
  }
  program_location = canonical(program_location);
  program_location.remove_filename();

  return program_location;
}

struct restore 
{
  ostream* stream;
  std::streambuf * old;

  restore(ostream& s)
    : stream(&s), old( stream->rdbuf() )
  { }

  ~restore( ) 
  {
    stream->rdbuf( old );
  }
};

/* 
 * 1. Add a PRANK-like initial algorithm.
 * 2. Add some kind of constraint.
 * 3. Improve the method for proposing new SPR attachment sites.
 *  3a. Can we walk along the tree making characters present?
 */

module_loader setup_module_loader(variables_map& args, const string& filename)
{
  module_loader L;
  if (args.count("modules-path"))
    L.modules_path = split(args["modules-path"].as<string>(),':');
  else
  {
    fs::path modules = find_exe_path(filename);
    if (not modules.empty())
    {
      modules.remove_filename();
      modules = modules / "lib" / "bali-phy" / "modules";
    }
    if (modules.empty() or not fs::exists(modules/"Prelude.hs"))
      throw myexception()<<"No module paths are specified!.  Use --modules-path=<path> to specify the directory containing Prelude.hs.";

    L.modules_path = {modules.string()};
  }
      
  if (args.count("builtins-path"))
    L.builtins_path = split(args["builtins-path"].as<string>(),':');
  else
  {
    fs::path builtins = find_exe_path(filename);
    if (not builtins.empty())
    {
      builtins.remove_filename();
      builtins = builtins / "lib" / "bali-phy";
    }
    if (builtins.empty() or not fs::exists(builtins/("Prelude"+plugin_extension)))
      throw myexception()<<"No paths to find builtins are specified!.  Use --builtins-path=<path> to specify the directory containing 'Prelude"<<plugin_extension<<"'.";

    L.builtins_path = {builtins.string()};
  }
  return L;
}

string get_command_line(int argc, char* argv[])
{
  vector<string> args;
  for(int i=0;i<argc;i++)
    args.push_back(argv[i]);

  return join(args," ");
}

int main(int argc,char* argv[])
{ 
  int n_procs = 1;
  int proc_id = 0;

#ifdef HAVE_MPI
  mpi::environment env(argc, argv);
  mpi::communicator world;

  proc_id = world.rank();
  n_procs = world.size();
#endif

  restore restore_cout(cout);
  restore restore_cerr(cerr);
  restore restore_clog(clog);

  std::ios::sync_with_stdio(false);

  ostream out_screen(cout.rdbuf());
  ostream err_screen(cerr.rdbuf());

  std::ostringstream out_cache;
  std::ostringstream err_cache;

  vector<shared_ptr<ostream>> files;

  teebuf tee_out(out_screen.rdbuf(), out_cache.rdbuf());
  teebuf tee_err(err_screen.rdbuf(), err_cache.rdbuf());

  ostream out_both(&tee_out);
  ostream err_both(&tee_err);

  int retval=0;

  try {

#if defined(HAVE_FEENABLEEXCEPT) && !defined(NDEBUG)
    feenableexcept(FE_DIVBYZERO|FE_OVERFLOW|FE_INVALID);
    //    feclearexcept(FE_DIVBYZERO|FE_OVERFLOW|FE_INVALID);
#endif
#if defined(HAVE_CLEAREXCEPT) && defined(NDEBUG)
    feclearexcept(FE_DIVBYZERO|FE_OVERFLOW|FE_INVALID);
#endif
    fp_scale::initialize();

    //---------- Parse command line  ---------//
    variables_map args = parse_cmd_line(argc,argv);

    //------ Capture copy of 'cerr' output in 'err_cache' ------//
    if (not args.count("show-only"))
      cerr.rdbuf(err_both.rdbuf());

    //------ Print version info for show-only ------//
    if (args.count("show-only") and proc_id == 0)
    {
      cout<<"command: "<<get_command_line(argc,argv)<<endl<<endl;
      print_version_info(cout);
      cout<<endl;
    }

    //------------- Setup module loader -------------//
    module_loader L = setup_module_loader(args, argv[0]);

    //---------- Initialize random seed -----------//
    unsigned long seed = init_rng_and_get_seed(args);
    
    out_cache<<"random seed = "<<seed<<endl<<endl;

    if (args["subA-index"].as<string>() == "leaf")
      use_internal_index = false;

    //------ Determine number of partitions ------//
    vector<string> filenames = args["align"].as<vector<string> >();
    const int n_partitions = filenames.size();

    //-------------Choose an indel model--------------//
    //FIXME - make a shared_items-like class that also holds the items to we can put the whole state in one object.
    //FIXME - make bali-phy.C a more focussed and readable file - remove setup junk to other places? (where?)
    vector<int> imodel_mapping(n_partitions, -1);
    shared_items<string> imodel_names_mapping(vector<string>(),imodel_mapping);

    if (args.count("traditional")) {
      if (args.count("imodel"))
	throw myexception()<<"Error: you specified both --imodel <arg> and --traditional";
    }
    else {
      imodel_names_mapping = get_mapping(args, "imodel", n_partitions);

      for(int i=0;i<imodel_names_mapping.n_unique_items();i++)
	if (imodel_names_mapping.unique(i) == "")
	  imodel_names_mapping.unique(i) = "RS07";
      
      imodel_mapping = imodel_names_mapping.item_for_partition;
    }

    //----------- Load alignments and tree ---------//
    vector<alignment> A;
    SequenceTree T;
    // FIXME - do I want to allow/remove internal node sequences here?
    vector<bool> internal_sequences(n_partitions);
    for(int i=0;i<internal_sequences.size();i++)
      internal_sequences[i] = (imodel_mapping[i] != -1);

    //       - and only if there is an indel model?
    if (args.count("tree"))
      load_As_and_T(args,A,T,internal_sequences);
    else
      load_As_and_random_T(args,A,T,internal_sequences);

    for(int i=0;i<A.size();i++) {
      check_alignment_names(A[i]);
      check_alignment_values(A[i],filenames[i]);
    }

    //--------- Handle branch lengths <= 0 --------//
    sanitize_branch_lengths(T);

    //--------- Do we have enough sequences? ------//
    //    if (T.n_leaves() < 3)
    //      throw myexception()<<"At least 3 sequences must be provided - you provided only "<<T.n_leaves()<<".";

    //--------- Set up the substitution model --------//

    // FIXME - change to return a (model, standardized name) pair.
    vector<expression_ref> full_imodels = get_imodels(imodel_names_mapping, T);

    //--------- Set up the substitution model --------//
    shared_items<string> smodel_names_mapping = get_mapping(args, "smodel", n_partitions);
    
    vector<int> smodel_mapping = smodel_names_mapping.item_for_partition;

    // FIXME - change to return a (model, standardized name) pair.
    vector<expression_ref> full_smodels = get_smodels(L,args,A,smodel_names_mapping);

    //-------------- Which partitions share a scale? -----------//
    shared_items<string> scale_names_mapping = get_mapping(args, "same-scale", A.size());

    vector<int> scale_mapping = scale_names_mapping.item_for_partition;

    //-------------Create the Parameters object--------------//
    Parameters P(L, A, T, full_smodels, smodel_mapping, full_imodels, imodel_mapping, scale_mapping);
    P.set_args(trailing_args(argc,argv,trailing_args_separator));

    // Write out a tree with branch numbers as branch lengths
    write_branch_numbers(out_cache, T);

    // If the tree has any foreground branch attributes, then set the corresponding branch to foreground, here.
    set_foreground_branches(P);

    //------------- Set the branch prior type --------------//
    string branch_prior = args["branch-prior"].as<string>();
    if (branch_prior == "Exponential")  
      P.branch_prior_type = 0;
    else if (branch_prior == "Gamma") 
      P.branch_prior_type = 1;
    else if (branch_prior == "Dirichlet") 
      P.branch_prior_type = 2;
    else
      throw myexception()<<"I don't understand --branch-prior argument '"<<branch_prior<<"'.\n  Only 'Exponential' and 'Gamma' are allowed.";

    //------------- Parse the Hierarchical Model description -----------//
    if (args.count("model"))
    {
      const string filename = args["model"].as<string>();
      add_model(P,filename);
    }
      
    set_initial_parameter_values(P,args);

    set_key_values(P,args);

    //-------------------- Log model -------------------------//
    log_summary(out_cache,out_screen,out_both,imodel_names_mapping,smodel_names_mapping,P,args);

    //----------------- Tree-based constraints ----------------//
    if (args.count("t-constraint"))
      P.TC = cow_ptr<SequenceTree>(load_constraint_tree(args["t-constraint"].as<string>(), T.get_leaf_labels()));

    if (args.count("a-constraint"))
      P.AC = load_alignment_branch_constraints(args["a-constraint"].as<string>(),*P.TC);

    if (not extends(T, *P.TC))
      throw myexception()<<"Initial tree violates topology constraints.";

    //---------- Alignment constraint (horizontal) -----------//
    vector<string> ac_filenames(P.n_data_partitions(),"");
    if (args.count("align-constraint")) 
    {
      ac_filenames = split(args["align-constraint"].as<string>(),':');

      if (ac_filenames.size() != P.n_data_partitions())
	throw myexception()<<"Need "<<P.n_data_partitions()<<" alignment constraints (possibly empty) separated by colons, but got "<<ac_filenames.size();
    }

    for(int i=0;i<P.n_data_partitions();i++)
      P[i].alignment_constraint = load_alignment_constraint(ac_filenames[i],T);

    //------------------- Handle heating ---------------------//
    setup_heating(proc_id,args,P);

    // read and store partitions and weights, if any.
    setup_partition_weights(args,P);

    //---------------Do something------------------//
    if (args.count("show-only"))
    {
      // FIXME ! How do we print the tree to stdout?
      print_stats(cout,P);
      // Separate the tree printer from the file writer?
    }
    else {
#if !defined(_MSC_VER) && !defined(__MINGW32__)
      raise_cpu_limit(err_both);

      signal(SIGHUP,SIG_IGN);
      signal(SIGXCPU,SIG_IGN);

      
      struct sigaction sa_old;
      struct sigaction sa_new;
      sa_new.sa_handler = &die_on_signal;

      sigaction(SIGINT,NULL,&sa_old);
      if (sa_old.sa_handler != SIG_IGN)
	sigaction(SIGINT,&sa_new,NULL);

      sigaction(SIGTERM,NULL,&sa_old);
      if (sa_old.sa_handler != SIG_IGN)
	sigaction(SIGTERM,&sa_new,NULL);

#endif

      long int max_iterations = args["iterations"].as<long int>();

      //---------- Open output files -----------//
      vector<owned_ptr<MCMC::Logger> > loggers;

      string dir_name="";
      if (not args.count("show-only")) {
#ifdef HAVE_MPI
	if (not proc_id) {
	  dir_name = init_dir(args);
	  
	  for(int dest=1;dest<n_procs;dest++) 
	    world.send(dest, 0, dir_name);
	}
	else
	  world.recv(0, 0, dir_name);

	// cerr<<"Proc "<<proc_id<<": dirname = "<<dir_name<<endl;
#else
	dir_name = init_dir(args);
#endif
	files = init_files(proc_id, dir_name, argc, argv);
	{
	  vector<string> Rao_Blackwellize;
	  if (args.count("Rao-Blackwellize"))
	      Rao_Blackwellize = split(args["Rao-Blackwellize"].as<string>(),',');
	  loggers = construct_loggers(P,Rao_Blackwellize,proc_id,dir_name);
	}
	write_initial_alignments(A,proc_id, dir_name);
      }
      else {
	files.push_back(shared_ptr<ostream>(new ostream(cout.rdbuf())));
	files.push_back(shared_ptr<ostream>(new ostream(cerr.rdbuf())));
      }

      //------ Redirect output to files -------//
      *files[0]<<out_cache.str(); out_cache.str("");
      *files[1]<<err_cache.str(); err_cache.str("");

      tee_out.setbuf2(files[0]->rdbuf());
      tee_err.setbuf2(files[1]->rdbuf());

      cout.flush() ; cout.rdbuf(files[0]->rdbuf());
      cerr.flush() ; cerr.rdbuf(files[1]->rdbuf());
      clog.flush() ; clog.rdbuf(files[1]->rdbuf());

      //------ Redirect output to files -------//
      owned_ptr<Probability_Model> Ptr(P);

      // Force the creation of parameters
      for(int i=0;i<Ptr->n_parameters();i++)
	Ptr->parameter_is_modifiable(i);

      avoid_zero_likelihood(Ptr, *files[0], out_both);

      do_pre_burnin(args, Ptr, *files[0], out_both);

      out_screen<<"\nBeginning "<<max_iterations<<" iterations of MCMC computations."<<endl;
      out_screen<<"   - Future screen output sent to '"<<dir_name<<"/C1.out'"<<endl;
      out_screen<<"   - Future debugging output sent to '"<<dir_name<<"/C1.err'"<<endl;
      out_screen<<"   - Sampled trees logged to '"<<dir_name<<"/C1.trees'"<<endl;
      out_screen<<"   - Sampled alignments logged to '"<<dir_name<<"/C1.P<partition>.fastas'"<<endl;
      out_screen<<"   - Sampled numerical parameters logged to '"<<dir_name<<"/C1.p'"<<endl;
      out_screen<<endl;
      out_screen<<"You can examine 'C1.p' using BAli-Phy tool statreport (command-line)"<<endl;
      out_screen<<"  or the BEAST program Tracer (graphical)."<<endl;
      out_screen<<"See the manual for further information."<<endl;

      //-------- Start the MCMC  -----------//
      do_sampling(args,Ptr ,max_iterations, *files[0], loggers);

      // Close all the streams, and write a notification that we finished all the iterations.
      // close_files(files);
    }
  }
  catch (std::bad_alloc&) 
  {
    // 1. If we haven't yet moved screen output to a file, then write cached screen output.
    out_screen<<out_cache.str(); out_screen.flush();
    err_screen<<err_cache.str(); err_screen.flush();

    // 2. Now, write message to either (screen+cache) or (screen+file), and flush.
    err_both<<"Doh!  Some kind of memory problem?\n"<<endl;
    // 3. Write memory report to either (screen) or (screen+cache) or (screen+file)
    report_mem();
    retval=2;
  }
  catch (std::exception& e) 
  {
    // 1. If we haven't yet moved screen output to a file, then write cached screen output.
    out_screen<<out_cache.str(); out_screen.flush();
    err_screen<<err_cache.str(); err_screen.flush();

    // 2. Now, write message to either (screen+cache) or (screen+file), and flush.
    if (n_procs > 1)
      err_both<<"bali-phy: Error["<<proc_id<<"]! "<<e.what()<<endl;
    else
      err_both<<"bali-phy: Error! "<<e.what()<<endl;

    retval=1;
  }

  show_ending_messages();

  out_both.flush();
  err_both.flush();
  return retval;
}
