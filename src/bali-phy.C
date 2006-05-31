/* Version 2: based on operating on multiple alignments */

#include <cmath>
#include <iostream>
#include <fstream>
#include <valarray>
#include <new>
#include <signal.h>

#include <boost/program_options.hpp>
#include <boost/filesystem/operations.hpp>

#include "myexception.H"
#include "mytypes.H"
#include "sequencetree.H"
#include "alignment.H"
#include "rng.H"
#include "sample.H"
#include "parameters.H"
#include "mcmc.H"
#include "likelihood.H"
#include "util.H"
#include "setup.H"
#include "alignment-constraint.H"
#include "alignment-util.H"
#include "substitution-index.H"
#include "monitor.H"
#include "pow2.H"
#include "proposals.H"

namespace fs = boost::filesystem;

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::clog;
using std::endl;
using std::ostream;

using std::valarray;

bool has_parameter(const Model& M, const string& name)
{
  for(int i=0;i<M.parameters().size();i++)
    if (M.parameter_name(i) == name)
      return true;
  return false;
}

vector<string> get_parameters(const Model& M,const string& prefix)
{
  vector<string> names;
  for(int i=0;i<M.parameters().size();i++)
  {
    string s = M.parameter_name(i);
    if (s.size() > prefix.size() and s.substr(0,prefix.size()) == prefix)
      names.push_back(s);
  }
  return names;
}

void add_MH_move(Parameters& P,const Proposal_Fn& p, const string& name, const string& pname,double sigma, MCMC::MoveAll& M)
{
  if (name.size() and name[name.size()-1] == '*')
  {
    vector<string> names = get_parameters(P,name.substr(0,name.size()-1));
    vector<string> names2;
    for(int i=0;i<names.size();i++)
      if (not P.fixed(find_parameter(P,names[i])))
	names2.push_back(names[i]);

    if (names2.empty()) return;
    set_if_undef(P.keys, pname, sigma);
    Proposal2 move_mu(p, names2, vector<string>(1,pname), P);

    M.add(1, MCMC::MH_Move(move_mu,string("sample_")+name));
  }
  else if (has_parameter(P,name) and not P.fixed(find_parameter(P,name))) {
    set_if_undef(P.keys, pname, sigma);
    Proposal2 move_mu(p, name, vector<string>(1,pname), P);
    M.add(1, MCMC::MH_Move(move_mu,string("sample_")+name));
  }
}

void do_sampling(const variables_map& args,alignment& A,Parameters& P,long int max_iterations,
		 ostream& s_out,ostream& s_trees, ostream& s_parameters,ostream& s_map)
{
  // args for branch-based stuff
  vector<int> branches(P.T.n_branches());
  for(int i=0;i<branches.size();i++)
    branches[i] = i;

  // args for branch-based stuff
  vector<int> internal_nodes;
  for(int i=P.T.n_leaves();i<P.T.n_nodes();i++)
    internal_nodes.push_back(i);

  // args for branch-based stuff
  vector<int> internal_branches;
  for(int i=P.T.n_leaves();i<P.T.n_branches();i++)
    internal_branches.push_back(i);

  using namespace MCMC;

  //----------------------- alignment -------------------------//
  MoveAll alignment_moves("alignment");

  //--------------- alignment::alignment_branch ---------------//
  MoveEach alignment_branch_moves("alignment_branch_master");
  alignment_branch_moves.add(1.0,
			     MoveArgSingle("sample_alignments","alignment:alignment_branch",
					   sample_alignments_one,
					   branches)
			     );
  if (P.T.n_leaves() >2) {
    alignment_branch_moves.add(0.15,MoveArgSingle("sample_tri","alignment:alignment_branch:nodes",
						 sample_tri_one,
						 branches)
			       );
    alignment_branch_moves.add(0.1,MoveArgSingle("sample_tri_branch","alignment:nodes:length",
						 sample_tri_branch_one,
						 branches)
			       ,false);
    alignment_branch_moves.add(0.1,MoveArgSingle("sample_tri_branch_aligned","alignment:nodes:length",
						 sample_tri_branch_type_one,
						 branches)
			       ,false);
  }
  alignment_moves.add(1, alignment_branch_moves, false);
  alignment_moves.add(1, SingleMove(walk_tree_sample_alignments, "walk_tree_sample_alignments","alignment:alignment_branch:nodes") );

  //---------- alignment::nodes_master (nodes_moves) ----------//
  MoveEach nodes_moves("nodes_master:nodes");
  if (P.T.n_leaves() >= 3)
    nodes_moves.add(10,MoveArgSingle("sample_node","alignment:nodes",
				   sample_node_move,
				   internal_nodes)
		   );
  if (P.T.n_leaves() >= 4)
    nodes_moves.add(1,MoveArgSingle("sample_two_nodes","alignment:nodes",
				   sample_two_nodes_move,
				   internal_nodes)
		   );

  alignment_moves.add(2, nodes_moves);

  //-------------------- tree (tree_moves) --------------------//
  MoveAll tree_moves("tree");
  MoveAll topology_move("topology");
  MoveEach NNI_move("NNI");
  MoveOne SPR_move("SPR");

  if (P.has_IModel())
    NNI_move.add(1,MoveArgSingle("three_way_NNI","alignment:nodes:topology",
				 three_way_topology_sample,
				 internal_branches)
		 );
  else
    NNI_move.add(1,MoveArgSingle("three_way_NNI","topology",
				 three_way_topology_sample,
				 internal_branches)
		 );

  NNI_move.add(1,MoveArgSingle("two_way_NNI","alignment:nodes:topology",
				    two_way_topology_sample,
				    internal_branches)
		    ,false
		    );

  //FIXME - doesn't yet deal with gaps=star
  if (P.has_IModel())
    NNI_move.add(0.025,MoveArgSingle("three_way_NNI_and_A","alignment:alignment_branch:nodes:topology",
				   three_way_topology_and_alignment_sample,
				     internal_branches)
		 ,false
		 );


  if (P.has_IModel()) {
    SPR_move.add(1,SingleMove(sample_SPR_flat,"SPR_and_A_flat","topology:lengths:nodes:alignment:alignment_branch"));
    SPR_move.add(1,SingleMove(sample_SPR_nodes,"SPR_and_A_nodes","topology:lengths:nodes:alignment:alignment_branch"));
  }
  else {
    SPR_move.add(1,SingleMove(sample_SPR_flat,"SPR_flat","topology:lengths"));
    SPR_move.add(1,SingleMove(sample_SPR_nodes,"SPR_and_A_nodes","topology:lengths"));
  }

  topology_move.add(1,NNI_move,false);
  topology_move.add(1,SPR_move);
  if (P.T.n_leaves() >3 and P.SModel().full_tree)
    tree_moves.add(1,topology_move);
  
  //-------------- tree::lengths (length_moves) -------------//
  MoveAll length_moves("lengths");
  MoveEach length_moves1("lengths1");

  length_moves1.add(1,MoveArgSingle("change_branch_length","lengths",
				   change_branch_length_move,
				   branches)
		   );
  length_moves1.add(1,MoveArgSingle("change_branch_length_multi","lengths",
				   change_branch_length_multi_move,
				   branches)
		   );
  if (P.SModel().full_tree)
    length_moves1.add(0.01,MoveArgSingle("change_branch_length_and_T","lengths:nodes:topology",
					change_branch_length_and_T,
					internal_branches)
		      );
  length_moves.add(1,length_moves1,false);
  length_moves.add(1,SingleMove(scale_branch_lengths_and_mean,
				"scale_branches_and_mean","lengths:mean")
		   );

  tree_moves.add(1,length_moves);
  tree_moves.add(1,SingleMove(sample_NNI_and_branch_lengths,"NNI_and_lengths","topology:lengths"));

  //------------- parameters (parameters_moves) --------------//
  MoveAll parameter_moves("parameters");

  add_MH_move(P, log_scaled(shift_gaussian),    "mu",             "mu_scale_sigma",     0.6,  parameter_moves);
  add_MH_move(P, log_scaled(shift_gaussian),    "HKY::kappa",     "kappa_scale_sigma",  0.3,  parameter_moves);
  add_MH_move(P, log_scaled(shift_gaussian),    "TN::kappa(pur)", "kappa_scale_sigma",  0.3,  parameter_moves);
  add_MH_move(P, log_scaled(shift_gaussian),    "TN::kappa(pyr)", "kappa_scale_sigma",  0.3,  parameter_moves);
  add_MH_move(P, log_scaled(shift_gaussian),    "YangM0::omega",  "omega_scale_sigma",  0.3,  parameter_moves);
  add_MH_move(P, log_scaled(more_than(0,shift_gaussian)),
	                                        "YangM2::omega",  "omega_scale_sigma",  0.3,  parameter_moves);
  add_MH_move(P, between(0,1,shift_gaussian),   "INV::p",         "INV::p_shift_sigma", 0.03, parameter_moves);
  add_MH_move(P, between(0,1,shift_gaussian),   "c",              "c_shift_sigma",      0.1,  parameter_moves);
  add_MH_move(P, between(0,1,shift_gaussian),   "f",              "f_shift_sigma",      0.1,  parameter_moves);
  add_MH_move(P, between(0,1,shift_gaussian),   "g",              "g_shift_sigma",      0.1,  parameter_moves);
  add_MH_move(P, between(0,1,shift_gaussian),   "h",              "h_shift_sigma",      0.1,  parameter_moves);
  add_MH_move(P, log_scaled(shift_gaussian),    "beta::mu",       "mu_scale_sigma",     0.2,  parameter_moves);
  add_MH_move(P, log_scaled(more_than(-5.7,shift_gaussian)),
	                                        "gamma::sigma/mu","gamma::sigma_scale_sigma",  0.25, parameter_moves);
  add_MH_move(P, log_scaled(more_than(-4.0,shift_gaussian)),
	                                        "beta::sigma/mu", "beta::sigma_scale_sigma",  0.25, parameter_moves);
  add_MH_move(P, log_scaled(shift_gaussian),
	                                        "log-normal::sigma/mu","log-normal::sigma_scale_sigma",  0.25, parameter_moves);

  if (P.has_IModel()) {
    add_MH_move(P, shift_delta,                 "delta",       "delta_shift_sigma",     0.35, parameter_moves);
    add_MH_move(P, less_than(0,shift_gaussian), "lambda",      "lambda_shift_sigma",    0.35, parameter_moves);
    add_MH_move(P, shift_epsilon,               "epsilon",     "epsilon_shift_sigma",   0.15, parameter_moves);
    add_MH_move(P, between(0,1,shift_gaussian), "invariant",   "invariant_shift_sigma", 0.15, parameter_moves);
  }
  
  set_if_undef(P.keys,"pi_dirichlet_N",1.0);
  P.keys["pi_dirichlet_N"] *= A.length();
  add_MH_move(P, dirichlet_proposal,    "pi*",    "pi_dirichlet_N",      1,  parameter_moves);

  set_if_undef(P.keys,"GTR_dirichlet_N",1.0);
  P.keys["GTR_dirichlet_N"] *= 100;
  add_MH_move(P, dirichlet_proposal,    "GTR::*", "GTR_dirichlet_N",     1,  parameter_moves);

  set_if_undef(P.keys,"v_dirichlet_N",1.0);
  P.keys["v_dirichlet_N"] *= A.length();
  add_MH_move(P, dirichlet_proposal,    "v*", "v_dirichlet_N",     1,  parameter_moves);

  set_if_undef(P.keys,"YangM2::f_dirichlet_N",1.0);
  P.keys["YangM2::f_dirichlet_N"] *= 10;
  add_MH_move(P, dirichlet_proposal,    "YangM2::f*", "YangM2::f_dirichlet_N",     1,  parameter_moves);

  set_if_undef(P.keys,"YangM3::f_dirichlet_N",1.0);
  P.keys["YangM3::f_dirichlet_N"] *= 10;
  add_MH_move(P, dirichlet_proposal,    "YangM3::f*", "YangM3::f_dirichlet_N",     1,  parameter_moves);

  set_if_undef(P.keys,"multi:p_dirichlet_N",1.0);
  P.keys["multi:p_dirichlet_N"] *= 10;
  add_MH_move(P, dirichlet_proposal,    "multi:p*", "multi:p_dirichlet_N",     1,  parameter_moves);

  for(int i=0;;i++) {
    string name = "YangM3::omega" + convertToString(i);
    if (not has_parameter(P,name))
      break;

    Proposal2 m(log_scaled(shift_gaussian), name, vector<string>(1,"omega_scale_sigma"), P);
    parameter_moves.add(1, MCMC::MH_Move(m,"sample_YangM3::omega"));
  }

  set_if_undef(P.keys,"Mixture::p_dirichlet_N",1.0);
  P.keys["Mixture::p_dirichlet_N"] *= 10*10;
  add_MH_move(P, dirichlet_proposal,    "Mixture::p*", "Mixture::p_dirichlet_N",     1,  parameter_moves);

  int subsample = args["subsample"].as<int>();

  // full sampler
  Sampler sampler("sampler");
  if (P.has_IModel())
    sampler.add(1,alignment_moves);
  sampler.add(2,tree_moves);
  sampler.add(4 + P.T.n_branches()/4.0,parameter_moves);

  vector<string> disable;
  vector<string> enable;
  if (args.count("disable"))
    disable = split(args["disable"].as<string>(),',');
  if (args.count("enable"))
    enable = split(args["enable"].as<string>(),',');
  
  for(int i=0;i<disable.size();i++)
    sampler.disable(disable[i]);
  
  for(int i=0;i<enable.size();i++)
    sampler.enable(enable[i]);
  
  sampler.show_enabled(s_out);
  s_out<<"\n";

  if (P.alignment_constraint.size1() > 0)
    std::cerr<<"Using "<<P.alignment_constraint.size1()<<" constraints.\n";

  valarray<bool> s2 = constraint_satisfied(P.alignment_constraint,A);
  valarray<bool> s1(false,s2.size());
  report_constraints(s1,s2);

  sampler.go(A,P,subsample,max_iterations,s_out,s_trees,s_parameters,s_map);
}

#ifdef DEBUG_MEMORY
void * operator new(size_t sz) throw(std::bad_alloc) {
  printf("new (_main.cpp) called, sz = %d\n",sz);
  return malloc(sz); 
}

void operator delete(void * p) throw() {
  printf("delete (_main.cpp) called, content = %d\n",(*(int*)p));
  free(p); 
}
#endif

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description general("General options");
  general.add_options()
    ("help", "produce help message")
    ("version", "print version information")
    ("config", value<string>(),"config file to read")
    ("show-only","analyze the initial values and exit")
    ("seed", value<unsigned long>(),"random seed")
    ("data-dir", value<string>()->default_value("Data"),"location of the Data/ directory")
    ("name", value<string>(),"name for the analysis, instead of the alignment filename.")
    ("align-constraint",value<string>(),"file with alignment constraints")
    ("traditional","Fix the alignment and don't model indels")
    ("letters",value<string>()->default_value("full_tree"),"if set to 'star', then use a star tree for substitution")
    ;
  
  options_description mcmc("MCMC options");
  mcmc.add_options()
    ("iterations",value<long int>()->default_value(100000),"the number of iterations to run")
    ("subsample",value<int>()->default_value(1),"factor by which to subsample")
    ("beta",value<string>(),"MCMCMC temperature")
    ("dbeta",value<string>(),"MCMCMC temperature changes")
    ("enable",value<string>(),"comma-separated list of kernels to enable")
    ("disable",value<string>(),"comma-separated list of kernels to disable")
    ("partition-weights",value<string>(),"file containing tree with partition weights")
    ;
    
  options_description parameters("Parameter options");
  parameters.add_options()
    ("align", value<string>(),"file with sequences and initial alignment")
    ("randomize-alignment","randomly realign the sequences before use.")
    ("internal",value<string>(),"if set to '+', then make all internal node entries wildcards")
    ("tree",value<string>(),"file with initial tree")
    ("set",value<vector<string> >()->composing(),"set parameter=<value>")
    ("fix",value<vector<string> >()->composing(),"fix parameter[=<value>]")
    ("unfix",value<vector<string> >()->composing(),"un-fix parameter[=<value>]")
    ("frequencies",value<string>(),"initial frequencies: 'uniform','nucleotides', or a comma-separated vector.") 
    ;

  options_description model("Model options");
  model.add_options()
    ("alphabet",value<string>(),"specify the alphabet: DNA, RNA, Amino Acids, Amino Acids + stop, Triplets, Codons, or Codons + stop")
    ("genetic-code",value<string>()->default_value("standard-code.txt"),"specify alternate genetic code file in data directory")
    ("smodel",value<string>(),"substitution model")
    ("imodel",value<string>()->default_value("fragment-based+T"),"indel model: simple, fragment-based, or fragment-based+T")
    ;
  options_description all("All options");
  all.add(general).add(mcmc).add(parameters).add(model);

  // positional options
  positional_options_description p;
  p.add("align", 1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).options(all).positional(p).run(), args);
  notify(args);    

  if (args.count("version")) {
    cout<<"VERSION: 1.9.8\nBUILD: "<<__DATE__<<"\n";
    exit(0);
  }

  if (args.count("help")) {
    cout<<"Usage: bali-phy <sequence-file> [OPTIONS]\n";
    cout<<all<<"\n";
    exit(0);
  }

  if (args.count("config")) 
  {
    string filename = args["config"].as<string>();
    ifstream file(filename.c_str());
    if (not file)
      throw myexception()<<"Con't load config file '"<<filename<<"'";

    store(parse_config_file(file, all), args);
    file.close();
    notify(args);
  }

  if (getenv("HOME")) {
    string home_dir = getenv("HOME");
    if (not fs::exists(home_dir))
      cerr<<"Home directory '"<<home_dir<<"' does not exist!"<<endl;
    else if (not fs::is_directory(home_dir))
      cerr<<"Home directory '"<<home_dir<<"' is not a directory!"<<endl;
    else {
      string filename = home_dir + "/.bali-phy";

      if (fs::exists(filename)) {
	cout<<"Reading ~/.bali-phy ...";
	ifstream file(filename.c_str());
	if (not file)
	  throw myexception()<<"Con't load config file '"<<filename<<"'";
      
	store(parse_config_file(file, all), args);
	file.close();
	notify(args);
	cout<<" done."<<endl;
      }
    }
  }
  else
    cerr<<"Environment variable HOME not set!"<<endl;


  if (not args.count("align"))
    throw myexception()<<"No sequence file given.";

  return args;
}

//FIXME - how to record that the user said '--fix A' ?

void set_parameters(Parameters& P, const variables_map& args) 
{
  //-------------- Specify fixed parameters ----------------//
  vector<string>   fix;
  if (args.count("fix"))
    fix = args["fix"].as<vector<string> >();

  vector<string> unfix;
  if (args.count("unfix"))
    unfix = args["unfix"].as<vector<string> >();

  vector<string> doset;
  if (args.count("set"))
    doset = args["set"].as<vector<string> >();

  // separate out 'set' operations from 'fixed'
  for(int i=0;i<fix.size();i++) {
    vector<string> parse = split(fix[i],'=');
    
    if (parse.size() > 1) {
      doset.push_back(fix[i]);
      fix[i] = parse[0];
    }
  }

  // separate out 'set' operations from 'unfixed'
  for(int i=0;i<unfix.size();i++) {
    vector<string> parse = split(unfix[i],'=');
    
    if (parse.size() > 1) {
      doset.push_back(unfix[i]);
      unfix[i] = parse[0];
    }
  }

  // fix parameters
  for(int i=0;i<fix.size();i++) {
    int p=-1;
    if (p=find_parameter(P,fix[i]),p!=-1)
      P.fixed(p,true);
    else
      throw myexception()<<"Can't find parameter '"<<fix[i]<<"' to unfix.";
  }

  // unfix parameters
  for(int i=0;i<unfix.size();i++) {
    int p=-1;
    if (p=find_parameter(P,unfix[i]),p!=-1)
      P.fixed(p,false);
    else
      throw myexception()<<"Can't find parameter '"<<unfix[i]<<"' to unfix.";
  }

  // set parameters
  for(int i=0;i<doset.size();i++) {
    //parse
    vector<string> parse = split(doset[i],'=');
    if (parse.size() != 2)
      throw myexception()<<"Ill-formed initial condition '"<<doset[i]<<"'.";

    string name = parse[0];
    double value = convertTo<double>(parse[1]);

    int p=-1;
    if (p=find_parameter(P,name),p!=-1)
      P.parameter(p,value);
    else
      P.keys[name] = value;
  }
}

void delete_files(vector<string>& filenames,vector<ofstream*>& files)
{
  for(int i=0;i<files.size();i++) {
    files[i]->close();
    fs::remove(filenames[i]);
  }
  files.clear();
  filenames.clear();
}

vector<ofstream*> open_files(const string& name, vector<string>& names)
{
  vector<ofstream*> files;
  vector<string> filenames;

  bool success=false;
  for(int i=0;not success;i++) 
  {
    success = true;
    for(int j=0;j<names.size();j++) 
    {
      string filename = name + convertToString(i+1)+"."+names[j];

      if (fs::exists(filename)) {
	delete_files(filenames,files);
	success = false;
	break;
      }
      else {
	files.push_back(new ofstream(filename.c_str()));
	filenames.push_back(filename);
      }
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
      fs::create_directory(dirname);
      return dirname;
    }
  }
}


string hostname()
{
  string hostname="";
  char temp[256];
  if (not gethostname(temp,256))
    hostname = temp;
  return hostname;
}

int main(int argc,char* argv[]) 
{ 
  try {

    fp_scale::initialize();
    std::ios::sync_with_stdio(false);
    fs::path::default_name_check(fs::portable_posix_name);

    //---------- Parse command line  ---------//
    variables_map args = parse_cmd_line(argc,argv);

    //---------- Open output files -----------//
    vector<string> filenames;
    ostream* s_out = NULL;
    ostream* s_err = NULL;
    ostream* s_trees = NULL;
    ostream* s_parameters = NULL;
    ostream* s_map = NULL;

    if (args.count("show-only")){
      s_out = &cout;
    }
    else {

      string name = fs::path( args["align"].as<string>() ).leaf();
      if (args.count("name"))
	name = args["name"].as<string>();

      string dirname = open_dir(name);
      cerr<<"Created directory '"<<dirname<<"' for output files."<<endl;

      filenames.push_back("out");
      filenames.push_back("err");
      filenames.push_back("trees");
      filenames.push_back("p");
      filenames.push_back("MAP");

      vector<ofstream*> files = open_files(dirname+"/",filenames);
      
      s_out = files[0];
      s_err = files[1];
      s_trees = files[2];
      s_parameters = files[3];
      s_map = files[4];

      cerr.rdbuf(s_err->rdbuf());
      clog.rdbuf(s_err->rdbuf());
      cerr.flush();
      clog.flush();

      *s_out<<"command: ";
      for(int i=0;i<argc;i++) {
	*s_out<<argv[i];
	if (i != argc-1) *s_out<<" ";
      }
      *s_out<<endl;
      *s_out<<"directory: "<<fs::initial_path().string()<<endl;
      if (getenv("JOB_ID"))
	*s_out<<"JOB_ID: "<<getenv("JOB_ID")<<endl;
      *s_out<<"hostname: "<<hostname()<<endl;
      *s_out<<"PID: "<<getpid()<<endl;
      *s_out<<endl;
    }
    s_out->precision(10);
    cerr.precision(10);


    //---------- Initialize random seed -----------//
    unsigned long seed = 0;
    if (args.count("seed")) {
      seed = args["seed"].as<unsigned long>();
      myrand_init(seed);
    }
    else
      seed = myrand_init();
    (*s_out)<<"random seed = "<<seed<<endl<<endl;
    
    //---------- Determine Data dir ---------------//
    {
      fs::path data_dir = args["data-dir"].as<string>();
      if (not fs::exists(data_dir)) {
	(*s_err)<<"Warning: BAli-Phy data directory '"<<data_dir.string()<<"' does not exist!"<<endl;
	(*s_err)<<"         You must correctly specify the data directory using --data-dir <dir>."<<endl;
      }
      else if (not fs::is_directory(data_dir)) {
	(*s_err)<<"Warning: BAli-Phy data directory '"<<data_dir.string()<<"' is not a directory!"<<endl;
	(*s_err)<<"         You must correctly specified the data directory using --data-dir <dir>."<<endl;
      }
      else if (not fs::exists( data_dir / "wag.dat")) {
	(*s_err)<<"Warning: BAli-Phy data directory '"<<data_dir.string()<<"' exists, but doesn't contain the important file 'wag.dat'."<<endl;
	(*s_err)<<"         Have you correctly specified the data directory using --data-dir <dir>?."<<endl;
      }
    }
    
    //----------- Load alignment and tree ---------//
    alignment A;
    SequenceTree T;
    if (args.count("tree"))
      load_A_and_T(args,A,T);
    else
      load_A_and_random_T(args,A,T);

    (*s_out)<<"data = "<<args["align"].as<string>()<<endl<<endl;

    (*s_out)<<"alphabet = "<<A.get_alphabet().name<<endl<<endl;

    if (A.n_sequences() < 3)
      throw myexception()<<"At least 3 sequences must be provided - you provided only "<<A.n_sequences()<<".\n(Perhaps you have BLANK LINES in your FASTA file?)";

    //--------- Set up the substitution model --------//
    OwnedPointer<substitution::MultiModel> full_smodel = get_smodel(args,A);
    
    if (not full_smodel->full_tree)
      for(int i=T.n_leaves();i<T.n_branches();i++)
	T.branch(i).set_length(0);

    //-------------Choose an indel model--------------//
    OwnedPointer<IndelModel> imodel;

    if (not args.count("traditional"))
      imodel = get_imodel(args);
    
    //-------------Create the Parameters object--------------//
    Parameters P(*full_smodel,T);
    if (imodel)
      P = Parameters(*full_smodel,*imodel,T);

    (*s_out)<<"subst model = "<<P.SModel().name();
    if (not P.SModel().full_tree)
      (*s_out)<<", *-tree";
    (*s_out)<<endl<<endl;

    (*s_out)<<"indel model = ";
    if (imodel)
      (*s_out)<<P.IModel().name();
    else
      (*s_out)<<"none";
    (*s_out)<<endl<<endl;

    P.alignment_constraint = load_alignment_constraint(args,T);

    if (args.count("beta")) {
      string beta_s = args["beta"].as<string>();
      vector<double> beta = split<double>(beta_s,',');
      for(int i=0;i<beta.size() and i<P.beta.size();i++)
	P.beta[i] = beta[i];

      P.beta_series.push_back(P.beta[0]);
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
	  next = std::max(0.0,std::min(1.0,next));
	  P.beta_series.push_back(next);
	}
      }
    }

    if (args.count("partition-weights")) {
      string filename = args["partition-weights"].as<string>();

      const double n = 0.6;

      ifstream partitions(filename.c_str());
      string line;
      while(getline(partitions,line)) {
	Partition p(T.get_sequences(),line);
	getline(partitions,line);
	double o = convertTo<double>(line);

	cerr<<p<<"      P = "<<o<<endl;
	if (o > n) {
	  double w = n/(1-n)*(1-o)/o;
	  efloat_t w2 = w;

	  P.partitions.push_back(p);
	  P.partition_weights.push_back(w2);

	  cerr<<P.partitions.back()<<"      weight = "<<w<<endl;
	}
      }
    }

    /*
    P.constants[0] = args.loadvalue("bandwidth",100.0);
    if (args.set("pinning") and args["pinning"] == "enable")
      P.features |= (1<<0);
    if (args.set("banding") and args["banding"] == "enable")
      P.features |= (1<<1);
    */

    // fix, unfix, and set parameters
    set_parameters(P,args);

    P.LC.set_length(A.length());

    add_leaf_seq_note(A,T.n_leaves());
    add_subA_index_note(A,T.n_branches());

    // Why do we need to do this, again?
    P.recalc_all();

    //---------------Do something------------------//
    if (args.count("show-only"))
      print_stats(cout,cout,A,P);
    else {
      signal(SIGHUP,SIG_IGN);
      signal(SIGXCPU,SIG_IGN);

      long int max_iterations = args["iterations"].as<long int>();

      do_sampling(args,A,P,max_iterations,*s_out,*s_trees,*s_parameters,*s_map);

      // Close all the streams, and write a notification that we finished all the iterations.
      // s_out->close(); s_trees->close(); s_parameters->close(); s_map->close();
    }
  }
  catch (std::bad_alloc) {
    cerr<<"Doh!  Some kind of memory problem?\n";
    report_mem();
    exit(1);
  }
  catch (std::exception& e) {
    cerr<<"Error: "<<e.what()<<endl;
    exit(1);
  }

  return 0;
}

