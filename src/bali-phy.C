/* Version 2: based on operating on multiple alignments */

#include <cmath>
#include <ctime>
#include <iostream>
#include <fstream>
#include <sstream>
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
#include "tree-util.H" //extends
#include "version.H"

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
		 const vector<ostream*> files)
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

  int nodes_weight = (int)(loadvalue(P.keys,"nodes_weight",1.0)+0.5);

  alignment_moves.add(nodes_weight, nodes_moves);

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

  length_moves.add(1,SingleMove(walk_tree_sample_branch_lengths,
				"walk_tree_sample_branch_lengths","lengths")
		   );

  tree_moves.add(1,length_moves);
  tree_moves.add(1,SingleMove(sample_NNI_and_branch_lengths,"NNI_and_lengths","topology:lengths"));

  //------------- parameters (parameters_moves) --------------//
  MoveAll parameter_moves("parameters");

  add_MH_move(P, log_scaled(shift_cauchy),    "mu",             "mu_scale_sigma",     0.6,  parameter_moves);
  add_MH_move(P, log_scaled(shift_cauchy),    "HKY::kappa",     "kappa_scale_sigma",  0.3,  parameter_moves);
  add_MH_move(P, log_scaled(shift_cauchy),    "TN::kappa(pur)", "kappa_scale_sigma",  0.3,  parameter_moves);
  add_MH_move(P, log_scaled(shift_cauchy),    "TN::kappa(pyr)", "kappa_scale_sigma",  0.3,  parameter_moves);
  add_MH_move(P, log_scaled(shift_cauchy),    "M0::omega",  "omega_scale_sigma",  0.3,  parameter_moves);
  add_MH_move(P, log_scaled(more_than(0,shift_cauchy)),
	                                        "M2::omega",  "omega_scale_sigma",  0.3,  parameter_moves);
  add_MH_move(P, between(0,1,shift_cauchy),   "INV::p",         "INV::p_shift_sigma", 0.03, parameter_moves);
  add_MH_move(P, between(0,1,shift_cauchy),   "f",              "f_shift_sigma",      0.1,  parameter_moves);
  add_MH_move(P, between(0,1,shift_cauchy),   "g",              "g_shift_sigma",      0.1,  parameter_moves);
  add_MH_move(P, between(0,1,shift_cauchy),   "h",              "h_shift_sigma",      0.1,  parameter_moves);
  add_MH_move(P, log_scaled(shift_cauchy),    "beta::mu",       "beta::mu_scale_sigma",     0.2,  parameter_moves);
  add_MH_move(P, log_scaled(shift_cauchy),
	                                        "gamma::sigma/mu","gamma::sigma_scale_sigma",  0.25, parameter_moves);
  add_MH_move(P, log_scaled(shift_cauchy),
	                                        "beta::sigma/mu", "beta::sigma_scale_sigma",  0.25, parameter_moves);
  add_MH_move(P, log_scaled(shift_cauchy),
	                                        "log-normal::sigma/mu","log-normal::sigma_scale_sigma",  0.25, parameter_moves);

  if (P.has_IModel()) {
    add_MH_move(P, shift_delta,                 "delta",       "lambda_shift_sigma",     0.35, parameter_moves);
    add_MH_move(P, less_than(0,shift_cauchy), "lambda",      "lambda_shift_sigma",    0.35, parameter_moves);
    add_MH_move(P, shift_epsilon,               "epsilon",     "epsilon_shift_sigma",   0.15, parameter_moves);
    add_MH_move(P, between(0,1,shift_cauchy), "invariant",   "invariant_shift_sigma", 0.15, parameter_moves);
  }
  
  set_if_undef(P.keys,"pi_dirichlet_N",1.0);
  P.keys["pi_dirichlet_N"] *= max(sequence_lengths(A,P.T.n_leaves()));
  add_MH_move(P, dirichlet_proposal,    "pi*",    "pi_dirichlet_N",      1,  parameter_moves);
  add_MH_move(P, dirichlet_proposal,    "INV::pi*",    "pi_dirichlet_N",      1,  parameter_moves);
  add_MH_move(P, dirichlet_proposal,    "VAR::pi*",    "pi_dirichlet_N",      1,  parameter_moves);

  set_if_undef(P.keys,"GTR_dirichlet_N",1.0);
  P.keys["GTR_dirichlet_N"] *= 100;
  add_MH_move(P, dirichlet_proposal,    "GTR::*", "GTR_dirichlet_N",     1,  parameter_moves);

  set_if_undef(P.keys,"v_dirichlet_N",1.0);
  P.keys["v_dirichlet_N"] *= A.length();
  add_MH_move(P, dirichlet_proposal,    "v*", "v_dirichlet_N",     1,  parameter_moves);

  set_if_undef(P.keys,"b_dirichlet_N",1.0);
  P.keys["b_dirichlet_N"] *= A.length();
  add_MH_move(P, dirichlet_proposal,    "b_*", "b_dirichlet_N",     1,  parameter_moves);

  set_if_undef(P.keys,"M2::f_dirichlet_N",1.0);
  P.keys["M2::f_dirichlet_N"] *= 10;
  add_MH_move(P, dirichlet_proposal,    "M2::f*", "M2::f_dirichlet_N",     1,  parameter_moves);

  set_if_undef(P.keys,"M3::f_dirichlet_N",1.0);
  P.keys["M3::f_dirichlet_N"] *= 10;
  add_MH_move(P, dirichlet_proposal,    "M3::f*", "M3::f_dirichlet_N",     1,  parameter_moves);

  set_if_undef(P.keys,"multi:p_dirichlet_N",1.0);
  P.keys["multi:p_dirichlet_N"] *= 10;
  add_MH_move(P, dirichlet_proposal,    "multi:p*", "multi:p_dirichlet_N",     1,  parameter_moves);

  for(int i=0;;i++) {
    string name = "M3::omega" + convertToString(i+1);
    if (not has_parameter(P,name))
      break;

    add_MH_move(P, log_scaled(shift_cauchy), name, "omega_scale_sigma", 1, parameter_moves);
    //    Proposal2 m(log_scaled(shift_cauchy), name, vector<string>(1,"omega_scale_sigma"), P);
    //    parameter_moves.add(1, MCMC::MH_Move(m,"sample_M3::omega"));
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
  
  ostream& s_out = *files[0];
  ostream& s_trees = *files[2];
  ostream& s_parameters = *files[3];
  ostream& s_map = *files[4];
  
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
    ("help,h", "Produce help message")
    ("version,v", "Print version information")
    ("config", value<string>(),"Config file to read")
    ("show-only","Analyze the initial values and exit")
    ("seed", value<unsigned long>(),"Random seed")
    ("data-dir", value<string>()->default_value("Data"),"Location of the Data/ directory")
    ("name", value<string>(),"Name for the analysis, instead of the alignment filename.")
    ("traditional,t","Fix the alignment and don't model indels")
    ("letters",value<string>()->default_value("full_tree"),"If set to 'star', then use a star tree for substitution")
    ;
  
  options_description mcmc("MCMC options");
  mcmc.add_options()
    ("iterations",value<long int>()->default_value(100000),"The number of iterations to run")
    ("subsample",value<int>()->default_value(1),"Factor by which to subsample")
    ("beta",value<string>(),"MCMCMC temperature")
    ("dbeta",value<string>(),"MCMCMC temperature changes")
    ("enable",value<string>(),"Comma-separated list of kernels to enable")
    ("disable",value<string>(),"Comma-separated list of kernels to disable")
    ("partition-weights",value<string>(),"File containing tree with partition weights")
    ;
    
  options_description parameters("Parameter options");
  parameters.add_options()
    ("align", value<string>(),"File with sequences and initial alignment")
    ("randomize-alignment","Randomly realign the sequences before use.")
    ("internal",value<string>(),"If set to '+', then make all internal node entries wildcards")
    ("tree",value<string>(),"File with initial tree")
    ("set",value<vector<string> >()->composing(),"Set parameter=<value>")
    ("fix",value<vector<string> >()->composing(),"Fix parameter[=<value>]")
    ("unfix",value<vector<string> >()->composing(),"Un-fix parameter[=<value>]")
    ("frequencies",value<string>(),"Initial frequencies: 'uniform','nucleotides', or a comma-separated vector.") 
    ;

  options_description model("Model options");
  model.add_options()
    ("alphabet",value<string>(),"Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.")
    ("genetic-code",value<string>()->default_value("standard-code.txt"),"Specify alternate genetic code file in data directory.")
    ("smodel",value<string>(),"Substitution model.")
    ("imodel",value<string>()->default_value("fragment-based+T"),"Indel model: simple, fragment-based, or fragment-based+T.")
    ("align-constraint",value<string>(),"File with alignment constraints.")
    ("t-constraint",value<string>(),"File with m.f. tree representing topology and branch-length constraints.")
    ("a-constraint",value<string>(),"File with groups of leaf taxa whose alignment is constrained.")
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
    print_version_info(cout);
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
      throw myexception()<<"Can't load config file '"<<filename<<"'";

    store(parse_config_file(file, all), args);
    file.close();
    notify(args);
  }

  load_bali_phy_rc(args,all);

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
  vector<double> parameters = P.parameters();
  for(int i=0;i<doset.size();i++) {
    //parse
    vector<string> parse = split(doset[i],'=');
    if (parse.size() != 2)
      throw myexception()<<"Ill-formed initial condition '"<<doset[i]<<"'.";

    string name = parse[0];
    double value = convertTo<double>(parse[1]);

    int p=-1;
    if (p=find_parameter(P,name),p!=-1)
      parameters[p] = value;
    else
      P.keys[name] = value;
  }
  P.parameters(parameters);
}

void close_files(vector<ofstream*>& files)
{
  for(int i=0;i<files.size();i++) {
    files[i]->close();
    delete files[i];
  }
  files.clear();
}

void delete_files(vector<string>& filenames)
{
  for(int i=0;i<filenames.size();i++)
    fs::remove(filenames[i]);
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
	close_files(files);
	delete_files(filenames);
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

vector<ostream*> init_files(const variables_map& args,int argc,char* argv[])
{
  vector<ostream*> files;

  if (args.count("show-only")){
    files.push_back(&cout);
    files.push_back(&cerr);
  }
  else {

    string name = remove_extension( fs::path( args["align"].as<string>() ).leaf() );
    if (args.count("name"))
      name = args["name"].as<string>();
    
    string dirname = open_dir(name);
    cerr<<"Created directory '"<<dirname<<"' for output files."<<endl;
    
    vector<string> filenames;
    filenames.push_back("out");
    filenames.push_back("err");
    filenames.push_back("trees");
    filenames.push_back("p");
    filenames.push_back("MAP");
    
    vector<ofstream*> files2 = open_files(dirname+"/",filenames);
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
    if (getenv("JOB_ID"))
      s_out<<"JOB_ID: "<<getenv("JOB_ID")<<endl;
    if (getenv("LSB_JOBID"))
      s_out<<"LSB_JOBID: "<<getenv("LSB_JOBID")<<endl;
    s_out<<"hostname: "<<hostname()<<endl;
    s_out<<"PID: "<<getpid()<<endl;
    s_out<<endl;
  }
  //  files[0]->precision(10);
  //  cerr.precision(10);

  return files;
}


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

vector<int> load_alignment_branch_constraints(const string& filename, const SequenceTree& TC)
{
  // open file
  ifstream file(filename.c_str());
  if (not file)
    throw myexception()<<"Can't load alignment-branch constraint file '"<<filename<<"'";

  // read file
  string line;
  vector<vector<string> > name_groups;
  while(getline_handle_dos(file,line)) {
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
  vector<valarray<bool> > mask_groups(name_groups.size());
  for(int i=0;i<mask_groups.size();i++) 
  {
    mask_groups[i].resize(TC.n_leaves());
    mask_groups[i] = false;

    for(int j=0;j<name_groups[i].size();j++) 
    {
      int index = find_index(TC.get_sequences(),name_groups[i][j]);

      if (index == -1)
	throw myexception()<<"Reading alignment constraint file '"<<filename<<"':\n"
			   <<"   Can't find leaf taxon '"<<name_groups[i][j]<<"' in the tree.";
      else
	mask_groups[i][index] = true;
    }
  }

  // check that each group is a fully resolved clade in the constraint tree
  vector<int> branches;
  for(int i=0;i<mask_groups.size();i++) 
  {
    // find the branch that corresponds to a mask
    valarray<bool> mask(TC.n_leaves());
    int found = -1;
    for(int b=0;b<2*TC.n_branches() and found == -1;b++) 
    {
      mask = TC.partition(b);

      if (equal(mask_groups[i],mask))
	found = b;
    }

    // complain if we can't find it
    if (found == -1) 
      throw myexception()<<"Alignment constraint: clade '"
			 <<join(name_groups[i],' ')
			 <<"' not found in topology constraint tree.";
    
    // add child branches if we can find it
    vector<const_branchview> b2 = branches_after(TC,found);
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

void my_gsl_error_handler(const char* reason, const char* file, int line, int gsl_errno)
{
  std::cerr<<"gsl: "<<file<<":"<<line<<" (errno="<<gsl_errno<<") ERROR:"<<reason<<endl;
  //  std::abort();
}

int main(int argc,char* argv[]) 
{ 
  std::ios::sync_with_stdio(false);

  ostream out_screen(cout.rdbuf());
  ostream err_screen(cerr.rdbuf());

  std::ostringstream out_cache;
  std::ostringstream err_cache;

  teebuf tee_out(out_screen.rdbuf(), out_cache.rdbuf());
  teebuf tee_err(err_screen.rdbuf(), err_cache.rdbuf());

  ostream out_both(&tee_out);
  ostream err_both(&tee_err);

  try {

    fp_scale::initialize();
    fs::path::default_name_check(fs::portable_posix_name);

    gsl_set_error_handler(&my_gsl_error_handler);

    //---------- Parse command line  ---------//
    variables_map args = parse_cmd_line(argc,argv);

    //------ Capture copy of 'cerr' output in 'err_cache' ------//
    if (not args.count("show-only"))
      cerr.rdbuf(err_both.rdbuf());

    //---------- Determine Data dir ---------------//
    {
      fs::path data_dir = args["data-dir"].as<string>();
      if (not fs::exists(data_dir)) {
	cerr<<"Warning: BAli-Phy data directory '"<<data_dir.string()<<"' does not exist!"<<endl;
	cerr<<"         You must correctly specify the data directory using --data-dir <dir>."<<endl<<endl;
      }
      else if (not fs::is_directory(data_dir)) {
	cerr<<"Warning: BAli-Phy data directory '"<<data_dir.string()<<"' is not a directory!"<<endl;
	cerr<<"         You must correctly specify the data directory using --data-dir <dir>."<<endl<<endl;
      }
      else if (not fs::exists( data_dir / "wag.dat")) {
	cerr<<"Warning: BAli-Phy data directory '"<<data_dir.string()<<"' exists, but doesn't contain the"<<endl;
	cerr<<"               important file 'wag.dat'."<<endl;
	cerr<<"         Have you correctly specified the data directory using --data-dir <dir>?"<<endl<<endl;
      }
    }

    //---------- Initialize random seed -----------//
    unsigned long seed = 0;
    if (args.count("seed")) {
      seed = args["seed"].as<unsigned long>();
      myrand_init(seed);
    }
    else
      seed = myrand_init();
    
    out_cache<<"random seed = "<<seed<<endl<<endl;

    //----------- Load alignment and tree ---------//
    alignment A;
    SequenceTree T;
    if (args.count("tree"))
      load_A_and_T(args,A,T);
    else
      load_A_and_random_T(args,A,T);

    out_cache<<"data = "<<args["align"].as<string>()<<endl<<endl;
    out_cache<<"alphabet = "<<A.get_alphabet().name<<endl<<endl;

    //--------- Handle branch lengths <= 0 --------//
    double min_branch = 0.000001;
    for(int i=0;i<T.n_branches();i++)
      if (T.branch(i).length() > 0)
	min_branch = std::min(min_branch,T.branch(i).length());

    for(int i=0;i<T.n_branches();i++) {
      if (T.branch(i).length() == 0)
	T.branch(i).set_length(min_branch);
      if (T.branch(i).length() < 0)
	T.branch(i).set_length( - T.branch(i).length() );
    }

    //--------- Do we have enough sequences? ------//
    if (A.n_sequences() < 3)
      throw myexception()<<"At least 3 sequences must be provided - you provided only "<<A.n_sequences()<<".\n(Perhaps you have BLANK LINES in your FASTA file?)";

    //---------- Open output files -----------//
    vector<ostream*> files = init_files(args,argc,argv);
    ostream& s_out = *files[0];
    ostream& s_err = *files[1];

    s_out<<out_cache.str(); s_out.clear();
    s_err<<err_cache.str(); s_err.clear();

    tee_out.setbuf2(s_out.rdbuf());
    tee_err.setbuf2(s_err.rdbuf());

    cerr.flush() ; cerr.rdbuf(s_err.rdbuf());
    clog.flush() ; clog.rdbuf(s_err.rdbuf());

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

    s_out<<"subst model = "<<P.SModel().name();
    if (not P.SModel().full_tree)
      s_out<<", *-tree";
    s_out<<endl<<endl;

    s_out<<"indel model = ";
    if (imodel)
      s_out<<P.IModel().name();
    else
      s_out<<"none";
    s_out<<endl<<endl;

    //----------------- Tree-based constraints ----------------//
    if (args.count("t-constraint"))
      P.TC = load_constraint_tree(args["t-constraint"].as<string>(), T.get_sequences());

    if (args.count("a-constraint"))
      P.AC = load_alignment_branch_constraints(args["a-constraint"].as<string>(),P.TC);

    if (not extends(T,P.TC))
      throw myexception()<<"Initial tree violates topology constraints.";

    //---------- Alignment constraint (horizontal) -----------//
    P.alignment_constraint = load_alignment_constraint(args,T);

    //---------- Alignment constraint (vertical) -----------//
    //P.alignment_constraint = load_alignment_constraint(args,T);

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
      while(getline_handle_dos(partitions,line)) {
	Partition p(T.get_sequences(),line);
	getline_handle_dos(partitions,line);
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

      do_sampling(args,A,P,max_iterations,files);

      // Close all the streams, and write a notification that we finished all the iterations.
      // close_files(files);
    }
  }
  catch (std::bad_alloc) {
    err_both<<"Doh!  Some kind of memory problem?\n";
    report_mem();
    exit(1);
  }
  catch (std::exception& e) {
    err_both<<"Error: "<<e.what()<<endl;
    exit(1);
  }

  return 0;
}

