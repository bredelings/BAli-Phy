/* Version 2: based on operating on multiple alignments */

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

#include <boost/program_options.hpp>
#include <boost/filesystem/operations.hpp>

#include "substitution.H"
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
#include "slice-sampling.H"

namespace fs = boost::filesystem;

namespace po = boost::program_options;
using po::variables_map;

using std::cout;
using std::cerr;
using std::clog;
using std::endl;
using std::ostream;

using boost::dynamic_bitset;

bool has_parameter(const Model& M, const string& name)
{
  for(int i=0;i<M.parameters().size();i++)
    if (M.parameter_name(i) == name)
      return true;
  return false;
}

bool match(const string& s1, const string& s2)
{
  if (s2.size() and s2[s2.size()-1] == '*') {
    int L = s2.size() - 1;
    if (L > s1.size()) return false;
    return (s1.substr(0,L) == s2.substr(0,L));
  }
  else
    return s1 == s2;
}


vector<int> parameters_with_extension(const Model& M, string name)
{
  bool complete_match = false;
  if (name.size() and name[0] == '^') {
    complete_match = true;
    name = name.substr(1,name.size()-1);
  }

  vector<int> indices;

  const vector<string> path2 = split(name,"::");

  if (not path2.size()) return indices;

  for(int i=0;i<M.n_parameters();i++)
  {
    vector<string> path1 = split(M.parameter_name(i),"::");

    if (not path2[0].size()) 
      path1.erase(path1.begin());
    else if (path2.size() > path1.size())
      continue;
    else if (not complete_match)
    {
      int n = path1.size() - path2.size();
      path1.erase(path1.begin(),path1.begin() + n);
    }

    if (not match(path1.back(),path2.back())) continue;

    path1.pop_back();

    vector<string> temp = path2;
    temp.pop_back();

    if (path1 == temp) indices.push_back(i);
  }
  return indices;
}

void add_MH_move(Parameters& P,const Proposal_Fn& p, const string& name, const string& pname,double sigma, MCMC::MoveAll& M)
{
  if (name.size() and name[name.size()-1] == '*')
  {
    vector<int> indices = parameters_with_extension(P,name);
    vector<string> names;
    for(int i=0;i<indices.size();i++)
      names.push_back(P.parameter_name(indices[i]));

    if (names.empty()) return;

    set_if_undef(P.keys, pname, sigma);
    Proposal2 move_mu(p, names, vector<string>(1,pname), P);

    M.add(1, MCMC::MH_Move(move_mu,string("MH_sample_")+name));
  }
  else {
    vector<int> indices = parameters_with_extension(P,name);
    for(int i=0;i<indices.size();i++) 
      if (not P.fixed(indices[i])) {
	set_if_undef(P.keys, pname, sigma);
	Proposal2 move_mu(p, P.parameter_name(indices[i]), vector<string>(1,pname), P);
	M.add(1, MCMC::MH_Move(move_mu,string("MH_sample_")+P.parameter_name(indices[i])));
      }
  }
}


// We need
//   1. A way to guess initial window size
//   2. 

void add_slice_moves(Parameters& P, const string& name, 
		     const string& pname, double W,
		     bool lower_bound, double lower,
		     bool upper_bound, double upper,
		     MCMC::MoveAll& M)
{
  vector<int> indices = parameters_with_extension(P,name);
  for(int i=0;i<indices.size();i++) 
  {
    if (P.fixed(indices[i])) continue;

    // Use W as default window size of "pname" is not set.
    set_if_undef(P.keys, pname, W);
    W = P.keys[pname];

    M.add(1, 
	  MCMC::Slice_Move(string("slice_sample_")+P.parameter_name(indices[i]),
			   indices[i],
			   lower_bound,lower,upper_bound,upper,W)
	  );
  }
}

void add_slice_moves(Parameters& P, const string& name, 
		     const string& pname, double W,
		     bool lower_bound, double lower,
		     bool upper_bound, double upper,
		     MCMC::MoveAll& M,
		     double(&f1)(double),
		     double(&f2)(double)
		     )
{
  vector<int> indices = parameters_with_extension(P,name);
  for(int i=0;i<indices.size();i++) 
  {
    if (P.fixed(indices[i])) continue;

    // Use W as default window size of "pname" is not set.
    set_if_undef(P.keys, pname, W);
    W = P.keys[pname];

    M.add(1, 
	  MCMC::Slice_Move(string("slice_sample_")+P.parameter_name(indices[i]),
			   indices[i],
			   lower_bound,lower,upper_bound,upper,W,f1,f2)
	  );
  }
}

//FIXME - how to make a number of variants with certain things fixed, for burn-in?
// 0. Get an initial tree estimate using NJ or something? (as an option...)
// 1. First estimate tree and parameters with alignment fixed.
// 2. Then allow the alignment to change.


void do_sampling(const variables_map& args,Parameters& P,long int max_iterations,
		 vector<ostream*>& files)
{
  // args for branch-based stuff
  vector<int> branches(P.T->n_branches());
  for(int i=0;i<branches.size();i++)
    branches[i] = i;

  // args for branch-based stuff
  vector<int> internal_nodes;
  for(int i=P.T->n_leaves();i<P.T->n_nodes();i++)
    internal_nodes.push_back(i);

  // args for branch-based stuff
  vector<int> internal_branches;
  for(int i=P.T->n_leaves();i<P.T->n_branches();i++)
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

  if (P.T->n_leaves() >2) {
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
  alignment_moves.add(1, SingleMove(sample_alignment_rates, "alignment_rates") );

  //---------- alignment::nodes_master (nodes_moves) ----------//
  MoveEach nodes_moves("nodes_master","alignment:nodes");
  if (P.T->n_leaves() >= 3)
    nodes_moves.add(10,MoveArgSingle("sample_node","alignment:nodes",
				   sample_node_move,
				   internal_nodes)
		   );
  if (P.T->n_leaves() >= 4)
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

  bool has_imodel = (P.n_imodels() > 0);

  if (has_imodel)
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
  if (has_imodel)
    NNI_move.add(0.025,MoveArgSingle("three_way_NNI_and_A","alignment:alignment_branch:nodes:topology",
				   three_way_topology_and_alignment_sample,
				     internal_branches)
		 ,false
		 );


  if (has_imodel) {
    SPR_move.add(1,SingleMove(sample_SPR_flat,"SPR_and_A_flat","topology:lengths:nodes:alignment:alignment_branch"));
    SPR_move.add(1,SingleMove(sample_SPR_nodes,"SPR_and_A_nodes","topology:lengths:nodes:alignment:alignment_branch"));
  }
  else {
    SPR_move.add(1,SingleMove(sample_SPR_flat,"SPR_flat","topology:lengths"));
    SPR_move.add(1,SingleMove(sample_SPR_nodes,"SPR_and_A_nodes","topology:lengths"));
  }

  topology_move.add(1,NNI_move,false);
  topology_move.add(1,SPR_move);
  if (P.T->n_leaves() >3 and P.smodel_full_tree)
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
  if (P.smodel_full_tree)
    length_moves1.add(0.01,MoveArgSingle("change_branch_length_and_T","lengths:nodes:topology",
					change_branch_length_and_T,
					internal_branches)
		      );
  length_moves.add(1,length_moves1,false);
  // FIXME - Do we really want to do this, under slice sampling?
  length_moves.add(1,SingleMove(walk_tree_sample_branch_lengths,
				"walk_tree_sample_branch_lengths","lengths")
		   );

  tree_moves.add(1,length_moves);
  tree_moves.add(1,SingleMove(sample_NNI_and_branch_lengths,"NNI_and_lengths","topology:lengths"));

  //------------- parameters (parameters_moves) --------------//
  MoveAll parameter_moves("parameters");
  MoveAll slice_moves("parameters:slice");
  MoveAll MH_moves("parameters:MH");

  add_MH_move(P, log_scaled(between(-20,20,shift_cauchy)),    "mu",             "mu_scale_sigma",     0.6,  MH_moves);
  for(int i=0;i<P.n_branch_means();i++)
    add_MH_move(P, log_scaled(between(-20,20,shift_cauchy)),    "mu"+convertToString(i+1),             "mu_scale_sigma",     0.6,  MH_moves);

  add_slice_moves(P, "mu",      "mu_slice_window",    0.3, true,0,false,0,slice_moves);
  for(int i=0;i<P.n_branch_means();i++)
    add_slice_moves(P, "mu"+convertToString(i+1),      "mu_slice_window",    0.3, true,0,false,0,slice_moves);
    
  add_slice_moves(P, "log-normal::sigma/mu",      "log-normal::sigma_slice_window",    1.0, true,0,false,0,slice_moves);
  add_slice_moves(P, "gamma::sigma/mu",      "gamma::sigma_slice_window",    1.0, true,0,false,0,slice_moves);
  add_slice_moves(P, "INV::p",         "INV::p_shift_sigma", 0.1, true,0,true,1,slice_moves);

  add_MH_move(P, log_scaled(between(-20,20,shift_cauchy)),    "HKY::kappa",     "kappa_scale_sigma",  0.3,  parameter_moves);
  add_MH_move(P, log_scaled(between(-20,20,shift_cauchy)),    "rho",     "rho_scale_sigma",  0.2,  parameter_moves);
  add_MH_move(P, log_scaled(between(-20,20,shift_cauchy)),    "TN::kappa(pur)", "kappa_scale_sigma",  0.3,  parameter_moves);
  add_MH_move(P, log_scaled(between(-20,20,shift_cauchy)),    "TN::kappa(pyr)", "kappa_scale_sigma",  0.3,  parameter_moves);
  add_MH_move(P, log_scaled(shift_cauchy),    "M0::omega",  "omega_scale_sigma",  0.3,  parameter_moves);
  add_MH_move(P, log_scaled(more_than(0,shift_cauchy)),
	                                        "M2::omega",  "omega_scale_sigma",  0.3,  parameter_moves);
  add_MH_move(P, between(0,1,shift_cauchy),   "INV::p",         "INV::p_shift_sigma", 0.03, MH_moves);
  add_MH_move(P, between(0,1,shift_cauchy),   "f",              "f_shift_sigma",      0.1,  parameter_moves);
  add_MH_move(P, between(0,1,shift_cauchy),   "g",              "g_shift_sigma",      0.1,  parameter_moves);
  add_MH_move(P, between(0,1,shift_cauchy),   "h",              "h_shift_sigma",      0.1,  parameter_moves);
  add_MH_move(P, log_scaled(shift_cauchy),    "beta::mu",       "beta::mu_scale_sigma",     0.2,  parameter_moves);
  add_MH_move(P, log_scaled(shift_cauchy),    "gamma::sigma/mu","gamma::sigma_scale_sigma",  0.25, MH_moves);
  add_MH_move(P, log_scaled(shift_cauchy),    "beta::sigma/mu", "beta::sigma_scale_sigma",  0.25, parameter_moves);
  add_MH_move(P, log_scaled(shift_cauchy),    "log-normal::sigma/mu","log-normal::sigma_scale_sigma",  0.25, MH_moves);
  parameter_moves.add(4,SingleMove(scale_means_only,
				   "scale_means_only","mean")
		      );


  
  add_MH_move(P, shift_delta,                 "delta",       "lambda_shift_sigma",     0.35, MH_moves);
  add_MH_move(P, less_than(0,shift_cauchy), "lambda",      "lambda_shift_sigma",    0.35, MH_moves);
  add_MH_move(P, less_than(0,shift_cauchy), "lambda_s",      "lambda_shift_sigma",    0.35, parameter_moves);
  add_MH_move(P, less_than(0,shift_cauchy), "lambda_f",      "lambda_shift_sigma",    0.35, parameter_moves);
  add_MH_move(P, shift_epsilon,               "epsilon",     "epsilon_shift_sigma",   0.30, MH_moves);

  // FIXME - check if we are accidentally evaluating the likelihood or something.
  add_slice_moves(P, "lambda",      "lambda_slice_window",    1.0, false,0,false,0,slice_moves);
  add_slice_moves(P, "epsilon",     "epsilon_slice_window",   1.0,
		  false,0,false,0,slice_moves,transform_epsilon,inverse_epsilon);

  add_MH_move(P, shift_epsilon,               "r_s",     "epsilon_shift_sigma",   0.15, parameter_moves);
  add_MH_move(P, shift_epsilon,               "r_f",     "epsilon_shift_sigma",   0.15, parameter_moves);
  add_MH_move(P, between(0,1,shift_cauchy), "switch",   "invariant_shift_sigma", 0.15, parameter_moves);
  add_MH_move(P, between(0,1,shift_cauchy), "invariant",   "invariant_shift_sigma", 0.15, parameter_moves);
  
  set_if_undef(P.keys,"pi_dirichlet_N",1.0);
  unsigned total_length = 0;
  for(int i=0;i<P.n_data_partitions();i++)
    total_length += max(sequence_lengths(*P[i].A, P.T->n_leaves()));
  P.keys["pi_dirichlet_N"] *= total_length;

  for(int s=0;s<=P.n_smodels();s++) 
  {
    string index = convertToString(s+1);
    string prefix = "^S" + index + "::";

    if (s==P.n_smodels())
      prefix = "^";

    add_MH_move(P, dirichlet_proposal,  prefix + "pi*",    "pi_dirichlet_N",      1,  parameter_moves);
    
    add_MH_move(P, dirichlet_proposal,  prefix + "INV::pi*",    "pi_dirichlet_N",      1,  parameter_moves);
    add_MH_move(P, dirichlet_proposal,  prefix + "VAR::pi*",    "pi_dirichlet_N",      1,  parameter_moves);

    set_if_undef(P.keys,"GTR_dirichlet_N",1.0);
    if (s==0) P.keys["GTR_dirichlet_N"] *= 100;
    add_MH_move(P, dirichlet_proposal,  prefix + "GTR::*", "GTR_dirichlet_N",     1,  parameter_moves);

    set_if_undef(P.keys,"v_dirichlet_N",1.0);
    if (s==0) P.keys["v_dirichlet_N"] *= total_length;
    add_MH_move(P, dirichlet_proposal,  prefix +  "v*", "v_dirichlet_N",     1,  parameter_moves);

    set_if_undef(P.keys,"b_dirichlet_N",1.0);
    if (s==0) P.keys["b_dirichlet_N"] *= total_length;
    add_MH_move(P, dirichlet_proposal,  prefix +  "b_*", "b_dirichlet_N",     1,  parameter_moves);

    set_if_undef(P.keys,"M2::f_dirichlet_N",1.0);
    if (s==0) P.keys["M2::f_dirichlet_N"] *= 10;
    add_MH_move(P, dirichlet_proposal,  prefix +  "M2::f*", "M2::f_dirichlet_N",     1,  parameter_moves);

    set_if_undef(P.keys,"M3::f_dirichlet_N",1.0);
    if (s==0) P.keys["M3::f_dirichlet_N"] *= 10;
    add_MH_move(P, dirichlet_proposal,   prefix + "M3::f*", "M3::f_dirichlet_N",     1,  parameter_moves);

    set_if_undef(P.keys,"multi::p_dirichlet_N",1.0);
    if (s==0) P.keys["multi::p_dirichlet_N"] *= 10;
    add_MH_move(P, dirichlet_proposal,   prefix + "multi::p*", "multi:p_dirichlet_N",     1,  parameter_moves);

    set_if_undef(P.keys,"DP::f_dirichlet_N",1.0);
    if (s==0) P.keys["DP::f_dirichlet_N"] *= 10;
    add_MH_move(P, dirichlet_proposal,   prefix + "DP::f*", "DP::f_dirichlet_N",     1,  parameter_moves);

    set_if_undef(P.keys,"DP::rate_dirichlet_N",1.0);
    //FIXME - this should probably be 20*#rate_categories...
    if (s==0) P.keys["DP::rate_dirichlet_N"] *= 10*10;
    add_MH_move(P, sorted(dirichlet_proposal), prefix + "DP::rate*", "DP::rate_dirichlet_N",     1,  parameter_moves);

    set_if_undef(P.keys,"Mixture::p_dirichlet_N",1.0);
    if (s==0) P.keys["Mixture::p_dirichlet_N"] *= 10*10;
    add_MH_move(P, dirichlet_proposal,         prefix + "Mixture::p*", "Mixture::p_dirichlet_N",     1,  parameter_moves);

    if (s >= P.n_smodels()) continue;

    // Handle multi-frequency models
    set_if_undef(P.keys,"MF::dirichlet_N",10.0);

    const alphabet& a = P.SModel(s).Alphabet();
    const int asize = a.size();

    for(int l=0;l<asize;l++) {
      string pname = prefix+ "a" + a.lookup(l) + "*";
      cerr<<pname<<endl;
      add_MH_move(P, dirichlet_proposal, pname, "MF::dirichlet_N",     1,  parameter_moves);
    }
  }

  // FIXME - we need a proposal that sorts after changing
  //         then we can un-hack the recalc function in smodel.C
  //         BUT, this assumes that we have the DP::rate* names in *numerical* order
  //          whereas we probably find them in *lexical* order....
  //          ... or creation order?  That might be OK for now! 


  for(int i=0;;i++) {
    string name = "M3::omega" + convertToString(i+1);
    if (not has_parameter(P,name))
      break;

    add_MH_move(P, log_scaled(shift_cauchy), name, "omega_scale_sigma", 1, parameter_moves);
    //    Proposal2 m(log_scaled(shift_cauchy), name, vector<string>(1,"omega_scale_sigma"), P);
    //    parameter_moves.add(1, MCMC::MH_Move(m,"sample_M3::omega"));
  }

  int subsample = args["subsample"].as<int>();

  // full sampler
  Sampler sampler("sampler");
  if (has_imodel)
    sampler.add(1,alignment_moves);
  sampler.add(2,tree_moves);

  // FIXME - do we really want to do this for (say) lambda and epsilon?  4+25/4 = 10.25
  // FIXME - can we separate the moves into (say) moves which depend on branch lengths, and those that don't?
  //       - MH_smodel / MH_imodel ...
  sampler.add(4 + P.T->n_branches()/4.0,parameter_moves);
  if (P.keys["disable_MH_sampling"] > 0.5)
    sampler.add(1,MH_moves);
  else
    sampler.add(4 + P.T->n_branches()/4.0,MH_moves);
  // Question: how are these moves intermixed with the other ones?

  if (P.keys["disable_slice_sampling"] < 0.5)
    sampler.add(1,slice_moves);

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

  int total_c = 0;
  for(int i=0;i<P.n_data_partitions();i++)
    total_c += P[i].alignment_constraint.size1();

  if (total_c > 0)
    std::cerr<<"Using "<<total_c<<" constraints.\n";

  //FIXME - partition

  for(int i=0;i<P.n_data_partitions();i++) {
    dynamic_bitset<> s2 = constraint_satisfied(P[i].alignment_constraint,*P[i].A);
    dynamic_bitset<> s1(s2.size());
    report_constraints(s1,s2);
  } 

  sampler.go(P,subsample,max_iterations,s_out,s_trees,s_parameters,s_map,files);
}

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

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description general("General options");
  general.add_options()
    ("help,h", "Produce help message")
    ("version,v", "Print version information")
    ("config,c", value<string>(),"Config file to read")
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
    ("align", value<vector<string> >()->composing(),"Files with sequences and initial alignment")
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
    ("smodel",value<vector<string> >()->composing(),"Substitution model.")
    ("imodel",value<vector<string> >()->composing(),"Indel model: RS05, RS07-no-T, or RS07.")
    ("same-scale",value<vector<string> >()->composing(),"Which partitions have the same scale?")
    ("align-constraint",value<string>(),"File with alignment constraints.")
    ("t-constraint",value<string>(),"File with m.f. tree representing topology and branch-length constraints.")
    ("a-constraint",value<string>(),"File with groups of leaf taxa whose alignment is constrained.")
    ;
  options_description all("All options");
  all.add(general).add(mcmc).add(parameters).add(model);

  // positional options
  positional_options_description p;
  p.add("align", -1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).options(all).positional(p).run(), args);
  notify(args);    

  if (args.count("version")) {
    print_version_info(cout);
    exit(0);
  }

  if (args.count("help")) {
    cout<<"Usage: bali-phy <sequence-file1> [<sequence-file2> [OPTIONS]]\n";
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
    throw myexception()<<"No sequence files given.";

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
      throw myexception()<<"Can't find parameter '"<<fix[i]<<"' to fix.";
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

vector<ofstream*> open_files(int proc_id, const string& name, vector<string>& names)
{
  vector<ofstream*> files;
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
      files.push_back(new ofstream(filename.c_str()));
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
      fs::create_directory(dirname);
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

string init_dir(const variables_map& args)
{
  vector<string> alignment_filenames = args["align"].as<vector<string> >();
  for(int i=0;i<alignment_filenames.size();i++)
    alignment_filenames[i] = remove_extension(fs::path( alignment_filenames[i] ).leaf());

  string name = join(alignment_filenames,'-');
  if (args.count("name"))
    name = args["name"].as<string>();
    
  string dirname = open_dir(name);
  cerr<<"Created directory '"<<dirname<<"' for output files."<<endl;

  return dirname;
}


vector<ostream*> init_files(int proc_id, const string& dirname,
			    int argc,char* argv[],int n_partitions)
{
  vector<ostream*> files;

  vector<string> filenames;
  filenames.push_back("out");
  filenames.push_back("err");
  filenames.push_back("trees");
  filenames.push_back("p");
  filenames.push_back("MAP");
  for(int i=0;i<n_partitions;i++) {
    string filename = string("P") + convertToString(i+1) + ".fastas";
    filenames.push_back(filename);
  }
    
  vector<ofstream*> files2 = open_files(proc_id, dirname+"/",filenames);
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
  vector< dynamic_bitset<> > mask_groups(name_groups.size());
  for(int i=0;i<mask_groups.size();i++) 
  {
    mask_groups[i].resize(TC.n_leaves());
    mask_groups[i].reset();

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


bool check_data_dir(const string& dir_name)
{
  fs::path data_dir = dir_name;

  if (not fs::exists(data_dir)) {
    cerr<<"Warning: BAli-Phy data directory '"<<data_dir.string()<<"' does not exist!"<<endl;
    cerr<<"         You must correctly specify the data directory using --data-dir <dir>."<<endl<<endl;
    return false;
  }
  else if (not fs::is_directory(data_dir)) {
    cerr<<"Warning: BAli-Phy data directory '"<<data_dir.string()<<"' is not a directory!"<<endl;
    cerr<<"         You must correctly specify the data directory using --data-dir <dir>."<<endl<<endl;
    return false;
  }
  else if (not fs::exists( data_dir / "WAG.dat")) {
    cerr<<"Warning: BAli-Phy data directory '"<<data_dir.string()<<"' exists, but doesn't contain the"<<endl;
    cerr<<"               important file 'WAG.dat'."<<endl;
    cerr<<"         Have you correctly specified the data directory using --data-dir <dir>?"<<endl<<endl;
    return false;
  }
  return true;
}

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

void sanitize_branch_lengths(SequenceTree& T)
{
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
}

void setup_heating(int proc_id, const variables_map& args, Parameters& P) 
{
  if (args.count("beta")) 
  {
    string beta_s = args["beta"].as<string>();
    vector<double> beta = split<double>(beta_s,',');

    if (proc_id > beta.size())
      throw myexception()<<"not enough temperatures given";

    for(int i=0;i<P.n_data_partitions();i++)
      P.beta[0] = P[i].beta[0] = beta[proc_id];
    
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
	next = std::max(0.0,std::min(1.0,next));
	P.beta_series.push_back(next);
      }
    }
  }
}

void setup_partition_weights(const variables_map& args, Parameters& P) 
{
  if (args.count("partition-weights")) {

    string filename = args["partition-weights"].as<string>();

    const double n = 0.6;

    ifstream partitions(filename.c_str());
    string line;
    while(getline_handle_dos(partitions,line)) {
      Partition p(P.T->get_sequences(),line);
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
}

// how about --smodel=1,3,5:HKY  --smodel=4,5,6:Empirical[WAG]

string parse_partitions_and_model(string model, vector<int>& partitions)
{
  partitions.clear();

  int colon = model.find(':');
  if (colon == -1)
    return model;

  string prefix = model.substr(0,colon);
  model = model.substr(colon+1);

  partitions = split<int>(prefix,',');

  return model;
}

template <typename T>
class shared_items
{
  // unique items
  vector<T> items;

public:

  // from partition -> item
  vector<int> item_for_partition;  

  // from item -> partition
  vector<vector<int> > partitions_for_item;

  int n_unique_items() const {return items.size();}

  int n_partitions() const {return item_for_partition.size();}

  const T& unique(int i) const {return items[i];}
        T& unique(int i)       {return items[i];}

  const T& operator[](int i) const {return items[item_for_partition[i]];}
        T& operator[](int i)       {return items[item_for_partition[i]];}

  int n_partitions_for_item(int i) const {return partitions_for_item[i].size();}

  shared_items(const vector<T>& v1, const vector<int>& v2)
    :items(v1),
     item_for_partition(v2),
     partitions_for_item(items.size())
  {
    for(int i=0;i<n_partitions();i++) {
      int item = item_for_partition[i];
      if (item != -1)
	partitions_for_item[item].push_back(i);
    }
  }
};


//allow partition NAMES from filename?
shared_items<string> get_mapping(const variables_map& args, const string& key, int n)
{
  vector<string> models;
  if (args.count(key))
    models = args[key].as<vector<string> >();

  vector<int> mapping(n,-2);
  vector<string> model_names;

  for(int i=0;i<models.size();i++) 
  {
    vector<int> partitions;

    int index = model_names.size();
    string model_name = parse_partitions_and_model(models[i],partitions);
    if (model_name == "none")
      index = -1;
    else 
      model_names.push_back(model_name);

    // partitions must be specified, ...
    if (partitions.size() == 0) 
    {
      //unless there is only one partition, or..
      if (n == 1)
	partitions.push_back(1);
      //this is the only model is specified, and then it gets ALL partitions
      else if (models.size() == 1) {
	for(int i=1;i<=n;i++)
	  partitions.push_back(i);
      }
      else
	throw myexception()<<"Failed to specify partition number(s) for '"<<key<<"' specification '"<<models[i];
    }

    // map partitions to this model, unless they are already mapped
    for(int j=0;j<partitions.size();j++) 
    {
      // check for bad partition numbers
      if (partitions[j] < 1 or partitions[j] > n)
	throw myexception()<<"Partition "<<partitions[j]<<" doesn't exist.";

      // check for partition already mapped
      if (mapping[partitions[j]-1] != -2)
	throw myexception()<<"Trying to set '"<<key<<"' for partition "<<partitions[j]<<" twice.";

      // map partition to this model
      mapping[partitions[j]-1] = index;
    }
  }

  // fill in default model mappings
  for(int i=0;i<mapping.size();i++)
    if (mapping[i] == -2) 
    {
      mapping[i] = model_names.size();
      model_names.push_back("");
    }

  return shared_items<string>(model_names,mapping);
}

vector<polymorphic_cow_ptr<substitution::MultiModel> > 
get_smodels(const variables_map& args, const vector<alignment>& A,
	    const shared_items<string>& smodel_names_mapping)
{
  vector<polymorphic_cow_ptr<substitution::MultiModel> > smodels;
  for(int i=0;i<smodel_names_mapping.n_unique_items();i++) 
  {
    vector<alignment> alignments;
    for(int j=0;j<smodel_names_mapping.n_partitions_for_item(i);j++)
      alignments.push_back(A[smodel_names_mapping.partitions_for_item[i][j]]);

    OwnedPointer<substitution::MultiModel> full_smodel = get_smodel(args,
								    smodel_names_mapping.unique(i),
								    alignments);
    polymorphic_cow_ptr<substitution::MultiModel> temp (*full_smodel);
    smodels.push_back(temp);
  }
  return smodels;
}

vector<polymorphic_cow_ptr<IndelModel> > 
get_imodels(const shared_items<string>& imodel_names_mapping)
{
  vector<polymorphic_cow_ptr<IndelModel> > imodels;
  for(int i=0;i<imodel_names_mapping.n_unique_items();i++) 
  {
    OwnedPointer<IndelModel> full_imodel = get_imodel(imodel_names_mapping.unique(i));

    polymorphic_cow_ptr<IndelModel> temp (*full_imodel);
    imodels.push_back(temp);
  }
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

string duration(time_t T)
{
  time_t total = T;
  unsigned long seconds = T%60;
  T = (T - seconds)/60;

  unsigned long minutes = T%60;
  T  = (T - minutes)/60;

  unsigned long hours = T%24;
  T  = (T - hours)/24;

  unsigned long days = T;

  string s = convertToString(total) + " seconds";

  if (not minutes) return s;

  s = convertToString(minutes) + "m " +
      convertToString(seconds) + "s  (" + s + ")";

  if (not hours) return s;

  s = convertToString(hours) + "h " + s;

  if (not days) return s;

  s = convertToString(days) + "days " + s;

  return s;
}

void raise_cpu_limit(ostream& o)
{
  rlimit limits;

  getrlimit(RLIMIT_CPU,&limits);

  o<<endl;
  o<<"OLD cpu time limits = "<<rlim_minutes(limits.rlim_cur)<<" / "<<rlim_minutes(limits.rlim_max)<<endl;

  limits.rlim_cur = RLIM_INFINITY;

  setrlimit(RLIMIT_CPU,&limits);
  getrlimit(RLIMIT_CPU,&limits);

  o<<"NEW cpu time limits = "<<rlim_minutes(limits.rlim_cur)<<" / "<<rlim_minutes(limits.rlim_max)<<endl;
}
#else
void raise_cpu_limit(ostream& o) 
{
  o<<"Not checking CPU time limits..."<<endl;
}
#endif

void my_gsl_error_handler(const char* reason, const char* file, int line, int gsl_errno)
{
  const int max_errors=100;
  static int n_errors=0;

  if (n_errors < max_errors) {
  
    std::cerr<<"gsl: "<<file<<":"<<line<<" (errno="<<gsl_errno<<") ERROR:"<<reason<<endl;
    n_errors++;
    if (n_errors == max_errors)
      std::cerr<<"gsl: "<<max_errors<<" errors reported - stopping error messages."<<endl;
  }

  //  std::abort();
}

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

time_t start_time = time(NULL);

void show_ending_messages()
{
  time_t end_time = time(NULL);
  
  cout<<endl;
  cout<<"start time: "<<ctime(&start_time)<<endl;
  cout<<"  end time: "<<ctime(&end_time)<<endl;
  cout<<"total time: "<<duration(end_time-start_time)<<endl;
  cout<<endl;
  cout<<"total likelihood evals = "<<substitution::total_likelihood<<endl;
  cout<<"total calc_root_prob evals = "<<substitution::total_calc_root_prob<<endl;
  cout<<"total branches peeled = "<<substitution::total_peel_branches<<endl;
}

void die_on_signal(int sig)
{
  // Throwing exceptions from signal handlers is not allowed.  Bummer.
  cout<<"recieved signal "<<sig<<".  Dying."<<endl;
  cerr<<"recieved signal "<<sig<<".  Dying."<<endl;

  show_ending_messages();

  exit(3);
}

int main(int argc,char* argv[])
{ 

  TKF1_Transducer Q(false);

  Q.get_branch_Transducer(1.0);

  FS_Transducer Q_FS(false);

  Q_FS.get_branch_Transducer(1.0);
  
  //  exit(0);

  int n_procs = 1;
  int proc_id = 0;

#ifdef HAVE_MPI
  mpi::environment env(argc, argv);
  mpi::communicator world;

  proc_id = world.rank();
  n_procs = world.size();
#endif

  std::ios::sync_with_stdio(false);

  ostream out_screen(cout.rdbuf());
  ostream err_screen(cerr.rdbuf());

  std::ostringstream out_cache;
  std::ostringstream err_cache;

  teebuf tee_out(out_screen.rdbuf(), out_cache.rdbuf());
  teebuf tee_err(err_screen.rdbuf(), err_cache.rdbuf());

  ostream out_both(&tee_out);
  ostream err_both(&tee_err);

  int retval=0;

  try {
#if defined(HAVE_FENV_H) && !defined(NDEBUG)
    feenableexcept(FE_DIVBYZERO|FE_OVERFLOW|FE_INVALID);
#endif
    fp_scale::initialize();
    fs::path::default_name_check(fs::portable_posix_name);

    gsl_set_error_handler(&my_gsl_error_handler);

    //---------- Parse command line  ---------//
    variables_map args = parse_cmd_line(argc,argv);

    //------ Capture copy of 'cerr' output in 'err_cache' ------//
    if (not args.count("show-only")) {
      cerr.rdbuf(err_both.rdbuf());
    }
    else {
      if (proc_id) return 0;
    }

    //---------- Determine Data dir ---------------//
    check_data_dir(args["data-dir"].as<string>());

    //------ Capture copy of 'cerr' output in 'err_cache' ------//
    if (not args.count("show-only"))
      cerr.rdbuf(err_both.rdbuf());

    //---------- Initialize random seed -----------//
    unsigned long seed = init_rng_and_get_seed(args);
    
    out_cache<<"random seed = "<<seed<<endl<<endl;

    //----------- Load alignment and tree ---------//
    vector<alignment> A;
    SequenceTree T;
    if (args.count("tree"))
      load_As_and_T(args,A,T);
    else
      load_As_and_random_T(args,A,T);

    vector<string> filenames = args["align"].as<vector<string> >();
    for(int i=0;i<filenames.size();i++) {
      out_cache<<"data"<<i+1<<" = "<<filenames[i]<<endl<<endl;
      out_cache<<"alphabet"<<i+1<<" = "<<A[i].get_alphabet().name<<endl<<endl;
    }
    for(int i=0;i<A.size();i++) {
      check_alignment_names(A[i]);
      check_alignment_values(A[i],filenames[i]);
    }

    //--------- Handle branch lengths <= 0 --------//
    sanitize_branch_lengths(T);

    //--------- Do we have enough sequences? ------//
    if (T.n_leaves() < 3)
      throw myexception()<<"At least 3 sequences must be provided - you provided only "<<T.n_leaves()<<".\n(Perhaps you have BLANK LINES in your FASTA file?)";

    //---------- Open output files -----------//
    vector<ostream*> files;
    if (not args.count("show-only")) {
      string dir_name="";
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
      files = init_files(proc_id, dir_name, argc, argv, A.size());
    }
    else {
      files.push_back(&cout);
      files.push_back(&cerr);
    }

    //------ Redirect output to files -------//
    ostream& s_out = *files[0];
    ostream& s_err = *files[1];

    s_out<<out_cache.str();
    s_err<<err_cache.str();

    tee_out.setbuf2(s_out.rdbuf());
    tee_err.setbuf2(s_err.rdbuf());

    cout.flush() ; cout.rdbuf(s_out.rdbuf());
    cerr.flush() ; cerr.rdbuf(s_err.rdbuf());
    clog.flush() ; clog.rdbuf(s_err.rdbuf());

    //--------- Set up the substitution model --------//
    shared_items<string> smodel_names_mapping = get_mapping(args, "smodel", A.size());
    
    vector<int> smodel_mapping = smodel_names_mapping.item_for_partition;

    vector<polymorphic_cow_ptr<substitution::MultiModel> > 
      full_smodels = get_smodels(args,A,smodel_names_mapping);

    if (args["letters"].as<string>() == "star")
      for(int i=T.n_leaves();i<T.n_branches();i++)
	T.branch(i).set_length(0);

    //-------------Choose an indel model--------------//
    vector<int> imodel_mapping(A.size(),-1);
    shared_items<string> imodel_names_mapping(vector<string>(),imodel_mapping);

    if (args.count("traditional")) {
      if (args.count("imodel"))
	throw myexception()<<"Error: you specified both --imodel <arg> and --traditional";
    }
    else {
      imodel_names_mapping = get_mapping(args, "imodel", A.size());

      imodel_mapping = imodel_names_mapping.item_for_partition;
    }

    vector<polymorphic_cow_ptr<TransducerIndelModel> > full_imodels;
    full_imodels.push_back(polymorphic_cow_ptr<TransducerIndelModel>(Q_FS));

    //-------------- Which partitions share a scale? -----------//
    shared_items<string> scale_names_mapping = get_mapping(args, "same-scale", A.size());

    vector<int> scale_mapping = scale_names_mapping.item_for_partition;

    //-------------Create the Parameters object--------------//
    Parameters P(A, T, full_smodels, smodel_mapping, full_imodels, imodel_mapping, scale_mapping);

    set_parameters(P,args);

    for(int i=0;i<P.n_data_partitions();i++) {
      s_out<<"smodel-index"<<i+1<<" = "<<smodel_mapping[i]<<endl;
      s_out<<"imodel-index"<<i+1<<" = "<<imodel_mapping[i]<<endl;
    }
    s_out<<endl;

    if (not P.smodel_full_tree)
      s_out<<"substitution model: *-tree"<<endl;

    for(int i=0;i<P.n_smodels();i++) 
      s_out<<"subst model"<<i+1<<" = "<<P.SModel(i).name()<<endl<<endl;

    for(int i=0;i<P.n_imodels();i++) 
      s_out<<"indel model"<<i+1<<" = "<<P.IModel(i).name()<<endl<<endl;

    //----------------- Tree-based constraints ----------------//
    if (args.count("t-constraint"))
      P.TC = cow_ptr<SequenceTree>(load_constraint_tree(args["t-constraint"].as<string>(), T.get_sequences()));

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

    //----- Initialize Likelihood caches and character index caches -----//
    for(int i=0;i<P.n_data_partitions();i++) {
      P[i].LC.set_length(P[i].A->length());

      add_leaf_seq_note(*P[i].A, T.n_leaves());
      add_subA_index_note(*P[i].A, T.n_branches());
      add_column_type_note(*P[i].A);
    }

    // Why do we need to do this, again?
    P.recalc_all();

    MCMC::MoveStats S;
    sample_alignment_rates(P,S);
    exit(0);

    //---------------Do something------------------//
    if (args.count("show-only"))
      print_stats(cout,cout,P);
    else {
#if !defined(_MSC_VER) && !defined(__MINGW32__)
      raise_cpu_limit(err_both);

      signal(SIGHUP,SIG_IGN);
      signal(SIGXCPU,SIG_IGN);

      
      struct sigaction sa_old;
      struct sigaction sa_new;
      sa_new.sa_handler = &die_on_signal;
      sigaction(SIGINT,&sa_new,&sa_old);
#endif

      long int max_iterations = args["iterations"].as<long int>();

      do_sampling(args,P,max_iterations,files);

      // Close all the streams, and write a notification that we finished all the iterations.
      // close_files(files);
    }
  }
  catch (std::bad_alloc&) {
    err_both<<"Doh!  Some kind of memory problem?\n";
    report_mem();
    retval=2;
  }
  catch (std::exception& e) {
    if (n_procs > 1)
      err_both<<"bali-phy: Error["<<proc_id<<"]! "<<e.what()<<endl;
    else
      err_both<<"bali-phy: Error! "<<e.what()<<endl;
    retval=1;
  }

  show_ending_messages();

  return retval;
}
