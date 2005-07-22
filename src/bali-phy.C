/* Version 2: based on operating on multiple alignments */

#include <cmath>
#include <iostream>
#include <valarray>
#include <new>

#include <boost/program_options.hpp>

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

namespace po = boost::program_options;
using po::variables_map;

using std::cin;
using std::cout;
using std::cerr;
using std::clog;
using std::endl;

using std::valarray;

void do_sampling(const variables_map& args,alignment& A,Parameters& P,long int max_iterations) 
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
			     MoveArgSingle("sample_alignments:alignment:alignment_branch",
					   sample_alignments_one,
					   branches)
			     );
  if (P.T.n_leaves() >2) {
    alignment_branch_moves.add(0.15,MoveArgSingle("sample_tri:alignment:alignment_branch:nodes",
						 sample_tri_one,
						 branches)
			       );
    alignment_branch_moves.add(0.1,MoveArgSingle("sample_tri_branch:alignment:nodes:length",
						 sample_tri_branch_one,
						 branches)
			       ,false);
  }
  alignment_moves.add(1, alignment_branch_moves, false);
  alignment_moves.add(1, SingleMove(walk_tree_sample_alignments, "walk_tree_sample_alignments:alignment:alignment_branch:nodes") );

  //---------- alignment::nodes_master (nodes_moves) ----------//
  MoveEach nodes_moves("nodes_master:nodes");
  if (P.T.n_leaves() >= 3)
    nodes_moves.add(10,MoveArgSingle("sample_node:alignment:nodes",
				   sample_node_move,
				   internal_nodes)
		   );
  if (P.T.n_leaves() >= 4)
    nodes_moves.add(1,MoveArgSingle("sample_two_nodes:alignment:nodes",
				   sample_two_nodes_move,
				   internal_nodes)
		   );

  alignment_moves.add(2, nodes_moves);

  //-------------------- tree (tree_moves) --------------------//
  MoveAll tree_moves("tree");
  MoveAll topology_move("topology");
  MoveEach NNI_move("NNI");
  MoveOne SPR_move("SPR");

  if (P.IModel().full_tree)
    NNI_move.add(1,MoveArgSingle("three_way_NNI:alignment:nodes:topology",
				 three_way_topology_sample,
				 internal_branches)
		 );
  else
    NNI_move.add(1,MoveArgSingle("three_way_NNI:topology",
				 three_way_topology_sample,
				 internal_branches)
		 );

  NNI_move.add(1,MoveArgSingle("two_way_NNI:alignment:nodes:topology",
				    two_way_topology_sample,
				    internal_branches)
		    ,false
		    );

  //FIXME - doesn't yet deal with gaps=star
  if (P.IModel().full_tree)
    NNI_move.add(0.025,MoveArgSingle("three_way_NNI_and_A:alignment:alignment_branch:nodes:topology",
				   three_way_topology_and_alignment_sample,
				     internal_branches)
		 ,false
		 );


  if (P.IModel().full_tree) {
    SPR_move.add(1,SingleMove(sample_SPR_flat,"SPR_and_A_flat:topology:lengths:nodes:alignment:alignment_branch"));
    SPR_move.add(1,SingleMove(sample_SPR_nodes,"SPR_and_A_nodes:topology:lengths:nodes:alignment:alignment_branch"));
  }
  else {
    SPR_move.add(1,SingleMove(sample_SPR_flat,"SPR_flat:topology:lengths"));
    SPR_move.add(1,SingleMove(sample_SPR_nodes,"SPR_and_A_nodes:topology:lengths"));
  }

  topology_move.add(1,NNI_move,false);
  topology_move.add(1,SPR_move);
  if (P.T.n_leaves() >3 and P.SModel().full_tree)
    tree_moves.add(1,topology_move);
  
  //-------------- tree::lengths (length_moves) -------------//
  MoveEach length_moves("lengths");
  MoveEach length_moves1("lengths1");

  length_moves1.add(1,MoveArgSingle("change_branch_length:lengths",
				   change_branch_length_move,
				   branches)
		   );
  length_moves1.add(1,MoveArgSingle("change_branch_length_multi:lengths",
				   change_branch_length_multi_move,
				   branches)
		   );
  if (P.SModel().full_tree)
    length_moves1.add(0.01,MoveArgSingle("change_branch_length_and_T:lengths:nodes:topology",
					change_branch_length_and_T,
					internal_branches)
		      );
  length_moves.add(1,length_moves1);

  tree_moves.add(1,length_moves,false);
  tree_moves.add(1,SingleMove(sample_NNI_and_branch_lengths,"NNI_and_lengths:topology:lengths"));

  //------------- parameters (parameters_moves) --------------//
  MoveAll parameter_moves("parameters");
  parameter_moves.add(4+P.T.n_branches()/8,SingleMove(sample_frequencies,"frequencies:parameters"));
  parameter_moves.add(4+P.T.n_branches()/8,SingleMove(change_parameters,"s_parameters:parameters"));
  if (P.IModel().full_tree)
    parameter_moves.add(8+P.T.n_branches()/4,SingleMove(change_gap_parameters,"g_parameters:parameters"));
  

  int subsample = args["subsample"].as<int>();

  // full sampler
  Sampler sampler("sampler");
  if (P.IModel().full_tree)
    sampler.add(1,alignment_moves);
  sampler.add(2,tree_moves);
  sampler.add(1,parameter_moves);

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
  
  sampler.show_enabled();
  cout<<"\n";

  if (P.alignment_constraint.size1() > 0)
    std::cerr<<"Using "<<P.alignment_constraint.size1()<<" constraints.\n";

  valarray<bool> s2 = constraint_satisfied(P.alignment_constraint,A);
  valarray<bool> s1(false,s2.size());
  report_constraints(s1,s2);
  sampler.go(A,P,subsample,max_iterations);
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

void set_parameters(Parameters& P, const variables_map& args) {

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
    if (p=find_parameter(P.SModel(),fix[i]),p!=-1)
      P.SModel().fixed(p,true);
    else if (p=find_parameter(P.IModel(),fix[i]),p!=-1)
      P.IModel().fixed(p,true);
    else
      throw myexception()<<"Can't find parameter '"<<fix[i]<<"' to fix!";
  }

  // unfix parameters
  for(int i=0;i<unfix.size();i++) {
    int p=-1;
    if (p=find_parameter(P.SModel(),unfix[i]),p!=-1)
      P.SModel().fixed(p,false);
    else if (p=find_parameter(P.IModel(),unfix[i]),p!=-1)
      P.IModel().fixed(p,false);
    else
      throw myexception()<<"Can't find parameter '"<<unfix[i]<<"' to unfix!";
  }

  // set parameters
  for(int i=0;i<doset.size();i++) {
    //parse
    vector<string> parse = split(doset[i],'=');
    if (parse.size() != 2)
      throw myexception()<<"Ill-formed initial condition '"<<doset[i]<<"'.";

    string name = parse[0];
    double value = convertTo<double>(parse[1]);

    P.keys[name] = value;

    int p=-1;
    if (p=find_parameter(P.SModel(),name),p!=-1)
      P.SModel().parameter(p,value);
    else if (p=find_parameter(P.IModel(),name),p!=-1)
      P.IModel().parameter(p,value);
  }

}

variables_map parse_cmd_line(int argc,char* argv[]) 
{ 
  using namespace po;

  // named options
  options_description general("General options");
  general.add_options()
    ("help", "produce help message")
    ("version", "print version information")
    ("show-only","analyze the initial values and exit")
    ("seed", value<unsigned long>(),"random seed")
    ("data-dir", value<string>()->default_value("Data"),"data directory")
    ("align-constraint",value<string>(),"file with alignment constraints")
    ("with-stop","include stop codons in amino-acid alphabets")
    ("internal",value<string>(),"if set to '+', then make all internal node entries wildcards")
    ("gaps",value<string>()->default_value("full_tree"),"if set to 'star', then don't use indel information")
    ("letters",value<string>()->default_value("full_tree"),"if set to 'star', then use a star tree for substitution")
    ;
  
  options_description mcmc("MCMC options");
  mcmc.add_options()
    ("iterations",value<long int>()->default_value(100000),"the number of iterations to run")
    ("subsample",value<int>()->default_value(1),"factor by which to subsample")
    ("T",value<double>()->default_value(1.0),"MCMCMC temperature")
    ("enable",value<string>(),"comma-separated list of kernels to enable")
    ("disable",value<string>(),"comma-separated list of kernels to disable")
    ;
    

  options_description parameters("Parameter options");
  parameters.add_options()
    ("align", value<string>(),"file with sequences and initial alignment")
    ("tree",value<string>(),"file with initial tree")
    ("set",value<vector<string> >()->multitoken(),"set parameter=<value>")
    ("fix",value<vector<string> >()->multitoken(),"fix parameter[=<value>]")
    ("unfix",value<vector<string> >()->multitoken(),"un-fix parameter[=<value>]")
    ("randomize-alignment","randomly realign the sequences before use.")
    ("smodel",value<string>(),"substitution model")
    ("imodel",value<string>()->default_value("fragment-based+T"),"indel model: simple, fragment-based, or fragment-based+T")
    ;

  options_description smodel("Substitution model options");
  smodel.add_options()
    ("frequencies",value<string>(),"initial frequencies: 'uniform','nucleotides', or a comma-separated vector.") 
    ("alphabet",value<string>(),"specify the alphabet: DNA, RNA, Amino Acids, Triplets, or Codons")
    ;
  options_description all("All options");
  all.add(general).add(mcmc).add(parameters).add(smodel);

  // positional options
  positional_options_description p;
  p.add("align", 1);
  
  variables_map args;     
  store(command_line_parser(argc, argv).
	    options(all).positional(p).run(), args);
  notify(args);    

  if (args.count("version")) {
    cout<<"VERSION: 1.9.6+\nBUILD: "<<__DATE__<<"\n";
    exit(0);
  }

  if (args.count("help")) {
    cout<<"Usage: bali-phy <sequence-file> [OPTIONS]\n";
    cout<<all<<"\n";
    exit(0);
  }

  return args;
}


int main(int argc,char* argv[]) { 

  try {

    fp_scale::initialize();
    cerr.precision(10);
    cout.precision(10);
    std::ios::sync_with_stdio(false);

    //---------- Parse command line  -------//
    variables_map args = parse_cmd_line(argc,argv);
     
    //---------- Initialize random seed -----------//
    unsigned long seed = 0;
    if (args.count("seed")) {
      seed = args["seed"].as<unsigned long>();
      myrand_init(seed);
    }
    else
      seed = myrand_init();
    cout<<"random seed = "<<seed<<endl<<endl;
    
    //---------- Determine Data dir ---------------//
    {
      string filename = args["data-dir"].as<string>() + "/wag.dat";
      ifstream temp(filename.c_str());
      if (temp)
	temp.close();
      else {
	cerr<<"Warning: couldn't open file '"<<filename<<"'"<<endl;
	cerr<<"         Is '"<<args["data-dir"].as<string>()<<"' a valid Data/ directory?\n\n";
      }
    }
    
    //----------- Load alignment and tree ---------//
    alignment A;
    SequenceTree T;
    if (args.count("tree"))
      load_A_and_T(args,A,T);
    else
      load_A_and_random_T(args,A,T);

    cout<<"data = "<<args["align"].as<string>()<<endl<<endl;

    cout<<"alphabet = "<<A.get_alphabet().name<<endl<<endl;

    if (A.n_sequences() < 3)
      throw myexception()<<"At least 3 sequences must be provided - you provided only "<<A.n_sequences()<<".";

    //--------- Set up the substitution model --------//
    OwnedPointer<substitution::MultiModel> full_smodel = get_smodel(args,A);
    
    if (not full_smodel->full_tree)
      for(int i=T.n_leaves();i<T.n_branches();i++)
	T.branch(i).set_length(0);

    //-------------Choose an indel model--------------//
    OwnedPointer<IndelModel> imodel = get_imodel(args);
    
    //-------------Create the Parameters object--------------//
    Parameters P(*full_smodel,*imodel,T);
    cout<<"subst model = "<<P.SModel().name();
    if (not P.SModel().full_tree)
      cout<<", *-tree";
    cout<<endl<<endl;

    cout<<"indel model = "<<P.IModel().name();
    if (not P.IModel().full_tree)
      cout<<", *-tree";
    cout<<endl<<endl;

    P.alignment_constraint = load_alignment_constraint(args,A,T);

    P.Temp = args["T"].as<double>();

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
    P.recalc();

    //---------------Do something------------------//
    if (args.count("show-only"))
      print_stats(cout,cout,cout,cout,A,P,"Initial");
    else {
      long int max_iterations = args["iterations"].as<long int>();

      do_sampling(args,A,P,max_iterations);
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

