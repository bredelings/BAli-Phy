/* Version 2: based on operating on multiple alignments */

#include <cmath>
#include <iostream>
#include <valarray>
#include "myexception.H"
#include "mytypes.H"
#include "sequencetree.H"
#include "alignment.H"
#include "rng.H"
#include "sample.H"
#include "parameters.H"
#include "mcmc.H"
#include "likelihood.H"
#include "arguments.H"
#include "util.H"
#include "setup.H"

using std::cout;
using std::cin;

void do_showonly(const alignment& A,const Parameters& P) {
  double PS = P.likelihood(A,P);
  double PA = prior_HMM(A,P);
  double PT = prior(P.T,P.branch_mean);
  double PP = P.SModel().prior();

  cout<<"ln P(data,A,t,T,Theta) =  "<<PS + PA + PT + PP<<" = "
	   <<PS<<" + "
	   <<PA<<" + "
	   <<PT<<" + "
	   <<PP<<endl<<endl;

  cout<<A<<endl<<endl;
  cout<<P.T<<endl<<endl;
}


void do_sampling(Arguments& args,alignment& A,Parameters& P,long int max_iterations) {

  // args for branch-based stuff
  vector<int> branches(P.T.branches());
  for(int i=0;i<branches.size();i++)
    branches[i] = i;

  // args for branch-based stuff
  vector<int> internal_nodes;
  for(int i=P.T.leaves();i<P.T.num_nodes()-1;i++)
    internal_nodes.push_back(i);

  // args for branch-based stuff
  vector<int> internal_branches;
  for(int i=P.T.leaves();i<P.T.branches();i++)
    internal_branches.push_back(i);

  using namespace MCMC;

  //--------------- alignment -----------------//
  MoveAll alignment_moves("alignment");

  //--------------- alignment::alignment_branch -----------------//
  MoveEach alignment_branch_moves("alignment_branch:alignment");
  alignment_branch_moves.add(1.0,
			     MoveArgSingle("sample_alignments:alignment:alignment_branch",
					   sample_alignments_one,
					   branches)
			     );
  if (P.T.leaves() >2) {
    alignment_branch_moves.add(0.15,MoveArgSingle("sample_tri:alignment:alignment_branch:nodes",
						 sample_tri_one,
						 branches)
			       );
    alignment_branch_moves.add(0.1,MoveArgSingle("sample_tri_branch:alignment:nodes:length",
						 sample_tri_branch_one,
						 branches)
			       ,false);
  }
  alignment_moves.add(1, alignment_branch_moves);

  //---------- alignment::nodes_master (nodes_moves) ----------//
  MoveEach nodes_moves("nodes_master:nodes");
  if (P.T.leaves() >= 3)
    nodes_moves.add(10,MoveArgSingle("sample_node:alignment:nodes",
				   sample_node_move,
				   internal_nodes)
		   );
  if (P.T.leaves() >= 4)
    nodes_moves.add(1,MoveArgSingle("sample_two_nodes:alignment:nodes",
				   sample_two_nodes_move,
				   internal_nodes)
		   );

  alignment_moves.add(2, nodes_moves);

  //------------------- tree (tree_moves)--------------------//
  MoveAll tree_moves("tree");
  MoveAll topology_move("topology");
  MoveEach NNI_move("NNI");
  MoveEach SPR_move("SPR");

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
  NNI_move.add(1,MoveArgSingle("two_way_NNI_MH:alignment:nodes:topology",
				    two_way_topology_sample_MH,
				    internal_branches)
		    ,false
		    );
  NNI_move.add(1,MoveArgSingle("two_way_NNI2:alignment:nodes:topology",
				    two_way_topology_sample2,
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


  if (P.IModel().full_tree)
    SPR_move.add(1,MoveArgSingle("SPR_and_A:topology:lengths:nodes:alignment:alignment_branch",
				 sample_SPR,
				 branches)
		 );
  else
    SPR_move.add(1,MoveArgSingle("SPR_and_A:topology:lengths",
				 sample_SPR,
				 branches)
		 );

  topology_move.add(1,NNI_move);
  topology_move.add(0.04,SPR_move);
  if (P.T.leaves() >3 and P.SModel().full_tree)
    tree_moves.add(1,topology_move);
  
  //-------------- tree::lengths (length_moves) -------------//
  MoveEach length_moves("lengths");
  MoveEach length_moves1("lengths1");

  length_moves1.add(1,MoveArgSingle("change_branch_length:lengths",
				   change_branch_length_move,
				   branches)
		   );
  if (P.SModel().full_tree)
    length_moves1.add(0.01,MoveArgSingle("change_branch_length_and_T:lengths:nodes:topology",
					change_branch_length_and_T,
					internal_branches)
		      );
  length_moves.add(1,length_moves1);
  if (P.SModel().full_tree)
    length_moves.add(1,MoveArgSingle("slide_branch_length:lengths",
				     slide_branch_lengths_one,
				     branches)
		     );
  tree_moves.add(2,length_moves);

  //------------- parameters (parameters_moves) --------------//
  MoveAll parameter_moves("parameters");
  parameter_moves.add(4+P.T.branches()/8,SingleMove(change_parameters,"s_parameters:parameters"));
  parameter_moves.add(8+P.T.branches()/4,SingleMove(change_gap_parameters,"g_parameters:parameters"));
  parameter_moves.add(4+P.T.branches()/8,SingleMove(sample_frequencies,"frequencies:parameters"));
  

  int subsample = args.loadvalue("subsample",1);

  // full sampler
  Sampler sampler("sampler");
  if (P.IModel().full_tree)
    sampler.add(1,alignment_moves);
  sampler.add(2,tree_moves);
  sampler.add(1,parameter_moves);

  vector<string> disable;
  vector<string> enable;
  if (args.set("disable"))
    disable = split(args["disable"],':');
  if (args.set("enable"))
    enable = split(args["enable"],':');
  
  for(int i=0;i<disable.size();i++)
    sampler.disable(disable[i]);
  
  for(int i=0;i<enable.size();i++)
    sampler.enable(enable[i]);
  
  sampler.show_enabled();
  cout<<"\n";

  sampler.go(A,P,subsample,max_iterations);
}


int main(int argc,char* argv[]) { 
  try {
    //---------- Get input, from file if necessary -------//
    Arguments args;
    args.read(argc,argv);

    if (args.set("file")) {
      if (args["file"] == "-")
	args.read(cin);
      else {
	std::ifstream input(args["file"].c_str());
	if (not input)
	  throw myexception(string("Couldn't open file '")+args["file"]+"'");
	args.read(input);
	input.close();
      }
    }

    args.print(cout);
    
    //---------- Initialize random seed -----------//
    unsigned long seed = 0;
    if (args.set("seed")) {
      seed = convertTo<unsigned long>(args["seed"]);
      myrand_init(seed);
    }
    else
      seed = myrand_init();
    cout<<"random seed = "<<seed<<endl<<endl;
    
    std::cerr.precision(10);
    cout.precision(10);
    
    //---------- Determine Data dir ---------------//
    if (not args.set("datadir")) args["datadir"] = "Data";

    {
      string filename = args["datadir"] + "/wag.dat";
      ifstream temp(filename.c_str());
      if (temp)
	temp.close();
      else {
	std::cerr<<"Warning: couldn't open file '"<<filename<<"'"<<std::endl;
	std::cerr<<"         Is '"<<args["datadir"]<<"' a valid Data/ directory?\n";
      }
    }

    
    //----------- Load alignment and tree ---------//
    args["random_tree_ok"] = "yes";
    alignment A;
    SequenceTree T;
    load_A_and_T(args,A,T);

    cout<<"data = "<<args["align"]<<endl<<endl;

    cout<<"alphabet = "<<A.get_alphabet().name<<endl<<endl;

    //--------- Set up the substitution model --------//
    OwnedPointer<substitution::MultiModel> full_smodel = get_smodel(args,A);
    
    if (not full_smodel->full_tree)
      for(int i=T.leaves();i<T.branches();i++)
	T.branch(i).length() = 0;

    //-------------Choose an indel model--------------//
    OwnedPointer<IndelModel> imodel = get_imodel(args);
    
    //-------------Create the Parameters object--------------//
    Parameters P(*full_smodel,*imodel,T);
    cout<<"subst model = "<<P.SModel().name();
    if (not P.SModel().full_tree)
      cout<<",*-tree";
    cout<<endl<<endl;

    cout<<"indel model = "<<P.IModel().name();
    if (not P.IModel().full_tree)
      cout<<",*-tree";
    cout<<endl<<endl;

    P.Temp = args.loadvalue("T",1.0);

    P.constants[0] = args.loadvalue("bandwidth",100.0);
    if (args.set("pinning") and args["pinning"] == "enable")
      P.features |= (1<<0);
    if (args.set("banding") and args["banding"] == "enable")
      P.features |= (1<<1);

    //-------------- Specify fixed parameters ----------------//
    vector<string> fixed;
    if (args.set("fixed"))
      fixed = split(args["fixed"],':');

    for(int i=0;i<fixed.size();i++) {
      if (fixed[i].size() > 2 and fixed[i].substr(0,2) == "pS") {
	int pS = convertTo<int>(fixed[i].substr(2,fixed[i].size()-2));
	P.s_fixed[pS] = true;
      }
      else if (fixed[i].size() > 2 and fixed[i].substr(0,2) == "pI") {
	int pI = convertTo<int>(fixed[i].substr(2,fixed[i].size()-2));
	P.i_fixed[pI] = true;
      }
    }

    //---------------Do something------------------//
    if (args.set("showonly"))
      print_stats(cout,cout,cout,cout,A,P,"Initial");
    //      do_showonly(A,P);
    // FIXME - use print_stats?
    else {
      long int max_iterations = args.loadvalue("iterations",(long int)1000000);

      do_sampling(args,A,P,max_iterations);
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Error: "<<e.what()<<endl;
    exit(1);
  }

  return 0;
}

