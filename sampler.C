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
  MoveEach alignment_branch_moves("alignment_branch");
  alignment_branch_moves.add(0.5,
			     MoveArgSingle("sample_alignments:alignment",
					   sample_alignments_one,
					   branches)
			     ,false);
  alignment_branch_moves.add(0.5,
			     MoveArgSingle("sample_alignments2:alignment",
					   sample_alignments2_one,
					   branches)
			     );
  if (P.T.leaves() >2) {
    alignment_branch_moves.add(0.15,MoveArgSingle("sample_tri:alignment:nodes",
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
    nodes_moves.add(1,MoveArgSingle("sample_two_nodes:nodes",
				   sample_two_nodes_move,
				   internal_branches)
		   );

  alignment_moves.add(2, nodes_moves);

  //------------------- tree (tree_moves)--------------------//
  MoveAll tree_moves("tree");
  MoveAll topology_move("topology");
  MoveEach NNI_move("NNI");
  MoveEach SPR_move("SPR");

  NNI_move.add(1,MoveArgSingle("three_way_NNI:nodes:topology",
				    three_way_topology_sample,
				    internal_branches)
	       );
  NNI_move.add(1,MoveArgSingle("two_way_NNI:nodes:topology",
				    two_way_topology_sample,
				    internal_branches)
		    ,false
		    );
  NNI_move.add(1,MoveArgSingle("two_way_NNI_MH:nodes:topology",
				    two_way_topology_sample_MH,
				    internal_branches)
		    ,false
		    );
  NNI_move.add(1,MoveArgSingle("two_way_NNI2:nodes:topology",
				    two_way_topology_sample2,
				    internal_branches)
		    ,false
		    );

  //FIXME - doesn't yet deal with gaps=star
  if (P.IModel().full_tree)
    NNI_move.add(0.05,MoveArgSingle("three_way_NNI_and_A:alignment:nodes:topology",
				   three_way_topology_and_alignment_sample,
				   internal_branches)
		 );


  // FIXME - awkward...
  // We don't mention 'alignment' because this stops messing with the alignment if gaps=star...
  SPR_move.add(1,MoveArgSingle("SPR_and_node:topology:length:nodes",
			       sample_SPR,
			       branches)
	       );

  if (P.IModel().full_tree) 
    SPR_move.add(1,MoveArgSingle("SPR_and_node:nodes:topology:length:alignment",
				 sample_SPR,
				 branches)
		 );

  topology_move.add(1,NNI_move);
  topology_move.add(0.4,SPR_move);
  if (P.T.leaves() >3 and P.SModel().full_tree)
    tree_moves.add(1,topology_move);
  
  //-------------- tree::lengths (length_moves) -------------//
  MoveEach length_moves("lengths");
  MoveEach length_moves1("lengths1");

  length_moves1.add(1,MoveArgSingle("change_branch_length:length",
				   change_branch_length_move,
				   branches)
		   );
  if (P.SModel().full_tree)
    length_moves1.add(0.01,MoveArgSingle("change_branch_length_and_T:length:nodes:topology",
					change_branch_length_and_T,
					internal_branches)
		      );
  length_moves.add(1,length_moves1);
  if (P.SModel().full_tree)
    length_moves.add(1,MoveArgSingle("slide_branch_length:length",
				     slide_branch_lengths_one,
				     branches)
		     );
  tree_moves.add(2,length_moves);

  //------------- parameters (parameters_moves) --------------//
  MoveAll parameter_moves("parameters");
  parameter_moves.add(P.T.branches(),SingleMove(change_parameters,"s_parameters:parameters"));
  parameter_moves.add(1+P.T.branches()/3,SingleMove(change_gap_parameters,"g_parameters:parameters"),false);
  

  int subsample = args.loadvalue("subsample",1);

  // full sampler
  Sampler sampler("sampler");
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
    /*---------- Get input, from file if necessary -------*/
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
    
    /*---------- Initialize random seed -----------*/
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
    
    /*----------- Load alignment and tree ---------*/
    alignment A;
    SequenceTree T;
    load_A_and_T(args,A,T,true);

    if (args.set("randomize_alignment"))
      A = randomize(A);
    
    /*------------ Specify Gap Penalties ----------*/
    double lambda_O = args.loadvalue("lambda_O",-5);

    double lambda_E = args.loadvalue("lambda_E",lambda_O/10.0);
    
    cout<<"lambda_O = "<<lambda_O<<"  lambda_E = "<<lambda_E<<endl<<endl;
    
    /*--------- Set up the substitution model --------*/
    substitution::MultiRateModel *full_smodel = get_smodel(args,A);
    
    if (not full_smodel->full_tree)
      for(int i=T.leaves();i<T.branches();i++)
	T.branch(i).length() = 0;

    /*-------------Choose an indel model--------------*/
    IndelModel* imodel = 0;

    if (args["imodel"] == "ordered") {
      cout<<"imodel = ordered\n";
      imodel = new IndelModel1(lambda_O,lambda_E);
    }
    else if (args["imodel"] == "single_indels") {
      cout<<"imodel = single indels\n";
      imodel = new SingleIndelModel(lambda_O);
    }
    else if (args["imodel"] == "upweighted") {
      cout<<"imodel = adjacent gaps upweighted by 2\n";
      imodel = new UpweightedIndelModel(lambda_O,lambda_E);
    }
    else {
      cout<<"imodel = symmetric\n";
      imodel = new IndelModel2(lambda_O,lambda_E);
    }
    if (args["gaps"]== "star") {
      imodel->full_tree = false;
    }
    else
      imodel->full_tree = true;
    
    //-------------Create the Parameters object--------------//
    cout<<"using smodel: "<<full_smodel->name()<<endl;

    Parameters P(*full_smodel,*imodel,T);
    cout<<"Using alphabet: "<<A.get_alphabet().name<<endl<<endl;
    cout<<"Using substitution model: "<<P.SModel().name()<<endl;
    cout<<"Full tree for substitution: "<<P.SModel().full_tree<<endl<<endl;
    cout<<"Full tree for gaps: "<<P.IModel().full_tree<<endl<<endl;

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

    // this isn't quite right in case of exceptions...
    delete imodel;
    delete full_smodel;
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
    exit(1);
  }

  return 0;
}

