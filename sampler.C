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

// 5. Read Marc's references on actually altering the tree

// 8. Use ublas::matrix<double>(a.size()) instead of valarray<double> in substitution.C

// 13. Put letters in the rate matrix file

// 16. *Better* output statistics?  Specify species to look at on command line?

// 20. How can we show conservation in the graph? (for 2 species?  more?)

// 28. Make sampling routines return P(Alignment|Data,Tree, etc)
//     Check to make sure that this is proportional to the likelihood...
void do_setup(Arguments& args,alignment& A,SequenceTree& T)
{
  /* ----- Alphabets to try ------ */
  alphabet dna("DNA nucleotides","AGTC","NYR");
  alphabet rna("RNA nucleotides","AGUC","NYR");
  alphabet amino_acids("Amino Acids","ARNDCQEGHILKMFPSTWYV","X");

  /* ----- Try to load alignment ------ */
  if (not args.set("align")) 
    throw myexception("Alignment file not specified! (align=<filename>)");

  try {
    A.load(dna,args["align"]);
  }
  catch (bad_letter& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;

    try {
      A.load(rna,args["align"]);
    }
    catch (bad_letter& e) {
      std::cerr<<"Exception: "<<e.what()<<endl;
      
      A.load(amino_acids,args["align"]);
    }
  }

  remove_empty_columns(A);

  if (A.num_sequences() == 0) 
    throw myexception(string("Alignment file ") + args["align"] + "didn't contain any sequences!");
    
  if (args.set("randomize_alignment"))
    A = randomize(A);

  /*------ Try to load tree -------------*/
  if (not args.set("tree")) {
    // FIXME - assumes that alignment doesn't specify internal nodes...
    vector<string> s;
    for(int i=0;i<A.num_sequences();i++)
      s.push_back(A.seq(i).name);
    T = RandomTree(s,0.05);
  }
  else 
    T.read(args["tree"]);

  /*------ Link Alignment and Tree ----------*/
  link(A,T);

  /*-------- Analyze 'internal'-------*/
  if (args.set("internal")) {
    if (args["internal"] == "+")
      for(int column=0;column< A.length();column++) {
	for(int i=T.leaves();i<A.size2();i++) 
	  A(column,i) = alphabet::not_gap;
      }
    else if (args["internal"] == "search")
      assert(0); // FIXME - not programmed yet
    else if (args["internal"] == "guess") 
      for(int column=0;column< A.length();column++) {
	vector<int> present_leaf(T.leaves());
	for(int i=0;i<T.leaves();i++)
	  present_leaf[i] = not A.gap(column,i);
	TreeFunc<int> present = mark_tree(present_leaf,T);
	for(int i=T.leaves();i<A.size2();i++) {
	  if (present(i))
	    A(column,i) = alphabet::not_gap;
	  else
	    A(column,i) = alphabet::gap;
	}
      }
  }

}

void do_showonly(const alignment& A,const Parameters& P) {
  double PS = P.likelihood(A,P);
  double PA = prior_HMM(A,P);
  double PT = prior(P.T,P.branch_mean);
  double PP = P.SModel().prior();

  std::cout<<"ln P(data,A,t,T,Theta) =  "<<PS + PA + PT + PP<<" = "
	   <<PS<<" + "
	   <<PA<<" + "
	   <<PT<<" + "
	   <<PP<<endl<<endl;

  std::cout<<A<<endl<<endl;
  std::cout<<P.T<<endl<<endl;
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

  // alignment
  MoveAll alignment_moves("alignment");

  // alignment :: alignment_branch
  MoveEach alignment_branch_moves("alignment_branch");
  alignment_branch_moves.add(0.5,
			     MoveArgSingle("sample_alignments:alignment",
					   sample_alignments_one,
					   branches)
			     );
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

  // aligment :: sample_nodes
  MoveEach internal_nodes_move("nodes_master:nodes");
  MoveArgSingle nodes_move2("sample_nodes2:alignment:nodes",sample_nodes2_one,internal_nodes);
  internal_nodes_move.add(0.01,nodes_move2);
  internal_nodes_move.add(1,MoveArgSingle("two_nodes:nodes",
					  sample_two_nodes_move,
					  internal_branches)
			  ,false);

  if (P.T.leaves() >2)
    alignment_moves.add(1,internal_nodes_move);

  // tree
  MoveAll tree_moves("tree");
  MoveEach topology_move("topology");
  //  topology_move.add(1,MoveArgSingle("sample_topologies:nodes:topology",
  //				    sample_topology,
  //				    internal_branches)
  //		    ,false);
  topology_move.add(1,MoveArgSingle("sample_topologies2:nodes:topology",
				    three_way_topology_sample,
				    internal_branches)
		    );

  if (P.T.leaves() >3)
    tree_moves.add(1,topology_move);
  
  // tree :: lengths
  MoveEach length_moves("lengths");
  MoveEach length_moves1("lengths1");

  length_moves1.add(1,MoveArgSingle("change_branch_length:length",
				   change_branch_length_move,
				   branches)
		   );
  if (P.SModel().full_tree)
    length_moves1.add(100,MoveArgSingle("change_branch_length_and_T:length:nodes:topology",
					change_branch_length_and_T,
					internal_branches)
		      );
  length_moves.add(1,length_moves1);
  if (P.SModel().full_tree)
    length_moves.add(1,MoveArgSingle("slide_branch_length:length",
				     slide_branch_lengths_one,
				     branches)
		     );
  tree_moves.add(1,length_moves);

  // parameters
  MoveAll parameter_moves("parameters");
  parameter_moves.add(P.T.branches(),SingleMove(change_parameters,"s_parameters:parameters"));
  parameter_moves.add(1+P.T.branches()/3,SingleMove(change_gap_parameters,"g_parameters:parameters"));
  

  //FIXME - use the right prior and likelihood here!
  // full sampler
  Sampler sampler("sampler");
  sampler.add(1,alignment_moves);
  sampler.add(1,tree_moves);
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
  std::cout<<"\n";

  sampler.go(A,P,max_iterations);
}


int main(int argc,char* argv[]) { 
  try {
    /*---------- Get input, from file if necessary -------*/
    Arguments args;
    args.read(argc,argv);

    if (args.set("file")) {
      if (args["file"] == "-")
	args.read(std::cin);
      else {
	std::ifstream input(args["file"].c_str());
	if (not input)
	  throw myexception(string("Couldn't open file '")+args["file"]+"'");
	args.read(input);
	input.close();
      }
    }

    args.print(std::cout);
    
    /*---------- Initialize random seed -----------*/
    unsigned long seed = 0;
    if (args.set("seed")) {
      seed = convertTo<unsigned long>(args["seed"]);
      myrand_init(seed);
    }
    else
      seed = myrand_init();
    std::cout<<"random seed = "<<seed<<endl<<endl;
    
    std::cerr.precision(10);
    std::cout.precision(10);
    
    /*------- Which parameters are fixed -------*/
    vector<string> fixed;
    if (args.set("fixed"))
      fixed = split(args["fixed"],':');

    /*----------- Load alignment and tree ---------*/
    alignment A;
    SequenceTree T;
    do_setup(args,A,T);
    
    /*------------ Specify Gap Penalties ----------*/
    double lambda_O = -8;
    if (args.set("lambda_O")) lambda_O = convertTo<double>(args["lambda_O"]);

    double lambda_E = lambda_O/10.0;
    if (args.set("lambda_E")) lambda_E = convertTo<double>(args["lambda_E"]);
    
    std::cout<<"lambda_O = "<<lambda_O<<"  lambda_E = "<<lambda_E<<endl<<endl;
    
    /*--------- Set up the substitution model --------*/
    substitution::MultiRateModel *full_smodel = get_smodel(args,A);
    
    if (not full_smodel->full_tree)
      for(int i=T.leaves();i<T.branches();i++)
	T.branch(i).length() = 0;

    /*-------------Choose an indel model--------------*/
    IndelModel* imodel = 0;

    if (args["imodel"] == "ordered") {
      std::cout<<"imodel = ordered\n";
      imodel = new IndelModel1(lambda_O,lambda_E);
    }
    else if (args["imodel"] == "single_indels") {
      std::cout<<"imodel = single indels\n";
      imodel = new SingleIndelModel(lambda_O);
    }
    else if (args["imodel"] == "upweighted") {
      std::cout<<"imodel = adjacent gaps upweighted by 2\n";
      imodel = new UpweightedIndelModel(lambda_O,lambda_E);
    }
    else {
      std::cout<<"imodel = symmetric\n";
      imodel = new IndelModel2(lambda_O,lambda_E);
      for(int i=0;i<fixed.size();i++) {
	if (fixed[i] == "beta")
	  imodel->fixed[2] = true;
      }
    }
    if (args["gaps"]== "star") {
      imodel->full_tree = false;
    }
    else
      imodel->full_tree = true;
    
    /*-------------Create the Parameters object--------------*/
    Parameters P(*full_smodel,*imodel,T);
    std::cout<<"Using alphabet: "<<A.get_alphabet().name<<endl<<endl;
    std::cout<<"Using substitution model: "<<P.SModel().name()<<endl;
    std::cout<<"Full tree for substitution: "<<P.SModel().full_tree<<endl<<endl;
    std::cout<<"Full tree for gaps: "<<P.IModel().full_tree<<endl<<endl;

    if (args.set("pinning") and args["pinning"] == "enable") {
      P.features |= (1<<0);
      double bandwidth = 100;
      if (args.set("bandwidth"))
	bandwidth = convertTo<double>(args["bandwidth"]);
      P.constants[0] = bandwidth;
    }

    /*---------------Do something------------------*/
    if (args.set("showonly"))
      do_showonly(A,P);
    else {
      long int max_iterations = 1000000;

      if (args.set("iterations"))
	max_iterations = convertTo<long int>(args["iterations"]);
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

