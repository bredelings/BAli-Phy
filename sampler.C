/* Version 2: based on operating on multiple alignments */

#include <cmath>
#include <iostream>
#include <valarray>
#include "myexception.H"
#include "mytypes.H"
#include "tree.H"
#include "substitution.H"
#include "alignment.H"
#include "rng.H"
#include "sample.H"
#include "parameters.H"
#include "mcmc.H"
#include "likelihood.H"
#include "arguments.H"
#include "util.H"


vector<move_stat> move_stats;

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
  alphabet dna("DNA nucleotides","AGTC","N");
  alphabet rna("RNA nucleotides","AGUC","N");
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
    throw myexception(string("Alignment file") + args["align"] + "didn't contain any sequences!");
    
  if (args.set("randomize_alignment"))
    A = randomize(A);

  /*------ Try to load tree -------------*/
  if (not args.set("tree")) {
    vector<string> s;
    for(int i=0;i<A.num_sequences();i++)
      s.push_back(A.seq(i).name);
    T = RandomTree(s,0.05);
  }
  else 
    T.read(args["tree"]);

  /*------ Link Alignment and Tree ----------*/

  if (A.num_sequences() < T.leaves())
    throw myexception(string("Tree has ") + convertToString(T.leaves()) + "leaves but Alignment only has " + convertToString(A.num_sequences()) + "sequences.");

  vector<int> mapping(T.leaves());
  for(int i=0;i<T.leaves();i++) {
    int target = -1;
    for(int j=0;j<T.leaves();j++) {
      if (T.seq(i) == A.seq(j).name) {
	target = j;
	break;
      }
    }
    if (target == -1)
      throw myexception(string("Couldn't find sequence \"")+T.seq(i)+"\" in alignment");
    mapping[i] = target;
  }

  T.standardize(mapping);

  /*------- Fill in internal nodes ---------*/
  if (A.num_sequences() == T.num_nodes() - 1)
    ;
  else if (A.num_sequences() == T.leaves()) {
    sequence s(A.get_alphabet());
    s.resize(A.length());
    for(int column=0;column<A.length();column++)
      s[column] = alphabet::not_gap;
    for(int i=T.leaves();i<T.num_nodes()-1;i++)
      A.add_sequence(s);
  }
  else if (A.num_sequences() > T.num_nodes())
    throw myexception(string("More sequences than tree nodes!"));
  else
    throw myexception(string("Not enough ancestral sequences!"));


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
  double PS = substitution(A,P);
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
  MCMC sampler;
  sampler.add(sample_alignments,1,"sample_alignments:alignment");

  if (P.T.leaves() >2) {
    sampler.add(sample_nodes,1,"sample_nodes:nodes");
    sampler.add(sample_tri,1,"sample_tri:alignment:nodes:tri");
  }

  if (P.T.leaves() >3)
    sampler.add(sample_topologies,1,"sample_topologies:nodes:topology");

  sampler.add(change_branch_lengths,0.5,"change_branch_lengths:nodes:lengths:topology");

  sampler.add(slide_branch_lengths,0.5,"slide_branch_lengths:lengths");

  sampler.add(change_parameters,1,"change_parameters:parameters");

  //FIXME - maybe just store name in object, use vector, search for name?
  //FIXME - make MCMC inherit from the collection of moves.
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
  
  for(int i=0;i<sampler.moves.size();i++) {
    std::cout<<"move "<<sampler.moves[i].attributes[0]<<": ";
    if (sampler.moves[i].enabled)
      std::cout<<"enabled.\n";
    else 
      std::cout<<"DISABLED.\n";
  }
  std::cout<<"\n";
  
  /*-----------Load Stat Counters-------------*/
  move_stats.push_back(move_stat("t-sample-normal"));
  move_stats.push_back(move_stat("t-sample-branch-based"));
  move_stats.push_back(move_stat("length-sample"));
  move_stats.push_back(move_stat("length-sample-non-negative"));
  move_stats.push_back(move_stat("length-sample-slide"));


  sampler.iterate(A,P,max_iterations);
}


int main(int argc,char* argv[]) { 
  try {
    Arguments args;
    args.read(argc,argv);
    args.print(std::cout);

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
    
    /*------- Nucleotide Substitution Models -------*/
    alphabet dna("DNA nucleotides","AGTC","N");
    
    EQU EQU_dna(dna);
    HKY HKY_dna(dna);
    
    /*------- Nucleotide Substitution Models -------*/
    alphabet rna("RNA nucleotides","AGUC","N");
    
    EQU EQU_rna(rna);
    HKY HKY_rna(rna);
    
    /*------- Amino Acid Substitution Models -------*/
    alphabet amino_acids("Amino Acids","ARNDCQEGHILKMFPSTWYV","X");
    
    EQU EQU_animo(amino_acids);
    Empirical WAG(amino_acids,"Data/wag.dat");
    
    /*----------- Load alignment and tree ---------*/
    alignment A;
    SequenceTree T;
    do_setup(args,A,T);
    
    /*------------ Specify Gap Penalties ----------*/
    double lambda_O = -8;
    double lambda_E = -1;
    
    if (args.set("lambda_O")) lambda_O = convertTo<double>(args["lambda_O"]);
    if (args.set("lambda_E")) lambda_E = convertTo<double>(args["lambda_E"]);
    
    std::cout<<"lambda_O = "<<lambda_O<<"  lambda_E = "<<lambda_E<<endl<<endl;
    
    /*--------- Set up the substitution model --------*/
    SubstitutionModel* smodel = 0;
    
    if (A.get_alphabet() == dna)
      smodel = &HKY_dna;
    else if (A.get_alphabet() == rna)
      smodel = &HKY_rna;
    else if (A.get_alphabet() == amino_acids)
      smodel = &WAG;
    else
      assert(0);
    
    std::cout<<"Using alphabet: "<<A.get_alphabet().name<<endl<<endl;
    smodel->frequencies(empirical_frequencies(A));
    
    /*-------------Choose an indel model--------------*/
    int IMlength = 500;    //FIXME - perhaps we should choose \tau here
    if (IMlength < A.length()*3)
      IMlength = A.length()*3;
    IndelModel1 IM1(IMlength,lambda_O,lambda_E);
    IndelModel2 IM2(IMlength,lambda_O,lambda_E);
    IndelModel* imodel = &IM2;
    if (args.set("Imodel") and args["Imodel"] == "ordered")
      imodel= &IM1;

    Parameters Theta(*smodel,*imodel,T);


    /*---------------Do something------------------*/
    if (args.set("showonly"))
      do_showonly(A,Theta);
    else {
      long int max_iterations = 1000000;
      if (args.set("iterations"))
	max_iterations = convertTo<long int>(args["iterations"]);
      do_sampling(args,A,Theta,max_iterations);
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
  }
  return 0;
}

