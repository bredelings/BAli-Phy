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
    
  /*------ Try to load tree -------------*/
  if (not args.set("tree")) {
    vector<string> s;
    for(int i=0;i<A.num_sequences();i++)
      s.push_back(A.seq(i).name);
    T = RandomTree(s,0.05);
  }
  else 
    T.read(args["tree"]);
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
    double lambda_O = -12;
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
    
    /*------------ Start the Sampling ---------------*/
    Parameters Theta(*smodel,lambda_O,lambda_E,T);
    MCMC sampler;
    sampler.add(sample_alignments,1,"sample_alignments:alignment");
    sampler.add(sample_nodes,1,"sample_nodes:nodes");
    sampler.add(sample_topologies,1,"sample_topologies:nodes:topology");
    sampler.add(change_branch_lengths,1,"change_branch_lengths:nodes:lengths:topology");
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

    sampler.iterate(A,Theta,1000000);
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
  }
  return 0;
}

