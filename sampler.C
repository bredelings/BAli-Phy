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


/**** TODO:  A. check likelihoods against someone else
             D. Benchmark (w,w/o fast-math?)
             B. Normalize/Parameterize rate matrix?

             C. Write code to change tree topology
	        (Need an interface so that node NAME doesn't change!)
 ****/


// 3. write a routine to load the Phylip files -> check out CAR.phy

// 5. Read Marc's references on actually altering the tree

// 7. Can we somehow ESTIMATE lambda_O and lambda_E?

// 8. Use ublas::matrix<double>(a.size()) instead of valarray<double> in substitution.C

// 10. Why is the distribution of log scores so WIDE?
//     Why are we often so much below the ML?

// 13. Put letters in the rate matrix file

// 16. *Better* output statistics?  Specify species to look at on command line?

// 20. How can we show conservation in the graph? (for 2 species?  more?)

// 24. Measure the correlation time - see how sorting by center of mass of alignment
//    look at the probability that an aligned node pair at time t is still aligned
//    at time t+deltat

//     and moving from k->N-k decreases it...
//
//     Perhaps we could output more samples than we are now...


// 25. Benchmark, and see how the speed depends on sequence length

// 28. Make sampling routines return P(Alignment|Data,Tree, etc)
//     I'd NEED to do this in order to use the value anyway -> it should
//     only return things on the order of lambda_e and lambda_o...
//     modify the 'choose' routine.

//     Check to make sure that this is proportional to the likelihood...
//     This is important, because while the probability ratio between 2
//      alignment won't change, the ratio between 2 likelihood levels
//      WILL change!  So this will change at least the "plot 'logp'"
//      graphs!


// How to check these samplers, and find cases where they are not working???
// These seems to be something in the tail region of the 5S rna data that is triggering a bug

int main(int argc,char* argv[]) {

  myrand_init();

  std::cerr.precision(10);
  std::cout.precision(10);

  /******* Nucleotide Substitution Model *******/
  alphabet nucleotides("AGTC");

  std::valarray<double> f(0.0,4);
  f[0] = 0.25;f[1]=0.25;f[2]=0.25;f[3]=0.25;
  
  HKY smodel1(nucleotides,2.0,f);
  EQU smodel2(nucleotides);

  /******* Amino Acid Substitution Model *******/
  alphabet amino_acids("ARNDCQEGHILKMFPSTWYV");

  EQU smodel3(amino_acids);
  Empirical smodel4(amino_acids,"../Data/wag.dat");

  /**********  Set up the alignment ***********/
  alignment A;


  /* ----- Parse command line ------------*/
  if (argc != 5) {
    std::cerr<<"Usage: "<<argv[0]<<" <alignment file.fasta> <tree file> <lambda_O> <lambda_E>\n";
    exit(1);
  }

  ifstream file(argv[1]);
  if (not file) {
    std::cerr<<"Error: can't open alignment file '"<<argv[1]<<"'"<<endl;
    std::cerr<<"Usage: "<<argv[0]<<" <alignment file.fasta> <tree file> <lambda_O> <lambda_E>\n";
    exit(1);
  }

  ifstream file2(argv[2]);
  if (not file2) {
    std::cerr<<"Error: can't open tree file '"<<argv[2]<<"'"<<endl;
    std::cerr<<"Usage: "<<argv[0]<<" <alignment file.fasta> <tree file> <lambda_O> <lambda_E>\n";
    exit(1);
  }

  double lambda_O=0;
  double lambda_E=0;
  {
    std::stringstream A1(argv[3]);
    A1>>lambda_O;
    std::stringstream A2(argv[4]);
    A2>>lambda_E;
    std::cerr<<"lambda_O = "<<lambda_O<<"  lambda_E = "<<lambda_E<<endl;
  }


  // FIXME: Can't construct trees w/ branch lengths right now

  /*********** Start the sampling ***********/
  std::cerr.precision(10);
  std::cout.precision(10);

  SubstitutionModel* smodel = &smodel1;
  
  try {
    try {
      A.load_fasta(nucleotides,file);
    }
    catch (bad_letter& e) {
      file.close();
      file.open(argv[1]);
      std::cerr<<"Exception: "<<e.what()<<endl;
      A.load_fasta(amino_acids,file);
      smodel = &smodel4;
    }
    if (A.num_sequences() == 0) {
      std::cerr<<"Alignment file \""<<argv[1]<<"\" didn't  contain any sequences!\n";
      exit(1);
    }
    
    SequenceTree T1(file2);

    {
      std::cout<<"rate matrix = \n";
      for(int i=0;i<smodel->rates().size1();i++) {
	for(int j=0;j<smodel->rates().size2();j++) 
	  std::cout<<smodel->rates()(i,j)<<" ";
	std::cout<<endl;
      }
      
      Parameters Theta(*smodel,lambda_O,lambda_E,T1);
      MCMC(A,Theta,50000,probability3);
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
  }
  return 0;
}

