/* Version 2: based on operating on multiple alignments */

#include <cmath>
#include <iostream>
#include <valarray>
#include "myexception.H"
#include "mytypes.H"
#include "tree.H"
#include "substitution.H"
#include "alignment.H"
#include "myrandom.H"
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

// 17. Output an ML phylogeny?

// 18. Normalize rate matrix so that Sum(m) lambda(m,m)*pi(m) = -1

// 20. How can we show conservation in the graph? (for 2 species?  more?)

// 22. ALL internal-node-resampling is 1D!  We can resample adjacent
//       internal nodes simultaneously w/ a 1D algorithm!
//     IS there a good way to just integrate over all internal node state?
//       The state space is probably at least 2^(#nodes) though.

// 23. Finish writing the new branch-length MH sampler

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

int main(int argc,char* argv[]) {

  myrand_init();
  alphabet nucleotides("AGTC");
  
  //  alphabet amino_acids("ARNDCQEGHILKMFPTWYV","PAM");

  /**********  Set up the alignment ***********/
  alignment A;

  
  /* ----- Parse command line ------------*/
  if (argc != 5) {
    std::cerr<<"Usage: "<<argv[0]<<" <alignment file.fasta> <tree file> <lambda_O> <lambda_E>\n";
    exit(1);
  }

  ifstream file(argv[1]);
  if (!file) {
    std::cerr<<"Error: can't open alignment file '"<<argv[1]<<"'"<<endl;
    std::cerr<<"Usage: "<<argv[0]<<" <alignment file.fasta> <tree file> <lambda_O> <lambda_E>\n";
    exit(1);
  }

  ifstream file2(argv[2]);
  if (!file2) {
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
  try {
    A.load_fasta(nucleotides,file);
    std::cerr<<A<<endl;
    
    SequenceTree T1(file2);
    std::cout<<"------begin tree---------\n";
    std::cout<<T1<<endl;
    std::cout<<"------end tree---------\n";
    
    {
      std::valarray<double> f(0.0,4);
      f[0] = 0.25;f[1]=0.25;f[2]=0.25;f[3]=0.25;

      HKY smodel(nucleotides,2.0,f);

      std::cerr<<"rate matrix = \n";
      for(int i=0;i<4;i++) {
	for(int j=0;j<4;j++) 
	  std::cerr<<smodel.rates()(i,j)<<" ";
	std::cerr<<endl;
      }
      Parameters Theta(smodel,T1);
      Theta.lambda_O = lambda_O;
      Theta.lambda_E = lambda_E;
      MCMC(A,Theta,250000,probability2);
    }
  }
  catch (std::exception& e) {
    std::cerr<<"Exception: "<<e.what()<<endl;
  }
  return 0;
}


/************************** ToDo -> Done! *****************************/


// 2. Check to make sure that the gap-resamping works - do we need to store TWO states?
//     or can we just modify choices depending on previous state?

// 4. make some output statistics so we can observe the probability distribution

// 6. Make it possible to specify Alignment(Sequences) and Tree on command line

// 9. Add branch lengths

// 14. How to read in tree structures in a text file?

// 15. Need to actually alter branch lengths w/ MCMC moves (use exp prior - estimate length parameter)

// 19. Does operator= work for parameters, or not? !!
//      operator= was working only the first time for trees - FIXED


// 21. Look at paper on RNA phylogenetics?

// 24. Write a script to output differences in alignment distributions

// 26. Put priors on branch lengths and parameters and stuff into probability() functions

// HIV1: -426.4
// HIV2: -421.5
// HIV3: -416.4 
