/* Version 2: based on operating on multiple alignments */

#include <cassert>
#include <iostream>
#include <valarray>
#include "myexception.H"
#include "mytypes.H"
#include "tree.H"
#include "likelihood.H"
#include "substitution.H"
#include "alignment.H"
#include "myrandom.H"
#include "sample.H"
#include "parameters.H"

static int total_samples = 0;

void print_stats(const alignment& A,const string& s1,const string& s2) {
  int n1 = A.index(s1);
  assert(n1 != -1);

  int n2 = A.index(s2);
  assert(n2 != -1);

  int pos1=0,new_pos1=0;
  int pos2=0,new_pos2=0;
  for(int column=0;column<A.length();column++) {
    if (A(column,n1) == alphabet::gap && A(column,n2) == alphabet::gap) 
      continue;

    if (A(column,n1) != alphabet::gap) 
      new_pos1++;
    if (A(column,n2) != alphabet::gap) 
      new_pos2++;

    std::cout<<s1<<" "<<s2<<" :     "<<pos1<<" "<<pos2<<"   "<<new_pos1<<"   "<<new_pos2<<endl;
	     
    pos1 = new_pos1;
    pos2 = new_pos2;
  }
}

/* 
void print_stats(const alignment& A,const Parameters& Theta) {
  total_samples++;

  print_stats(A,"H_sapiens","Sulfaci1");
  print_stats(A,"H_sapiens","Halomari");
  print_stats(A,"H_sapiens","Esch_coli3");
  print_stats(A,"Sulfaci1","Halomari");
  print_stats(A,"Sulfaci1","Esch_coli3");
  print_stats(A,"Halomari","Esch_coli3");
  
  std::cout<<endl;
  for(int i=0;i<Theta.T.branches();i++) 
    std::cout<<"branch "<<i<<" "<<Theta.T.branch(i).length<<endl;
  std::cout<<endl;
}
*/

void print_stats(const alignment& A,const Parameters& Theta) {
  total_samples++;

  print_stats(A,"CAR4081","consGenv");
  print_stats(A,"CAR4081","consAenv");
  print_stats(A,"CAR4081","consBenv");
  print_stats(A,"consGenv","consAenv");
  print_stats(A,"consGenv","consBenv");
  print_stats(A,"consAenv","consBenv");

}

void MCMC2(alignment& A,Parameters& Theta,
	   const int max,double probability(const alignment&,const Parameters&)) {
  const SequenceTree& T = Theta.T;
  A.create_internal(T);
  std::cerr<<A<<endl;

  SequenceTree Sum = T;

  const int correlation_time = int(8.0*T.leaves()*log(T.leaves()));
  const int start_after = int( 600.0*T.leaves()*log(T.leaves()) );
  int total_samples = 0;

  double p=probability(A,Theta);
  double new_p=0;
  for(int iterations=0; iterations < max; iterations++) {
    std::cerr<<"iterations: "<<iterations<<"    logp = "<<p<<endl;

    /******************** Record Statistics *******************/
    if (iterations > start_after) {
      if (iterations%correlation_time == 0) 
      	print_stats(A,Theta);
      for(int i=0;i<T.num_nodes();i++)
	Sum.branch(i).length += T.branch(i).length;
      total_samples++;
      if (iterations % 100 == 0) {
	SequenceTree Average = Sum;
	for(int i=0;i<T.num_nodes();i++)
	  Average.branch(i).length /= total_samples;
	std::cout<<"------begin tree---------\n";
	std::cout<<Average<<endl;
	std::cout<<"------end tree---------\n";

      }
    }


    /******************* Propose new position *********************/
    alignment NewA = sample(A,Theta);
    new_p = probability(NewA,Theta);

    /***************** Print Diagnostic Output ********************/
    if (iterations %250 == 0 or fabs(p - new_p)>5) {
      std::cerr<<"previous = "<<
	probability_no_tree(A,Theta)<<"  "<<
	probability_simple_tree(A,Theta)<<"  "<<
	probability(A,Theta)<<"  ["<<probability2(A,Theta)<<": "<<prior_internal(A,Theta)<<" + "<<substitution(A,Theta)<<"]"<<endl;

      std::cerr<<A<<endl;
      std::cerr<<"new = "<<
	probability_no_tree(NewA,Theta)<<"  "<<
	probability_simple_tree(NewA,Theta)<<"  "<<
	probability(NewA,Theta)<<"  ["<<probability2(NewA,Theta)<<": "<<prior_internal(NewA,Theta)<<" + "<<substitution(NewA,Theta)<<"]"<<endl;
      std::cerr<<NewA<<endl;
      NewA.print_fasta(std::cerr);
    }


    /*****************Actually Move to new position ***************/
    A = NewA;
    p = new_p;
  }
}


/**** TODO:  A. Check distribution of logs - now use RNA data
             B. Normalize/Parameterize rate matrix?
             C. Write code to change tree topology
	        (Need an interface so that node NAME doesn't change!)
             D. Benchmark
 ****/


// 3. write a routine to load the Phylip files -> check out CAR.phy

// 5. Read Marc's references on actually altering the tree

// 7. Can we somehow ESTIMATE lambda_O and lambda_E?

// 8. Use ublas::matrix<double>(a.size()) instead of valarray<double> in substitution.C

// 10. We still have problems jumping logs I think??
//     Why is the distribution of log scores so WIDE?
//     At least, with the HIV stuff, there seems to be a clear maximum!
//     Check various moves with this!  First cut out the branch-length code.

// 13. Put letters in the rate matrix file

// 16. *Better* output statistics?  Specify species to look at on command line?

// 17. Output an ML phylogeny?

// 18. Normalize rate matrix so that Sum(m) lambda(m,m)*pi(m) = -1

// 20. How can we show conservation in the graph?

// 22. ALL internal-node-resampling is 1D!  We can resample adjacent
//       internal nodes simultaneously w/ a 1D algorithm!
//     IS there a good way to just integrate over all internal node state?
//       The state space is probably at least 2^(#nodes) though.

// 23. Finish writing the new branch-length MH sampler

// 24. Measure the correlation time - see how sorting by center of mass of alignment
//     and moving from k->N-k decreases it...
//
//     Perhaps we could output more samples than we are now...


// 25. Benchmark, and see how the speed depends on sequence length

// 27. Make output less verbose?

int main(int argc,char* argv[]) {

  myrand_init();
  alphabet nucleotides("AGUC");
  
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



  // FIXME: I don't have any branch lengths right now!
  //  tree t3 = tree( tree(human,1,chimp,2),1,tree(gorilla,2,orangutan,2),2);

  //  We could have classes derive from the parameters struct!  
  //    maybe classes of matrix

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
      Parameters Theta(nucleotides,T1);
      Theta.load_rates("DNA");
      Theta.lambda_O = lambda_O;
      Theta.lambda_E = lambda_E;
      MCMC2(A,Theta,250000,probability2);
    }
    std::cerr<<"total sample = "<<total_samples<<endl;
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

