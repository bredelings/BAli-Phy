/* Version 2: based on operating on multiple alignments */

#include <cassert>
#include <iostream>
#include <valarray>
#include "mytypes.H"
#include "tree.H"
#include "likelihood.H"
#include "substitution.H"
#include "alignment.H"
#include "myrandom.H"
#include "sample.H"
#include "parameters.H"

void print_stats(const alignment& A,int n1,int n2) {
  
}

void print_stats(const alignment& A) {
  print_stats(A,0,1);
  print_stats(A,0,2);
  print_stats(A,0,3);
  print_stats(A,1,2);
  print_stats(A,1,3);
  print_stats(A,2,3);
}

void MCMC2(alignment& A,Parameters& Theta,
	   const int max,double probability(const alignment&,const Parameters&)) {
  const SequenceTree& T = Theta.T;
  A.create_internal(T);
  std::cout<<A<<endl;

  int correlation_time = int(2.0*T.leaves()*log(T.leaves()));

  double p=probability(A,Theta);
  double new_p=0;
  for(int iterations=0; iterations < max; iterations++) {
    std::cout<<"iterations: "<<iterations<<"    logp = "<<p<<endl;

    if (iterations > 1000 && iterations%correlation_time == 0) 
      print_stats(A);


    alignment NewA = sample(A,Theta);
    new_p = probability(NewA,Theta);

    if (iterations %50 == 0 or fabs(p - new_p)>8) {
      std::cout<<"previous = "<<
	probability_no_tree(A,Theta)<<"  "<<
	//	probability_simple_tree(A,Theta)<<"  "<<
	probability(A,Theta)<<"  ["<<probability2(A,Theta)<<": "<<prior_internal(A,Theta)<<" + "<<substitution(A,Theta)<<"]"<<endl;

      std::cout<<A<<endl;
      std::cout<<"new = "<<
	probability_no_tree(NewA,Theta)<<"  "<<
	//	probability_simple_tree(NewA,Theta)<<"  "<<
	probability(NewA,Theta)<<"  ["<<probability2(NewA,Theta)<<": "<<prior_internal(NewA,Theta)<<" + "<<substitution(NewA,Theta)<<"]"<<endl;
      std::cout<<NewA<<endl;
      NewA.print_fasta(std::cerr);
    }

    A = NewA;
    p = new_p;
  }
}
  

/**** TODO: other data (CAR), output marginal distributions (m01,m12..), change branch lengths  ****/

// 2. Check to make sure that the gap-resamping works

// 3. write a routine to load the Phylip files -> check out CAR.phy

// 4. make some output statistics so we can observe the probability distribution

// 5. Read Marc's references on actually altering the tree

// 6. Make it possible to specify Alignment(Sequences) and Tree on command line

// 7. Can we somehow ESTIMATE lambda_O and lambda_E?

// 8. Use ublas::matrix<double>(a.size()) instead of valarray<double> in substitution.C

// 10. We still have problems jumping logs I think??

// 13. Put letters in the rate matrix file

// 14. How to read in tree structures in a text file?

int main(int argc,char* argv[]) {

  myrand_init();
  alphabet nucleotides("AGUC");
  
  //  alphabet amino_acids("ARNDCQEGHILKMFPTWYV","PAM");

  /*************  Set up the tree **************/
  SequenceTree Human("H_sapiens");
  SequenceTree Frog ("X_boreal");
  SequenceTree Sulfur1("Sulfaci1");
  SequenceTree Sulfur2("Sulfaci2");
  SequenceTree Salt1("Halomari");
  SequenceTree Salt2("Halosali");
  
  SequenceTree Cyano("Anacy_nidu");
  
  SequenceTree EColi1("Esch_coli1");
  SequenceTree EColi2("Esch_coli2");
  SequenceTree EColi3("Esch_coli3");
  SequenceTree EColi4("Esch_coli4");
  SequenceTree EColi5("Esch_coli5");

  
  SequenceTree Pro = (EColi1+EColi2)+(EColi3+(EColi4+EColi5));
  SequenceTree Sulfur = (Sulfur1 + Sulfur2);
  SequenceTree Salt = (Salt1 + Salt2);
  SequenceTree Metazoa = Human+Frog;
  SequenceTree Archae = Sulfur + Salt;
  SequenceTree Eubacteria = Cyano + Pro;
  
  SequenceTree T1 = ((Metazoa+Archae)+Eubacteria);
  SequenceTree T2 = (Metazoa + Sulfur)+(Eubacteria+Salt);
  SequenceTree T3 = (Metazoa + Eubacteria) + (Sulfur + Salt);
  
  /**********  Set up the alignment ***********/
  alignment A;

  
  /* ----- Parse command line ------------*/
  if (argc != 4) {
    std::cerr<<"Usage: "<<argv[0]<<" <alignment file.fasta> <lambda_O> <lambda_E>\n";
    exit(0);
  }
  ifstream file(argv[1]);
  if (file) {
    A.load_fasta(nucleotides,file);
    std::cout<<A<<endl;
  }
  else {
    std::cerr<<"Error: can't open alignment file '"<<argv[1]<<"'"<<endl;
    exit(1);
  }
  double lambda_O=0;
  double lambda_E=0;
  {
    std::stringstream A1(argv[2]);
    A1>>lambda_O;
    std::stringstream A2(argv[3]);
    A2>>lambda_E;
    std::cout<<lambda_O<<"  "<<lambda_E<<endl;
  }



  // FIXME: I don't have any branch lengths right now!
  //  tree t3 = tree( tree(human,1,chimp,2),1,tree(gorilla,2,orangutan,2),2);

  //  We could have classes derive from the parameters struct!  
  //    maybe classes of matrix

  /*********** Start the sampling ***********/
  
  {
    Parameters Theta(nucleotides,T1);
    Theta.load_rates("DNA");
    Theta.lambda_O = lambda_O;
    Theta.lambda_E = lambda_E;
    MCMC2(A,Theta,10000,probability2);
  }

  {
    Parameters Theta(nucleotides,T3);
    Theta.load_rates("DNA");
    Theta.lambda_O = lambda_O;
    Theta.lambda_E = lambda_E;
    
    MCMC2(A,Theta,10000,probability2);
  }

  {
    Parameters Theta(nucleotides,T3);
    Theta.load_rates("DNA");
    Theta.lambda_O = lambda_O;
    Theta.lambda_E = lambda_E;
    
    MCMC2(A,Theta,10000,probability2);
  }

  return 0;
}
