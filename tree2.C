/* Version 2: based on operating on multiple alignments */

#include <cassert>
#include <iostream>
#include "mytypes.H"
#include "seq_tree.H"
#include "gaps.H"
#include "substitution.H"
#include "alignment.H"
#include "moves.H"
#include "myrandom.H"
#include "sample.H"

// ln(P(A,Data|T)) = ln(P(A|T)) + ln(P(Data|A,T))
double probability(const alignment& A,const SequenceTree& T) {
  double p = 0;
  p += prior(A,T);  
  p += substitution(A,T); // also deals w/ frequencies

  return p;
}

// ln(P(A,Data|T)) = ln(P(A)) + ln(P(Data|A,T))
double probability_no_tree(const alignment& A,const SequenceTree& T) {
  double p = 0;
  p += prior_no_tree(A);  
  p += substitution(A,T); // also deals w/ frequencies

  return p;
}

// ln(P(A,Data|T)) = ln(P(A)) + ln(P(Data|A,T))
double probability2(const alignment& A,const SequenceTree& T) {
  double p = 0;
  p += prior_internal(A,T);
  p += substitution(A,T); // also deals w/ frequencies
  return p;
}


  //FIXME: How to store our sample statistics?

void MCMC(alignment& A,const SequenceTree& T,const int max,double probability(const alignment&,const SequenceTree&)) {
  std::cout<<A<<endl;

  double logp = probability(A,T);
  int moves = 0;
  for(int iterations=0; iterations < max; iterations++) {
    std::cout<<"iterations: "<<iterations<<endl;

    alignment NewA = move(A,T);
    NewA = sample(A,T);
    double new_logp = probability(NewA,T);

    if (iterations %50 == 0) {
      std::cout<<A<<endl;
      std::cout<<NewA<<endl;
    }
    double ratio = exp(new_logp-logp);

    std::cout<<"templogp = "<<logp<<"  ratio = "<<new_logp-logp<<endl;
    
    if (ratio >= 1.0 || myrandomf() < ratio) {
      moves++;
      std::cout<<A<<endl;
      std::cout<<NewA<<endl;
      A = NewA;
      logp = new_logp;
      std::cout<<"iteration = "<<iterations<<"  move = "<<moves<<"  LogP = "<<logp<<"  "<<new_logp-logp<<endl;
    }
  }
}
  
void MCMC2(alignment& A,const SequenceTree& T,const int max,double probability(const alignment&,const SequenceTree&)) {
  A.create_internal(T);
  std::cout<<A<<endl;

  for(int iterations=0; iterations < max; iterations++) {
    std::cout<<"iterations: "<<iterations<<"    logp = "<<probability(A,T)<<endl;

    alignment NewA = sample(A,T);

    if (iterations %50 == 0) {
      std::cout<<"previous = "<<probability_no_tree(A,T)<<"  "<<probability(A,T)<<"  "<<probability2(A,T)<<endl;
      std::cout<<A<<endl;
      std::cout<<"new = "<<probability_no_tree(NewA,T)<<"  "<<probability(NewA,T)<<"  "<<probability2(NewA,T)<<endl;
      std::cout<<NewA<<endl;
    }

    A = NewA;
  }
}
  
// use some of Marc's data now!

// PROBLEM!!! - something is inserting gaps for no reason!

/**** How to load data ****/
// sequences
// (?) initial alignment
// alphabet
// tree
int main() {

  myrand_init();
  alphabet nucleotides("AGTC","DNA");

  //  alphabet amino_acids("ARNDCQEGHILKMFPTWYV","PAM");

  /*************  Set up the tree **************/
  SequenceTree T1 = SequenceTree("1");
  SequenceTree T2 = SequenceTree("2");
  SequenceTree T3 = SequenceTree("3");
  SequenceTree T4 = SequenceTree("4");

  SequenceTree T = (T1+T2)+(T3+T4);

  /**********  Set up the alignment ***********/
  alignment A;

  ifstream file("test.fasta");
  A.load_fasta(nucleotides,file);
  std::cout<<A<<endl;

  // FIXME: I don't have any branch lengths right now!
  //  tree t3 = tree( tree(human,1,chimp,2),1,tree(gorilla,2,orangutan,2),2);


  /*********** Start the MCMC sampling ***********/
  
  MCMC2(A,T,2000,probability2);

  return 1;
}
