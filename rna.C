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

  double p=probability(A,T);
  double new_p=0;
  for(int iterations=0; iterations < max; iterations++) {
    std::cout<<"iterations: "<<iterations<<"    logp = "<<p<<endl;

    alignment NewA = sample(A,T);
    new_p = probability(NewA,T);

    if (iterations %50 == 0 or fabs(p - new_p)>10) {
      std::cout<<"previous = "<<probability_no_tree(A,T)<<"  "<<probability(A,T)<<"  ["<<probability2(A,T)<<": "<<prior_internal(A,T)<<" + "<<substitution(A,T)<<"]"<<endl;
      std::cout<<A<<endl;
      std::cout<<"new = "<<probability_no_tree(NewA,T)<<"  "<<probability(NewA,T)<<"  ["<<probability2(NewA,T)<<": "<<prior_internal(NewA,T)<<" + "<<substitution(NewA,T)<<"]"<<endl;
      std::cout<<NewA<<endl;
      NewA.print_fasta(std::cerr);
    }

    A = NewA;
    p = new_p;
  }
}
  

// 1. ADD BRANCH LENGTHS!

// 2. Check to make sure that the gap-resamping works

// 3. write a routine to load the Phylip files -> check out CAR.phy

// 4. make some output statistics so we can observe the probability distribution

// 5. Read Marc's references on actually altering the tree

// 6. Make it possible to specify Alignment(Sequences) and Tree on command line

// 7. Can we somehow ESTIMATE lambda_O and lambda_E?

int main() {

  myrand_init(0);
  alphabet nucleotides("AGUC","DNA");
  
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

  ifstream file("5SRNA.fasta");
  A.load_fasta(nucleotides,file);
  A.create_internal(T1);
  std::cout<<A<<endl;

  // FIXME: I don't have any branch lengths right now!
  //  tree t3 = tree( tree(human,1,chimp,2),1,tree(gorilla,2,orangutan,2),2);

  /*********** Start the MCMC sampling ***********/
  
  MCMC2(A,T1,10000,probability2);

  MCMC2(A,T2,10000,probability2);

  MCMC2(A,T2,10000,probability2);

  return 1;
}
