#include "sample.H"
#include "likelihood.H"
#include "rng.H"
#include <algorithm>

void change_branch_lengths(alignment& A, Parameters& Theta) {
  for(int i=0;i<Theta.T.leaves();i++) {
    int b = myrandom(Theta.T.branches());
    if (b<Theta.T.leaves())
      change_branch_length(A,Theta,b);
    else
      change_branch_length_and_T(A,Theta,b);
  }
}

void sample_alignments(alignment& A,const Parameters& Theta) {
  for(int i=0;i<Theta.T.leaves();i++) {
    int b = myrandom(Theta.T.branches());

    A = sample_alignment(A,Theta,b);
  }
}

void sample_nodes(alignment& A,const Parameters& Theta) {
  for(int i=0;i<Theta.T.num_nodes()-Theta.T.leaves();i++) {
    int node = myrandom(Theta.T.leaves(),Theta.T.num_nodes()-1);

    A = sample_node(A,Theta,node);
  }
}

void sample_topologies(alignment& A,Parameters& Theta) {
  for(int i=0;i<Theta.T.branches()-Theta.T.leaves();i++) {
    int b = myrandom(Theta.T.leaves(),Theta.T.branches());
    sample_topology(A,Theta,b);
  }
}

void change_parameters(const alignment& A,Parameters& Theta) {
  Parameters Theta2 = Theta;

  //  Theta2.SModel->fiddle();

  double lL_1 = substitution(A,Theta)  + prior(Theta);
  double lL_2 = substitution(A,Theta2) + prior(Theta);
  if (myrandomf() < exp(lL_2 - lL_1)) {
    Theta = Theta2;
  }
}

void sample(alignment& A,Parameters& Theta) {

  double r = myrandomf();
  if (r < 0.2) 
    sample_alignments(A,Theta);
  else if (r < 0.4) 
    sample_nodes(A,Theta);
  else if (r<0.6) 
    sample_topologies(A,Theta);
  else if (r<2.0)
    change_branch_lengths(A,Theta);
  else
    change_parameters(A,Theta);
}

