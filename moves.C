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

void sample_alignments(alignment& A, Parameters& Theta) {
  for(int i=0;i<Theta.T.leaves();i++) {
    int b = myrandom(Theta.T.branches());

    A = sample_alignment(A,Theta,b);
  }
}

void sample_nodes(alignment& A, Parameters& Theta) {
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

void change_parameters(alignment& A,Parameters& Theta) {
  Parameters Theta2 = Theta;

  Theta2.SModel->fiddle();

  double lL_1 = substitution(A,Theta)  + prior(Theta);
  double lL_2 = substitution(A,Theta2) + prior(Theta2);
  if (myrandomf() < exp(lL_2 - lL_1)) {
    Theta = Theta2;
  }
}

