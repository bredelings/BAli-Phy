#include "sample.H"
#include "likelihood.H"
#include "rng.H"
#include <algorithm>

void slide_branch_lengths(alignment& A, Parameters& Theta) {
  for(int i=0;i<Theta.T.leaves();i++) {
    int b = myrandom(Theta.T.branches());

    bool up = true;
    if (myrandom(2))
      up = false;

    slide_branch_length(A,Theta,b,up);
  }
}

void change_branch_lengths(alignment& A, Parameters& Theta) {
  for(int i=0;i<Theta.T.leaves();i++) {
    int b = myrandom(Theta.T.branches());
    if (b<Theta.T.leaves())
      change_branch_length(A,Theta,b);
    else
      change_branch_length_and_T(A,Theta,b);
  }
}


void sample_tri(alignment& A, Parameters& Theta) {
  const SequenceTree& T = Theta.T;

  for(int i=0;i<T.leaves();i++) {
    int b = myrandom(T.branches());

    int node1 = T.branch(b).parent();
    int node2 = T.branch(b).child();

    if (myrandomf() < 0.5)
      std::swap(node1,node2);

    if (node1 < T.leaves())
      std::swap(node1,node2);
    
    A = tri_sample_alignment(A,Theta,node1,node2);
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

  Theta2.fiddle();

  double lL_1 = substitution(A,Theta)  + prior(Theta);
  double lL_2 = substitution(A,Theta2) + prior(Theta2);
  std::cerr<<"L1 = "<<lL_1<<" = "<<substitution(A,Theta)<<" + "<<prior(Theta)<<endl;
  std::cerr<<"L2 = "<<lL_2<<" = "<<substitution(A,Theta2)<<" + "<<prior(Theta2)<<endl;

  if (myrandomf() < exp(lL_2 - lL_1)) {
    Theta = Theta2;
    std::cerr<<"accepted"<<endl;
  }
  else
    std::cerr<<"rejected"<<endl;
}

