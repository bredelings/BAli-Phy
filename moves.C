#include "sample.H"
#include "likelihood.H"
#include "rng.H"
#include <algorithm>

void slide_branch_lengths(alignment& A, Parameters& P) {
  for(int i=0;i<P.T.leaves();i++) {
    int b = myrandom(P.T.branches());

    bool up = true;
    if (myrandom(2))
      up = false;

    slide_branch_length(A,P,b,up);
  }
}

void change_branch_lengths(alignment& A, Parameters& P) {
  for(int i=0;i<P.T.leaves();i++) {
    int b = myrandom(P.T.branches());
    //    if (b<P.T.leaves())
      change_branch_length(A,P,b);
      //    else
      //      change_branch_length_and_T(A,P,b);
  }
}


void sample_tri(alignment& A, Parameters& P) {
  const SequenceTree& T = P.T;

  for(int i=0;i<T.leaves();i++) {
    int b = myrandom(T.branches());

    int node1 = T.branch(b).parent();
    int node2 = T.branch(b).child();

    if (myrandomf() < 0.5)
      std::swap(node1,node2);

    if (node1 < T.leaves())
      std::swap(node1,node2);
    
    A = tri_sample_alignment(A,P,node1,node2);
  }
}


void sample_alignments(alignment& A, Parameters& P) {
  for(int i=0;i<P.T.leaves();i++) {
    int b = myrandom(P.T.branches());

    A = sample_alignment(A,P,b);
  }
}

void sample_nodes(alignment& A, Parameters& P) {
  for(int i=0;i<P.T.num_nodes()-P.T.leaves();i++) {
    int node = myrandom(P.T.leaves(),P.T.num_nodes()-1);

    A = sample_node(A,P,node);
  }
}

void sample_topologies(alignment& A,Parameters& P) {
  for(int i=0;i<P.T.branches()-P.T.leaves();i++) {
    int b = myrandom(P.T.leaves(),P.T.branches());
    sample_topology(A,P,b);
  }
}

void change_parameters(alignment& A,Parameters& P) {
  Parameters P2 = P;

  P2.fiddle();

  double lL_1 = probability3(A,P);
  double lL_2 = probability3(A,P2);

  for(int i=0;i<P.SModel().parameters().size();i++)
    std::cerr<<"    p"<<i<<" = "<<P.SModel().parameters()[i];
  std::cerr<<endl<<endl;

  for(int i=0;i<P2.SModel().parameters().size();i++)
    std::cerr<<"    p"<<i<<" = "<<P2.SModel().parameters()[i];
  std::cerr<<endl<<endl;


  std::cerr<<"L1 = "<<lL_1<<" = "<<substitution::Pr(A,P)<<" + "<<prior(P)<<endl;
  std::cerr<<"L2 = "<<lL_2<<" = "<<substitution::Pr(A,P2)<<" + "<<prior(P2)<<endl;

  if (myrandomf() < exp(lL_2 - lL_1)) {
    P = P2;
    std::cerr<<"accepted"<<endl;
  }
  else
    std::cerr<<"rejected"<<endl;
}

