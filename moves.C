#include "sample.H"
#include "rng.H"
#include <algorithm>
#include "mcmc.H"

MCMC::result_t slide_branch_lengths_one(alignment& A, Parameters& P,int b) {
  if (not P.SModel().full_tree)
    return MCMC::no_result;

  bool up = true;
  if (myrandom(2))
    up = false;

  if (b < P.T.leaves())
    up = true;
  return slide_branch_length(A,P,b,up);
}


MCMC::result_t change_branch_length_move(alignment& A, Parameters& P,int b) {
  if (not P.SModel().full_tree and b>=P.T.leaves())
    return MCMC::no_result;

  return change_branch_length(A,P,b);
}

MCMC::result_t sample_tri_one(alignment& A, Parameters& P,int b) {
  assert(P.IModel().full_tree); 

  const SequenceTree& T = P.T;

  int node1 = T.branch(b).parent();
  int node2 = T.branch(b).child();

  if (myrandomf() < 0.5)
    std::swap(node1,node2);

  if (node1 < T.leaves())
    std::swap(node1,node2);
    
  A = tri_sample_alignment(A,P,node1,node2);

  return MCMC::no_result;
}

MCMC::result_t sample_alignments_one(alignment& A, Parameters& P,int b) {
  assert(P.IModel().full_tree); 

  A = sample_alignment(A,P,b);
  return MCMC::no_result;
}

MCMC::result_t sample_alignments2_one(alignment& A, Parameters& P,int b) {
  assert(P.IModel().full_tree); 

  A = sample_alignment2(A,P,b);
  return MCMC::no_result;
}

MCMC::result_t sample_nodes2_one(alignment& A, Parameters& P,int node) {
  assert(P.IModel().full_tree); 

  A = sample_node2(A,P,node);

  return MCMC::no_result;
}

MCMC::result_t change_parameters(alignment& A,Parameters& P) {
  Parameters P2 = P;

  P2.fiddle();

#ifndef NDEBUG  
  for(int i=0;i<P.SModel().parameters().size();i++)
    std::cerr<<"    p"<<i<<" = "<<P.SModel().parameters()[i];
  std::cerr<<endl<<endl;

  for(int i=0;i<P2.SModel().parameters().size();i++)
    std::cerr<<"    p"<<i<<" = "<<P2.SModel().parameters()[i];
  std::cerr<<endl<<endl;
#endif

  if (P.accept_MH(A,P,A,P2)) {
    P = P2;
#ifndef NDEBUG
    std::cerr<<"success\n";
#endif
    return MCMC::success;
  }
  else {
#ifndef NDEBUG
    std::cerr<<"failure\n";
#endif
    return MCMC::failure;
  }
  
}

