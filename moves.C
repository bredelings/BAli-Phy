#include "sample.H"
#include "rng.H"
#include <algorithm>
#include "mcmc.H"

MCMC::result_t slide_branch_lengths_one(alignment& A, Parameters& P,int b) {

  if (not P.SModel().full_tree) 
    return MCMC::result_t(0.0,6); // no_result

  bool up = true;
  if (myrandom(2))
    up = false;

  if (b < P.T.leaves())
    up = true;
  return slide_branch_length(A,P,b,up);
}


MCMC::result_t change_branch_length_move(alignment& A, Parameters& P,int b) {
  if (not P.SModel().full_tree and b>=P.T.leaves())
    return MCMC::result_t(0.0,6); // no_result

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

  return MCMC::result_t(); // no_result
}

MCMC::result_t sample_tri_branch_one(alignment& A, Parameters& P,int b) {
  if (not P.SModel().full_tree and b>=P.T.leaves())
    return MCMC::result_t(0.0,4); // no_result

  MCMC::result_t result(0.0,4);
  result[0] = 1.0;
  result[2] = 1.0;

  assert(P.IModel().full_tree); 

  const SequenceTree& T = P.T;

  int node1 = T.branch(b).parent();
  int node2 = T.branch(b).child();

  if (myrandomf() < 0.5)
    std::swap(node1,node2);

  if (node1 < T.leaves())
    std::swap(node1,node2);
    
  const double sigma = 0.3/2;
  double length1 = T.branch(b).length();
  double length2 = length1 + gaussian(0,sigma);
  if (length2 < 0) length2 = -length2;

  if (tri_sample_alignment_branch(A,P,node1,node2,b,length2)) {
    result[1] = 1;
    result[3] = std::abs(length2 - length1);
  }

  return result;
}


MCMC::result_t sample_alignments_one(alignment& A, Parameters& P,int b) {
  assert(P.IModel().full_tree); 

  A = sample_alignment(A,P,b);
  return MCMC::result_t(); // no_result
}

MCMC::result_t sample_node_move(alignment& A, Parameters& P,int node) {
  assert(P.IModel().full_tree); 

  A = sample_node(A,P,node);

  return MCMC::result_t(); // no_result
}

MCMC::result_t sample_two_nodes_move(alignment& A, Parameters& P,int b) {
  assert(P.IModel().full_tree); 

  A = sample_two_nodes(A,P,b);

  return MCMC::result_t(); // no_result
}

MCMC::result_t change_parameters(alignment& A,Parameters& P) {
  MCMC::result_t result(0.0,2);
  result[0] = 1.0;

  Parameters P2 = P;

  P2.fiddle();

#ifndef NDEBUG  
  for(int i=0;i<P.SModel().parameters().size();i++)
    std::cerr<<"    p"<<i<<" = "<<P.SModel().parameters()[i];
  std::cerr<<P.probability(A,P);
  std::cerr<<endl<<endl;

  for(int i=0;i<P2.SModel().parameters().size();i++)
    std::cerr<<"    p"<<i<<" = "<<P2.SModel().parameters()[i];
  std::cerr<<P.probability(A,P2);
  std::cerr<<endl<<endl;
#endif

  if (P.accept_MH(A,P,A,P2)) {
    P = P2;
    result[1] = 1;
#ifndef NDEBUG
    std::cerr<<"success\n";
#endif
  }
  else {
#ifndef NDEBUG
    std::cerr<<"failure\n";
#endif
  }
  return result;
}

MCMC::result_t change_gap_parameters(alignment& A,Parameters& P) {
  MCMC::result_t result(0.0,2);
  result[0] = 1.0;

  Parameters P2 = P;
  P2.IModel().fiddle(P.i_fixed);

  if (P.accept_MH(A,P,A,P2)) {
    P = P2;
    result[1] = 1;
  }

  return result;
}
