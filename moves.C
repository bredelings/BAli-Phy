#include "sample.H"
#include "likelihood.H"
#include "rng.H"
#include <algorithm>
#include "mcmc.H"

MCMC::result_t slide_branch_lengths_one(alignment& A, Parameters& P,int b) {
  bool up = true;
  if (myrandom(2))
    up = false;

  if (b < P.T.leaves())
    up = true;
  return slide_branch_length(A,P,b,up);
}



MCMC::result_t slide_branch_lengths(alignment& A, Parameters& P) {
  for(int i=0;i<P.T.leaves();i++) {
    int b = myrandom(P.T.branches());

    slide_branch_lengths_one(A,P,b);

  }
  return MCMC::no_result;
}

MCMC::result_t change_branch_length_move(alignment& A, Parameters& P,int b) {
  return change_branch_length(A,P,b);
}

MCMC::result_t change_branch_lengths(alignment& A, Parameters& P) {
  for(int i=0;i<P.T.leaves();i++) {
    int b = myrandom(P.T.branches());
    if (b<P.T.leaves())
      change_branch_length_move(A,P,b);
    else
      change_branch_length_and_T(A,P,b);
  }
  return MCMC::no_result;
}

MCMC::result_t sample_tri_one(alignment& A, Parameters& P,int b) {
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

MCMC::result_t sample_tri(alignment& A, Parameters& P) {
  const SequenceTree& T = P.T;

  for(int i=0;i<T.leaves();i++) {
    int b = myrandom(T.branches());
    sample_tri_one(A,P,b);
  }
  return MCMC::no_result;
}


MCMC::result_t sample_alignments_one(alignment& A, Parameters& P,int b) {
  A = sample_alignment(A,P,b);
  return MCMC::no_result;
}

MCMC::result_t sample_alignments2_one(alignment& A, Parameters& P,int b) {
  A = sample_alignment2(A,P,b);
  return MCMC::no_result;
}

MCMC::result_t sample_alignments(alignment& A, Parameters& P) {
  for(int i=0;i<P.T.leaves();i++) {
    int b = myrandom(P.T.branches());

    sample_alignments_one(A,P,b);
  }
  return MCMC::no_result;
}


MCMC::result_t sample_nodes_one(alignment& A, Parameters& P,int node) {
  A = sample_node(A,P,node);

  return MCMC::no_result;
}

MCMC::result_t sample_nodes2_one(alignment& A, Parameters& P,int node) {
  A = sample_node2(A,P,node);

  return MCMC::no_result;
}

MCMC::result_t sample_nodes(alignment& A, Parameters& P) {
  for(int i=0;i<P.T.num_nodes()-P.T.leaves();i++) {
    int node = myrandom(P.T.leaves(),P.T.num_nodes()-1);

    sample_nodes_one(A,P,node);
  }
  return MCMC::no_result;
}

MCMC::result_t sample_topologies(alignment& A,Parameters& P) {
  for(int i=0;i<P.T.branches()-P.T.leaves();i++) {
    int b = myrandom(P.T.leaves(),P.T.branches());
    sample_topology(A,P,b);
  }
  return MCMC::no_result;
}

MCMC::result_t change_parameters(alignment& A,Parameters& P) {
  Parameters P2 = P;

  P2.fiddle();

  double LS1 = likelihood3(A,P);
  double LP1 = prior3(A,P);

  double LS2 = likelihood3(A,P2);
  double LP2 = prior3(A,P2);

  double lL_1 = LS1 + LP1;
  double lL_2 = LS2 + LP2;

  for(int i=0;i<P.SModel().parameters().size();i++)
    std::cerr<<"    p"<<i<<" = "<<P.SModel().parameters()[i];
  std::cerr<<endl<<endl;

  for(int i=0;i<P2.SModel().parameters().size();i++)
    std::cerr<<"    p"<<i<<" = "<<P2.SModel().parameters()[i];
  std::cerr<<endl<<endl;

  std::cerr<<"L1 = "<<lL_1<<" = "<<LS1<<" + "<<LP1<<endl;
  std::cerr<<"L2 = "<<lL_2<<" = "<<LS2<<" + "<<LP2<<endl;

  if (myrandomf() < exp(lL_2 - lL_1)) {
    P = P2;
    std::cerr<<"accepted"<<endl;
    return MCMC::success;
  }
  else {
    std::cerr<<"rejected"<<endl;
    return MCMC::failure;
  }
  
}

