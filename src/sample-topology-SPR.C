#include <cmath>
#include <valarray>
#include <iostream>
#include "sample.H"
#include "rng.H"
#include "choose.H"
#include "likelihood.H"

#include "3way.H"
#include "alignment-sums.H"

///Sample between 2 topologies, ignoring gap priors on each case
bool sample_SPR_and_A(alignment& A,Parameters& P1,const Parameters& P2,int n1,int n2) {

  //----------- Generate the Different Matrices ---------//
  vector<Parameters> p(2,P1);
  p[1] = P2;

  vector< vector<int> > nodes(2);
  nodes[0] = A3::get_nodes_branch_random(P1.T,n1,n2);
  nodes[1] = A3::get_nodes_branch_random(P2.T,n1,n2);

  bool success = sample_tri_multi(A,p,nodes,true,true);
  P1 = p[0];

  return success;
}

///Sample between 2 topologies, ignoring gap priors on each case
bool topology_sample_SPR_sgaps(alignment& A,Parameters& P1,const Parameters& P2) {
  double Pr1 = P1.probability(A,P1);
  double Pr2 = P1.probability(A,P2);

  /*********** Choose A Topology ************/
  int choice = choose2(Pr1,Pr2);

  bool success = false;
  if (choice == 1) {
    P1 = P2;
    success = true;
  }
  return success;
}

bool topology_sample_SPR(alignment& A,Parameters& P1,const Parameters& P2,int n1, int n2) {
  assert(P1.IModel().full_tree == P2.IModel().full_tree);

  if (P1.IModel().full_tree)
    return sample_SPR_and_A(A,P1,P2,n1,n2);
  else
    return topology_sample_SPR_sgaps(A,P1,P2);
}

SequenceTree do_SPR(const SequenceTree& T1, int n1, int n2, int b1) {
  //----- Select the branch to move to ------//
  valarray<bool> subtree_nodes = T1.partition(n1,n2);
  vector<int> branches;
  vector<double> lengths;

  for(int i=0;i<T1.branches();i++) {
    if (not subtree_nodes[T1.branch(i).parent()] or 
	not subtree_nodes[T1.branch(i).child()]) {
      branches.push_back(i);
      lengths.push_back(T1.branch(i).length());
    }
  }

  int b2 = branches[ choose_nonlog(lengths) ];

  SequenceTree T2 = T1;

  //------ Generate the new topology ------//
  if (T2.branch(b2).parent() == n1 or T2.branch(b2).child() == n1)
    ;
  else
    T2.SPR(n1,n2,b2);

  //------ Find the two new branches ------//
  vector<int> connected;
  for(int i=0;i<T2.branches();i++) {
    if (i == b1) 
      continue;

    if (T2.branch(i).parent() == n1 or T2.branch(i).child() == n1)
      connected.push_back(i);
  }
  assert(connected.size() == 2);
  

  //------- Place the split randomly -------//
  double total = T2.branch(connected[0]).length() + T2.branch(connected[1]).length();
  T2.branch(connected[0]).length() = myrandomf() * total;
  T2.branch(connected[1]).length() = total - T2.branch(connected[0]).length();

  return T2;
}


MCMC::result_t sample_SPR(alignment& A,Parameters& P1,int b) {
  MCMC::result_t result(0.0,2);
  result[0] = 1.0;

  Parameters P2 = P1;

  SequenceTree& T1 = P1.T;
  SequenceTree& T2 = P2.T;

  //----- Get nodes for directed branch ------//
  int n1 = T1.branch(b).parent();
  int n2 = T1.branch(b).child();
  if (myrandomf()< 0.5)
    std::swap(n1,n2);
  if (T1[n1].leaf())
    std::swap(n1,n2);

  //----- Generate the Different Topologies ----//

  //  std::cerr<<"before = "<<T1<<endl;

  T2 = do_SPR(T1,n1,n2,b);
  P2.recalc();
  
  //  std::cerr<<"after = "<<T2<<endl;

  bool success = topology_sample_SPR(A,P1,P2,n1,n2);

  if (success)
    result[1] = 1;

  return result;
}
