#include <valarray>
#include <iostream>
#include <cmath>
#include "sample.H"
#include "2way.H"
#include "alignment-sums.H"
#include "alignment-constraint.H"
#include "alignment-util.H"

// SYMMETRY: Because we are only sampling from alignments with the same fixed length
// for both sequences, this process is symmetric

using std::abs;
using namespace A2;

vector< Matrix > distributions_star(const alignment& A,const Parameters& P,
						  const vector<int>& seq,int b,bool up) {

  //--------------- Find our branch, and orientation ----------------//
  const SequenceTree& T = P.T;
  int root = T.branch(b).target();      //this is an arbitrary choice

  int node1 = T.branch(b).source();
  int node2 = T.branch(b).target();
  if (not up) std::swap(node1,node2);

  valarray<bool> group = T.partition(node1,node2);

  return ::distributions_star(A,P,seq,root,group);
}

vector< Matrix > distributions_tree(const alignment& A,const Parameters& P,
						  const vector<int>& seq,int b,bool up) {
  //--------------- Find our branch, and orientation ----------------//
  const SequenceTree& T = P.T;
  int root = T.branch(b).target();      //this is an arbitrary choice

  int node1 = T.branch(b).source();
  int node2 = T.branch(b).target();
  if (not up) std::swap(node1,node2);

  valarray<bool> group = T.partition(node1,node2);

  return ::distributions_tree(A,P,seq,root,group);
}

typedef vector< Matrix > (*distributions_t_local)(const alignment&, const Parameters&,
							      const vector<int>&,int,bool);

void sample_alignment(alignment& A,Parameters& P,int b) {
  assert(P.IModel().full_tree);

  const Tree& T = P.T;
  alignment old = A;

  const Matrix frequency = substitution::frequency_matrix(P.SModel());

  int node1 = T.branch(b).target();
  int node2 = T.branch(b).source();

  valarray<bool> group1 = T.partition(node2,node1);

  // Find sub-alignments and sequences
  vector<int> seq1;
  vector<int> seq2;
  for(int column=0;column<old.length();column++) {
    if (old(column,node1) != alphabet::gap)
      seq1.push_back(column);
    if (old(column,node2) != alphabet::gap)
      seq2.push_back(column);
  }

  if (not seq1.size() or not seq2.size()) return;

  /******** Precompute distributions at node2 from the 2 subtrees **********/
  distributions_t_local distributions = distributions_tree;
  if (not P.SModel().full_tree)
    distributions = distributions_star;

  vector< Matrix > dists1 = distributions(old,P,seq1,b,true);
  vector< Matrix > dists2 = distributions(old,P,seq2,b,false);

  vector<int> state_emit(4,0);
  state_emit[0] |= (1<<1)|(1<<0);
  state_emit[1] |= (1<<1);
  state_emit[2] |= (1<<0);
  state_emit[3] |= 0;

  DPmatrixSimple Matrices(state_emit, P.branch_HMMs[b].start_pi(),P.branch_HMMs[b], P.Temp,
			  P.SModel().distribution(), dists1, dists2, frequency);

  /*------------------ Compute the DP matrix ---------------------*/

  vector<int> path_old = get_path(old,node1,node2);
  vector<vector<int> > pins = get_pins(P.alignment_constraint,old,group1,not group1,seq1,seq2);
  vector<int> path = Matrices.forward(pins);

  path.erase(path.begin()+path.size()-1);

  A = construct(old,path,group1,seq1,seq2);

  //  std::cerr<<"bandwidth = "<<bandwidth(Matrices,path)<<std::endl;

  //  std::cerr<<"bandwidth2 = "<<bandwidth2(Matrices,path)<<std::endl;
  //--------------------------------------------------------------//
#ifndef NDEBUG_DP
  //  vector<int> path_old = get_path(old,node1,node2);
  vector<int> path_new = get_path(A,node1,node2);

  path.push_back(3);
  assert(path_new == path);
  vector<int> nodes;
  nodes.push_back(node1);
  nodes.push_back(node2);
  check_match_P(old,P,other_subst(old,P,nodes),other_prior(old,P,nodes),path_old,Matrices);
  double ls1 = P.likelihood(old,P);

  P.LC.set_length(A.length());
  P.LC.invalidate_branch_alignment(T,b);

  check_match_P(A  ,P,other_subst(A  ,P,nodes),other_prior(A,  P,nodes),path_new,Matrices);
  double ls2 = P.likelihood(A  ,P);

  double lp1 = P.prior(old,P);
  double lp2 = P.prior(A  ,P);

  double diff = Matrices.check(path_old,path_new,lp1,ls1,lp2,ls2);
  if (abs(diff) > 1.0e-9) {
    std::cerr<<old<<endl;
    std::cerr<<A<<endl;

    throw myexception()<<__PRETTY_FUNCTION__<<": sampling probabilities were incorrect";
  }

  std::cerr<<"P(Y|A,tau,T,Theta) = "<<ls2<<"    P(Y|tau,T,Theta) = "<<Matrices.Pr_sum_all_paths()<<endl;

#else
  P.LC.set_length(A.length());
  P.LC.invalidate_branch_alignment(T,b);
#endif

  /*--------------------------------------------------------------*/
  assert(valid(A));
}


