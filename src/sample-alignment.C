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

  valarray<bool> s1 = constraint_satisfied(P.alignment_constraint,A);

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

  //--------------------------------------------------------------//
#ifndef NDEBUG_DP
  vector<int> nodes;
  nodes.push_back(node1);
  nodes.push_back(node2);

  vector<alignment> a;    a.push_back(A);   a.push_back(old);
  vector<Parameters> p(2,P);  p[0].LC.set_length(A.length()); p[0].LC.invalidate_branch_alignment(T,b);
  vector< efloat_t > OS(2, 0.0);
  vector< efloat_t > OP(2, 0.0);
  vector< vector<int> > paths;

  //------------------- Check offsets from path_Q -> P -----------------//
  for(int i=0;i<p.size();i++) {
    paths.push_back( get_path(a[i],node1,node2) );
    
    OS[i] = other_subst(a[i],p[i],nodes);
    OP[i] = other_prior(a[i],p[i],nodes);

    check_match_P(a[i], p[i], OS[i], OP[i], paths[i], Matrices);
  }

  //--------- Compute path probabilities and sampling probabilities ---------//
  vector< vector<efloat_t> > PR(p.size());

  efloat_t P_choice = 1;
  for(int i=0;i<p.size();i++) 
    PR[i] = sample_P(a[i], p[i], OS[i], OP[i] , P_choice, paths[i], Matrices);

  //--------- Check that each choice is sampled w/ the correct Probability ---------//
  check_sampling_probabilities(PR,a);

  //--------- Check construction of A  ---------//
  vector<int> path_new = get_path(A,node1,node2);
  path.push_back(3);
  assert(path_new == path);

#endif

  P.LC.set_length(A.length());
  P.LC.invalidate_branch_alignment(T,b);

  //--------------------------------------------------------------//
  assert(valid(A));
  valarray<bool> s2 = constraint_satisfied(P.alignment_constraint,A);
  report_constraints(s1,s2);
}


