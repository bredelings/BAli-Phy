#include <cmath>
#include <valarray>
#include <iostream>
#include "sample.H"
#include "rng.H"
#include "choose.H"
#include "likelihood.H"

#include "3way.H"
#include "alignment-sums.H"
#include "alignment-constraint.H"
#include "substitution-index.H"

///Sample between 2 topologies, ignoring gap priors on each case
int sample_SPR_and_A(vector<alignment>& a,vector<Parameters>& p,int n1, int n2) 
{
  //----------- Generate the Different node lists ---------//
  vector< vector<int> > nodes(2);
  nodes[0] = A3::get_nodes_branch_random(p[0].T,n1,n2);
  nodes[1] = A3::get_nodes_branch_random(p[1].T,n1,n2);

  return sample_tri_multi(a,p,nodes,true,true);
}

///Sample between 2 topologies, ignoring gap priors on each case
int topology_sample_SPR_sgaps(vector<alignment>& a,vector<Parameters>& p) 
{
  efloat_t Pr1 = p[0].probability(a[0],p[0]);
  efloat_t Pr2 = p[0].probability(a[1],p[1]);

  return choose2(Pr1,Pr2);
}

int topology_sample_SPR(vector<alignment>& a,vector<Parameters>& p,int n1, int n2) 
{
  assert(p[0].IModel().full_tree == p[1].IModel().full_tree);

  if (p[0].IModel().full_tree)
    return sample_SPR_and_A(a,p,n1,n2);
  else
    return topology_sample_SPR_sgaps(a,p);
}

SequenceTree do_SPR(const SequenceTree& T1, int n1, int n2, int b1) {
  //----- Select the branch to move to ------//
  valarray<bool> subtree_nodes = T1.partition(n1,n2);
  vector<int> branches;
  vector<double> lengths;

  for(int i=0;i<T1.n_branches();i++) {
    if (not subtree_nodes[T1.branch(i).target()] or 
	not subtree_nodes[T1.branch(i).source()]) {
      branches.push_back(i);
      lengths.push_back(T1.branch(i).length());
    }
  }

  int b2 = branches[ choose(lengths) ];

  SequenceTree T2 = T1;

  //------ Generate the new topology ------//
  if (T2.branch(b2).target() == n1 or T2.branch(b2).source() == n1)
    ;
  else {
    int b_temp = T2.directed_branch(n1,n2);
    T2.SPR(b_temp,b2);
  }

  //------ Find the two new branches ------//
  vector<int> connected;
  for(int i=0;i<T2.n_branches();i++) {
    if (i == b1) 
      continue;

    if (T2.branch(i).target() == n1 or T2.branch(i).source() == n1)
      connected.push_back(i);
  }
  assert(connected.size() == 2);
  

  //------- Place the split randomly -------//
  double total = T2.branch(connected[0]).length() + T2.branch(connected[1]).length();
  T2.branch(connected[0]).set_length( myrandomf() * total );
  T2.branch(connected[1]).set_length( total - T2.branch(connected[0]).length() );

  return T2;
}

void remove_duplicates(vector<int>& v) {
  for(int i=v.size()-1;i>=0;i--) {
    bool dup=false;
    for(int j=0;j<i and not dup;j++)
      if (v[j] == v[i]) dup=true;
    if (dup)
      v.erase(v.begin()+i);
  }
}

MCMC::result_t sample_SPR(alignment& A,Parameters& P,int b) {
  MCMC::result_t result(0.0,2);
  result[0] = 1.0;

  SequenceTree& T1 = P.T;

  //----- Get nodes for directed branch ------//
  int n1 = T1.branch(b).target();
  int n2 = T1.branch(b).source();
  if (myrandomf()< 0.5)
    std::swap(n1,n2);
  if (T1[n1].is_leaf_node())
    std::swap(n1,n2);

  //----- Generate the Different Topologies ----//
  P.LC.root = n1;
  vector<alignment> a(2,A);
  vector<Parameters> p(2,P);

  SequenceTree& T2 = p[1].T;

  // find the changed branches
  vector<int> branches;
  for(edges_after_iterator i=T2.directed_branch(n2,n1).branches_after();
      i;i++)
    branches.push_back((*i).undirected_name());
  //  std::cerr<<"before = "<<T1<<endl;
  T2 = do_SPR(T1,n1,n2,b);
  //  std::cerr<<"after = "<<T2<<endl;
  for(edges_after_iterator i=T2.directed_branch(n2,n1).branches_after();
      i;i++)
    branches.push_back((*i).undirected_name());


  remove_duplicates(branches);
    
  // recompute/invalidate caches associated with changed branch lengths
  assert(branches.size() <= 3);
  for(int i=0;i<branches.size();i++) {
    int b = branches[i];
    p[1].setlength(b,p[1].T.branch(b).length());
    invalidate_subA_index_branch(a[1], p[1].T, branches[i]);
  }
  
  int C = topology_sample_SPR(a,p,n1,n2);

  if (C != -1) 
  {
    valarray<bool> s1 = constraint_satisfied(P.alignment_constraint,A);
    A = a[C];
    P = p[C];
    valarray<bool> s2 = constraint_satisfied(P.alignment_constraint,A);

    // If the new topology conflicts with the constraints, then it should have P=0
    // and therefore not be chosen.  So the following SHOULD be safe!
    report_constraints(s1,s2);
  }

  if (C>0)
    result[1] = 1;

  return result;
}
