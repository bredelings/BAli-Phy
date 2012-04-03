/*
   Copyright (C) 2004-2009 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

#include <valarray>
#include <iostream>
#include <cmath>
#include "sample.H"
#include "2way.H"
#include "alignment-sums.H"
#include "alignment-constraint.H"
#include "alignment-util.H"
#include "substitution.H"
#include "substitution-index.H"
#include "dp-matrix.H"
#include <boost/shared_ptr.hpp>
#include "timer_stack.H"

// SYMMETRY: Because we are only sampling from alignments with the same fixed length
// for both sequences, this process is symmetric

using std::abs;
using std::vector;
using boost::dynamic_bitset;
using namespace A2;

vector< Matrix > distributions_star(const data_partition& P,const vector<int>& seq,int b,bool up) 
{
  //--------------- Find our branch, and orientation ----------------//
  const SequenceTree& T = P.T();
  int root = T.branch(b).target();      //this is an arbitrary choice

  int node1 = T.branch(b).source();
  int node2 = T.branch(b).target();
  if (not up) std::swap(node1,node2);

  dynamic_bitset<> group = T.partition(node1,node2);

  return ::distributions_star(P,seq,root,group);
}

vector< Matrix > distributions_tree(const data_partition& P,const vector<int>& seq,int b,bool up)
{
  //--------------- Find our branch, and orientation ----------------//
  const SequenceTree& T = P.T();
  int root = T.branch(b).target();      //this is an arbitrary choice

  int node1 = T.branch(b).source();
  int node2 = T.branch(b).target();
  if (not up) std::swap(node1,node2);

  dynamic_bitset<> group = T.partition(node1,node2);

  return ::distributions_tree(P,seq,root,group);
}

typedef vector< Matrix > (*distributions_t_local)(const data_partition&,
						  const vector<int>&,int,bool);

boost::shared_ptr<DPmatrixSimple> sample_alignment_base(data_partition& P,int b) 
{
  default_timer_stack.push_timer("alignment::DP2/2-way");
  assert(P.variable_alignment());

  dynamic_bitset<> s1 = constraint_satisfied(P.alignment_constraint, *P.A);

  const Tree& T = P.T();
  alignment& A = *P.A;

  const Matrix frequency = substitution::frequency_matrix(P.SModel());

  int node1 = T.branch(b).target();
  int node2 = T.branch(b).source();

  dynamic_bitset<> group1 = T.partition(node2,node1);

  // Find sub-alignments and sequences
  vector<int> seq1;
  vector<int> seq2;
  vector<int> seq12;

  for(int column=0;column<A.length();column++)
  {
    if (not A.gap(column,node1))
      seq1.push_back(column);
    if (not A.gap(column,node2))
      seq2.push_back(column);

    if (not A.gap(column,node1) or A.gap(column,node2))
      seq12.push_back(column);
  }

  /******** Precompute distributions at node2 from the 2 subtrees **********/
  distributions_t_local distributions = distributions_tree;
  //  if (not P.smodel_full_tree)
  //    distributions = distributions_star;

  vector< Matrix > dists1 = distributions(P,seq1,b,true);
  vector< Matrix > dists2 = distributions(P,seq2,b,false);

  vector<int> state_emit(4,0);
  state_emit[0] |= (1<<1)|(1<<0);
  state_emit[1] |= (1<<1);
  state_emit[2] |= (1<<0);
  state_emit[3] |= 0;

  boost::shared_ptr<DPmatrixSimple> 
    Matrices( new DPmatrixSimple(state_emit, P.get_branch_HMM(b).start_pi(),
				 P.get_branch_HMM(b), P.get_beta(),
				 P.SModel().distribution(), dists1, dists2, frequency)
	      );

  //------------------ Compute the DP matrix ---------------------//
  vector<int> path_old = get_path(A,node1,node2);
  vector<vector<int> > pins = get_pins(P.alignment_constraint,A,group1,~group1,seq1,seq2,seq12);

  Matrices->forward_constrained(pins);

  // If the DP matrix ended up having probability 0, don't try to sample a path through it!
  if (Matrices->Pr_sum_all_paths() <= 0.0)
  {
    std::cerr<<"sample_alignment_base( ): All paths have probability 0!"<<std::endl;
    default_timer_stack.pop_timer();
    return Matrices;
  }

  vector<int> path = Matrices->sample_path();

  path.erase(path.begin()+path.size()-1);

  *P.A = construct(A,path,node1,node2,T,seq1,seq2);
  P.LC.invalidate_branch_alignment(T,b);
  P.set_pairwise_alignment(T.directed_branch(node1,node2), A2::get_pairwise_alignment_from_path(path));

#ifndef NDEBUG_DP
  assert(valid(*P.A));

  vector<int> path_new = get_path(*P.A, node1, node2);
  path.push_back(3);
  assert(path_new == path);
#endif

  default_timer_stack.pop_timer();
  return Matrices;
}

void sample_alignment(Parameters& P,int b)
{
  if (any_branches_constrained(vector<int>(1,b), *P.T, *P.TC, P.AC))
    return;

  P.select_root(b);

  vector<dynamic_bitset<> > s1(P.n_data_partitions());
  for(int i=0;i<P.n_data_partitions();i++) 
  {
    s1[i].resize(P[i].alignment_constraint.size1());
    s1[i] = constraint_satisfied(P[i].alignment_constraint, *P[i].A);
#ifndef NDEBUG
    check_alignment(*P[i].A, P[i].T(), "tri_sample_alignment:in");
#endif
  }

#if !defined(NDEBUG_DP) || !defined(NDEBUG)
  const Parameters P0 = P;
#endif
  vector<Parameters> p;
  p.push_back(P);

  vector< vector< boost::shared_ptr<DPmatrixSimple> > > Matrices(1);
  for(int i=0;i<p.size();i++) 
  {
    for(int j=0;j<p[i].n_data_partitions();j++) 
      if (p[i][j].variable_alignment()) 
      {
	Matrices[i].push_back(sample_alignment_base(p[i][j], b));
	// If Pr_sum_all_paths() == 0, then the alignment for this partition will be unchanged.
#ifndef NDEBUG
	check_subA(*P0[j].subA, *P0[j].A, *p[i][j].subA, *p[i][j].A, *p[0].T);
	p[i][j].likelihood();  // check the likelihood calculation
#endif
      }
      else
	Matrices[i].push_back(boost::shared_ptr<DPmatrixSimple>());
  }

#ifndef NDEBUG_DP
  std::cerr<<"\n\n----------------------------------------------\n";

  int node1 = P.T->branch(b).target();
  int node2 = P.T->branch(b).source();

  vector<int> nodes;
  nodes.push_back(node1);
  nodes.push_back(node2);

  p.push_back(P0);
  Matrices.push_back(Matrices[0]);

  vector< vector< efloat_t > > OS(p.size());
  vector< vector< efloat_t > > OP(p.size());
  vector< vector< vector<int> > > paths(p.size());

  //------------------- Check offsets from path_Q -> P -----------------//
  for(int i=0;i<p.size();i++) 
    for(int j=0;j<p[i].n_data_partitions();j++) 
      if (p[i][j].variable_alignment())
      {
	paths[i].push_back( get_path(*p[i][j].A, node1, node2) );
    
	OS[i].push_back( other_subst(p[i][j],nodes) );
	OP[i].push_back( other_prior(p[i][j],nodes) );
	
	check_match_P(p[i][j], OS[i][j], OP[i][j], paths[i][j], *Matrices[i][j]);
      }
      else {
	paths[i].push_back( vector<int>() );
	OS[i].push_back( 1 );
	OP[i].push_back( 1 );
      }
  
  //--------- Compute path probabilities and sampling probabilities ---------//
  vector< vector<efloat_t> > PR(p.size());

  for(int i=0;i<p.size();i++)
  {
    // sample_P(p[i][j], 1, 1, paths[i][j], *Matrices[j]);
    PR[i] = vector<efloat_t>(4,1);
    PR[i][0] = p[i].heated_probability();
    for(int j=0;j<p[i].n_data_partitions();j++) 
      if (p[i][j].variable_alignment())
      {
	vector<int> path_g = Matrices[i][j]->generalize(paths[i][j]);
	PR[i][1] *= Matrices[i][j]->path_P(path_g)* Matrices[i][j]->generalize_P(paths[i][j]);
      }
  }

  for(int i=0;i<p[1].n_data_partitions();i++) 
  {
    if (paths[0][i].size() > paths[1][i].size())
      std::cerr<<"path "<<i+1<<" got longer by "<<paths[0][0].size() - paths[1][0].size()<<"!\n";
    if (paths[0][i].size() < paths[1][i].size())
      std::cerr<<"path "<<i+1<<" got shorter by "<<paths[1][0].size() - paths[0][0].size()<<"!\n";
  }

  //--------- Check that each choice is sampled w/ the correct Probability ---------//
  check_sampling_probabilities(PR);

  //--------- Check construction of A  ---------//
  for(int j=0;j<P.n_data_partitions();j++) 
  {
    double diff = log(PR[0][0]) - log(PR[1][0]);
    std::cerr<<"before = "<<PR[1][0]<<"       after = "<<PR[0][0]<<
      " diff = "<<diff<<std::endl;

    if (diff < -10) {
      efloat_t L1 = p[1].likelihood();
      efloat_t L2 = p[0].likelihood();
      
      efloat_t prior1 = p[1].prior();
      efloat_t prior2 = p[0].prior();
      
      std::cerr<<"Yelp!\n";
      
      std::cerr<<std::endl;
      std::cerr<<"DELTA Likelihood = "<<L2/L1<<std::endl;
      std::cerr<<"DELTA prior = "<<prior2/prior1<<std::endl;
      std::cerr<<std::endl;
      
      std::cerr<<"Sampling probability of good path is: "<<PR[1][1]<<std::endl;
    }
  }

#endif

  for(int i=0;i<P.n_data_partitions();i++) 
  {
#ifndef NDEBUG
    check_alignment(*P[i].A, P[i].T(),"tri_sample_alignment:out");
#endif

    dynamic_bitset<> s2 = constraint_satisfied(P[i].alignment_constraint, *P[i].A);
    report_constraints(s1[i],s2,i);
  }
}

