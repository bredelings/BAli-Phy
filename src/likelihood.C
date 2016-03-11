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

#include "likelihood.H"
#include "substitution/substitution.H"
#include "setup.H"
#include "probability/probability.H"
#include "alignment/alignment-util.H"
#include "util.H"
#include "dp/2way.H"

/// Tree prior: topology & branch lengths (exponential)
log_double_t prior_exponential(const TreeInterface& t,double branch_mean) 
{
  log_double_t p = 1;

  // --------- uniform prior on topologies --------//
  if (t.n_leaves()>3)
    p /= num_topologies(t.n_leaves());

  // ---- Exponential prior on branch lengths ---- //
  for(int i=0;i<t.n_branches();i++) 
    p *= exponential_pdf(t.branch_length(i), branch_mean);

  return p;
}

/// Tree prior: topology & branch lengths (gamma)
log_double_t prior_gamma(const TreeInterface& t,double branch_mean) 
{
  log_double_t p = 1;

  // --------- uniform prior on topologies --------//
  if (t.n_leaves()>3)
    p /= num_topologies(t.n_leaves());

  // ---- Exponential prior on branch lengths ---- //
  double a = 0.5;
  double b = branch_mean*2;

  for(int i=0;i<t.n_branches();i++) 
    p *= gamma_pdf(t.branch_length(i), a, b);

  return p;
}

/// Tree prior: topology & branch lengths (dirichlet)
log_double_t prior_dirichlet(const TreeInterface& t,double branch_mean) 
{
  log_double_t p = 1;

  // --------- uniform prior on topologies --------//
  if (t.n_leaves()>3)
    p /= num_topologies(t.n_leaves());

  // ---- Gamma (sum) + Dirichlet (relative lengths) prior on branch lengths ---- //
  std::valarray<double> branch_lengths(t.n_branches());
  for(int i=0;i<branch_lengths.size();i++)
    branch_lengths[i] = t.branch_length(i);
  double branch_length_sum = branch_lengths.sum();
  branch_lengths /= branch_length_sum;

  // The branch-length sum.
  p *= gamma_pdf(branch_length_sum, 0.5, branch_mean*2.0);

  // The relative branch lengths.  Probably I should only do the 
  p *= dirichlet_pdf(branch_lengths, 0.5);

  // The current method has uncertainly in the total length, PLUS uncertain in the partition-specific
  //   scaling factor.
  // So, perhaps the sum should be partition-specific, and I should only do the relative branch lengths
  //   here.
  // That might require that changing a single length changes all the lengths, though.
  return p;
}

/// Tree prior: branch lengths & topology
log_double_t prior(const Parameters& P, const TreeInterface& t,double branch_mean) 
{
  log_double_t p = 1;

  if (P.branch_prior_type() == 0)
    p *= prior_exponential(t, branch_mean);
  else if (P.branch_prior_type() == 1)
    p *= prior_gamma(t, branch_mean);
  else if (P.branch_prior_type() == 2)
    p *= prior_dirichlet(t, branch_mean);
  else
    throw myexception()<<"I don't understand branch prior type = "<<P.branch_prior_type();

  return p;
}

matrix<int> get_path_counts(const alignment& A,int node1, int node2) 
{
  using namespace A2;

  int state1 = states::S;

  matrix<int> counts(5,5,0);

  for(int column=0;column<A.length();column++) 
  {
    int state2 = -1;
    if (A.gap(column,node1)) {
      if (A.gap(column,node2)) 
       continue;
      else
       state2 = states::G1;
    }
    else {
      if (A.gap(column,node2))
       state2 = states::G2;
      else
       state2 = states::M;
    }

    counts(state1,state2)++;
    state1 = state2;
  }

  counts(state1,states::E)++;

  return counts;
}

/// Probability of a pairwise alignment
log_double_t prior_branch_from_counts(const matrix<int>& counts,const indel::PairHMM& Q)
{
  using namespace A2;

  log_double_t P=1;

  // Account for S-? start probability
  for(int i=0;i<Q.size2();i++)
    if (counts(states::S,i))
      P *= Q.start(i);

  // Account for the mass of transitions
  for(int i=0;i<3;i++)
    for(int j=0;j<3;j++) {
      log_double_t Qij = Q(i,j);
      // FIXME - if we propose really bad indel parameters, we can get log(Q_ij) where Qij == 0
      if (counts(i,j))
	P *= pow(Qij,counts(i,j));
    }
  
  // Account for ?-E end probability
  if (not counts(states::S,states::E))
    for(int i=0;i<Q.size1();i++)
      if (counts(i,states::E))
       P *= Q(i,states::E);

  return P;
}

/// Probability of a pairwise alignment
log_double_t prior_branch(const alignment& A,const indel::PairHMM& Q,int target,
int source) 
{
  matrix<int> counts = get_path_counts(A,target,source);

  return prior_branch_from_counts(counts,Q);
}

/// Probability of a multiple alignment if branch alignments independent
log_double_t prior_HMM_nogiven(const data_partition& P) 
{
  const alignment& A = P.A();
  const auto  t = P.t();

#ifndef NDEBUG
  assert(P.has_IModel());
  check_internal_nodes_connected(A,t);
#endif
  
  log_double_t Pr = 1;

  for(int b=0;b<t.n_branches();b++) {
    int target = t.target(t.undirected(b));
    int source  = t.source(t.undirected(b));
    Pr *= prior_branch(A, P.get_branch_HMM(b), target, source);
  }
  
  return Pr;
}


log_double_t prior_HMM_rootless_scale(const data_partition& P)
{
  const auto& t = P.t();

#ifndef NDEBUG
  assert(P.has_IModel());
  check_internal_nodes_connected(P.A(),t);
#endif
  
  log_double_t Pr = 1;

  for(int i=t.n_leaves();i<t.n_nodes();i++) {
    int l = P.seqlength(i);
    log_double_t temp = P.sequence_length_pr(l);
    Pr /= (temp*temp);
  }

  return Pr;
}

//NOTE  - this will have to change if we ever have sequences at internal nodes
//        that have other than 3 neighbors
log_double_t prior_HMM(const data_partition& P) 
{
  return prior_HMM_nogiven(P) * prior_HMM_rootless_scale(P);
}
