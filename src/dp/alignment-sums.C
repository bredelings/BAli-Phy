/*
   Copyright (C) 2004-2007,2009-2010 Benjamin Redelings

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

///
/// \file alignment-sums.H
///
/// \brief Contains utility functions for sampling from and summing over parts of the alignment.
///

#include "alignment-sums.H"
#include "likelihood.H"
#include "substitution/substitution.H"
#include "util.H"

using boost::dynamic_bitset;
using std::vector;
using std::cerr;
using std::endl;

log_double_t other_subst(const data_partition& P, const vector<int>& nodes) 
{
  return substitution::other_subst(P,nodes);
}

log_double_t other_prior(const data_partition& P,const vector<int>& nodes) 
{
  if (not P.variable_alignment()) 
    return 1;

  auto t = P.t();

  log_double_t p = 1;

  // Add in the branch alignments
  for(int b=0;b<t.n_branches();b++) {
    int target = t.target(b);
    int source = t.source(b);

    if (includes(nodes,target) and includes(nodes,source))
      continue;

    p *= prior_branch(P.get_pairwise_alignment(b), P.get_branch_HMM(b));
  }


  // Add in the node length corrections
  for(int n=0;n<t.n_nodes();n++) {
    if (t.is_leaf_node(n))
      continue;

    if (includes(nodes,n)) {
      if (includes(nodes, t.neighbors(n)))
	continue;
    }

    p /= pow(P.sequence_length_pr(P.seqlength(n)) , 2.0);
  }

  return p;
}

/// Check offset between (HMM path probabilities) and P (true probabilities) 
void check_match_P(const data_partition& P, log_double_t OS, log_double_t OP, const vector<int>& path, const DPengine& Matrices) 
{
  vector<int> path_g = Matrices.generalize(path);

  //--- Compare path emission probability VS likelihood
  log_double_t qs = Matrices.path_Q_subst(path_g) * pow(OS,P.get_beta());
  log_double_t ls = pow(P.likelihood(), P.get_beta());
  
  //--- Compare the path probability (Q) and collapsed/generalized path probability (GQ)
  log_double_t qpGQ = Matrices.path_GQ_path(path_g) *  Matrices.generalize_P(path);
  log_double_t qpQ  = Matrices.path_Q_path(path);

  cerr<<"GQ(path) = "<<qpGQ<<"   Q(path) = "<<qpQ<<endl<<endl;
  assert(std::abs(log(qpGQ)-log(qpQ)) < 1.0e-9);
  
  //--- Compare the path transition probabilities (Q) and the alignment prior
  log_double_t qp = Matrices.path_Q_path(path) * OP;
  log_double_t lp = P.prior_alignment();

  //--- Compare the offset path probability and the true heated probability
  log_double_t qt = qs * qp * P.prior_no_alignment();
  log_double_t lt = P.heated_probability();

  cerr<<"ls = "<<ls<<"    qs = "<<qs<<endl;
  cerr<<"lp = "<<lp<<"    qp = "<<qp
	   <<" = "<<Matrices.path_GQ_path(path_g)<<" + "<<Matrices.generalize_P(path)<<" + "<<OP<<endl;
  cerr<<"lt = "<<lt<<"    qt = "<<qt<<endl;
  cerr<<endl;

  if ( (std::abs(log(qs) - log(ls)) > 1.0e-9) or 
       (std::abs(log(qp) - log(lp)) > 1.0e-9) or 
       (std::abs(log(qt) - log(lt)) > 1.0e-9)) {
    cerr<<P.A()<<endl;
    cerr<<"Can't match up DP probabilities to real probabilities!\n"<<show_stack_trace();
    std::abort();
  }

}

/// Computes true, sampling, and proposal probabilities
vector<log_double_t> sample_P(const data_partition& P,
			  log_double_t ratio, log_double_t rho,
			  const vector<int>& path, const DPengine& Matrices) 
{
  vector<log_double_t> PR(4);

  vector<int> path_g = Matrices.generalize(path);

  // Probability
  PR[0] = P.heated_probability();

  // Probability of sampling A | i
  PR[1] = Matrices.path_P(path_g) * Matrices.generalize_P(path);

  // Proposal probability
  PR[2] = rho;

  // Ratio of P_0i/P_i0
  PR[3] = ratio;

  //  cerr<<"PrS = "<<P_choice<<" + "<<Matrices.path_P(path_g)<<" + "<<Matrices.generalize_P(path)<<endl;

  return PR;
}

// pi[i]*rho[i]*Q(i,0) = pi[0]*rho[0]*Q(0,i)

// P(A,i) * rho[i] * choose_p(i->0) * P(A'|0) = P(A',0) * rho[0] * choose_p(0->i) * P(A|i)

/// Check that [ pi(A,i) * rho[i] / P(A|i) ] * P(i,0)/P(0,i) = [ pi(A`,0) * rho[0] / P(A`|0) ]

void check_sampling_probabilities(const vector< vector<log_double_t> >& PR) 
{
  const vector<log_double_t>& P1 = PR.back();
  log_double_t ratio1 = P1[0]*P1[2]/P1[1];

  if (PR.back()[0] == 0.0 or PR[0][0] == 0.0)
    throw myexception()<<"check_sampling_probabilities: Default choice should not be impossible!";

  for(int i=0;i<PR.size();i++) 
  {
    const vector<log_double_t>& P2 = PR[i];
    
    if (P2[0] == 0.0) continue;

    cerr<<"\noption = "<<i<<"     rho"<<i<<" = "<<P2[2]<<endl;

    cerr<<" Pr1 * Rho1  = "<<P1[0]*P1[2]<<"    Pr2 * Rho2  = "<<P2[0]*P2[2]<<
      "    Pr2 * Rho2  - Pr1 * Rho1  = "<<P2[0]*P2[2]/(P1[0]*P1[2])<<endl;

    cerr<<" PrS1 = "<<P1[1]<<"    PrS2 = "<<P2[1]<<"    PrS2 - PrS1 = "<<P2[1] / PR.back()[1]<<endl;
    
    log_double_t ratio2 = (P2[0]*P2[2]/P2[1]) / P2[3];
    double diff = log(ratio2/ratio1);

    cerr<<"diff = "<<diff<<endl;
    if (std::abs(diff) > 1.0e-9) {
      //      cerr<<a.back()<<endl;
      //      cerr<<a[i]<<endl;
      cerr<<"i = "<<i<<endl;
      
      throw myexception()<<"Sampling probabilities were incorrect\n"<<show_stack_trace();
    }
  }
}
