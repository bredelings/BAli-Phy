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
/// \file alignment-sums.hh
///
/// \brief Contains utility functions for sampling from and summing over parts of the alignment.
///

#include "alignment-sums.hh"
#include "2way.hh"
#include "3way.hh"
#include "probability/choose.hh"
#include "util/rng.hh"
#include <cmath>                                    // for pow
#include <cstdlib>                                  // for abort
#include <algorithm>                                // for find
#include <boost/dynamic_bitset/dynamic_bitset.hpp>  // for dynamic_bitset
#include <iostream>                                 // for operator<<, ostream
#include <string>                                   // for operator<<
#include "alignment/alignment.hh"                    // for operator<<
#include "dp/1way.hh"                                // for pairwise_alignment_t
#include "dp/A1_states.hh"                           // for E, S
#include "dp/dp-engine.hh"                           // for DPengine
#include "imodel/imodel.hh"                          // for PairHMM
#include "models/TreeInterface.hh"                   // for TreeInterface
#include "models/parameters.hh"                      // for data_partition
#include "substitution/likelihood.hh"                // for other_subst
#include "util/assert.hh"                           // for assert
#include "util/matrix.hh"                            // for matrix
#include "util/myexception.hh"                       // for show_stack_trace
#include "util/set.hh"                               // for includes

extern int log_verbose;

using boost::dynamic_bitset;
using std::vector;
using std::cerr;
using std::endl;
using std::optional;

matrix<int> get_path_counts(const pairwise_alignment_t& a)
{
  using namespace A1;

  matrix<int> counts(5,5,0);

  int prev = A1::states::S;
  for(int i=0;i<a.size();i++)
  {
      counts(prev, a.get_state(i))++;
      prev = a.get_state(i);
  }
  counts(prev, A1::states::E)++;

  return counts;
}

/// Probability of a pairwise alignment
log_double_t prior_branch_from_counts(const matrix<int>& counts,const indel::PairHMM& Q)
{
  using namespace A1;

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
log_double_t prior_branch(const pairwise_alignment_t& a,const indel::PairHMM& Q) 
{
  matrix<int> counts = get_path_counts(a);

  return prior_branch_from_counts(counts,Q);
}
ProbDensity other_subst(const data_partition& P, const vector<int>& nodes)
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
    for(int b: t.branches())
    {
	int target = t.target(b);
	int source = t.source(b);

	if (includes(nodes,target) and includes(nodes,source))
	    continue;

	p *= prior_branch(P.get_pairwise_alignment(b), P.get_branch_HMM(b));
    }


    // Add in the node length corrections
    for(int n: t.nodes())
    {
	if (t.is_leaf_node(n))
	    continue;

	if (includes(nodes,n)) {
	    if (includes(nodes, t.neighbors(n)))
		continue;
	}

	p /= pow(P.sequence_length_pr(n) , t.degree(n)-1);
    }

    return p;
}

/// Check offset between (HMM path probabilities) and P (true probabilities) 
void check_match_P(const data_partition& P, ProbDensity OS, log_double_t OP, const vector<int>& path,
                   const DPengine& Matrices)
{
    vector<int> path_g = Matrices.generalize(path);

    // This diagnostic compares conventional finite log probabilities; MCMC factor
    // registration retains the uncollapsed substitution density separately.
    log_double_t collapsed_OS = exp_to_log_space(static_cast<double>(OS.log()));

    //--- Compare path emission probability VS likelihood
    log_double_t qs = Matrices.path_Q_subst(path_g) * pow(collapsed_OS,P.get_beta());
    log_double_t ls = pow(P.likelihood(), P.get_beta());
  
    //--- Compare the path probability (Q) and collapsed/generalized path probability (GQ)
    log_double_t qpGQ = Matrices.path_GQ_path(path_g) *  Matrices.generalize_P(path);
    log_double_t qpQ  = Matrices.path_Q_path(path);

    int prec = cerr.precision(10);

    if (log_verbose >= 4)
	cerr<<"GQ(path) = "<<qpGQ<<"   Q(path) = "<<qpQ<<endl<<endl;
    // A retained alignment can have zero probability under proposed parameters.
    // Compare zeros directly instead of subtracting two negative infinities.
    assert(close_in_log_space(qpGQ, qpQ, 1.0e-9));
  
    //--- Compare the path transition probabilities (Q) and the alignment prior
    log_double_t qp = Matrices.path_Q_path(path) * OP;
    log_double_t lp = P.prior_alignment();

    //--- Compare the offset path probability and the true heated probability
    log_double_t qt = qs * qp;
    log_double_t lt = P.heated_likelihood() * P.prior_alignment();

    bool ok = close_in_log_space(qs, ls, 1.0e-9) &&
              close_in_log_space(qp, lp, 1.0e-9) &&
              close_in_log_space(qt, lt, 1.0e-9);

    if (log_verbose >= 4 or not ok)
    {
	cerr<<"ls = "<<ls<<"    qs = "<<qs<<endl;
	cerr<<"lp = "<<lp<<"    qp = "<<qp
	    <<" = "<<Matrices.path_GQ_path(path_g)<<" + "<<Matrices.generalize_P(path)<<" + "<<OP<<endl;
	cerr<<"lt = "<<lt<<"    qt = "<<qt<<endl;
	cerr<<endl;
    }

    if ( not ok )
    {
//	cerr<<P.A()<<endl;
	cerr<<"Can't match up DP probabilities to real probabilities!\n"<<show_stack_trace();
	std::abort();
    }

    cerr.precision(prec);
}

/// Computes true, sampling, and proposal probabilities
vector<log_double_t> sample_P(const data_partition& P,
			      log_double_t ratio, log_double_t rho,
			      const vector<int>& path, const DPengine& Matrices) 
{
    vector<log_double_t> PR(4);

    vector<int> path_g = Matrices.generalize(path);

    // Probability
    PR[0] = P.heated_likelihood() * P.prior_alignment();

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

	log_double_t ratio2 = (P2[0]*P2[2]/P2[1]) / P2[3];
	// The relative ratio must be near one; two zero ratios do not satisfy this check.
        auto relative_ratio = ratio2 / ratio1;
        bool ok = close_in_log_space(relative_ratio, log_double_t(1), 1.0e-9);
        double diff = log(relative_ratio);

	if (log_verbose >= 4 or not ok)
	{
	    cerr<<"\noption = "<<i<<"     rho"<<i<<" = "<<P2[2]<<endl;

	    cerr<<" Pr1 * Rho1  = "<<P1[0]*P1[2]<<"    Pr2 * Rho2  = "<<P2[0]*P2[2]<<
		"    Pr2 * Rho2  - Pr1 * Rho1  = "<<P2[0]*P2[2]/(P1[0]*P1[2])<<endl;

	    cerr<<" PrS1 = "<<P1[1]<<"    PrS2 = "<<P2[1]<<"    PrS2 - PrS1 = "<<P2[1] / PR.back()[1]<<endl;

	    cerr<<"diff = "<<diff<<endl;
	}
    
	if (not ok) {
	    //      cerr<<a.back()<<endl;
	    //      cerr<<a[i]<<endl;
	    cerr<<"i = "<<i<<endl;
      
	    throw myexception()<<"Sampling probabilities were incorrect\n"<<show_stack_trace();
	}
    }
}

// Incident branch factors count the middle length once per branch; the tree prior
// counts it once. This is the factor removed from the DP proposal by MH acceptance.
log_double_t node_length_correction(const data_partition& P, int node)
{
    if (not P.variable_alignment())
        return 1;
    return pow(P.sequence_length_pr(node), P.t().degree(node)-1);
}

// Independent alignment partitions contribute multiplicatively to the correction.
log_double_t node_length_correction(const Parameters& P, int node)
{
    log_double_t C = 1;
    for(int j=0;j<P.n_data_partitions();j++)
        C *= node_length_correction(P[j], node);
    return C;
}

sample_star_multi_calculation::sample_star_multi_calculation(vector<Parameters>& pp,const vector< vector<int> >& nodes_,
                                                         optional<int> b)
    :
    p(pp),
#ifndef NDEBUG_DP
    P0(p[0]),
#endif
    nodes(nodes_),
    Matrices(p.size()),
    sampled(p.size(), vector<bool>(p[0].n_data_partitions(), false)),
    Pr(p.size()),
    bandwidth(b)
{
}

void sample_star_multi_calculation::run_dp()
{
    assert(p.size() == nodes.size());

    //------------ Check the alignment branch constraints ------------//
    /*
      for(int i=0;i<p.size();i++) {
      vector<int> branches;
      branches.push_back(p[i].t().find_branch(nodes[i][0],nodes[i][1]));
      branches.push_back(p[i].t().find_branch(nodes[i][0],nodes[i][2]));
      branches.push_back(p[i].t().find_branch(nodes[i][0],nodes[i][3]));

      //    if (any_branches_constrained(branches, p[i].t(), p[i].PC->TC, p[i].PC->AC))
      //      return;// -1;
      }
    */

    //----------- Generate the different states and Matrices ---------//
    C1 = node_length_correction(p[0], nodes[0][0]);

    for(int i=0;i<p.size();i++) 
    {
        auto joint_sampling_pr = available(log_double_t(1.0));
        log_double_t correction = 1.0;

#ifndef NDEBUG
	Matrices[i].resize(p[i].n_data_partitions());
#endif
	for(int j=0;j<p[i].n_data_partitions();j++) {
	    if (p[i][j].variable_alignment())
            {
		auto [M, sampling_pr] = compute_matrix(i,j);
#ifndef NDEBUG
		Matrices[i][j] = M;
#endif
		sampled[i][j] = sampling_pr.has_value();
		joint_sampling_pr *= sampling_pr;
		if (not sampling_pr)
		    continue;
		correction *= node_length_correction(p[i][j], nodes[i][0]);
            }
	}

        if (joint_sampling_pr and std::isfinite(joint_sampling_pr->log()))
        {
            auto factor = correction / *joint_sampling_pr;
            Pr[i] = factor * available(ChoiceWeight(p[i].heated_probability()));

#ifndef NDEBUG
            // A complete sampling probability means that every affected alignment was replaced.
            // Validate the candidate before a later choice changes the active state.
            for(int j=0;j<p[i].n_data_partitions();j++)
                if (p[i][j].has_pairwise_alignments())
                    for(int b: p[i].t().directed_branches())
                    {
                        auto A = p[i][j].get_pairwise_alignment(b);
                        int n1 = p[i].t().source(b);
                        int n2 = p[i].t().target(b);
                        assert(A.length1() == p[i][j].seqlength(n1));
                        assert(A.length2() == p[i][j].seqlength(n2));
                    }
#endif
        }
    }
}

void sample_star_multi_calculation::set_proposal_probabilities(const vector<log_double_t>& r)
{
    rho.resize(Pr.size());
    for(int i=0;i<Pr.size();i++) 
    {
	rho[i] = r[i];
	Pr[i] = rho[i] * Pr[i];
    }
}

optional<int> sample_star_multi_calculation::choose(bool correct)
{
    assert(p.size() == nodes.size());

    int C;
    try {
	auto choice = choose_MH(0,Pr);
	if (not choice)
	    return {};
	C = *choice;
    }
    catch (choose_exception<Availability<ChoiceWeight>>& c)
    {
	c.prepend(std::string(__PRETTY_FUNCTION__)+"\n");

	c<<show_parameters(p[0]);
	c<<p[0].probability()<<" = "<<p[0].likelihood()<<" + "<<p[0].prior()<<"\n";

	throw c;
    }

    // \todo What do we do if partition 0 works, but other partitions fail cuz of constraints?
    assert(Pr[C]);

#ifndef NDEBUG_DP
    if (log_verbose >= 4) std::cerr<<"choice = "<<C<<endl;

    // FIXME: check that alignment of unaffected sequences w/in 2 blocks is unchanged!
    
    // Add another entry for the incoming configuration
    p.push_back( P0 );
    nodes.push_back(nodes[0]);
    rho.push_back( rho[0] );
    Matrices.push_back( Matrices[0] );
    sampled.push_back(sampled[0]);

    vector< vector< vector<int> > > paths(p.size());

    // For the choices that violate a constraint, why don't we just ignore them?
    // Well, this shifts the indices of each choice.  So, we don't do it yet.

    //------------------- Check offsets from path_Q -> P -----------------//
    for(int i=0;i<p.size();i++) 
    {
	bool ok = i >= Pr.size() or Pr[i].has_value();

	if (not ok)
	    assert(i != 0 and i != p.size()-1);

        // Compare at the DP combination node so numerical rerooting error does not
        // obscure this check. The copy leaves the candidate's computational roots unchanged.
        auto diagnostic = p[i];
        if (ok)
            diagnostic.set_root(nodes[i][0]);

	for(int j=0;j<p[i].n_data_partitions();j++) 
	    if (p[i][j].variable_alignment() and ok and sampled[i][j])
	    {
		assert(nodes[i].size() == 3 or nodes[i].size() == 4);
                auto bitpath = nodes[i].size() == 3 ? A2::get_bitpath(p[i][j], nodes[i])
                                                   : A3::get_bitpath(p[i][j], nodes[i]);
                paths[i].push_back(get_path_unique(bitpath, *Matrices[i][j]));
	  
		auto OS = other_subst(diagnostic[j],nodes[i]);
		auto OP = other_prior(diagnostic[j],nodes[i]) / node_length_correction(diagnostic[j], nodes[i][0]);
	
		check_match_P(diagnostic[j], OS, OP, paths[i][j], *Matrices[i][j]);
	    }
	    else
		paths[i].push_back( vector<int>() );
    }

    //--------- Compute path probabilities and sampling probabilities ---------//
    vector< vector<log_double_t> > PR(p.size(), vector<log_double_t>(4,1));

    for(int i=0;i<p.size();i++)
    {
	bool ok = i >= Pr.size() or Pr[i].has_value();

	if (not ok) {
	    PR[i][0] = 0;
	    assert(i != 0 and i != p.size()-1);
	    continue;
	}

	log_double_t choice_ratio = 1;
	if (i > 0 and i<Pr.size())
	{
	    auto forward = choose_MH_P(0, i, Pr);
	    auto reverse = choose_MH_P(i, 0, Pr);
	    if (not forward or not reverse or *reverse == 0.0)
	    {
		PR[i][0] = 0;
		continue;
	    }
	    choice_ratio = *forward / *reverse;
	}
	else
	    choice_ratio = 1;

	//    sample_P(p[i], choice_ratio, rho[i], paths[i], Matrices[i]) );
	PR[i][0] = p[i].heated_probability();
	PR[i][2] = rho[i];
	PR[i][3] = choice_ratio;
	for(int j=0;j<p[i].n_data_partitions();j++)
	    if (p[i][j].variable_alignment() and sampled[i][j])
	    {
		vector<int> path_g = Matrices[i][j]->generalize(paths[i][j]);
		PR[i][0] *= node_length_correction(p[i][j], nodes[i][0]);
		PR[i][1] *= Matrices[i][j]->path_P(path_g)* Matrices[i][j]->generalize_P(paths[i][j]);
	    }
    }

    //--------- Check that each choice is sampled w/ the correct Probability ---------//
    check_sampling_probabilities(PR);
#endif

    // Correct for the change in the middle sequence length in each partition.
    // See Appendix A of Redelings & Suchard (2007) for an explanation.

    log_double_t C2 = node_length_correction(p[C], nodes[C][0]);
    // If we reject the proposed move, then don't do anything.
    if (correct and uniform() > double(C1/C2))
	return {};

    return C;
}

