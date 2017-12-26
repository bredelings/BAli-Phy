/*
  Copyright (C) 2004-2007,2009-2012,2017 Benjamin Redelings

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

#include <iostream>
#include <cmath>

#include "probability/choose.H"
#include "util.H"
#include "rng.H"
#include "dp/3way.H"
#include "dp/alignment-sums.H"
#include "alignment/alignment-util.H"
#include "alignment/alignment-util2.H"
#include "alignment/alignment-constraint.H"
#include <boost/shared_ptr.hpp>
#include "dp/dp-cube.H"
#include "substitution/substitution.H"

//Assumptions:
//  a) we assume that the internal node is the parent sequence in each of the sub-alignments

using std::abs;
using std::vector;
using std::pair;
using std::endl;
using boost::dynamic_bitset;

boost::shared_ptr<DPcubeSimple> cube_sample_alignment_base(data_partition P, const data_partition& P0, 
							   const vector<int>& nodes, const vector<int>& nodes0,
							   int bandwidth)
{
    const auto t = P.t();
    const auto t0 = P0.t();
  
    assert(P.variable_alignment());

    assert(t.is_connected(nodes[0],nodes[1]));
    assert(t.is_connected(nodes[0],nodes[2]));
    assert(t.is_connected(nodes[0],nodes[3]));

    assert(t0.is_connected(nodes[0],nodes[1]));
    assert(nodes0[0] == nodes[0]);
    bool tree_changed = not t0.is_connected(nodes[0],nodes[2]) or not t0.is_connected(nodes[0],nodes[3]);

    // If the tree changed, assert that previously nodes 2 and 3 were connected.
    if (tree_changed)
    {
	assert(bandwidth < 0);
	assert(t0.is_connected(nodes[2],nodes[3]));
    }
    else
    {
	assert(t0.is_connected(nodes[0],nodes[2]));
	assert(t0.is_connected(nodes[0],nodes[3]));
    }

    // std::cerr<<"A = "<<A<<endl;

    int b1 = t.find_branch(nodes[1],nodes[0]);
    int b2 = t.find_branch(nodes[2],nodes[0]);
    int b3 = t.find_branch(nodes[3],nodes[0]);

    HMM m1 = P.get_branch_HMM(b1);
    m1.remap_bits({0,3});
    HMM m2 = P.get_branch_HMM(t.reverse(b2));
    m2.remap_bits({3,1});
    HMM m3 = P.get_branch_HMM(t.reverse(b3));
    m3.remap_bits({3,2});

    HMM m123 = Glue(m1,Glue(m2,m3));
    m123.hidden_bits.set(3);
    m123.B = P.get_beta();

    //------------- Compute sequence properties --------------//
    dynamic_bitset<> group1 = t.partition(t.find_branch(nodes[0],nodes[1]));
    dynamic_bitset<> group2 = t.partition(t.find_branch(nodes[0],nodes[2]));
    dynamic_bitset<> group3 = t.partition(t.find_branch(nodes[0],nodes[3]));

    vector<HMM::bitmask_t> a23;

    HMM::bitmask_t m23; m23.set(1); m23.set(2);

    if (tree_changed)
    {
	int b4 = t0.find_branch(nodes[2],nodes[3]);
	// Does this give the right order so that the move is reversible?
	// FIXME: Check that when we project a123_new to a12_new at the end, this does not change!!!
	a23 = convert_to_bits(P0.get_pairwise_alignment(b4),1,2);

	// The branch that the subtree was pruned from.
	int b5 = t.find_branch(nodes0[2],nodes0[3]);
	assert(t0.is_connected(nodes0[2],nodes0[0]));
	assert(t0.is_connected(nodes0[3],nodes0[0]));

	// Make sure the column order on the pruned branch matches the projected column order from the original alignment.
	//    vector<HMM::bitmask_t> b123 = A3::get_bitpath(P0, nodes0);
	//    P.set_pairwise_alignment(b5, get_pairwise_alignment_from_bits(b123,1,2));
    }
    else
    {
	vector<HMM::bitmask_t> a123 = A3::get_bitpath(P, nodes);
	a23 = remove_silent(a123, m23);
    }

    auto dists1 = substitution::get_column_likelihoods(P, {b1}, get_indices_n(P.seqlength(nodes[1])), 2);
    auto dists2 = substitution::get_column_likelihoods(P, {b2}, get_indices_n(P.seqlength(nodes[2])), 2);
    auto dists3 = substitution::get_column_likelihoods(P, {b3}, get_indices_n(P.seqlength(nodes[3])), 2);

    //-------------- Create alignment matrices ---------------//

    vector<int> branches(3);
    for(int i=0;i<3;i++)
	branches[i] = t.find_branch(nodes[0],nodes[i+1]);

    boost::shared_ptr<DPcubeSimple>
	Matrices(new DPcubeSimple(m123, std::move(dists1), std::move(dists2), std::move(dists3), P.WeightedFrequencyMatrix()));
    Matrices->forward_cube();

    if (Matrices->Pr_sum_all_paths() <= 0.0) 
	std::cerr<<"Constraints give this choice probability 0"<<std::endl;

    // If the DP matrix ended up having probability 0, don't try to sample a path through it!
    if (Matrices->Pr_sum_all_paths() <= 0.0) 
    {
#ifndef NDEBUG_DP
	Matrices->clear();
#endif
	return Matrices;
    }

    vector<int> path_g = Matrices->sample_path();

    vector<int> path = Matrices->ungeneralize(path_g);

    for(int i=0;i<3;i++) {
	int b = t.find_branch(nodes[0],nodes[i+1]);
	P.set_pairwise_alignment(b, get_pairwise_alignment_from_path(path, *Matrices, 3, i));
    }

#ifdef NDEBUG_DP
    Matrices->clear();
#endif

    return Matrices;
}

sample_cube_multi_calculation::sample_cube_multi_calculation(vector<Parameters>& p,const vector< vector<int> >& nodes_,
							   bool do_OS,bool do_OP, int b)
    :
#ifndef NDEBUG_DP
    P0(p[0]),
#endif
    nodes(nodes_),
    Matrices(p.size()),
    OS(p.size()),
    OP(p.size()),
    Pr(p.size()),
    bandwidth(b)
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
    C1 = A3::correction(p[0],nodes[0]);

    for(int i=0;i<p.size();i++) 
    {
	for(int j=0;j<p[i].n_data_partitions();j++) {
	    if (p[i][j].variable_alignment())
		Matrices[i].push_back( cube_sample_alignment_base(p[i][j], p[0][j], nodes[i], nodes[0], bandwidth) );
	    else
		Matrices[i].push_back( boost::shared_ptr<DPcubeSimple>());
	}
    }

    //-------- Calculate corrections to path probabilities ---------//

    for(int i=0; i<p.size(); i++) 
    {
	if (do_OS)
	    for(int j=0;j<p[i].n_data_partitions();j++)  {
		if (p[i][j].variable_alignment())
		    OS[i].push_back( other_subst(p[i][j],nodes[i]));
		else
		    OS[i].push_back( 1 );
	    }
	else
	    OS[i] = vector<log_double_t>(p[i].n_data_partitions(),log_double_t(1));

	if (do_OP)
	    for(int j=0;j<p[i].n_data_partitions();j++) 
		OP[i].push_back( other_prior(p[i][j],nodes[i]) );
	else
	    OP[i] = vector<log_double_t>(p[i].n_data_partitions(),log_double_t(1));
    }

    //---------------- Calculate choice probabilities --------------//
    for(int i=0;i<Pr.size();i++) 
    {
	Pr[i] = p[i].prior_no_alignment();

	// sum of substitution and alignment probability over all paths
	for(int j=0;j<p[i].n_data_partitions();j++)
	    if (p[i][j].variable_alignment()) {
		Pr[i] *= Matrices[i][j]->Pr_sum_all_paths();
		Pr[i] *= pow(OS[i][j], p[i][j].get_beta());
		Pr[i] *= OP[i][j];
	    }
	    else
		Pr[i] *= p[i][j].heated_likelihood();
    }
    assert(Pr[0] > 0.0);
}

void sample_cube_multi_calculation::set_proposal_probabilities(const vector<log_double_t>& r)
{
    rho.resize(Pr.size());
    for(int i=0;i<Pr.size();i++) 
    {
	rho[i] = r[i];
	Pr[i] *= rho[i];
    }

    assert(Pr[0] > 0.0);
}

int sample_cube_multi_calculation::choose(vector<Parameters>& p, bool correct)
{
    assert(p.size() == nodes.size());

    if (Pr[0] <= 0.0) return -1;

    int C = -1;
    try {
	C = choose_MH(0,Pr);
    }
    catch (choose_exception<log_double_t>& c)
    {
	c.prepend(std::string(__PRETTY_FUNCTION__)+"\n");

	c<<show_parameters(p[0]);
	c<<p[0].probability()<<" = "<<p[0].likelihood()<<" + "<<p[0].prior()<<"\n";

	throw c;
    }
  
    // \todo What do we do if partition 0 works, but other partitions fail cuz of constraints?
    assert(C == -1 or Pr[C] > 0.0);

#ifndef NDEBUG_DP
    std::cerr<<"choice = "<<C<<endl;

    // FIXME: check that alignment of unaffected sequences w/in 2 blocks is unchanged!
    
    // Add another entry for the incoming configuration
    p.push_back( P0 );
    nodes.push_back(nodes[0]);
    rho.push_back( rho[0] );
    Matrices.push_back( Matrices[0] );
    OS.push_back( OS[0] );
    OP.push_back( OP[0] );

    vector< vector< vector<int> > > paths(p.size());

    // For the choices that violate a constraint, why don't we just ignore them?
    // Well, this shifts the indices of each choice.  So, we don't do it yet.

    //------------------- Check offsets from path_Q -> P -----------------//
    for(int i=0;i<p.size();i++) 
    {
	// check whether this arrangement violates a constraint in any partition
	bool ok = true;
	for(int j=0;j<p[i].n_data_partitions();j++) 
	    if (p[i][j].variable_alignment() and Matrices[i][j]->Pr_sum_all_paths() <= 0.0) 
		ok = false;

	if (not ok)
	    assert(i != 0 and i != p.size()-1);

	for(int j=0;j<p[i].n_data_partitions();j++) 
	    if (p[i][j].variable_alignment() and ok)
	    {
		paths[i].push_back( get_path_unique(A3::get_bitpath(p[i][j],nodes[i]),*Matrices[i][j]) );
	  
		OS[i][j] = other_subst(p[i][j],nodes[i]);
		OP[i][j] = other_prior(p[i][j],nodes[i]);
	
		log_double_t OP_i = OP[i][j] / A3::correction(p[i][j],nodes[i]);
	
		check_match_P(p[i][j], OS[i][j], OP_i, paths[i][j], *Matrices[i][j]);
	    }
	    else
		paths[i].push_back( vector<int>() );
    }

    //--------- Compute path probabilities and sampling probabilities ---------//
    vector< vector<log_double_t> > PR(p.size(), vector<log_double_t>(4,1));

    for(int i=0;i<p.size();i++)
    {
	// check whether this arrangement violates a constraint in any partition
	bool ok = true;
	for(int j=0;j<p[i].n_data_partitions();j++) 
	    if (p[i][j].variable_alignment() and Matrices[i][j]->Pr_sum_all_paths() <= 0.0) 
		ok = false;

	if (not ok) {
	    PR[i][0] = 0;
	    assert(i != 0 and i != p.size()-1);
	    continue;
	}

	log_double_t choice_ratio = 1;
	if (i<Pr.size())
	    choice_ratio = choose_MH_P(0,i,Pr)/choose_MH_P(i,0,Pr);
	else
	    choice_ratio = 1;

	//    sample_P(p[i], choice_ratio, rho[i], paths[i], Matrices[i]) );
	PR[i][0] = p[i].heated_probability();
	PR[i][2] = rho[i];
	PR[i][3] = choice_ratio;
	for(int j=0;j<p[i].n_data_partitions();j++)
	    if (p[i][j].variable_alignment())
	    {
		vector<int> path_g = Matrices[i][j]->generalize(paths[i][j]);
		PR[i][0] *= A3::correction(p[i][j],nodes[i]);
		PR[i][1] *= Matrices[i][j]->path_P(path_g)* Matrices[i][j]->generalize_P(paths[i][j]);
	    }
    }

    //--------- Check that each choice is sampled w/ the correct Probability ---------//
    check_sampling_probabilities(PR);
#endif

    //---------------- Adjust for length of n4 and n5 changing --------------------//
    // See Appendix A of Redelings & Suchard (2007) for an explanation.

    log_double_t C2 = A3::correction(p[C],nodes[C]);
    // If we reject the proposed move, then don't do anything.
    if (correct and uniform() > double(C1/C2))
	return -1;

    return C;
}

// Consider making into object! That would make it easier to mix
// and match parts of the routine, while saving state.

int sample_cube_multi(vector<Parameters>& p,const vector< vector<int> >& nodes,
		     const vector<log_double_t>& rho, bool do_OS,bool do_OP) 
{
    try {
	sample_cube_multi_calculation tri(p, nodes, do_OS, do_OP);

	// The DP matrix construction didn't work.
	if (tri.Pr[0] <= 0.0) return -1;

	tri.set_proposal_probabilities(rho);

	return tri.choose(p);
    }
    catch (std::bad_alloc&) {
	std::cerr<<"Allocation failed in sample_cube_multi!  Proceeding."<<std::endl;
	return -1;
    }
}

int sample_cube_multi(vector<Parameters>& p,const vector< vector<int> >& nodes,
		     const vector<log_double_t>& rho, bool do_OS,bool do_OP, int bandwidth) 
{
    assert(bandwidth >= 0);
    try {
	vector<Parameters> p2 = p;

	//----------------- Part 1: Forward -----------------//
	sample_cube_multi_calculation tri1(p, nodes, do_OS, do_OP, bandwidth);

	// The DP matrix construction didn't work.
	if (tri1.Pr[0] <= 0.0) return -1;

	tri1.set_proposal_probabilities(rho);

	int C1 = tri1.choose(p,false);
	assert(C1 != -1);

	//----------------- Part 2: Backward -----------------//

	// Set the initial alignment, so that our bandwidth is relative to this one.
	//
	// This just selects another alignment w/in the range of possible alignments, so the previous
	//   cache invalidations should work for this alignment as well.
	//

	// FIXME: direct access to A_?
	//    for(int i=0;i<p2.size();i++)
	//      for(int j=0;j<p2[i].n_data_partitions();j++)
	//	p2[i][j].A_ = p[C1][j].A_;
	std::abort();

	sample_cube_multi_calculation tri2(p2, nodes, do_OS, do_OP, bandwidth);

	// The DP matrix construction didn't work.
	if (tri2.Pr[0] <= 0.0) return -1;

	tri2.set_proposal_probabilities(rho);

	log_double_t ratio = tri1.Pr[C1]*choose_MH_P(0,C1,tri1.Pr)/(tri2.Pr[0]*choose_MH_P(C1,0,tri2.Pr));

	ratio *= tri1.C1 / tri2.C1;

	if (uniform() < double(ratio))
	    return C1;
	else
	    return -1;
    }
    catch (std::bad_alloc&) {
	std::cerr<<"Allocation failed in sample_cube_multi!  Proceeding."<<std::endl;
	return -1;
    }
}


void cube_sample_alignment(Parameters& P,int node1,int node2) 
{
    int bandwidth = P.load_value("bandwidth",-1.0);

    P.set_root(node1);

    //------------(Gibbs) sample from proposal distribution ------------------//
    vector<Parameters> p(1,P);

    vector< vector<int> > nodes(1);
    nodes[0] = A3::get_nodes_branch_random(P.t(),node1,node2);

    vector<log_double_t> rho(1,1);

    int C = -1;
    if (bandwidth >= 0)
	C = sample_cube_multi(p,nodes,rho,false,false, bandwidth);
    else
	C = sample_cube_multi(p,nodes,rho,false,false);

    if (C != -1) {
	P = p[C];
    }
}

/// Resample branch alignment, internal nodes, and branch length

/// Assumptions:
///  We assume that the probability of the other branch alignments is unaffected...

bool cube_sample_alignment_branch(Parameters& P,
				 int node1,int node2,int b,
				 double rho_,double length2)
{
    //----------- Generate the Different Matrices ---------//
    vector<Parameters> p(2,P);
    p[1].setlength(b,length2);

    vector< vector<int> > nodes (2, A3::get_nodes_branch_random(P.t(),node1,node2) );

    vector<log_double_t> rho(2);
    rho[0] = 1;
    rho[1] = rho_;

    int C = sample_cube_multi(p,nodes,rho,false,false);

    if (C != -1) {
	P = p[C];
    }

    return (C > 0);
}

bool cube_sample_alignment_and_parameter(Parameters& P,
					int node1,int node2,int p_index,
					double rho_,double v2)
{
    //----------- Generate the Different Matrices ---------//
    vector<Parameters> p(2,P);
    p[1].set_parameter_value(p_index,v2);

    vector< vector<int> > nodes (2, A3::get_nodes_branch_random(P.t(),node1,node2) );

    vector<log_double_t> rho(2);
    rho[0] = 1;
    rho[1] = rho_;

    int C = sample_cube_multi(p,nodes,rho,false,false);

    if (C != -1) {
	P = p[C];
    }

    return (C > 0);
}


bool cube_sample_alignment_branch_model(Parameters& P,int node1,int node2)
{
    //----------- Generate the Different Matrices ---------//
    vector<Parameters> p(2,P);

    //  int b = P.t().find_branch(node1,node2);
    // need to make this into a parameters if we are sampling it
    //  p[1].branch_HMM_type[b] = 1 - p[1].branch_HMM_type[b];

    vector< vector<int> > nodes (2, A3::get_nodes_branch_random(P.t(), node1,node2) );

    vector<log_double_t> rho(2,1.0);

    int C = sample_cube_multi(p,nodes,rho,false,false);

    if (C != -1) {
	P = p[C];
    }

    return (C > 0);
}
