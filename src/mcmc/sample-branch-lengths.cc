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

#include <algorithm>
#include <array>
#include <cmath>
#include "util/log-level.hh"
#include "util/rng.hh"
#include "util/settings.hh"              // for get_setting_or( )
#include "sample.hh"
#include "sample-alignment.hh"
#include "util/permute.hh"
#include "dp/5way.hh"
#include "substitution/likelihood.hh"

using MCMC::MoveStats;
using std::vector;
using std::string;
using std::abs;

log_double_t scale_gaussian(double& x, double sigma)
{
    auto scale = exp_to_log_space( gaussian(0,sigma) );
    x *= (double)scale;
    return scale;
}

log_double_t branch_twiddle(double& T,double sigma) {
    T += gaussian(0,sigma);
    return 1;
}

log_double_t branch_twiddle_positive(double& T,double sigma) {
    auto ratio = branch_twiddle(T,sigma);
    T = abs(T);
    return ratio;
}

MCMC::Result change_branch_length_or_duration_(owned_ptr<context>& P,int b,double sigma,
					       log_double_t (*twiddle)(double&,double))
{
    MCMC::Result result(3);

    // A single-branch move changes the stored length or duration.  Any effective length is
    // recomputed from this value and the fixed branch rate in the proposed context.
    const double length_or_duration = P.as<const Parameters>()->t().branch_length_or_duration(b);
    double new_length_or_duration = length_or_duration;

    auto ratio = twiddle(new_length_or_duration,sigma);
  
    //---------- Construct proposed Tree ----------//
    owned_ptr<context> P2  = P;

    P2.as<Parameters>()->select_root(b);
    P2.as<Parameters>()->t().set_branch_length_or_duration(b,new_length_or_duration);

    //--------- Do the M-H step if OK--------------//
    if (perform_MH(*P, *P2, ratio)) {
	result.totals[0] = 1;
	result.totals[1] = abs(length_or_duration - new_length_or_duration);
	result.totals[2] = abs(log(length_or_duration/new_length_or_duration));
    }

    return result;
}


void change_branch_length_or_duration_flat(owned_ptr<context>& P,
					   MoveStats& Stats,int b,double sigma)
{
    Parameters& PP = *P.as<Parameters>();

    const double x = PP.t().branch_length_or_duration(b);
    const double mu = PP.branch_mean();

    MCMC::Result result =
        change_branch_length_or_duration_(P, b, sigma*PP.branch_mean(), branch_twiddle_positive);

    Stats.inc("branch-length *",result);
    if (x < mu/2.0)
	Stats.inc("branch-length 1",result);
    else if (x < mu)
	Stats.inc("branch-length 2",result);
    else if (x < mu*2)
	Stats.inc("branch-length 3",result);
    else 
	Stats.inc("branch-length 4",result);
}

void change_branch_length_or_duration_log_scale(owned_ptr<context>& P,
						MoveStats& Stats,
						int b,
						double sigma)
{
    const double x = P.as<Parameters>()->t().branch_length_or_duration(b);
    const double mu = P.as<Parameters>()->branch_mean();

    MCMC::Result result = change_branch_length_or_duration_(P, b, sigma, scale_gaussian );

    Stats.inc("branch-length (log) *",result);
    if (x < mu/2.0)
	Stats.inc("branch-length (log) 1",result);
    else if (x < mu)
	Stats.inc("branch-length (log) 2",result);
    else if (x < mu*2)
	Stats.inc("branch-length (log) 3",result);
    else 
	Stats.inc("branch-length (log) 4",result);
}

#include "slice-sampling.hh"

void slice_sample_branch_length_or_duration(owned_ptr<context>& P,MoveStats& Stats,int b)
{
    Parameters& PP = *P.as<Parameters>();
    if (not PP.t().can_set_branch_length_or_duration(b))
        return;

    PP.select_root(b);
  
    const double x = PP.t().branch_length_or_duration(b);
    const double mu = PP.branch_mean();

    MCMC::Result result(3);
  
    // Slice-sample the stored variable.  Its context density needs no rate Jacobian because
    // branch-rate layers remain fixed and the program already defines a density for this variable.
    double sigma = get_setting_or("slice_branch_sigma",1.5);
    // mu is the reciprocal branch count, so the reverse slice starts with the same width.
    const double w = sigma * mu;
    branch_length_or_duration_slice_function logp(PP,b);
    double x2 = slice_sample(x,logp,w,50);

    //---------- Record Statistics - -------------//
    // A declined slice can return its original nonfinite coordinate, so identify
    // an unchanged move before subtraction or division can produce a NaN.
    bool unchanged = not std::isfinite(x) or x2 == x;
    result.totals[0] = unchanged ? 0 : abs(x2 - x);
    result.totals[1] = unchanged ? 0 : abs(log(x2/x));
    result.totals[2] = logp.count;

    Stats.inc("branch-length (slice) *",result);
    if (x < mu/2.0)
	Stats.inc("branch-length (slice) 1",result);
    else if (x < mu)
	Stats.inc("branch-length (slice) 2",result);
    else if (x < mu*2)
	Stats.inc("branch-length (slice) 3",result);
    else 
	Stats.inc("branch-length (slice) 4",result);
}

void slice_sample_node_time(owned_ptr<context>& P,MoveStats& /*Stats*/,int n)
{
    Parameters& PP = *P.as<Parameters>();
    if (not PP.t().can_set_node_time(n))
        return;

    if (log_verbose >= 3) std::cerr<<"\n\n  [slice_sample_node_time]\n";

    PP.set_root(n);

    const double T = PP.t().node_time(n);
    // const double mu = PP.branch_mean();

    //------------- Find new length --------------//
    double sigma = get_setting_or("slice_branch_sigma",1.5);

    double w = sigma;

    node_time_slice_function logp(PP, n);

    /* double T2 = */ slice_sample(T, logp, w, 50);
}

void alignment_slice_sample_branch_length_or_duration(owned_ptr<context>& P,MoveStats& Stats,int b)
{
    Parameters& PP = *P.as<Parameters>();
    if (not PP.t().can_set_branch_length_or_duration(b)) return;

    if (log_verbose >= 3) std::cerr<<"\n\n  [alignment_slice_sample_branch_length_or_duration]\n";

    const double x = PP.t().branch_length_or_duration(b);
    const double mu = PP.branch_mean();
    MCMC::Result result(3);

    // mu is the reciprocal branch count, so the reverse slice starts with the same width.
    const double w = get_setting_or("slice_branch_sigma",1.5) * mu;
    double x2 = x;
    {
        // The initial alignment refresh and all length trials form one update. Even a failure
        // during the reverse-bracket check must retain the alignment from before the refresh.
        Parameters candidate = PP;
        std::optional<alignment_branch_length_or_duration_slice_function> logp;
        try
        {
            if (auto ratio = sample_alignment_with_ratio(candidate, b))
            {
                // The effective length is recomputed at each trial of the stored coordinate.
                logp.emplace(candidate, b, *ratio);
                x2 = slice_sample(x, *logp, w, 50);
            }
            PP = candidate;
        }
        catch (const dp_table_allocation_failure& e)
        {
            if (log_verbose >= 1)
                std::cerr << "Skipping joint alignment and branch length update: " << e.what() << '\n';
        }
        result.totals[2] = logp ? logp->count : 0;
    }
    PP.evaluate_program();

    //---------- Record Statistics - -------------//
    // A declined slice can return its original nonfinite coordinate, so identify
    // an unchanged move before subtraction or division can produce a NaN.
    bool unchanged = not std::isfinite(x) or x2 == x;
    result.totals[0] = unchanged ? 0 : abs(x2 - x);
    result.totals[1] = unchanged ? 0 : abs(log(x2/x));

    Stats.inc("alignment-branch-length (slice) *",result);
    if (x < mu/2.0)
	Stats.inc("alignment-branch-length (slice) 1",result);
    else if (x < mu)
	Stats.inc("alignment-branch-length (slice) 2",result);
    else if (x < mu*2)
	Stats.inc("alignment-branch-length (slice) 3",result);
    else
	Stats.inc("alignment-branch-length (slice) 4",result);
}

void change_branch_length_or_duration(owned_ptr<context>& P,MoveStats& Stats,int b)
{
    if (uniform() < 0.5)
    {
	double sigma = get_setting_or("log_branch_sigma",0.6);
	change_branch_length_or_duration_log_scale(P, Stats, b, sigma);
    }
    else {
	double sigma = get_setting_or("branch_sigma",0.6);
	change_branch_length_or_duration_flat(P, Stats, b, sigma);
    }
}

void change_branch_length_or_duration_multi(owned_ptr<context>& P,MoveStats& Stats,int b)
{
    const int n=3;

    for(int i=1;i<n;i++)
	change_branch_length_or_duration(P,Stats,b);
}

void change_branch_length_and_T(owned_ptr<context>& P,MoveStats& Stats,int b) 
{
    Parameters& PP = *P.as<Parameters>();
    // The topology part of this hybrid proposal requires an A5 neighborhood.
    if (PP.t().degree(PP.t().source(b)) != 3 or PP.t().degree(PP.t().target(b)) != 3) return;

    MCMC::Result result(5,0);

    result.counts[0] = 1;

    // Apply the original hybrid proposal to the stored length or duration.  A
    // negative result remains only a signal to attempt the alternative topology.
    const double length_or_duration = PP.t().branch_length_or_duration(b);
    double new_length_or_duration = length_or_duration;
    auto ratio = branch_twiddle(new_length_or_duration,PP.branch_mean()*0.6);

    //----- positive  =>  propose length change -----//
    if (new_length_or_duration >= 0)
    {
	result.counts[1] = 1;
	result.counts[3] = 1;

	//---------- Construct proposed Tree ----------//
	owned_ptr<context> P2 = P;

	P2.as<Parameters>()->select_root(b);
	P2.as<Parameters>()->t().set_branch_length_or_duration(b,new_length_or_duration);

	//--------- Do the M-H step if OK--------------//
	if (perform_MH(*P, *P2, ratio)) {
	    result.totals[0] = 1;
	    result.totals[1] = 1;
	    result.totals[3] = abs(new_length_or_duration - length_or_duration);
	}
    }

    //----- negative  => propose topology ---------//
    else 
    {
	result.counts[2] = 1;
	result.counts[4] = 1;

	//----- Generate the Different Topologies ------//
	vector<Parameters> p(2,PP);
    
	auto t2 = p[1].t();
    
	vector<int> nodes = A5::get_nodes_random(t2,b).nodes;
	int b1 = t2.find_branch(nodes[4],nodes[1]);
	int b2 = t2.find_branch(nodes[5],nodes[2]);
	p[1].NNI(b1,b2);

	vector<log_double_t> rho(2,1);
	rho[1] = ratio;

	//------ Sample the Different Topologies ------//
	auto choice = two_way_topology_sample(p,rho,b);

	if (choice)
	    PP = p[*choice];

	if (choice and *choice > 0) {
	    result.totals[0] = 1;
	    result.totals[2] = 1;
	    result.totals[4] = abs(length_or_duration - new_length_or_duration);
	}
    }

    Stats.inc("change_branch_length_and_T",result);
}

log_double_t slide_node_no_expand_branch(vector<double>& lengths,double) 
{
    double L = lengths[0] + lengths[1];

    lengths[0] = L * uniform();
    lengths[1] = L - lengths[0];

    return 1;
}


log_double_t slide_node_expand_branch(vector<double>& lengths,double sigma) 
{
    auto ratio = exp_to_log_space( gaussian(0,sigma) );

    double L = (lengths[0] + lengths[1]) * double(ratio);

    lengths[0] = L * uniform();
    lengths[1] = L - lengths[0];

    return ratio*ratio;
}

bool slide_node(owned_ptr<context>& P,
		const vector<int>& branches,
		log_double_t (*slide)(vector<double>&,double)
    ) 
{
    auto t = P.as<Parameters>()->t();
  
    // check that we've got three branches
    assert(branches.size() == 2);

    // Apply the original branch-length proposal to the stored coordinates.  On a
    // rate-scaled tree these are durations; rates remain fixed during the move.
    vector<double> lengths(2);
    lengths[0] = t.branch_length_or_duration(branches[0]);
    lengths[1] = t.branch_length_or_duration(branches[1]);

    double sigma = get_setting_or("slide_node_sigma",0.3);
    auto ratio = slide(lengths,sigma);

    //---------------- Propose new lengths ---------------//
    owned_ptr<context> P2 = P;

    auto t2 = P2.as<Parameters>()->t();
    t2.set_branch_length_or_duration(branches[0], lengths[0]);
    t2.set_branch_length_or_duration(branches[1], lengths[1]);
    
    return perform_MH(*P, *P2, ratio);
}


void slide_node(owned_ptr<context>& P, MoveStats& Stats,int b)
{
    Parameters* PP = P.as<Parameters>();
    auto t = PP->t();
    assert(is_degree3_edge(t,b));

    // choose branches to alter
    if (uniform() < 0.5)
	b = t.reverse(b);
    if (t.degree(t.target(b)) != 3)
	b = t.reverse(b);
    vector<int> branches = t.branches_after(b);

    double L1a = PP->t().branch_length_or_duration(branches[0]);
    double L2a = PP->t().branch_length_or_duration(branches[1]);

    PP->set_root(t.target(b));

    double p = get_setting_or("branch_slice_fraction",0.9);
    if (uniform() < p)
    {
	slide_node_slice_function logp(*PP,b);
	double w = get_setting_or("slide_branch_slice_window",0.3);
	slice_sample(logp,w,50);
	double L1b = PP->t().branch_length_or_duration(branches[0]);
    
	MCMC::Result result(2);
	// A declined slice can leave the same nonfinite coordinate, for which
	// subtracting the before and after values would produce a NaN.
	bool unchanged = not std::isfinite(L1a) or L1b == L1a;
	result.totals[0] = unchanged ? 0 : 2.0*abs(L1b-L1a);
	result.totals[1] = logp.count;
	Stats.inc("slide_node_slice",result);
    }
    else {
        PP->cache_likelihood_branches();
	bool success; string name;
	if (uniform() < 0.5) {
	    success = slide_node(P, branches, slide_node_no_expand_branch);
	    name = "slide_node";
	}
	else {
	    success = slide_node(P, branches, slide_node_expand_branch);
	    name = "slide_node_expand_branch";
	}
	PP = P.as<Parameters>();
	double L1b = PP->t().branch_length_or_duration(branches[0]);
	double L2b = PP->t().branch_length_or_duration(branches[1]);

	MCMC::Result result(2);
	result.totals[0] = success?1:0;
	result.totals[1] = abs(L1b-L1a) + abs(L2b-L2a);
	Stats.inc(name,result);
    }
}

/// Propose one lognormal change to each pairwise sum at a degree-three node.
void change_3_branch_lengths(owned_ptr<context>& P,MoveStats& Stats,int n) 
{
    if (log_verbose >= 3) std::cerr<<"\n\n[change_3_branch_lengths]\n";

    Parameters* PP = P.as<Parameters>();
    MCMC::Result result(2);

    auto t = PP->t();
    if (t.degree(n) != 3) return;

    //-------------- Find branches ------------------//
    vector<int> branches = randomize(t.branches_out(n));

    //------------ Change coordinates ---------------//
    // The proposal predates branch-specific rates.  Apply its pairwise-sum
    // transformation to the actual stored lengths or durations, leaving rates fixed.
    double T1 = t.branch_length_or_duration(branches[0]);
    double T2 = t.branch_length_or_duration(branches[1]);
    double T3 = t.branch_length_or_duration(branches[2]);

    double S12 = T1 + T2;
    double S23 = T2 + T3;
    double S31 = T3 + T1;

    //----------- Propose new distances -------------//
    double sigma = get_setting_or("log_branch_sigma",0.6)/2.0;
    auto R12 = exp_to_log_space(gaussian(0,sigma));
    auto R23 = exp_to_log_space(gaussian(0,sigma));
    auto R31 = exp_to_log_space(gaussian(0,sigma));

    double S12_ = double(S12 * R12);
    double S23_ = double(S23 * R23);
    double S31_ = double(S31 * R31);

    double T1_ = (S12_ + S31_ - S23_)/2.0;
    double T2_ = (S12_ + S23_ - S31_)/2.0;
    double T3_ = (S23_ + S31_ - S12_)/2.0;

    // Each feasible draw has reverse/forward density ratio R12 R23 R31. Retrying an
    // infeasible draw would condition that proposal on a state-dependent success probability.
    auto ratio = R12 * R23 * R31;
    if (not (T1_ > 0 and T2_ > 0 and T3_ > 0) or
        not (std::isfinite(T1_) and std::isfinite(T2_) and std::isfinite(T3_)))
    {
        Stats.inc("3-branch-lengths",result);
        return;
    }

    //----------- Construct proposed Tree -----------//
    owned_ptr<context> P2 = P;
    P2.as<Parameters>()->set_root(n);
    auto t2 = P2.as<Parameters>()->t();
    t2.set_branch_length_or_duration(branches[0], T1_);
    t2.set_branch_length_or_duration(branches[1], T2_);
    t2.set_branch_length_or_duration(branches[2], T3_);
  
    //--------- Do the M-H step if OK--------------//
    if (perform_MH(*P, *P2, ratio)) {
	result.totals[0] = 1;
	result.totals[1] = abs(T1_-T1) + abs(T2_-T2) + abs(T3_-T3);
    }

    Stats.inc("3-branch-lengths",result);
}

// Evaluate (a+b)-u without first rounding the pairwise sum. Near the upper endpoint,
// subtracting u from the larger summand first retains the contribution of the smaller one.
double branch_length_at_fixed_pair_sum(double a, double b, double u)
{
    return (std::max(a,b) - u) + std::min(a,b);
}

namespace
{
    // On this line the stored lengths are (u, s-u, t-u); changing coordinates to
    // (u,s,t) has unit Jacobian, so the base context density is already the slice density.
    class three_branch_slice_function: public context_slice_function
    {
        const vector<int>& branches;
        const std::array<double,3> initial;

    public:
        three_branch_slice_function(Parameters& p, const vector<int>& branches, std::array<double,3> lengths)
            :context_slice_function(p, between(0.0, lengths[0] + std::min(lengths[1], lengths[2]))),
             branches(branches), initial(lengths)
        {}

        double current_value() const override
        {
            return static_cast<const Parameters&>(C).t().branch_length_or_duration(branches[0]);
        }

        // Each evaluation starts from the saved context. Reusing it at the original focal
        // length reproduces all three starting values exactly.
        void set_value(double u) override
        {
            const double a = initial[0];
            if (u == a) return;
            auto tree = static_cast<Parameters&>(C).t();
            tree.set_branch_length_or_duration(branches[0], u);
            for (int i: {1, 2})
                tree.set_branch_length_or_duration(branches[i], branch_length_at_fixed_pair_sum(a, initial[i], u));
        }
    };
}

// Choose a focal branch independently of the stored lengths and keep its two pairwise sums fixed.
void slice_sample_3_branch_lengths(owned_ptr<context>& P, MoveStats& Stats, int n)
{
    auto& p = *P.as<Parameters>();
    auto tree = p.t();
    auto branches = randomize(tree.branches_out(n));
    if (branches.size() != 3) return;

    std::array<double,3> lengths;
    for (int i=0; i<3; i++)
        lengths[i] = tree.branch_length_or_duration(branches[i]);

    MCMC::Result result(2);
    if (std::ranges::all_of(lengths, [](double x) {return x > 0 and std::isfinite(x);}) and
        std::isfinite(lengths[0] + std::max(lengths[1], lengths[2])))
    {
        p.set_root(n);
        three_branch_slice_function logp(p, branches, lengths);
        double u = slice_sample_full_interval(logp);
        result.totals[0] = 3 * abs(u - lengths[0]);
        result.totals[1] = logp.count;
    }
    Stats.inc("3-branch-lengths (slice)", result);
}
