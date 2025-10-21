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

#include "util/log-level.H"
#include "util/rng.H"
#include "util/settings.H"              // for get_setting_or( )
#include "sample.H"
#include "util/permute.H"
#include "dp/5way.H"
#include "substitution/likelihood.H"

using MCMC::MoveStats;
using std::vector;
using std::string;
using std::abs;

log_double_t scale_gaussian(double& x, double sigma)
{
    auto scale = exp_to<log_double_t>( gaussian(0,sigma) );
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

MCMC::Result change_branch_length_(owned_ptr<context>& P,int b,double sigma,
				   log_double_t (*twiddle)(double&,double)) 
{
    MCMC::Result result(3);
  
    //------------ Propose new length -------------//
    const double length = P.as<const Parameters>()->t().branch_length(b);
    double newlength = length;

    auto ratio = twiddle(newlength,sigma);
  
    //---------- Construct proposed Tree ----------//
    owned_ptr<context> P2  = P;

    P2.as<Parameters>()->select_root(b);
    P2.as<Parameters>()->setlength(b,newlength);

    //--------- Do the M-H step if OK--------------//
    if (perform_MH(*P, *P2, ratio)) {
	result.totals[0] = 1;
	result.totals[1] = abs(length - newlength);
	result.totals[2] = abs(log(length/newlength));
    }

    return result;
}


void change_branch_length_flat(owned_ptr<context>& P,
			       MoveStats& Stats,int b,double sigma)
{
    Parameters& PP = *P.as<Parameters>();

    const double L = PP.t().branch_length(b);
    const double mu = PP.branch_mean();

    MCMC::Result result = change_branch_length_(P, b, sigma*PP.branch_mean(), branch_twiddle_positive);

    Stats.inc("branch-length *",result);
    if (L < mu/2.0)
	Stats.inc("branch-length 1",result);
    else if (L < mu)
	Stats.inc("branch-length 2",result);
    else if (L < mu*2)
	Stats.inc("branch-length 3",result);
    else 
	Stats.inc("branch-length 4",result);
}

void change_branch_length_log_scale(owned_ptr<context>& P,
				    MoveStats& Stats,
				    int b,
				    double sigma)
{
    const double L = P.as<Parameters>()->t().branch_length(b);
    const double mu = P.as<Parameters>()->branch_mean();

    MCMC::Result result = change_branch_length_(P, b, sigma, scale_gaussian );

    Stats.inc("branch-length (log) *",result);
    if (L < mu/2.0)
	Stats.inc("branch-length (log) 1",result);
    else if (L < mu)
	Stats.inc("branch-length (log) 2",result);
    else if (L < mu*2)
	Stats.inc("branch-length (log) 3",result);
    else 
	Stats.inc("branch-length (log) 4",result);
}

#include "slice-sampling.H"

// In one case, we had x~1e-99 and y~1e26
// In that case, setting the window size to 1e26 meant that
// we couldn't shrink to a small enough window in 200 steps.
double scale_between(double x, double y)
{
    if (x > 0 and y > 0)
        return exp( (log(x) + log(y))/2 );
    else
        return (x + y)/2.0;
}

void slice_sample_branch_length(owned_ptr<context>& P,MoveStats& Stats,int b)
{
    Parameters& PP = *P.as<Parameters>();
    if (not PP.t().can_set_branch_length(b))
        return;

    PP.select_root(b);
  
    const double L = PP.t().branch_length(b);
    const double mu = PP.branch_mean();

    MCMC::Result result(3);
  
    //------------- Find new length --------------//
    double sigma = get_setting_or("slice_branch_sigma",1.5);
    // NOTE - it is OK to depend on L below -- IF AND ONLY IF the likelihood is unimodal.
    double w = sigma * scale_between(PP.branch_mean(),L);
    branch_length_slice_function logp(PP,b);
    double L2 = slice_sample(L,logp,w,50);

    //---------- Record Statistics - -------------//
    result.totals[0] = abs(L2 - L);
    result.totals[1] = abs(log(L2/L));
    result.totals[2] = logp.count;

    Stats.inc("branch-length (slice) *",result);
    if (L < mu/2.0)
	Stats.inc("branch-length (slice) 1",result);
    else if (L < mu)
	Stats.inc("branch-length (slice) 2",result);
    else if (L < mu*2)
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

void alignment_slice_sample_branch_length(owned_ptr<context>& P,MoveStats& Stats,int b)
{
    Parameters& PP = *P.as<Parameters>();
    if (not PP.t().can_set_branch_length(b)) return;

    if (log_verbose >= 3) std::cerr<<"\n\n  [alignment_slice_sample_branch_length]\n";

    PP.select_root(b);

    const double L = PP.t().branch_length(b);
    const double mu = PP.branch_mean();


    MCMC::Result result(3);

    //------------- Find new length --------------//

    double sigma = get_setting_or("slice_branch_sigma",1.5);
    // NOTE - it is OK to depend on L below -- IF AND ONLY IF the likelihood is unimodal.
    double w = sigma * scale_between(PP.branch_mean(),L);
    alignment_branch_length_slice_function logp(PP,b);
    double L2 = slice_sample(L,logp,w,50);

    //---------- Record Statistics - -------------//
    result.totals[0] = abs(L2 - L);
    result.totals[1] = abs(log(L2/L));
    result.totals[2] = logp.count;

    Stats.inc("alignment-branch-length (slice) *",result);
    if (L < mu/2.0)
	Stats.inc("alignment-branch-length (slice) 1",result);
    else if (L < mu)
	Stats.inc("alignment-branch-length (slice) 2",result);
    else if (L < mu*2)
	Stats.inc("alignment-branch-length (slice) 3",result);
    else
	Stats.inc("alignment-branch-length (slice) 4",result);
}

void change_branch_length(owned_ptr<context>& P,MoveStats& Stats,int b)
{
    if (uniform() < 0.5)
    {
	double sigma = get_setting_or("log_branch_sigma",0.6);
	change_branch_length_log_scale(P, Stats, b, sigma);
    }
    else {
	double sigma = get_setting_or("branch_sigma",0.6);
	change_branch_length_flat(P, Stats, b, sigma);
    }
}

void change_branch_length_multi(owned_ptr<context>& P,MoveStats& Stats,int b) 
{
    const int n=3;

    for(int i=1;i<n;i++)
	change_branch_length(P,Stats,b);
}

void change_branch_length_and_T(owned_ptr<context>& P,MoveStats& Stats,int b) 
{
    Parameters& PP = *P.as<Parameters>();
    MCMC::Result result(5,0);

    result.counts[0] = 1;

    //------------- Propose new length --------------//
    const double length = PP.t().branch_length(b);
    double newlength = length;
    auto ratio = branch_twiddle(newlength,PP.branch_mean()*0.6);

    //----- positive  =>  propose length change -----//
    if (newlength >= 0) 
    {
	result.counts[1] = 1;
	result.counts[3] = 1;

	//---------- Construct proposed Tree ----------//
	owned_ptr<context> P2 = P;

	P2.as<Parameters>()->select_root(b);
	P2.as<Parameters>()->setlength(b,newlength);

	//--------- Do the M-H step if OK--------------//
	if (perform_MH(*P, *P2, ratio)) {
	    result.totals[0] = 1;
	    result.totals[1] = 1;
	    result.totals[3] = abs(newlength - length);
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
	int C = two_way_topology_sample(p,rho,b);

	if (C != -1) {
	    PP = p[C];
	}

	if (C > 0) {
	    result.totals[0] = 1;
	    result.totals[2] = 1;
	    result.totals[4] = abs(length - newlength);
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
    auto ratio = exp_to<log_double_t>( gaussian(0,sigma) );

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

    //---------------- Propose new lengths ---------------//
    vector<double> lengths(2);
    lengths[0] = t.branch_length(branches[0]);
    lengths[1] = t.branch_length(branches[1]);

    double sigma = get_setting_or("slide_node_sigma",0.3);
    auto ratio = slide(lengths,sigma);

    //---------------- Propose new lengths ---------------//
    owned_ptr<context> P2 = P;

    P2.as<Parameters>()->setlength(branches[0], lengths[0]);
    P2.as<Parameters>()->setlength(branches[1], lengths[1]);
    
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

    double L1a = PP->t().branch_length(branches[0]);
    double L2a = PP->t().branch_length(branches[1]);

    PP->set_root(t.target(b));

    double p = get_setting_or("branch_slice_fraction",0.9);
    if (uniform() < p)
    {
	slide_node_slice_function logp(*PP,b);
	double w = scale_between(logp.x0, logp.y0) * get_setting_or("slide_branch_slice_window",0.3);
	double L1b = slice_sample(logp,w,50);
    
	MCMC::Result result(2);
	result.totals[0] = 2.0*abs(L1b-L1a);
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
	double L1b = PP->t().branch_length(branches[0]);
	double L2b = PP->t().branch_length(branches[1]);

	MCMC::Result result(2);
	result.totals[0] = success?1:0;
	result.totals[1] = abs(L1b-L1a) + abs(L2b-L2a);
	Stats.inc(name,result);
    }
}

/// Propose three neighboring branch lengths all anti-correlated
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
    double T1 = t.branch_length(branches[0]);
    double T2 = t.branch_length(branches[1]);
    double T3 = t.branch_length(branches[2]);

    double S12 = T1 + T2;
    double S23 = T2 + T3;
    double S31 = T3 + T1;

    //----------- Propose new distances -------------//
    double sigma = get_setting_or("log_branch_sigma",0.6)/2.0;
    log_double_t ratio = 1.0;

    double T1_ = T1;
    double T2_ = T2;
    double T3_ = T3;

    for(int i=0;i<20;i++) 
    {
	auto R12 = exp_to<log_double_t>(gaussian(0,sigma));
	auto R23 = exp_to<log_double_t>(gaussian(0,sigma));
	auto R31 = exp_to<log_double_t>(gaussian(0,sigma));

	double S12_ = double(S12 * R12);
	double S23_ = double(S23 * R23);
	double S31_ = double(S31 * R31);

	//---------------- Change back ------------------//
	T1_ = (S12_ + S31_ - S23_)/2.0;
	T2_ = (S12_ + S23_ - S31_)/2.0;
	T3_ = (S23_ + S31_ - S12_)/2.0;

	ratio = R12 * R23 * R31;

	if (T1_ > 0.0 and T2_ > 0.0 and T3_ > 0.0) break;
    }
    if (T1_ <= 0.0 or T2_ <= 0.0 or T3_ <= 0.0) return;

    //----------- Construct proposed Tree -----------//
    owned_ptr<context> P2 = P;
    P2.as<Parameters>()->set_root(n);
    P2.as<Parameters>()->setlength(branches[0], T1_);
    P2.as<Parameters>()->setlength(branches[1], T2_);
    P2.as<Parameters>()->setlength(branches[2], T3_);
  
    //--------- Do the M-H step if OK--------------//
    if (perform_MH(*P, *P2, ratio)) {
	result.totals[0] = 1;
	result.totals[1] = abs(T1_-T1) + abs(T2_-T2) + abs(T3_-T3);
    }

    Stats.inc("3-branch-lengths",result);
}
