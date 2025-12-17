/*
  Copyright (C) 2005-2008 Benjamin Redelings

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

/**
 * @file dp-matrix.C
 *
 * @brief This file contains routines for 2-dimensional dynamic programming.
 */

#include <cmath>

#include <climits>
#include "dp-cube.H"
#include "math/pow2.H"
#include "probability/choose.H"
#include "util/mapping.H"
#include "alignment/alignment-constraint.H"
#include "math/logprod.H"

using std::vector;
using std::valarray;
using std::max;
using std::endl;
using std::isfinite;
using std::pair;

void state_matrix4::clear() 
{
    delete[] data; 
    data = NULL;
  
    delete[] scale_; 
    scale_ = NULL;
}

state_matrix4::~state_matrix4() 
{
    clear();
}

inline void DPcube::clear_cell(int i2,int j2,int k2) 
{
    scale(i2,j2,k2) = INT_MIN;
    for(int S=0;S<n_dp_states();S++)
	(*this)(i2,j2,k2,S) = 0;
}

// 1. dp_order( ) must be considered here, because the 3-way HMM has
//     a silent state at 7.  
// 2. Alternatively, we could just ignore S1==S2, since both the
//     3-way and 1-way HMMs have no more than 1 silent state
//     (not counting the start or end states).

inline void DPcube::forward_first_cell(int i2,int j2,int k2) 
{ 
    assert(0 < i2 and i2 < size1());
    assert(0 < j2 and j2 < size2());
    assert(0 < k2 and k2 < size3());

    // determine initial scale for this cell
    scale(i2,j2,k2) = 0;

    double maximum = 0;

    for(int s2=0;s2<n_dp_states();s2++) 
    {
	double temp;

	int S2 = dp_order(s2);
	if (di(S2) or dj(S2) or dk(S2))
	    temp = start_P[S2];
	else {
	    //--- compute arrival probability ----
	    temp = 0;
	    // bound is s2, since this is only for silent states
	    for(int s1=0;s1<s2;s1++)  {
		int S1 = dp_order(s1);

		temp += (*this)(i2,j2,k2,S1) * GQ(S1,S2);
	    }
	}

	// record maximum
	if (temp > maximum) maximum = temp;

	// store the result
	(*this)(i2,j2,k2,S2) = temp;
    }

    //------- if exponent is too high or too low, rescale ------//
    if (maximum > fp_scale::hi_cutoff or (maximum > 0 and maximum < fp_scale::lo_cutoff))
    {
	int logs = -(int)log2(maximum);
	double scale_ = pow2(logs);
	for(int S2=0;S2<n_dp_states();S2++) 
	    (*this)(i2,j2,k2,S2) *= scale_;
	scale(i2,j2,k2) -= logs;
    }
} 

inline void DPcube::forward_cube_first(int x1,int y1,int z1,int x2,int y2,int z2) {
    assert(0 < x1);
    assert(0 < y1);
    assert(0 < z1);
    assert(x1 <= x2 or y1 <= y2 or z1 <= z2);
    assert(x2 < size1());
    assert(y2 < size2());
    assert(z2 < size3());

    // Since we are using M(0,0) instead of S(0,0), we need to run only the silent states at (0,0)
    // We can only use non-silent states at (0,0) to simulate S

    // forward_first_cell(x1,y1,z1) sets the (1,1,1) cell based on start_P.
    // This cell is actually the (0,0,0) cell except that we don't want to
    //   constantly check if substracting things will run off the edge.

    // clear plane (*,*,0) - not safe if we are chaining cubes at their corneres
    for(int x=x1-1;x<=x2;x++)
	for(int y=y1-1;y<=y2;y++)
	    clear_cell(x,y,z1-1);

    // clear plane (*,0,*)
    for(int x=x1-1;x<=x2;x++)
	for(int z=z1-1;z<=z2;z++)
	    clear_cell(x,y1-1,z);

    // clear plane (*,*,0)
    for(int y=y1-1;y<=y2;y++)
	for(int z=z1-1;z<=z2;z++)
	    clear_cell(x1-1,y,z);

    // fill first plane (x1,y,z)
    {
	// (x1,=,=)
	forward_first_cell(x1,y1,z1);

	// (x1,=,>)
	for(int z=z1+1;z<=z2;z++)
	    forward_cell(x1,y1,z);

	// (x1,>,>=)
	for(int y=y1+1;y<=y2;y++)
	    for(int z=z1;z<=z2;z++)
		forward_cell(x1,y,z);
    }

    for(int x=x1+1; x<=x2; x++)
	for(int y=y1; y<=y2; y++)
	    for(int z=z1; z<=z2; z++)
		forward_cell(x,y,z);
}

void DPcube::compute_Pr_sum_all_paths()
{
    const int I = size1()-1;
    const int J = size2()-1;
    const int K = size3()-1;

    double total = 0.0;
    for(int state1=0;state1<n_dp_states();state1++)
	total += (*this)(I,J,K,state1)*GQ(state1,endstate());

    Pr_total *= pow(log_double_t(2.0),scale(I,J,K)) * total;
    assert(not std::isnan(log(Pr_total)) and isfinite(log(Pr_total)));

    // This really is a probability, so it should be <= 1
    assert(Pr_total <= 1.0);
}

void DPcube::forward_cube() 
{
    const int I = size1()-1;
    const int J = size2()-1;
    const int K = size3()-1;

    forward_cube_first(1,1,1,I,J,K);

    compute_Pr_sum_all_paths();
}

log_double_t DPcube::path_P(const vector<int>& path) const 
{
    const int I = size1()-1;
    const int J = size2()-1;
    const int K = size3()-1;

    int i = I;
    int j = J;
    int k = K;

    int l = path.size()-1;
    int S2 = path[l];
    assert(S2 == endstate());

    log_double_t Pr=1.0;

    vector<double> transition(n_dp_states());

    //We should really stop when we reach the Start state.
    // - since the start state is simulated by a non-silent state
    //   NS(0,0) we should go negative
    // - but we would have to check path[-1] to see which state
    //   made sample_path go negative
    // - instead we can check if l==0 - we know that the start state
    //   is at path[-1]
    while (l>0) {

	for(int s1=0;s1<n_dp_states();s1++)
	{
	    int S1 = dp_order(s1);
	    transition[s1] = (*this)(i,j,k,S1)*GQ(S1,S2);
	}

	int S1 = path[l-1];
	auto s1 = find_index(dp_order(),S1);

	double p = choose_P(*s1,transition);
	assert(p > 0.0);

	if (di(S1)) i--;
	if (dj(S1)) j--;
	if (dk(S1)) k--;

	l--;
	S2 = S1;
	Pr *= p;
    }
    assert(l == 0);
    assert(i == 1 and j == 1 and k == 1);

    // include probability of choosing 'Start' vs ---+ !
    for(int S1=0;S1<n_dp_states();S1++)
	transition[S1] = (*this)(1,1,1,S1) * GQ(S1,S2);

    // Get the probability that the previous state was 'Start'
    double p=0.0;
    for(int S1=0;S1<n_dp_states();S1++)  
	if (not silent(S1))
	    p += choose_P(S1,transition);

    Pr *= p;

    assert(Pr > 0.0);
    //std::cerr<<"P(path) = "<<log(Pr)<<std::endl;
    return Pr;
}

std::optional<vector<int>> DPcube::sample_path() const 
{
    auto total = Pr_sum_all_paths();
    if (not (std::isfinite(total.log()) and total > 0.0)) return {};

    vector<int> path;

    const int I = size1()-1;
    const int J = size2()-1;
    const int K = size3()-1;
    int i = I;
    int j = J;
    int k = K;

    int S2 = endstate();

    vector<double> transition(n_dp_states());

    //We should really stop when we reach the Start state.
    // - since the start state is simulated by a non-silent state
    //   NS(0,0) we should go negative
    // - check that we came from (0,0) though
    while (i>=1 and j>=1 and k >=1) 
    {
	path.push_back(S2);

	for(int s1=0;s1<n_dp_states();s1++)
	{
	    int S1 = dp_order(s1);
	    transition[s1] = (*this)(i,j,k,S1)*GQ(S1,S2);
	}

	int s1 = -1;
	try {
	    s1 = choose_scratch(transition);
	}
	catch (choose_exception<double>& c)
	{
	    std::cerr<<"(I,J,K) = ("<<I<<","<<J<<","<<K<<")\n";
	    std::cerr<<"(i,j,k) = ("<<i<<","<<j<<","<<k<<")\n";
	    for(int state1=0;state1<n_dp_states();state1++)
		std::cerr<<"transition["<<state1<<"] = "<<transition[state1]<<std::endl;

	    c.prepend(__PRETTY_FUNCTION__);

	    throw c;
	}
	int S1 = dp_order(s1);

	if (di(S1)) i--;
	if (dj(S1)) j--;
	if (dk(S1)) k--;

	S2 = S1;
    }
    assert(i+di(S2)==1 and j+dj(S2)==1 and k+dk(S2)==1);

    std::reverse(path.begin(),path.end());
#ifndef NDEBUG_DP
    check_sampling_probability(path);
#endif

    return path;
}

DPcube::DPcube(int i1,
	       int i2,
	       int i3,
	       const HMM& M)
    :DPengine(M),
     state_matrix4(i1,i2,i3,n_dp_states())
{
    const int I = size1()-1;
    const int J = size2()-1;
    const int K = size3()-1;

    for(int state1=0;state1<n_dp_states();state1++)
	(*this)(I,J,K,state1) = 0;
}

inline double sum(const valarray<double>& v) {
    return v.sum();
}

inline double DPcubeEmit::emitMMM(int i,int j,int k) const {
    return s123_sub(i,j,k);
}

inline double DPcubeEmit::emitMM_(int i,int j,int) const {
    return s12_sub(i,j);
}

inline double DPcubeEmit::emitM_M(int i,int,int k) const {
    return s13_sub(i,k);
}

inline double DPcubeEmit::emit_MM(int,int j,int k) const {
    return s23_sub(j,k);
}


log_double_t DPcubeEmit::path_Q_subst(const vector<int>& path) const 
{
    log_double_t P_sub=1.0;
    int i=1,j=1,k=1;
    for(int l=0;l<path.size();l++) 
    {
	int state2 = path[l];
	if (di(state2))
	    i++;
	if (dj(state2))
	    j++;
	if (dk(state2))
	    k++;

	double sub;
	if (di(state2) and dj(state2) and dk(state2))
	    sub = emitMMM(i,j,k);

	else if (    di(state2) and     dj(state2) and not dk(state2))
	    sub = emitMM_(i,j,k);
	else if (    di(state2) and not dj(state2) and     dk(state2))
	    sub = emitM_M(i,j,k);
	else if (not di(state2) and     dj(state2) and     dk(state2))
	    sub = emit_MM(i,j,k);

	else if (    di(state2) and not dj(state2) and not dk(state2))
	    sub = emitM__(i,j,k);
	else if (not di(state2) and     dj(state2) and not dk(state2))
	    sub = emit_M_(i,j,k);
	else if (not di(state2) and not dj(state2) and     dk(state2))
	    sub = emit__M(i,j,k);

	else
	    sub = emit___(i,j,k);

	P_sub *= sub;
    }
    assert(i == size1()-1 and j == size2()-1 and k == size3()-1);
    return P_sub * Pr_extra_subst;
}

inline double safe_pow(double x, double p)
{
    if (p == 1.0)
	return x;
    else
	return pow(x,p);
}

void DPcubeEmit::prepare_cell(int i,int j,int k) 
{
    assert(i > 0);
    assert(j > 0);
    assert(k > 0);
  
    const double* __restrict__ m1 = dists1[i];
    const double* __restrict__ m2 = dists2[j];
    const double* __restrict__ m3 = dists3[k];
    const double* __restrict__ wf = weighted_frequencies.begin();

    double total123=0;
    double total12=0;
    double total13=0;
    double total23=0;
    const int MS = dists1.matrix_size();
    // The terms with m2 already have wf[t] multiplied in.
    for(int t=0;t<MS;t++)
    {
	total123 += m1[t] * m2[t] * m3[t];
	total12  += m1[t] * m2[t];
	total13  += m1[t] * m3[t] * wf[t];
	total23  += m2[t] * m3[t];
    }

    s123_sub(i,j,k) = safe_pow(total123,B);
    s12_sub(i,j) = safe_pow(total12,B);
    s23_sub(j,k) = safe_pow(total23,B);
    s13_sub(i,k) = safe_pow(total13,B);
}

DPcubeEmit::DPcubeEmit(const HMM& M,
			   EmissionProbs&& d1,
			   EmissionProbs&& d2,
			   EmissionProbs&& d3,
			   const Matrix& WF)
    :DPcube(d1.n_columns(), d2.n_columns(), d3.n_columns(), M),
     s123_sub(d1.n_columns(), d2.n_columns(), d3.n_columns()),
     s12_sub(d1.n_columns(), d2.n_columns()),
     s13_sub(d1.n_columns(), d3.n_columns()),
     s23_sub(d2.n_columns(), d3.n_columns()),
     weighted_frequencies(WF),
     dists1(std::move(d1)), dists2(std::move(d2)), dists3(std::move(d3))
{
    //----- cache G1,G2 emission probabilities -----//
    int scale = 0;
    log_prod prod;
    for(int i=2;i<dists1.n_columns();i++)
    {
	double sum = dists1.dot(i, weighted_frequencies);
	assert(sum <= 1.000000001);
	if (sum != 0)
	{
	    dists1.mul(i, 1.0/sum); // currently ignoring possibility that sum is subnormal and 1.0/sum = Inf
	    prod *= sum;
	}

	scale += dists1.scale(i);
    }

    for(int i=2;i<dists2.n_columns();i++)
    {
	dists2.mul(i, weighted_frequencies);

	double sum = dists2.sum(i);
	assert(sum <= 1.000000001);
	if (sum != 0)
	{
	    dists2.mul(i, 1.0/sum); // currently ignoring possibility that sum is subnormal and 1.0/sum = Inf
	    prod *= sum;
	}

	scale += dists2.scale(i);
    }

    for(int i=2;i<dists3.n_columns();i++)
    {
	double sum = dists3.dot(i, weighted_frequencies);
	assert(sum <= 1.000000001);
	if (sum != 0)
	{
	    dists3.mul(i, 1.0/sum); // currently ignoring possibility that sum is subnormal and 1.0/sum = Inf
	    prod *= sum;
	}

	scale += dists3.scale(i);
    }
    Pr_extra_subst = prod;
    Pr_extra_subst.log() += log_scale_min*scale;

    Pr_extra_subst.log() *= B;

    Pr_total = Pr_extra_subst;
    // So far we have only multiplied by number <= 1, so thus should be true.
    assert(Pr_extra_subst <= 1.0);
}


void DPcubeSimple::forward_cell(int i2,int j2,int k2) 
{
    assert(0 < i2 and i2 < size1());
    assert(0 < j2 and j2 < size2());
    assert(0 < k2 and k2 < size3());

    prepare_cell(i2,j2,k2);

    // determine initial scale for this cell
    scale(i2,j2,k2) = max(scale(i2-1, j2  , k2  ),
		      max(scale(i2  , j2-1, k2  ),
		      max(scale(i2  , j2  , k2-1),
		      max(scale(i2-1, j2-1, k2  ),
		      max(scale(i2-1, j2  , k2-1),
		      max(scale(i2  , j2-1, k2-1),
			  scale(i2-1, j2-1, k2-1)))))));

    double maximum = 0;

    for(int s2 = 0; s2<n_dp_states(); s2++)
    {
	int S2 = dp_order(s2);

	//--- Get (i1,j1) from (i2,j2) and S2
	int i1 = i2;
	if (di(S2)) i1--;

	int j1 = j2;
	if (dj(S2)) j1--;

	int k1 = k2;
	if (dk(S2)) k1--;

	//--- Compute Arrival Probability ----
	unsigned MAX = n_dp_states();
	if (not di(S2) and not dj(S2) and not dk(S2)) MAX = s2;

	double temp  = 0;
	for(int s1=0;s1<MAX;s1++) {
	    int S1 = dp_order(s1);
	    temp += (*this)(i1,j1,k1,S1) * GQ(S1,S2);
	}

	//--- Include Emission Probability----
	double sub;
	// 3 letters emitted
	if (i1 != i2 and j1 != j2 and k1 != k2)
	    sub = emitMMM(i2,j2,k2);

	// 2 letters emitted
	else if (i1 != i2 and j1 != j2 and k1 == k2)
	    sub = emitMM_(i2,j2,k2);
	else if (i1 != i2 and j1 == j2 and k1 != k2)
	    sub = emitM_M(i2,j2,k2);
	else if (i1 == i2 and j1 != j2 and k1 != k2)
	    sub = emit_MM(i2,j2,k2);

	// 1 letter emitted
	else if (i1 != i2 and j1 == j2 and k1 == k2)
	    sub = emitM__(i2,j2,k2);
	else if (i1 == i2 and j1 != j2 and k1 == k2)
	    sub = emit_M_(i2,j2,k2);
	else if (i1 == i2 and j1 == j2 and k1 != k2)
	    sub = emit__M(i2,j2,k2);

	// silent state - nothing emitted
	else
	{
	    assert(i1 == i2 and j1 == j2 and k1 == k2);
	    sub = emit___(i2,j2,k2);
	}

	temp *= sub;

	// rescale result to scale of this cell
	if (scale(i1,j1,k1) != scale(i2,j2,k2))
	    temp *= pow2(scale(i1,j1,k2)-scale(i2,j2,k2));

	// record maximum
	if (temp > maximum) maximum = temp;

	// store the result
	(*this)(i2,j2,k2,S2) = temp;
    }

    //------- if exponent is too high or too low, rescale ------//
    if (maximum > fp_scale::hi_cutoff or (maximum > 0 and maximum < fp_scale::lo_cutoff))
    {
	int logs = -(int)log2(maximum);
	double scale_ = pow2(logs);
	for(int S2=0;S2<n_dp_states();S2++) 
	    (*this)(i2,j2,k2,S2) *= scale_;
	scale(i2,j2,k2) -= logs;
    }
} 

