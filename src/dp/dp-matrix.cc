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
#include "dp-matrix.H"
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

void state_matrix::clear() 
{
    delete[] data; 
    data = NULL;
  
    delete[] scale_; 
    scale_ = NULL;
}

state_matrix::~state_matrix() 
{
    clear();
}

inline void DPmatrix::clear_cell(int i2,int j2) 
{
    scale(i2,j2) = INT_MIN;
    for(int S=0;S<n_dp_states();S++)
	(*this)(i2,j2,S) = 0;
}

// 1. dp_order( ) must be considered here, because the 3-way HMM has
//     a silent state at 7.  
// 2. Alternatively, we could just ignore S1==S2, since both the
//     3-way and 1-way HMMs have no more than 1 silent state
//     (not counting the start or end states).

inline void DPmatrix::forward_first_cell(int i2,int j2) 
{ 
    assert(0 < i2 and i2 < size1());
    assert(0 < j2 and j2 < size2());

    // determine initial scale for this cell
    scale(i2,j2) = 0;

    double maximum = 0;

    for(int s2=0;s2<n_dp_states();s2++) 
    {
	double temp;

	int S2 = dp_order(s2);
	if (di(S2) or dj(S2))
	    temp = start_P[S2];
	else {
	    //--- compute arrival probability ----
	    temp = 0;
	    // bound is s2, since this is only for silent states
	    for(int s1=0;s1<s2;s1++)  {
		int S1 = dp_order(s1);

		temp += (*this)(i2,j2,S1) * GQ(S1,S2);
	    }
	}

	// record maximum
	if (temp > maximum) maximum = temp;

	// store the result
	(*this)(i2,j2,S2) = temp;
    }

    //------- if exponent is too high or too low, rescale ------//
    if (maximum > fp_scale::hi_cutoff or (maximum > 0 and maximum < fp_scale::lo_cutoff))
    {
	int logs = -(int)log2(maximum);
	double scale_ = pow2(logs);
	for(int S2=0;S2<n_dp_states();S2++) 
	    (*this)(i2,j2,S2) *= scale_;
	scale(i2,j2) -= logs;
    }
} 

void DPmatrix::forward_band(const vector< pair<int,int> >& yboundaries) 
{
    // note: (x,y) is located at (x+1,y+1) in the matrix.

    const int I = size1()-1;
    const int J = size2()-1;

    const int x1 = 1;
    const int x2 = I;

    assert(yboundaries.size());

    assert(yboundaries.size() - 1 == I - 1);

    assert(yboundaries[0].first == 0);
    assert(yboundaries.back().second == J - 1);

    // Since we are using M(0,0) instead of S(0,0), we need to run only the silent states at (0,0)
    // We can only use non-silent states at (0,0) to simulate S

    // clear left border: x = -1 (what is adjacent to the first row?)
    {
	int y1 = 1 + yboundaries[0].first;
	int y2 = 1 + yboundaries[0].second;
	assert(y1 <= y2);
	for(int y=y1;y<=y2;y++)
	    clear_cell(x1-1,y);
    }

    // forward first row, with exception for S(0,0): x = 0
    {
	int y1 = 1 + yboundaries[0].first;
	int y2 = 1 + yboundaries[0].second;
	assert(y1 <= y2);
	clear_cell(x1,y1-1);
	forward_first_cell(x1,y1);
	for(int y=y1+1;y<=y2;y++)
	    forward_cell(x1,y);
    }

    // forward other rows: x = 1...I-1
    for(int x=x1+1;x<=x2;x++) 
    {
	int y1 = 1 + yboundaries[x-1].first;
	int y2 = 1 + yboundaries[x-1].second;
	assert(y1 <= y2);
	assert(yboundaries[x-1].first >= yboundaries[x-2].first);
	assert(yboundaries[x-1].second >= yboundaries[x-2].second);

	// clear the untouched empty cells to our left
	int z2 = 1 + yboundaries[x-2].second;
	assert(z2 >= y1-1);
	for(int y=z2+1;y<=y2;y++)
	    clear_cell(x-1,y);

	// clear the untouched empty cell below us
	clear_cell(x,y1-1);

	// compute the untouched cells in this row
	for(int y=y1;y<=y2;y++)
	    forward_cell(x,y);
    }

    compute_Pr_sum_all_paths();
}

void DPmatrix::compute_Pr_sum_all_paths()
{
    const int I = size1()-1;
    const int J = size2()-1;

    double total = 0.0;
    for(int state1=0;state1<n_dp_states();state1++)
	total += (*this)(I,J,state1)*GQ(state1,endstate());

    Pr_total *= pow(log_double_t(2.0),scale(I,J)) * total;
    assert(not std::isnan(log(Pr_total)) and isfinite(log(Pr_total)));

    // This really is a probability, so it should be <= 1
    assert(Pr_total <= 1.0);
}

log_double_t DPmatrix::path_P(const vector<int>& path) const 
{
    const int I = size1()-1;
    const int J = size2()-1;
    int i = I;
    int j = J;
    log_double_t Pr=1.0;

    int l = path.size()-1;
    int state2 = path[l];

    vector<double> transition(n_dp_states());

    //We should really stop when we reach the Start state.
    // - since the start state is simulated by a non-silent state
    //   NS(0,0) we should go negative
    // - but we would have to check path[-1] to see which state
    //   made sample_path go negative
    // - instead we can check if l==0 - we know that the start state
    //   is at path[-1]
    while (l>0) {

	for(int state1=0;state1<n_dp_states();state1++)
	    transition[state1] = (*this)(i,j,state1)*GQ(state1,state2);

	int state1 = path[l-1];
	double p = choose_P(state1,transition);
	assert(p > 0.0);

	if (di(state1)) i--;
	if (dj(state1)) j--;

	l--;
	state2 = state1;
	Pr *= p;
    }
    assert(l == 0);
    assert(i == 1 and j == 1);

    // include probability of choosing 'Start' vs ---+ !
    for(int state1=0;state1<n_dp_states();state1++)
	transition[state1] = (*this)(1,1,state1) * GQ(state1,state2);

    // Get the probability that the previous state was 'Start'
    double p=0.0;
    for(int state1=0;state1<n_dp_states();state1++)  
	if (not silent(state1))
	    p += choose_P(state1,transition);

    Pr *= p;

    assert(Pr > 0.0);
    //std::cerr<<"P(path) = "<<log(Pr)<<std::endl;
    return Pr;
}

vector<int> DPmatrix::sample_path() const 
{
    if (Pr_sum_all_paths() <= 0.0)
	throw myexception()<<"DPmatrix::sample_path( ): trying to sample when total probability is 0.";

    vector<int> path;

    const int I = size1()-1;
    const int J = size2()-1;
    int i = I;
    int j = J;

    int state2 = endstate();

    vector<double> transition(n_dp_states());

    //We should really stop when we reach the Start state.
    // - since the start state is simulated by a non-silent state
    //   NS(0,0) we should go negative
    // - check that we came from (0,0) though
    while (i>=1 and j>=1) 
    {
	path.push_back(state2);

	for(int state1=0;state1<n_dp_states();state1++)
	    transition[state1] = (*this)(i,j,state1)*GQ(state1,state2);

	int state1 = -1;
	try {
	    state1 = choose_scratch(transition);
	}
	catch (choose_exception<double>& c)
	{
	    std::cerr<<"(I,J) = ("<<I<<","<<J<<")\n";
	    std::cerr<<"(i,i) = ("<<i<<","<<i<<")\n";
	    for(int state1=0;state1<n_dp_states();state1++)
		std::cerr<<"transition["<<state1<<"] = "<<transition[state1]<<std::endl;

	    c.prepend(__PRETTY_FUNCTION__);

	    throw c;
	}

	if (di(state1)) i--;
	if (dj(state1)) j--;

	state2 = state1;
    }
    assert(i+di(state2)==1 and j+dj(state2)==1);

    std::reverse(path.begin(),path.end());
#ifndef NDEBUG_DP
    check_sampling_probability(path);
#endif

    return path;
}

DPmatrix::DPmatrix(int i1,
		   int i2,
		   const HMM& M)
    :DPengine(M),
     state_matrix(i1,i2,n_dp_states())
{
    const int I = size1()-1;
    const int J = size2()-1;

    for(int state1=0;state1<n_dp_states();state1++)
	(*this)(I,J,state1) = 0;
}

inline double sum(const valarray<double>& v) {
    return v.sum();
}

// switching dists1[] to matrices actually made things WORSE!
inline double DPmatrixEmit::emitMM(int i,int j) const {
    return s12_sub(i,j);
}

log_double_t DPmatrixEmit::path_Q_subst(const vector<int>& path) const 
{
    log_double_t P_sub=1.0;
    int i=1,j=1;
    for(int l=0;l<path.size();l++) 
    {
	int state2 = path[l];
	if (di(state2))
	    i++;
	if (dj(state2))
	    j++;

	if (di(state2) and dj(state2))
	    P_sub *= emitMM(i,j);
    }
    assert(i == size1()-1 and j == size2()-1);
    return P_sub * Pr_extra_subst;
}

void DPmatrixEmit::prepare_cell(int i,int j) 
{
    assert(i > 0);
    assert(j > 0);
  
    double* __restrict__ m1 = dists1[i];
    double* __restrict__ m2 = dists2[j];

    double total=0;
    const int MS = dists1.matrix_size();
    for(int t=0;t<MS;t++)
	total += m1[t] * m2[t];

    if (B != 1.0)
	total = pow(total,B);

    s12_sub(i,j) = total;
}

DPmatrixEmit::DPmatrixEmit(const HMM& M,
			   EmissionProbs&& d1,
			   EmissionProbs&& d2,
			   const Matrix& weighted_frequencies)
    :DPmatrix(d1.n_columns(), d2.n_columns(), M),
     s12_sub(d1.n_columns(), d2.n_columns()),
     dists1(std::move(d1)), dists2(std::move(d2))
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
    Pr_extra_subst = prod;
    Pr_extra_subst.log() += log_scale_min*scale;

    Pr_extra_subst.log() *= B;

    Pr_total = Pr_extra_subst;
    // So far we have only multiplied by number <= 1, so thus should be true.
    assert(Pr_extra_subst <= 1.0);
}


void DPmatrixSimple::forward_cell(int i2,int j2) 
{
    assert(0 < i2 and i2 < size1());
    assert(0 < j2 and j2 < size2());

    prepare_cell(i2,j2);

    // determine initial scale for this cell
    scale(i2,j2) = max(scale(i2-1,j2), max( scale(i2-1,j2-1), scale(i2,j2-1) ) );

    double maximum = 0;

    // If we have silent states, then we have to process them in
    // the correct order: after all non-silent states and maybe
    // after some silent states.
    assert(not silent(dp_order(n_dp_states()-1)));

    for(int S2=0;S2<n_dp_states();S2++) 
    {
	//--- Get (i1,j1) from (i2,j2) and S2
	int i1 = i2;
	if (di(S2)) i1--;

	int j1 = j2;
	if (dj(S2)) j1--;

	//--- Compute Arrival Probability ----
	double temp  = 0;
	for(int S1=0;S1<n_dp_states();S1++)
	    temp += (*this)(i1,j1,S1) * GQ(S1,S2);

	//--- Include Emission Probability----
	if (i1 != i2 and j1 != j2)
	    temp *= emitMM(i2,j2);

	// rescale result to scale of this cell
	if (scale(i1,j1) != scale(i2,j2))
	    temp *= pow2(scale(i1,j1)-scale(i2,j2));

	// record maximum
	if (temp > maximum) maximum = temp;

	// store the result
	(*this)(i2,j2,S2) = temp;
    }

    //------- if exponent is too high or too low, rescale ------//
    if (maximum > fp_scale::hi_cutoff or (maximum > 0 and maximum < fp_scale::lo_cutoff))
    {
	int logs = -(int)log2(maximum);
	double scale_ = pow2(logs);
	for(int S2=0;S2<n_dp_states();S2++) 
	    (*this)(i2,j2,S2) *= scale_;
	scale(i2,j2) -= logs;
    }
} 

//DPmatrixSimple::~DPmatrixSimple() {}


// Make this no longer virtual?
inline void DPmatrixConstrained::clear_cell(int i2,int j2) 
{
    scale(i2,j2) = INT_MIN;
    for(int S=0;S<n_dp_states();S++)
	(*this)(i2,j2,S) = 0;
}

inline void DPmatrixConstrained::forward_cell(int i2,int j2) 
{
    assert(0 < i2 and i2 < size1());
    assert(0 < j2 and j2 < size2());

    prepare_cell(i2,j2);

    // determine initial scale for this cell
    scale(i2,j2) = max(scale(i2-1,j2), max( scale(i2-1,j2-1), scale(i2,j2-1) ) );

    double maximum = 0;

    for(int s2=0;s2<states(j2).size();s2++) 
    {
	int S2 = states(j2)[s2];

	//--- Get (i1,j1) from (i2,j2) and S2
	int i1 = i2;
	if (di(S2)) i1--;

	int j1 = j2;
	if (dj(S2)) j1--;

	//--- Compute Arrival Probability ----
	unsigned MAX = states(j1).size();
	if (not di(S2) and not dj(S2)) MAX = s2;

	double temp = 0.0;
	for(int s1=0;s1<MAX;s1++) {
	    int S1 = states(j1)[s1];

	    temp +=  (*this)(i1,j1,S1) * GQ(S1,S2);
	}

	//--- Include Emission Probability----
	if (i1 != i2 and j1 != j2)
	    temp *= emitMM(i2,j2);

	// rescale result to scale of this cell
	if (scale(i1,j1) != scale(i2,j2))
	    temp *= pow2(scale(i1,j1)-scale(i2,j2));

	// record maximum
	if (temp > maximum) maximum = temp;

	// store the result
	(*this)(i2,j2,S2) = temp;
    }

    //------- if exponent is too high or too low, rescale ------//
    if (maximum > fp_scale::hi_cutoff or (maximum > 0 and maximum < fp_scale::lo_cutoff))
    {
	int logs = -(int)log2(maximum);
	double scale_ = pow2(logs);
	for(int i=0;i<states(j2).size();i++) {
	    int S2 = states(j2)[i];
	    (*this)(i2,j2,S2) *= scale_;
	}
	scale(i2,j2) -= logs;
    }
}

void DPmatrixConstrained::compute_Pr_sum_all_paths()
{
    const int I = size1()-1;
    const int J = size2()-1;

    double total = 0.0;
    for(int s1=0;s1<states(J).size();s1++) {
	int S1 = states(J)[s1];
	total += (*this)(I,J,S1)*GQ(S1,endstate());
    }

    Pr_total *= pow(log_double_t(2.0),scale(I,J)) * total;
    assert(not std::isnan(log(Pr_total)) and isfinite(log(Pr_total)));

    // This really is a probability, so it should be <= 1
    assert(Pr_total <= 1.0);
}

log_double_t DPmatrixConstrained::path_P(const vector<int>& path) const 
{
    const int I = size1()-1;
    const int J = size2()-1;

    // we are at S1(i,j) coming from S2, and seek to determine S1
    int i = I;
    int j = J;
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
    while (l>0) 
    {
	transition.resize(states(j).size());
	for(int s1=0;s1<states(j).size();s1++)
	{
	    int S1 = states(j)[s1];
	    transition[s1] = (*this)(i,j,S1)*GQ(S1,S2);
	}

	int S1 = path[l-1];
	auto s1 = find_index(states(j),S1);

	double p = choose_P(*s1,transition);
	assert(p > 0.0);

	if (di(S1)) i--;
	if (dj(S1)) j--;

	l--;
	S2 = S1;
	Pr *= p;
    }
    assert(l == 0);
    assert(i == 1 and j == 1);

    // include probability of choosing 'Start' vs ---+ !
    transition.resize(n_dp_states());
    for(int S1=0;S1<n_dp_states();S1++)
	transition[S1] = (*this)(1,1,S1) * GQ(S1,S2);

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

vector<int> DPmatrixConstrained::sample_path() const 
{
    if (Pr_sum_all_paths() <= 0.0)
	throw myexception()<<"DPmatrixConstrained::sample_path( ): trying to sample when total probability is 0.";

    vector<int> path;

    const int I = size1()-1;
    const int J = size2()-1;

    // we are at S1(i,j) coming from S2, and seek to determine S1
    int S2 = endstate();
    int i = I;
    int j = J;

    vector<double> transition(n_dp_states());

    //We should really stop when we reach the Start state.
    // - since the start state is simulated by a non-silent state
    //   NS(0,0) we should go negative
    // - check that we came from (0,0) though
    while (i>=1 and j>=1) 
    {
	path.push_back(S2);

	transition.resize(states(j).size());
	for(int s1=0;s1<states(j).size();s1++) 
	{
	    int S1 = states(j)[s1];
	    transition[s1] = (*this)(i,j,S1)*GQ(S1,S2);
	}

	int s1 = -1;
	try {
	    s1 = choose_scratch(transition);
	}
	catch (choose_exception<double>& c)
	{
	    std::cerr<<"(I,J) = ("<<I<<","<<J<<")\n";
	    std::cerr<<"(i,i) = ("<<i<<","<<i<<")\n";
	    for(int state1=0;state1<n_dp_states();state1++)
		std::cerr<<"transition["<<state1<<"] = "<<transition[state1]<<std::endl;

	    c.prepend(__PRETTY_FUNCTION__);
	    throw c;
	}
	int S1 = states(j)[s1];

	if (di(S1)) i--;
	if (dj(S1)) j--;

	S2 = S1;
    }
    assert(i+di(S2)==1 and j+dj(S2)==1);

    std::reverse(path.begin(),path.end());

#ifndef NDEBUG_DP
    check_sampling_probability(path);
#endif

    return path;
}

int DPmatrixConstrained::order_of_computation() const {
    unsigned total=0;
    for(int c=0;c<allowed_states.size()-1;c++)
	total += allowed_states[c].size() * allowed_states[c+1].size();

    return total;
}


DPmatrixConstrained::DPmatrixConstrained(const HMM& M,
					 EmissionProbs&& d1,
					 EmissionProbs&& d2,
					 const Matrix& f):
    DPmatrixEmit(M,std::move(d1),std::move(d2),f),
    allowed_states(dists2.n_columns())
{ }

