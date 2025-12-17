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
#include "util/log-level.H"
#include "alignment/alignment-constraint.H"
#include "math/logprod.H"

using std::vector;
using std::valarray;
using std::max;
using std::endl;
using std::isfinite;
using std::pair;
using std::optional;

MatrixShape::MatrixShape(const MatrixSize& ms, YBoundaries&& yb)
    :MatrixSize(ms),
     yboundaries( std::move(yb) )
{
}

void state_matrix::clear() 
{
    delete[] data; 
    data = nullptr;
}

state_matrix::~state_matrix() 
{
    clear();
}

inline void DPmatrix::clear_cell(Cell C)
{
    C.scale() = INT_MIN/2;
    for(int S=0;S<n_dp_states();S++)
        C.prob_for_state(S) = 0;
}

// 1. dp_order( ) must be considered here, because the 3-way HMM has
//     a silent state at 7.  
// 2. Alternatively, we could just ignore S1==S2, since both the
//     3-way and 1-way HMMs have no more than 1 silent state
//     (not counting the start or end states).

inline void DPmatrix::forward_first_cell(Cell C)
{ 
    // determine initial scale for this cell
    C.scale() = 0;

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

                temp += C.prob_for_state(S1) * GQ(S1,S2);
            }
        }

        // record maximum
        if (temp > maximum) maximum = temp;

        // store the result
        C.prob_for_state(S2) = temp;
    }

    //------- if exponent is too high or too low, rescale ------//
    if (maximum > fp_scale::hi_cutoff or (maximum > 0 and maximum < fp_scale::lo_cutoff))
    {
        int logs = -(int)log2(maximum);
        double scale_ = pow2(logs);
        for(int S2=0;S2<n_dp_states();S2++) 
            C.prob_for_state(S2) *= scale_;
        C.scale() -= logs;
    }
} 


YBoundaries yboundaries_everything(int I, int J)
{
    return {std::size_t(I+1), {0,J}};
}


// The equation through a point (x1,y1) of slope 1 is y = y1 + (x-x1)
// For the upper boundaries, we take the higher of two lines through (0,W) and (I-W,J).
// (Then we do min(J,y) to avoid getting outside the rectangle.
// For the lower bounadries, we take the lower  of two lines through (W,0) and (I,J-W).
// (Then we do max(0,u) to avoid getting outside the rectangle.

YBoundaries yboundaries_simple_band(int I, int J, int W)
{
    assert(I >= 0);
    assert(J >= 0);
    assert(W > 0);

    auto yboundaries = vector<pair<int,int>>(I+1);

    for(int i=0;i<I+1;i++)
    {
        int ymax = std::min(J, i + W + std::max(0, J - I) );
        int ymin = std::max(0, i - W + std::min(0, J - I) );
        yboundaries[i] = {ymin, ymax};
    }

    return yboundaries;
}

void DPmatrix::forward_band() 
{
    auto& yboundaries = shape().yboundaries;

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
            clear_cell( cell(x1-1,y) );
    }

    // forward first row, with exception for S(0,0): x = 0
    {
        int y1 = 1 + yboundaries[0].first;
        int y2 = 1 + yboundaries[0].second;
        assert(y1 <= y2);
        clear_cell( cell(x1,y1-1) );
        forward_first_cell( cell(x1,y1) );
        for(int y=y1+1;y<=y2;y++)
        {
            auto C = cell(x1,y);
            auto D = cell(x1-1,y);
            auto I = cell(x1,y-1);
            auto M = cell(x1-1,y-1);

            Cell cells[4] = {C,D,I,M};

            forward_cell(x1,y,cells);
        }
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
            clear_cell( cell(x-1,y) );

        // clear the untouched empty cell below us
        clear_cell( cell(x,y1-1) );

        // compute the untouched cells in this row
        for(int y=y1;y<=y2;y++)
        {
            auto C = cell(x,y);
            auto D = cell(x-1,y);
            auto I = cell(x,y-1);
            auto M = cell(x-1,y-1);

            Cell cells[4] = {C,D,I,M};

            forward_cell(x,y,cells);
        }
    }

    compute_Pr_sum_all_paths();
}

void DPmatrix::compute_Pr_sum_all_paths()
{
    const int I = size1()-1;
    const int J = size2()-1;

    auto E = cell(I,J);

    double total = 0.0;
    for(int state1=0;state1<n_dp_states();state1++)
        total += E.prob_for_state(state1)*GQ(state1,endstate());

    Pr_total *= pow(log_double_t(2.0), E.scale()) * total;
    assert(not std::isnan(log(Pr_total)) and isfinite(log(Pr_total)));

    // This really is a probability, so it should be <= 1
    assert(Pr_total <= 1.0);
}

log_double_t DPmatrix::path_P(const vector<int>& path) const 
{
    // If all paths have probability 0, then this probability of choosing one of them is undefined.
    assert(Pr_total > 0);

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
        auto C = cell(i,j);
        for(int state1=0;state1<n_dp_states();state1++)
            transition[state1] = C.prob_for_state(state1)*GQ(state1,state2);

        int state1 = path[l-1];

        // If forward probability of being in (i,j) in state1 and transitioning to state2 is 0, then the path has probability 0.
        if (transition[state1] == 0) return 0;

        // Otherwise we can compute the probability of choosing state1, and it should not be 0 or NaN.
        double p = choose_P(state1,transition);

        if (di(state1)) i--;
        if (dj(state1)) j--;

        l--;
        state2 = state1;
        Pr *= p;
    }
    assert(l == 0);
    assert(i == 1 and j == 1);

    // include probability of choosing 'Start' vs ---+ !
    auto C = cell(1,1);
    for(int state1=0;state1<n_dp_states();state1++)
        transition[state1] = C.prob_for_state(state1) * GQ(state1,state2);

    // Get the probability that the previous state was 'Start'
    double p=0.0;
    for(int state1=0;state1<n_dp_states();state1++)  
        if (not silent(state1))
            p += choose_P(state1,transition);

    Pr *= p;

    //assert(Pr > 0.0);
    //std::cerr<<"P(path) = "<<log(Pr)<<std::endl;
    return Pr;
}

optional<vector<int>> DPmatrix::sample_path() const 
{
    // If the total probability isn't finite and positive, we can't sample a path.
    auto total = Pr_sum_all_paths();
    if (not (std::isfinite(total.log()) and total > 0.0)) return {};

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

        auto C = cell(i,j);
        for(int state1=0;state1<n_dp_states();state1++)
            transition[state1] = C.prob_for_state(state1)*GQ(state1,state2);

        int state1 = -1;
        try {
            state1 = choose_scratch(transition);
        }
        catch (choose_exception<double>& c)
        {
            std::cerr<<"DPMatrix\n";
            std::cerr<<"(I,J) = ("<<I<<","<<J<<")\n";
            std::cerr<<"(i,j) = ("<<i<<","<<j<<")\n";
            for(int state1=0;state1<n_dp_states();state1++)
                std::cerr<<"transition["<<state1<<"] = "<<transition[state1]<<std::endl;
            for(int state1=0;state1<n_dp_states();state1++)
                for(int state2=0;state2<n_dp_states();state2++)
                {
                    auto x = GQ(state1, state2);
                    if (not std::isfinite(x))
                    {
                        auto y = Q(state1,state2);
                        std::cerr<<"GQ("<<state1<<","<<state2<<") = "<<x<<"      Q("<<state1<<","<<state2<<") = "<<y<<"\n";
                    }
                }

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

DPmatrix::DPmatrix(MatrixShape&& ms,
                   const HMM& M)
    :DPengine(M),
     state_matrix(std::move(ms), n_dp_states())
{
    const int I = size1()-1;
    const int J = size2()-1;

    auto E = cell(I,J);
    for(int state1=0;state1<n_dp_states();state1++)
        E.prob_for_state(state1) = 0;
}

inline double sum(const valarray<double>& v) {
    return v.sum();
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
            P_sub *= cell(i,j).emitMM();
    }
    assert(i == size1()-1 and j == size2()-1);
    return P_sub * Pr_extra_subst;
}

double DPmatrixEmit::emitMM(int i, int j) const
{
    assert(i > 0);
    assert(j > 0);
  
    const double* __restrict__ m1 = dists1[i];
    const double* __restrict__ m2 = dists2[j];

    double total=0;
    const int MS = dists1.matrix_size();
    for(int t=0;t<MS;t++)
        total += m1[t] * m2[t];

    if (B != 1.0)
        total = pow(total,B);

    assert(total > 0 or i < 2 or j < 2);
    return total;
}

DPmatrixEmit::DPmatrixEmit(MatrixShape&& ms,
                           const HMM& M,
                           EmissionProbs&& d1,
                           EmissionProbs&& d2,
                           const Matrix& weighted_frequencies)
    :DPmatrix(std::move(ms), M),
     dists1(std::move(d1)), dists2(std::move(d2))
{
    assert(shape().size1 == dists1.n_columns());
    assert(shape().size2 == dists2.n_columns());

    //----- cache G1,G2 emission probabilities -----//
    int scale = 0;
    log_prod prod;
    bool at_root = not dists1.away_from_root_WF and not dists2.away_from_root_WF;
    assert(not (dists1.away_from_root() and dists2.away_from_root()));

    Matrix WF = weighted_frequencies;
    if (dists1.away_from_root_WF)
        WF = dists1.away_from_root_WF.value();
    if (dists2.away_from_root_WF)
        WF = dists2.away_from_root_WF.value();

    for(int i=2;i<dists1.n_columns();i++)
    {
        // `sum` is the emission probability of a Deletion.
        // Dividing dists1 by this ensures that this probability is 1.0.
        double sum;
        // If the root is within dists1, then the frequencies are already included.
        if (dists1.away_from_root_WF)
            sum = dists1.sum(i);
        else
            sum = dists1.dot(i, WF);

        assert(sum <= 1.000000001);
        sum = std::min(sum,1.0);
        if (sum != 0)
        {
            dists1.mul(i, 1.0/sum); // currently ignoring possibility that sum is subnormal and 1.0/sum = Inf
            prod *= sum;
        }

        scale += dists1.scale(i);
    }

    for(int i=2;i<dists2.n_columns();i++)
    {
        // `sum` is the emission probability of a Insertion.
        // Dividing dists1 by this ensures that this probability is 1.0.
        double sum;
        // If the root is within dists2, then the frequencies are already included.
        if (dists2.away_from_root_WF)
            sum = dists2.sum(i);
        else
            sum = dists2.dot(i, WF);

        assert(sum <= 1.000000001);
        sum = std::min(sum,1.0);
        if (sum != 0)
        {
            dists2.mul(i, 1.0/sum); // currently ignoring possibility that sum is subnormal and 1.0/sum = Inf
            prod *= sum;
        }

        scale += dists2.scale(i);

        // If we are at the root then we want the match probabilities to include WF.
        if (at_root)
            dists2.mul(i, WF);
    }
    Pr_extra_subst = prod;
    Pr_extra_subst.log() += log_scale_min*scale;

    Pr_extra_subst.log() *= B;

    Pr_total = Pr_extra_subst;
    // So far we have only multiplied by number <= 1, so thus should be true.
    assert(Pr_extra_subst <= 1.0);
}


void DPmatrixSimple::forward_cell(int i2,int j2,Cell* cells)
{
    assert(0 < i2 and i2 < size1());
    assert(0 < j2 and j2 < size2());

    auto C=cells[0];

    // compute subst probability
    C.emitMM() = emitMM(i2,j2);

    // determine initial scale for this cell
    C.scale() = max(cells[1].scale(), max( cells[3].scale(), cells[2].scale()));

    double maximum = 0;

    // If we have silent states, then we have to process them in
    // the correct order: after all non-silent states and maybe
    // after some silent states.
    assert(not silent(dp_order(n_dp_states()-1)));

    for(int S2=0;S2<n_dp_states();S2++) 
    {
        //--- Get (i1,j1) from (i2,j2) and S2
        int delta = 0;
        int i1 = i2;
        if (di(S2))
        {
            i1--;
            delta+=1;
        }

        int j1 = j2;
        if (dj(S2))
        {
            j1--;
            delta+=2;
        }

        auto P = cells[delta];

        //--- Compute Arrival Probability ----
        double temp  = 0;
        for(int S1=0;S1<n_dp_states();S1++)
            temp += P.prob_for_state(S1) * GQ(S1,S2);

        //--- Include Emission Probability----
        if (i1 != i2 and j1 != j2)
            temp *= C.emitMM();

        // rescale result to scale of this cell
        if (int scale_delta = P.scale() - C.scale(); scale_delta != 0)
            temp *= pow2(scale_delta);

        // record maximum
        if (temp > maximum) maximum = temp;

        // store the result
        C.prob_for_state(S2) = temp;
    }

    //------- if exponent is too high or too low, rescale ------//
    if (maximum > fp_scale::hi_cutoff or (maximum > 0 and maximum < fp_scale::lo_cutoff))
    {
        int logs = -(int)log2(maximum);
        double scale_ = pow2(logs);
        for(int S2=0;S2<n_dp_states();S2++) 
            C.prob_for_state(S2) *= scale_;
        C.scale() -= logs;
    }
} 

//DPmatrixSimple::~DPmatrixSimple() {}


// Make this no longer virtual?
inline void DPmatrixConstrained::clear_cell(Cell C)
{
    C.scale() = INT_MIN/2;
    for(int S=0;S<n_dp_states();S++)
        C.prob_for_state(S) = 0;
}

inline void DPmatrixConstrained::forward_cell(int i2,int j2,Cell* cells)
{
    assert(0 < i2 and i2 < size1());
    assert(0 < j2 and j2 < size2());

    auto C=cells[0];

    // compute subst probability
    C.emitMM() = emitMM(i2,j2);

    // determine initial scale for this cell
    C.scale() = max(cells[1].scale(), max( cells[3].scale(), cells[2].scale()));

    double maximum = 0;

    for(int s2=0;s2<states(j2).size();s2++) 
    {
        int S2 = states(j2)[s2];

        //--- Get (i1,j1) from (i2,j2) and S2
        int delta = 0;
        int i1 = i2;
        if (di(S2))
        {
            i1--;
            delta+=1;
        }

        int j1 = j2;
        if (dj(S2))
        {
            j1--;
            delta+=2;
        }

        auto P = cells[delta];

        //--- Compute Arrival Probability ----
        unsigned MAX = states(j1).size();
        if (not di(S2) and not dj(S2)) MAX = s2;

        double temp = 0.0;
        for(int s1=0;s1<MAX;s1++) {
            int S1 = states(j1)[s1];

            temp += P.prob_for_state(S1) * GQ(S1,S2);
        }

        //--- Include Emission Probability----
        if (i1 != i2 and j1 != j2)
            temp *= C.emitMM();

        // rescale result to scale of this cell
        if (int scale_delta = P.scale() - C.scale(); scale_delta != 0)
            temp *= pow2(scale_delta);

        // record maximum
        if (temp > maximum) maximum = temp;

        // store the result
        C.prob_for_state(S2) = temp;
    }

    //------- if exponent is too high or too low, rescale ------//
    if (maximum > fp_scale::hi_cutoff or (maximum > 0 and maximum < fp_scale::lo_cutoff))
    {
        int logs = -(int)log2(maximum);
        double scale_ = pow2(logs);
        for(int S2: states(j2))
            C.prob_for_state(S2) *= scale_;
        C.scale() -= logs;
    }
}

void DPmatrixConstrained::compute_Pr_sum_all_paths()
{
    const int I = size1()-1;
    const int J = size2()-1;

    auto E = cell(I,J);

    double total = 0.0;
    for(int s1=0;s1<states(J).size();s1++) {
        int S1 = states(J)[s1];
        total += E.prob_for_state(S1)*GQ(S1,endstate());
    }

    Pr_total *= pow(log_double_t(2.0), E.scale()) * total;
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
        auto C = cell(i,j);
        for(int s1=0;s1<states(j).size();s1++)
        {
            int S1 = states(j)[s1];
            transition[s1] = C.prob_for_state(S1)*GQ(S1,S2);
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
    auto C = cell(1,1);
    transition.resize(n_dp_states());
    for(int S1=0;S1<n_dp_states();S1++)
        transition[S1] = C.prob_for_state(S1) * GQ(S1,S2);

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

optional<vector<int>> DPmatrixConstrained::sample_path() const 
{
    // If the total probability isn't finite and positive, we can't sample a path.
    auto total = Pr_sum_all_paths();
    if (not (std::isfinite(total.log()) and total > 0.0)) return {};

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

        auto C = cell(i,j);
        transition.resize(states(j).size());
        for(int s1=0;s1<states(j).size();s1++) 
        {
            int S1 = states(j)[s1];
            transition[s1] = C.prob_for_state(S1)*GQ(S1,S2);
        }

        int s1 = -1;
        try {
            s1 = choose_scratch(transition);
        }
        catch (choose_exception<double>& c)
        {
            std::cerr<<"DPMatrixConstrained\n";
            std::cerr<<"(I,J) = ("<<I<<","<<J<<")\n";
            std::cerr<<"(i,j) = ("<<i<<","<<j<<")\n";
            for(int s1=0;s1<states(j).size();s1++)
            {
                int S1 = states(j)[s1];
                std::cerr<<"transition["<<s1<<"] = "<<transition[s1]<<" = "<<C.prob_for_state(S1)<<" * "<<GQ(S1,S2)<<std::endl;
            }

            for(int state1=0;state1<n_dp_states();state1++)
                for(int state2=0;state2<n_dp_states();state2++)
                {
                    auto x = GQ(state1, state2);
                    if (not std::isfinite(x))
                    {
                        auto y = Q(state1,state2);
                        std::cerr<<"GQ("<<state1<<","<<state2<<") = "<<x<<"      Q("<<state1<<","<<state2<<") = "<<y<<"\n";
                    }
                }

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


DPmatrixConstrained::DPmatrixConstrained(MatrixShape&& ms,
                                         const HMM& M,
                                         EmissionProbs&& d1,
                                         EmissionProbs&& d2,
                                         const Matrix& f):
    DPmatrixEmit(std::move(ms),M,std::move(d1),std::move(d2),f),
    allowed_states(dists2.n_columns())
{ }

