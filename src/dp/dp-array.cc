/*
  Copyright (C) 2005-2007,2010 Benjamin Redelings

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
 * @file dp-array.C
 *
 * @brief This file contains routines for 1-dimensional dynamic programming.
 */

#include <cmath>
#include "dp-array.H"
#include "math/pow2.H"
#include "probability/choose.H"
#include "util/mapping.H"

using std::vector;
using std::optional;

// We can ignore scale(i) here, because it factors out.
log_double_t DParray::path_P(const vector<int>& g_path) const 
{
    // If all paths have probability 0, then this probability of choosing one of them is undefined.
    assert(Pr_total > 0);

    const int I = size()-1;
    int i=I;
    int l=g_path.size()-1;
    int state2 = g_path[l];

    vector<double> transition(n_dp_states());

    log_double_t Pr=1.0;
    while (l>0) 
    {
        for(int state1=0;state1<n_dp_states();state1++)
            transition[state1] = (*this)(i,state1)*GQ(state1,state2);

        int state1 = g_path[l-1];

        // If forward probability of being in (i,j) in state1 and transitioning to state2 is 0, then the path has probability 0.
        if (transition[state1] == 0) return 0;

        // Otherwise we can compute the probability of choosing state1, and it should not be 0 or NaN.
        double p = choose_P(state1,transition);

        if (di(state1)) i--;

        l--;
        state2 = state1;
        Pr *= p;
    }

    // include probability of choosing 'Start' vs ---+ !
    for(int state1=0;state1<n_dp_states();state1++)
        transition[state1] = (*this)(0,state1) * GQ(state1,state2);

    // Get the probability that the previous state was 'Start'
    double p=0.0;
    for(int state1=0;state1<n_dp_states();state1++)  
        if (not silent(state1))
            p += choose_P(state1,transition);

    Pr *= p;

    assert(Pr > 0.0);
    return Pr;
}

void DParray::forward() {
    for(int i=0;i<size();i++)
        forward(i);
}


std::optional<vector<int>> DParray::sample_path() const
{
    // If the total probability isn't finite and positive, we can't sample a path.
    auto total = Pr_sum_all_paths();
    if (not (std::isfinite(total.log()) and total > 0.0)) return {};

    vector<int> path;
  
    const int I = size()-1;
    int i = I;

    int state2 = endstate();

    vector<double> transition(n_dp_states());

    while(i >= 0) 
    {
        path.push_back(state2);
        for(int state1=0;state1<n_dp_states();state1++)
            transition[state1] = (*this)(i,state1)*GQ(state1,state2);

        int state1 = -1;
        try {
            state1 = choose_scratch(transition);
        }
        catch (choose_exception<double>& c)
        {
            std::cerr<<"I = "<<I<<"\n";
            std::cerr<<"i = "<<i<<"\n";
            for(int state1=0;state1<n_dp_states();state1++)
                std::cerr<<"transition["<<state1<<"] = "<<transition[state1]<<std::endl;

            c.prepend(__PRETTY_FUNCTION__);
            throw c;
        }

        if (di(state1)) i--;

        state2 = state1;
    }
    assert(i+di(state2)==0);

    std::reverse(path.begin(),path.end());

#ifndef NDEBUG_DP
    check_sampling_probability(path);
#endif

    return path;
}

log_double_t DParray::Pr_sum_all_paths() const
{
    const int I = size()-1;
    return Pr_sum_all_paths_to_column(I);
}

/// Compute the probability of all possible paths through theHMM
log_double_t DParray::Pr_sum_all_paths_to_column(int i) const
{
    const int I = size()-1;
    assert(i >= 0 and i <= I);

    double total = 0.0;
    for(int state1=0;state1<n_dp_states();state1++)
        total += (*this)(i,state1) * GQ(state1,endstate());

    return pow(log_double_t(2.0),scale(i)) * total;
}

/// Here we set the SEQUENCE length 'l'.  This gives rise to a
/// DP array of length 'l+1'.
void DParray::set_length(int l)
{
    length = l+1;
    state_array::resize(length, n_dp_states());

    for(int i=0;i<n_dp_states();i++)
        (*this)(0,i) = start_P[i];

#if 0
    // Set the start probabilities.
    for(int i=0;i<n_dp_states();i++)
    {
        int s = dp_order(i);
        if (s < start_P.size())
            (*this)(0,i) = start_P[s];
        else
            (*this)(0,i) = 0;
    }
#endif

    Pr_total = 0;
}

DParray::DParray(int l, const HMM& H)
    :DPengine(H)
{
    set_length(l);
}

// We can ignore scale(i) here, because it factors out.
log_double_t DParrayConstrained::path_P(const vector<int>& g_path) const 
{
    const int I = size()-1;
    int i=I;
    int l=g_path.size()-1;
    int state2 = g_path[l];

    vector<double> transition(n_dp_states());

    log_double_t Pr=1.0;
    while (l>0) 
    {
        transition.resize(states(i).size());
        for(int s1=0;s1<transition.size();s1++) {
            int state1 = states(i)[s1];
            transition[s1] = (*this)(i,state1)*GQ(state1,state2);
        }

        int state1 = g_path[l-1];
        auto s1 = find_index(states(i),state1);
        double p = choose_P(*s1,transition);
        assert(p > 0.0);

        if (di(state1)) i--;

        l--;
        state2 = state1;
        Pr *= p;
        assert(Pr > 0.0);
    }

    // In column 0, all states are allowed:
    // - silent states can't contradict anything
    // - non-silent states reprent the start state.
    transition.resize(states(0).size());

    // include probability of choosing 'Start' vs ---+ !
    for(int state1=0;state1<n_dp_states();state1++)
        transition[state1] = (*this)(0,state1) * GQ(state1,state2);

    // Get the probability that the previous state was 'Start'
    double p=0.0;
    for(int state1=0;state1<n_dp_states();state1++)  
        if (not silent(state1))
            p += choose_P(state1,transition);

    Pr *= p;

    assert(Pr > 0.0);
    return Pr;
}

optional<vector<int>> DParrayConstrained::sample_path() const
{
    // If the total probability isn't finite and positive, we can't sample a path.
    auto total = Pr_sum_all_paths();
    if (not (std::isfinite(total.log()) and total > 0.0)) return {};

    vector<int> path;
  
    const int I = size()-1;
    int i = I;

    int state2 = endstate();

    vector<double> transition(n_dp_states());

    while(i >= 0) {
        path.push_back(state2);
        transition.resize(states(i).size());
        for(int s1=0;s1<transition.size();s1++) {
            int state1 = states(i)[s1];
            transition[s1] = (*this)(i,state1)*GQ(state1,state2);
        }

        int s1 = -1;
        try {
            s1 = choose_scratch(transition);
        }
        catch (choose_exception<double>& c)
        {
            std::cerr<<"I = "<<I<<"\n";
            std::cerr<<"i = "<<i<<"\n";
            for(int state1=0;state1<n_dp_states();state1++)
                std::cerr<<"transition["<<state1<<"] = "<<transition[state1]<<std::endl;

            c.prepend(__PRETTY_FUNCTION__);
            throw c;
        }
        int state1 = states(i)[s1];

        if (di(state1)) i--;

        state2 = state1;
    }
    assert(i+di(state2)==0);

    std::reverse(path.begin(),path.end());

#ifndef NDEBUG_DP
    check_sampling_probability(path);
#endif

    return path;
}

inline void DParrayConstrained::forward(int i2) 
{
    assert(i2<size());

    // determine initial scale for this cell
    if (i2==0)
        scale(i2) = 0;
    else
        scale(i2) = scale(i2-1);

    double maximum = 0;

    for(int s2=0;s2<states(i2).size();s2++) 
    {
        int S2 = states(i2)[s2];

        int i1 = i2;
        if (di(S2)) // we need to change this to (state_emit[S2]&~hidden).any()
            i1--;

        //----- don't go off the boundary -----//
        if (i1<0) continue;

        //---- compute arrival probability ----//
        unsigned MAX = states(i1).size();
        if (not di(S2)) MAX = s2;

        double temp = 0;
        for(int s1=0;s1<MAX;s1++) {
            int S1 = states(i1)[s1];

            temp += (*this)(i1,S1) * GQ(S1,S2);
        }

        // record maximum 
        if (temp > maximum) maximum = temp;

        // store the result
        (*this)(i2,S2) = temp;
    }

    //------- if exponent is too low, rescale ------//
    if (maximum > 0)
    {
        if (maximum < std::numeric_limits<double>::min())
        {
            for(int S2: states(i2))
                (*this)(i2,S2) = 0;
        }
        else if (maximum < fp_scale::lo_cutoff)
        {
            int logs = -(int)log2(maximum);
            for(int S2: states(i2))
            {
                assert(std::isfinite((*this)(i2,S2)));
                (*this)(i2,S2) = std::scalbn( (*this)(i2,S2), logs);
                assert(std::isfinite((*this)(i2,S2)));
            }
            scale(i2) -= logs;
        }
    }
}

void DParrayConstrained::forward() {
    for(int i=0;i<size();i++)
        forward(i);
}


int DParrayConstrained::order_of_computation() const {
    unsigned total=0;
    for(int c=0;c<allowed_states.size()-1;c++)
        total += allowed_states[c].size() * allowed_states[c+1].size();

    return total;
}


void DParrayConstrained::prune() {

    std::abort();

    unsigned order1 = order_of_computation();

    // For column
    for(int c = 1;c<allowed_states.size();c++) {

        // and for each allowed state in that column
        for(int s2=allowed_states[c].size()-1;s2 >= 0;s2--) {

            // check to see whether any states in the previous column are connected to it
            bool used = false;
            for(int s1 = 0;s1<allowed_states[c-1].size() and not used;s1++) {
                if (connected(allowed_states[c-1][s1],allowed_states[c][s2]))
                    used = true;
            }

            // if nothing is connected, then remove it
            if (not used)
                allowed_states[c].erase(allowed_states[c].begin()+s2);
        }
    }

    unsigned order2 = order_of_computation();
    std::cerr<<" order1 = "<<order1<<"    order2 = "<<order2<<"  fraction = "<<double(order2)/double(order1)<<std::endl;
}

log_double_t DParrayConstrained::Pr_sum_all_paths() const 
{
    const int I = size()-1;
    return Pr_sum_all_paths_to_column(I);
}

log_double_t DParrayConstrained::Pr_sum_all_paths_to_column(int i) const
{
    const int I = size()-1;
    assert(i >= 0 and i <= I);

    double total = 0.0;
    for(int state1: states(i))
        total += (*this)(i,state1) * GQ(state1,endstate());

    assert(not std::isnan(total) and std::isfinite(total));
    return pow(log_double_t(2.0),scale(i)) * total;
}

void DParrayConstrained::set_length(int l)
{
    DParray::set_length(l);
    allowed_states.resize(size());
}

DParrayConstrained::DParrayConstrained(int l, const HMM& H)
    :DParray(l,H)
{ 
    allowed_states.resize(size());
}
