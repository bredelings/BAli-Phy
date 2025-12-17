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
 * @file dp-array.H
 *
 * @brief This file contains routines for 1-dimensional dynamic programming.
 */

#ifndef DP_ARRAY_H
#define DP_ARRAY_H

#include <vector>
#include <optional>
#include "dp-engine.H"

class state_array
{
    int s1 = 0;
    int s2 = 0;

    double* data = nullptr;
    int data_size = 0;

    int* scale_ = nullptr;

public:
    // Guarantee that these things aren't ever copied
    state_array& operator=(const state_array&)  = delete;

    int size1() const {return s1;}
    int size2() const {return s2;}

    double& operator()(int i,int j) {
        assert(0 <= i and i < s1);
        assert(0 <= j and j < s2);
        return data[i*s2+j];
    }

    double operator()(int i,int j) const {
        assert(0 <= i and i < s1);
        assert(0 <= j and j < s2);
        return data[i*s2+j];
    }

    int& scale(int i) {
        assert(0 <= i and i < s1);
        return scale_[i];
    }

    int scale(int i) const {
        assert(0 <= i and i < s1);
        return scale_[i];
    }

    void resize(int i1,int i2)
        {
            if (i1*i2 > data_size)
            {
                delete[] data;
                data = new double[i1*i2];
                data_size = i1*i2;
            }

            if (i1 > s1)
            {
                delete[] scale_;
                scale_ = new int[i1];
            }

            s1 = i1;
            s2 = i2;
        }

    state_array() = default;

    state_array(const state_array&) = delete;

    state_array(int i1,int i2)
        { resize(i1,i2); }

    ~state_array()
     {
         delete[] data;
         delete[] scale_;
     }
};


/// 1D Dynamic Programming Matrix
class DParray : public DPengine, public state_array {
    /// Length of the DP array
    int length;
public:
    log_double_t path_Q_subst(const std::vector<int>&) const {return 1;}

    /// Does state S emit?
    bool di(int S) const {return (state_emit[S]&~hidden_bits).any();}

    /// Return the size of the array
    unsigned size() const {return length;}

    virtual log_double_t path_P(const std::vector<int>& path) const;

    /// compute FP for the whole array
    void forward();

    /// compute FP for a cell
    virtual void forward(int)=0;

    /// Sample a path from the HMM
    virtual std::optional<std::vector<int>> sample_path() const;

    /// Compute the probability of all possible paths through theHMM
    log_double_t Pr_sum_all_paths() const;

    /// Compute the probability of all possible paths through theHMM
    virtual log_double_t Pr_sum_all_paths_to_column(int i) const;

    /// Change length
    virtual void set_length(int l);

    /// Construct an 1D DP matrix from a length, and an HMM
    DParray(int, const HMM&);

    virtual ~DParray() {}
};



/// 1D Dynamic Programming Matrix - with constraints on which states are allowed at each position
class DParrayConstrained: public DParray {
    int order_of_computation() const;
    /// The list of allowed states for each position
    std::vector< std::vector<int> > allowed_states;
public:
    /// Access the states allowed at position j
    const std::vector<int>& states(int j) const {return allowed_states[j];}
    /// Access the states allowed at position j
    std::vector<int>& states(int j) {return allowed_states[j];}

    /// Compute the forward probabilities for the entire array
    void forward();

    /// Compute the forward probabilities for one cell
    void forward(int);

    log_double_t path_P(const std::vector<int>& path) const;

    /// Sample a path from the HMM
    std::optional<std::vector<int>> sample_path() const;

    void prune();

    /// Compute the probability of all possible paths through theHMM
    log_double_t Pr_sum_all_paths() const;

    /// Compute the probability of all possible paths through theHMM
    log_double_t Pr_sum_all_paths_to_column(int i) const;

    /// Change length
    void set_length(int l);

    /// Construct an 1D DP matrix from a length, and an HMM
    DParrayConstrained(int,const HMM&);

    virtual ~DParrayConstrained() {}
};



#endif
