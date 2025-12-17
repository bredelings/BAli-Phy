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
 * @file dp-matrix.H
 *
 * @brief This file contains routines for 2-dimensional dynamic programming.
 */

#ifndef DP_CUBE_H
#define DP_CUBE_H

#include <vector>
#include "dp-engine.H"
#include "substitution/cache.H"

class state_matrix4
{
    const int s1;
    const int s2;
    const int s3;
    const int s4;

    double* data;
    int* scale_;

    // Guarantee that these things aren't ever copied
    state_matrix4& operator=(const state_matrix4&) = delete;
    state_matrix4& operator=(state_matrix4&&) = delete;

public:

    void clear();

    int size1() const {return s1;}
    int size2() const {return s2;}
    int size3() const {return s3;}
    int size4() const {return s3;}

    double& operator()(int i,int j,int k,int l) {
	assert(0 <= i and i < s1);
	assert(0 <= j and j < s2);
	assert(0 <= k and k < s3);
	assert(0 <= l and l < s4);
	return data[s4*(s3*(s2*(i) + j) + k) + l];
    }

    double operator()(int i,int j,int k,int l) const {
	assert(0 <= i and i < s1);
	assert(0 <= j and j < s2);
	assert(0 <= k and k < s3);
	return data[s4*(s3*(s2*(i) + j) + k) + l];
    }

    int& scale(int i,int j,int k) {
	assert(0 <= i and i < s1);
	assert(0 <= j and j < s2);
	return scale_[s3*(s2*i + j) +k];
    }


    int scale(int i,int j,int k) const {
	assert(0 <= i and i < s1);
	assert(0 <= j and j < s2);
	return scale_[s3*(s2*i + j) +k];
    }

    state_matrix4(int i1,int i2,int i3, int i4)
	:s1(i1),s2(i2),s3(i3),s4(i4),
	 data(new double[s1*s2*s3*s4]),
	 scale_(new int[s1*s2*s3]) 
	{}

    state_matrix4(const state_matrix4&) = delete;
    state_matrix4(state_matrix4&&) = delete;
    ~state_matrix4();
};




/// 2D Dynamic Programming Matrix
class DPcube : public DPengine, public state_matrix4 
{
protected:
    /// Access size of dim 1
    int size1() const {return state_matrix4::size1();}
    /// Access size of dim 2
    int size2() const {return state_matrix4::size2();}
    /// Access size of dim 3
    int size3() const {return state_matrix4::size3();}

    virtual void compute_Pr_sum_all_paths();

public:
    bitmask_t emit1 = 1;
    bitmask_t emit2 = 2;
    bitmask_t emit3 = 4;

    /// Does state S emit in dimension 1?
    bool di(int S) const {return (state_emit[S]&emit1).any();}
    /// Does state S emit in dimension 2?
    bool dj(int S) const {return (state_emit[S]&emit2).any();}
    /// Does state S emit in dimension 2?
    bool dk(int S) const {return (state_emit[S]&emit3).any();}

    /// Zero out all (relevant) probabilities for a cell
    virtual void clear_cell(int,int,int);

    /// Compute the forward probabilities for a cell
    void forward_first_cell(int,int,int);
    virtual void forward_cell(int,int,int)=0;

    /// Compute the forward probabilities for a square
    void forward_cube_first(int,int,int,int,int,int);
    void forward_cube();

    /// Sample a path from the HMM
    std::optional<std::vector<int>> sample_path() const;

    log_double_t path_P(const std::vector<int>& path) const;

    DPcube(int i1, int i2, int i3, const HMM& M);

    virtual ~DPcube() {}
};


/// 3D Dynamic Programming Cube for chains which emit different things
class DPcubeEmit : public DPcube {
protected:

    log_double_t Pr_extra_subst = 1;

    Matrix3 s123_sub;
    Matrix s12_sub;
    Matrix s13_sub;
    Matrix s23_sub;

    Matrix weighted_frequencies;
    
    inline void prepare_cell(int i,int j,int k);

public:
    typedef Likelihood_Cache_Branch EmissionProbs;

    /// Emission probabilities for first sequence
    EmissionProbs dists1;
    /// Emission probabilities for second sequence
    EmissionProbs dists2;
    /// Emission probabilities for third sequence
    EmissionProbs dists3;

    /// The number of different rates
    int nrates() const {return dists1.n_models();}

    int nstates() const {return dists1.n_states();}

    log_double_t path_Q_subst(const std::vector<int>& path) const;

    /// Emission probabilities for +++
    double emitMMM(int i,int j,int k) const;

    /// Emission probabilities for ++-
    double emitMM_(int i,int j,int k) const;
    /// Emission probabilities for +-+
    double emitM_M(int i,int j,int k) const;
    /// Emission probabilities for -++
    double emit_MM(int i,int j,int k) const;

    /// Emission probabilities for --+
    static constexpr double emit__M(int,int,int) {return 1;}
    /// Emission probabilities for -+-
    static constexpr double emit_M_(int,int,int) {return 1;}
    /// Emission probabilities for +--
    static constexpr double emitM__(int,int,int) {return 1;}
    /// Emission probabilities for --
    static constexpr double emit___(int,int,int) {return 1;}

    /// Construct a DP array from an HMM, emission probabilities, and substitution model
    DPcubeEmit(const HMM& M,
	       EmissionProbs&& dists1,
	       EmissionProbs&& dists2,
	       EmissionProbs&& dists3,
	       const Matrix& frequency);
  
    virtual ~DPcubeEmit() {}
};



/// 3D Dynamic Programming cube with no constraints on states at each cell
class DPcubeSimple: public DPcubeEmit {
public:
    void forward_cell(int,int,int);

    using DPcubeEmit::DPcubeEmit;

    virtual ~DPcubeSimple() {}
};

#endif
