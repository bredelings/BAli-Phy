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

#ifndef DP_MATRIX_H
#define DP_MATRIX_H

#include <vector>
#include "dp-engine.H"
#include "substitution/cache.H"

struct Cell
{
    double* start = nullptr;

    int  scale()  const                  { return *(int*)start; }
    int& scale()                         { return *(int*)start; }
    double  emitMM() const               { return start[1];     }
    double& emitMM()                     { return start[1];     }
    double  prob_for_state(int s) const  { return start[s+2]; }
    double& prob_for_state(int s)        { return start[s+2]; }
};

struct MatrixSize
{
    int size1;
    int size2;
};


typedef std::vector<std::pair<int,int>> YBoundaries;

struct MatrixShape: public MatrixSize
{
    YBoundaries yboundaries;
    MatrixShape(const MatrixSize& ms, YBoundaries&& yb);
};

struct Extent
{
    long int offset;
    long int start_index;
    long int size;
};

class state_matrix
{
    MatrixShape shape_;
    std:: vector<Extent> extents;

    const int s1;
    const int s2;
    const int s3;

    double* data;

    // Guarantee that these things aren't ever copied
    state_matrix& operator=(const state_matrix&) = delete;
    state_matrix& operator=(state_matrix&&) = delete;

public:

    void clear();

    const MatrixShape& shape() const {return shape_;}

    int size1() const {return s1;}
    int size2() const {return s2;}
    int size3() const {return s3;}
    int n_states() const {return s3-2;}

    Cell cell(int i,int j)
    {
        assert(data);
        assert(0 <= i and i < s1);
        assert(0 <= j and j < s2);

        assert(0 <= i and i < extents.size());
        i--;
        j--;
        auto [offset, y1, length] = extents[i+1];
        int  delta = (j - y1);
        assert( 0 <= delta and delta < length);
        double* start = data + s3*(offset + delta);

        return {start};
    }

    const Cell cell(int i,int j) const {
        assert(data);
        assert(0 <= i and i < s1);
        assert(0 <= j and j < s2);

        assert(0 <= i and i < extents.size());
        i--;
        j--;
        auto [offset, y1, length] = extents[i+1];
        int  delta = (j - y1);
        assert( 0 <= delta and delta < length);
        double* start = data + s3*(offset + delta);

        return {start};
    }

    state_matrix(MatrixShape&& ms,int i3)
        :shape_(std::move(ms)),
         s1(shape_.size1),
         s2(shape_.size2),
         s3(i3+2)
    {
        auto& yboundaries = shape().yboundaries;

        // We need an extent for column -1 also.
        extents.resize(yboundaries.size()+1);

        long int offset = 0;
        for(int i=0;i<extents.size();i++)
        {
            // Column (i-1)

            // Column -1 starts at -1.
            int y1 = -1;
            if (i>0)
                y1 = yboundaries[i-1].first-1;

            // Column I ends at J.
            int y2 = yboundaries.back().second;
            if (i < yboundaries.size())
                y2 = yboundaries[i].second;

            int length = y2 - y1 + 1;
            assert(length > 0);
            extents[i] = {offset, y1, length};
            offset += length;
        }

        long int mem = offset * s3;
        if (mem > UINT_MAX)
            throw std::bad_alloc();
        data = new double[mem];
    }

    state_matrix(const state_matrix&) = delete;
    state_matrix(state_matrix&&) = delete;
    ~state_matrix();
};




/// 2D Dynamic Programming Matrix
class DPmatrix : public DPengine, public state_matrix 
{
protected:
    /// Access size of dim 1
    int size1() const {return state_matrix::size1();}
    /// Access size of dim 2
    int size2() const {return state_matrix::size2();}

    virtual void compute_Pr_sum_all_paths();

public:
    bitmask_t emit1 = 1;
    bitmask_t emit2 = 2;

    int seqlength1() const {return size1()-2;}
    int seqlength2() const {return size2()-2;}

    /// Does state S emit in dimension 1?
    bool di(int S) const {return (state_emit[S]&emit1).any();}
    /// Does state S emit in dimension 2?
    bool dj(int S) const {return (state_emit[S]&emit2).any();}

    /// Zero out all (relevant) probabilities for a cell
    virtual void clear_cell(Cell);

    /// Compute the forward probabilities for a cell
    void forward_first_cell(Cell);
    virtual void forward_cell(int,int,Cell*)=0;

    /// Compute the forward probabilities between y1(x) and y2(x)
    void forward_band();

    /// Sample a path from the HMM
    std::optional<std::vector<int>> sample_path() const;

    log_double_t path_P(const std::vector<int>& path) const;

    DPmatrix(MatrixShape&& shape, const HMM& M);

    virtual ~DPmatrix() {}
};


/// 2D Dynamic Programming Matrix for chains which emit different things
class DPmatrixEmit : public DPmatrix {
protected:

    log_double_t Pr_extra_subst = 1;

    inline double emitMM(int i,int j) const;

public:
    typedef Likelihood_Cache_Branch EmissionProbs;

    /// Emission probabilities for first sequence
    EmissionProbs dists1;
    /// Emission probabilities for second sequence
    EmissionProbs dists2;

    /// The number of different rates
    int nrates() const {return dists1.n_models();}

    int nstates() const {return dists1.n_states();}

    log_double_t path_Q_subst(const std::vector<int>& path) const;

    /// Construct a DP array from an HMM, emission probabilities, and substitution model
    DPmatrixEmit(MatrixShape&& shape,
                 const HMM& M,
                 EmissionProbs&& dists1,
                 EmissionProbs&& dists2,
                 const Matrix& frequency);
  
    virtual ~DPmatrixEmit() {}
};



/// 2D Dynamic Programming matrix with no constraints on states at each cell
class DPmatrixSimple final: public DPmatrixEmit {
public:
    void forward_cell(int,int,Cell*);

    using DPmatrixEmit::DPmatrixEmit;

    virtual ~DPmatrixSimple() {}
};



/// Dynamic Programming matrix with constraints on the states
class DPmatrixConstrained final: public DPmatrixEmit 
{
    int order_of_computation() const;
    std::vector< std::vector<int> > allowed_states;

    virtual void compute_Pr_sum_all_paths();
public:

    log_double_t path_P(const std::vector<int>& path) const;

    /// Sample a path from the HMM
    std::optional<std::vector<int>> sample_path() const;

    const std::vector<int>& states(int j) const {return allowed_states[j];}
    std::vector<int>& states(int j) {return allowed_states[j];}

    void clear_cell(Cell);
    void forward_cell(int,int,Cell*);

    DPmatrixConstrained(MatrixShape&& shape,
                        const HMM& M,
                        EmissionProbs&& d1,
                        EmissionProbs&& d2, 
                        const Matrix& f);

    virtual ~DPmatrixConstrained() {}
};

YBoundaries yboundaries_everything(int I, int J);
YBoundaries yboundaries_simple_band(int I, int J, int bandwidth);
#endif
