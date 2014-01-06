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

class state_matrix
{
  const int s1;
  const int s2;
  const int s3;

  double* data;
  int* scale_;

  // Guarantee that these things aren't ever copied
  state_matrix& operator=(const state_matrix&) {return *this;}

public:

  void clear();

  int size1() const {return s1;}
  int size2() const {return s2;}
  int size3() const {return s3;}

  double& operator()(int i,int j,int k) {
    assert(0 <= i and i < s1);
    assert(0 <= j and j < s2);
    assert(0 <= k and k < s3);
    return data[s3*(i*s2+j)+k];
  }

  double operator()(int i,int j,int k) const {
    assert(0 <= i and i < s1);
    assert(0 <= j and j < s2);
    assert(0 <= k and k < s3);
    return data[s3*(i*s2+j)+k];
  }

  int& scale(int i,int j) {
    assert(0 <= i and i < s1);
    assert(0 <= j and j < s2);
    return scale_[i*s2 + j];
  }


  int scale(int i,int j) const {
    assert(0 <= i and i < s1);
    assert(0 <= j and j < s2);
    return scale_[i*s2 + j];
  }

  state_matrix(int i1,int i2,int i3)
    :s1(i1),s2(i2),s3(i3),
     data(new double[s1*s2*s3]),
     scale_(new int[s1*s2]) 
  {}

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

  /// Does state S emit in dimension 1?
  bool di(int S) const {return (state_emit[S]&emit1).any();}
  /// Does state S emit in dimension 2?
  bool dj(int S) const {return (state_emit[S]&emit2).any();}

  /// Zero out all (relevant) probabilities for a cell
  virtual void clear_cell(int,int);

  /// Compute the forward probabilities for a cell
  void forward_first_cell(int,int);
  virtual void forward_cell(int,int)=0;

  /// Compute the forward probabilities for a square
  void forward_square_first(int,int,int,int);
  void forward_square(int,int,int,int);
  void forward_square();

  /// Compute the forward probabilities for a square
  void forward_band(const std::vector< std::pair<int,int> >& boundaries);

  /// compute FP for entire matrix, with some points on path pinned
  void forward_constrained(const std::vector<std::vector<int> >&);

  /// Sample a path from the HMM
  std::vector<int> sample_path() const;

  efloat_t path_P(const std::vector<int>& path) const;

  DPmatrix(int i1, int i2, const HMM& M);

  /// Construct a 2D DP matrix from the dimensions, and an HMM
  DPmatrix(int i1,
	   int i2,
	   const std::vector<bitmask_t>& v1,
	   const std::vector<double>& v2,
	   const Matrix& M,
	   double Beta);
  virtual ~DPmatrix() {}
};


/// 2D Dynamic Programming Matrix for chains which only emit or don't emit
class DPmatrixNoEmit: public DPmatrix {
public:
  /// Compute the forward probabilities for a cell
  void forward_cell(int,int);

  efloat_t path_Q_subst(const std::vector<int>&) const {return 1;}

  DPmatrixNoEmit(int i1,
		 int i2,
		 const std::vector<bitmask_t>& v1,
		 const std::vector<double>& v2,
		 const Matrix& M,
		 double Beta
		 )
    :DPmatrix(i1,i2,v1,v2,M,Beta) 
  {}

  virtual ~DPmatrixNoEmit() {}
};



/// 2D Dynamic Programming Matrix for chains which emit different things
class DPmatrixEmit : public DPmatrix {
protected:

  Matrix s12_sub;
  /// Precomputed emission probabilies for +-
  std::vector<double> s1_sub;
  /// Precomputed emission probabilies for -+
  std::vector<double> s2_sub;

  inline void prepare_cell(int i,int j);

public:
  /// Emission probabilities for first sequence
  std::vector< Matrix > dists1;
  /// Emission probabilities for second sequence
  std::vector< Matrix > dists2;

  /// The number of different rates
  int nrates() const {return dists1[0].size1();}

  efloat_t path_Q_subst(const std::vector<int>& path) const;

  /// Emission probabilities for ++
  double emitMM(int i,int j) const;
  /// Emission probabilities for -+
  double emit_M(int i,int j) const;
  /// Emission probabilities for +-
  double emitM_(int i,int j) const;
  /// Emission probabilities for --
  double emit__(int i,int j) const;

  /// Construct a DP array from an HMM, emission probabilities, and substitution model
  DPmatrixEmit(const std::vector<bitmask_t>& state_emit,
	       const std::vector<double>& start_pi,
	       const Matrix& HMM,
	       double Beta,
	       const std::vector< Matrix >& dists1,
	       const std::vector< Matrix >& dists2, 
	       const Matrix& frequency);
  
  virtual ~DPmatrixEmit() {}
};



/// 2D Dynamic Programming matrix with no constraints on states at each cell
class DPmatrixSimple: public DPmatrixEmit {
public:
  void forward_cell(int,int);

  DPmatrixSimple(const std::vector<bitmask_t> & v1,
		 const std::vector<double> & v2,
		 const Matrix& M,
		 double Beta,
		 const std::vector< Matrix >& d1,
		 const std::vector< Matrix >& d2, 
		 const Matrix& f):
    DPmatrixEmit(v1,v2,M,Beta,d1,d2,f)
  { }

  virtual ~DPmatrixSimple() {}
};



/// Dynamic Programming matrix with constraints on the states
class DPmatrixConstrained: public DPmatrixEmit 
{
  int order_of_computation() const;
  std::vector< std::vector<int> > allowed_states;

  virtual void compute_Pr_sum_all_paths();
public:

  efloat_t path_P(const std::vector<int>& path) const;


  /// Sample a path from the HMM
  std::vector<int> sample_path() const;

  const std::vector<int>& states(int j) const {return allowed_states[j];}
  std::vector<int>& states(int j) {return allowed_states[j];}

  void clear_cell(int,int);
  void forward_cell(int,int);

  void prune();

  DPmatrixConstrained(const std::vector<bitmask_t> & v1,
		      const std::vector<double> & v2,
		      const Matrix& M,
		      double Beta,
		      const std::vector< Matrix >& d1,
		      const std::vector< Matrix >& d2, 
		      const Matrix& f):
    DPmatrixEmit(v1,v2,M,Beta,d1,d2,f), allowed_states(d2.size())
  { }

  virtual ~DPmatrixConstrained() {}
};

#endif
