/*
   Copyright (C) 2005-2007 Benjamin Redelings

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

#ifndef DP_ARRAY_H
#define DP_ARRAY_H

#include <vector>
#include "dp-engine.H"

class state_array
{
  int s1;
  int s2;

  vector<double> data;
  vector<int> scale_;

  // Guarantee that these things aren't ever copied
  state_array& operator=(const state_array&) {return *this;}

public:
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
    s1 = i1; s2 = i2;
    data.resize(s1*s2);
    scale_.resize(s1);
  }

  state_array()
    :s1(0),
     s2(0)
  { }

  state_array(int i1,int i2)
    :s1(0),
     s2(0)
  { resize(i1,i2); }
};


/// 1D Dynamic Programming Matrix
class DParray : public DPengine, public state_array {
  /// Length of the DP array
  int length;
public:
  efloat_t path_Q_subst(const vector<int>&) const {return 1;}

  /// Does state S emit?
  bool di(int S) const {bool e = false; if (state_emit[S]&(1<<0)) e=true;return e;}

  /// Return the size of the array
  unsigned size() const {return length;}

  virtual efloat_t path_P(const vector<int>& path) const;

  /// compute FP for the whole array
  void forward();

  /// compute FP for a cell
  virtual void forward(int)=0;

  /// Sample a path from the HMM
  virtual vector<int> sample_path() const;

  /// Compute the probability of all possible paths through theHMM
  efloat_t Pr_sum_all_paths() const;

  /// Change length
  virtual void set_length(int l);

  /// Construct an 1D DP matrix from a length, and an HMM
  DParray(int,const vector<int>&,const vector<double>&,const Matrix&,double Beta);

  virtual ~DParray() {}
};



/// 1D Dynamic Programming Matrix - with constraints on which states are allowed at each position
class DParrayConstrained: public DParray {
  int order_of_computation() const;
  /// The list of allowed states for each position
  vector< vector<int> > allowed_states;
public:
  /// Access the states allowed at position j
  const vector<int>& states(int j) const {return allowed_states[j];}
  /// Access the states allowed at position j
  vector<int>& states(int j) {return allowed_states[j];}

  /// Compute the forward probabilities for the entire array
  void forward();

  /// Compute the forward probabilities for one cell
  void forward(int);

  efloat_t path_P(const vector<int>& path) const;

  /// Sample a path from the HMM
  vector<int> sample_path() const;

  void prune();

  /// Compute the probability of all possible paths through theHMM
  efloat_t Pr_sum_all_paths() const;

  /// Change length
  void set_length(int l);

  /// Construct an 1D DP matrix from a length, and an HMM
  DParrayConstrained(int,const vector<int>&,const vector<double>&,const Matrix&, double Beta);

  virtual ~DParrayConstrained() {}
};



#endif
