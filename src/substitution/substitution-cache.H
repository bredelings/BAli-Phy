/*
   Copyright (C) 2004-2005,2007,2010 Benjamin Redelings

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

#ifndef SUBSTITUTION_CACHE_H
#define SUBSTITUTION_CACHE_H

#include <boost/shared_ptr.hpp>
#include <vector>
#include "tree/tree.H"
#include "models/TreeInterface.H"

class data_partition;

/// An object to store cached conditional likelihoods for a single branch
class Likelihood_Cache_Branch: public Object
{
  double* data;
  int matrix_size_;

  int n_states_;
  int n_models_;
  int n_columns_;
public:
  Likelihood_Cache_Branch* clone() const {std::abort();}
  
  log_double_t other_subst = 1;

  int n_states() const {return n_states_;}
  int n_models() const {return n_models_;}
  int n_columns() const {return n_columns_;}
  int matrix_size() const {return matrix_size_;}
  
  double* scratch(int i) {return (*this)[n_columns() + i];}

  double* operator[](int i) {return data + matrix_size()*i;}
  const double* operator[](int i) const {return data + matrix_size()*i;}
  
  Likelihood_Cache_Branch(int C,int M, int S)
    :data(new double[(C+2)*M*S]),
     matrix_size_(M*S),
     n_states_(S),
     n_models_(M),
     n_columns_(C)
  {}
  ~Likelihood_Cache_Branch() {delete[] data;}
};


/// A single view into the shared Multi_Likelihood_Cache
class Likelihood_Cache {

  const data_partition* dp;

  /// Mark cached conditional likelihoods for b,b* and all branches after either invalid.
  void invalidate_one_branch(int b);

  /// Mark cached conditional likelihoods for b and all branches after invalid.
  void invalidate_directed_branch(int b);

  /// Get the length columns.
  int get_length(int b) const { return (*this)[b].n_columns(); }

public:

  void set_dp(const data_partition* dp_)
  {
    dp = dp_;
  }

  /// Mark cached conditional likelihoods for all branches invalid.
  void invalidate_all();

  /// Mark cached conditional likelihoods for b,b* and all branches after either invalid.
  void invalidate_branch(int b);

  /// Mark cached conditional likelihoods for all branches after b or b* invalid.
  void invalidate_branch_alignment(int b);

  /// Mark cached conditional likelihoods all branches after n invalid.
  void invalidate_node(int n);

  /// Are cached conditional likelihoods for branch b, up to date?
  bool up_to_date(int b) const;

  /// Ensure there is backing store for us to work with
  void set_branch(int b, Likelihood_Cache_Branch* LCB);

  /// Cached conditional likelihoods for branch b
  const Likelihood_Cache_Branch& operator[](int b) const;

  Likelihood_Cache(const data_partition*);
};

#endif
