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


/// \brief A class to manage storage and sharing of cached conditional likelihoods.
///
/// The Multi_Likelihood_Cache maintains a number of "locations", each
/// of which store cached conditional likelihoods for one branch.  Different
/// Likelihood_Cache objects each have a unique token, which maps each
/// of their branches to a "location" that indicates a Likelihood_Cache_Branch.
/// Different Likelihood_Cache objects may share the same Likelihood_Cache_Branch
/// if the cached likelihoods on that branch are the same for both objects.
///
class Multi_Likelihood_Cache
{
protected:
  std::vector<Likelihood_Cache_Branch*> data;
  
  /// mapping[token][branch] -> location
  std::vector<std::vector<int> > mapping;

  /// number of users of each location
  std::vector<int> n_uses;

  /// a stack of unused_locations
  std::vector<int> unused_locations;

  /// is each token in use or not?
  std::vector<bool> active;

  /// acquire and return an unused location
  int get_unused_location();

  /// release one use of location loc
  void release_location(int loc);

  /// Allocate space for s new 'branches'
  void allocate_branch_slots(int s);

  /// Is each location up to date?
  std::vector<int> up_to_date_;

  /// Can each token re-use the previously computed likelihood?
  std::vector<int> cv_up_to_date_;

public:
  Likelihood_Cache_Branch& operator[](int b) {return *data[b];}
  
  /// Can token t re-use its previously computed likelihood?
  int  cv_up_to_date(int t) const {return cv_up_to_date_[t];}
  /// Can token t re-use its previously computed likelihood?
  int& cv_up_to_date(int t)       {return cv_up_to_date_[t];}

  /// Point t/b to object
  void set_location(int t, int b, Likelihood_Cache_Branch* LCB);

  /// Where do we store caches for token t, branch b?
  int location(int t,int b) const {
    assert(mapping[t][b] != -1);
    assert(data[mapping[t][b]]);
    return mapping[t][b];
  }

  /// Has branch b for token t been allocated a location yet?
  int location_allocated(int t, int b) const {return mapping[t][b] != -1;}

  /// Are cached conditional likelihoods for token t/branch b, up to date?
  bool up_to_date(int t,int b) const 
  {
    return location_allocated(t,b) and up_to_date_[ location(t,b) ];
  }

  /// Mark cached conditional likelihoods for token t/branch b up to date.
  void validate_branch(int token,int branch);

  /// Mark cached conditional likelihoods for token t/branch b invalid, and unshare.
  void invalidate_one_branch(int token,int branch);
  /// Mark cached conditional likelihoods for all branches of token t invalid.
  void invalidate_all(int token);

  /// Return an unused token.
  int find_free_token() const;
  /// Create an unused token.
  int add_token(int B);
  /// Acquire a token for use with C columns and B/2 branches.
  int claim_token(int B);
  /// Setup token2 to point to cached likelihoods for token1
  void copy_token(int token1,int token2);
  /// Initialize token
  void init_token(int token);
  /// Release token and mark unused.
  void release_token(int token);
  
  ~Multi_Likelihood_Cache();
};

/// A single view into the shared Multi_Likelihood_Cache
class Likelihood_Cache {
  boost::shared_ptr<Multi_Likelihood_Cache> cache;

  /// The number of directed branches
  int B;

  /// The slot in 'cache' that we use.
  int token;

  const data_partition* dp;

  /// Mark cached conditional likelihoods for b,b* and all branches after either invalid.
  void invalidate_one_branch(int b);

  /// Mark cached conditional likelihoods for b and all branches after invalid.
  void invalidate_directed_branch(int b);

  /// Get the length columns.
  int get_length(int b) const { return (*this)[b].n_columns(); }

public:
  /// Previously computed likelihood.
  log_double_t cached_value;

  void set_dp(const data_partition* dp_)
  {
    dp = dp_;
  }

  /// Can we re-use our previously computed likelihood?
  int  cv_up_to_date() const {return cache->cv_up_to_date(token);}
  /// Can we re-use our previously computed likelihood?
  int& cv_up_to_date()       {return cache->cv_up_to_date(token);}

  /// Mark cached conditional likelihoods for all branches invalid.
  void invalidate_all();

  /// Mark cached conditional likelihoods for b,b* and all branches after either invalid.
  void invalidate_branch(int b);

  /// Mark cached conditional likelihoods for all branches after b or b* invalid.
  void invalidate_branch_alignment(int b);

  /// Mark cached conditional likelihoods all branches after n invalid.
  void invalidate_node(int n);

  /// Are cached conditional likelihoods for branch b, up to date?
  bool up_to_date(int b) const {return cache->up_to_date(token,b);}
  /// Mark cached conditional likelihoods for branch b up to date.
  void validate_branch(int b) {cache->validate_branch(token,b);}
  /// Is there is backing store for us to work with?
  bool branch_available(int b) const {return cache->location_allocated(token,b);}

  /// Ensure there is backing store for us to work with
  void set_branch(int b, Likelihood_Cache_Branch* LCB) {
    cache->set_location(token, b, LCB);
  }

  /// Cached conditional likelihoods for branch b
  const Likelihood_Cache_Branch& operator[](int b) const {
    int loc = cache->location(token,b);
    assert(loc != -1);
    return (*cache)[loc];
  }

  /// Construct a duplicate view to the same conditional likelihood caches
  Likelihood_Cache& operator=(const Likelihood_Cache&);

  Likelihood_Cache(const Likelihood_Cache& LC);
  Likelihood_Cache(const data_partition*);

  ~Likelihood_Cache();
};

#endif
