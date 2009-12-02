/*
   Copyright (C) 2004-2005,2007 Benjamin Redelings

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
#include "mytypes.H"
#include "tree.H"
#include "smodel.H"


/// A class to manage storage and sharing of cached conditional likelihoods.
class Multi_Likelihood_Cache: public std::vector< std::vector< Matrix > >
{

protected:
  int C; // the (maximum) number of columns available per branch
  int M; // number of models
  int S; // number of states

  /// mapping[token][branch] -> location
  std::vector<std::vector<int> > mapping;

  /// number of users of each location
  std::vector<int> n_uses;

  /// a stack of unused_locations
  std::vector<int> unused_locations;

  /// is each token in use or not?
  std::vector<bool> active;

  /// length of each active cache (in columns)
  std::vector<int> length;

  /// acquire and return an unused location
  int get_unused_location();

  /// release one use of location loc
  void release_location(int loc);

  /// Allocate space for s new 'branches'
  void allocate(int s);

  /// Is each location up to date?
  std::vector<int> up_to_date_;

  /// Can each token re-use the previously computed likelihood?
  std::vector<int> cv_up_to_date_;

public:

  /// Can token t re-use its previously computed likelihood?
  int  cv_up_to_date(int t) const {return cv_up_to_date_[t];}
  /// Can token t re-use its previously computed likelihood?
  int& cv_up_to_date(int t)       {return cv_up_to_date_[t];}

  /// Where do we store caches for token t, branch b?
  int location(int t,int b) const {return mapping[t][b];}

  /// Are cached conditional likelihoods for token t/branch b, up to date?
  bool up_to_date(int token,int branch) const {
    return up_to_date_[ mapping[token][branch] ];
  }

  /// Mark cached conditional likelihoods for token t/branch b up to date.
  void validate_branch(int token,int branch);

  /// Determine the number of CTMC models out model is a mixture of.
  int n_models() const {return M;}
  /// The size of the alphabet
  int n_states() const {return S;}

  /// Mark cached conditional likelihoods for token t/branch b invalid.
  void invalidate_one_branch(int token,int branch);
  /// Mark cached conditional likelihoods for all branches of token t invalid.
  void invalidate_all(int token);

  /// Set the length of token t to l columns.
  void set_length(int token, int l);
  /// Get the length of token t in columns.
  int  get_length(int token) const {return length[token];}

  /// Return an unused token.
  int find_free_token() const;
  /// Create an unused token.
  int add_token(int B);
  /// Acquire a token for use with C columns and B/2 branches.
  int claim_token(int C,int B);
  /// Setup token2 to point to cached likelihoods for token1
  void copy_token(int token1,int token2);
  /// Initialize token
  void init_token(int token);
  /// Release token and mark unused.
  void release_token(int token);
  
  Multi_Likelihood_Cache(const substitution::MultiModel& M);
};

/// A single view into the shared Multi_Likelihood_Cache
class Likelihood_Cache {
  boost::shared_ptr<Multi_Likelihood_Cache> cache;

  /// The number of directed branches + 1
  int B;

  /// The slot in 'cache' that we use.
  int token;

public:
  /// Previously computed likelihood.
  efloat_t cached_value;

  /// Can we re-use our previously computed likelihood?
  int  cv_up_to_date() const {return cache->cv_up_to_date(token);}
  /// Can we re-use our previously computed likelihood?
  int& cv_up_to_date()       {return cache->cv_up_to_date(token);}

  /// Starting point for our likelihood calculations
  int root;

  /// Number of columns of our alignment
  int length() const {return cache->get_length(token);}
  /// The scratch branch slot
  int scratch() const {return B-1;}
  /// The number of undirected branches;
  int n_branches() const {return B/2;}
  /// The number of CTMC models in our mixture.
  int n_models() const {return cache->n_models();}
  /// The number of states in our alphabet.
  int n_states() const {return cache->n_states();}

  /// Mark cached conditional likelihoods for all branches invalid.
  void invalidate_all();

  /// Mark cached conditional likelihoods for b and all branches after invalid.
  void invalidate_directed_branch(const Tree&,int b);

  /// Mark cached conditional likelihoods for b,b* and all branches after either invalid.
  void invalidate_branch(const Tree&,int b);

  /// Mark cached conditional likelihoods for b,b* and all branches after either invalid.
  void invalidate_one_branch(int b);

  /// Mark cached conditional likelihoods for all branches after b or b* invalid.
  void invalidate_branch_alignment(const Tree&,int b);

  /// Mark cached conditional likelihoods all branches after n invalid.
  void invalidate_node(const Tree&,int n);

  /// Set the length to l columns.
  void set_length(int l);
  /// Get the length columns.
  int get_length() const {return cache->get_length(token);}

  /// Are cached conditional likelihoods for branch b, up to date?
  bool up_to_date(int b) const {return cache->up_to_date(token,b);}
  /// Mark cached conditional likelihoods for branch b up to date.
  void validate_branch(int b) {cache->validate_branch(token,b);}

  /// Cached conditional likelihoods for branch b
  const vector<Matrix>& operator[](int b) const {
    int loc = cache->location(token,b);
    return (*cache)[loc];
  }

  /// Cached conditional likelihoods for branch b
  vector<Matrix>& operator[](int b) {
    int loc = cache->location(token,b);
    return (*cache)[loc];
  }

  /// Cached conditional likelihoods for index i, branch b
  const Matrix& operator()(int i,int b) const {
    int loc = cache->location(token,b);
    assert(0 <= i and i < get_length());
    return (*cache)[loc][i];
  }

  /// Cached conditional likelihoods for index i, branch b
  Matrix& operator()(int i,int b) {
    int loc = cache->location(token,b);
    assert(0 <= i and i < get_length());
    return (*cache)[loc][i];
  }

  /// Scratch matrix i
  const Matrix& scratch(int i) const {
    int loc = cache->location(token,B-1);
    assert(0 <= i and i < get_length());
    return (*cache)[loc][i];
  }

  /// Scratch matrix i
  Matrix& scratch(int i) {
    int loc = cache->location(token,B-1);
    assert(0 <= i and i < get_length());
    return (*cache)[loc][i];
  }

  /// Construct a duplicate view to the same conditional likelihood caches
  Likelihood_Cache& operator=(const Likelihood_Cache&);

  Likelihood_Cache(const Likelihood_Cache& LC);
  Likelihood_Cache(const Tree& T, const substitution::MultiModel& M,int l=0);

  ~Likelihood_Cache();
};

/// Set the root to whichever node of b is closer to the current root.
void select_root(const Tree& T,int b,Likelihood_Cache& LC);

#endif
