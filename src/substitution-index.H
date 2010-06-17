/*
   Copyright (C) 2005 Benjamin Redelings

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

#ifndef SUBSTITUTION_INDEX_H
#define SUBSTITUTION_INDEX_H

#include "mytypes.H"
#include <iostream>
#include <vector>
#include "alignment.H"
#include "tree.H"

struct subA_index_t: public ublas::matrix<int>
{
protected:
  void update_one_branch(const alignment& A,const Tree& T,int b);

  /* This is for SPR all, where we only need branches pointing towards the
     root to be valid, but we don't want to discard the other ones, because
     they will be valid for a different root/SPR-attachment-point.
   */
  bool allow_invalid_branches_;
public:
  subA_index_t(int s1, int s2);

  bool branch_index_valid(int b) const {
    return operator()(0,b) != -1;
  }

  int branch_index_length(int b) const 
  {
    assert(0 <= b and b < size2());
    assert(branch_index_valid(b));
    return operator()(0,b);
  }

  /// align sub-alignments corresponding to branches in b
  ublas::matrix<int> get_subA_index(const std::vector<int>& b,const alignment& A,const Tree& T);

  /// align sub-alignments corresponding to branches in b
  ublas::matrix<int> get_subA_index(const std::vector<int>& b) const;

  /// align sub-alignments corresponding to branches in b, but only keep entries corresponding to present/absent characters
  ublas::matrix<int> get_subA_index_aligned(const std::vector<int>& b,const alignment& A,const Tree& T, bool present);

  /// align sub-alignments corresponding to branches pointing to node
  ublas::matrix<int> get_subA_index(int node,const alignment& A,const Tree& T); 

  /// align sub-alignments corresponding to branches in b
  ublas::matrix<int> get_subA_index_select(const std::vector<int>& b,const alignment& A,const Tree& T);

  /// align sub-alignments corresponding to branches in b
  ublas::matrix<int> get_subA_index_select(const std::vector<int>& b) const;

  /// align sub-alignments corresponding to branches in b, and select columns with a node in \a nodes
  ublas::matrix<int> get_subA_index_any(const std::vector<int>& b,const alignment& A,const Tree& T,const std::vector<int>& nodes);

  /// align sub-alignments corresponding to branches in b, and select columns in 'seq'
  ublas::matrix<int> get_subA_index_any(const std::vector<int>& b,const alignment& A, const Tree& T,
					const std::vector<int>& nodes, const std::vector<int>& seq);

  /// align sub-alignments corresponding to branches in b, and reject columns with a node in \a nodes
  ublas::matrix<int> get_subA_index_none(const std::vector<int>& b,const alignment& A, const Tree& T,
					 const std::vector<int>& nodes);

  void invalidate_one_branch(int b);
  void invalidate_all_branches();
  void invalidate_directed_branch(const Tree& T,int b);
  void invalidate_branch(const Tree& T,int b);

  void update_branch(const alignment& A,const Tree& T,int b);
  void recompute_all_branches(const alignment& A, const Tree& T);

  bool may_have_invalid_branches() const;
  void allow_invalid_branches(bool allowed);

  void check_footprint_for_branch(const alignment& A1,const Tree& T,int b) const;
  void check_footprint(const alignment& A1,const Tree& T) const;
};

void check_regenerate(const subA_index_t& I, const alignment& A1,const Tree& T);
void check_regenerate(const subA_index_t& I, const alignment& A1,const Tree& T,int root);


/// Count the number of entries in a subA index that are not null
int n_non_null_entries(const ublas::matrix<int>&);

/// Count the number of columns in a subA index that are not empty.
int n_non_empty_columns(const ublas::matrix<int>&);

std::ostream& print_subA(std::ostream& o,const ublas::matrix<int>& I);

void check_subA(const subA_index_t& I1, const alignment& A1,const subA_index_t& I2, const alignment& A2,const Tree& T);

inline int leaf_seq_length(const alignment& A,int i) {
  assert(i < A.note(0).size2());
  return A.note(0,0,i);
}

int add_leaf_seq_note(alignment& A,int n);
int add_leaf_seq_note(alignment& A,const ublas::matrix<int>& M);

alignment blank_copy(const alignment&,int length=0);
#endif
