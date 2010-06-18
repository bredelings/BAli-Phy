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

/* The algorithms in this file provide each column in a sub-alignment
 * with a name that persists through alignment and tree changes that
 * do not make the column non-identifiable.  The name is a column
 * number, so that naming columns corresponds to ordering the columns
 * in the sub-alignment.   We need persistent names in order to cache
 * conditional likelihoods at columns in the sub-alignment,
 * specifically in order to cache across internal-node resampling.
 */

/* Naming Scheme #1
 *
 * Our naming scheme for sub-alignment columns satisfies two
 * important properties: 
 * a) ordering depends only on pairwise alignments behind b and the
 *    topology of the subtree behind b
 * b) ordering depends only on the pairwise alignments PROJECTED TO
 *    THE LEAF SEQUENCES and does not change when the alignment of
 *    leaf sequences is unchanged. 
 *
 * a -> Each sub-alignment corresponds to a directed branch b on the
 * tree, and contains the leaf sequences BEHIND b. If alignments on
 * branches behind b do not change, then the sub-alignment b should
 * not change either.  This isn't automatic because the columns of the
 * sub-alignment may not be fully ordered.  Note that changing the
 * topology behind b would necessitate invalidating the cached
 * conditional likelihoods anyway, so we lose nothing by not
 * preserving column numbers in that case.
 * 
 * b-> This gives the useful property that re-sampling internal nodes
 * does not invalidate any of the names.  However, this implies that
 * the ordering of columns in a sub-alignment can conflict with
 * ordering in the full alignment, or with the order of sub-alignments
 * which include this sub-alignment. 
 */


struct subA_index_t: public ublas::matrix<int>
{
protected:
  virtual void update_one_branch(const alignment& A,const Tree& T,int b)=0;

  /* This is for SPR all, where we only need branches pointing towards the
     root to be valid, but we don't want to discard the other ones, because
     they will be valid for a different root/SPR-attachment-point.
   */
  bool allow_invalid_branches_;
public:
  virtual subA_index_t* clone() const=0;
  
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

  /// select sub-alignments columns in branches (b-b.back()) that are aligned to nothing in b.back()
  ublas::matrix<int> get_subA_index_vanishing(const std::vector<int>& b,const alignment& A,const Tree& T);

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

  virtual void check_footprint_for_branch(const alignment& A1,const Tree& T,int b) const=0;
  void check_footprint(const alignment& A1,const Tree& T) const;
};

struct subA_index_leaf: public subA_index_t
{
protected:
  void update_one_branch(const alignment& A,const Tree& T,int b);

public:
  subA_index_t* clone() const {return new subA_index_leaf(*this);}

  void check_footprint_for_branch(const alignment& A1,const Tree& T,int b) const;

  subA_index_leaf(int s1, int s2);
};

struct subA_index_internal: public subA_index_t
{
protected:
  void update_one_branch(const alignment& A,const Tree& T,int b);

public:
  subA_index_t* clone() const {return new subA_index_internal(*this);}

  void check_footprint_for_branch(const alignment& A1,const Tree& T,int b) const;

  subA_index_internal(int s1, int s2);
};

void check_regenerate(const subA_index_t& I, const alignment& A1,const Tree& T);
void check_regenerate(const subA_index_t& I, const alignment& A1,const Tree& T,int root);


/// Count the number of entries in a subA index that are not null
int n_non_null_entries(const ublas::matrix<int>&);

/// Count the number of columns in a subA index that are not empty.
int n_non_empty_columns(const ublas::matrix<int>&);

std::ostream& print_subA(std::ostream& o,const ublas::matrix<int>& I);

void check_subA(const subA_index_t& I1, const alignment& A1,const subA_index_t& I2, const alignment& A2,const Tree& T);
#endif
