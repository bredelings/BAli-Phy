/*
   Copyright (C) 2005,2010-2011 Benjamin Redelings

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
 *
 * These indices are used when performing the Felsenstein peeling
 * algorithm for each branch b.  When combining the conditional
 * likelihoods (CLs) for branches (b1,b2)->b0 to produce the CLs
 * for branch b0, each alignment column leads to a triplet (i,j,k).
 * This triplet specifies that CL i for branch b1 should be combined
 * with CL[j] for branch b2, to produce CL k for branch b0.  The
 * indices i, j, and k can be -1, indicating that no CL for that branch
 * corresponds to this column.  If i or j is -1, then there are only
 * gaps behind that branch.  If k is -1 (which can only occur in
 * subA_index_internal) then the likelihood for the column is evaluated
 * immediately and is added to the total of already evaluated likelihoods
 * behind branch b0.
 *
 * 1. For both naming schemes, the index for a branch pointing away from
 * a leaf node is just the index into the sequence at that leaf.  
 *
 * 2. The index for branches pointing away from an internal node merges
 * the indices from the two branches behind it. The way of doing this is
 * specific to the naming scheme.  See *::update_one_branch( ).
 *
 * 3. Currently the indices are stored in a L*B matrix, where B is the
 * number of DIRECTED branches (e.g. 2*number of undirected branches) and
 * L is the alignment length. If I(c,b) = i, then the column for LC index i
 * on branch b is c.  If I(c,b) = -1, then this column is not indexed for 
 * branch b.
 * - The current method is wasteful when most of the I(c,b)'s == -1.
 */

/* PROJECT: Fixing alignments to handle walking when there's lots of gaps.
 * - We currently need to 
 *   (a) Map internal node characters to subA-indices. 
 *     - Do we need to map back?
 *   (b) Map subA-indices from one branch to subA-indices for an adjacent branch.
 *   (c) Map subA-indices from three adjacent branches to each other. (at the root)
 *   (d) DONE - Separate leaf sequence data from the alignment.
 *   (e) Write a generic interface for things you want to do, like walk the alignment.
 *   (f) Modify substitution.C to avoid refering to the alignment (esp. A.alphabet) where unnecessary.
 *   
 *   
 * - We also need to walk through the alignment ...
 *   + ... that has a character in sequence s.
 *   + ... that has a character in any of sequence s[0], s[1], ...., or s[n-1].
 *   This is for get_order( )
 *
 * - So why do we do it this way?  When do we need the column information?
 *   + We do not need it in get_column_likelihoods( ): there we are just reordering
 *     sub-columns.
 *   + We do it this way only to indicate which subA indices are in the same column.
 *   + Could we instead use a DAG?
 *     - Well, we would need to be able to *partially* compute the DAG behind a directed branch b
 *     - We would also need to be able to generate the DAG for the entire alignment and then
 *       do a topo-sort in order to print the alignment. 
 *     - We could then CACHE the column information on the DAG nodes, if we were trying to match a 
 *       specific linear order.
 *     - We would then still have to handle ordering the columns in/for get_column_likelihoods( )
 *
 * IDEA: Suppose I used a partially ordered graph.
 *   + Modifying the alignment becomes more complicated.
 *   + Mostly we modify alignments only in 
 *     - alignment.C
 *     - 2way.C (construct)
 *     - 3way.C (construct, project)
 *     - 5way.C (construct, project)
 *     - alignment-random.C (randomize)
 *     - alignment-util.C (minimally_connect_leaf_characters, connect_leaf_characters, select_columns, reverse, complement, reverse_complement, unalign_all)
 *     - setup.C (various load_A_T functions: clear internal node sequences)
 *     - tools/alignment-thin.C (remove_almost_empty_columns)
 *     - tools/index-matrix.C (get_alignment)
 *     - tools/alignment-compare.C (get_alignment)
 *
 * 4. Note that even for the indexing scheme subA_index_leaf (in which all
 * columns correspond to CL index at the root of the tree) the ORDER
 * of these CL indices may differ from the order of columns in the alignment.
 */

/* Naming Scheme #1 (subA_index_leaf)
 *
 * The first naming scheme for sub-alignment columns satisfies two
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

/* Naming Scheme #2 (subA_index_internal)
 *
 * In this naming scheme, we index a column c behind a directed branch
 * b whenever the alignment matrix has a non-gap in the cell A(c, b.source()).
 * Therefore, this naming scheme requires the alignment to contain homology
 * information for internal node sequences, since the node b.source() will
 * often be internal.
 *
 * In this naming scheme, when we process two branches b1,b2 that point to
 * a branch b0, it sometimes occurs that a column that is present in the 
 * indices for b1 and b2 is NOT present in the index for b0.  In this case
 * we calculate the likelihood for the column when processing branch b0, 
 * before we get to the root.
 *
 * This approach has two main benefits.  First, if saves computation time.
 * This is because when a column contains only a single character, we do not
 * need to peel it all the way up to the root.  Instead, after peeling one
 * branch, we calculate the likelihood and propagate these summed
 * log-likelihoods up to the root, which is much cheaper.
 * 
 * Second, we do not need to allocate memory for a the conditional likelihood
 * on all the branches up to the root.  This can result in a huge decrease in
 * memory use when analyzing a large number of taxa, because insertions in
 * a single sequence now incur a constant memory cost.  Previously, an insertion
 * in a single sequence would incur a cost of storing 2*B conditional likelihood
 * matrices if the tree had B branches.  You can visualize this by considering
 * that in a matrix with many branches, we no longer incur a memory cost for
 * alignment cells that contain a gap, but only for cells that contain a character.
 * This, the memory cost of unaligning two sequences is drastically decreased.
 *
 * On the downside, this method of indexing requires recalculating the indexes
 * (and thus the conditional likelihoods) for a branch b if the alignment changes
 * on any branch behind b.  This is true even if the alignment of leaf sequences
 * is unchanged.  Therefore, conditional likelihoods must be recalculated after 
 * performing an NNI move, because a column containing internal node characters
 * but not leaf characters MAY be added or removed by the alignment resampling.
 * In theory this could make subA_index_internal slower than subA_index_leaf, but
 * this cost does not seem to be too high in practice.
 *
 * Question: is subA_index_internal affected by the reordering of unordered
 *  columns in the alignment, like subA_index_leaf is?
 * 
 */

/*
 * The get_subA_index( ) functions are used ONLY in substitution.C and substitution-index.C
 * 
 */

/*
 * We may wish to separate the naming scheme for the indices (leaf, internal) from the
 * method used to calculate it.  Since the "internal" scheme only makes sense with
 * augmented alignments, we could define update_one_branch( ) in terms of the pairwise
 * alignments for the previous branches...*/

struct subA_index_t
{
  enum subA_index_kind {leaf_index=1, internal_index=2};

protected:
  subA_index_kind kind_;

  std::vector< std::vector< std::pair<int,int> > > indices;

  std::vector<bool> up_to_date;

  virtual void update_one_branch(const alignment& A,const Tree& T,int b)=0;

  /* This is for SPR all, where we only need branches pointing towards the
     root to be valid, but we don't want to discard the other ones, because
     they will be valid for a different root/SPR-attachment-point.
   */
  bool allow_invalid_branches_;
public:
  virtual subA_index_t* clone() const=0;
  
  subA_index_t(subA_index_kind k, int s1, int s2);

  int n_rows() const;

  bool branch_index_valid(int b) const {
    return up_to_date[b];
  }

  int branch_index_length(int b) const 
  {
    assert(0 <= b and b < n_rows());
    assert(branch_index_valid(b));
    return indices[b].size();
  }

  const std::vector<std::pair<int,int> > operator[](int r) const {return indices[r];}

  /// align sub-alignments corresponding to branches in b
  ublas::matrix<int> get_subA_index(const std::vector<int>& b,const alignment& A,const Tree& T,bool=false);

  /// align sub-alignments corresponding to branches in b
  ublas::matrix<int> get_subA_index(const std::vector<int>& b,bool=false) const;

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

  /// align sub-alignments corresponding to branches in b, and reject columns with a node in \a nodes
  ublas::matrix<int> get_subA_index_none(const std::vector<int>& b,const alignment& A, const Tree& T,
					 const std::vector<int>& nodes);

  /// align sub-alignments corresponding to branches in b, and select columns in 'seq'
  ublas::matrix<int> get_subA_index_columns(const std::vector<int>& b,const alignment& A, const Tree& T,
					    const std::vector<int>& seq);

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

  virtual std::vector<int> characters_to_indices(int branch, const alignment& A, const Tree& T);

  int kind() const {return kind_;}

  virtual ~subA_index_t() {}
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
