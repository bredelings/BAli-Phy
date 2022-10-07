/*
   Copyright (C) 2004-2009 Benjamin Redelings

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

///
/// \file alignment-util.H
///
/// \brief This file implements alignment utility functions.
///

#ifndef ALIGNMENT_UTIL_H
#define ALIGNMENT_UTIL_H

#include <boost/dynamic_bitset.hpp>
#include <functional>
#include <optional>
#include <string>
#include <vector>

#include "alignment.H"
class Tree;
class SequenceTree;


// Remove sequences to \a A corresponding to internal nodes in \a T
alignment chop_internal(alignment A,bool keep_empty_columns=false);

// Remove sequences to \a A corresponding to internal nodes in \a T
alignment chop_internal(alignment A, const SequenceTree& T);

// Remove sequences to \a A corresponding to internal nodes in \a T
alignment chop_internal(alignment A, int n_leaves);

// Add sequences to A corresponding to internal nodes in T
alignment add_internal(alignment A,const SequenceTree& T);

/// Construct a mapping of letters to columns for each leaf sequence
std::vector< std::vector<int> > column_lookup(const alignment& A,int nleaves = -1);

/// Replace each letter with its position in its sequence
matrix<int> M(const alignment& A1);

/// Is the homology A1(column,s1)::A1(column,s2) preserved in A2 ?
bool A_match(const matrix<int>& M1, int column, int s1, int s2, 
	     const matrix<int>& M2,
	     const std::vector< std::vector< int> >& column_indices);

/// Is the alignment of un-ignored sequences in A1 and A2 the same?
bool A_constant(alignment A1, alignment A2, const boost::dynamic_bitset<>& ignore);

void check_names_unique(const alignment& A);

bool names_are_unique(const alignment& A);

/// Check that internal nodes don't have letters 
void check_internal_sequences_composition(const alignment& A,int n_leaves);

/// Check that internal node states are consistent
void check_internal_nodes_connected(const alignment& A,const Tree& T);

/// Are all un-ignored characters in column 'present' connected on T?
bool all_characters_connected(const Tree& T,boost::dynamic_bitset<> present,const std::vector<int>& ignore);

/// Force internal node states to be consistent by connecting leaf characters
void connect_leaf_characters(alignment& A,const Tree& T);
void minimally_connect_leaf_characters(alignment& A,const Tree& T);

bool check_leaf_characters_minimally_connected(const alignment& A,const Tree& T);

void check_leaf_sequences(const alignment& A,int n_leaves);

void check_letters_OK(const alignment& A);
void letters_OK(const alignment& A);

void check_alignment(const alignment& A,const Tree& T,bool internal_sequences);

std::vector<int> get_splitgroup_columns(const matrix<int>& M1,
					int column,
					const matrix<int>& M2,
					const std::vector<std::vector<int> >& columns);

double asymmetric_pairs_distance(const alignment& A1,const alignment& A2);

double asymmetric_pairs_distance(const matrix<int>& M1,const matrix<int>& M2,
				   const std::vector<std::vector<int> >& column_indices2);

double asymmetric_splits_distance(const alignment& A1,const alignment& A2);

double asymmetric_splits_distance(const matrix<int>& M1,const matrix<int>& M2,
				    const std::vector<std::vector<int> >& column_indices2);

double asymmetric_splits_distance2(const alignment& A1,const alignment& A2);

double asymmetric_splits_distance2(const matrix<int>& M1,const matrix<int>& M2,
				    const std::vector<std::vector<int> >& column_indices2);

double homologies_distance(const matrix<int>& M1,const matrix<int>& M2,
			   const std::vector<std::vector<int> >& column_indices2);

double pairs_distance(const alignment& A1,const alignment& A2);

double pairs_distance(const matrix<int>& M1,const std::vector<std::vector<int> >& column_indices1,
			const matrix<int>& M2,const std::vector<std::vector<int> >& column_indices2);

double splits_distance(const alignment& A1,const alignment& A2);

double splits_distance(const matrix<int>& M1,const std::vector<std::vector<int> >& column_indices1,
			 const matrix<int>& M2,const std::vector<std::vector<int> >& column_indices2);

double splits_distance2(const alignment& A1,const alignment& A2);

double splits_distance2(const matrix<int>& M1,const std::vector<std::vector<int> >& column_indices1,
			 const matrix<int>& M2,const std::vector<std::vector<int> >& column_indices2);

void check_disconnected(const alignment& A, const Tree& T, const std::vector<int>& disconnected);

double fraction_identical(const alignment& A,int s1,int s2,bool gaps_count);

double fraction_homologous(const alignment& A,int s1,int s2);

unsigned n_homologous(const alignment& A,int s1,int s2);

unsigned n_letters(const alignment& A, int c, bool);

boost::dynamic_bitset<> letter_informative_sites(const alignment& A);
unsigned n_letter_informative_sites(const alignment& A);

boost::dynamic_bitset<> letter_variable_sites(const alignment& A);
unsigned n_letter_variable_sites(const alignment& A);

boost::dynamic_bitset<> gap_informative_sites(const alignment& A);
unsigned n_gap_informative_sites(const alignment& A);

boost::dynamic_bitset<> gap_variable_sites(const alignment& A);
unsigned n_gap_variable_sites(const alignment& A);

std::vector<unsigned> sequence_lengths(const alignment& A);

std::vector<unsigned> sequence_lengths(const alignment& A,unsigned n);

alignment select_rows(const alignment& A,const std::vector<int>& keep);

void select_columns_inplace(alignment& A, const std::vector<int>& columns);

alignment select_columns(const alignment& A,const std::vector<int>& sites);

alignment select_columns(const alignment& A,const boost::dynamic_bitset<>& keep);

alignment reverse(const alignment& A);

alignment complement(const alignment& A);

alignment reverse_complement(const alignment& A);

std::vector<int> alignment_row_letters(const alignment& A, int i);

std::vector<int> alignment_row_counts(const alignment& A, int i, const std::vector<int>& counts);

std::vector< std::vector<int> > alignment_letters(const alignment& A, int n);

std::vector< std::vector<int> > alignment_letters_counts(const alignment& A, int n, const std::vector<int>& counts);

void check_same_sequence_lengths(const std::vector<int>&, const alignment&);

alignment unalign_all(const alignment& A, int);

std::optional<int> check_characters_present(const alignment& A, const std::vector<boost::dynamic_bitset<>>& present);

bool is_masked_column(const alignment& A, int c);

bool is_variant_column(const alignment& A, int c);

bool is_column_with_gap(const alignment& A, int c);

int count_variant_columns(const alignment& A, int c1, int c2);

void remove_columns(alignment& A, const std::function<bool(int)>& remove);

void mask_column(alignment& A, int column);

void remove_and_mask_columns(alignment& A, const std::function<int(int)>& remove_or_mask);

boost::dynamic_bitset<> find_columns(const alignment& A, const std::function<bool(const alignment&,int)>& pred);

std::vector<int> find_columns(const alignment& A, const std::function<bool(int)>& keep);

std::vector<int> find_columns(const alignment& A, const std::function<bool(const alignment&,int)>& pred, int label);

boost::dynamic_bitset<> gap_columns(const alignment& A);

std::vector<int> gap_columns(const alignment& A, int label);

std::vector<int> gap_columns(const alignment& A, int label, double fraction);

boost::dynamic_bitset<> variant_columns(const alignment& A);

boost::dynamic_bitset<> variant_column_at_distance(const alignment& A,int d);

#endif
