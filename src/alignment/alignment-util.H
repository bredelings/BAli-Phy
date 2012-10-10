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

#include <vector>
#include <boost/program_options.hpp>
#include <boost/dynamic_bitset.hpp>
#include <istream>
#include <string>

#include "alignment.H"
#include "sequence/alphabet.H"
#include "sequencetree.H"

// Remove sequences to \a A corresponding to internal nodes in \a T
alignment chop_internal(alignment A,bool keep_empty_columns=false);

// Add sequences to A corresponding to internal nodes in T
alignment add_internal(alignment A,const SequenceTree& T);

/// Load an alignment from command line args --align=filename
alignment load_A(const boost::program_options::variables_map& args,bool keep_internal=true);

/// Load an alignment from command line args align=filename
alignment load_alignment(const std::string& filename, const std::vector<object_ptr<const alphabet> >& alphabets);

/// Load alignments from command line args align=filename
std::vector<alignment> load_alignments(const std::vector<std::string>& filenames,
				       const std::vector<object_ptr<const alphabet> >& alphabets);

/// Construct a mapping of letters to columns for each leaf sequence
std::vector< std::vector<int> > column_lookup(const alignment& A,int nleaves = -1);

/// Replace each letter with its position in its sequence
ublas::matrix<int> M(const alignment& A1);

/// Is the homology A1(column,s1)::A1(column,s2) preserved in A2 ?
bool A_match(const ublas::matrix<int>& M1, int column, int s1, int s2, 
	     const ublas::matrix<int>& M2,
	     const std::vector< std::vector< int> >& column_indices);

/// Is the alignment of un-ignored sequences in A1 and A2 the same?
bool A_constant(alignment A1, alignment A2, const boost::dynamic_bitset<>& ignore);

void check_names_unique(const alignment& A);

bool names_are_unique(const alignment& A);

/// Check that internal nodes don't have letters 
void check_internal_sequences_composition(const alignment& A,int n_leaves);

/// Check that internal node states are consistent
void check_internal_nodes_connected(const alignment& A,const Tree& T,const std::vector<int>& ignore=std::vector<int>());

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

std::vector<int> get_splitgroup_columns(const ublas::matrix<int>& M1,
					int column,
					const ublas::matrix<int>& M2,
					const std::vector<std::vector<int> >& columns);

long int asymmetric_pairs_distance(const alignment& A1,const alignment& A2);

long int asymmetric_pairs_distance(const ublas::matrix<int>& M1,const ublas::matrix<int>& M2,
				   const std::vector<std::vector<int> >& column_indices2);

long int asymmetric_splits_distance(const alignment& A1,const alignment& A2);

long int asymmetric_splits_distance(const ublas::matrix<int>& M1,const ublas::matrix<int>& M2,
				    const std::vector<std::vector<int> >& column_indices2);

long int asymmetric_splits_distance2(const alignment& A1,const alignment& A2);

long int asymmetric_splits_distance2(const ublas::matrix<int>& M1,const ublas::matrix<int>& M2,
				    const std::vector<std::vector<int> >& column_indices2);

double homologies_distance(const ublas::matrix<int>& M1,const ublas::matrix<int>& M2,
			   const std::vector<std::vector<int> >& column_indices2);

long int pairs_distance(const alignment& A1,const alignment& A2);

long int pairs_distance(const ublas::matrix<int>& M1,const std::vector<std::vector<int> >& column_indices1,
			const ublas::matrix<int>& M2,const std::vector<std::vector<int> >& column_indices2);

long int splits_distance(const alignment& A1,const alignment& A2);

long int splits_distance(const ublas::matrix<int>& M1,const std::vector<std::vector<int> >& column_indices1,
			 const ublas::matrix<int>& M2,const std::vector<std::vector<int> >& column_indices2);

long int splits_distance2(const alignment& A1,const alignment& A2);

long int splits_distance2(const ublas::matrix<int>& M1,const std::vector<std::vector<int> >& column_indices1,
			 const ublas::matrix<int>& M2,const std::vector<std::vector<int> >& column_indices2);

std::list<alignment> load_alignments(std::istream& ifile, const std::vector<std::string>& names, const alphabet& a,
				     int skip, int maxalignments);

std::list<alignment> load_alignments(std::istream& ifile, const std::vector<object_ptr<const alphabet> >& alphabets, 
				     int skip, int maxalignments);

std::list<alignment> load_alignments(std::istream& ifile, const std::vector<std::string>& names,
				     const std::vector<object_ptr<const alphabet> >& alphabets,
				     int skip, int maxalignments);

std::vector<alignment> load_alignments(std::istream&, const std::vector<object_ptr<const alphabet> >&);

alignment find_last_alignment(std::istream& ifile, const std::vector<object_ptr<const alphabet> >& alphabets);

alignment find_first_alignment(std::istream& ifile, const std::vector<object_ptr<const alphabet> >& alphabets);

std::vector<object_ptr<const alphabet> > load_alphabets(const boost::program_options::variables_map& args);

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

alignment select_columns(const alignment& A,const std::vector<int>& sites);

alignment reverse(const alignment& A);

alignment complement(const alignment& A);

alignment reverse_complement(const alignment& A);

std::vector<int> alignment_row_letters(const alignment& A, int i);

std::vector< std::vector<int> > alignment_letters(const alignment& A, int n);

void check_same_sequence_lengths(const std::vector<int>&, const alignment&);

alignment unalign_all(const alignment& A, int);
#endif
