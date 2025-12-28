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

/**
 * @file alignment.H
 */

#ifndef ALIGNMENT_H
#define ALIGNMENT_H

#include <iostream>
#include <vector>
#include <boost/dynamic_bitset.hpp>
#include <filesystem>

#include "sequence/alphabet.H"
#include "sequence/sequence.H"
#include "sequence/sequence-format.H"
#include "util/matrix.H"

// TODO - This kind of alignment requires searching every row to find the characters
//        in that row.

// TODO - I am interested in data structures / algorithms that make it quick and simple
//        to find all the internal nodes of some tree that connect the present characters 
//        of some column.

/// A multiple sequence alignment
class alignment
{
    /// The homology array - accessed through operator()
    matrix<int> array;
  
    /// The unaligned sequences on which the alignment is based.
    std::vector<sequence> sequences;

    /// Reset the alignment: no sequences, an empty array.
    void clear();

    /// Add a new row to the homology array.
    void add_row(const std::vector<int>&);

    /// The alphabet that translates integers back to letters.
    std::shared_ptr<const alphabet> a;
  
public:

    /// Unsafe - Make a copy of the alignment without preserving the homology array
    friend alignment blank_copy(const alignment&,int length);

    /// The order of the sequence named s
    int index(const std::string& s) const;

    /// Get the sorted list of columns that contain characters
    std::vector<int> get_columns_for_characters(int row) const;

    /// Get the sequence (actual letters) for a row
    std::string get_sequence_for_row(int row) const;

    /// The i-th sequence
    const sequence& seq(int i) const { return sequences[i];}
    /// The i-th sequence
    sequence& seq(int i) { return sequences[i];}

    const std::vector<sequence>& seqs() const {return sequences;}

    /// Remove sequence 'i' from the alignment
    void del_sequence(int);
    /// Remove a bunch of sequences' from the alignment
    void del_sequences(const std::vector<int>&);
    /// Add sequence 's' to the alignment
    void add_sequence(const sequence& s);
    /// Add a bunch of sequences to the alignment
    void add_sequences(const std::vector<sequence>&);

    /// Add sequences sequences to the alignment.
    void load(const std::vector<sequence>& sequences);
    /// Add sequences sequences to the alignment.
    void load(const std::string& alph_name,const std::vector<sequence>& sequences);

    /// Parse the file file using loader, and add the resulting sequences.
    void load(sequence_format::loader_t loader,std::istream& file);
    /// Parse the file file using loader, and add the resulting sequences.
    void load(const std::string& alph_name,sequence_format::loader_t,std::istream&);

    /// Load PHYLIP or FASTA file 'filename', depending on extention
    void load(const std::filesystem::path& filename);
    /// Load PHYLIP or FASTA file 'filename', depending on extention
    void load(const std::string& alph_name,const std::filesystem::path& filename);

    /// Get a list of sequences (with gaps) that represent the current alignment.
    std::vector<sequence> convert_to_sequences() const;
    std::vector<std::vector<int>> convert_to_letters() const;

    /// Get a list of sequences (with gaps) that represent the current alignment.
    void write_sequences(sequence_format::dumper_t,std::ostream&) const;

    /// Print alignment in default format (FASTA)
    void print_to_stream(std::ostream&) const;
    /// Print alignment in FASTA format
    void print_fasta_to_stream(std::ostream&) const;
    /// Print alignment in PHYLIP format
    void print_phylip_to_stream(std::ostream&) const;

    /// Resize the alignment array
    void changelength(int l);
    /// Remove a column from the alignment, preserving the information in other columns.
    void delete_column(int i);

    /// The feature (letter,gap,non-gap) of sequence s in column l
    void set_value(int l,int s,int v) {array(l,s) = v; }
    /// The feature (letter,gap,non-gap) of sequence s in column l
    int operator()(int l,int s) const {return array(l,s); }

    /// Does sequence i have a gap at position j ?
    bool gap(int i,int j) const {return array(i,j)==alphabet::gap;}
    /// Does sequence i have an unknown at position j ?
    bool unknown(int i,int j) const {return array(i,j)==alphabet::unknown;}
    /// Does sequence i have an character at position j ?
    bool character(int i,int j) const {return not gap(i,j) and not unknown(i,j);}
    /// Does sequence i have missing data at position j?
    bool missing(int i,int j) const {return array(i,j) < 0;}

    /// Number of columns
    int length() const {return array.size1();}

    /// Total number of sequences
    int n_sequences() const {assert(array.size2() == sequences.size()); return array.size2();}

    //FIXME!  I should cache the results of this
    /// Compute the length of a particular sequence from the array
    int seqlength(int) const;

    /// Access the alignment's alphabet.
    const alphabet& get_alphabet() const {return *a;}
    /// Access the alignment's alphabet.
    std::shared_ptr<const alphabet> get_alphabet_ptr() const {return a;}

    /// Access the alignment's alphabet.
    bool has_alphabet() const {return bool(a);}

    /// Construct an empty alignment
    alignment() {}

    alignment& operator=(const alignment&) = default;

    alignment& operator=(alignment&&) = default;

    alignment(const alignment&) = default;

    alignment(alignment&&) = default;

    /// Construct an empty alignment, with alphabet a
    explicit alignment(const alphabet& a);

    /// Unsafe - Construct an alignment from sequences, but with any empty array
    explicit alignment(const alphabet&,const std::vector<sequence>& sequence, int L);

    /// Unsafe - Construct an alignment from sequences, but with any empty array
    explicit alignment(const alphabet&, int n, int L);

    /// Construct an alignment from file filename using alphabet a
    explicit alignment(const alphabet& a,const std::filesystem::path& filename);
/*
    Techniques to load alignments include:
    1. alignment(); A.load(alph_name, sequences)
    * alignment(a); A.load(           sequences)

    2. alignment(); A.load(alph_name, loader_t, istream)
    * alignment(a); A.load(           loader_t, istream);

    3. alignment(); A.load(alph_name, filename)
    * alignment(a); A.load(           filename)

    Also see load_file( ) in alignment-cat
*/
};

/// Reorder the sequences of \a A according to the permutation @mapping
alignment reorder_sequences(const alignment& A, const std::vector<int>& mapping);

/// Reorder the sequences of \a A so that the names are in the specified order.
alignment reorder_sequences(const alignment& A, const std::vector<std::string>& names);

/// Write alignment A to file file
std::ostream& operator<<(std::ostream& file,const alignment& A);

/// Read alignment A from file file
std::istream& operator>>(std::istream& file,alignment& A);

/// Does the alignment contain any empty columns?
bool valid(const alignment&);

/// Does the column contain only gaps for taxa in mask?
bool all_gaps(const alignment& A,int column,const boost::dynamic_bitset<>& mask);

/// Does the column contain only gaps?
bool all_gaps(const alignment& A,int column);

/// How many non-gap characters are in this column of the alignment?
int n_characters(const alignment& A, int column);

/// remove columns that contain only gaps
int remove_empty_columns(alignment&);

/// Construct a path through the A2 HMM based on the alignment of nodes n1 and n2
std::vector<int> get_path(const alignment& A,int n1,int n2);

/// Return the names of the sequences for alignment A
std::vector<std::string> sequence_names(const alignment& A);
/// Return the names of the sequences of the first n sequences in alignment A
std::vector<std::string> sequence_names(const alignment& A,int n);

/// Generate a randomized alignment
alignment randomize(const alignment&,int n=-1);

alignment blank_copy(const alignment&,int length=0);

std::vector<int> get_sparse_alignment_row(const alignment& A, int i);

// Consider making an iterator type for moving forward through the sparse
// matrix and ignoring columns that are empty for some subset of rows.

class homology_matrix: public matrix<int>
{
    bool character(int c, int s) const {return operator()(c,s) >= 0;}
    bool gap(int c, int s) const {return not character(c,s);}
    int operator()(int c, int s) const {return matrix<int>::operator()(c,s);}
    int& operator()(int c, int s)  = delete;
    void set_cell(int c, int s, int v) {matrix<int>::operator()(c,s) = v;}
    int seqlength(int s) const;
    std::vector<int> get_columns_for_characters(int row) const;
    int length() const {return size1();}
    int n_sequences() const {return size2();}

    using matrix<int>::matrix;
};

class sparse_increasing_index_matrix
{
    int L;
public:
    int length() const;
    std::vector< std::vector<int> > columns;
    void finish_column();
    void set_present(int i);
    sparse_increasing_index_matrix(int);
};

#endif


