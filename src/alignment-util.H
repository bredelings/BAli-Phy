#ifndef ALIGNMENT_UTIL_H
#define ALIGNMENT_UTIL_H

#include <vector>
#include <valarray>
#include <boost/program_options.hpp>
#include <istream>
#include <string>

#include "alignment.H"
#include "alphabet.H"
#include "tree.H"
#include "clone.H"

/// Reorder the sequences of @A according to the permutation @mapping
alignment reorder_sequences(const alignment& A, const std::vector<int>& mapping);

// Remove sequences to A corresponding to internal nodes in T
alignment chop_internal(alignment A);

// Add sequences to A corresponding to internal nodes in T
alignment add_internal(alignment A,const Tree& T);

/// Load an alignment from command line args align=filename
alignment load_A(const boost::program_options::variables_map& args,bool keep_internal=true);

/// Load an alignment from command line args align=filename
alignment load_alignment(const std::string& filename, const vector<OwnedPointer<alphabet> >& alphabets);

/// Load alignments from command line args align=filename
std::vector<alignment> load_alignments(const vector<std::string>& filenames,
				       const vector<OwnedPointer<alphabet> >& alphabets);

/// Construct a mapping of letters to columns for each leaf sequence
std::vector< std::vector<int> > column_lookup(const alignment& A,int nleaves = -1);

/// Replace each letter with its position in its sequence
ublas::matrix<int> M(const alignment& A1);

/// Is the homology A1(column,s1)::A1(column,s2) preserved in A2 ?
bool A_match(const ublas::matrix<int>& M1, int column, int s1, int s2, 
	     const ublas::matrix<int>& M2,
	     const std::vector< std::vector< int> >& column_indices);

/// Is the alignment of un-ignored sequences in A1 and A2 the same?
bool A_constant(alignment A1, alignment A2, const std::valarray<bool>& ignore);

/// Check that internal nodes don't have letters 
void check_internal_sequences_composition(const alignment& A,int n_leaves);

/// Check that internal node states are consistent
void check_internal_nodes_connected(const alignment& A,const Tree& T,const std::vector<int>& ignore=vector<int>());

/// Are all un-ignored characters in column 'present' connected on T?
bool all_characters_connected(const Tree& T,std::valarray<bool> present,const std::vector<int>& ignore);

void check_leaf_sequences(const alignment& A,int n_leaves);

void check_letters_OK(const alignment& A);
void letters_OK(const alignment& A);

void check_alignment(const alignment& A,const Tree& T,bool internal_sequences);

vector<int> get_splitgroup_columns(const ublas::matrix<int>& M1,
				   int column,
				   const ublas::matrix<int>& M2,
				   const std::vector<std::vector<int> >& columns);

long int asymmetric_pairs_distance(const alignment& A1,const alignment& A2);

long int asymmetric_pairs_distance(const ublas::matrix<int>& M1,const ublas::matrix<int>& M2,
				   const std::vector<std::vector<int> >& column_indices2);

long int asymmetric_splits_distance(const alignment& A1,const alignment& A2);

long int asymmetric_splits_distance(const ublas::matrix<int>& M1,const ublas::matrix<int>& M2,
				    const std::vector<std::vector<int> >& column_indices2);

double homologies_distance(const ublas::matrix<int>& M1,const ublas::matrix<int>& M2,
			   const std::vector<std::vector<int> >& column_indices2);

long int pairs_distance(const alignment& A1,const alignment& A2);

long int pairs_distance(const ublas::matrix<int>& M1,const std::vector<std::vector<int> >& column_indices1,
			const ublas::matrix<int>& M2,const std::vector<std::vector<int> >& column_indices2);

long int splits_distance(const alignment& A1,const alignment& A2);

long int splits_distance(const ublas::matrix<int>& M1,const std::vector<std::vector<int> >& column_indices1,
			 const ublas::matrix<int>& M2,const std::vector<std::vector<int> >& column_indices2);

std::list<alignment> load_alignments(std::istream& ifile, const std::vector<OwnedPointer<alphabet> >& alphabets, int maxalignments);

std::vector<alignment> load_alignments(std::istream&, const std::vector<OwnedPointer<alphabet> >&);

alignment find_last_alignment(std::istream& ifile, const std::vector<OwnedPointer<alphabet> >& alphabets);

alignment find_first_alignment(std::istream& ifile, const std::vector<OwnedPointer<alphabet> >& alphabets);

vector<OwnedPointer<alphabet> > load_alphabets(const boost::program_options::variables_map& args);

void check_disconnected(const alignment& A, const Tree& T, const std::vector<int>& disconnected);

double fraction_identical(const alignment& A,int s1,int s2,bool gaps_count);

double fraction_homologous(const alignment& A,int s1,int s2);

unsigned n_homologous(const alignment& A,int s1,int s2);

std::vector<unsigned> sequence_lengths(const alignment& A);

std::vector<unsigned> sequence_lengths(const alignment& A,unsigned n);

#endif
