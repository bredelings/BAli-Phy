#ifndef ALIGNMENT_UTIL_H
#define ALIGNMENT_UTIL_H

#include <vector>
#include <valarray>
#include "alignment.H"
#include "tree.H"

/// Construct a mapping of letters to columns for each leaf sequence
std::vector< std::vector<int> > column_lookup(const alignment& A,int nleaves = -1);

/// Replace each letter with its position in its sequence
alignment M(const alignment& A1);

/// Is the homology A1(column,s1)::A1(column,s2) preserved in A2 ?
bool A_match(const alignment& A1, int column, int s1, int s2, const alignment& A2,
	     const std::vector< std::vector< int> >& column_indices);

/// Is the alignment of un-ignored sequences in A1 and A2 the same?
bool A_constant(alignment A1, alignment A2, const std::valarray<bool>& ignore);

/// Are all un-ignored characters in column 'present' connected on T?
bool all_characters_connected(const Tree& T,std::valarray<bool> present,const std::vector<int>& ignore);

bool letters_OK(const alignment& A);

#endif
