#ifndef SUBSTITUTION_INDEX_H
#define SUBSTITUTION_INDEX_H

#include "mytypes.H"
#include <vector>
#include "alignment.H"
#include "tree.H"

namespace substitution {

  /// get an Lx0 matrix containing the leaf letters
  ublas::matrix<int> leaf_index(int b,const alignment& A);

  /// align sub-alignments corresponding to branches in b
  ublas::matrix<int> subA_index(const std::vector<int>& b,const alignment& A,const Tree& T);

  /// align sub-alignments corresponding to branches in b, and select columns with a node in @nodes
  ublas::matrix<int> subA_index_any(const std::vector<int>& b,const alignment& A,const Tree& T,
				    const std::vector<int>& nodes);

  /// align sub-alignments corresponding to branches in b, and select columns in 'seq'
  ublas::matrix<int> subA_index_any(const std::vector<int>& b,const alignment& A,const Tree& T,
				    const std::vector<int>& nodes, const std::vector<int>& seq);

  /// align sub-alignments corresponding to branches in b, and reject columns with a node in @nodes
  ublas::matrix<int> subA_index_none(const std::vector<int>& b,const alignment& A,const Tree& T,
				     const std::vector<int>& nodes);
}

#endif
