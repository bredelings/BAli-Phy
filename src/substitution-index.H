#ifndef SUBSTITUTION_INDEX_H
#define SUBSTITUTION_INDEX_H

#include "mytypes.H"
#include <iostream>
#include <vector>
#include "alignment.H"
#include "tree.H"

namespace substitution {

  /// align sub-alignments corresponding to branches in b
  ublas::matrix<int> subA_index(const std::vector<int>& b,const alignment& A);

  /// align sub-alignments corresponding to branches pointing to node
  ublas::matrix<int> subA_index(int node,const alignment& A,const Tree& T); 

  /// align sub-alignments corresponding to branches in b
  ublas::matrix<int> subA_index_select(const std::vector<int>& b,const alignment& A);

  /// align sub-alignments corresponding to branches in b, and select columns with a node in @nodes
  ublas::matrix<int> subA_index_any(const std::vector<int>& b,const alignment& A,const std::vector<int>& nodes);

  /// align sub-alignments corresponding to branches in b, and select columns in 'seq'
  ublas::matrix<int> subA_index_any(const std::vector<int>& b,const alignment& A,
				    const std::vector<int>& nodes, const std::vector<int>& seq);

  /// align sub-alignments corresponding to branches in b, and reject columns with a node in @nodes
  ublas::matrix<int> subA_index_none(const std::vector<int>& b,const alignment& A,
				     const std::vector<int>& nodes);

  bool subA_identical(const ublas::matrix<int>& I1,const ublas::matrix<int>& I2);

  std::ostream& print_subA(std::ostream& o,const ublas::matrix<int>& I);

  void check_subA(const alignment& A1,const alignment& A2,const Tree& T);
}

inline int leaf_seq_length(const alignment& A,int i) {
  assert(i < A.note(0).size2());
  return A.note(0,0,i);
}


int add_subA_index_note(const alignment& A,int b);

inline bool subA_index_valid(const alignment& A,int b) {
  return A.note(1,0,b) != -1;
}

inline int subA_length(const alignment& A,int b) {
  assert(0 <= b and b < A.note(1).size2());
  assert(subA_index_valid(A,b));
  return A.note(1,0,b);
}

void invalidate_subA_index_all(const alignment& A);
void recompute_subA_notes(const alignment& A,const Tree& T);

void subA_index_check_regenerate(const alignment& A1,const Tree& T);
void subA_index_check_footprint(const alignment& A1,const Tree& T);
#endif
