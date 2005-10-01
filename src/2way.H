#ifndef TWOWAY_H
#define TWOWAY_H

#include <vector>
#include <valarray>
#include "alignment.H"
#include "tree.H"

namespace A2 {

namespace states {
  const int M  = 0;
  const int G1 = 1;
  const int G2 = 2;
  const int E  = 3;
  const int S  = 4;
}

  alignment construct(const alignment& old, const std::vector<int>& path, int n1,int n2, 
		      const Tree& T, const std::vector<int>& seq1,const std::vector<int>& seq2);




}

#endif
