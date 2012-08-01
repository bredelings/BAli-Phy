/*
   Copyright (C) 2004-2005,2009 Benjamin Redelings

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

#ifndef TWOWAY_H
#define TWOWAY_H

#include <vector>
#include "alignment.H"
#include "tree.H"

struct pairwise_alignment_t: public std::vector<int>
{
  void flip();
  pairwise_alignment_t flipped() const;

  int length1() const;
  int length2() const;

  pairwise_alignment_t(const std::vector<int>&);
  pairwise_alignment_t(const pairwise_alignment_t&);
  pairwise_alignment_t();
};

bool operator==(const pairwise_alignment_t&, const pairwise_alignment_t&);

namespace A2 {

  namespace states {
    const int M  = 0;
    const int G1 = 1;
    const int G2 = 2;
    const int E  = 3;
    const int S  = 4;
  }

  int flip(int s);

  pairwise_alignment_t get_pairwise_alignment(const alignment& A, int n1, int n2);


  alignment construct(const alignment& old, const std::vector<int>& path, int n1,int n2, 
		      const Tree& T, const std::vector<int>& seq1,const std::vector<int>& seq2);



  ublas::matrix<int> construct(const Tree& T, const std::vector<pairwise_alignment_t>& A);

  pairwise_alignment_t get_pairwise_alignment_from_path(const std::vector<int>& path);
}

#endif
