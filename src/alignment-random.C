/*
   Copyright (C) 2004-2005,2007-2009 Benjamin Redelings

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

#include "alignment/alignment.H"
#include "rng.H"
#include "alignment/alignment-util.H"

using std::vector;

/// \brief Create an alignment with randomized homology
///
/// \param A An alignment containing the sequences to re-align
alignment randomize(const alignment& A,int n) {
  if (n == -1)
    n = A.n_sequences();

  int maxlength = -1;
  for(int s=0;s<n;s++) {
    if (A.seqlength(s) > maxlength)
      maxlength = A.seqlength(s);
  }

  // Choose the length of the new alignment
  alignment A2 = A;
  int newlength = int( maxlength + 2 + 0.1*maxlength);
  A2.changelength(newlength);

  // For each row of the alignment
  const int temp = alphabet::gap;
  for(int i=0;i<n;i++)
  {
    /// Collect the letters of the row
    vector<int> s = alignment_row_letters(A,i);

    /// Randomly insert gaps until the row is filled
    while(s.size() < newlength) {
      int pos = myrandom(s.size()+1);
      s.insert(s.begin()+pos,temp);
    }

    for(int c=0;c<A2.length();c++)
      A2.set_value(c,i, s[c]);
  }

  remove_empty_columns(A2);
  return A2;
}
