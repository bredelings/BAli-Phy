/*
   Copyright (C) 2004 Benjamin Redelings

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

#ifndef BITS_H
#define BITS_H

inline bool bitset(int m,int b) {
  return m&(1<<b);
}

inline bool bits_present(int i,int m) {
  return ((i&m) == m);
}

inline int clearbit(int m,int b) {
  return m&(~(1<<b));
}

inline int setbit(int m,int b) {
  return m|(1<<b);
}

inline int setbit(int m,int b,bool c) {
  if (c) m = setbit(m,b);
  return m;
}

inline int num_bits(int n) {
  int sum=0;
  for(int i=0;i<6;i++) 
    if (bitset(n,i)) sum++;
  return sum;
}

#endif
