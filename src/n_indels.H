/*
   Copyright (C) 2006 Benjamin Redelings

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

#ifndef N_INDELS_H
#define N_INDELS_H

#include "tree.H"
#include "alignment/alignment.H"

std::vector<int> get_indel_lengths(const alignment& A,const Tree& T, int b);
unsigned total_length_indels(const alignment& A,const Tree& T,int b);
unsigned n_indels(const alignment& A,const Tree& T, int b);

unsigned total_length_indels(const alignment& A,const Tree& T);
unsigned n_indels(const alignment& A,const Tree& T);

#endif
