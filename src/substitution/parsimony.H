/*
   Copyright (C) 2016 Benjamin Redelings

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

#ifndef PARSIMONY_H
#define PARSIMONY_H

#include "util/matrix.H"
#include "sequence/alphabet.H"
#include "models/parameters.H"
#include "tools/parsimony.H"

struct ParsimonyCacheBranch: public Object
{
    int n_letters;
    boost::dynamic_bitset<> bits;
    int sequence_length;
    int alignment_length;
    std::vector<int> n_muts;

    int other_subst = 0;

    ParsimonyCacheBranch* clone() const {return new ParsimonyCacheBranch(*this);}

          int& operator[](int i)       {assert(0 <= i and i<n_muts.size()); return n_muts[i];}
    const int& operator[](int i) const {assert(0 <= i and i<n_muts.size()); return n_muts[i];}

          int& operator()(int c,int l)       {assert(0 <= c and c < sequence_length); return n_muts[c*n_letters+l];}
    const int& operator()(int c,int l) const {assert(0 <= c and c < sequence_length); return n_muts[c*n_letters+l];}

    int min(int c) const
    {
        int x = (*this)(c,0);
        for(int i=1;i<n_letters;i++)
            x = std::min(x, (*this)(c,i));
        return x;
    }

    int max(int c) const
    {
        int x = (*this)(c,0);
        for(int i=1;i<n_letters;i++)
            x = std::max(x, (*this)(c,i));
        return x;
    }

    ParsimonyCacheBranch(int n, int l1)
        :n_letters(n),
         sequence_length(l1),
         n_muts(n_letters*sequence_length)
        {
            assert(n > 0);
        }

    ParsimonyCacheBranch(int n, int l1, int l2)
        :n_letters(n),
         bits(l2),
         sequence_length(l1),
         alignment_length(l2),
         n_muts(n_letters*sequence_length)
        {
            assert(n > 0);
            assert(l1 >= 0);
            assert(l2 >= 0);
        }

    ParsimonyCacheBranch(int n, const boost::dynamic_bitset<>& bits1, const boost::dynamic_bitset<>& bits2)
        :n_letters(n),
         bits(bits1 | bits2),
         sequence_length(bits.count()),
         alignment_length(bits1.size()),
         n_muts(n_letters*sequence_length)
        {
            assert(n > 0);
            assert(bits1.size() == bits2.size());
        }
};

int n_mutations(const data_partition& P, const matrix<int>& cost);

int n_mutations(const data_partition& P);
#endif
