/*
   Copyright (C) 2005,2007-2009 Benjamin Redelings

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

#include <vector>
#include "mytypes.H"
#include "sequence/alphabet.H"
#include "alignment.H"
#include "sequencetree.H"

ublas::matrix<int> unit_cost_matrix(unsigned size);

ublas::matrix<int> unit_cost_matrix(const alphabet&);

ublas::matrix<int> nucleotide_cost_matrix(const Triplets& T);

ublas::matrix<int> amino_acid_cost_matrix(const Codons& C);

template <class B>
B n_mutations(const alphabet& a, const std::vector<int>& letters, const SequenceTree& T,const ublas::matrix<B>& cost);

template <typename B>
B n_mutations(const alignment& A, const SequenceTree& T,const ublas::matrix<B>& cost);

int n_mutations(const alignment& A, const SequenceTree& T);

std::vector<int> get_parsimony_letters(const alphabet& a, const std::vector<int>& letters, const SequenceTree& T,const ublas::matrix<int>& cost);

std::vector<std::vector<int> > get_all_parsimony_letters(const alphabet& a, const std::vector<int>& letters, const SequenceTree& T,const ublas::matrix<int>& cost);
