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
#include "sequence/alphabet.H"
#include "sequence/doublets.H"
#include "sequence/RNAEdits.H"
#include "sequence/codons.H"
#include "alignment/alignment.H"
#include "tree/sequencetree.H"
#include "util/matrix.H"

matrix<int> unit_cost_matrix(unsigned size);

matrix<int> unit_cost_matrix(const alphabet&);

matrix<int> nucleotide_cost_matrix(const Triplets& T);

matrix<int> nucleotide_cost_matrix(const Doublets& D);

matrix<int> pos1_cost_matrix(const RNAEdits& E);

matrix<int> pos2_cost_matrix(const RNAEdits& E);

matrix<int> amino_acid_cost_matrix(const Codons& C);

template <class B>
B n_mutations(const alphabet& a, const std::vector<int>& letters, const SequenceTree& T,const matrix<B>& cost);

template <typename B>
B n_mutations(const alignment& A, const SequenceTree& T,const matrix<B>& cost);

int n_mutations(const alignment& A, const SequenceTree& T);

std::vector<int> get_parsimony_letters(const alphabet& a, const std::vector<int>& letters, const SequenceTree& T,const matrix<int>& cost);

std::vector<std::vector<int> > get_all_parsimony_letters(const alphabet& a, const std::vector<int>& letters, const SequenceTree& T,const matrix<int>& cost);
