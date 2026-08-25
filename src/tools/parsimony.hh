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
#include "sequence/alphabet.hh"
#include "sequence/doublets.hh"
#include "sequence/RNAEdits.hh"
#include "sequence/codons.hh"
#include "alignment/alignment.hh"
#include "tree/sequencetree.hh"
#include "util/matrix.hh"
#include "util/dense-matrix.hh"

DenseMatrix<int> unit_cost_matrix(unsigned size);

DenseMatrix<int> unit_cost_matrix(const alphabet&);

DenseMatrix<int> nucleotide_cost_matrix(const Triplets& T);

DenseMatrix<int> nucleotide_cost_matrix(const Doublets& D);

DenseMatrix<int> pos1_cost_matrix(const RNAEdits& E);

DenseMatrix<int> pos2_cost_matrix(const RNAEdits& E);

DenseMatrix<int> amino_acid_cost_matrix(const Codons& C);

template <class B>
B n_mutations(const alphabet& a, const ambiguity_database& ambiguities, const std::vector<int>& letters,
              const SequenceTree& T, const DenseMatrix<B>& cost);

template <typename B>
B n_mutations(const alignment& A, const SequenceTree& T,const DenseMatrix<B>& cost);

int n_mutations(const alignment& A, const SequenceTree& T);

std::vector<int> get_parsimony_letters(const alphabet& a, const ambiguity_database& ambiguities,
                                       const std::vector<int>& letters, const SequenceTree& T,
                                       const DenseMatrix<int>& cost);

std::vector<std::vector<int>> get_all_parsimony_letters(const alphabet& a, const ambiguity_database& ambiguities,
                                                        const std::vector<int>& letters, const SequenceTree& T,
                                                        const DenseMatrix<int>& cost);
