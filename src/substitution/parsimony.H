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

#include "matrix.H"
#include "sequence/alphabet.H"
#include "models/parameters.H"

matrix<int> unit_cost_matrix(unsigned size);

matrix<int> unit_cost_matrix(const alphabet&);

matrix<int> nucleotide_cost_matrix(const Triplets& T);

matrix<int> amino_acid_cost_matrix(const Codons& C);

int n_mutations(const data_partition& P, const matrix<int>& cost);

int n_mutations(const data_partition& P);
#endif
