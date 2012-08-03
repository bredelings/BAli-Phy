/*
   Copyright (C) 2004-2009 Benjamin Redelings

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

#ifndef LIKELIHOOD_H
#define LIKELIHOOD_H

#include "imodel.H"
#include "alignment/alignment.H"
#include "parameters.H"

//-------------- Alignment Priors -----------------//

efloat_t prior_HMM_rootless_scale(const data_partition& P);
efloat_t prior_branch(const alignment&,const indel::PairHMM&,int parent,int child);
efloat_t prior_branch_from_counts(const ublas::matrix<int>& counts, const indel::PairHMM&);
efloat_t prior_HMM_nogiven(const data_partition&);
efloat_t prior_HMM(const data_partition&);
ublas::matrix<int> get_path_counts(const alignment& A,int node1, int node2);

//------------------- Priors ----------------------//
efloat_t topology_weight(const Parameters& P, const SequenceTree& T);

// Tree prior: branch lengths & topology
efloat_t prior_exponential(const SequenceTree& T,double branch_mean);

efloat_t prior_gamma(const SequenceTree& T,double branch_mean);

// Tree prior: branch lengths & topology
efloat_t prior(const Parameters& P, const SequenceTree& T,double branch_mean);
#endif
