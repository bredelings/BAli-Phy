/*
   Copyright (C) 2004-2006,2010 Benjamin Redelings

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

///
/// \file hmm.H
///
/// \brief This file defines the generic HMM class.
///

#ifndef HMM_H
#define HMM_H

#include "mytypes.H"
#include "math/logsum.H"
#include <bitset>
#include "2way.H"

// Can I make Q into a general function, backed by a matrix in some cases?

namespace indel { class PairHMM; }

/// A Hidden Markov Model - represented by the transition matrix and accessory functions
class HMM {
public:
  typedef std::bitset<64> bitmask_t;

  /// bitmasks of whether states emit features in each sequence
  std::vector<bitmask_t> state_emit;

  /// Which state is the start state?
  int start = -1;

  /// Which state is the end state?
  int end;

  /// Probabilities of starting in each state
  std::vector<double> start_P;

  /// The transition matrix for the HMM
  Matrix Q;

  /// The inverse temperature parameter - it is applied only to emission likelihoods.
  double B;

  /// Which bits are not considered emitted?
  bitmask_t hidden_bits;

  /// Is state S a silent state?
  bool silent(int S) const {return (state_emit[S]&~hidden_bits).none();}

  bool connected_Q(int S1,int S2) const {return Q(S1,S2) != 0.0;}

  bitmask_t all_bits() const;

  int n_characters() const;

  void remap_bits(const std::vector<int>& map);

  /// Number of states, minus the end state
  int n_states() const {return state_emit.size();}

  /// The index of the start state
  int startstate() const {return start;}

  /// The index of the end state
  int endstate() const {return end;}

  /// Calculates the (log) product of each move, according to the generalized HMM
  efloat_t path_Q_path(const std::vector<int>& path) const;

  HMM() = default;

  HMM(const indel::PairHMM&);

  /// Construct an HMM from the states emit bitmasks, the start probabilities, and the transition matrix
  HMM(const std::vector<bitmask_t>&,const std::vector<double>&,const Matrix&,double);

  /// Construct an HMM from the states emit bitmasks, the start and end states, the start probabilities, and the transition matrix
  HMM(const std::vector<bitmask_t>&, int, int, const std::vector<double>&,const Matrix&,double);

  virtual ~HMM() {}
};

int bitlength(const std::vector<HMM::bitmask_t>& bits, int b);

int bitslength(const std::vector<HMM::bitmask_t>& bits, HMM::bitmask_t m);

std::vector<HMM::bitmask_t> remove_silent(const std::vector<HMM::bitmask_t>& bits, HMM::bitmask_t m);

HMM Glue(const HMM& top, const HMM& bottom);

std::vector<HMM::bitmask_t> Glue_A(const std::vector<HMM::bitmask_t>&, const std::vector<HMM::bitmask_t>&);

std::vector<HMM::bitmask_t> get_bits_from_path(const std::vector<int>& path, const HMM&);

pairwise_alignment_t get_pairwise_alignment_from_bits(const std::vector<HMM::bitmask_t>& bit_path, int n1, int n2);

pairwise_alignment_t get_pairwise_alignment_from_path(const std::vector<int>& path, const HMM&, int n1, int n2);

std::vector<int> get_path_unique(const std::vector<HMM::bitmask_t>& bit_path, const HMM& H);
#endif
