/*
  Copyright (C) 2004-2005,2009 Benjamin Redelings

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

#ifndef TWOWAY_H
#define TWOWAY_H

#include <vector>
#include <bitset>
#include "alignment/alignment.H"
#include "tree/tree.H"
#include "object.H"
#include "matrix.H"
#include "models/TreeInterface.H"
#include "hmm.H"

namespace A2
{
    namespace states {
	constexpr int M  = 0;
	constexpr int G1 = 1;
	constexpr int G2 = 2;
	constexpr int E  = 3;
	constexpr int S  = 4;
    }
}

class pairwise_alignment_t: public Object
{
    std::vector<int> states_;

public:
    pairwise_alignment_t* clone()  const {return new pairwise_alignment_t(*this);}

    void flip();
    pairwise_alignment_t flipped() const;

    int length1() const;
    int length2() const;

    int count(int s) const;
    int count_match() const {return count(A2::states::M);};
    int count_insert() const {return count(A2::states::G1);};
    int count_delete() const {return count(A2::states::G2);};

    int size() const {return states_.size();};
    void resize(int s) {states_.resize(s);};
    void reserve(int s) {states_.reserve(s);};
    void push_back(int state) {states_.push_back(state);};

    bool is_match(int i) const {return states_[i] == A2::states::M;};
    bool is_insert(int i) const {return states_[i] == A2::states::G1;};
    bool is_delete(int i) const {return states_[i] == A2::states::G2;};

    int operator[](int i) const {return states_[i];};
    void set_state(int i, int S) {states_[i] = S;};
    void set_match(int i) {states_[i] = A2::states::M;};
    void set_insert(int i) {states_[i] = A2::states::G1;};
    void set_delete(int i) {states_[i] = A2::states::G2;};

    explicit pairwise_alignment_t(const std::vector<int>&);
    pairwise_alignment_t(const pairwise_alignment_t&) = default;
    pairwise_alignment_t();
};

bool operator==(const pairwise_alignment_t&, const pairwise_alignment_t&);

std::bitset<64> convert_to_bits(int, int, int);
std::vector<std::bitset<64>> convert_to_bits(const pairwise_alignment_t&, int, int );

matrix<int> construct(const TreeInterface& t, const std::vector<pairwise_alignment_t>& A);

alignment get_alignment(const alphabet& a, const std::vector<sequence>&, const std::vector< std::vector<int>>& sequences, const matrix<int>& M);

alignment get_alignment(const alignment& A1, const std::vector< std::vector<int>>& sequences, const matrix<int>& M);

namespace A2 {

    int flip(int s);

    pairwise_alignment_t get_pairwise_alignment(const alignment& A, int n1, int n2);

    pairwise_alignment_t get_pairwise_alignment(const matrix<int>& A, int n1, int n2);

    pairwise_alignment_t get_pairwise_alignment_from_path(const std::vector<int>& path);
}

pairwise_alignment_t get_pairwise_alignment_from_bits(const std::vector<HMM::bitmask_t>& bit_path, int n1, int n2);

pairwise_alignment_t get_pairwise_alignment_from_path(const std::vector<int>& path, const HMM&, int n1, int n2);

int n_indels(const pairwise_alignment_t& a);

int total_length_indels(const pairwise_alignment_t& a);

pairwise_alignment_t make_unaligned_pairwise_alignment(int L1, int L2);
#endif
