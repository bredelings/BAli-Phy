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

#include "util/assert.hh"          // for assert
#include <algorithm>               // for max
#include "util/bitmask.H"          // for bitmask
#include <vector>                  // for vector
#include "A2_states.H"             // for G1, G2, M, E
#include "alignment/alignment.H"   // for alignment
#include "hmm.H"                   // for HMM, HMM::bitmask_t
#include "models/TreeInterface.H"  // for TreeInterface
#include "sequence/alphabet.H"     // for alphabet
#include "util/matrix.H"           // for matrix
struct sequence;

class pairwise_alignment_t
{
    std::vector<char> states_;

public:
    void flip();
    pairwise_alignment_t flipped() const;

    int length1() const;
    int length2() const;

    int count(int s) const;
    int count_match() const {return count(A2::states::M);};
    int count_insert() const {return count(A2::states::G1);};
    int count_delete() const {return count(A2::states::G2);};
    int count_indels() const;

    int size() const {return states_.size();};
    void resize(int s) {states_.resize(s);};
    void reserve(int s) {states_.reserve(s);};

    void push_back(char state) { assert(state != A2::states::E); states_.push_back(state);};
    void push_back_match() {states_.push_back(A2::states::M);};
    void push_back_insert() {states_.push_back(A2::states::G1);};
    void push_back_delete() {states_.push_back(A2::states::G2);};
    bool push_back(bool c1, bool c2) {
	if (not c1 and not c2) return false;

	if (c1 and c2)
	    push_back_match();
	else if (c1)
	    push_back_delete();
	else if (c2)
	    push_back_insert();

	return true;
    }

    bool is_match(int i) const {return states_[i] == A2::states::M;};
    bool is_insert(int i) const {return states_[i] == A2::states::G1;};
    bool is_delete(int i) const {return states_[i] == A2::states::G2;};

    bool has_character1(int i) const {return not is_insert(i);}
    bool has_character2(int i) const {return not is_delete(i);}

    char  get_state(int i) const {return states_[i];};
    void set_state(int i, char S) {states_[i] = S;};
    void set_match(int i) {states_[i] = A2::states::M;};
    void set_insert(int i) {states_[i] = A2::states::G1;};
    void set_delete(int i) {states_[i] = A2::states::G2;};

    pairwise_alignment_t& operator=(const pairwise_alignment_t&) = default;

    explicit pairwise_alignment_t(const std::vector<int>&);
    pairwise_alignment_t(const pairwise_alignment_t&) = default;
    pairwise_alignment_t();
};

bool operator==(const pairwise_alignment_t&, const pairwise_alignment_t&);

bitmask_8 convert_to_bits(int, int, int);
std::vector<bitmask_8> convert_to_bits(const pairwise_alignment_t&, int, int );

object_ptr<Box<std::map<int,expression_ref>>> construct2(const expression_ref& a);

matrix<int> construct(const TreeInterface& t, const std::vector<pairwise_alignment_t>& A);

alignment get_alignment(const alphabet& a, const std::vector<sequence>&, const std::vector< std::vector<int>>& sequences, const matrix<int>& M);

alignment get_alignment(const alignment& A1, const std::vector< std::vector<int>>& sequences, const matrix<int>& M);

namespace A2 {

    pairwise_alignment_t get_pairwise_alignment(const alignment& A, int n1, int n2);

    pairwise_alignment_t get_pairwise_alignment(const matrix<int>& A, int n1, int n2);

    pairwise_alignment_t get_pairwise_alignment_from_path(const std::vector<int>& path);

    std::vector<int> get_path_from_pairwise_alignment(const pairwise_alignment_t& A);
}

pairwise_alignment_t get_pairwise_alignment_from_bits(const std::vector<HMM::bitmask_t>& bit_path, int n1, int n2);

pairwise_alignment_t get_pairwise_alignment_from_path(const std::vector<int>& path, const HMM&, int n1, int n2);

int n_indels(const pairwise_alignment_t& a);

int total_length_indels(const pairwise_alignment_t& a);

pairwise_alignment_t make_unaligned_pairwise_alignment(int L1, int L2);
pairwise_alignment_t make_left_aligned_pairwise_alignment(int L1, int L2);
#endif
