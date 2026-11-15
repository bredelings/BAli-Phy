/*
   Copyright (C) 2004-2007,2009-2010 Benjamin Redelings

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

#include "2way.hh"
#include "1way.hh"
#include "models/parameters.hh"
#include "util/assert.hh"

namespace A2 {

// Order the path as {middle, left, right}, preserving the chosen left neighbor.
std::vector<int> get_nodes_branch(const TreeInterface& t, int middle, int left)
{
    assert(t.degree(middle) == 2);
    assert(t.is_connected(middle, left));
    auto neighbors = t.neighbors(middle);
    int right = neighbors[0] == left ? neighbors[1] : neighbors[0];
    return {middle, left, right};
}

// Use the same orientation and canonical interleaving as Glue in the A2 sampler.
std::vector<HMM::bitmask_t> get_bitpath(const data_partition& P, const std::vector<int>& nodes)
{
    assert(nodes.size() == 3);
    auto t = P.t();
    int b1 = t.find_branch(nodes[1], nodes[0]);
    int b2 = t.find_branch(nodes[0], nodes[2]);
    auto a1 = convert_to_bits(P.get_pairwise_alignment(b1), 0, 2);
    auto a2 = convert_to_bits(P.get_pairwise_alignment(b2), 2, 1);
    return Glue_A(a1, a2);
}

}
