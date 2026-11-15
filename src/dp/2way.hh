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

#ifndef TWOWAY_H
#define TWOWAY_H

#include <vector>
#include "hmm.hh"

class TreeInterface;
class data_partition;

// Two branch alignments joined through an unobserved degree-two node.
namespace A2 {
    std::vector<int> get_nodes_branch(const TreeInterface& t, int middle, int left);
    std::vector<HMM::bitmask_t> get_bitpath(const data_partition& P, const std::vector<int>& nodes);
}

#endif
