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

///
/// \file alignment-util.H
///
/// \brief This file implements alignment utility functions.
///

#ifndef ALIGNMENT_UTIL2_H
#define ALIGNMENT_UTIL2_H

#include <vector>                  // for vector
#include "models/TreeInterface.H"  // for TreeInterface
class alignment;

void connect_leaf_characters(alignment& A,const TreeInterface& T);
void minimally_connect_leaf_characters(alignment& A,const TreeInterface& t);

bool check_leaf_characters_minimally_connected(const alignment& A,const TreeInterface& t);

/// Check that internal node states are consistent
void check_internal_nodes_connected(const alignment& A,const TreeInterface& t,const std::vector<int>& ignore = std::vector<int>());

void check_alignment(const alignment& A,const TreeInterface& t,bool internal_sequences);

// Add sequences to A corresponding to internal nodes in T
alignment add_internal(alignment A, const std::vector<std::string>& labels, const TreeInterface& T);

#endif
