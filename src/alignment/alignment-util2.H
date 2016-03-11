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

#include <vector>
#include <boost/dynamic_bitset.hpp>

#include "alignment.H"
#include "sequence/alphabet.H"
#include "models/TreeInterface.H"

bool all_characters_connected(const TreeInterface& t,boost::dynamic_bitset<> present,const std::vector<int>& ignore);

void minimally_connect_leaf_characters(alignment& A,const TreeInterface& t);

bool check_leaf_characters_minimally_connected(const alignment& A,const TreeInterface& t);

#endif
