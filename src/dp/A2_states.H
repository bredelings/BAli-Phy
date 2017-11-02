/*
  Copyright (C) 2017 Benjamin Redelings

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

#ifndef A2_states_H
#define A2_states_H

namespace A2
{
    namespace states {
	constexpr int M  = 0;
	constexpr int G1 = 1;
	constexpr int G2 = 2;
	constexpr int E  = 3;
	constexpr int S  = 4;
    }

    inline int flip(int s) 
    {
	if (s == states::G1)
	    return states::G2;
	else if (s == states::G2) 
	    return states::G1;
	else return s;
    }
}
#endif
