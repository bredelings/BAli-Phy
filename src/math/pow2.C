/*
   Copyright (C) 2005 Benjamin Redelings

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

#include "math/pow2.H"

namespace fp_scale {

  double table[max*2+1];


  void initialize() {
    table[shift] = 1.0;
    for(int i=0;i<max;i++) {
      table[shift+i+1] = table[shift+i] * 2.0;
      table[shift-i-1] = table[shift-i] * 0.5;
    }
  }

}
