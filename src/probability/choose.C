/*
   Copyright (C) 2004-2005 Benjamin Redelings

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

#include <cassert>
#include "probability/choose.H"
#include "rng.H"
#include "math/logsum.H"
#include "myexception.H"

using std::vector;

int choose2(log_double_t x, log_double_t y) 
{
  std::vector<log_double_t> Pr(2);
  Pr[0] = x;
  Pr[1] = y;

  return choose_scratch(Pr);
}

template <> choose_exception<log_double_t>::choose_exception(int i, const std::vector<log_double_t>& V)
  :Pr(V)
{
  (*this)<<"No option chosen! (current = "<<i<<")\n";
  for(int j=0;j<Pr.size();j++) {
    if (i == j) (*this)<<"*";
    (*this)<<"log(Pr["<<j<<"]) = "<<Pr[j].log()<<"\n";
  }
  (*this)<<show_stack_trace();
  //    std::abort();
}

