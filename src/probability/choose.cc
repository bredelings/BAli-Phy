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

#include "util/assert.hh"
#include "probability/choose.H"
#include "util/rng.H"
#include "util/myexception.H"

using std::vector;

int choose2(log_double_t x, log_double_t y) 
{
    auto sum0 = x;
    auto sum1 = x + y;

    log_double_t r = log_double_t(uniform()) * sum1;

    if (r < sum0)
        return 0;
    if (r < sum1)
        return 1;

    choose_exception<log_double_t> c(0, {x,y});
    c.prepend(":\n");
    c.prepend(__PRETTY_FUNCTION__);
    throw c;
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

