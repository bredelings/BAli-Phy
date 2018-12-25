/*
   Copyright (C) 2010 Benjamin Redelings

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

/**
 * @file timer_stack.C
 */

#include "util/time.H"
#include <sstream>
#include <iomanip>
#include "util/assert.hh"
#include "util/util.H"

using namespace std;
using namespace boost::chrono;

duration_t total_cpu_time()
{
  return process_user_cpu_clock::now() - process_user_cpu_clock::time_point();
}

string duration_string(seconds t)
{
  unsigned long T = t.count();

  string s = convertToString(t);

  unsigned long seconds = T%60;
  T = (T - seconds)/60;

  // return if 0 minutes
  if (not T) return s;

  unsigned long minutes = T%60;
  T  = (T - minutes)/60;

  s = convertToString(minutes) + "m " +
      convertToString(seconds) + "s  (" + s + ")";

  // return if 0 hours
  if (not T) return s;

  unsigned long hours = T%24;
  T  = (T - hours)/24;

  s = convertToString(hours) + "h " + s;

  // return if 0 days
  if (not T) return s;

  unsigned long days = T;

  s = convertToString(days) + "days " + s;

  return s;
}

