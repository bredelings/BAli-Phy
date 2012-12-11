/*
   Copyright (C) 2012 Benjamin Redelings

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
 * @file object.C
 *
 * @brief This file contains a number of utility routines.
 *
 */

#include "object.H"
#include "myexception.H"
#include "computation/expression.H"

using std::string;

object_ref parse_object(const string& s)
{
  bool bool_value;
  int int_value;
  double double_value;

  if (can_be_converted_to<int>(s, int_value))
    return Int(int_value);
  else if (can_be_converted_to<double>(s, double_value))
    return Double(double_value);
  else if (can_be_converted_to<bool>(s, bool_value))
  {
    if (bool_value)
      return constructor("True",0);
    else
      return constructor("False",0);
  }
  else if (s.size() >= 2 and s[0] == '"' and s[s.size()-1] == '"')
    return String(s.substr(1,s.size()-2));
  else
    throw myexception()<<"Can't convert '"<<s<<"' to Bool, Int, Double, or String!";
}
