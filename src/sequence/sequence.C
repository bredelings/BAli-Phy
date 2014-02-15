/*
   Copyright (C) 2004-2005,2008 Benjamin Redelings

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
#include "sequence.H"
#include "myexception.H"
#include "util.H"

using namespace std;


sequence_info::sequence_info(const string& n)
  :name(n)
{}

sequence_info::sequence_info(const string& n,const string& c)
  :name(n),comment(c) 
{}

void sequence::strip_gaps() {
  string ungapped;

  for(int i=0;i<size();i++) {
    char c = (*this)[i];

    // FIXME - this hardcodes the - and ? characters...
    if (c != '-' and c != '?')
      ungapped += c;
  }
  string::operator=(ungapped);
}

sequence::sequence(const sequence_info& si)
  :sequence_info(si)
{}

sequence::sequence(const string& n,const string& c)
  :sequence_info(n,c)
{}

bool operator==(const sequence& s1,const sequence& s2) {
  return s1.name == s2.name and
    (string&)s1 == (string&)s2;
}
