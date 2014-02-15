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

#ifndef SEQUENCE_H
#define SEQUENCE_H
#include <string>

struct sequence_info
{
  std::string name;
  std::string comment;
  sequence_info() {}

  sequence_info(const std::string& n);

  sequence_info(const std::string& n,const std::string& c);
};

struct sequence: public std::string, public sequence_info
{
  void strip_gaps();

  sequence() {}

  sequence(const sequence_info&);

  sequence(const std::string& n,const std::string& c);
};

bool operator==(const sequence&,const sequence&);
#endif
