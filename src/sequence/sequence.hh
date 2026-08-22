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
#include <vector>

struct sequence_info
{
    std::string name;
    std::string comment;
    sequence_info() {}

    explicit sequence_info(const std::string& n);

    explicit sequence_info(const std::string& n,const std::string& c);
};

struct sequence: public std::string, public sequence_info
{
    void strip_gaps();

    int seq_length() const;

    sequence() {}

    explicit sequence(const sequence_info&);

    explicit sequence(const std::string& n,const std::string& c);
};

bool operator==(const sequence&,const sequence&);

std::string guess_alphabet(const std::vector<sequence>& sequences);
std::string guess_alphabet(const std::string& name_, const std::vector<sequence>& sequences);

std::vector<sequence> select(const std::vector<sequence>& s,const std::vector<int>& columns);
std::vector<sequence> select(const std::vector<sequence>& s,const std::string& range);

void pad_to_same_length(std::vector<sequence>& s);
#endif
