/*
  Copyright (C) 2005,2019 Benjamin Redelings

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

#ifndef JOINT_A_T_H
#define JOINT_A_T_H

#include <string>
#include <vector>
#include <optional>
#include <boost/program_options.hpp>
#include "util/file-readers.H"

#include "tree/sequencetree.H"
#include "alignment/alignment.H"

// This is like read-trees.H.  Maybe we should use range-v3...

class alignment_reader: public file_reader<std::string>
{
public:
    std::optional<std::string> next_one();

    alignment_reader(std::istream&);
};


//--------------------------- Class joint_A_T ----------------------------//
class joint_A_T: public std::vector<std::pair<alignment,SequenceTree>>
{
    std::optional<std::vector<std::string>> leaf_names_;

public:

    const std::vector<std::string>& leaf_names() const;

    const alignment& A(int i) {return (*this)[i].first;}
    const SequenceTree& T(int i) {return (*this)[i].second;}
    joint_A_T& load(const std::vector<alignment>& A1,const std::vector<SequenceTree>& T1,bool internal);

    joint_A_T() {};
    joint_A_T(const std::vector<alignment>& A1,const std::vector<SequenceTree>& T1,bool internal);
};

joint_A_T get_joint_A_T(const boost::program_options::variables_map& args,bool internal);

joint_A_T get_multiple_joint_A_T(const boost::program_options::variables_map& args,bool internal);

#endif
