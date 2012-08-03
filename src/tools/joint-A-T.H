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

#ifndef JOINT_A_T_H
#define JOINT_A_T_H

#include <string>
#include <vector>
#include <boost/program_options.hpp>

#include "sequencetree.H"
#include "alignment/alignment.H"


//--------------------------- Class joint_A_T ----------------------------//
struct joint_A_T 
{
  int size() {return A.size();}

  std::vector<alignment> A;
  std::vector<SequenceTree> T;

  joint_A_T(const std::vector<alignment>& A1,const std::vector<SequenceTree>& T1,bool internal);
};

joint_A_T get_joint_A_T(const boost::program_options::variables_map& args,bool internal);


#endif
