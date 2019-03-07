/*
   Copyright (C) 2005,2008,2010 Benjamin Redelings

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

#include "joint-A-T.H"

#include <fstream>
#include <cmath>

#include "alignment/alignment.H"
#include "alignment/load.H"
#include "tree-align/link.H"
#include "tree/tree-util.H"

#include "util/myexception.H"
#include "util/io.H"

namespace po = boost::program_options;
using po::variables_map;

using std::vector;
using std::string;
using std::endl;

const std::vector<std::string>& joint_A_T::leaf_names() const
{
    return leaf_names_;
}

joint_A_T::joint_A_T(const vector<alignment>& A1,const vector<SequenceTree>& T1,bool internal)
  :A(A1),T(T1)
{
  unsigned s = std::min(A.size(),T.size());
  if (s != A.size())
    std::cerr<<"joint-A-T: Warning! only using "<<s<<"/"<<A.size()<<" alignments to match number of trees."<<endl;

  if (s != T.size())
    std::cerr<<"joint-A-T: Warning! only using "<<s<<"/"<<T.size()<<" trees to match number of alignments."<<endl;

  A.resize(s);
  T.resize(s);

  if (s == 0) return;

  leaf_names_ = T1[0].get_leaf_labels();
  
  for(int i=0;i<size();i++)
  {
    remap_T_leaf_indices(T[i], leaf_names_);
    link(A[i],T[i],true);
    link(A[i],T[i],internal);
  }
}


joint_A_T get_joint_A_T(const variables_map& args,bool internal)
{
  checked_ifstream a_file(args["alignments"].as<string>(), "alignment samples file");

  checked_ifstream t_file(args["trees"].as<string>(), "tree samples file");

  unsigned subsample = args["subsample"].as<unsigned>();

  vector<alignment> A = load_alignments(a_file, get_alphabet_name(args));
  vector<SequenceTree> T = load_trees(t_file, 0, subsample);

  return joint_A_T(A,T,internal);
}
