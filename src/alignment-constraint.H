/*
   Copyright (C) 2004-2007,2009 Benjamin Redelings

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

#include <boost/program_options.hpp>
#include <boost/dynamic_bitset.hpp>
#include <utility>
#include "alignment/alignment.H"
#include "sequencetree.H"
#include "io.H"

ublas::matrix<int> load_alignment_constraint(const std::string& filename,SequenceTree& T);

std::vector< std::vector<int> > get_pins(const ublas::matrix<int>& constraint,const alignment& A,
					 const boost::dynamic_bitset<>& group1,const boost::dynamic_bitset<>& group2,
					 const std::vector<int>& seq1,const std::vector<int>& seq2,
					 const std::vector<int>& seq12);

boost::dynamic_bitset<> constraint_satisfied(const ublas::matrix<int>& constraint,const alignment& A);

void report_constraints(const boost::dynamic_bitset<>& s1, const boost::dynamic_bitset<>& s2, int p);

bool any_branches_constrained(const std::vector<int>&,const SequenceTree& T,const SequenceTree& TC, const std::vector<int>&);

std::vector< std::pair<int,int> > get_x_ranges_for_band(int D, 
							const std::vector<int>& seq1, 
							const std::vector<int>& seq2, 
							const std::vector<int>& seq12);
std::vector< std::pair<int,int> > get_y_ranges_for_band(int D, 
							const std::vector<int>& seq1, 
							const std::vector<int>& seq2, 
							const std::vector<int>& seq12);

std::vector< std::pair<int,int> > get_yboundaries_from_pins(int I, int J, const std::vector< std::vector<int> >& pins);

std::vector< std::pair<int,int> > boundaries_intersection(const std::vector< std::pair<int,int> >&,const std::vector< std::pair<int,int> >&);
