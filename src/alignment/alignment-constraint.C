/*
   Copyright (C) 2004-2009 Benjamin Redelings

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

#include <sstream>
#include <iostream>
#include <fstream>
#include <utility>
#include "alignment/alignment-constraint.H"
#include "alignment/alignment-util.H"
#include "tree/tree-util.H"
#include "util.H"
#include "io.H"

using std::string;
using std::ifstream;
using std::vector;
using std::valarray;
using std::pair;

using boost::program_options::variables_map;
using boost::dynamic_bitset;

string clean(const string& in) {
  string out;
  char c=' ';
  for(int i=0;i<in.size();i++)
    if (in[i] != ' ' or c != ' ') {
      out += in[i];
      c = in[i];
    }

  // strip (single) final ' '
  if (out.size() > 0 and out[out.size()-1] == ' ')
    out = out.substr(0,out.size()-1);
  return out;
}
    
ublas::matrix<int> load_alignment_constraint(const string& filename,SequenceTree& T) 
{
  ublas::matrix<int> constraint(0,T.n_leaves());

  if (filename.size()) {
    // Load constraint file
    checked_ifstream constraint_file(filename,"alignment-constraint file");

    // Map columns to species
    string line;
    portable_getline(constraint_file,line);
    vector<string> names = split(clean(line),' ');
    vector<int> mapping;
    try {
      mapping = compute_mapping(names,T.get_leaf_labels());
    }
    catch (myexception& e) 
    {
      myexception error;
      error <<"Problem loading alignment constraints from file '" <<filename<<"':\n";

      // complain about the names;
      if (names.size() != T.n_leaves())
	error <<"Data set contains "<<T.n_leaves()<<" sequences but "
	  "alignment-constraint header has "<<names.size()<<" names.\n";

      for(int i=0;i<names.size();i++) {
	if (not includes(T.get_leaf_labels(), names[i]))
	  error<<"'"<<names[i]<<"' found in header but not data set.\n";
      }

      for(int i=0;i<T.n_leaves();i++) {
	if (not includes(names,T.get_label(i)))
	  error<<"'"<<T.get_label(i)<<"' found in data set but not in header.\n";
      }
      throw error;
    }

    // Load constraints
    vector<vector<int> > constraints;

    // We start on line 1
    int line_no=0;
    while(portable_getline(constraint_file,line)) 
    {
      line_no++;

      // Check for comment marker -- stop before it.
      int loc = line.find('#');
      if (loc == -1)
	loc = line.length();

      while (loc > 0 and line[loc-1] == ' ')
	loc--;

      line = line.substr(0,loc);

      if (not line.size()) continue;

      // lex contraint line
      vector<string> entries = split(clean(line),' ');
      if (entries.size() != T.n_leaves())
	throw myexception()<<"constraint: line "<<line_no<<
	  " only has "<<entries.size()<<"/"<<T.n_leaves()<<" entries.";

      // parse contraint line
      int n_characters = 0;
      vector<int> c_line(T.n_leaves());
      for(int i=0;i<entries.size();i++) {
	if (entries[i] == "-")
	  c_line[mapping[i]] = alphabet::gap;
	else {
	  int index = convertTo<int>(entries[i]);

	  if (index < 0)
	    throw myexception()<<"constraint: line "<<line_no<<
	      " has negative index '"<<index<<"' for species '"<<names[i]<<"' (entry "<<i+1<<").";

	  //FIXME - we should probably check that the index is less than the length of the sequence

	  c_line[mapping[i]] = index;

	  n_characters++;
	}
      }

      // Only add a constraint if we are "constraining" more than 1 character
      if (n_characters >= 2)
	constraints.push_back(c_line);
    }

    // load constraints into matrix
    constraint.resize(constraints.size(),T.n_leaves());
    for(int i=0;i<constraint.size1();i++)
      for(int j=0;j<constraint.size2();j++)
	constraint(i,j) = constraints[i][j];
  }

  return constraint;
}

bool constrained(const dynamic_bitset<>& group,const ublas::matrix<int>& constraint,int c) 
{
  bool present = false;
  for(int i=0;i<constraint.size2();i++)
    if (group[i] and constraint(c,i) != -1)
      present = true;
  return present;
}

// This procedure bases the constraint columns ENTIRELY on the leaf sequence alignment!
// Therefore these constrained columns may be unalignable, depending on the internal node
//  states!
vector<int> constraint_columns(const ublas::matrix<int>& constraint,const alignment& A) 
{
  // determine which constraints are satisfied, and can be enforced
  vector<int> columns(constraint.size1(),-1);

  // get columns for each residue
  vector<vector<int> > column_indices = column_lookup(A);

  // for all constraints (i,*) != -1, check...
  for(int i=0;i<constraint.size1();i++) 
    for(int j=0;j<constraint.size2();j++) 
      if (constraint(i,j) != -1) 
      {
	int c = column_indices[j][constraint(i,j)];

	if (columns[i] == -1)
	  columns[i] = c;
	else if (columns[i] != c) {
	  columns[i] = -1;
	  break;
	}
      }

  return columns;
}


// When the alignment is project onto 2 or 3 sequences, its columns are ordered 
// in a particular way so that only one path through the DP matrix corresponds to 
// each alignment.
// 
// seq1 is a list of columns that contain the x-ordinate sequence(s)
// seq2 is a list of columns that contain the y-coordinate sequence(s)
// seq12 is a list of columns that occur in either of the two sequences, and represents
//      the current path through the DP matrix.

// This routine moves a square at [x-D,x+D]*[y-D,y+D] along the points x,y in seq12 to
// find the leftmost and rightmost x-coordinates for each y.  As y increases, these
// two x-coordinates may not decrease.

// Question.  Is the (0,0) square actually at (1,1) in the matrix?
// Answer. Yes, but we are going to store ranges in the unadjusted (0,0) coordinates.

vector< pair<int,int> > get_x_ranges_for_band(int D, const vector<int>& seq1, const vector<int>& seq2, 
					      const vector<int>& seq12)
{
  // The DP matrix has size (W+1)*(H+1) and is [0,W]x[0,H]
  int W = seq1.size();
  int H = seq2.size();

  int x = 0;
  int y = 0;

  // we'll compute the first and last indices, instead of first and last+1
  vector< pair<int,int> > xboundaries(H+1, pair<int,int>(0,W));

  if (D<0) return xboundaries;

  // Determine xmin[y]
  for(int x=0,y=0,k=0;k<seq12.size();k++)
  {
    if (x<seq1.size() and seq1[x] == seq12[k])
      x++;
    if (y<seq2.size() and seq2[y] == seq12[k])
    {
      y++;

      // Avoid setting an xmin not in [0,W]
      if (x-D < 0) continue;

      // If the top of the square is out of the DP matrix then we are done.
      if (y+D > H) break;

      // Set the xmax for row y+D
      xboundaries[y+D].first = x-D;
    }
  }

  // Determine xmax[y]
  for(int x=W,y=H,k=seq12.size()-1;k>=0;k--)
  {
    if (x>0 and seq1[x-1] == seq12[k])
      x--;
    if (y>0 and seq2[y-1] == seq12[k])
    {
      y--;

      // Avoid setting an xmin not in [0,W]
      if (x+D > W) continue;

      // If the top of the square is out of the DP matrix then we are done
      if (y-D < 0) break;

      // Set the xmax for row y-D
      xboundaries[y-D].second = x+D;
    }
  }

  return xboundaries;
}

vector< pair<int,int> > get_y_ranges_for_band(int D, const vector<int>& seq1, const vector<int>& seq2, 
					      const vector<int>& seq12)
{
  return get_x_ranges_for_band(D, seq1, seq2, seq12);
}

// We need to make sure that the pinned column coordinates always increase.
// By considering constraints between seq1 and seq2 in the order of seq12 we can guarantee this,
//  or bail out if it is impossible.

vector< vector<int> > get_pins(const ublas::matrix<int>& constraint,const alignment& A,
			       const dynamic_bitset<>& group1,const dynamic_bitset<>& group2,
			       const vector<int>& seq1,const vector<int>& seq2)
{
  // determine which constraints are satisfied (not necessarily enforceable!)
  vector<int> satisfied = constraint_columns(constraint,A);

  // ignore columns in which all constrained residues are in either group1 or group2
  // we cannot enforce these constraints, and also cannot affect them
  for(int i=0;i<satisfied.size();i++) 
    if (not (constrained(group1,constraint,i) and constrained(group2,constraint,i)))
      satisfied[i] = -1;

  // Mark and check each alignment column which is going to get pinned.
  vector<int> pinned(A.length(),0);
  for(int i=0;i<satisfied.size();i++) 
  {
    int column = satisfied[i];

    if (column != -1 and not pinned[column]) 
    {
      pinned[column] = 1;
    
      int x = find_index(seq1,column);
      int y = find_index(seq2,column);

      // Even if the constraints for the leaf nodes are satisfied, we can't align
      // to the relevant leaf characters THROUGH the relevant internal nodes, if the
      // character is not present at the internal nodes that we have
      // access to.  Therefore, no alignment that we choose can satisfy
      // this constraint, so we must bail out.
      if (x == -1 or y == -1) {
	vector< vector<int> > impossible;
	impossible.push_back(vector<int>(2,-1));
	return impossible;
      }    
    }
  }

  // Go through the pinned columns in seq12 order to guarantee that x and y always increase
  vector<vector<int> > pins(2);
  vector<int>& X = pins[0];
  vector<int>& Y = pins[1];

  /* TODO: Rewrite this to go through the pinned columns in order.
           The order of x-columns with constraints must match the y-order.
	   If two constraints occur in the same x-column, the same two constraints must occur in the y-column.
	   I guess I could make a set<int> for each x-column and each y-column.
	   I could then go through and find matching columns. */

  /*
  for(int i=0;i<seq12.size();i++)
  {
    int column = seq12[i];

    if (not pinned[column]) continue;
    
    int x = find_index(seq1,column);
    int y = find_index(seq2,column);

    assert(x >=0 and x < seq1.size());
    assert(y >=0 and y < seq2.size());

    if (x == -1 or y == -1)
      throw myexception()<<"Did not already bail out on un-pinnable column?!?";

    X.push_back(x+1);
    Y.push_back(y+1);

    if (X.size() >= 2 and X[pins.size()-2] > X[pins.size()-1])
      throw myexception()<<"X pins not always increasing!";
    if (Y.size() >= 2 and Y[pins.size()-2] > Y[pins.size()-1])
      throw myexception()<<"X pins not always increasing!";
  }
  */
  return pins;
}


dynamic_bitset<> constraint_satisfied(const ublas::matrix<int>& constraint,const alignment& A) 
{
  vector<int> columns = constraint_columns(constraint,A);

  dynamic_bitset<> satisfied(columns.size());
  for(int i=0;i<satisfied.size();i++)
    satisfied[i] = columns[i] != -1;

  return satisfied;
}

namespace {
int sum(const dynamic_bitset<>& v) {
  int count = 0;
  for(int i=0;i<v.size();i++)
    if (v[i]) count++;
  return count;
}
}

void report_constraints(const dynamic_bitset<>& s1, const dynamic_bitset<>& s2, int p) 
{
  p++;
  assert(s1.size() == s2.size());

  if (not s1.size()) return;

  for(int i=0;i<s1.size();i++) {
    if (s1[i] and not s2[i])
      throw myexception()<<"Partition "<<p<<": constraint "<<i<<" went from satisfied -> unsatisfied!";
    if (s2[i] and not s1[i])
      std::cout<<"Partition "<<p<<": constraint "<<i<<" satisfied."<<std::endl;
  }

  if (sum(s1) != sum(s2)) {
    std::cout<<"Partition "<<p<<": "<<sum(s2)<<"/"<<s2.size()<<" constraints satisfied.\n";

    if (sum(s2) == s2.size())
      std::cout<<"Partition "<<p<<": All constraints satisfied!"<<std::endl;
  }
}

bool any_branches_constrained(const vector<int>& branches,const SequenceTree& T,const SequenceTree& TC, const vector<int>& AC)
{
  if (AC.size() == 0)
    return false;

  vector<int> c_branches = compose(AC,extends_map(T,TC));

  for(int i=0;i<branches.size();i++)
    if (includes(c_branches,branches[i]))
      return true;

  return false;
}

vector< pair<int,int> > get_yboundaries_from_pins(int I, int J, const vector< vector<int> >& pins)
{
  vector< pair<int,int> > yboundaries(I, pair<int,int>(0,J-1));

  const vector<int>& x = pins[0];
  const vector<int>& y = pins[1];

  if (x.size() != 0)
  {
    for(int i=0;i<x[0];i++)
      yboundaries[i] = pair<int,int>(0, y[0]-1);

    for(int k=0;k+1<(int)x.size();k++)
      for(int i=x[k];i<x[k+1];i++)
	yboundaries[i] = pair<int,int>(y[k], y[k+1]-1);

    for(int i=x.back();i<I;i++)
      yboundaries[i] = pair<int,int>(y.back(), J-1);
  }

  return yboundaries;
}

vector< pair<int,int> > boundaries_intersection(const vector< pair<int,int> >& boundaries1,const vector< pair<int,int> >& boundaries2)
{
  assert(boundaries1.size());

  assert(boundaries1.size() == boundaries2.size());

  assert(boundaries1[0].first == 0);
  assert(boundaries2[0].first == 0);

  int L1 = boundaries1.size()-1;

  assert(boundaries1.back().second == boundaries2.back().second);
  int L2 = boundaries1.back().second;

  vector< pair<int,int> > boundaries3 = boundaries1;

  for(int i=0; i<boundaries3.size();i++)
  {
    boundaries3[i].first = std::max(boundaries3[i].first, boundaries2[i].first);
    boundaries3[i].second = std::min(boundaries3[i].second, boundaries2[i].second);
    assert(boundaries3[i].second >= boundaries3[i].first);
  }

  return boundaries3;
}
