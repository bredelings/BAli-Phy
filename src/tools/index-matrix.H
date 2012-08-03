/*
   Copyright (C) 2007-2009 Benjamin Redelings

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

#include <map>
#include <set>
#include <vector>
#include "mytypes.H"
#include "alignment/alignment.H"

struct Edge 
{
  int s1;
  int x1;

  int s2;
  int x2;

  unsigned count;
  double p;
};

struct edge_comp {
  bool operator()(const Edge& E1, const Edge& E2) {
    return E1.p  > E2.p;
  }
};

class Edges: public std::multiset<Edge,edge_comp>
{
  std::vector<std::vector<std::vector<std::multiset<Edge,edge_comp>::iterator> > > lookup;

public:
  void build_index();

  double PP(int s1,int x1,int s2,int x2) const;
  int index_in_sequence(int s1,int x1,int s2) const;

  Edges(const std::vector<int>& L);
};

void add_edges(Edges& E, const std::vector< ublas::matrix<int> >& Ms,
	       int s1,int s2,int L1, int L2,double cutoff);

class index_matrix: public ublas::matrix<int> 
{
  std::vector<std::vector<int> > column_index;

public:

  void check_column_indices() const;

  int columns;
  int unknowns;

  int  index(int i,int j) const {return (*this)(i,j);}
  int& index(int i,int j)       {return (*this)(i,j);}

  int  column(int i,int j) const {assert(j>=0); return column_index[i][j];}
  int& column(int i,int j)       {assert(j>=0); return column_index[i][j];}

  int length(int i) const {return column_index[i].size();}

  bool columns_conflict(int c1, int c2) const;

  bool consistent(int c1, int s,int x, const Edges& E,double cutoff) const;
  bool consistent(int c1, int c2,const Edges& E, double cutoff) const;

  void merge_columns(int c1,int c2);
  std::map<unsigned,std::pair<unsigned,unsigned> > merge(const Edges& E,double p,bool strict);

  std::map<unsigned,std::pair<unsigned,unsigned> > merge2(const Edges& E,double p,bool strict);

  unsigned n_unknown() const;

  unsigned n_columns() const;

  index_matrix(int C, const std::vector<int>& L)
    :ublas::matrix<int>(C,L.size()),columns(C),unknowns(0)
  {
    for(int i=0;i<L.size();i++)
      column_index.push_back(std::vector<int>(L[i]));
  }

  index_matrix(const alignment& A);
};

index_matrix unaligned_matrix(const std::vector<int>& L);

unsigned count_unknowns(const index_matrix& M,int c);

ublas::matrix<int> get_ordered_matrix(const index_matrix& M);

alignment get_alignment(const ublas::matrix<int>& M, const alignment& A);

alignment get_ordered_alignment(const alignment& A);

