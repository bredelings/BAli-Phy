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
#include "alignment/alignment.H"

/*
 * OK, so we want to avoid storing the entire matrix, including gaps.
 * And we also want to be able to modify the matrix.
 *
 * We could store the entire graph, in which graph vertices replace columns.
 * We don't really need to store edges though, unless we are doing graph operations,
 *  since the in-edge are just the columns for all the previous letters.
 *
 * We would need to actually make the graph in order to write out a matrix though -- that's the sort.
 * What is this "better algorithm" in the FSA paper?
 *
 * In order to access cells as a matrix, we need to be able to look up the index by (seq,column).
 */

// TODO: Unify with sparse_increasing_index_matrix in alignment.{H,cc}?
class sparse_index_matrix
{
    // The sequence -> index -> column map.
    std::vector<std::vector<int> > column_index;

    // The column -> sequence -> index map.
    std::map<int, std::map<int,int>> letters_for_column_;
    
    int next_column = 0;
public:

    int column(int seq, int index) const
    {
        assert(seq >= 0);
        assert(index >= 0);
        return column_index[seq][index];
    }

    const std::map<int, std::map<int,int>>& letters_for_column() const
    {
        return letters_for_column_;
    }

    std::map<int, std::map<int,int>>& letters_for_column()
    {
        return letters_for_column_;
    }

    const std::map<int,int>& letters_for_column(int col) const
    {
        return letters_for_column_.at(col);
    }

    void add_sequence();
    void extend_sequence(int seq);
    void add(int seq, int index, int col);
    int erase(int seq, int index);
    int new_column() {return next_column++;}

    int seqlength(int seq) const {return column_index[seq].size();}

    void check_column_indices() const;

    int n_sequences() const {return column_index.size();}

    int n_columns() const {return letters_for_column_.size();}

    sparse_index_matrix(const matrix<int>& M);
    sparse_index_matrix(int n);
};

/// Replace each letter with its position in its sequence
matrix<int> M(const alignment& A1);
sparse_index_matrix SM(const alignment& A1);

/// Structures for alignment-consensus.

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
    bool operator()(const Edge& E1, const Edge& E2) const {
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

void add_edges(Edges& E, const std::vector< matrix<int> >& Ms,
	       int s1,int s2,int L1, int L2,double cutoff);

void add_edges(Edges& E, const std::vector< sparse_index_matrix >& Ms,
	       int s1,int s2,int L1, int L2,double cutoff);


class index_matrix: public matrix<int> 
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
	:matrix<int>(C,L.size()),columns(C),unknowns(0)
	{
	    for(int i=0;i<L.size();i++)
		column_index.push_back(std::vector<int>(L[i]));
	}

    index_matrix(const alignment& A);
};

struct sparse_alignment
{
    std::vector<std::string> names;
    std::vector<std::vector<int>> sequences;
    std::shared_ptr<const alphabet> a;

    sparse_index_matrix homology;
};

std::ostream& operator<<(std::ostream& o, const sparse_alignment& a);

index_matrix unaligned_matrix(const std::vector<int>& L);
sparse_index_matrix unaligned_sparse_matrix(const std::vector<int>& L);

unsigned count_unknowns(const index_matrix& M,int c);

matrix<int> get_ordered_matrix(const index_matrix& M);

std::vector<int> get_ordered_columns(const sparse_index_matrix& M);

alignment get_alignment(const matrix<int>& M, const alignment& A);

alignment get_ordered_alignment(const alignment& A);

