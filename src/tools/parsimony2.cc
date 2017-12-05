/*
   Copyright (C) 2005-2009 Benjamin Redelings

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

#include "util.H"
#include "parsimony.H"
#include "substitution/parsimony.H"
using namespace std;

template <class B>
B row_min(const matrix<B>& M,int row)
{
  B min=M(row,0);
  for(int i=1;i<M.size2();i++)
    min = std::min(min,M(row,i));
  return min;
}

// Decompose into 2 functions
// (a) one should construct the cost matrix
// (b) one should use this to reconstruct the letters at each node
// Also change the vector< vector< double> > to a Matrix

// Finally, I should templatize these functions, in order to allow an <int> and
// a <double> version.  Do I need to put ALL the code in the headers, or can I
// just instantiate the <int> and <double> versions, with the code in the *.C file?

template <class B>
void peel_n_mutations(const alphabet& a, const vector<int>& letters, const TreeInterface& T,
		      const matrix<B>& cost,matrix<B>& n_muts,
		      const vector<int>& branches)
{
  const int A = a.size();

  assert(letters.size() == T.n_leaves());
  assert(cost.size1() == A);
  assert(cost.size2() == A);

  // we need a scratch row in the matrix
  assert(n_muts.size1() == T.n_nodes());
  assert(n_muts.size2() == A);

  // compute the max cost -- is this approach a good idea?
  // Well... this apparently doesn't work.
  B max_cost = 0;
  for(int i=0;i<A;i++)
    for(int j=0;j<A;j++)
      max_cost = std::max(cost(i,j)+1, max_cost);
    
  // clear the length matrix.
  for(int i=0;i<n_muts.size1();i++)
    for(int j=0;j<n_muts.size2();j++)
      n_muts(i,j)=0;
  
  // set the leaf costs
  for(int s=0;s<T.n_leaves();s++)
  {
    int L = letters[s];

    if (a.is_letter_class(L))
      for(int l=0;l<A;l++)
      {
	if (a.matches(l,L))
	  n_muts(s,l) = 0;
	else
	  n_muts(s,l) = max_cost;
      }
  }


  // compute the costs for letters at each node
  for(int i=0;i<branches.size();i++)
  {
    int s = T.source(branches[i]);
    int t = T.target(branches[i]);

    // for each letter l of node target...
    for(int l=0;l<A;l++)
    {
      // compute minimum treelength for data behind source.
      B temp = n_muts(s,0)+cost(0,l);
      for(int k=1;k<A;k++)
	temp = min(temp, n_muts(s,k)+cost(k,l) );

      // add it to treelengths for data behind target
      n_muts(t,l) += temp;
    }
  }
}

template <class B>
B n_mutations(const alphabet& a, const vector<int>& letters, const TreeInterface& T,const matrix<B>& cost,
	      matrix<B>& n_muts, const vector<int>& branches)
{
  int root = T.target(0);

  peel_n_mutations(a,letters,T,cost,n_muts,branches);

  return row_min(n_muts,root);
}

template <class B>
B n_mutations(const alphabet& a, const vector<int>& letters, const TreeInterface& T,const matrix<B>& cost)
{
  int root = T.target(0);

  vector<int> branches = T.all_branches_toward_node(root);

  matrix<B> n_muts(T.n_nodes(), a.size());

  return n_mutations(a,letters,T,cost,n_muts,branches);
}

template int n_mutations(const alphabet& a, const vector<int>& letters, const TreeInterface& T,const matrix<int>& cost);

template double n_mutations(const alphabet& a, const vector<int>& letters, const TreeInterface& T,const matrix<double>& cost);

vector<int> get_parsimony_letters(const alphabet& a, const vector<int>& letters, const TreeInterface& T,
				  const matrix<int>& cost)
{
  int root = T.target(0);
  matrix<int> n_muts(T.n_nodes(),a.size());

  peel_n_mutations(a,letters,T,cost,n_muts, T.all_branches_toward_node(root) );

  // get an order list of branches point away from the root;
  vector<int> branches = T.all_branches_from_node(root);
  
  // Allocate space to store the letter for each node
  vector<int> node_letters(T.n_nodes(),-1);

  // choose the cheapest letter at the root
  node_letters[root] = row_min(n_muts,root);

  const unsigned A = a.size();
  vector<double> temp(A);

  for(int i=0;i<branches.size();i++) 
  {
    int s = T.source(branches[i]);
    int t = T.target(branches[i]);

    int k = node_letters[s];
    assert(k != -1);

    for(int l=0;l<A;l++)
      temp[l] = n_muts(t,l)+cost(l,k);

    node_letters[t] = argmin(temp);
  }

  return node_letters;
}



vector<vector<int> > get_all_parsimony_letters(const alphabet& a, const vector<int>& letters, const TreeInterface& T,
					       const matrix<int>& cost)
{
  int root = T.target(0);

  matrix<int> n_muts(T.n_nodes(), a.size());
  peel_n_mutations(a,letters,T,cost,n_muts, T.all_branches_toward_node(root) );

  // get an order list of branches point away from the root;
  vector<int> branches = T.all_branches_from_node(root);
  
  // Allocate space to store the letters for each node
  vector<vector<int> > node_letters(T.n_nodes());

  const unsigned A = a.size();

  // choose the cheapest letters at the root
  {
    double m = row_min(n_muts,root);
    for(int l=0;l<A;l++)
      if (n_muts(root,l) <= m)
	node_letters[root].push_back(l);
  }

  vector<double> temp(A);

  for(int i=0;i<branches.size();i++) 
  {
    int s = T.source(branches[i]);
    int t = T.target(branches[i]);

    vector<double> best(node_letters[s].size());

    for(int j=0;j<node_letters[s].size();j++) 
    {
      for(int l=0;l<A;l++)
	temp[l] = n_muts(t,l)+cost(l,node_letters[s][j]);
      best[j] = min(temp);
    }
    
    for(int l=0;l<A;l++) 
    {
      bool is_best = false;
      for(int j=0;j<node_letters[s].size() and not is_best;j++) 
	if (n_muts(t,l)+cost(l,node_letters[s][j]) <= best[j])
	  is_best=true;
      if (is_best)
	node_letters[t].push_back(l);
    }

  }

  return node_letters;
}

template <class B>
B n_mutations(const alphabet& a, const vector<int>& letters, const TreeInterface& T) 
{
  return n_mutations<B>(a,letters,T,unit_cost_matrix(a));
}


template <class B>
B n_mutations(const alignment& A, const TreeInterface& T,const matrix<B>& cost)
{
  const alphabet& a = A.get_alphabet();

  vector<int> letters(T.n_leaves());

  int root = T.target(0);

  vector<int> branches = T.all_branches_toward_node(root);

  matrix<B> n_muts(T.n_nodes(), a.size());

  double tree_length = 0;
  for(int c=0;c<A.length();c++) {
    for(int i=0;i<T.n_leaves();i++)
      letters[i] = A(c,i);
    double length = n_mutations<B>(a,letters,T,cost,n_muts,branches);
    tree_length += length;
  }

  return tree_length;
}

template 
int n_mutations(const alignment& A, const TreeInterface& T,const matrix<int>& cost);

int n_mutations(const alignment& A, const TreeInterface& T)
{
  return n_mutations<int>(A,T,unit_cost_matrix(A.get_alphabet()));
}
