/*
   Copyright (C) 2004-2005,2009 Benjamin Redelings

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

#include "2way.H"
#include "alignment-util.H"
#include "substitution-index.H"

using namespace std;
using boost::dynamic_bitset;

void pairwise_alignment_t::flip()
{
  for(int i=0;i<size();i++)
    (*this)[i] = A2::flip((*this)[i]);
}

pairwise_alignment_t pairwise_alignment_t::flipped() const
{
  pairwise_alignment_t pi (*this);
  pi.flip();
  return pi;
}

int pairwise_alignment_t::length1() const
{
  int total = 0;
  for(int i=0;i<size();i++)
    if ((*this)[i] == A2::states::M or (*this)[i] == A2::states::G2)
      total++;
  return total;
}

int pairwise_alignment_t::length2() const
{
  int total = 0;
  for(int i=0;i<size();i++)
    if ((*this)[i] == A2::states::M or (*this)[i] == A2::states::G1)
      total++;
  return total;
}

pairwise_alignment_t::pairwise_alignment_t()
{ }

pairwise_alignment_t::pairwise_alignment_t(const vector<int>& pi)
  :vector<int>(pi)
{ }

pairwise_alignment_t::pairwise_alignment_t(const pairwise_alignment_t& pi)
  :vector<int>(pi)
{ }

bool operator==(const pairwise_alignment_t& pi1, const pairwise_alignment_t& pi2)
{
  if (pi1.size() != pi2.size()) return false;

  for(int i=0;i<pi1.size();i++)
    if (pi1[i] != pi2[i]) return false;

  return true;
}

namespace A2 {


  int flip(int s)
  {
    if (s == states::G1)
      return states::G2;
    else if (s == states::G2) 
      return states::G1;
    else return s;
  }

  pairwise_alignment_t get_pairwise_alignment(const alignment& A, int n1, int n2)
  {
    pairwise_alignment_t pi;
    pi.reserve(A.length()+2);
    pi.push_back(A2::states::S);
    for(int c=0;c<A.length();c++)
    {
      int S = -1;
      if (A.character(c,n1))
      {
	if (A.character(c,n2))
	  S = A2::states::M;
	else
	  S = A2::states::G2;
      }
      else
      {
	if (A.character(c,n2))
	  S = A2::states::G1;
      }
      if (S != -1)
	pi.push_back(S);
    }
    pi.push_back(A2::states::E);

    return pi;
  }

alignment construct(const alignment& old, const vector<int>& path, int n1,int n2,
		    const Tree& T, const vector<int>& seq1,const vector<int>& seq2) {

  dynamic_bitset<> group1 = T.partition(n2,n1);
  dynamic_bitset<> group2 = ~group1;

  vector<int> subA1;
  vector<int> subA2;
  for(int column=0;column<old.length();column++) {
    if (not all_gaps(old,column,group1))
      subA1.push_back(column);
    if (not all_gaps(old,column,group2))
      subA2.push_back(column);
  }


  const int newlength = path.size() + (subA1.size()-seq1.size()) + (subA2.size() - seq2.size());
  alignment A = blank_copy(old,newlength);
  assert(A.length() == newlength);

  //  std::clog<<"old = "<<old<<"\n";

  int c1=0,c2=0,c3=0,c4=0,l=0;
  for(int column=0;column<A.length();column++) {
    //    std::clog<<column<<" "<<c1<<" "<<c2<<" "<<c3<<" "<<c4<<"  "<<l<<"\n";

    assert(c1>=c2);
    assert(c3>=c4);
    assert(c1 <= subA1.size());
    assert(c3 <= subA2.size());

    if (c1 < subA1.size() and (c2 == seq1.size() or (c2<seq1.size() and subA1[c1] < seq1[c2]))) {
      for(int i=0;i<A.n_sequences();i++) {
	if (group1[i])
	  A.set_value(column,i, old(subA1[c1],i));
	else
	  A.set_value(column,i, alphabet::gap );
      }
      c1++;
      assert(not all_gaps(A,column));
    }
    else if (c3 < subA2.size() and (c4 == seq2.size() or (c4<seq2.size() and subA2[c3] < seq2[c4]))) {
      for(int i=0;i<A.n_sequences();i++) {
	if (group1[i])
	  A.set_value(column,i, alphabet::gap);
	else
	  A.set_value(column,i, old(subA2[c3],i) );
      }
      c3++;
      assert(not all_gaps(A,column));
    }
    else if (path[l]==0) {
      for(int i=0;i<A.n_sequences();i++) {
	if (group1[i])
	  A.set_value(column,i, old(seq1[c2],i));
	else
	  A.set_value(column,i, old(seq2[c4],i));
      }
      c1++;c2++;c3++;c4++;l++;
      assert(not all_gaps(A,column));
    }
    else if (path[l]==1) {
      for(int i=0;i<A.n_sequences();i++) {
	if (group1[i])
	  A.set_value(column,i, alphabet::gap);
	else
	  A.set_value(column,i, old(seq2[c4],i) );
      }
      c3++;c4++;l++;
      assert(not all_gaps(A,column));
    }
    else {
      for(int i=0;i<A.n_sequences();i++) {
	if (group1[i])
	  A.set_value(column,i, old(seq1[c2],i) );
	else
	  A.set_value(column,i, alphabet::gap );
      }
      c1++;c2++;l++;
      assert(not all_gaps(A,column));
    }
    //    std::clog<<column<<" "<<c1<<" "<<c2<<" "<<c3<<" "<<c4<<"  "<<l<<"\n";;
    assert(not all_gaps(A,column));
  }

  assert(c1 == subA1.size());
  assert(c2 == seq1.size());
  assert(c3 == subA2.size());
  assert(c4 == seq2.size());
  assert(l == path.size());

  for(int i=0;i<A.n_sequences();i++) 
    assert(A.seqlength(i) == old.seqlength(i));

  //  std::cerr<<"new = "<<A<<endl;  
  assert(valid(A));

  return A;
}

}
