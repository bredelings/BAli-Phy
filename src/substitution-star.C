/*
   Copyright (C) 2004-2005,2007 Benjamin Redelings

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

#include "substitution.H"
#include "rng.H"
#include <cmath>
#include <valarray>
#include <vector>


using std::valarray;
using std::vector;

// This file assumes that 
// * the matrix is reversible.  This means that we evaluate
//   frequencies at the root - even for insertions, where they actually
//   apply somewhere down the tree.
//
// * we don't need to work in log space for a single column
//
// * 
namespace substitution {

  double Pr_star(const vector<int>& column,const Tree& T,const ReversibleAdditiveObject& SModel,
		 const Mat_Cache& MC,int m) {
    const alphabet& a = SModel.Alphabet();

    double p=0;
    for(int lroot=0;lroot<a.size();lroot++) {
      double temp=SModel.frequencies()[lroot];
      for(int b=0;b<T.n_leaves();b++) {
	const Matrix& Q = MC.transition_P(b)[m];

	int lleaf = column[b];
	if (a.is_letter(lleaf))
	  temp *= Q(lroot,lleaf);
	else if (a.is_letter_class(lleaf))
	  temp *= sum(Q,lroot,lleaf,a);
      }
      p += temp;
    }

    // we don't get too close to zero, normally
    assert(0 <= p and p <= 1.00000000001);

    return p;
  }

  efloat_t Pr_star(const alignment& A, const Tree& T, const MultiModelObject& MModel, const Mat_Cache& MC) 
  {
    efloat_t p = 1;
  
    vector<int> residues(A.n_sequences());

    // Do each node before its parent
    for(int column=0;column<A.length();column++) {
      for(int i=0;i<residues.size();i++)
	residues[i] = A(column,i);

      double total=0;
      for(int m=0;m<MModel.n_base_models();m++)
	  total += MModel.distribution()[m] * Pr_star(residues,
						      T,
						      MModel.base_model(m),
						      MC,
						      m
						      );

      // we don't get too close to zero, normally
      assert(0 < total and total <= 1.00000000001);

      p *= total;
    }

    return p;
  }

  efloat_t Pr_star(const data_partition& P) 
  {
    if (P.T_->n_leaves() == 2)
      return Pr(P);

    return Pr_star(*P.A, *P.T_, P.SModel(), P);
  }

  efloat_t Pr_unaligned(const data_partition& P) 
  {
    const alignment& A = *P.A;
    const alphabet& a = A.get_alphabet();

    vector<efloat_t> f(P.SModel().frequencies().size());
    for(int l=0;l<f.size();l++)
      f[l] = P.SModel().frequencies()[l];


    efloat_t total = 1;
    for(int i=0;i<A.n_sequences();i++) {
      for(int column=0;column<A.length();column++) {
	int l = A(column,i);
	if (a.is_letter(l))
	  total *= f[l];
	else if (a.is_letter_class(l))
	  total *= sum(P.SModel().frequencies(),l,a);
      }
    }

    return total;
  }

  efloat_t Pr_single_sequence(const data_partition& P) 
  {
    const alignment& A = *P.A;
    const alphabet& a = A.get_alphabet();

    vector<efloat_t> f(P.SModel().frequencies().size());
    for(int l=0;l<f.size();l++)
      f[l] = P.SModel().frequencies()[l];

    efloat_t min = 1;
    for(int i=0;i<A.n_sequences();i++) 
    {
      efloat_t total = 1;
      for(int column=0;column<A.length();column++) {
	int l = A(column,i);
	if (a.is_letter(l))
	  total *= f[l];
	else if (a.is_letter_class(l))
	  total *= sum(P.SModel().frequencies(),l,a);
      }

      if (total < min)
	min = total;
    }
    
    return min;
  }
}
