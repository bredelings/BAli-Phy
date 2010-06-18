/*
   Copyright (C) 2004-2005,2007,2009-2010 Benjamin Redelings

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
#include "substitution-index.H"
#include "rng.H"
#include <cmath>
#include <valarray>
#include <vector>
#include "timer_stack.H"

#ifdef NDEBUG
#define IF_DEBUG(x)
#else
#define IF_DEBUG(x) x
#endif

// recalculate a likelihood immediate afterwards, and see if we get the same answer...
// perhaps move the collection root node one branch away?
// then we have to do re-validation...

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


inline void element_assign(Matrix& M1,double d)
{
  const int size = M1.data().size();
  double * __restrict__ m1 = M1.data().begin();
  
  for(int i=0;i<size;i++)
    m1[i] = d;
}

inline void element_assign(Matrix& M1,const Matrix& M2)
{
  assert(M1.size1() == M2.size1());
  assert(M1.size2() == M2.size2());
  
  const int size = M1.data().size();
  double * __restrict__ m1 = M1.data().begin();
  const double * __restrict__ m2 = M2.data().begin();
  
  for(int i=0;i<size;i++)
    m1[i] = m2[i];
}

inline void element_prod_modify(Matrix& M1,const Matrix& M2)
{
  assert(M1.size1() == M2.size1());
  assert(M1.size2() == M2.size2());
  
  const int size = M1.data().size();
  double * __restrict__ m1 = M1.data().begin();
  const double * __restrict__ m2 = M2.data().begin();
  
  for(int i=0;i<size;i++)
    m1[i] *= m2[i];
}

inline void element_prod_assign(Matrix& M1,const Matrix& M2,const Matrix& M3)
{
  assert(M1.size1() == M2.size1());
  assert(M1.size2() == M2.size2());
  
  assert(M1.size1() == M3.size1());
  assert(M1.size2() == M3.size2());
  
  const int size = M1.data().size();
  double * __restrict__ m1 = M1.data().begin();
  const double * __restrict__ m2 = M2.data().begin();
  const double * __restrict__ m3 = M3.data().begin();
  
  for(int i=0;i<size;i++)
    m1[i] = m2[i]*m3[i];
}

inline double element_sum(const Matrix& M1)
{
  const int size = M1.data().size();
  const double * __restrict__ m1 = M1.data().begin();
  
  double sum = 0;
  for(int i=0;i<size;i++)
    sum += m1[i];
  return sum;
}


inline double element_prod_sum(Matrix& M1,const Matrix& M2)
{
  assert(M1.size1() == M2.size1());
  assert(M1.size2() == M2.size2());
  
  const int size = M1.data().size();
  const double * __restrict__ m1 = M1.data().begin();
  const double * __restrict__ m2 = M2.data().begin();

  double sum = 0;
  for(int i=0;i<size;i++)
    sum += m1[i] * m2[i];

  return sum;
}

inline double element_prod_sum(Matrix& M1,const Matrix& M2,const Matrix& M3)
{
  assert(M1.size1() == M2.size1());
  assert(M1.size2() == M2.size2());
  
  assert(M1.size1() == M3.size1());
  assert(M1.size2() == M3.size2());
  
  const int size = M1.data().size();
  const double * __restrict__ m1 = M1.data().begin();
  const double * __restrict__ m2 = M2.data().begin();
  const double * __restrict__ m3 = M3.data().begin();

  double sum = 0;
  for(int i=0;i<size;i++)
    sum += m1[i] * m2[i] * m3[i];

  return sum;
}

inline double element_prod_sum(Matrix& M1,const Matrix& M2,const Matrix& M3,const Matrix& M4)
{
  assert(M1.size1() == M2.size1());
  assert(M1.size2() == M2.size2());
  
  assert(M1.size1() == M3.size1());
  assert(M1.size2() == M3.size2());
  
  assert(M1.size1() == M4.size1());
  assert(M1.size2() == M4.size2());
  
  const int size = M1.data().size();
  const double * __restrict__ m1 = M1.data().begin();
  const double * __restrict__ m2 = M2.data().begin();
  const double * __restrict__ m3 = M3.data().begin();
  const double * __restrict__ m4 = M4.data().begin();

  double sum = 0;
  for(int i=0;i<size;i++)
    sum += m1[i] * m2[i] * m3[i] * m4[i];

  return sum;
}

namespace substitution {

  int total_peel_leaf_branches=0;
  int total_peel_internal_branches=0;
  int total_peel_branches=0;
  int total_likelihood=0;
  int total_calc_root_prob=0;

  struct peeling_info: public vector<int> {
    peeling_info(const Tree&T) { reserve(T.n_branches()); }
  };

  /// compute log(probability) from conditional likelihoods (S) and equilibrium frequencies as in MModel
  efloat_t Pr(const Matrix& S,const MultiModel& MModel) 
  {
    const int n_models = MModel.n_base_models();
    const int n_states = MModel.n_states();
    const vector<double>& d = MModel.distribution();

    double total = 0;
    for(int m=0;m<n_models;m++) 
    {
      const valarray<double>& f = MModel.base_model(m).frequencies();

      double p = 0;
      for(int s=0;s<n_states;s++)
	p += S(m,s) * f[s];

      total += p * d[m];

      // A specific model (e.g. the INV model) could be impossible
      assert(0 <= p and p <= 1.00000000001);
    }

    // SOME model must be possible
    assert(0 <= total and total <= 1.00000000001);

    return total;
  }

  void WeightedFrequencyMatrix(Matrix& F, const MultiModel& MModel) 
  {
    // cache matrix of frequencies
    const int n_models = F.size1();
    const int n_states = F.size2();

    for(int m=0;m<n_models;m++) {
      double p = MModel.distribution()[m];
      const valarray<double>& f = MModel.base_model(m).frequencies();
      for(int s=0;s<n_states;s++) 
	F(m,s) = f[s]*p;
    }
  }

  efloat_t calc_root_probability(const alignment&, const Tree& T,Likelihood_Cache& cache,
			       const MultiModel& MModel,const vector<int>& rb,const ublas::matrix<int>& index) 
  {
    total_calc_root_prob++;
    default_timer_stack.push_timer("substitution::calc_root");

    assert(index.size2() == rb.size());

    const int root = cache.root;

    if (T[root].is_leaf_node())
      throw myexception()<<"Trying to accumulate conditional likelihoods at a leaf node is not allowed.";
    assert(rb.size() == 3);

    // scratch matrix 
    Matrix & S = cache.scratch(0);
    const int n_models = S.size1();
    const int n_states = S.size2();

    // cache matrix F(m,s) of p(m)*freq(m,l)
    Matrix F(n_models,n_states);
    WeightedFrequencyMatrix(F, MModel);

    // look up the cache rows now, once, instead of for each column
    vector< vector<Matrix>* > branch_cache;
    for(int i=0;i<rb.size();i++)
      branch_cache.push_back(&cache[rb[i]]);
    
    efloat_t total = 1;
    for(int i=0;i<index.size1();i++)
    {
      double p_col = 1;

      int i0 = index(i,0);
      int i1 = index(i,1);
      int i2 = index(i,2);

      Matrix* m[3];
      int mi=0;

      if (i0 != -1)
	m[mi++] = &((*branch_cache[0])[i0]);
      if (i1 != -1)
	m[mi++] = &((*branch_cache[1])[i1]);
      if (i2 != -1)
	m[mi++] = &((*branch_cache[2])[i2]);

      if (mi==3)
	p_col = element_prod_sum(F, *m[0], *m[1], *m[2]);
      else if (mi==2)
	p_col = element_prod_sum(F, *m[0], *m[1]);
      else if (mi==1)
	p_col = element_prod_sum(F, *m[0]);

#ifndef NDEBUG
      //-------------- Set letter & model prior probabilities  ---------------//
      element_assign(S,F);

      //-------------- Propagate and collect information at 'root' -----------//
      for(int j=0;j<rb.size();j++) {
	int i0 = index(i,j);
	if (i0 != alphabet::gap)
	  element_prod_modify(S,(*branch_cache[j])[i0]);
      }

      //------------ Check that individual models are not crazy -------------//
      for(int m=0;m<n_models;m++) {
	double p_model=0;
	for(int s=0;s<n_states;s++)
	  p_model += S(m,s);
	// A specific model (e.g. the INV model) could be impossible
	assert(0 <= p_model and p_model <= 1.00000000001);
      }

      double p_col2 = element_sum(S);

      assert((p_col - p_col2)/std::max(p_col,p_col2) < 1.0e-9);
#endif

      // SOME model must be possible
      assert(0 <= p_col and p_col <= 1.00000000001);

      // This does a log( ) operation.
      total *= p_col;
      //      std::clog<<" i = "<<i<<"   p = "<<p_col<<"  total = "<<total<<"\n";
    }

    for(int i=0;i<rb.size();i++)
      total *= cache[rb[i]].other_subst;

    default_timer_stack.pop_timer();
    return total;
  }

  efloat_t calc_root_probability_unaligned(const alignment&,const Tree& T,Likelihood_Cache& cache,
					   const MultiModel& MModel,const vector<int>& rb,const ublas::matrix<int>& index) 
  {
    total_calc_root_prob++;
    default_timer_stack.push_timer("substitution::calc_root_unaligned");

    assert(index.size2() == rb.size());

    if (T[cache.root].is_leaf_node())
      throw myexception()<<"Trying to accumulate conditional likelihoods at a leaf node is not allowed.";
    assert(rb.size() == 3);

    // scratch matrix 
    const int n_models = cache.n_models();
    const int n_states = cache.n_states();

    // cache matrix F(m,s) of p(m)*freq(m,l)
    Matrix F(n_models,n_states);
    WeightedFrequencyMatrix(F, MModel);

    // look up the cache rows now, once, instead of for each column
    vector< vector<Matrix>* > branch_cache;
    for(int i=0;i<rb.size();i++)
      branch_cache.push_back(&cache[rb[i]]);
    
    efloat_t total = 1;
    for(int i=0;i<index.size1();i++)
    {
      double p_col = 1;

      int i0 = index(i,0);
      int i1 = index(i,1);
      int i2 = index(i,2);

      Matrix* m[3];
      int mi=0;

      if (i0 != -1)
	m[mi++] = &((*branch_cache[0])[i0]);
      if (i1 != -1)
	m[mi++] = &((*branch_cache[1])[i1]);
      if (i2 != -1)
	m[mi++] = &((*branch_cache[2])[i2]);

      if (mi > 0)
	p_col = element_prod_sum(F,*m[0]);
      if (mi > 1)
	p_col *= element_prod_sum(F,*m[1]);
      if (mi > 2)
	p_col *= element_prod_sum(F,*m[2]);

      // SOME model must be possible
      assert(0 <= p_col and p_col <= 1.00000000001);

      // This does a log( ) operation.
      total *= p_col;
      //      std::clog<<" i = "<<i<<"   p = "<<p_col<<"  total = "<<total<<"\n";
    }

    for(int i=0;i<rb.size();i++)
      total *= cache[rb[i]].other_subst;

    default_timer_stack.pop_timer();
    return total;
  }

  efloat_t calc_root_probability(const data_partition& P,const vector<int>& rb,
			       const ublas::matrix<int>& index) 
  {
    return calc_root_probability(*P.A, *P.T, P.LC, P.SModel(), rb, index);
  }

  inline double sum(const Matrix& Q, const vector<unsigned>& smap, int n_letters, 
		    int s1, int l)
  {
    double total = 0;
    int n_states = smap.size();
#ifdef DEBUG_SMAP
    for(int s2=0; s2<n_states; s2++)
      if (smap[s2] == l)
	total += Q(s1,s2);
#else    
    for(int s2=l; s2<n_states; s2+=n_letters)
      total += Q(s1,s2);
#endif
    return total;
  }

  inline double sum(const Matrix Q,const vector<unsigned>& smap,
		    int s1, int l2, const alphabet& a)
  {
    double total=0;
    int n_letters = a.n_letters();
#ifdef DEBUG_SMAP
    for(int s=0;s<smap.size();s++)
      if (a.matches(smap[s],l2))
	total += Q(s1,s);
#else
    for(int L=0;L<n_letters;L++)
      if (a.matches(L,l2))
	total += sum(Q,smap,n_letters,s1,L);
#endif
    return total;
  }


  void peel_leaf_branch(int b0,subA_index_t& I, Likelihood_Cache& cache, const alignment& A, const Tree& T, 
			const vector<Matrix>& transition_P,const MultiModel& MModel)
  {
    total_peel_leaf_branches++;
    default_timer_stack.push_timer("substitution::peel_leaf_branch");

    const alphabet& a = A.get_alphabet();

    // scratch matrix
    Matrix& S = cache.scratch(0);
    const int n_models  = S.size1();
    const int n_states  = S.size2();
    //    const int n_letters = a.n_letters();
    assert(MModel.n_states() == n_states);

    if (not I.branch_index_valid(b0))
      I.update_branch(A,T,b0);

    //    const vector<unsigned>& smap = MModel.state_letters();

    for(int i=0;i<I.branch_index_length(b0);i++)
    {
      Matrix& R = cache(i,b0);
      // compute the distribution at the parent node
      int l2 = A.note(0,i+1,b0);

      if (a.is_letter(l2))
	for(int m=0;m<n_models;m++) {
	  const Matrix& Q = transition_P[m];
	  for(int s1=0;s1<n_states;s1++)
	    R(m,s1) = Q(s1,l2);
	}
      else if (a.is_letter_class(l2)) {
	for(int m=0;m<n_models;m++) {
	  const Matrix& Q = transition_P[m];
	  for(int s1=0;s1<n_states;s1++)
	    R(m,s1) = sum(Q,s1,l2,a);
	}
      }
      else
	element_assign(R,1);
    }
    default_timer_stack.pop_timer();
  }

  void FrequencyMatrix(Matrix& F, const MultiModel& MModel) 
  {
    // cache matrix of frequencies
    const int n_models = F.size1();
    const int n_states = F.size2();

    for(int m=0;m<n_models;m++) {
      const valarray<double>& f = MModel.base_model(m).frequencies();
      for(int s=0;s<n_states;s++) 
	F(m,s) = f[s];
    }
  }

  void peel_leaf_branch_F81(int b0, subA_index_t& I, Likelihood_Cache& cache, const alignment& A, const Tree& T, 
			    const MultiModel& MModel)
  {
    total_peel_leaf_branches++;
    default_timer_stack.push_timer("substitution::peel_leaf_branch");

    //    std::cerr<<"got here! (leaf)"<<endl;

    const alphabet& a = A.get_alphabet();

    // scratch matrix
    Matrix& S = cache.scratch(0);
    const int n_models  = S.size1();
    const int n_states  = S.size2();
    //    const int n_letters = a.n_letters();
    assert(MModel.n_states() == n_states);

    //    std::clog<<"length of subA for branch "<<b0<<" is "<<length<<"\n";
    if (not I.branch_index_valid(b0))
      I.update_branch(A,T,b0);

    //    const vector<unsigned>& smap = MModel.state_letters();

    vector<const F81_Model*> SubModels(n_models);
    for(int m=0;m<n_models;m++) {
      SubModels[m] = static_cast<const F81_Model*>(&MModel.base_model(m));
      assert(SubModels[m]);
    }
    const double t = T.directed_branch(b0).length();

    valarray<double> exp_a_t(n_models);
    for(int m=0;m<n_models;m++) 
      exp_a_t[m] = exp(-t * SubModels[m]->alpha());

    Matrix& F = cache.scratch(1);
    FrequencyMatrix(F,MModel); // F(m,l2)

    for(int i=0;i<I.branch_index_length(b0);i++)
    {
      Matrix& R = cache(i,b0);
      // compute the distribution at the parent node
      int l2 = A.note(0,i+1,b0);

      if (a.is_letter(l2))
	for(int m=0;m<n_models;m++) {
	  double temp = (1.0-exp_a_t[m])*F(m,l2); // move load out of loop for GCC vectorizer
	  for(int s1=0;s1<n_states;s1++)
	    R(m,s1) = temp;
	  R(m,l2) += exp_a_t[m];
	}
      else if (a.is_letter_class(l2)) 
      {
	for(int m=0;m<n_models;m++) 
	{
	  double sum=0;
	  for(int l=0;l<a.size();l++)
	    if (a.matches(l,l2))
	      sum += F(m,l);
	  double temp = (1.0-exp_a_t[m])*sum; // move load out of loop for GCC vectorizer
	  for(int s1=0;s1<n_states;s1++)
	    R(m,s1) = temp;
	  for(int l=0;l<a.size();l++)
	    if (a.matches(l,l2))
	      R(m,l) += exp_a_t[m];
	}
      }
      else
	element_assign(R,1);
    }
    default_timer_stack.pop_timer();
  }

  void peel_leaf_branch_modulated(int b0,subA_index_t& I, Likelihood_Cache& cache, const alignment& A, 
				  const Tree& T, 
				  const vector<Matrix>& transition_P,const MultiModel& MModel)
  {
    total_peel_leaf_branches++;
    default_timer_stack.push_timer("substitution::peel_leaf_branch");

    const alphabet& a = A.get_alphabet();

    // scratch matrix
    Matrix& S = cache.scratch(0);
    const int n_models  = S.size1();
    const int n_states  = S.size2();
    const int n_letters = a.n_letters();
    //    const int N = n_states/n_letters;
    assert(MModel.n_states() == n_states);

    if (not I.branch_index_valid(b0))
      I.update_branch(A,T,b0);

    const vector<unsigned>& smap = MModel.state_letters();

    for(int i=0;i<I.branch_index_length(b0);i++)
    {
      Matrix& R = cache(i,b0);
      // compute the distribution at the parent node
      int l2 = A.note(0,i+1,b0);

      if (a.is_letter(l2))
	for(int m=0;m<n_models;m++) {
	  const Matrix& Q = transition_P[m];
	  for(int s1=0;s1<n_states;s1++)
	    R(m,s1) = sum(Q,smap,n_letters,s1,l2);
	}
      else if (a.is_letter_class(l2)) {
	for(int m=0;m<n_models;m++) {
	  const Matrix& Q = transition_P[m];
	  for(int s1=0;s1<n_states;s1++)
	    R(m,s1) = sum(Q,smap,s1,l2,a);
	}
      }
      else
	element_assign(R,1);
    }
    default_timer_stack.pop_timer();
  }

  void collect_internal_branch(const vector<int>& b, ublas::matrix<int>& index, Likelihood_Cache& cache,
			       const MultiModel& MModel)
  {
    // look up the cache rows now, once, instead of for each column
    vector< vector<Matrix>* > branch_cache;
    for(int i=0;i<b.size();i++)
      branch_cache.push_back(&cache[b[i]]);

    ///    Hmm... See calc_root_probability_unaligned( )...
    
    efloat_t other_subst = 1;
    cache[b[2]].other_subst = cache[b[0]].other_subst * cache[b[1]].other_subst * other_subst;
  }

  void peel_internal_branch(const vector<int>& b,ublas::matrix<int>& index, Likelihood_Cache& cache,
			    const vector<Matrix>& transition_P,const MultiModel& IF_DEBUG(MModel))
  {
    // scratch matrix
    Matrix& S = cache.scratch(0);
    const int n_models = S.size1();
    const int n_states = S.size2();
    assert(MModel.n_states() == n_states);

    // look up the cache rows now, once, instead of for each column
    vector< vector<Matrix>* > branch_cache;
    for(int i=0;i<b.size();i++)
      branch_cache.push_back(&cache[b[i]]);
    
    for(int i=0;i<index.size1();i++) 
    {
      // compute the source distribution from 2 branch distributions
      int i0 = index(i,0);
      int i1 = index(i,1);

      const Matrix* C = &S;
      if (i0 != alphabet::gap and i1 != alphabet::gap)
	element_prod_assign(S, (*branch_cache[0])[i0], (*branch_cache[1])[i1]);
      else if (i0 != alphabet::gap)
	C = &(*branch_cache[0])[i0];
      else if (i1 != alphabet::gap)
	C = &(*branch_cache[1])[i1];
      else
	std::abort(); // columns like this should not be in the index

      // propagate from the source distribution
      Matrix& R = (*branch_cache[2])[i];            //name the result matrix
      for(int m=0;m<n_models;m++) {
	
	// FIXME!!! - switch order of MatCache to be MC[b][m]
	const Matrix& Q = transition_P[m];
	
	// compute the distribution at the target (parent) node - multiple letters
	for(int s1=0;s1<n_states;s1++) {
	  double temp=0;
	  for(int s2=0;s2<n_states;s2++)
	    temp += Q(s1,s2)*(*C)(m,s2);
	  R(m,s1) = temp;
	}
      }
    }
  }

  void peel_internal_branch(int b0,subA_index_leaf& I, Likelihood_Cache& cache, const alignment& A, const Tree& T, 
			    const vector<Matrix>& transition_P,const MultiModel& MModel)
  {
    total_peel_internal_branches++;
    default_timer_stack.push_timer("substitution::peel_internal_branch");

    // find the names of the (two) branches behind b0
    vector<int> b;
    for(const_in_edges_iterator i = T.directed_branch(b0).branches_before();i;i++)
      b.push_back(*i);
    b.push_back(b0);

    // get the relationships with the sub-alignments for the (two) branches behind b0
    ublas::matrix<int> index = I.get_subA_index_select(b,A,T);
    assert(index.size1() == I.branch_index_length(b0));
    assert(I.branch_index_valid(b0));

    peel_internal_branch(b, index, cache, transition_P, MModel);

    default_timer_stack.pop_timer();
  }

  void peel_internal_branch(int b0,subA_index_internal& I, Likelihood_Cache& cache, const alignment& A, const Tree& T, 
			    const vector<Matrix>& transition_P,const MultiModel& MModel)
  {
    total_peel_internal_branches++;
    default_timer_stack.push_timer("substitution::peel_internal_branch");

    // find the names of the (two) branches behind b0
    vector<int> b;
    for(const_in_edges_iterator i = T.directed_branch(b0).branches_before();i;i++)
      b.push_back(*i);
    b.push_back(b0);

    /*---------------------- Do the propagation part --------------------------*/
    // get the relationships with the sub-alignments for the (two) branches behind b0
    ublas::matrix<int> index_propagate = I.get_subA_index_select(b,A,T);
    assert(index_propagate.size1() == I.branch_index_length(b0));
    assert(I.branch_index_valid(b0));

    peel_internal_branch(b, index_propagate, cache, transition_P, MModel);

    /*---------------------- Do the propagation part --------------------------*/
    ublas::matrix<int> index_collect = I.get_subA_index_vanishing(b,A,T);

    collect_internal_branch(b, index_collect, cache, MModel);

    default_timer_stack.pop_timer();
  }

  void peel_internal_branch(int b0,subA_index_t& I, Likelihood_Cache& cache, const alignment& A, const Tree& T, 
			    const vector<Matrix>& transition_P,const MultiModel& MModel)
  {
    if (dynamic_cast<subA_index_leaf*>(&I))
      peel_internal_branch(b0,dynamic_cast<subA_index_leaf&>(I),cache,A,T,transition_P,MModel);
    else if (dynamic_cast<subA_index_internal*>(&I))
      peel_internal_branch(b0,dynamic_cast<subA_index_internal&>(I),cache,A,T,transition_P,MModel);
    else
      throw myexception()<<"subA_index_t is of unrecognized type!";
  }


  void peel_internal_branch_F81(int b0,subA_index_t& I, Likelihood_Cache& cache, const alignment& A, const Tree& T, 
				const MultiModel& MModel)
  {
    //    std::cerr<<"got here! (internal)"<<endl;
    total_peel_internal_branches++;
    default_timer_stack.push_timer("substitution::peel_internal_branch");

    // find the names of the (two) branches behind b0
    vector<int> b;
    for(const_in_edges_iterator i = T.directed_branch(b0).branches_before();i;i++)
      b.push_back(*i);

    // get the relationships with the sub-alignments for the (two) branches behind b0
    b.push_back(b0);
    ublas::matrix<int> index = I.get_subA_index_select(b,A,T);
    b.pop_back();
    assert(index.size1() == I.branch_index_length(b0));
    assert(I.branch_index_valid(b0));

    // The number of directed branches is twice the number of undirected branches
    //    const int B        = T.n_branches();

    // scratch matrix
    Matrix& S = cache.scratch(0);
    const int n_models = S.size1();
    const int n_states = S.size2();
    assert(MModel.n_states() == n_states);

    // look up the cache rows now, once, instead of for each column
    vector< vector<Matrix>* > branch_cache;
    for(int i=0;i<b.size();i++)
      branch_cache.push_back(&cache[b[i]]);
    branch_cache.push_back(&cache[b0]);
    
    vector<const F81_Model*> SubModels(n_models);
    for(int m=0;m<n_models;m++) {
      SubModels[m] = static_cast<const F81_Model*>(&MModel.base_model(m));
      assert(SubModels[m]);
    }
    const double t = T.directed_branch(b0).length();

    valarray<double> exp_a_t(n_models);
    for(int m=0;m<n_models;m++) 
      exp_a_t[m] = exp(-t * SubModels[m]->alpha());

    Matrix& F = cache.scratch(1);
    FrequencyMatrix(F,MModel); // F(m,l2)

    //    std::clog<<"length of subA for branch "<<b0<<" is "<<length<<"\n";
    for(int i=0;i<I.branch_index_length(b0);i++) 
    {
      // compute the source distribution from 2 branch distributions
      int i0 = index(i,0);
      int i1 = index(i,1);

      const Matrix* C = &S;
      if (i0 != alphabet::gap and i1 != alphabet::gap)
	element_prod_assign(S, (*branch_cache[0])[i0], (*branch_cache[1])[i1]);
      else if (i0 != alphabet::gap)
	C = &(*branch_cache[0])[i0];
      else if (i1 != alphabet::gap)
	C = &(*branch_cache[1])[i1];
      else
	std::abort(); // columns like this should not be in the index

      // propagate from the source distribution
      Matrix& R = (*branch_cache[2])[i];            //name the result matrix
      for(int m=0;m<n_models;m++) 
      {
	// compute the distribution at the target (parent) node - multiple letters

	//  sum = (1-exp(-a*t))*(\sum[s2] pi[s2]*L[s2])
	double sum = 0;
	for(int s2=0;s2<n_states;s2++)
	  sum += F(m,s2)*(*C)(m,s2);
	sum *= (1.0 - exp_a_t[m]);

	// L'[s1] = exp(-a*t)L[s1] + sum
	double temp = exp_a_t[m]; //move load out of loop for GCC 4.5 vectorizer.
	for(int s1=0;s1<n_states;s1++) 
	  R(m,s1) = temp*(*C)(m,s1) + sum;
      }
    }
    default_timer_stack.pop_timer();
  }



  void peel_branch(int b0,subA_index_t& I, Likelihood_Cache& cache, const alignment& A, const Tree& T, 
		   const MatCache& transition_P, const MultiModel& MModel)
  {
    total_peel_branches++;
    default_timer_stack.push_timer("substitution::peel_branch");

    // compute branches-in
    int bb = T.directed_branch(b0).branches_before().size();

    int B0 = T.directed_branch(b0).undirected_name();

    if (bb == 0) {
      int n_states = cache.scratch(0).size2();
      int n_letters = A.get_alphabet().n_letters();
      if (n_states == n_letters) {
	if (dynamic_cast<const F81_Model*>(&MModel.base_model(0)))
	  peel_leaf_branch_F81(b0, I, cache, A, T, MModel);
	else
	  peel_leaf_branch(b0, I, cache, A, T, transition_P[B0], MModel);
      }
      else
	peel_leaf_branch_modulated(b0, I, cache, A, T, transition_P[B0], MModel);
    }
    else if (bb == 2) {
      if (dynamic_cast<const F81_Model*>(&MModel.base_model(0)))
	peel_internal_branch_F81(b0, I, cache, A, T, MModel);
      else
	peel_internal_branch(b0, I, cache, A, T, transition_P[B0], MModel);
    }
    else
      std::abort();

    cache.validate_branch(b0);
    default_timer_stack.pop_timer();
  }


  /// Compute an ordered list of branches to process
  inline peeling_info get_branches(const Tree& T, const Likelihood_Cache& LC) 
  {
    //------- Get ordered list of not up_to_date branches ----------///
    peeling_info peeling_operations(T);

    vector<const_branchview> branches; branches.reserve(T.n_branches());
    append(T[LC.root].branches_in(),branches);

    for(int i=0;i<branches.size();i++) {
	const const_branchview& db = branches[i];
	if (not LC.up_to_date(db)) {
	  append(db.branches_before(),branches);
	  peeling_operations.push_back(db);
	}
    }

    std::reverse(peeling_operations.begin(),peeling_operations.end());

    return peeling_operations;
  }

  static 
  int calculate_caches(const alignment& A, subA_index_t& I, const MatCache& MC, const Tree& T,Likelihood_Cache& cache,
		       const MultiModel& MModel) {
    //---------- determine the operations to perform ----------------//
    peeling_info ops = get_branches(T, cache);

    //-------------- Compute the branch likelihoods -----------------//
    for(int i=0;i<ops.size();i++)
      peel_branch(ops[i],I,cache,A,T,MC,MModel);

    return ops.size();
  }

  int calculate_caches(const data_partition& P) {
    return calculate_caches(*P.A, *P.subA, P.MC, *P.T, P.LC, P.SModel());
  }

  Matrix get_rate_probabilities(const alignment& A,subA_index_t& I, const MatCache& MC,const Tree& T,
				Likelihood_Cache& cache,const MultiModel& MModel)
  {
    const alphabet& a = A.get_alphabet();

    const int root = cache.root;
    
    // make sure that we are up-to-date
    calculate_caches(A,I,MC,T,cache,MModel);

    // declare a matrix to store our results in
    Matrix probs(A.length(),MModel.n_base_models());

    // initialize the entries to prior probability of each sub-model
    for(int m=0;m<probs.size2();m++)
      for(int c=0;c<probs.size1();c++)
	probs(c,m) = MModel.distribution()[m];

    // compute root branches
    vector<int> rb;
    for(const_in_edges_iterator i = T[root].branches_in();i;i++)
      rb.push_back(*i);

    // get the index
    ublas::matrix<int> index = I.get_subA_index(root,A,T);

    // scratch matrix 
    Matrix & S = cache.scratch(0);
    const int n_models = S.size1();
    const int n_states    = S.size2();

    // cache matrix of frequencies
    Matrix F(n_models,n_states);
    WeightedFrequencyMatrix(F, MModel);

    const vector<unsigned>& smap = MModel.state_letters();

    for(int i=0;i<index.size1();i++) {
      double p_col = 0;
      for(int m=0;m<n_models;m++) {

	//-------------- Set letter & model prior probabilities  ---------------//
	for(int s=0;s<n_states;s++) 
	  S(m,s) = F(m,s);

	//-------------- Propagate and collect information at 'root' -----------//
	for(int j=0;j<rb.size();j++) {
	  int i0 = index(i,j);
	  if (i0 != alphabet::gap)
	    for(int s=0;s<n_states;s++) 
	      S(m,s) *= cache(i0,rb[j])(m,s);
	}

	//--------- If there is a letter at the root, condition on it ---------//
	if (root < T.n_leaves()) {
	  int rl = A.seq(root)[i];
	  if (a.is_letter_class(rl))
	    for(int s=0;s<n_states;s++)
	      if (not a.matches(smap[s],rl))
		S(m,s) = 0;
	}

	//--------- If there is a letter at the root, condition on it ---------//
	probs(i,m) = 0;
	for(int s=0;s<n_states;s++)
	  probs(i,m) += S(m,s);

	// A specific model (e.g. the INV model) could be impossible
	assert(0 <= probs(i,m) and probs(i,m) <= 1.00000000001);

	p_col += probs(i,m);
      }

      // SOME model must be possible
      assert(0 <= p_col and p_col <= 1.00000000001);
      for(int m=0;m<n_models;m++)
	probs(i,m) /= p_col;
    }
    return probs;
  }


  /// Find the probabilities of each letter at the root, given the data at the nodes in 'group'
  vector<Matrix>
  get_column_likelihoods(const data_partition& P, const vector<int>& b,
			 const vector<int>& req,const vector<int>& seq,int delta)
  {
    default_timer_stack.push_timer("substitution");
    default_timer_stack.push_timer("substitution::column_likelihoods");

    const alphabet& a = P.get_alphabet();

    const alignment& A = *P.A;
    const Tree& T = *P.T;
    Likelihood_Cache& LC = P.LC;
    subA_index_t& I = *P.subA;

#ifndef NDEBUG
    I.check_footprint(A, T);
    check_regenerate(I, A, T);
#endif

    //------ Check that all branches point to a 'root' node -----------//
    assert(b.size());
    int root = T.directed_branch(b[0]).target();
    for(int i=1;i<b.size();i++)
      assert(T.directed_branch(b[i]).target() == root);
    LC.root = root;

    ublas::matrix<int> index = I.get_subA_index_any(b,A,T,req,seq);

    IF_DEBUG(int n_br =) calculate_caches(P);
#ifndef NDEBUG
    std::clog<<"get_column_likelihoods: Peeled on "<<n_br<<" branches.\n";
#endif

    vector<Matrix> L;
    L.reserve(A.length()+2);

    Matrix& S = LC.scratch(0);
    const int n_models = S.size1();
    const int n_states = S.size2();

    //Add the padding matrices
    {
      element_assign(S,0);

      for(int i=0;i<delta;i++)
	L.push_back(S);
    }

    const vector<unsigned>& smap = P.SModel().state_letters();

    for(int i=0;i<index.size1();i++) {

      for(int m=0;m<n_models;m++) {
	for(int s=0;s<n_states;s++) 
	  S(m,s) = 1;

	//-------------- Propagate and collect information at 'root' -----------//
	for(int j=0;j<b.size();j++) {
	  int i0 = index(i,j);
	  if (i0 != alphabet::gap)
	    for(int s=0;s<n_states;s++) 
	      S(m,s) *= LC(i0,b[j])(m,s);
	}

	if (root < T.n_leaves()) {
	  int rl = A.seq(root)[i];
	  if (a.is_letter_class(rl))
	    for(int s=0;s<n_states;s++)
	      if (not a.matches(smap[s],rl))
		S(m,s) = 0;
	}
      }
      L.push_back(S);
    }
    default_timer_stack.pop_timer();
    default_timer_stack.pop_timer();

    return L;
  }

  efloat_t other_subst(const data_partition& P, const vector<int>& nodes) 
  {
    const alignment& A = *P.A;
    const Tree& T = *P.T;
    Likelihood_Cache& LC = P.LC;
    subA_index_t& I = *P.subA;

    default_timer_stack.push_timer("substitution");
    default_timer_stack.push_timer("substitution::other_subst");

    IF_DEBUG(int n_br =) calculate_caches(P);
#ifndef NDEBUG
    std::clog<<"other_subst: Peeled on "<<n_br<<" branches.\n";
#endif

    // compute root branches
    vector<int> rb;
    for(const_in_edges_iterator i = T[LC.root].branches_in();i;i++)
      rb.push_back(*i);

    // get the relationships with the sub-alignments
    ublas::matrix<int> index1 = I.get_subA_index_none(rb,A,T,nodes);
    efloat_t Pr1 = calc_root_probability(P,rb,index1);

#ifndef NDEBUG
    ublas::matrix<int> index2 = I.get_subA_index_any(rb,A,T,nodes);
    ublas::matrix<int> index  = I.get_subA_index(rb,A,T);

    efloat_t Pr2 = calc_root_probability(P,rb,index2);
    efloat_t Pr  = calc_root_probability(P,rb,index);

    assert(std::abs(log(Pr1 * Pr2) - log(Pr) ) < 1.0e-9);
#endif

    default_timer_stack.pop_timer();
    default_timer_stack.pop_timer();

    return Pr1;
  }

  bool check_equal(double x, double y)
  {
    return (std::abs(x-y) < std::min(x,y)*1.0e-9);
  }

  void compare_caches(const subA_index_t& I1, const subA_index_t& IF_DEBUG(I2),
		      const Likelihood_Cache& LC1, const Likelihood_Cache& LC2, int b)
  {
    int L = I1.branch_index_length(b);
    assert(L == I2.branch_index_length(b));

    const int n_models  = LC1.n_models();
    const int n_states  = LC1.n_states();

    bool equal = true;
    for(int i=0;i<L;i++) 
    {
      const Matrix& M1 = LC1(i,b);
      const Matrix& M2 = LC2(i,b);
      
      for(int m=0;m<n_models;m++) 
	for(int s1=0;s1<n_states;s1++)
	  equal = equal and check_equal(M1(m,s1), M2(m,s1));
    }
    if (equal)
      ; //std::cerr<<"branch "<<b<<": caches are equal"<<endl;
    else
      std::cerr<<"branch "<<b<<": caches are NOT equal"<<std::endl;
  }

  void compare_caches(const subA_index_t& I1, const subA_index_t& I2,
		      const Likelihood_Cache& LC1, const Likelihood_Cache& LC2, const Tree& T)
  {
    assert(LC1.root == LC2.root);
    
    vector<const_branchview> branches; branches.reserve(T.n_branches());
    append(T[LC1.root].branches_in(),branches);

    for(int i=0;i<branches.size();i++)
    {
	const const_branchview& db = branches[i];

	append(db.branches_before(),branches);

	if (LC1.up_to_date(db) and LC2.up_to_date(db))
	  compare_caches(I1,I2,LC1,LC2,db);
    }

  }

  ///
  /// This routine unaligns sub-columns that do not have a '+' at the base of the
  /// branch pointing to the substitution root.  (The sequence at the root node is
  /// ignored).  This routine allows us to estimate the likelihood an SPR move would
  /// have after all the necessary columns are unaligned to prevent + -> - -> +.
  ///
  /// This routine is called Pr_unaligned_root( ) because it assumed that unaligned can
  /// only happen at the substitution root.  This is actually true when called from
  /// the SPR_all routines, but may not make sense otherwise.
  ///
  efloat_t Pr_unaligned_root(const alignment& A,subA_index_t& I, const MatCache& MC,const Tree& T,Likelihood_Cache& LC,
			     const MultiModel& MModel)
  {
    total_likelihood++;
    default_timer_stack.push_timer("substitution");
    default_timer_stack.push_timer("substitution::likelihood_unaligned");

#ifndef NDEBUG
    I.check_footprint(A, T);
    check_regenerate(I, A, T, LC.root);
#endif

    IF_DEBUG(int n_br =) calculate_caches(A,I,MC,T,LC,MModel);
#ifndef NDEBUG
    std::clog<<"Pr: Peeled on "<<n_br<<" branches.\n";
#endif

    // compute root branches
    vector<int> rb;
    for(const_in_edges_iterator i = T[LC.root].branches_in();i;i++)
      rb.push_back(*i);

    ublas::matrix<int> index_aligned   = I.get_subA_index_aligned(rb,A,T,true);
    ublas::matrix<int> index_unaligned = I.get_subA_index_aligned(rb,A,T,false);

    // Combine the likelihoods from present nodes
    efloat_t Pr = calc_root_probability(A,T,LC,MModel,rb,index_aligned);
    // Combine the likelihoods from absent nodes
    Pr *= calc_root_probability_unaligned(A,T,LC,MModel,rb,index_unaligned);
    
#ifndef NDEBUG
    int n1 = n_non_null_entries(index_aligned);
    int l1 = n_non_empty_columns(index_aligned);

    int n2 = n_non_null_entries(index_unaligned);
    int l2 = n_non_empty_columns(index_unaligned);

    ublas::matrix<int> index = I.get_subA_index(rb,A,T);
    int n3 = n_non_null_entries(index);
    int l3 = n_non_empty_columns(index);

    int unaligned  = l1 + l2 - l3;

    assert(unaligned >= 0);
    std::cerr<<"     unaligned = "<<unaligned<<std::endl;

    // Each index for each branch should end up in exactly ONE of
    // index_aligned or index_unaligned.
    assert(n1 + n2 == n3);
    //    std::cerr<<"     n1 = "<<n1<<"    n2 = "<<n2<<std::endl;

    if (unaligned == 0) 
    {
      efloat_t Pr2 = calc_root_probability(A,T,LC,MModel,rb,index);
      assert(std::abs(Pr.log() - Pr2.log()) < 1.0e-9);
    }
#endif

    default_timer_stack.pop_timer();
    default_timer_stack.pop_timer();
    return Pr;
  }

  efloat_t Pr_unaligned_root(const data_partition& P,Likelihood_Cache& LC) {
    return Pr_unaligned_root(*P.A, *P.subA, P.MC, *P.T, LC, P.SModel());
  }

  efloat_t Pr_unaligned_root(const data_partition& P) {
    return Pr_unaligned_root(P, P.LC);
  }

  efloat_t Pr(const alignment& A,subA_index_t& I, const MatCache& MC,const Tree& T,Likelihood_Cache& LC,
	    const MultiModel& MModel)
  {
    total_likelihood++;
    default_timer_stack.push_timer("substitution");
    default_timer_stack.push_timer("substitution::likelihood");

#ifndef DEBUG_CACHING
    if (LC.cv_up_to_date()) {
#ifndef NDEBUG
      std::clog<<"Pr: Using cached value "<<log(LC.cached_value)<<"\n";
#endif
      default_timer_stack.pop_timer();
      default_timer_stack.pop_timer();
      return LC.cached_value;
    }
#endif

#ifndef NDEBUG
    I.check_footprint(A, T);
    check_regenerate(I, A, T, LC.root);
#endif

    IF_DEBUG(int n_br =) calculate_caches(A,I,MC,T,LC,MModel);
#ifndef NDEBUG
    std::clog<<"Pr: Peeled on "<<n_br<<" branches.\n";
#endif

    // compute root branches
    vector<int> rb;
    for(const_in_edges_iterator i = T[LC.root].branches_in();i;i++)
      rb.push_back(*i);

    // get the relationships with the sub-alignments
    ublas::matrix<int> index = I.get_subA_index(rb,A,T);

    // get the probability
    efloat_t Pr = calc_root_probability(A,T,LC,MModel,rb,index);

    LC.cached_value = Pr;
    LC.cv_up_to_date() = true;

    default_timer_stack.pop_timer();
    default_timer_stack.pop_timer();
    return Pr;
  }

  efloat_t Pr(const data_partition& P,Likelihood_Cache& LC) {
    return Pr(*P.A, *P.subA, P.MC, *P.T, LC, P.SModel());
  }

  efloat_t Pr(const data_partition& P) {
    efloat_t result = Pr(P, P.LC);

#ifdef DEBUG_CACHING
    data_partition P2 = P;
    P2.LC.invalidate_all();
    P2.invalidate_subA_index_all();
    for(int i=0;i<P2.T->n_branches();i++)
      P2.setlength(i,P2.T->branch(i).length());
    efloat_t result2 = Pr(P2, P2.LC);

    if (std::abs(log(result) - log(result2))  > 1.0e-9) {
      std::cerr<<"Pr: diff = "<<log(result)-log(result2)<<std::endl;
      compare_caches(*P.subA, *P2.subA, P.LC, P2.LC, *P.T);
      std::abort();
    }
#endif

    return result;
  }
}
