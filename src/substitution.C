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
#include "smodel/objects.H"
#include "matcache.H"
#include "rng.H"
#include <cmath>
#include <valarray>
#include <vector>
#include "timer_stack.H"
#include "alignment-util.H"
#include "util.H"

#ifdef NDEBUG
#define IF_DEBUG(x)
#else
#define IF_DEBUG(x) x
#endif

#ifndef DEBUG_SUBSTITUTION
#define IF_DEBUG_S(x)
#else
#define IF_DEBUG_S(x) x
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

  void WeightedFrequencyMatrix(Matrix& F, const MultiModelObject& MModel) 
  {
    // cache matrix of frequencies
    const int n_models = MModel.n_base_models();
    const int n_states = MModel.n_states();

    F.resize(n_models, n_states);

    for(int m=0;m<n_models;m++) {
      double p = MModel.distribution()[m];
      const valarray<double>& f = MModel.base_model(m).frequencies();
      for(int s=0;s<n_states;s++) 
	F(m,s) = f[s]*p;
    }
  }

  efloat_t calc_root_probability(const alignment&, const Tree& T,Likelihood_Cache& cache,
			       const MultiModelObject& MModel,const vector<int>& rb,const ublas::matrix<int>& index) 
  {
    total_calc_root_prob++;
    default_timer_stack.push_timer("substitution::calc_root");

    assert(index.size2() == rb.size());

    for(int i=0;i<rb.size();i++)
      assert(cache.up_to_date(rb[i]));

    const int root = cache.root;

    assert(T.directed_branch(rb[0]).target().name() == root);

    if (T[root].is_leaf_node())
      throw myexception()<<"Trying to accumulate conditional likelihoods at a leaf node is not allowed.";
    assert(rb.size() == 3);

    // scratch matrix 
    Matrix & S = cache.scratch(0);
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

      if (mi==3)
	p_col = element_prod_sum(F, *m[0], *m[1], *m[2]);
      else if (mi==2)
	p_col = element_prod_sum(F, *m[0], *m[1]);
      else if (mi==1)
	p_col = element_prod_sum(F, *m[0]);

#ifndef DEBUG_SUBSTITUTION
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

  efloat_t calc_root_probability2(const alignment&, const Tree& T,Likelihood_Cache& cache,
			       const MultiModelObject& MModel,const vector<int>& rb,const ublas::matrix<int>& index) 
  {
    total_calc_root_prob++;
    default_timer_stack.push_timer("substitution::calc_root");

    assert(index.size2() == rb.size());

    for(int i=0;i<rb.size();i++)
      assert(cache.up_to_date(rb[i]));

    const int root = cache.root;

    assert(T.directed_branch(rb[0]).target().name() == root);

    assert(rb.size() == 2);

    // scratch matrix 
    Matrix & S = cache.scratch(0);
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

      Matrix* m[2];
      int mi=0;

      if (i0 != -1)
	m[mi++] = &((*branch_cache[0])[i0]);
      if (i1 != -1)
	m[mi++] = &((*branch_cache[1])[i1]);

      if (mi==2)
	p_col = element_prod_sum(F, *m[0], *m[1]);
      else if (mi==1)
	p_col = element_prod_sum(F, *m[0]);

#ifndef DEBUG_SUBSTITUTION
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
					   const MultiModelObject& MModel,const vector<int>& rb,const ublas::matrix<int>& index) 
  {
    total_calc_root_prob++;
    default_timer_stack.push_timer("substitution::calc_root_unaligned");

    assert(index.size2() == rb.size());

    for(int i=0;i<rb.size();i++)
      assert(cache.up_to_date(rb[i]));

    if (T[cache.root].is_leaf_node())
      throw myexception()<<"Trying to accumulate conditional likelihoods at a leaf node is not allowed.";
    assert(rb.size() == 3);

    assert(T.directed_branch(rb[0]).target().name() == cache.root);

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
    return calc_root_probability(*P.A, *P.T_, P.LC, P.SModel(), rb, index);
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


  void peel_leaf_branch(int b0,subA_index_t& I, Likelihood_Cache& cache,
			const vector<int>& sequence, const alignment& A, const Tree& T, 
			const vector<Matrix>& transition_P,const MultiModelObject& MModel)
  {
    total_peel_leaf_branches++;
    default_timer_stack.push_timer("substitution::peel_leaf_branch");

    const alphabet& a = A.get_alphabet();

    if (not I.branch_index_valid(b0))
      I.update_branch(A,T,b0);

    //    const vector<unsigned>& smap = MModel.state_letters();

    // Do this before accessing matrices or other_subst
    cache.prepare_branch(b0);

    cache.set_length(I.branch_index_length(b0), b0);

    const int n_models  = cache.n_models();
    const int n_states  = cache.n_states();

    assert(MModel.n_states() == n_states);

    for(int i=0;i<I.branch_index_length(b0);i++)
    {
      Matrix& R = cache(i,b0);
      // compute the distribution at the parent node
      int l2 = sequence[i];

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

    cache[b0].other_subst = 1;

    default_timer_stack.pop_timer();
  }

  void FrequencyMatrix(Matrix& F, const MultiModelObject& MModel) 
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

  void peel_leaf_branch_F81(int b0, subA_index_t& I, Likelihood_Cache& cache,
			    const vector<int>& sequence, const alignment& A, const Tree& T, 
			    const MultiModelObject& MModel)
  {
    total_peel_leaf_branches++;
    default_timer_stack.push_timer("substitution::peel_leaf_branch");

    if (not I.branch_index_valid(b0))
      I.update_branch(A,T,b0);

    // Do this before accessing matrices or other_subst
    cache.prepare_branch(b0);

    cache.set_length(I.branch_index_length(b0), b0); 

    const alphabet& a = A.get_alphabet();

    const int n_models  = cache.n_models();
    const int n_states  = cache.n_states();

    assert(MModel.n_states() == n_states);

    //    const vector<unsigned>& smap = MModel.state_letters();

    vector<const F81_Object*> SubModels(n_models);
    for(int m=0;m<n_models;m++) {
      SubModels[m] = static_cast<const F81_Object*>(&MModel.base_model(m));
      assert(SubModels[m]);
    }
    const double t = T.directed_branch(b0).length();

    valarray<double> exp_a_t(n_models);
    for(int m=0;m<n_models;m++) 
      exp_a_t[m] = exp(-t * SubModels[m]->alpha_);

    Matrix& F = cache.scratch(1);
    FrequencyMatrix(F,MModel); // F(m,l2)

    for(int i=0;i<I.branch_index_length(b0);i++)
    {
      Matrix& R = cache(i,b0);
      // compute the distribution at the parent node
      int l2 = sequence[i];

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

    cache[b0].other_subst = 1;

    default_timer_stack.pop_timer();
  }

  void peel_leaf_branch_modulated(int b0,subA_index_t& I, Likelihood_Cache& cache, 
				  const vector<int>& sequence, const alignment& A, const Tree& T, 
				  const vector<Matrix>& transition_P,const MultiModelObject& MModel)
  {
    total_peel_leaf_branches++;
    default_timer_stack.push_timer("substitution::peel_leaf_branch");

    // Do this before accessing matrices or other_subst
    cache.prepare_branch(b0);

    cache.set_length(I.branch_index_length(b0), b0);

    const alphabet& a = A.get_alphabet();

    const int n_models  = cache.n_models();
    const int n_states  = cache.n_states();
    const int n_letters = a.n_letters();

    assert(n_states >= n_letters and n_states%n_letters == 0);

    assert(MModel.n_states() == n_states);

    if (not I.branch_index_valid(b0))
      I.update_branch(A,T,b0);

    const vector<unsigned>& smap = MModel.state_letters();

    for(int i=0;i<I.branch_index_length(b0);i++)
    {
      Matrix& R = cache(i,b0);
      // compute the distribution at the parent node
      int l2 = sequence[i];

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

    cache[b0].other_subst = 1;

    default_timer_stack.pop_timer();
  }

  /// Apply frequencies and collect probability for subA columns that go away on b.back()
  /// Question: can this routine handle both 
  /// (i) "going away" in terms of subA indices==-1 on b.back()
  ///    AND
  /// (ii) "going away" in terms of the node not being present at b.back().source()?
  ///
  /// Answer: Yes, because which columns "go away" is computed and then passed in via \a index.
  efloat_t collect_vanishing_internal(const vector<int>& b, ublas::matrix<int>& index, Likelihood_Cache& cache,
				      const MultiModelObject& MModel)
  {
    assert(b.size() == 3);
    assert(index.size2() == 2);

    // Both leaf branches must have valid caches
    assert(cache.up_to_date(b[0]) and cache.up_to_date(b[1]));

    // scratch matrix 
    const int n_models = cache.n_models();
    const int n_states = cache.n_states();

    // cache matrix F(m,s) of p(m)*freq(m,l)
    Matrix F(n_models,n_states);
    WeightedFrequencyMatrix(F, MModel);

    // look up the cache rows now, once, instead of for each column
    vector<Matrix>* branch_cache[2];
    for(int i=0;i<2;i++)
      branch_cache[i] = &cache[b[i]];
    
    efloat_t total = 1;
    for(int i=0;i<index.size1();i++)
    {
      double p_col = 1;

      int i0 = index(i,0);
      int i1 = index(i,1);

      if (i0 != alphabet::gap) 
      {
	assert(i1 == alphabet::gap);
	p_col = element_prod_sum(F, (*branch_cache[0])[i0] );
      }
      else if (i1 != alphabet::gap)
      {
	assert(i0 == alphabet::gap);
	p_col = element_prod_sum(F, (*branch_cache[1])[i1] );
      }

      // Situation: i0 ==-1 and i1 == -1
      // This situation should never come from subA_index_vanishing( ) (e.g. if subA_index_internal)
      // These kinds of columns ARE generated in subA_index_none ( ) (e.g. if subA_index_leaf)
      //
      // Perhaps we should screen them out in subA_index_none( ) and do 
      //   std::abort() 
      // in this case.

      // SOME model must be possible
      assert(0 <= p_col and p_col <= 1.00000000001);

      // This does a log( ) operation.
      total *= p_col;
      //      std::clog<<" i = "<<i<<"   p = "<<p_col<<"  total = "<<total<<"\n";
    }
    return cache[b[0]].other_subst * cache[b[1]].other_subst * total;
  }

  /// Get the total likelihood for columns behind b0 that have been deleted before b0.source (e.g. and so b0.source is -).
  efloat_t get_other_subst_behind_branch(int b0, const alignment& A, const Tree& T, subA_index_t& I, Likelihood_Cache& cache,
					 const MultiModelObject& MModel)
  {
    // This only makes sense if we have presence/absence information to sequences at internal nodes
    assert(A.n_sequences() == T.n_nodes());

    // There are no branches behind a leaf branch
    if (T.directed_branch(b0).source().is_leaf_node()) return 1;

    // find the names of the (two) branches behind b0
    vector<int> b;
    for(const_in_edges_iterator i = T.directed_branch(b0).branches_before();i;i++)
      b.push_back(*i);

    if (dynamic_cast<subA_index_leaf*>(&I))
    {
      // Get an alignment of subA indices on branches b[0] and b[1] where b0.source is not present
      int node = T.directed_branch(b0).source();

      ublas::matrix<int> index_vanishing = I.get_subA_index_none(b,A,T, vector<int>(1,node));

      b.push_back(b0);
      return collect_vanishing_internal(b, index_vanishing, cache, MModel);
    }
    else if (dynamic_cast<subA_index_internal*>(&I))
    {
      b.push_back(b0);
      ublas::matrix<int> index_vanishing = I.get_subA_index_vanishing(b,A,T);

      return collect_vanishing_internal(b, index_vanishing, cache, MModel);
    }
    else
      std::abort();
  }

  void peel_internal_branch(const vector<int>& b,ublas::matrix<int>& index, Likelihood_Cache& cache,
			    const vector<Matrix>& transition_P,const MultiModelObject& IF_DEBUG(MModel))
  {
    assert(b.size() == 3);

    assert(cache.up_to_date(b[0]) and cache.up_to_date(b[1]));

    // Do this before accessing matrices or other_subst
    cache.prepare_branch(b[2]);

    cache.set_length(index.size1(), b[2]);

    // scratch matrix
    Matrix& S = cache.scratch(0);
    const int n_models = cache.n_models();
    const int n_states = cache.n_states();
    assert(MModel.n_states() == n_states);

    // look up the cache rows now, once, instead of for each column
    vector< vector<Matrix>* > branch_cache;
    for(int i=0;i<b.size();i++)
      branch_cache.push_back(&cache[b[i]]);

    Matrix ones(n_models, n_states);
    element_assign(ones, 1);
    
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
	C = &ones;

      //      else
      //	std::abort(); // columns like this should not be in the index
      // Columns like this would not be in subA_index_leaf, but might be in subA_index_internal

      // propagate from the source distribution
      Matrix& R = (*branch_cache[2])[i];            //name the result matrix
      for(int m=0;m<n_models;m++) {
	
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

  void peel_internal_branch(int b0,subA_index_t& I, Likelihood_Cache& cache, const alignment& A, const Tree& T, 
			    const vector<Matrix>& transition_P,const MultiModelObject& MModel)
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
    // the call to I.get_subA-index_select ( ) updates the index for branches in b.
    assert(I.branch_index_valid(b0));

    /*-------------------- Do the peeling part------------- --------------------*/
    peel_internal_branch(b, index, cache, transition_P, MModel);

    /*-------------------- Do the other_subst collection part -------------------*/
    if (dynamic_cast<subA_index_internal*>(&I))
    {
      ublas::matrix<int> index_collect = I.get_subA_index_vanishing(b,A,T);
      cache[b[2]].other_subst = collect_vanishing_internal(b, index_collect, cache, MModel);
    }
    else if (dynamic_cast<subA_index_leaf*>(&I))
      cache[b0].other_subst = 1;
    else
      throw myexception()<<"subA_index_t is of unrecognized type!";

    default_timer_stack.pop_timer();
  }

  void peel_internal_branch_F81(int b0,subA_index_t& I, Likelihood_Cache& cache, const alignment& A, const Tree& T, 
				const MultiModelObject& MModel)
  {
    //    std::cerr<<"got here! (internal)"<<endl;
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

    assert(b.size() == 3);

    assert(cache.up_to_date(b[0]) and cache.up_to_date(b[1]));

    // Do this before accessing matrices or other_subst
    cache.prepare_branch(b0);

    cache.set_length(I.branch_index_length(b0), b0);

    // scratch matrix
    Matrix& S = cache.scratch(0);
    const int n_models = cache.n_models();
    const int n_states = cache.n_states();
    assert(MModel.n_states() == n_states);

    // look up the cache rows now, once, instead of for each column
    vector< vector<Matrix>* > branch_cache;
    for(int i=0;i<b.size();i++)
      branch_cache.push_back(&cache[b[i]]);
    
    vector<const F81_Object*> SubModels(n_models);
    for(int m=0;m<n_models;m++) {
      SubModels[m] = static_cast<const F81_Object*>(&MModel.base_model(m));
      assert(SubModels[m]);
    }
    const double t = T.directed_branch(b0).length();

    valarray<double> exp_a_t(n_models);
    for(int m=0;m<n_models;m++) 
      exp_a_t[m] = exp(-t * SubModels[m]->alpha_);

    Matrix& F = cache.scratch(1);
    FrequencyMatrix(F,MModel); // F(m,l2)

    Matrix ones(n_models, n_states);
    element_assign(ones, 1);
    
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
	C = &ones;

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

    /*-------------------- Do the other_subst collection part -------------b-------*/
    if (dynamic_cast<subA_index_internal*>(&I))
    {
      ublas::matrix<int> index_collect = I.get_subA_index_vanishing(b,A,T);
      cache[b[2]].other_subst = collect_vanishing_internal(b, index_collect, cache, MModel);
    }
    else if (dynamic_cast<subA_index_leaf*>(&I))
      cache[b0].other_subst = 1;
    else
      throw myexception()<<"subA_index_t is of unrecognized type!";

    default_timer_stack.pop_timer();
  }

  vector<Matrix>
  get_leaf_seq_likelihoods(const vector<int>& sequence, const alphabet& a, const MultiModelObject& MM, int n, int delta);


  void peel_branch(int b0,subA_index_t& I, Likelihood_Cache& cache, 
		   const vector< vector<int> >& sequences, const alignment& A, const Tree& T, 
		   const Mat_Cache& MC, const MultiModelObject& MModel)
  {
    total_peel_branches++;
    default_timer_stack.push_timer("substitution::peel_branch");

    // compute branches-in
    int bb = T.directed_branch(b0).branches_before().size();

    int B0 = T.directed_branch(b0).undirected_name();

    if (T.n_nodes() == 2 and b0 == 1)
    {
      assert(bb == 0);
      if (not I.branch_index_valid(b0))
	I.update_branch(A,T,b0);

      cache.prepare_branch(b0);
      cache.set_length(I.branch_index_length(b0), b0);

      vector<Matrix> L = get_leaf_seq_likelihoods(sequences[1], A.get_alphabet(), MModel, 1, 0);

      for(int i=0;i<I.branch_index_length(b0);i++)
	cache(i,b0) = L[i];
      cache[b0].other_subst = 1;
    }
    else if (bb == 0) {
      int n_states = cache.scratch(0).size2();
      int n_letters = A.get_alphabet().n_letters();
      if (n_states == n_letters) {
	if (dynamic_cast<const F81_Object*>(&MModel.base_model(0)))
	  peel_leaf_branch_F81(b0, I, cache, sequences[b0], A, T, MModel);
	else
	  peel_leaf_branch(b0, I, cache, sequences[b0], A, T, MC.transition_P(B0), MModel);
      }
      else
	peel_leaf_branch_modulated(b0, I, cache, sequences[b0], A, T, MC.transition_P(B0), MModel);
    }
    else if (bb == 2) {
      if (dynamic_cast<const F81_Object*>(&MModel.base_model(0)))
	peel_internal_branch_F81(b0, I, cache, A, T, MModel);
      else
	peel_internal_branch(b0, I, cache, A, T, MC.transition_P(B0), MModel);
    }
    else
      std::abort();

    cache.validate_branch(b0);
    default_timer_stack.pop_timer();
  }


  /// Compute an ordered list of branches to process
  inline peeling_info get_branches(const Tree& T, const Likelihood_Cache& LC, vector<const_branchview> branches) 
  {
    //------- Get ordered list of not up_to_date branches ----------///
    peeling_info peeling_operations(T);

    for(int i=0;i<branches.size();i++) 
    {
      const_branchview db = branches[i];
      if (not LC.up_to_date(db)) {
	append(db.branches_before(),branches);
	peeling_operations.push_back(db);
      }
    }

    std::reverse(peeling_operations.begin(),peeling_operations.end());

    return peeling_operations;
  }

  /// Compute an ordered list of branches to process to validate branch b
  inline peeling_info get_branches_for_branch(int b, const Tree& T, const Likelihood_Cache& LC) 
  {
    vector<const_branchview> branches(1,T.directed_branch(b)); branches.reserve(T.n_branches());

    return get_branches(T,LC,branches);
  }

  /// Compute an ordered list of branches to process
  inline peeling_info get_branches_for_node(int n, const Tree& T, const Likelihood_Cache& LC) 
  {
    vector<const_branchview> branches; branches.reserve(T.n_branches());
    append(T[n].branches_in(),branches);

    return get_branches(T, LC, branches);
  }

  static 
  int calculate_caches_for_node(int n, const vector< vector<int> >& sequences, const alignment& A, 
				subA_index_t& I, const Mat_Cache& MC, const Tree& T,Likelihood_Cache& cache,
				const MultiModelObject& MModel) 
  {
    //---------- determine the operations to perform ----------------//
    peeling_info ops = get_branches_for_node(n, T, cache);

    if (T.n_nodes() == 2)
    {
      assert(n == 1);
      ops.push_back(1);
    }

    // FIXME? Currently we require that ALL branches towards this node are up-to-date.
    // This is used in e.g. get_likelihoods_by_alignment_column( ) but isn't necessary for
    //  computing likelihoods: if just the branches pointing to the root are up-to-date, then
    //  we're OK.

    //-------------- Compute the branch likelihoods -----------------//
    for(int i=0;i<ops.size();i++)
      peel_branch(ops[i],I,cache,sequences,A,T,MC,MModel);

    return ops.size();
  }

  int calculate_caches_for_node(int n, const data_partition& P) {
    return calculate_caches_for_node(n, *P.sequences, *P.A, *P.subA, P, *P.T_, P.LC, P.SModel());
  }

  static 
  int calculate_caches_for_branch(int b, const vector< vector<int> >& sequences, const alignment& A, 
				  subA_index_t& I, const Mat_Cache& MC, const Tree& T,Likelihood_Cache& cache,
				  const MultiModelObject& MModel) 
  {
    //---------- determine the operations to perform ----------------//
    peeling_info ops = get_branches_for_branch(b, T, cache);

    //-------------- Compute the branch likelihoods -----------------//
    for(int i=0;i<ops.size();i++)
      peel_branch(ops[i],I,cache,sequences,A,T,MC,MModel);

    return ops.size();
  }

  /// Construct a likelihood matrix R(m,s) = Pr(observe letter l | model = m, state = 2)
  Matrix get_letter_likelihoods(int l, const alphabet& a, const MultiModelObject& MM)
  {
    assert(a.is_feature(l));

    const int n_letters = a.size();

    const int n_models = MM.n_base_models();
    const int n_states = MM.n_states();

    Matrix R(n_models,n_states);

    if (l == alphabet::not_gap)
    {
      element_assign(R,1);
      return R;
    }

    element_assign(R,0.0);

    const vector<unsigned>& smap = MM.state_letters();

    if (a.is_letter(l))
    {
      for(int m=0;m<n_models;m++)
	for(int s=0;s<n_states;s++)
	  if (l == smap[s])
	    R(m,s) = 1;
    }
    else if (a.is_letter_class(l))
    {
      for(int l2=0;l2<n_letters;l2++)
	if (a.matches(l,l2))
	  for(int m=0;m<n_models;m++)
	    for(int s=0;s<n_states;s++)
	      if (l2 == smap[s])
		R(m,s) = 1;
    }
    else
      std::abort();

    return R;
  }

  /// Get the likelihood matrix for each letter l of sequence n, where the likelihood matrix R(m,s) = Pr(observe letter l | model = m, state = 2)
  vector<Matrix>
  get_leaf_seq_likelihoods(const vector<int>& sequence, const alphabet& a, const MultiModelObject& MM, int n, int delta)
  {
    int L = sequence.size();

    const int n_letters = a.size();

    const int n_models = MM.n_base_models();
    const int n_states = MM.n_states();

    const vector<unsigned>& smap = MM.state_letters();

    // Compute the likelihood matrix just one for each letter (not letter classes)
    vector<Matrix> letter_likelihoods;
    for(int l=0;l<n_letters;l++)
      letter_likelihoods.push_back( get_letter_likelihoods(l, a, MM) );

    // Compute the likelihood matrices for each letter in the sequence
    vector<Matrix> likelihoods(L+delta, Matrix(n_models,n_states));

    for(int i=0;i<L;i++)
    {
      int letter = sequence[i];
      if (a.is_letter(letter))
	likelihoods[i+delta] = letter_likelihoods[letter];
      else
	likelihoods[i+delta] = get_letter_likelihoods(letter, a, MM);
    }

    return likelihoods;
  }

  vector<Matrix>
  get_leaf_seq_likelihoods(const data_partition& P, int n, int delta)
  {
    const vector<int>& sequence = (*P.sequences)[n];
    const alignment& A = *P.A;
    const alphabet& a = P.get_alphabet();
    const MultiModelObject& MM = P.SModel();
    return get_leaf_seq_likelihoods(sequence, a, MM, n, delta);
  }

  /// Find the probabilities of each PRESENT letter at the root, given the data at the nodes in 'group'
  vector<Matrix>
  get_column_likelihoods(const data_partition& P, const vector<int>& b,
			 const vector<int>& ordered_columns,int delta)
  {
    // FIXME - this now handles only internal sequences.  But see get_leaf_seq_likelihoods( ).
    default_timer_stack.push_timer("substitution");
    default_timer_stack.push_timer("substitution::column_likelihoods");

    const alphabet& a = P.get_alphabet();

    const alignment& A = *P.A;
    const Tree& T = *P.T_;
    Likelihood_Cache& LC = P.LC;
    subA_index_t& I = *P.subA;
    const MultiModelObject& MM = P.SModel();

#ifdef DEBUG_INDEXING
    I.check_footprint(A, T);
    check_regenerate(I, A, T);
#endif

    //------ Check that all branches point to a 'root' node -----------//
    assert(b.size());
    int root = T.directed_branch(b[0]).target();
    for(int i=1;i<b.size();i++)
      assert(T.directed_branch(b[i]).target() == root);
    LC.root = root;

    // select columns with at least one node in 'required_nodes', and re-order them according to the permutation 'ordered_columns'
    ublas::matrix<int> index = I.get_subA_index_columns(b,A,T,ordered_columns);

    IF_DEBUG_S(int n_br = ) calculate_caches_for_node(LC.root, P);

#ifdef DEBUG_SUBSTITUTION
    std::clog<<"get_column_likelihoods: Peeled on "<<n_br<<" branches.\n";
#endif

    vector<Matrix> L;
    L.reserve(A.length()+2);

    Matrix& S = LC.scratch(0);

    //Add the padding matrices
    {
      element_assign(S,0);

      for(int i=0;i<delta;i++)
	L.push_back(S);
    }

    const vector<unsigned>& smap = P.SModel().state_letters();

    // For each column in the index (e.g. for each present character at node 'root')
    for(int i=0;i<index.size1();i++) 
    {
      element_assign(S,1);

      // Note that we could do ZERO products in this loop
      for(int j=0;j<b.size();j++) 
      {
	int i0 = index(i,j);
	if (i0 == alphabet::gap) continue;

	element_prod_modify(S, LC(i0,b[j]) );
      }
      
      L.push_back(S);
    }
    default_timer_stack.pop_timer();
    default_timer_stack.pop_timer();

    return L;
  }

  /// Find the leaf branches of a connected subtree of nodes \a nodes instead of tree \a T
  vector<int> get_leaf_branches_from_subtree_nodes(const Tree& T, const vector<int>& nodes)
  {
    vector<int> branch_list;
    for(int i=0;i<nodes.size();i++)
    {
      const int n = nodes[i];
      vector<const_branchview> node_branches;
      append(T[n].branches_out(), node_branches);

      if (node_branches.size() == 1) {
	branch_list.push_back(node_branches[0]);
	continue;
      }

      assert(node_branches.size() == 3);
      int count = 0;
      int which = -1;
      for(int j=0;j<node_branches.size();j++)
      {
	int target = node_branches[j].target();
	if (includes(nodes, target))
	{
	  which = j;
	  count++;
	}
      }
      
      if (count == 1)
	branch_list.push_back(node_branches[which]);
      else
	assert(count == 3);
    }
    assert(branch_list.size() == 2 or branch_list.size() == 3 or branch_list.size() == 4);
    assert(nodes.size() == 2 or nodes.size() == 4 or nodes.size() == 6);

    return branch_list;
  }

  /// Get the total likelihood for columns behind b0 that have been deleted before b0.source (e.g. and so b0.source is -).
  efloat_t other_subst_behind_branch(int b0, const vector< vector<int> >& sequences, const alignment& A, const Tree& T, 
				     subA_index_t& I, Likelihood_Cache& LC,
				     const Mat_Cache& MC, const MultiModelObject& MModel)
  {
    if (LC.up_to_date(b0) and dynamic_cast<subA_index_internal*>(&I))
      return LC[b0].other_subst;

    for(const_in_edges_iterator j = T.directed_branch(b0).branches_before();j;j++)
      calculate_caches_for_branch(*j, sequences, A, I, MC, T, LC, MModel);

    return get_other_subst_behind_branch(b0, A, T, I, LC, MModel);
  }

  /// This routine requires that nodes denotes a connected subtree.
  /// 
  /// So, technically, we don't need to peel these columns all the way
  ///  up to the root even in the subA_index_leaf case...
  ///
  /// Instead we could simply apply equilibrium frequencies to them right
  /// at the leaf branches of the subtree.
  ///
  efloat_t other_subst(const data_partition& P, const vector<int>& nodes) 
  {
    const vector< vector<int> >& sequences = *P.sequences;
    const alignment& A = *P.A;
    const SequenceTree& T = *P.T_;
    const Mat_Cache& MC = P;
    Likelihood_Cache& LC = P.LC;
    subA_index_t& I = *P.subA;

    default_timer_stack.push_timer("substitution");
    default_timer_stack.push_timer("substitution::other_subst");

    // compute root branches
    vector<int> rb;
    for(const_in_edges_iterator i = T[LC.root].branches_in();i;i++)
      rb.push_back(*i);

    const MultiModelObject& MModel= P.SModel();
    vector<int> leaf_branch_list = get_leaf_branches_from_subtree_nodes(T,nodes);

    efloat_t Pr3 = 1;
    for(int i=0;i<leaf_branch_list.size();i++)
      Pr3 *= other_subst_behind_branch(leaf_branch_list[i], sequences, A, T, I, LC, MC, MModel);

#ifdef DEBUG_SUBSTITUTION
    int n_br = calculate_caches_for_node(LC.root, P);
    std::clog<<"other_subst: Peeled on "<<n_br<<" branches.\n";
#endif
#ifdef DEBUG_INDEXING
    //    std::cerr<<"other_subst: there are "<<leaf_branch_list.size()<<" subtree leaf branches."<<std::endl;
    //    std::cerr<<"other_subst: there are "<<nodes.size()<<" subtree nodes."<<std::endl;
    //    std::cerr<<A<<std::endl;
    //    std::cerr<<T<<std::endl;

    // What would index1 MEAN for subA_index_internal?
    // - It combines columns that are + at a node next to the root
    //   unless that column contains character in 'nodes'.
    //
    // - For subA_index_leaf, all leaf sub-columns are + everywhere, so it
    //   just means counts columns that don't contain a character in 'nodes'
    //
    // - For subA_index_internal, if would count sub-columns that contain
    //   a character next to the root, unless it also contains a character 
    //   'nodes'.  But it would also include other_subst for columns that
    //   might have a character in 'nodes', as long as they had been
    //   collected as other subst on branches before the ones leading to
    //   the root.
    //  
    //   Therefore, this doesn't make any sense for subA_index_internal,
    //   because of they way it handles other_subst.  Instead, we need
    //   include other subst only on branches that are not inside
    //   the subtree indicated by 'nodes'.  But that is already the
    //   complete calculation of other_subst, which does not really relate
    //   to this one.

    // get the relationships with the sub-alignments
    if (P.subA.as<subA_index_leaf>())
    {
      ublas::matrix<int> index1 = I.get_subA_index_none(rb,A,T,nodes);
      efloat_t Pr1 = calc_root_probability(P,rb,index1);
      assert(std::abs(log(Pr1) - log(Pr3) ) < 1.0e-9);

      ublas::matrix<int> index2 = I.get_subA_index_any(rb,A,T,nodes);
      ublas::matrix<int> index  = I.get_subA_index(rb,A,T);

      efloat_t Pr2 = calc_root_probability(P,rb,index2);
      efloat_t Pr  = calc_root_probability(P,rb,index);

      assert(std::abs(log(Pr1 * Pr2) - log(Pr) ) < 1.0e-9);
    }
#endif

    default_timer_stack.pop_timer();
    default_timer_stack.pop_timer();

    return Pr3;
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

    const int n_models = LC1.n_models();
    const int n_states = LC1.n_states();

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
      ; //std::cerr<<"branch "<<b<<": cached conditional likelihoods are equal"<<endl;
    else
      std::cerr<<"branch "<<b<<": cached conditional likelihoods are NOT equal"<<std::endl;

    efloat_t other_subst_current = LC1[b].other_subst;
    efloat_t other_subst_recomputed = LC2[b].other_subst;
    bool other_subst_equal = (std::abs(other_subst_current.log() - other_subst_recomputed.log()) < 1.0e-9);
    if (other_subst_equal)
      ; //std::cerr<<"branch "<<b<<": other_subst valies are equal"<<endl;
    else {
      std::cerr<<"branch "<<b<<": other_subst values are NOT equal:"<<
	" current = "<<other_subst_current.log()<<
	" recomputed = "<<other_subst_recomputed.log()<<
	" diff = "<<other_subst_recomputed.log() - other_subst_current.log()<<std::endl;
    }
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

  efloat_t branch_total(int b0, const subA_index_t& I, const Likelihood_Cache& cache, const MultiModelObject& MModel)
  {
    // scratch matrix 
    const int n_models = cache.n_models();
    const int n_states = cache.n_states();

    // cache matrix F(m,s) of p(m)*freq(m,l)
    Matrix F(n_models,n_states);
    WeightedFrequencyMatrix(F, MModel);

    ublas::matrix<int> index = I.get_subA_index(vector<int>(1,b0));

    efloat_t total = 1;
    for(int i=0;i<index.size1();i++)
    {
      double p_col = 1;

      int i0 = index(i,0);

      if (i0 != -1)
	p_col = element_prod_sum(F,cache[b0][i0]);

      // SOME model must be possible
      assert(0 <= p_col and p_col <= 1.00000000001);

      // This does a log( ) operation.
      total *= p_col;
      //      std::clog<<" i = "<<i<<"   p = "<<p_col<<"  total = "<<total<<"\n";
    }

    total *= cache[b0].other_subst;

    return total;
  }

  void compare_branch_totals(subA_index_t& I1, subA_index_t& I2,
			     Likelihood_Cache& LC1, Likelihood_Cache& LC2, const Tree& T,
			     const vector< vector<int> >& sequences, const alignment& A, const MultiModelObject& MModel)
  {
    assert(LC1.root == LC2.root);
    
    //---------- determine the operations to perform ----------------//
    vector<const_branchview> branches = branches_toward_node(T,LC1.root);

    //-------------- Compute the branch likelihoods -----------------//
    for(int i=0;i<branches.size();i++)
    {
      int b = branches[i];

      assert(LC1.up_to_date(b));
      assert(LC2.up_to_date(b));

      efloat_t branch_total1 = branch_total(b,I1,LC1,MModel);
      efloat_t other_subst1 = get_other_subst_behind_branch(b, A, T, I1, LC1, MModel);

      efloat_t branch_total2 = branch_total(b,I2,LC2,MModel);
      efloat_t other_subst2 = get_other_subst_behind_branch(b, A, T, I2, LC2, MModel);

      assert(std::abs(log(other_subst1) - log(other_subst2)) < 1.0e-9);
      assert(std::abs(log(branch_total1) - log(branch_total2)) < 1.0e-9);
    }
  }

  ///
  /// This routine unaligns sub-columns that do not have a '+' at the base of the
  /// branch pointing to the substitution root.  (The sequence at the root node is
  /// ignored).  This routine allows us to estimate the likelihood an SPR move would
  /// have after all the necessary columns are unaligned to prevent + -> - -> +.
  ///
  /// This routine is called Pr_unaligned_root( ) because it assumes that unaligned can
  /// only happen at the substitution root.  This is actually true when called from
  /// the SPR_all routines, but may not make sense otherwise.
  ///
  efloat_t Pr_unaligned_root(const vector< vector<int> >& sequences, const alignment& A,
			     subA_index_t& I, const Mat_Cache& MC,const Tree& T,Likelihood_Cache& LC,
			     const MultiModelObject& MModel)
  {
    total_likelihood++;
    default_timer_stack.push_timer("substitution");
    default_timer_stack.push_timer("substitution::likelihood_unaligned");

#ifdef DEBUG_INDEXING
    I.check_footprint(A, T);
    check_regenerate(I, A, T, LC.root);
#endif

    IF_DEBUG_S(int n_br = ) calculate_caches_for_node(LC.root, sequences,A,I,MC,T,LC,MModel);

#ifdef DEBUG_SUBSTITUTION
    std::clog<<"Pr: Peeled on "<<n_br<<" branches.\n";
#endif

    // compute root branches
    vector<int> rb;
    for(const_in_edges_iterator i = T[LC.root].branches_in();i;i++)
      rb.push_back(*i);

    // Combine the likelihoods from present nodes
    ublas::matrix<int> index_aligned   = I.get_subA_index_aligned(rb,A,T,true);
    efloat_t Pr = calc_root_probability(A,T,LC,MModel,rb,index_aligned);

    // FIXME - The problem is that this includes other_subst TWICE
    // Probably we need to factor other_subst collection out of calc_root_probability.
    ublas::matrix<int> index_unaligned = I.get_subA_index_aligned(rb,A,T,false);
    if (dynamic_cast<subA_index_leaf*>(&I))
    {
      // Combine the likelihoods from absent nodes
      Pr *= calc_root_probability_unaligned(A,T,LC,MModel,rb,index_unaligned);
    }
    
#ifdef DEBUG_INDEXING
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
    return Pr_unaligned_root(*P.sequences, *P.A, *P.subA, P, *P.T_, LC, P.SModel());
  }

  efloat_t Pr_unaligned_root(const data_partition& P) {
    return Pr_unaligned_root(P, P.LC);
  }

  efloat_t Pr(const vector< vector<int> >& sequences, const alignment& A,subA_index_t& I, 
	      const Mat_Cache& MC,const Tree& T,Likelihood_Cache& LC,
	      const MultiModelObject& MModel)
  {
    total_likelihood++;
    default_timer_stack.push_timer("substitution");
    default_timer_stack.push_timer("substitution::likelihood");

#ifndef DEBUG_CACHING
    if (LC.cv_up_to_date()) {
#ifdef DEBUG_CACHING
      std::clog<<"Pr: Using cached value "<<log(LC.cached_value)<<"\n";
#endif
      default_timer_stack.pop_timer();
      default_timer_stack.pop_timer();
      return LC.cached_value;
    }
#endif

#ifdef DEBUG_INDEXING
    I.check_footprint(A, T);
    check_regenerate(I, A, T, LC.root);
#endif

    IF_DEBUG_S(int n_br =) calculate_caches_for_node(LC.root, sequences, A,I,MC,T,LC,MModel);
#ifdef DEBUG_SUBSTITUTION
    std::clog<<"Pr: Peeled on "<<n_br<<" branches.\n";
#endif

    // compute root branches
    vector<int> rb;
    if (T.n_nodes() == 2)
    {
      // This is the 
      rb.push_back(0);
      rb.push_back(1);
    }
    else
    {
      for(const_in_edges_iterator i = T[LC.root].branches_in();i;i++)
	rb.push_back(*i);
    }

    // get the relationships with the sub-alignments
    ublas::matrix<int> index = I.get_subA_index(rb,A,T);

    // get the probability
    efloat_t Pr = 1;
    if (T.n_nodes() == 2)
      Pr = calc_root_probability2(A,T,LC,MModel,rb,index);
    else
      Pr = calc_root_probability(A,T,LC,MModel,rb,index);

    LC.cached_value = Pr;
    LC.cv_up_to_date() = true;

    default_timer_stack.pop_timer();
    default_timer_stack.pop_timer();
    return Pr;
  }

  efloat_t Pr(const data_partition& P,Likelihood_Cache& LC) 
  {
    return Pr(*P.sequences, *P.A, *P.subA, P, *P.T_, LC, P.SModel());
  }



  efloat_t Pr_from_scratch_leaf(data_partition P)
  {
    subA_index_leaf subA(P.A->length()+1, P.T_->n_branches()*2);

    Likelihood_Cache LC(*P.T_, P.SModel());
    LC.root = P.LC.root;

    return Pr(*P.sequences, *P.A, subA, P, *P.T_, LC, P.SModel());
  }



  efloat_t Pr_from_scratch_internal(data_partition P)
  {
    assert(P.variable_alignment());

    subA_index_internal subA(P.A->length()+1, P.T_->n_branches()*2);

    Likelihood_Cache LC(*P.T_, P.SModel());
    LC.root = P.LC.root;

    check_internal_nodes_connected(*P.A,*P.T_,vector<int>(1,LC.root));

    return Pr(*P.sequences, *P.A, subA, P, *P.T_, LC, P.SModel());
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
      compare_caches(*P.subA, *P2.subA, P.LC, P2.LC, *P.T_);
      std::abort();
    }
#endif

#ifdef DEBUG_INDEXING
    efloat_t result3 = Pr_from_scratch_leaf(P);

    if (P.variable_alignment())
    {
      efloat_t result4 = Pr_from_scratch_internal(P);

      compare_branch_totals(subA3,subA4,LC3,LC4, *P.T_, *P.A, P.SModel());
      assert(std::abs(log(result3) - log(result4)) < 1.0e-9);
    }

    assert(std::abs(log(result) - log(result3)) < 1.0e-9);
#endif

    return result;
  }

  efloat_t combine_likelihoods(const vector<Matrix>& likelihoods)
  {
    efloat_t Pr = 1;
    for(int i=0;i<likelihoods.size();i++)
      Pr *= element_sum(likelihoods[i]);
    return Pr;
  }


  vector<Matrix> 
  get_likelihoods_by_alignment_column(const vector< vector<int> >& sequences, const alignment& A,
				      subA_index_t& I, const Mat_Cache& MC,
				      const Tree& T,Likelihood_Cache& cache,const MultiModelObject& MModel)
  {
#ifdef DEBUG_INDEXING
    I.check_footprint(A, T);
    check_regenerate(I, A, T, cache.root);
#endif

    // Make sure that all conditional likelihoods have been calculated.
    IF_DEBUG_S(int n_br =) calculate_caches_for_node(cache.root, sequences, A,I,MC,T,cache,MModel);
#ifdef DEBUG_SUBSTITUTION
    std::clog<<"Pr: Peeled on "<<n_br<<" branches.\n";
#endif

    // Compute matrix F(m,s) = Pr(m)*Pr(s|m) = p(m)*freq(m,s) 
    Matrix F;
    WeightedFrequencyMatrix(F, MModel);

    // 1. Initialize with values that are true if all data is missing.
    vector<Matrix> likelihoods(A.length(), F);

    // 2. Record likelihoods for disappearing columns
    vector<const_branchview> branches = branches_toward_node(T, cache.root);

    IF_DEBUG_S(efloat_t other_subst1 = 1);
    /*
    for(int i=0;i<branches.size();i++)
    {
      int b = branches[i];

      // Get previous branches
      vector<int> prev;
      for(const_in_edges_iterator j = branches[i].branches_before();j;j++)
      {
	// update conditional likelihoods
	calculate_caches_for_branch(*j, sequences, A, I, MC, T, cache, MModel);

	// update substitution indices
	if (not I.branch_index_valid(*j))
	  I.update_branch(A,T,*j);

	// record branch
	prev.push_back(*j);
      }

      // Ignore leaf branches, since they columns don't disappear on leaf  branches.
      if (prev.size() == 0) continue;

      // Find the list of columns c where...
      for(int column=0;column<I.size1()-1;column++)
      {
	//  (a) this branch (e.g. b) has no index
	if (I(column+1,b) != alphabet::gap) continue;
	
	//  (b) at least one prev branch 'branch' has an index 'index'.
	for(int j=0;j<prev.size();j++)
	{
	  int branch = prev[j];
	  int index = I(column+1, branch);

	  if (index == alphabet::gap) continue;

	  element_prod_modify(likelihoods[column],cache(index,branch));

	  IF_DEBUG_S(other_subst1 *= element_sum(likelihoods[column]));
	  // We should never get here with subA_index_leaf.
	}
      }
    }

    // 3. Record likelihoods for columns that survive to the root.

    vector<int> root_branches;
    for(const_in_edges_iterator i = T[cache.root].branches_in();i;i++)
    {
      // update conditional likelihoods
      calculate_caches_for_branch(*i, sequences, A, I, MC, T, cache, MModel);

      // update substitution indices
      if (not I.branch_index_valid(*i))
	I.update_branch(A,T,*i);

      // record branch
      root_branches.push_back(*i);
    }

    for(int column=0;column<I.size1()-1;column++)
      for(int j=0;j<root_branches.size();j++)
      {
	int branch = root_branches[j];
	int index = I(column+1,branch);

	if (index == alphabet::gap) continue;

	element_prod_modify(likelihoods[column],cache(index,branch));
    }

    // Is there some way of iterating over matrices cache(index,branch) where EITHER
    // this (index,branch) goes away OR this branch.target() == cache.root ?
#ifdef DEBUG_SUBSTITUTION
    efloat_t other_subst2 = 1;
    for(int i=0;i<root_branches.size();i++)
    {
      assert(cache.up_to_date(root_branches[i]));
      efloat_t L1 = cache[root_branches[i]].other_subst;
      other_subst2 *= L1;
      //      efloat_t L2 = other_subst3[root_branches[i]];
      //      assert(std::abs(log(L2) - log(L1)) < 1.0e-9);
    }

    assert(std::abs(log(other_subst1) - log(other_subst2)) < 1.0e-9);
#endif
    */
    return likelihoods;
  }



  vector<Matrix> get_likelihoods_by_alignment_column(const data_partition& P)
  {
    vector<Matrix> likelihoods = get_likelihoods_by_alignment_column(*P.sequences, *P.A, *P.subA, P, *P.T_, P.LC, P.SModel());

#ifdef DEBUG_SUBSTITUTION
    efloat_t L1 = combine_likelihoods(likelihoods);
    efloat_t L2 = Pr_from_scratch_leaf(P);
    if (P.variable_alignment()) {
      efloat_t L3 = Pr_from_scratch_internal(P);
      assert(std::abs(log(L3) - log(L2)) < 1.0e-9);
    }

    assert(std::abs(log(L1) - log(L2)) < 1.0e-9);
#endif
    
    return likelihoods;
  }
  


  vector< vector<double> > get_model_likelihoods_by_alignment_column(const data_partition& P)
  {
    vector< vector<double> > model_likelihoods;
    
    vector<Matrix> likelihoods = get_likelihoods_by_alignment_column(P);
    
    for(int i=0; i<likelihoods.size(); i++)
    {
      int n_models = likelihoods[i].size1();
      int n_states = likelihoods[i].size2();
      
      vector<double> v(n_models,0);
      for(int m=0;m<n_models;m++)
      {
	double d = 0;
	for(int s=0;s<n_states;s++)
	  d += likelihoods[i](m,s);
	v[m] = d;
      }
      model_likelihoods.push_back(v);
    }

    return model_likelihoods;
  }

  vector< vector<double> > get_model_probabilities_by_alignment_column(const data_partition& P)
  {
    vector< vector<double> > probabilities = get_model_likelihoods_by_alignment_column(P);
    
    for(int i=0; i<probabilities.size(); i++)
    {
      double total = ::sum(probabilities[i]);

      int n_models = probabilities[i].size();

      for(int m=0;m<n_models;m++)
	probabilities[i][m] /= total;
    }

    return probabilities;
  }

}
