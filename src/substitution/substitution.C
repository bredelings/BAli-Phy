/*
   Copyright (C) 2004-2005,2007,2009-2016 Benjamin Redelings

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
#include "models/parameters.H"
#include "sequence/alphabet.H"
#include "rng.H"
#include <cmath>
#include <valarray>
#include <vector>
#include "util.H"
#include "dp/hmm.H"

// #define DEBUG_SUBSTITUTION
// #define DEBUG_CACHING

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
using std::pair;

// This file assumes that 
// * the matrix is reversible.  This means that we evaluate
//   frequencies at the root - even for insertions, where they actually
//   apply somewhere down the tree.
//
// * we don't need to work in log space for a single column
//
// * 

struct F81_Object: public Object
{
  double alpha_;
};

inline void element_assign(Matrix& M1,double d)
{
  M1.fill(d);
}

inline void element_assign(Matrix& M1,const Matrix& M2)
{
  assert(M1.size1() == M2.size1());
  assert(M1.size2() == M2.size2());
  
  const int size = M1.size();
  double * __restrict__ m1 = M1.begin();
  const double * __restrict__ m2 = M2.begin();
  
  for(int i=0;i<size;i++)
    m1[i] = m2[i];
}

inline void element_prod_modify(Matrix& M1,const Matrix& M2)
{
  assert(M1.size1() == M2.size1());
  assert(M1.size2() == M2.size2());
  
  const int size = M1.size();
  double * __restrict__ m1 = M1.begin();
  const double * __restrict__ m2 = M2.begin();
  
  for(int i=0;i<size;i++)
    m1[i] *= m2[i];
}

inline void element_prod_assign(Matrix& M1,const Matrix& M2,const Matrix& M3)
{
  assert(M1.size1() == M2.size1());
  assert(M1.size2() == M2.size2());
  
  assert(M1.size1() == M3.size1());
  assert(M1.size2() == M3.size2());
  
  const int size = M1.size();
  double * __restrict__ m1 = M1.begin();
  const double * __restrict__ m2 = M2.begin();
  const double * __restrict__ m3 = M3.begin();
  
  for(int i=0;i<size;i++)
    m1[i] = m2[i]*m3[i];
}

inline double element_sum(const Matrix& M1)
{
  const int size = M1.size();
  const double * __restrict__ m1 = M1.begin();
  
  double sum = 0;
  for(int i=0;i<size;i++)
    sum += m1[i];
  return sum;
}


inline double element_prod_sum(Matrix& M1,const Matrix& M2)
{
  assert(M1.size1() == M2.size1());
  assert(M1.size2() == M2.size2());
  
  const int size = M1.size();
  const double * __restrict__ m1 = M1.begin();
  const double * __restrict__ m2 = M2.begin();

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
  
  const int size = M1.size();
  const double * __restrict__ m1 = M1.begin();
  const double * __restrict__ m2 = M2.begin();
  const double * __restrict__ m3 = M3.begin();

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
  
  const int size = M1.size();
  const double * __restrict__ m1 = M1.begin();
  const double * __restrict__ m2 = M2.begin();
  const double * __restrict__ m3 = M3.begin();
  const double * __restrict__ m4 = M4.begin();

  double sum = 0;
  for(int i=0;i<size;i++)
    sum += m1[i] * m2[i] * m3[i] * m4[i];

  return sum;
}

pair<int,int> sample(const Matrix& M)
{
  double total = element_sum(M);
  double r = uniform()*total;

  double sum = 0;
  for(int m=0;m<M.size1();m++)
    for(int l=0;l<M.size2();l++)
    {
      sum += M(m,l);
      if (r <= sum)
	return {m,l};
    }
  return {-1,-1};
}

inline void element_assign(double* M1, int size, double d)
{
  for(int i=0;i<size;i++)
    M1[i] = d;
}

inline void element_assign(double* __restrict__ M1, const double* __restrict__ M2, int size)
{
  for(int i=0;i<size;i++)
    M1[i] = M2[i];
}

inline void element_prod_modify(double* __restrict__ M1, const double* __restrict__ M2, int size)
{
  for(int i=0;i<size;i++)
    M1[i] *= M2[i];
}

inline void element_prod_assign(double* __restrict__ M1,
				const double* __restrict__ M2,
				const double* __restrict__ M3, int size)
{
  for(int i=0;i<size;i++)
    M1[i] = M2[i]*M3[i];
}

inline double element_sum(const double* M1, int size)
{
  double sum = 0;
  for(int i=0;i<size;i++)
    sum += M1[i];
  return sum;
}


inline double element_prod_sum(const double* __restrict__ M1, const double* __restrict__ M2, int size)
{
  double sum = 0;
  for(int i=0;i<size;i++)
    sum += M1[i] * M2[i];

  return sum;
}

inline double element_prod_sum(const double* __restrict__ M1,
			       const double* __restrict__ M2,
			       const double* __restrict__ M3,
			       int size)
{
  double sum = 0;
  for(int i=0;i<size;i++)
    sum += M1[i] * M2[i] * M3[i];

  return sum;
}

inline double element_prod_sum(const double* __restrict__ M1,
			       const double* __restrict__ M2,
			       const double* __restrict__ M3,
			       const double* __restrict__ M4,
			       int size)
{
  double sum = 0;
  for(int i=0;i<size;i++)
    sum += M1[i] * M2[i] * M3[i] * M4[i];

  return sum;
}

int sample(const double* M, int size)
{
  double total = element_sum(M,size);
  double r = uniform()*total;
  double sum = 0;
  for(int i=0;i<size;i++)
  {
    sum += M[i];
    if (r <= sum)
      return i;
  }
  return -1;
}

pair<int,int> sample(const double* M, int n_models, int n_states)
{
  int size = n_models * n_states;
  int i = sample(M, size);
  if (i == -1)
    return {-1,-1};
  int m = i / n_states;
  int s = i % n_states;
  return {m,s};
}

namespace substitution {

  int total_peel_leaf_branches=0;
  int total_peel_internal_branches=0;
  int total_peel_branches=0;
  int total_likelihood=0;
  int total_calc_root_prob=0;

  inline double sum(const std::vector<double>& f,int l1,const alphabet& a)
  {
    double total=0;
    for(int l=0;l<a.size();l++)
      if (a.matches(l,l1))
	total += f[l];
    return total;
  }

  inline double sum(const std::valarray<double>& f,int l1,const alphabet& a)
  {
    double total=0;
    for(int l=0;l<a.size();l++)
      if (a.matches(l,l1))
	total += f[l];
    return total;
  }

  inline double sum(const Matrix Q,int l1, int l2, const alphabet& a)
  {
    double total=0;
    for(int l=0;l<a.size();l++)
      if (a.matches(l,l2))
	total += Q(l1,l);
    return total;
  }


  struct peeling_info: public vector<int> {
    peeling_info(const TreeInterface& t) { reserve(t.n_branches()); }
  };

  log_double_t calc_root_probability(const Likelihood_Cache_Branch* LCB1,
				     const Likelihood_Cache_Branch* LCB2,
				     const Likelihood_Cache_Branch* LCB3,
				     const pairwise_alignment_t& A1,
				     const pairwise_alignment_t& A2,
				     const pairwise_alignment_t& A3,
				     const Matrix& F)
  {
    total_calc_root_prob++;

    auto a10 = convert_to_bits(A1,1,0);
    auto a20 = convert_to_bits(A2,2,0);
    auto a30 = convert_to_bits(A3,3,0);
    auto a0123 = Glue_A(a10, Glue_A(a20,a30));
    auto index = get_indices_from_bitpath(a0123, {1,2,3});

    const int n_models = F.size1();
    const int n_states = F.size2();
    const int matrix_size = n_models * n_states;

    assert(n_models == LCB1->n_models());
    assert(n_states == LCB1->n_states());

    assert(n_models == LCB2->n_models());
    assert(n_states == LCB2->n_states());

    assert(n_models == LCB3->n_models());
    assert(n_states == LCB3->n_states());

    assert(index.size2() == 3);

#ifdef DEBUG_SUBSTITUTION
    // scratch matrix 
    Matrix S(n_models,n_states);
#endif

    log_double_t total = 1;
    for(int i=0;i<index.size1();i++)
    {
      double p_col = 1;

      int i0 = index(i,0);
      int i1 = index(i,1);
      int i2 = index(i,2);

      const double* m[3];
      int mi=0;

      if (i0 != -1)
	m[mi++] = ((*LCB1)[i0]);
      if (i1 != -1)
	m[mi++] = ((*LCB2)[i1]);
      if (i2 != -1)
	m[mi++] = ((*LCB3)[i2]);

      if (mi==3)
	p_col = element_prod_sum(F.begin(), m[0], m[1], m[2], matrix_size);
      else if (mi==2)
	p_col = element_prod_sum(F.begin(), m[0], m[1], matrix_size);
      else if (mi==1)
	p_col = element_prod_sum(F.begin(), m[0], matrix_size);

#ifdef DEBUG_SUBSTITUTION
      //-------------- Set letter & model prior probabilities  ---------------//
      element_assign(S,F);

      //-------------- Propagate and collect information at 'root' -----------//
      if (i0 != alphabet::gap)
	element_prod_modify(S.begin(),(*LCB1)[i0], matrix_size);
      if (i1 != alphabet::gap)
	element_prod_modify(S.begin(),(*LCB2)[i1], matrix_size);
      if (i2 != alphabet::gap)
	element_prod_modify(S.begin(),(*LCB3)[i2], matrix_size);

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

    total *= LCB1->other_subst;
    total *= LCB2->other_subst;
    total *= LCB3->other_subst;

    return total;
  }

  log_double_t calc_root_probability2(const data_partition& P,const vector<int>& rb,const matrix<int>& index) 
  {
    total_calc_root_prob++;

    assert(index.size2() == rb.size());

    //    assert(T.directed_branch(rb[0]).target().name() == cache.root);

    assert(rb.size() == 2);

    const int n_models = P.n_base_models();
    const int n_states = P.n_states();
    const int matrix_size = n_models * n_states;

#ifdef DEBUG_SUBSTITUTION    
    // scratch matrix
    Matrix S(n_models,n_states);
#endif    

    // cache matrix F(m,s) of p(m)*freq(m,l)
    Matrix F = P.WeightedFrequencyMatrix();

    // look up the cache rows now, once, instead of for each column
    auto& LCB1 = P.cache(rb[0]);
    auto& LCB2 = P.cache(rb[1]);
    
    log_double_t total = 1;
    for(int i=0;i<index.size1();i++)
    {
      double p_col = 1;

      int i0 = index(i,0);
      int i1 = index(i,1);

      const double* m[2];
      int mi=0;

      if (i0 != -1)
	m[mi++] = LCB1[i0];
      if (i1 != -1)
	m[mi++] = LCB2[i1];

      if (mi==2)
	p_col = element_prod_sum(F.begin(), m[0], m[1], matrix_size);
      else if (mi==1)
	p_col = element_prod_sum(F.begin(), m[0], matrix_size);

#ifdef DEBUG_SUBSTITUTION
      //-------------- Set letter & model prior probabilities  ---------------//
      element_assign(S,F);

      //-------------- Propagate and collect information at 'root' -----------//
      for(int j=0;j<rb.size();j++) {
	int i0 = index(i,j);
	if (i0 != alphabet::gap)
	  element_prod_modify(S.begin(), (*branch_cache[j])[i0], matrix_size);
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

    total *= LCB1.other_subst;
    total *= LCB2.other_subst;

    return total;
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


  Likelihood_Cache_Branch*
  peel_leaf_branch(const vector<int>& sequence, const alphabet& a, const vector<Matrix>& transition_P)
  {
    total_peel_leaf_branches++;

    //    const vector<unsigned>& smap = MC.state_letters();

    int L0 = sequence.size();

    const int n_models  = transition_P.size();
    const int n_states  = transition_P[0].size1();
    const int matrix_size = n_models * n_states;

    auto LCB = new Likelihood_Cache_Branch(L0, n_models, n_states);
    
    for(int i=0;i<L0;i++)
    {
      double* R = (*LCB)[i];
      // compute the distribution at the parent node
      int l2 = sequence[i];

      if (a.is_letter(l2))
	for(int m=0;m<n_models;m++) 
	{
	  const Matrix& Q = transition_P[m];
	  for(int s1=0;s1<n_states;s1++)
	    R[m*n_states + s1] = Q(s1,l2);
	}
      else if (a.is_letter_class(l2)) 
      {
	// FIXME - why is the sum(Q,l1,l2,a) function so much slower?
	// FIXME - would this slowness affect the modulated peeling functions also?
	const alphabet::fmask_t& fmask = a.letter_fmask(l2);
	for(int m=0;m<n_models;m++) 
	{
	  const Matrix& Q = transition_P[m];
	  for(int s1=0;s1<n_states;s1++)
	  {
	    double sum = 0.0;
	    for(int s2=0;s2<n_states;s2++)
	      sum += Q(s1,s2) * fmask[s2];
	    R[m*n_states + s1] = sum;
	  }
	}
      }
      else
	element_assign(R, matrix_size, 1);
    }

    LCB->other_subst = 1;

    return LCB;
  }

  vector<double> f81_exp_a_t(const data_partition& P, int b0, double L)
  {
    const int n_models  = P.n_base_models();
    //    const vector<unsigned>& smap = P.state_letters();

    vector<object_ptr<const F81_Object> > SubModels(n_models);
    for(int m=0;m<n_models;m++) {
      SubModels[m] = P.base_model(m,b0).assert_is_a<F81_Object>();
      assert(SubModels[m]);
    }
    //    const double L = t.branch_length(b0);

    vector<double> exp_a_t(n_models);
    for(int m=0;m<n_models;m++) 
      exp_a_t[m] = exp(-L * SubModels[m]->alpha_);

    return exp_a_t;
  }
  
  Likelihood_Cache_Branch*
  peel_leaf_branch_F81(const vector<int>& sequence, const alphabet& a, const vector<double>& exp_a_t, const Matrix& FF)
  {
    total_peel_leaf_branches++;

    // Do this before accessing matrices or other_subst
    int L0 = sequence.size();

    const int n_models  = exp_a_t.size();
    const int n_states  = a.n_letters();
    const int matrix_size = n_models * n_states;

    auto LCB = new Likelihood_Cache_Branch(L0, n_models, n_states);

    //    const vector<unsigned>& smap = P.state_letters();

    // This could be wrong, if the code below assumes row or column major incorrectly
    const double* F = FF.begin();

    for(int i=0;i<L0;i++)
    {
      double* R = (*LCB)[i];

      // compute the distribution at the parent node
      int l2 = sequence[i];

      if (a.is_letter(l2))
	for(int m=0;m<n_models;m++) {
	  double temp = (1.0-exp_a_t[m])*F[m*n_states + l2]; // move load out of loop for GCC vectorizer
	  for(int s1=0;s1<n_states;s1++)
	    R[m*n_states + s1] = temp;
	  R[m*n_states + l2] += exp_a_t[m];
	}
      else if (a.is_letter_class(l2)) 
      {
	for(int m=0;m<n_models;m++) 
	{
	  double sum=0;
	  for(int l=0;l<a.size();l++)
	    if (a.matches(l,l2))
	      sum += F[m*n_states + l];
	  double temp = (1.0-exp_a_t[m])*sum; // move load out of loop for GCC vectorizer
	  for(int s1=0;s1<n_states;s1++)
	    R[m*n_states + s1] = temp;
	  for(int l=0;l<a.size();l++)
	    if (a.matches(l,l2))
	      R[m*n_states + l] += exp_a_t[m];
	}
      }
      else
	element_assign(R, matrix_size, 1);
    }

    LCB->other_subst = 1;

    return LCB;
  }

  Likelihood_Cache_Branch*
  peel_leaf_branch_modulated(const vector<int>& sequence, const alphabet& a,
			     const vector<Matrix>& transition_P, const vector<unsigned>& smap)
  {
    total_peel_leaf_branches++;

    // Do this before accessing matrices or other_subst
    int L0 = sequence.size();

    const int n_models  = transition_P.size();
    const int n_states  = transition_P[0].size1();
    const int matrix_size = n_models * n_states;
    const int n_letters = a.n_letters();

    auto LCB = new Likelihood_Cache_Branch(L0, n_models, n_states);

    assert(n_states >= n_letters and n_states%n_letters == 0);

    for(int i=0;i<L0;i++)
    {
      double* R = (*LCB)[i];
      // compute the distribution at the parent node
      int l2 = sequence[i];

      if (a.is_letter(l2))
	for(int m=0;m<n_models;m++) {
	  const Matrix& Q = transition_P[m];
	  for(int s1=0;s1<n_states;s1++)
	    R[m*n_states + s1] = sum(Q,smap,n_letters,s1,l2);
	}
      else if (a.is_letter_class(l2)) {
	for(int m=0;m<n_models;m++) {
	  const Matrix& Q = transition_P[m];
	  for(int s1=0;s1<n_states;s1++)
	    R[m*n_states + s1] = sum(Q,smap,s1,l2,a);
	}
      }
      else
	element_assign(R, matrix_size, 1);
    }

    LCB->other_subst = 1;

    return LCB;
  }

  /// Apply frequencies and collect probability for subA columns that go away on b.back()
  /// Question: can this routine handle both 
  /// (i) "going away" in terms of subA indices==-1 on b.back()
  ///    AND
  /// (ii) "going away" in terms of the node not being present at b.back().source()?
  ///
  /// Answer: Yes, because which columns "go away" is computed and then passed in via \a index.
  log_double_t collect_vanishing_internal(const Likelihood_Cache_Branch* LCB1,
					  const Likelihood_Cache_Branch* LCB2,
					  const matrix<int>& index,
					  const Matrix& F)
  {
    assert(index.size2() == 2);

    // matrix F(m,s) is p(m)*freq(m,l)
    const int n_models = F.size1();
    const int n_states = F.size2();
    const int matrix_size = n_models * n_states;

    log_double_t total = 1;
    for(int i=0;i<index.size1();i++)
    {
      double p_col = 1;

      int i0 = index(i,0);
      int i1 = index(i,1);

      if (i0 != alphabet::gap) 
      {
	assert(i1 == alphabet::gap);
	p_col = element_prod_sum(F.begin(), (*LCB1)[i0], matrix_size );
      }
      else if (i1 != alphabet::gap)
      {
	assert(i0 == alphabet::gap);
	p_col = element_prod_sum(F.begin(), (*LCB2)[i1], matrix_size );
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
    return LCB1->other_subst * LCB2->other_subst * total;
  }

  Likelihood_Cache_Branch*
  peel_internal_branch(const Likelihood_Cache_Branch* LCB1,
		       const Likelihood_Cache_Branch* LCB2,
		       matrix<int>& index,
		       const vector<Matrix>& transition_P)
  {
    const int n_models = transition_P.size();
    const int n_states = transition_P[0].size1();
    const int matrix_size = n_models * n_states;
    
    // Do this before accessing matrices or other_subst
    auto* LCB3 = new Likelihood_Cache_Branch(index.size1(), n_models, n_states);

    // scratch matrix
    double* S = LCB3->scratch(0);

    Matrix ones(n_models, n_states);
    element_assign(ones, 1);
    
    for(int i=0;i<index.size1();i++) 
    {
      // compute the source distribution from 2 branch distributions
      int i0 = index(i,0);
      int i1 = index(i,1);

      const double* C = S;
      if (i0 != alphabet::gap and i1 != alphabet::gap)
	element_prod_assign(S, (*LCB1)[i0], (*LCB2)[i1], matrix_size);
      else if (i0 != alphabet::gap)
	C = (*LCB1)[i0];
      else if (i1 != alphabet::gap)
	C = (*LCB2)[i1];
      else
	C = ones.begin();

      //      else
      //	std::abort(); // columns like this should not be in the index
      // Columns like this would not be in subA_index_leaf, but might be in subA_index_internal

      // propagate from the source distribution
      double* R = (*LCB3)[i];            //name the result matrix
      for(int m=0;m<n_models;m++) {
	
	const Matrix& Q = transition_P[m];
	
	// compute the distribution at the target (parent) node - multiple letters
	for(int s1=0;s1<n_states;s1++) {
	  double temp=0;
	  for(int s2=0;s2<n_states;s2++)
	    temp += Q(s1,s2)*C[m*n_states + s2];
	  R[m*n_states + s1] = temp;
	}
      }
    }

    return LCB3;
  }
  
  Likelihood_Cache_Branch*
  peel_internal_branch(const Likelihood_Cache_Branch* LCB1,
		       const Likelihood_Cache_Branch* LCB2,
		       const pairwise_alignment_t& A0,
		       const pairwise_alignment_t& A1,
		       const vector<Matrix>& transition_P,
		       const Matrix& F)
  {
    total_peel_internal_branches++;

    auto a0 = convert_to_bits(A0, 0, 2);
    auto a1 = convert_to_bits(A1, 1, 2);
    auto a012 = Glue_A(a0, a1);

    // get the relationships with the sub-alignments for the (two) branches behind b0
    matrix<int> index = get_indices_from_bitpath_w(a012, {0,1}, 1<<2);

    /*-------------------- Do the peeling part------------- --------------------*/
    auto LCB3 = peel_internal_branch(LCB1, LCB2, index, transition_P);

    /*-------------------- Do the other_subst collection part -------------------*/
    matrix<int> index_collect = get_indices_from_bitpath_wo(a012, {0,1}, 1<<2);
    LCB3->other_subst = collect_vanishing_internal(LCB1, LCB2, index_collect, F);

    return LCB3;
  }

  Likelihood_Cache_Branch*
  peel_internal_branch_F81(const Likelihood_Cache_Branch* LCB1,
			   const Likelihood_Cache_Branch* LCB2,
			   const pairwise_alignment_t& A0,
			   const pairwise_alignment_t& A1,
			   const vector<double>& exp_a_t,
			   const Matrix& FF,
			   const Matrix& WF)
  {
    //    std::cerr<<"got here! (internal)"<<endl;
    total_peel_internal_branches++;

    auto a0 = convert_to_bits(A0, 0, 2);
    auto a1 = convert_to_bits(A1, 1, 2);
    auto a012 = Glue_A(a0, a1);

    // get the relationships with the sub-alignments for the (two) branches behind b0
    matrix<int> index = get_indices_from_bitpath_w(a012, {0,1}, 1<<2);

    const double* F = FF.begin();

    const int n_models = FF.size1();
    const int n_states = FF.size2();
    const int matrix_size = n_models * n_states;

    auto LCB3 = new Likelihood_Cache_Branch(index.size1(), n_models, n_states);

    // scratch matrix
    double* S = LCB3->scratch(0);

    Matrix ones(n_models, n_states);
    element_assign(ones, 1);
    
    for(int i=0;i<index.size1();i++) 
    {
      // compute the source distribution from 2 branch distributions
      int i0 = index(i,0);
      int i1 = index(i,1);

      const double* C = S;
      if (i0 != alphabet::gap and i1 != alphabet::gap)
	element_prod_assign(S, (*LCB1)[i0], (*LCB2)[i1], matrix_size);
      else if (i0 != alphabet::gap)
	C = (*LCB1)[i0];
      else if (i1 != alphabet::gap)
	C = (*LCB2)[i1];
      else
	C = ones.begin();

      // propagate from the source distribution
      double* R = (*LCB3)[i];            //name the result matrix
      for(int m=0;m<n_models;m++) 
      {
	// compute the distribution at the target (parent) node - multiple letters

	//  sum = (1-exp(-a*t))*(\sum[s2] pi[s2]*L[s2])
	double sum = 0;
	for(int s2=0;s2<n_states;s2++)
	  sum += F[m*n_states + s2]*C[m*n_states + s2];
	sum *= (1.0 - exp_a_t[m]);

	// L'[s1] = exp(-a*t)L[s1] + sum
	double temp = exp_a_t[m]; //move load out of loop for GCC 4.5 vectorizer.
	for(int s1=0;s1<n_states;s1++) 
	  R[m*n_states + s1] = temp*C[m*n_states + s1] + sum;
      }
    }

    /*-------------------- Do the other_subst collection part -------------b-------*/
    matrix<int> index_collect = get_indices_from_bitpath_wo(a012, {0,1}, 1<<2);
    LCB3->other_subst = collect_vanishing_internal(LCB1, LCB2, index_collect, WF);
    return LCB3;
  }

  vector<Matrix>
  get_leaf_seq_likelihoods(const vector<int>& sequence, const alphabet& a, const data_partition& P, int delta);


  /// Construct a likelihood matrix R(m,s) = Pr(observe letter l | model = m, state = 2)
  Matrix get_letter_likelihoods(int l, const alphabet& a, const data_partition& P)
  {
    assert(a.is_feature(l));

    const int n_letters = a.size();

    const int n_models = P.n_base_models();
    const int n_states = P.n_states();

    Matrix R(n_models,n_states);

    if (l == alphabet::not_gap)
    {
      element_assign(R,1);
      return R;
    }

    element_assign(R,0.0);

    const vector<unsigned>& smap = P.state_letters();

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

  /// Get the likelihood matrix for each letter l of sequence 'sequence', where the likelihood matrix R(m,s) = Pr(observe letter l | model = m, state = 2)
  vector<Matrix>
  get_leaf_seq_likelihoods(const vector<int>& sequence, const alphabet& a, const data_partition& P, int delta)
  {
    int L = sequence.size();

    const int n_letters = a.size();

    const int n_models = P.n_base_models();
    const int n_states = P.n_states();

    // Compute the likelihood matrix just one for each letter (not letter classes)
    vector<Matrix> letter_likelihoods;
    for(int l=0;l<n_letters;l++)
      letter_likelihoods.push_back( get_letter_likelihoods(l, a, P) );

    // Compute the likelihood matrices for each letter in the sequence
    vector<Matrix> likelihoods(L+delta, Matrix(n_models,n_states));

    for(int i=0;i<L;i++)
    {
      int letter = sequence[i];
      if (a.is_letter(letter))
	likelihoods[i+delta] = letter_likelihoods[letter];
      else
	likelihoods[i+delta] = get_letter_likelihoods(letter, a, P);
    }

    return likelihoods;
  }

  vector<Matrix>
  get_leaf_seq_likelihoods(const data_partition& P, int n, int delta)
  {
    const vector<int>& sequence = P.get_sequence(n);
    const alphabet& a = P.get_alphabet();
    return get_leaf_seq_likelihoods(sequence, a, P, delta);
  }

  /// Find the probabilities of each PRESENT letter at the root, given the data at the nodes in 'group'
  vector<Matrix>
  get_column_likelihoods(const data_partition& P, const vector<int>& b, const matrix<int>& index, int delta)
  {
    // FIXME - this now handles only internal sequences.  But see get_leaf_seq_likelihoods( ).
    auto t = P.t();

    //------ Check that all branches point to a 'root' node -----------//
    assert(b.size());
    //    assert(not t.is_leaf_node(P.subst_root()));
    int root = t.target(b[0]);
    for(int i=1;i<b.size();i++)
      assert(t.target(b[i]) == root);

    assert(not t.is_leaf_node(P.subst_root()));

    vector<Matrix> L;
    L.reserve(index.size1()+2);

    const int n_models = P.n_base_models();
    const int n_states = P.n_states();
    const int matrix_size = n_models * n_states;
    Matrix S(n_models, n_states);

    //Add the padding matrices
    {
      element_assign(S,0);

      for(int i=0;i<delta;i++)
	L.push_back(S);
    }

    vector<const Likelihood_Cache_Branch*> cache;
    for(int branch: b)
      cache.push_back(&P.cache(branch));

    // For each column in the index (e.g. for each present character at node 'root')
    for(int i=0;i<index.size1();i++) 
    {
      element_assign(S,1);

      // Note that we could do ZERO products in this loop
      for(int j=0;j<b.size();j++) 
      {
	int i0 = index(i,j);
	if (i0 == alphabet::gap) continue;

	element_prod_modify(S.begin(), (*cache[j])[i0], matrix_size);
      }
      
      L.push_back(S);
    }

    return L;
  }

  /// Find the leaf branches of a connected subtree of nodes \a nodes instead of tree \a T
  vector<int> get_leaf_branches_from_subtree_nodes(const TreeInterface& t, const vector<int>& nodes)
  {
    vector<int> branch_list;
    for(int n:nodes)
    {
      vector<int> node_branches = t.branches_out(n);

      if (node_branches.size() == 1) {
	branch_list.push_back(node_branches[0]);
	continue;
      }

      assert(node_branches.size() == 3);
      int count = 0;
      int which = -1;
      for(int j=0;j<node_branches.size();j++)
      {
	int target = t.target(node_branches[j]);
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

  /// This routine requires that nodes denotes a connected subtree.
  /// 
  /// So, technically, we don't need to peel these columns all the way
  ///  up to the root even in the subA_index_leaf case...
  ///
  /// Instead we could simply apply equilibrium frequencies to them right
  /// at the leaf branches of the subtree.
  ///
  log_double_t other_subst(const data_partition& P, const vector<int>& nodes) 
  {
    auto t = P.t();

    // compute root branches
    vector<int> rb = t.branches_in(P.subst_root());

    vector<int> leaf_branch_list = get_leaf_branches_from_subtree_nodes(t,nodes);

    log_double_t Pr3 = 1;
    for(int b: leaf_branch_list)
      Pr3 *= P.cache(b).other_subst;

    return Pr3;
  }

  log_double_t combine_likelihoods(const vector<Matrix>& likelihoods)
  {
    log_double_t Pr = 1;
    for(int i=0;i<likelihoods.size();i++)
      Pr *= element_sum(likelihoods[i]);
    return Pr;
  }

  vector<vector<pair<int,int>>> 
  sample_subst_history(const data_partition& P, const TreeInterface& t)
  {
    const vector<unsigned>& smap = P.state_letters();

    const int n_models = P.n_base_models();
    const int n_states = P.n_states();
    const int matrix_size = n_models * n_states;

    // scratch matrix 
    Matrix S(n_models, n_states);

    int root = P.subst_root();

    // Compute matrix F(m,s) = Pr(m)*Pr(s|m) = p(m)*freq(m,s) 
    Matrix F = P.WeightedFrequencyMatrix();

    // 1. Allocate arrays for storing results and temporary results.
    vector<vector<pair<int,int> > > ancestral_characters (t.n_nodes());
    vector<vector<pair<int,int> > > subA_index_parent_characters (t.n_branches()*2);
    
    // All the (-1,-1)'s should be overwritten with the sampled character.
    for(int i=0;i<t.n_nodes();i++)
      ancestral_characters[i] = vector<pair<int,int>>(P.seqlength(i), {-1,-1});

    {
      // compute root branches
      vector<int> rb;
      for(int b: t.branches_in(root))
      {
	rb.push_back(b);

	subA_index_parent_characters[b] = vector<pair<int,int>>(P.seqlength(P.t().source(b)), {-1,-1});
      }

      const auto& cache0 = P.cache(rb[0]);
      const auto& cache1 = P.cache(rb[1]);
      const auto& cache2 = P.cache(rb[2]);

      vector<int> nodes = {root};
      auto a10 = convert_to_bits(P.get_pairwise_alignment(rb[0]),1,0);
      auto a20 = convert_to_bits(P.get_pairwise_alignment(rb[1]),2,0);
      auto a30 = convert_to_bits(P.get_pairwise_alignment(rb[2]),3,0);
      auto a0123 = Glue_A(a10, Glue_A(a20,a30));
      auto index = get_indices_from_bitpath(a0123, {1,2,3,0});

      // FIXME - this doesn't handle case where tree has only 2 leaves.
      for(int i=0;i<index.size1();i++)
      {
	int i0 = index(i,0);
	int i1 = index(i,1);
	int i2 = index(i,2);

	S = F;

	if (i0 != -1)
	  element_prod_modify(S.begin(), cache0[i0], matrix_size);
	if (i1 != -1)
	  element_prod_modify(S.begin(), cache1[i1], matrix_size);
	if (i2 != -1)
	  element_prod_modify(S.begin(), cache2[i2], matrix_size);

	pair<int,int> state_model = sample(S);

	{
	  int ii = index(i,3);
	  if(ii != -1)
	    ancestral_characters[root][ii] = state_model;
	}

	if (i0 != -1)
	  subA_index_parent_characters[rb[0]][i0] = state_model;
	if (i1 != -1)
	  subA_index_parent_characters[rb[1]][i1] = state_model;
	if (i2 != -1)
	  subA_index_parent_characters[rb[2]][i2] = state_model;
      }
    }

    vector<int> branches = t.all_branches_toward_node(P.subst_root());
    std::reverse(branches.begin(), branches.end());

    for(int b: branches)
    {
      int node = t.source(b);
      auto& sequence = P.get_sequence(node);

      const vector<Matrix>& transition_P = P.transition_P(b);

      vector<int> local_branches = {b};
      for(int b: t.branches_before(b))
      {
	local_branches.push_back(b);

	subA_index_parent_characters[b] = vector<pair<int,int>>(P.seqlength(P.t().source(b)), {-1,-1});
      }

      assert(local_branches.size() == 3 or local_branches.size() == 1);
      const auto& cache1 = P.cache(local_branches[1]);
      const auto& cache2 = P.cache(local_branches[2]);

      matrix<int> index;
      if (local_branches.size() == 1)
	index = get_indices_n(P.seqlength(P.t().source(b)));
      else
      {
	auto a1 = convert_to_bits(P.get_pairwise_alignment(local_branches[1]),1,0);
	auto a2 = convert_to_bits(P.get_pairwise_alignment(local_branches[2]),2,0);
	auto a012 = Glue_A(a1,a2);
	index = get_indices_from_bitpath(a012,{0,1,2});
      }
      
      for(int i=0;i<index.size1();i++)
      {
	int i0 = index(i,0);

	int i1 = -1;
	int i2 = -1;

	if (local_branches.size() == 3)
	{
	  i1 = index(i,1);
	  i2 = index(i,2);
	}

	// If there IS no parent character, then we can sample from F
	if (i0 == -1)
	  S = F;
	// If there is a parent character, then it MUST have an (l,m) pair.
	// This is because it was incoming-present
	else
	{
	  pair<int,int> state_model_parent = subA_index_parent_characters[b][i0];
	  int mp = state_model_parent.first;
	  int lp = state_model_parent.second;
	  assert(mp != -1);
	  element_assign(S,0);

	  const Matrix& Q = transition_P[mp];

	  for(int l=0;l<n_states;l++)
	    S(mp,l) = Q(lp,l);
	}

	if (local_branches.size() == 1)
	{
	  int ii = index(i,0);
	  int l = sequence[ii];
	  const alphabet& a = P.get_alphabet();
	  if (l == alphabet::not_gap)
	    ;
	  else if (a.is_letter(l))
	  {
	    // Clear S(m,s) for every state s that doesn't map to the observed letter l
	    for(int s=0;s<n_states;s++)
	      if (smap[s] != l)
		for(int m=0;m<n_models;m++)
		  S(m,s) = 0;
	  }
	  else
	  {
	    assert(a.is_letter_class(l));
	    alphabet::bitmask_t letters = a.letter_mask(l);
	    for(int l=0;l<letters.size();l++)
	      if (letters.test(l))
		for(int s=0;s<n_states;s++)
		  if (smap[s] != l)
		    for(int m=0;m<n_models;m++)
		      S(m,s) = 0;
	  }
	}
	
	if (i1 != -1)
	  element_prod_modify(S.begin(), cache1[i1], matrix_size);
	if (i2 != -1)
	  element_prod_modify(S.begin(), cache2[i2], matrix_size);
	
	pair<int,int> state_model = sample(S);
	
	{
	  int ii = index(i,0);
	  if (ii != -1)
	    ancestral_characters[node][ii] = state_model;
	}
	
	if (i1 != -1)
	  subA_index_parent_characters[local_branches[1]][i1] = state_model;
	if (i2 != -1)
	  subA_index_parent_characters[local_branches[2]][i2] = state_model;
      }
    }

    return ancestral_characters;
  }

  vector<vector<pair<int,int>>> sample_ancestral_states(const data_partition& P)
  {
    return sample_subst_history(P, P.t());
  }

  vector<Matrix> get_likelihoods_by_alignment_column(const data_partition&)
  {
    std::abort();
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
