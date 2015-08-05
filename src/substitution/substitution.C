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
#include "matcache.H"
#include "rng.H"
#include <cmath>
#include <valarray>
#include <vector>
#include "alignment/alignment-util.H"
#include "alignment/alignment-util2.H"
#include "util.H"

// #define DEBUG_SUBSTITUTION
// #define DEBUG_CACHING
// #define DEBUG_INDEXING

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

namespace substitution {

  int total_peel_leaf_branches=0;
  int total_peel_internal_branches=0;
  int total_peel_branches=0;
  int total_likelihood=0;
  int total_calc_root_prob=0;

  struct peeling_info: public vector<int> {
    peeling_info(const TreeInterface& t) { reserve(t.n_branches()); }
  };

  log_double_t calc_root_probability(const TreeInterface& t,Likelihood_Cache& cache,
				     const Mat_Cache& MC,const vector<int>& rb,const matrix<int>& index) 
  {
    total_calc_root_prob++;

    assert(index.size2() == rb.size());

    for(int i=0;i<rb.size();i++)
      assert(cache.up_to_date(rb[i]));

    const int root = cache.root;

    assert(t.target(rb[0]) == root);

    if (t.is_leaf_node(root))
      throw myexception()<<"Trying to accumulate conditional likelihoods at a leaf node is not allowed.";
    assert(rb.size() == 3);

    // scratch matrix 
#ifdef DEBUG_SUBSTITUTION
    Matrix & S = cache.scratch(0);
#endif
    const int n_models = cache.n_models();
    const int n_states = cache.n_states();

    // cache matrix F(m,s) of p(m)*freq(m,l)
    Matrix F(n_models,n_states);
    MC.WeightedFrequencyMatrix(F);

    // look up the cache rows now, once, instead of for each column
    vector< vector<Matrix>* > branch_cache;
    for(int i=0;i<rb.size();i++)
      branch_cache.push_back(&cache[rb[i]]);
    
    log_double_t total = 1;
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

#ifdef DEBUG_SUBSTITUTION
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

    return total;
  }

  log_double_t calc_root_probability2(Likelihood_Cache& cache, const Mat_Cache& MC,const vector<int>& rb,const matrix<int>& index) 
  {
    total_calc_root_prob++;

    assert(index.size2() == rb.size());

    for(int i=0;i<rb.size();i++)
      assert(cache.up_to_date(rb[i]));

    //    assert(T.directed_branch(rb[0]).target().name() == cache.root);

    assert(rb.size() == 2);

    // scratch matrix
#ifdef DEBUG_SUBSTITUTION    
    Matrix & S = cache.scratch(0);
#endif    
    const int n_models = cache.n_models();
    const int n_states = cache.n_states();

    // cache matrix F(m,s) of p(m)*freq(m,l)
    Matrix F(n_models,n_states);
    MC.WeightedFrequencyMatrix(F);

    // look up the cache rows now, once, instead of for each column
    vector< vector<Matrix>* > branch_cache;
    for(int i=0;i<rb.size();i++)
      branch_cache.push_back(&cache[rb[i]]);
    
    log_double_t total = 1;
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

#ifdef DEBUG_SUBSTITUTION
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

    return total;
  }

  log_double_t calc_root_probability(const data_partition& P,const vector<int>& rb,
			       const matrix<int>& index) 
  {
    return calc_root_probability(P.t(), P.LC, P, rb, index);
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
			const vector<int>& sequence, const alphabet& a,
			const vector<Matrix>& transition_P)
  {
    total_peel_leaf_branches++;

    //    const vector<unsigned>& smap = MC.state_letters();

    // Do this before accessing matrices or other_subst
    cache.prepare_branch(b0);

    cache.set_length(I.branch_index_length(b0), b0);

    const int n_models  = cache.n_models();
    const int n_states  = cache.n_states();

    assert(transition_P.back().size1() == n_states);

    for(int i=0;i<I.branch_index_length(b0);i++)
    {
      Matrix& R = cache(i,b0);
      // compute the distribution at the parent node
      int l2 = sequence[i];

      if (a.is_letter(l2))
	for(int m=0;m<n_models;m++) 
	{
	  const Matrix& Q = transition_P[m];
	  for(int s1=0;s1<n_states;s1++)
	    R(m,s1) = Q(s1,l2);
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
	    R(m,s1) = sum;
	  }
	}
      }
      else
	element_assign(R,1);
    }

    cache[b0].other_subst = 1;
  }


  void peel_leaf_branch_F81(int b0, subA_index_t& I, Likelihood_Cache& cache,
			    const vector<int>& sequence, const alphabet& a, const TreeInterface& t, 
			    const Mat_Cache& MC)
  {
    total_peel_leaf_branches++;

    // Do this before accessing matrices or other_subst
    cache.prepare_branch(b0);

    cache.set_length(I.branch_index_length(b0), b0); 

    const int n_models  = cache.n_models();
    const int n_states  = cache.n_states();

    assert(MC.n_states() == n_states);

    //    const vector<unsigned>& smap = MC.state_letters();

    vector<object_ptr<const F81_Object> > SubModels(n_models);
    for(int m=0;m<n_models;m++) {
      SubModels[m] = MC.base_model(m,b0).assert_is_a<F81_Object>();
      assert(SubModels[m]);
    }
    const double L = t.branch_length(b0);

    valarray<double> exp_a_t(n_models);
    for(int m=0;m<n_models;m++) 
      exp_a_t[m] = exp(-L * SubModels[m]->alpha_);

    Matrix& F = cache.scratch(1);
    MC.FrequencyMatrix(F); // F(m,l2)

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
  }

  void peel_leaf_branch_modulated(int b0,subA_index_t& I, Likelihood_Cache& cache, 
				  const vector<int>& sequence, const alphabet& a,
				  const vector<Matrix>& transition_P,const Mat_Cache& MC)
  {
    total_peel_leaf_branches++;

    // Do this before accessing matrices or other_subst
    cache.prepare_branch(b0);

    cache.set_length(I.branch_index_length(b0), b0);

    const int n_models  = cache.n_models();
    const int n_states  = cache.n_states();
    const int n_letters = a.n_letters();

    assert(n_states >= n_letters and n_states%n_letters == 0);

    assert(MC.n_states() == n_states);

    const vector<unsigned>& smap = MC.state_letters();

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
  }

  /// Apply frequencies and collect probability for subA columns that go away on b.back()
  /// Question: can this routine handle both 
  /// (i) "going away" in terms of subA indices==-1 on b.back()
  ///    AND
  /// (ii) "going away" in terms of the node not being present at b.back().source()?
  ///
  /// Answer: Yes, because which columns "go away" is computed and then passed in via \a index.
  log_double_t collect_vanishing_internal(const vector<int>& b, matrix<int>& index, Likelihood_Cache& cache,
				      const Mat_Cache& MC)
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
    MC.WeightedFrequencyMatrix(F);

    // look up the cache rows now, once, instead of for each column
    vector<Matrix>* branch_cache[2];
    for(int i=0;i<2;i++)
      branch_cache[i] = &cache[b[i]];
    
    log_double_t total = 1;
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
  log_double_t get_other_subst_behind_branch(int b0, const TreeInterface& t, subA_index_t& I, Likelihood_Cache& cache,
					 const Mat_Cache& MC)
  {
    // This only makes sense if we have presence/absence information to sequences at internal nodes
    //    assert(A.n_sequences() == t.n_nodes());

    // There are no branches behind a leaf branch
    if (t.is_leaf_node(t.source(b0))) return 1;

    // find the names of the (two) branches behind b0
    vector<int> b = t.branches_before(b0);

    if (I.kind() == subA_index_t::leaf_index)
    {
      // Get an alignment of subA indices on branches b[0] and b[1] where b0.source is not present
      int node = t.source(b0);

      matrix<int> index_vanishing = I.get_subA_index_none(b, vector<int>(1,node));

      b.push_back(b0);
      return collect_vanishing_internal(b, index_vanishing, cache, MC);
    }
    else if (I.kind() == subA_index_t::internal_index)
    {
      b.push_back(b0);
      matrix<int> index_vanishing = I.get_subA_index_vanishing(b);

      return collect_vanishing_internal(b, index_vanishing, cache, MC);
    }
    else
      std::abort();
  }

  void peel_internal_branch(const vector<int>& b,matrix<int>& index, Likelihood_Cache& cache,
			    const vector<Matrix>& transition_P,const Mat_Cache& IF_DEBUG(MC))
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
    assert(MC.n_states() == n_states);

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

  void peel_internal_branch(int b0,subA_index_t& I, Likelihood_Cache& cache, const TreeInterface& t, 
			    const vector<Matrix>& transition_P,const Mat_Cache& MC)
  {
    total_peel_internal_branches++;

    // find the names of the (two) branches behind b0
    vector<int> b = t.branches_before(b0);
    b.push_back(b0);

    // get the relationships with the sub-alignments for the (two) branches behind b0
    matrix<int> index = I.get_subA_index_select(b);
    assert(index.size1() == I.branch_index_length(b0));
    // the call to I.get_subA-index_select ( ) updates the index for branches in b.
    assert(I.branch_index_valid(b0));

    /*-------------------- Do the peeling part------------- --------------------*/
    peel_internal_branch(b, index, cache, transition_P, MC);

    /*-------------------- Do the other_subst collection part -------------------*/
    if (I.kind() == subA_index_t::internal_index)
    {
      matrix<int> index_collect = I.get_subA_index_vanishing(b);
      cache[b[2]].other_subst = collect_vanishing_internal(b, index_collect, cache, MC);
    }
    else if (I.kind() == subA_index_t::leaf_index)
      cache[b0].other_subst = 1;
    else
      throw myexception()<<"subA_index_t is of unrecognized type!";
  }

  void peel_internal_branch_F81(int b0,subA_index_t& I, Likelihood_Cache& cache, const TreeInterface& t, 
				const Mat_Cache& MC)
  {
    //    std::cerr<<"got here! (internal)"<<endl;
    total_peel_internal_branches++;

    // find the names of the (two) branches behind b0
    vector<int> b = t.branches_before(b0);
    b.push_back(b0);

    // get the relationships with the sub-alignments for the (two) branches behind b0
    matrix<int> index = I.get_subA_index_select(b);
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
    assert(MC.n_states() == n_states);

    // look up the cache rows now, once, instead of for each column
    vector< vector<Matrix>* > branch_cache;
    for(int i=0;i<b.size();i++)
      branch_cache.push_back(&cache[b[i]]);
    
    vector<object_ptr<const F81_Object> > SubModels(n_models);
    for(int m=0;m<n_models;m++) {
      SubModels[m] = MC.base_model(m,b0).assert_is_a<F81_Object>();
      assert(SubModels[m]);
    }
    const double L = t.branch_length(b0);

    valarray<double> exp_a_t(n_models);
    for(int m=0;m<n_models;m++) 
      exp_a_t[m] = exp(-L * SubModels[m]->alpha_);

    Matrix& F = cache.scratch(1);
    MC.FrequencyMatrix(F); // F(m,l2)

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
    if (I.kind() == subA_index_t::internal_index)
    {
      matrix<int> index_collect = I.get_subA_index_vanishing(b);
      cache[b[2]].other_subst = collect_vanishing_internal(b, index_collect, cache, MC);
    }
    else if (I.kind() == subA_index_t::leaf_index)
      cache[b0].other_subst = 1;
    else
      throw myexception()<<"subA_index_t is of unrecognized type!";
  }

  vector<Matrix>
  get_leaf_seq_likelihoods(const vector<int>& sequence, const alphabet& a, const Mat_Cache& MC, int delta);


  void peel_branch(int b0,subA_index_t& I, Likelihood_Cache& cache, 
		   const vector< vector<int> >& sequences, const alignment& A, 
		   const TreeInterface& t,
		   const Mat_Cache& MC)
  {
    total_peel_branches++;

    // compute branches-in
    int bb = t.branches_before(b0).size();

    int B0 = t.undirected(b0);

    if (t.n_nodes() == 2 and b0 == 1)
    {
      assert(bb == 0);
      cache.prepare_branch(b0);
      cache.set_length(I.branch_index_length(b0), b0);

      vector<Matrix> L = get_leaf_seq_likelihoods(sequences[1], A.get_alphabet(), MC, 0);

      for(int i=0;i<I.branch_index_length(b0);i++)
	cache(i,b0) = L[i];
      cache[b0].other_subst = 1;
    }
    else if (bb == 0) {
      const alphabet& a = A.get_alphabet();
      int n_states = cache.scratch(0).size2();
      int n_letters = a.n_letters();
      if (n_states == n_letters) {
	if (MC.base_model(0,0).is_a<F81_Object>())
	  peel_leaf_branch_F81(b0, I, cache, sequences[b0], a, t, MC);
	else
	  peel_leaf_branch(b0, I, cache, sequences[b0], a, MC.transition_P(B0));
      }
      else
	peel_leaf_branch_modulated(b0, I, cache, sequences[b0], a, MC.transition_P(B0), MC);
    }
    else if (bb == 2) {
      if (MC.base_model(0,0).is_a<F81_Object>())
	peel_internal_branch_F81(b0, I, cache, t, MC);
      else
	peel_internal_branch(b0, I, cache, t, MC.transition_P(B0), MC);
    }
    else
      std::abort();

    cache.validate_branch(b0);
  }


  /// Compute an ordered list of branches to process
  inline peeling_info get_branches(const TreeInterface& t, const Likelihood_Cache& LC, vector<int> branches) 
  {
    //------- Get ordered list of not up_to_date branches ----------///
    peeling_info peeling_operations(t);

    for(int i=0;i<branches.size();i++) 
    {
      int b = branches[i];
      if (not LC.up_to_date(b)) {
	t.append_branches_before(b, branches);
	peeling_operations.push_back(b);
      }
    }

    std::reverse(peeling_operations.begin(),peeling_operations.end());

    return peeling_operations;
  }

  /// Compute an ordered list of branches to process to validate branch b
  inline peeling_info get_branches_for_branch(int b, const TreeInterface& t, const Likelihood_Cache& LC) 
  {
    vector<int> branches(1,b);
    branches.reserve(t.n_branches());

    return get_branches(t,LC,branches);
  }

  /// Compute an ordered list of branches to process
  inline peeling_info get_branches_for_node(int n, const TreeInterface& t, const Likelihood_Cache& LC) 
  {
    vector<int> branches;
    branches.reserve(t.n_branches());
    for(int i=0;i<t.degree(n);i++)
      branches.push_back(t.reverse(t.branch_out(n,i)));

    return get_branches(t, LC, branches);
  }

  static 
  int calculate_caches_for_node(int n, const vector< vector<int> >& sequences, const alignment& A, 
				subA_index_t& I, const Mat_Cache& MC, const TreeInterface& t,
				Likelihood_Cache& cache)
  {
    //---------- determine the operations to perform ----------------//
    peeling_info ops = get_branches_for_node(n, t, cache);

    if (t.n_nodes() == 2)
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
      peel_branch(ops[i],I,cache,sequences,A,t,MC);

    return ops.size();
  }

  int calculate_caches_for_node(int n, const data_partition& P) {
    return calculate_caches_for_node(n, *P.sequences, P.A(), P.subA(), P, P.t(), P.LC);
  }

  static 
  int calculate_caches_for_branch(int b, const vector< vector<int> >& sequences, const alignment& A, 
				  subA_index_t& I, const Mat_Cache& MC, const TreeInterface& t,
				  Likelihood_Cache& cache)
  {
    //---------- determine the operations to perform ----------------//
    peeling_info ops = get_branches_for_branch(b, t, cache);

    //-------------- Compute the branch likelihoods -----------------//
    for(int i=0;i<ops.size();i++)
      peel_branch(ops[i],I,cache,sequences,A,t,MC);

    return ops.size();
  }

  /// Construct a likelihood matrix R(m,s) = Pr(observe letter l | model = m, state = 2)
  Matrix get_letter_likelihoods(int l, const alphabet& a, const Mat_Cache& MC)
  {
    assert(a.is_feature(l));

    const int n_letters = a.size();

    const int n_models = MC.n_base_models();
    const int n_states = MC.n_states();

    Matrix R(n_models,n_states);

    if (l == alphabet::not_gap)
    {
      element_assign(R,1);
      return R;
    }

    element_assign(R,0.0);

    const vector<unsigned>& smap = MC.state_letters();

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
  get_leaf_seq_likelihoods(const vector<int>& sequence, const alphabet& a, const Mat_Cache& MC, int delta)
  {
    int L = sequence.size();

    const int n_letters = a.size();

    const int n_models = MC.n_base_models();
    const int n_states = MC.n_states();

    // Compute the likelihood matrix just one for each letter (not letter classes)
    vector<Matrix> letter_likelihoods;
    for(int l=0;l<n_letters;l++)
      letter_likelihoods.push_back( get_letter_likelihoods(l, a, MC) );

    // Compute the likelihood matrices for each letter in the sequence
    vector<Matrix> likelihoods(L+delta, Matrix(n_models,n_states));

    for(int i=0;i<L;i++)
    {
      int letter = sequence[i];
      if (a.is_letter(letter))
	likelihoods[i+delta] = letter_likelihoods[letter];
      else
	likelihoods[i+delta] = get_letter_likelihoods(letter, a, MC);
    }

    return likelihoods;
  }

  vector<Matrix>
  get_leaf_seq_likelihoods(const data_partition& P, int n, int delta)
  {
    const vector<int>& sequence = (*P.sequences)[n];
    const alphabet& a = P.get_alphabet();
    return get_leaf_seq_likelihoods(sequence, a, P, delta);
  }

  /// Find the probabilities of each PRESENT letter at the root, given the data at the nodes in 'group'
  vector<Matrix>
  get_column_likelihoods(const data_partition& P, const vector<int>& b,
			 const vector<int>& ordered_columns,int delta)
  {
    // FIXME - this now handles only internal sequences.  But see get_leaf_seq_likelihoods( ).
    const alignment& A = P.A();
    auto t = P.t();
    Likelihood_Cache& LC = P.LC;
    subA_index_t& I = P.subA();

#ifdef DEBUG_INDEXING
    I.check_footprint();
#endif

    //------ Check that all branches point to a 'root' node -----------//
    assert(b.size());
    assert(not t.is_leaf_node(LC.root));
    int root = t.target(b[0]);
    for(int i=1;i<b.size();i++)
      assert(t.target(b[i]) == root);
    LC.root = root;
    assert(not t.is_leaf_node(LC.root));

    // select columns with at least one node in 'required_nodes', and re-order them according to the permutation 'ordered_columns'
    matrix<int> index = I.get_subA_index_columns(b,ordered_columns);

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

    return L;
  }

  /// Find the leaf branches of a connected subtree of nodes \a nodes instead of tree \a T
  vector<int> get_leaf_branches_from_subtree_nodes(const TreeInterface& t, const vector<int>& nodes)
  {
    vector<int> branch_list;
    for(int i=0;i<nodes.size();i++)
    {
      const int n = nodes[i];
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

  /// Get the total likelihood for columns behind b0 that have been deleted before b0.source (e.g. and so b0.source is -).
  log_double_t other_subst_behind_branch(int b0, const vector< vector<int> >& sequences, const alignment& A, 
					 const TreeInterface& t,
					 subA_index_t& I, Likelihood_Cache& LC, const Mat_Cache& MC)
  {
    if (LC.up_to_date(b0) and I.kind() == subA_index_t::internal_index)
      return LC[b0].other_subst;

    for(int j: t.branches_before(b0))
      calculate_caches_for_branch(j, sequences, A, I, MC, t, LC);

    return get_other_subst_behind_branch(b0, t, I, LC, MC);
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
    const vector< vector<int> >& sequences = *P.sequences;
    const alignment& A = P.A();
    auto t = P.t();
    const Mat_Cache& MC = P;
    Likelihood_Cache& LC = P.LC;
    subA_index_t& I = P.subA();

    // compute root branches
    vector<int> rb = t.branches_in(LC.root);

    vector<int> leaf_branch_list = get_leaf_branches_from_subtree_nodes(t,nodes);

    log_double_t Pr3 = 1;
    for(int i=0;i<leaf_branch_list.size();i++)
      Pr3 *= other_subst_behind_branch(leaf_branch_list[i], sequences, A, t, I, LC, MC);

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
    if (P.subA().kind() == subA_index_t::leaf_index)
    {
      matrix<int> index1 = I.get_subA_index_none(rb,nodes);
      log_double_t Pr1 = calc_root_probability(P,rb,index1);
      assert(std::abs(log(Pr1) - log(Pr3) ) < 1.0e-9);

      matrix<int> index2 = I.get_subA_index_any(rb,nodes);
      matrix<int> index  = I.get_subA_index(rb);

      log_double_t Pr2 = calc_root_probability(P,rb,index2);
      log_double_t Pr  = calc_root_probability(P,rb,index);

      assert(std::abs(log(Pr1 * Pr2) - log(Pr) ) < 1.0e-9);
    }
#endif

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

    log_double_t other_subst_current = LC1[b].other_subst;
    log_double_t other_subst_recomputed = LC2[b].other_subst;
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
		      const Likelihood_Cache& LC1, const Likelihood_Cache& LC2, const TreeInterface& t)
  {
    assert(LC1.root == LC2.root);
    
    vector<int> branches = t.branches_in(LC1.root);
    branches.reserve(t.n_branches());

    for(int i=0;i<branches.size();i++)
    {
      int  b = branches[i];

      t.append_branches_before(b, branches);

      if (LC1.up_to_date(b) and LC2.up_to_date(b))
	compare_caches(I1,I2,LC1,LC2,b);
    }

  }

  log_double_t branch_total(int b0, const subA_index_t& I, const Likelihood_Cache& cache, const Mat_Cache& MC)
  {
    // scratch matrix 
    const int n_models = cache.n_models();
    const int n_states = cache.n_states();

    // cache matrix F(m,s) of p(m)*freq(m,l)
    Matrix F(n_models,n_states);
    MC.WeightedFrequencyMatrix(F);

    matrix<int> index = I.get_subA_index(vector<int>(1,b0));

    log_double_t total = 1;
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

  /*
  void compare_branch_totals(subA_index_t& I1, subA_index_t& I2,
			     Likelihood_Cache& LC1, Likelihood_Cache& LC2, const TreeInterface& t,
			     const alignment& A, const Mat_Cache& MC)
  {
    assert(LC1.root == LC2.root);
    
    //---------- determine the operations to perform ----------------//
    vector<int> branches = t.all_branches_toward_node(LC1.root);

    //-------------- Compute the branch likelihoods -----------------//
    for(int i=0;i<branches.size();i++)
    {
      int b = branches[i];

      assert(LC1.up_to_date(b));
      assert(LC2.up_to_date(b));

      log_double_t branch_total1 = branch_total(b,I1,LC1,MC);
      log_double_t other_subst1 = get_other_subst_behind_branch(b, t, I1, LC1, MC);

      log_double_t branch_total2 = branch_total(b,I2,LC2,MC);
      log_double_t other_subst2 = get_other_subst_behind_branch(b, t, I2, LC2, MC);

      assert(std::abs(log(other_subst1) - log(other_subst2)) < 1.0e-9);
      assert(std::abs(log(branch_total1) - log(branch_total2)) < 1.0e-9);
    }
  }
  */
  

  log_double_t Pr(const vector< vector<int> >& sequences, const alignment& A,subA_index_t& I, 
		  const Mat_Cache& MC,const TreeInterface& t, Likelihood_Cache& LC)
  {
    total_likelihood++;

#ifndef DEBUG_CACHING
    if (LC.cv_up_to_date()) {
#ifdef DEBUG_SUBSTITUTION
      std::clog<<"Pr: Using cached value "<<log(LC.cached_value)<<"\n";
#endif
      return LC.cached_value;
    }
#endif

#ifdef DEBUG_INDEXING
    I.check_footprint();
#endif

    IF_DEBUG_S(int n_br =) calculate_caches_for_node(LC.root, sequences, A,I,MC,t,LC);
#ifdef DEBUG_SUBSTITUTION
    std::clog<<"Pr: Peeled on "<<n_br<<" branches.\n";
#endif

    // compute root branches
    vector<int> rb;
    if (t.n_nodes() == 2)
    {
      // This is the 
      rb.push_back(0);
      rb.push_back(1);
    }
    else
      rb = t.branches_in(LC.root);

    // get the relationships with the sub-alignments
    matrix<int> index = I.get_subA_index(rb);

    // get the probability
    log_double_t Pr = 1;
    if (t.n_nodes() == 2)
      Pr = calc_root_probability2(LC,MC,rb,index);
    else
      Pr = calc_root_probability(t,LC,MC,rb,index);

#ifdef DEBUG_CACHING
    if (LC.cv_up_to_date())
    {
      assert(std::abs(LC.cached_value.log() - Pr.log()) < 1.0e-9);
    }
#endif
    LC.cached_value = Pr;
    LC.cv_up_to_date() = true;

    return Pr;
  }

  log_double_t Pr(const data_partition& P,Likelihood_Cache& LC) 
  {
    return Pr(*P.sequences, P.A(), P.subA(), P, P.t(), LC);
  }



  /*
  log_double_t Pr_from_scratch_leaf(data_partition P)
  {
    subA_index_leaf subA(P.P, P.t().n_branches()*2);

    Likelihood_Cache LC(P.t(), P);
    LC.root = P.LC.root;

    return Pr(*P.sequences, P.A(), subA, P, P.t(), LC);
  }



  log_double_t Pr_from_scratch_internal(data_partition P)
  {
    assert(P.variable_alignment());

    subA_index_internal subA(P.P, P.t().n_branches()*2);

    Likelihood_Cache LC(P.t(), P);
    LC.root = P.LC.root;

    check_internal_nodes_connected(P.A(),P.t(),vector<int>(1,LC.root));

    return Pr(*P.sequences, P.A(), subA, P, P.t(), LC);
  }
  */
  log_double_t Pr(const data_partition& P) {
    log_double_t result = Pr(P, P.LC);

#ifdef DEBUG_CACHING
    data_partition P2 = P;
    P2.LC.invalidate_all();
    P2.invalidate_subA_index_all();
    for(int i=0;i<P2.t().n_branches();i++)
      P2.setlength(i);
    log_double_t result2 = Pr(P2, P2.LC);

    if (std::abs(log(result) - log(result2))  > 1.0e-9) {
      std::cerr<<"Pr: diff = "<<log(result)-log(result2)<<std::endl;
      compare_caches(P.subA(), P2.subA(), P.LC, P2.LC, P.t());
      std::abort();
    }
#endif

    /*
#ifdef DEBUG_INDEXING
    log_double_t result3 = Pr_from_scratch_leaf(P);

    if (P.variable_alignment())
    {
      log_double_t result4 = Pr_from_scratch_internal(P);

      //      compare_branch_totals(subA3,subA4,LC3,LC4, P.t(), P.A(), P);
      assert(std::abs(log(result3) - log(result4)) < 1.0e-9);
    }

    assert(std::abs(log(result) - log(result3)) < 1.0e-9);
#endif
    */
    
    return result;
  }

  log_double_t combine_likelihoods(const vector<Matrix>& likelihoods)
  {
    log_double_t Pr = 1;
    for(int i=0;i<likelihoods.size();i++)
      Pr *= element_sum(likelihoods[i]);
    return Pr;
  }

  vector<vector<pair<int,int>>> 
  sample_subst_history(const vector< vector<int> >& sequences, const alignment& A,
		       subA_index_t& I, const Mat_Cache& MC,
		       const TreeInterface& t, Likelihood_Cache& cache)
  {
#ifdef DEBUG_INDEXING
    I.check_footprint();
#endif

    const vector<unsigned>& smap = MC.state_letters();

    // scratch matrix 
    Matrix & S = cache.scratch(0);
    const int n_models = cache.n_models();
    const int n_states = cache.n_states();

    int root = cache.root;

    // Compute matrix F(m,s) = Pr(m)*Pr(s|m) = p(m)*freq(m,s) 
    Matrix F(n_models, n_states);
    MC.WeightedFrequencyMatrix(F);

    bool has_internal_nodes = (A.n_sequences() == t.n_nodes());
    if (not has_internal_nodes) assert(A.n_sequences() == t.n_leaves());

    // 1. Allocate arrays for storing results and temporary results.
    vector<vector<pair<int,int> > > ancestral_characters (A.n_sequences());
    vector<vector<pair<int,int> > > subA_index_parent_characters (t.n_branches()*2);
    
    // All the (-1,-1)'s should be overwritten with the sampled character.
    for(int i=0;i<A.n_sequences();i++)
      ancestral_characters[i] = vector<pair<int,int>>(A.seqlength(i), {-1,-1});

    {
      // compute root branches
      vector<int> rb;
      for(int b: t.branches_in(root))
      {
	calculate_caches_for_branch(b, sequences, A,I,MC,t,cache);

	rb.push_back(b);

	subA_index_parent_characters[b] = vector<pair<int,int>>(I.branch_index_length(b), {-1,-1});
      }

      // FIXME - but what if root is internal and A doesn't have internal sequences?
      vector<int> nodes;
      if (has_internal_nodes)
	nodes = {root};
      matrix<int> index = I.get_subA_index_with_nodes(rb, nodes);

      // FIXME - this doesn't handle case where tree has only 2 leaves.
      for(int i=0;i<index.size1();i++)
      {
	int i0 = index(i,0);
	int i1 = index(i,1);
	int i2 = index(i,2);

	S = F;

	if (i0 != -1)
	  element_prod_modify(S, cache[rb[0]][i0]);
	if (i1 != -1)
	  element_prod_modify(S, cache[rb[1]][i1]);
	if (i2 != -1)
	  element_prod_modify(S, cache[rb[2]][i2]);

	pair<int,int> state_model = sample(S);

	if (has_internal_nodes)
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

    vector<int> branches = t.all_branches_toward_node(cache.root);
    std::reverse(branches.begin(), branches.end());

    for(int b: branches)
    {
      int node = t.source(b);

      const vector<Matrix>& transition_P = MC.transition_P(b);

      vector<int> local_branches = {b};
      for(int b: t.branches_before(b))
      {
	calculate_caches_for_branch(b, sequences, A,I,MC,t,cache);

	local_branches.push_back(b);

	subA_index_parent_characters[b] = vector<pair<int,int>>(I.branch_index_length(b), {-1,-1});
      }

      assert(local_branches.size() == 3 or local_branches.size() == 1);

      vector<int> nodes;
      if (t.is_leaf_node(node) or has_internal_nodes)
	nodes = {node};

      // FIXME - but what if node is internal and A doesn't have internal sequences?
      matrix<int> index = I.get_subA_index_with_nodes(local_branches, nodes);
      
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
	  int ii = index(i,index.size2()-1);
	  int l = sequences[node][ii];
	  const alphabet& a = A.get_alphabet();
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
	  element_prod_modify(S, cache[local_branches[1]][i1]);
	if (i2 != -1)
	  element_prod_modify(S, cache[local_branches[2]][i2]);
	
	pair<int,int> state_model = sample(S);
	
	if (t.is_leaf_node(node) or (has_internal_nodes))
	{
	  int ii = index(i,index.size2()-1);
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
    return sample_subst_history(*P.sequences, P.A(), P.subA(), P, P.t(), P.LC);
  }

  vector<Matrix> 
  get_likelihoods_by_alignment_column(const vector< vector<int> >& sequences, const alignment& A,
				      subA_index_t& I, const Mat_Cache& MC,
				      const TreeInterface& t, Likelihood_Cache& cache)
  {
#ifdef DEBUG_INDEXING
    I.check_footprint();
#endif

    // Make sure that all conditional likelihoods have been calculated.
    IF_DEBUG_S(int n_br =) calculate_caches_for_node(cache.root, sequences, A,I,MC,t,cache);
#ifdef DEBUG_SUBSTITUTION
    std::clog<<"Pr: Peeled on "<<n_br<<" branches.\n";
#endif

    // Compute matrix F(m,s) = Pr(m)*Pr(s|m) = p(m)*freq(m,s) 
    Matrix F;
    MC.WeightedFrequencyMatrix(F);

    // 1. Initialize with values that are true if all data is missing.
    vector<Matrix> likelihoods(A.length(), F);

    // 2. Record likelihoods for disappearing columns
    vector<int> branches = t.all_branches_toward_node(cache.root);

    IF_DEBUG_S(log_double_t other_subst1 = 1);
    /*
    for(int i=0;i<branches.size();i++)
    {
      int b = branches[i];

      // Get previous branches
      vector<int> prev;
      for(const_in_edges_iterator j = branches[i].branches_before();j;j++)
      {
	// update conditional likelihoods
	calculate_caches_for_branch(*j, sequences, A, I, MC, T, cache);

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
    for(const_in_edges_iterator i = T.node(cache.root).branches_in();i;i++)
    {
      // update conditional likelihoods
      calculate_caches_for_branch(*i, sequences, A, I, MC, T, cache);

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
    log_double_t other_subst2 = 1;
    for(int i=0;i<root_branches.size();i++)
    {
      assert(cache.up_to_date(root_branches[i]));
      log_double_t L1 = cache[root_branches[i]].other_subst;
      other_subst2 *= L1;
      //      log_double_t L2 = other_subst3[root_branches[i]];
      //      assert(std::abs(log(L2) - log(L1)) < 1.0e-9);
    }

    assert(std::abs(log(other_subst1) - log(other_subst2)) < 1.0e-9);
#endif
    */
    return likelihoods;
  }



  vector<Matrix> get_likelihoods_by_alignment_column(const data_partition& P)
  {
    vector<Matrix> likelihoods = get_likelihoods_by_alignment_column(*P.sequences, P.A(), P.subA(), P, P.t(), P.LC);

#ifdef DEBUG_SUBSTITUTION
    log_double_t L1 = combine_likelihoods(likelihoods);
    log_double_t L2 = Pr_from_scratch_leaf(P);
    if (P.variable_alignment()) {
      log_double_t L3 = Pr_from_scratch_internal(P);
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
