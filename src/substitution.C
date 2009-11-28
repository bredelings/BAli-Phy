/*
   Copyright (C) 2004-2005,2007,2009 Benjamin Redelings

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

inline void element_prod_assign(Matrix& M1,const Matrix& M2)
{
  assert(M1.size1() == M2.size1());
  assert(M1.size2() == M2.size2());
  
  const int size = M1.data().size();
  double * __restrict__ m1 = M1.data().begin();
  const double * __restrict__ m2 = M2.data().begin();
  
  for(int i=0;i<size;i++)
    m1[i] *= m2[i];
}

inline void element_prod_assign3(Matrix& M1,const Matrix& M2,const Matrix& M3)
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

  efloat_t calc_root_probability(const alignment& A,const Tree& T,Likelihood_Cache& cache,
			       const MultiModel& MModel,const vector<int>& rb,const ublas::matrix<int>& index) 
  {
    total_calc_root_prob++;

    const alphabet& a = A.get_alphabet();

    const int root = cache.root;

    if (T[root].is_leaf_node())
      throw myexception()<<"Trying to accumulate conditional likelihoods at a root node is not allowed.";

    // scratch matrix 
    Matrix & S = cache.scratch(0);
    const int n_models = S.size1();
    const int n_states = S.size2();

    // cache matrix F(m,s) of p(m)*freq(m,l)
    Matrix F(n_models,n_states);
    for(int m=0;m<n_models;m++) {
      double p = MModel.distribution()[m];
      const valarray<double>& f = MModel.base_model(m).frequencies();
      for(int s=0;s<n_states;s++) 
	F(m,s) = f[s]*p;
    }

    efloat_t total = 1;
    for(int i=0;i<index.size1();i++) 
    {
      //-------------- Set letter & model prior probabilities  ---------------//
      element_assign(S,F); // noalias(S) = F;

      //-------------- Propagate and collect information at 'root' -----------//
      for(int j=0;j<rb.size();j++) {
	int i0 = index(i,j);
	if (i0 != alphabet::gap)
	  element_prod_assign(S,cache(i0,rb[j]));
      }

      //--------- If there is a letter at the root, condition on it ---------//
      if (root < T.n_leaves()) {
	int rl = A.seq(root)[i];
	// How about if its NOT a letter class?
	if (a.is_letter_class(rl))
	  for(int s=0;s<n_states;s++)
	    if (not a.matches(MModel.state_letters()[s],rl))
	      for(int m=0;m<n_models;m++) 
		S(m,s) = 0;
      }

#ifndef NDEBUG     
      //--------- If there is a letter at the root, condition on it ---------//
      for(int m=0;m<n_models;m++) {
	double p_model=0;
	for(int s=0;s<n_states;s++)
	  p_model += S(m,s);
	// A specific model (e.g. the INV model) could be impossible
	assert(0 <= p_model and p_model <= 1.00000000001);
      }
#endif

      // What is the total probability of the models?
      double p_col = element_sum(S);

      // SOME model must be possible
      assert(0 <= p_col and p_col <= 1.00000000001);
      
      // This does a log( ) operation.
      total *= p_col;
      //      std::clog<<" i = "<<i<<"   p = "<<p_col<<"  total = "<<total<<"\n";
    }

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


  void peel_leaf_branch(int b0,Likelihood_Cache& cache, const alignment& A, const Tree& T, 
			const MatCache& transition_P,const MultiModel& MModel)
  {
    total_peel_leaf_branches++;

    const alphabet& a = A.get_alphabet();

    // The number of directed branches is twice the number of undirected branches
    const int B        = T.n_branches();

    // scratch matrix
    Matrix& S = cache.scratch(0);
    const int n_models  = S.size1();
    const int n_states  = S.size2();
    //    const int n_letters = a.n_letters();
    assert(MModel.n_states() == n_states);

    //    std::clog<<"length of subA for branch "<<b0<<" is "<<length<<"\n";
    if (not subA_index_valid(A,b0))
      update_subA_index_branch(A,T,b0);

    //    const vector<unsigned>& smap = MModel.state_letters();

    for(int i=0;i<subA_length(A,b0);i++)
    {
      // compute the distribution at the parent node
      int l2 = A.note(0,i+1,b0);

      if (a.is_letter(l2))
	for(int m=0;m<n_models;m++) {
	  const Matrix& Q = transition_P[m][b0%B];
	  for(int s1=0;s1<n_states;s1++)
	    cache(i,b0)(m,s1) = Q(s1,l2);
	}
      else if (a.is_letter_class(l2)) {
	for(int m=0;m<n_models;m++) {
	  const Matrix& Q = transition_P[m][b0%B];
	  for(int s1=0;s1<n_states;s1++)
	    cache(i,b0)(m,s1) = sum(Q,s1,l2,a);
	}
      }
      else
	for(int m=0;m<n_models;m++)
	  for(int s=0;s<n_states;s++)
	    cache(i,b0)(m,s) = 1;
    }
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

  void peel_leaf_branch_F81(int b0,Likelihood_Cache& cache, const alignment& A, const Tree& T, 
			    const MultiModel& MModel)
  {
    total_peel_leaf_branches++;

    //    std::cerr<<"got here! (leaf)"<<endl;

    const alphabet& a = A.get_alphabet();

    // The number of directed branches is twice the number of undirected branches
    const int B        = T.n_branches();

    // scratch matrix
    Matrix& S = cache.scratch(0);
    const int n_models  = S.size1();
    const int n_states  = S.size2();
    //    const int n_letters = a.n_letters();
    assert(MModel.n_states() == n_states);

    //    std::clog<<"length of subA for branch "<<b0<<" is "<<length<<"\n";
    if (not subA_index_valid(A,b0))
      update_subA_index_branch(A,T,b0);

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

    for(int i=0;i<subA_length(A,b0);i++)
    {
      // compute the distribution at the parent node
      int l2 = A.note(0,i+1,b0);

      if (a.is_letter(l2))
	for(int m=0;m<n_models;m++) {
	  const valarray<double>& pi = SubModels[m]->frequencies();
	  for(int s1=0;s1<n_states;s1++)
	    cache(i,b0)(m,s1) = (1.0-exp_a_t[m])*pi[l2];
	  cache(i,b0)(m,l2) += exp_a_t[m];
	}
      else if (a.is_letter_class(l2)) 
      {
	for(int m=0;m<n_models;m++) 
	{
	  double sum=0;
	  for(int l=0;l<a.size();l++)
	    if (a.matches(l,l2))
	      sum += F(m,l);
	  for(int s1=0;s1<n_states;s1++)
	    cache(i,b0)(m,s1) = (1.0-exp_a_t[m])*sum;
	  for(int l=0;l<a.size();l++)
	    if (a.matches(l,l2))
	      cache(i,b0)(m,l) += exp_a_t[m];
	}
      }
      else
	for(int m=0;m<n_models;m++)
	  for(int s=0;s<n_states;s++)
	    cache(i,b0)(m,s) = 1;
    }
  }

  void peel_leaf_branch_modulated(int b0,Likelihood_Cache& cache, const alignment& A, 
				  const Tree& T, 
				  const MatCache& transition_P,const MultiModel& MModel)
  {
    total_peel_leaf_branches++;

    const alphabet& a = A.get_alphabet();

    // The number of directed branches is twice the number of undirected branches
    const int B        = T.n_branches();

    // scratch matrix
    Matrix& S = cache.scratch(0);
    const int n_models  = S.size1();
    const int n_states  = S.size2();
    const int n_letters = a.n_letters();
    //    const int N = n_states/n_letters;
    assert(MModel.n_states() == n_states);

    //    std::clog<<"length of subA for branch "<<b0<<" is "<<length<<"\n";
    if (not subA_index_valid(A,b0))
      update_subA_index_branch(A,T,b0);

    const vector<unsigned>& smap = MModel.state_letters();

    for(int i=0;i<subA_length(A,b0);i++)
    {
      // compute the distribution at the parent node
      int l2 = A.note(0,i+1,b0);

      if (a.is_letter(l2))
	for(int m=0;m<n_models;m++) {
	  const Matrix& Q = transition_P[m][b0%B];
	  for(int s1=0;s1<n_states;s1++)
	    cache(i,b0)(m,s1) = sum(Q,smap,n_letters,s1,l2);
	}
      else if (a.is_letter_class(l2)) {
	for(int m=0;m<n_models;m++) {
	  const Matrix& Q = transition_P[m][b0%B];
	  for(int s1=0;s1<n_states;s1++)
	    cache(i,b0)(m,s1) = sum(Q,smap,s1,l2,a);
	}
      }
      else
	for(int m=0;m<n_models;m++)
	  for(int s=0;s<n_states;s++)
	    cache(i,b0)(m,s) = 1;
    }
  }


  void peel_internal_branch(int b0,Likelihood_Cache& cache, const alignment& A, const Tree& T, 
			    const MatCache& transition_P,const MultiModel& IF_DEBUG(MModel))
  {
    total_peel_internal_branches++;

    // find the names of the (two) branches behind b0
    vector<int> b;
    for(const_in_edges_iterator i = T.directed_branch(b0).branches_before();i;i++)
      b.push_back(*i);

    // get the relationships with the sub-alignments for the (two) branches behind b0
    b.push_back(b0);
    ublas::matrix<int> index = subA_index_select(b,A,T);
    b.pop_back();
    assert(index.size1() == subA_length(A,b0));
    assert(subA_index_valid(A,b0));

    // The number of directed branches is twice the number of undirected branches
    const int B        = T.n_branches();

    // scratch matrix
    Matrix& S = cache.scratch(0);
    const int n_models = S.size1();
    const int n_states = S.size2();
    assert(MModel.n_states() == n_states);

    //    std::clog<<"length of subA for branch "<<b0<<" is "<<length<<"\n";
    for(int i=0;i<subA_length(A,b0);i++) 
    {
      // compute the source distribution from 2 branch distributions
      int i0 = index(i,0);
      int i1 = index(i,1);
      if (i0 != alphabet::gap and i1 != alphabet::gap)
	for(int m=0;m<n_models;m++) 
	  for(int s=0;s<n_states;s++)
	    S(m,s) = cache(i0,b[0])(m,s)* cache(i1,b[1])(m,s);
      else if (i0 != alphabet::gap)
	S = cache(i0,b[0]);
      else if (i1 != alphabet::gap)
	S = cache(i1,b[1]);
      else
	std::abort(); // columns like this should not be in the index

      // propagate from the source distribution
      Matrix& R = cache(i,b0);            //name result matrix
      for(int m=0;m<n_models;m++) {
	
	// FIXME!!! - switch order of MatCache to be MC[b][m]
	const Matrix& Q = transition_P[m][b0%B];
	
	// compute the distribution at the target (parent) node - multiple letters
	for(int s1=0;s1<n_states;s1++) {
	  double temp=0;
	  for(int s2=0;s2<n_states;s2++)
	    temp += Q(s1,s2)*S(m,s2);
	  R(m,s1) = temp;
	}
      }
    }
  }

  void peel_internal_branch_F81(int b0,Likelihood_Cache& cache, const alignment& A, const Tree& T, 
				const MultiModel& MModel)
  {
    //    std::cerr<<"got here! (internal)"<<endl;
    total_peel_internal_branches++;

    // find the names of the (two) branches behind b0
    vector<int> b;
    for(const_in_edges_iterator i = T.directed_branch(b0).branches_before();i;i++)
      b.push_back(*i);

    // get the relationships with the sub-alignments for the (two) branches behind b0
    b.push_back(b0);
    ublas::matrix<int> index = subA_index_select(b,A,T);
    b.pop_back();
    assert(index.size1() == subA_length(A,b0));
    assert(subA_index_valid(A,b0));

    // The number of directed branches is twice the number of undirected branches
    //    const int B        = T.n_branches();

    // scratch matrix
    Matrix& S = cache.scratch(0);
    const int n_models = S.size1();
    const int n_states = S.size2();
    assert(MModel.n_states() == n_states);

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
    for(int i=0;i<subA_length(A,b0);i++) 
    {
      // compute the source distribution from 2 branch distributions
      int i0 = index(i,0);
      int i1 = index(i,1);
      if (i0 != alphabet::gap and i1 != alphabet::gap)
	for(int m=0;m<n_models;m++) 
	  for(int s=0;s<n_states;s++)
	    S(m,s) = cache(i0,b[0])(m,s)* cache(i1,b[1])(m,s);
      else if (i0 != alphabet::gap)
	S = cache(i0,b[0]);
      else if (i1 != alphabet::gap)
	S = cache(i1,b[1]);
      else
	std::abort(); // columns like this should not be in the index

      // propagate from the source distribution
      Matrix& R = cache(i,b0);            //name result matrix
      for(int m=0;m<n_models;m++) 
      {
	// compute the distribution at the target (parent) node - multiple letters

	//  sum = (1-exp(-a*t))*(\sum[s2] pi[s2]*L[s2])
	double sum = 0;
	for(int s2=0;s2<n_states;s2++)
	  sum += F(m,s2)*S(m,s2);
	sum *= (1.0 - exp_a_t[m]);

	// L'[s1] = exp(-a*t)L[s1] + sum
	for(int s1=0;s1<n_states;s1++) 
	  R(m,s1) = exp_a_t[m]*S(m,s1) + sum;
      }
    }
  }



  void peel_branch(int b0,Likelihood_Cache& cache, const alignment& A, const Tree& T, 
		   const MatCache& transition_P, const MultiModel& MModel)
  {
    total_peel_branches++;

    // compute branches-in
    int bb = T.directed_branch(b0).branches_before().size();

    if (bb == 0) {
      int n_states = cache.scratch(0).size2();
      int n_letters = A.get_alphabet().n_letters();
      if (n_states == n_letters) {
	if (dynamic_cast<const F81_Model*>(&MModel.base_model(0)))
	  peel_leaf_branch_F81(b0, cache, A, T, MModel);
	else
	  peel_leaf_branch(b0, cache, A, T, transition_P, MModel);
      }
      else
	peel_leaf_branch_modulated(b0, cache, A, T, transition_P, MModel);
    }
    else if (bb == 2) {
      if (dynamic_cast<const F81_Model*>(&MModel.base_model(0)))
	peel_internal_branch_F81(b0, cache, A, T, MModel);
      else
	peel_internal_branch(b0, cache, A, T, transition_P, MModel);
    }
    else
      std::abort();

    cache.validate_branch(b0);
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
  int calculate_caches(const alignment& A, const MatCache& MC, const Tree& T,Likelihood_Cache& cache,
		       const MultiModel& MModel) {
    //---------- determine the operations to perform ----------------//
    peeling_info ops = get_branches(T, cache);

    //-------------- Compute the branch likelihoods -----------------//
    for(int i=0;i<ops.size();i++)
      peel_branch(ops[i],cache,A,T,MC,MModel);

    return ops.size();
  }

  int calculate_caches(const data_partition& P) {
    return calculate_caches(*P.A, P.MC, *P.T, P.LC, P.SModel());
  }

  Matrix get_rate_probabilities(const alignment& A,const MatCache& MC,const Tree& T,
				Likelihood_Cache& cache,const MultiModel& MModel)
  {
    const alphabet& a = A.get_alphabet();

    const int root = cache.root;
    
    // make sure that we are up-to-date
    calculate_caches(A,MC,T,cache,MModel);

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
    ublas::matrix<int> index = subA_index(root,A,T);

    // scratch matrix 
    Matrix & S = cache.scratch(0);
    const int n_models = S.size1();
    const int n_states    = S.size2();

    // cache matrix of frequencies
    Matrix F(n_models,n_states);
    for(int m=0;m<n_models;m++) {
      double p = MModel.distribution()[m];
      const valarray<double>& f = MModel.base_model(m).frequencies();
      for(int s=0;s<n_states;s++) 
	F(m,s) = f[s]*p;
    }

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
    const alphabet& a = P.get_alphabet();

    const alignment& A = *P.A;
    const Tree& T = *P.T;
    Likelihood_Cache& LC = P.LC;

#ifndef NDEBUG
    subA_index_check_footprint(A,T);
    subA_index_check_regenerate(A,T);
#endif

    //------ Check that all branches point to a 'root' node -----------//
    assert(b.size());
    int root = T.directed_branch(b[0]).target();
    for(int i=1;i<b.size();i++)
      assert(T.directed_branch(b[i]).target() == root);
    LC.root = root;

    ublas::matrix<int> index = subA_index_any(b,A,T,req,seq);

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
      for(int i=0;i<S.size1();i++)
	for(int j=0;j<S.size2();j++)
	  S(i,j) = 0;

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
    return L;
  }

  efloat_t other_subst(const data_partition& P, const vector<int>& nodes) 
  {
    const alignment& A = *P.A;
    const Tree& T = *P.T;
    Likelihood_Cache& LC = P.LC;

    IF_DEBUG(int n_br =) calculate_caches(P);
#ifndef NDEBUG
    std::clog<<"other_subst: Peeled on "<<n_br<<" branches.\n";
#endif

    // compute root branches
    vector<int> rb;
    for(const_in_edges_iterator i = T[LC.root].branches_in();i;i++)
      rb.push_back(*i);

    // get the relationships with the sub-alignments
    ublas::matrix<int> index1 = subA_index_none(rb,A,T,nodes);
    efloat_t Pr1 = calc_root_probability(P,rb,index1);

#ifndef NDEBUG
    ublas::matrix<int> index2 = subA_index_any(rb,A,T,nodes);
    ublas::matrix<int> index  = subA_index(rb,A,T);

    efloat_t Pr2 = calc_root_probability(P,rb,index2);
    efloat_t Pr  = calc_root_probability(P,rb,index);

    assert(std::abs(log(Pr1 * Pr2) - log(Pr) ) < 1.0e-9);
#endif

    return Pr1;
  }

  bool check_equal(double x, double y)
  {
    return (std::abs(x-y) < std::min(x,y)*1.0e-9);
  }

  void compare_caches(const alignment& A1, const alignment& A2, const Likelihood_Cache& LC1, const Likelihood_Cache& LC2, int b)
  {
    int L = subA_length(A1,b);
    assert(L == subA_length(A2,b));

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
      std::cerr<<"branch "<<b<<": caches are NOT equal"<<endl;
  }

  void compare_caches(const alignment& A1, const alignment& A2, const Likelihood_Cache& LC1, const Likelihood_Cache& LC2, const Tree& T)
  {
    assert(LC1.root == LC2.root);
    
    vector<const_branchview> branches; branches.reserve(T.n_branches());
    append(T[LC1.root].branches_in(),branches);

    for(int i=0;i<branches.size();i++)
    {
	const const_branchview& db = branches[i];

	append(db.branches_before(),branches);

	if (LC1.up_to_date(db) and LC2.up_to_date(db))
	  compare_caches(A1,A2,LC1,LC2,db);
    }

  }

  efloat_t Pr(const alignment& A,const MatCache& MC,const Tree& T,Likelihood_Cache& LC,
	    const MultiModel& MModel)
  {
    total_likelihood++;

#ifndef DEBUG_CACHING
    if (LC.cv_up_to_date()) {
#ifndef NDEBUG
      std::clog<<"Pr: Using cached value "<<log(LC.cached_value)<<"\n";
#endif
      return LC.cached_value;
    }
#endif

#ifndef NDEBUG
    subA_index_check_footprint(A,T);
    subA_index_check_regenerate(A,T);
#endif

    IF_DEBUG(int n_br =) calculate_caches(A,MC,T,LC,MModel);
#ifndef NDEBUG
    std::clog<<"Pr: Peeled on "<<n_br<<" branches.\n";
#endif

    // compute root branches
    vector<int> rb;
    for(const_in_edges_iterator i = T[LC.root].branches_in();i;i++)
      rb.push_back(*i);

    // get the relationships with the sub-alignments
    ublas::matrix<int> index = subA_index(rb,A,T);

    // get the probability
    efloat_t Pr = calc_root_probability(A,T,LC,MModel,rb,index);

    LC.cached_value = Pr;
    LC.cv_up_to_date() = true;

    return Pr;
  }

  efloat_t Pr(const data_partition& P,Likelihood_Cache& LC) {
    return Pr(*P.A, P.MC, *P.T, LC, P.SModel());
  }

  efloat_t Pr(const data_partition& P) {
    efloat_t result = Pr(P, P.LC);

#ifdef DEBUG_CACHING
    data_partition P2 = P;
    P2.LC.invalidate_all();
    invalidate_subA_index_all(*P2.A);
    for(int i=0;i<P2.T->n_branches();i++)
      P2.setlength(i,P2.T->branch(i).length());
    efloat_t result2 = Pr(P2, P2.LC);

    if (std::abs(log(result) - log(result2))  > 1.0e-9) {
      std::cerr<<"Pr: diff = "<<log(result)-log(result2)<<std::endl;
      compare_caches(*P.A, *P2.A, P.LC, P2.LC, *P.T);
      std::abort();
    }
#endif

    return result;
  }
}
