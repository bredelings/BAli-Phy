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

#include "likelihood.H"
#include "ops.H"
#include "models/parameters.H"
#include "sequence/alphabet.H"
#include "util/rng.H"
#include "util/log-level.H"
#include <cmath>
#include <valarray>
#include <vector>
#include "util/set.H"
#include "math/logprod.H"
#include "dp/hmm.H"
#include "dp/2way.H"
#include <boost/dynamic_bitset.hpp>

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
using std::optional;

// This file assumes that 
// * the matrix is reversible.  This means that we evaluate
//   frequencies at the root - even for insertions, where they actually
//   apply somewhere down the tree.
//
// * we don't need to work in log space for a single column
//
// * 

pair<int,int> sample(const Matrix& M)
{
    double total = element_sum(M);
    if (std::isnan(total))
        throw myexception()<<"substitution.cc:sample(M): Matrix contains NaN!\n";

    double r = uniform()*total;

    double sum = 0;
    for(int m=0;m<M.size1();m++)
        for(int l=0;l<M.size2();l++)
        {
            sum += M(m,l);
            if (r <= sum)
                return {m,l};
        }

    return {-2,-2};
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
    long int total_root_clv_length=0;

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

    inline double sum(const Matrix& Q,int l1, int l2, const alphabet& a)
    {
        double total=0;
        for(int l=0;l<a.size();l++)
            if (a.matches(l,l2))
                total += Q(l1,l);
        return total;
    }


    log_double_t calc_root_probability(const Likelihood_Cache_Branch& LCB1,
                                       const Likelihood_Cache_Branch& LCB2,
                                       const Likelihood_Cache_Branch& LCB3,
                                       const pairwise_alignment_t& A0,
                                       const pairwise_alignment_t& A1,
                                       const pairwise_alignment_t& A2,
                                       const Matrix& F)
    {
        assert(LCB1.n_columns() == A0.length1());
        assert(LCB2.n_columns() == A1.length1());
        assert(LCB3.n_columns() == A2.length1());
        total_calc_root_prob++;

        const int n_models = F.size1();
        const int n_states = F.size2();
        const int matrix_size = n_models * n_states;

        assert(n_models == LCB1.n_models());
        assert(n_states == LCB1.n_states());

        assert(n_models == LCB2.n_models());
        assert(n_states == LCB2.n_states());

        assert(n_models == LCB3.n_models());
        assert(n_states == LCB3.n_states());

#ifdef DEBUG_SUBSTITUTION
        // scratch matrix
        Matrix S(n_models,n_states);
#endif

        log_prod total;
        int scale = 0;
        const int AL0 = A0.size();
        const int AL1 = A1.size();
        const int AL2 = A2.size();
        int s0=0,s1=0,s2=0,s3=0;
        assert(A0.length2() == A1.length2());
        assert(A0.length2() == A2.length2());
        for(int i0=0,i1=0,i2=0;;)
        {
            while(i0 < AL0 and not A0.has_character2(i0))
            {
                assert(A0.has_character1(i0));
                double p_col = element_prod_sum(F.begin(), LCB1[s0], matrix_size );
                assert(std::isnan(p_col) or (0 <= p_col and p_col <= 1.00000000001));
                total *= p_col;
                scale += LCB1.scale(s0);
                i0++;
                s0++;
                total_root_clv_length++;
            }
            while (i1 < AL1 and not A1.has_character2(i1))
            {
                assert(A1.has_character1(i1));
                double p_col = element_prod_sum(F.begin(), LCB2[s1], matrix_size );
                assert(std::isnan(p_col) or (0 <= p_col and p_col <= 1.00000000001));
                total *= p_col;
                scale += LCB2.scale(s1);
                i1++;
                s1++;
                total_root_clv_length++;
            }
            while (i2 < AL2 and not A2.has_character2(i2))
            {
                assert(A2.has_character1(i2));
                double p_col = element_prod_sum(F.begin(), LCB3[s2], matrix_size );
                assert(std::isnan(p_col) or (0 <= p_col and p_col <= 1.00000000001));
                total *= p_col;
                scale += LCB3.scale(s2);
                i2++;
                s2++;
                total_root_clv_length++;
            }

            if (i2 >= AL2)
            {
                assert(i0 == AL0);
                assert(i1 == AL1);
                break;
            }
            else
            {
                assert(i0 < AL0 and i1 < AL1 and i2 < AL2);
                assert(A0.has_character2(i0) and A1.has_character2(i1) and A2.has_character2(i2));
            }

            bool not_gap0 = A0.has_character1(i0);
            bool not_gap1 = A1.has_character1(i1);
            bool not_gap2 = A2.has_character1(i2);
            i0++;
            i1++;
            i2++;

            const double* m[3];
            int mi=0;
            if (not_gap0)
            {
                m[mi++] = LCB1[s0];
                scale += LCB1.scale(s0);
                s0++;
            }
            if (not_gap1)
            {
                m[mi++] = LCB2[s1];
                scale += LCB2.scale(s1);
                s1++;
            }
            if (not_gap2)
            {
                m[mi++] = LCB3[s2];
                scale += LCB3.scale(s2);
                s2++;
            }

            double p_col = 1;
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
            if (not_gap0)
            if (not_gap1)
                element_prod_modify(S.begin(),LCB2[s1], matrix_size);
            if (not_gap2)
                element_prod_modify(S.begin(),LCB3[s2], matrix_size);

            //------------ Check that individual models are not crazy -------------//
            for(int m=0;m<n_models;m++) {
                double p_model=0;
                for(int s=0;s<n_states;s++)
                    p_model += S(m,s);
                // A specific model (e.g. the INV model) could be impossible
                assert(std::isnan(p_model) or (0 <= p_model and p_model <= 1.00000000001));
            }

            double p_col2 = element_sum(S);

            assert((p_col - p_col2)/std::max(p_col,p_col2) < 1.0e-9);
#endif

            // SOME model must be possible
            assert(std::isnan(p_col) or (0 <= p_col and p_col <= 1.00000000001));

            // This might do a log( ) operation.
            total *= p_col;
            //      std::clog<<" i = "<<i<<"   p = "<<p_col<<"  total = "<<total<<"\n";

            s3++;
            total_root_clv_length++;
        }

        log_double_t Pr = total;
        Pr *= LCB1.other_subst;
        Pr *= LCB2.other_subst;
        Pr *= LCB3.other_subst;
        Pr.log() += log_scale_min * scale;
        if (std::isnan(Pr.log()) and log_verbose > 0)
        {
            std::cerr<<"calc_root_probability: probability is NaN!\n";
            return log_double_t(0.0);
        }
        return Pr;
    }

    object_ptr<const Likelihood_Cache_Branch>
    simple_sequence_likelihoods(const EVector& sequence,
				const alphabet& a,
				const EVector& smap,
				int n_models)
    {
	int n_states = smap.size();
	int matrix_size = n_models * n_states;

	int L = sequence.size();

	auto LCB = object_ptr<Likelihood_Cache_Branch>(new Likelihood_Cache_Branch(L, n_models, n_states));

        for(int i=0;i<L;i++)
	{
	    int letter = sequence[i].as_int();

	    double* S = (*LCB)[i];

            for(int k=0; k<matrix_size; k++)
                S[k] = 1.0;

	    // We need to zero out the inconsistent characters.
	    // Observing the complete state doesn't decouple subtrees unless there is only 1 mixture component.
	    if (letter >= 0)
	    {
		auto& ok = a.letter_mask(letter);
		for(int m=0;m<n_models;m++)
		{
		    for(int s1=0;s1<n_states;s1++)
		    {
			int l = smap[s1].as_int();
			if (not ok[l])
			{
			    // Pr *= Pr(observation | state )
			    // Currently we are doing Pr *= Pr(observation | letter(state))
			    // So maybe I should make a Pr(observation | state) matrix.
			    S[m*n_states + s1] = 0;
			}
		    }
		}
	    }
	}

	return LCB;
    }

    inline double sum(const Matrix& Q, const EVector& smap, int s1, int l)
    {
        double total = 0;
        int n_states = smap.size();

        for(int s2=0; s2<n_states; s2++)
            if (smap[s2].as_int() == l)
                total += Q(s1,s2);

        assert(total - 1.0 < 1.0e-9*Q.size1());
        return total;
    }

    inline double sum(const Matrix& Q,const EVector& smap,
                      int s1, int l2, const alphabet& a)
    {
        double total=0;

        for(int s=0;s<smap.size();s++)
            if (a.matches(smap[s].as_int(),l2))
                total += Q(s1,s);

        assert(total - 1.0 < 1.0e-9*Q.size1());
        return total;
    }


    object_ptr<const Likelihood_Cache_Branch>
    peel_leaf_branch_simple(const EVector& sequence, const alphabet& a, const EVector& transition_P)
    {
        total_peel_leaf_branches++;

        int L0 = sequence.size();

        const int n_models  = transition_P.size();
        const int n_states  = transition_P[0].as_<Box<Matrix>>().size1();
        const int matrix_size = n_models * n_states;
        const int n_letters = a.n_letters();

        auto LCB = object_ptr<Likelihood_Cache_Branch>(new Likelihood_Cache_Branch(L0, n_models, n_states));
    
        assert(n_states >= n_letters and n_states%n_letters == 0);

        for(int i=0;i<L0;i++)
        {
            double* R = (*LCB)[i];
            // compute the distribution at the parent node
            int l2 = sequence[i].as_int();

            if (a.is_letter(l2))
                for(int m=0;m<n_models;m++) 
                {
                    const Matrix& Q = transition_P[m].as_<Box<Matrix>>();
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
                    const Matrix& Q = transition_P[m].as_<Box<Matrix>>();
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
                element_assign(R, 1.0, matrix_size);
        }

        return LCB;
    }

    bool is_iota(const EVector& v)
    {
        for(int i=0;i<v.size();i++)
            if (i != v[i].as_int())
                return false;
        return true;
    }


    // This version differs from the 'simple' version because it has to use sum(Q,smap,s1,l2)
    // instead of just Q(s1,l2).
    object_ptr<const Likelihood_Cache_Branch>
    peel_leaf_branch(const EVector& sequence, const alphabet& a, const EVector& transition_P, const EVector& smap)
    {
        // Do this before accessing matrices or other_subst
        int L0 = sequence.size();

        const int n_models  = transition_P.size();
        const int n_states  = transition_P[0].as_<Box<Matrix>>().size1();
        const int matrix_size = n_models * n_states;
        const int n_letters = a.n_letters();

        if (n_states == n_letters and is_iota(smap))
            return peel_leaf_branch_simple(sequence, a, transition_P);

        total_peel_leaf_branches++;

        auto LCB = object_ptr<Likelihood_Cache_Branch>(new Likelihood_Cache_Branch(L0, n_models, n_states));

        assert(n_states >= n_letters and n_states%n_letters == 0);

        for(int i=0;i<L0;i++)
        {
            double* R = (*LCB)[i];
            // compute the distribution at the parent node
            int l2 = sequence[i].as_int();

            if (a.is_letter(l2))
                for(int m=0;m<n_models;m++) {
                    const Matrix& Q = transition_P[m].as_<Box<Matrix>>();
                    for(int s1=0;s1<n_states;s1++)
                        R[m*n_states + s1] = sum(Q,smap,s1,l2);
                }
            else if (a.is_letter_class(l2)) {
                for(int m=0;m<n_models;m++) {
                    const Matrix& Q = transition_P[m].as_<Box<Matrix>>();
                    for(int s1=0;s1<n_states;s1++)
                        R[m*n_states + s1] = sum(Q,smap,s1,l2,a);
                }
            }
            else
                element_assign(R, 1, matrix_size);
        }

        return LCB;
    }

    object_ptr<const Likelihood_Cache_Branch>
    peel_internal_branch(const Likelihood_Cache_Branch& LCB1,
                         const Likelihood_Cache_Branch& LCB2,
                         const pairwise_alignment_t& A0,
                         const pairwise_alignment_t& A1,
                         const EVector& transition_P,
                         const Matrix& F)
    {
        total_peel_internal_branches++;

        const int n_models = transition_P.size();
        const int n_states = transition_P[0].as_<Box<Matrix>>().size1();
        const int matrix_size = n_models * n_states;

        // get the relationships with the sub-alignments for the (two) branches behind b0

        // Do this before accessing matrices or other_subst
        auto LCB3 = object_ptr<Likelihood_Cache_Branch>(new Likelihood_Cache_Branch(A0.length2(), n_models, n_states));
        assert(A0.length2() == A1.length2());
        assert(A0.length1() == LCB1.n_columns());
        assert(A1.length1() == LCB2.n_columns());

        // scratch matrix
        double* S = LCB3->scratch(0);

        Matrix ones(n_models, n_states);
        element_assign(ones, 1);

        log_prod total;
        int total_scale = 0;
        const int AL0 = A0.size();
        const int AL1 = A1.size();
        int s0=0,s1=0,s2=0;
        for(int i0=0,i1=0;;)
        {
            while (i0 < AL0 and not A0.has_character2(i0))
            {
                assert(A0.has_character1(i0));
                double p_col = element_prod_sum(F.begin(), LCB1[s0], matrix_size );
                assert(std::isnan(p_col) or (0 <= p_col and p_col <= 1.00000000001));
                total *= p_col;
                total_scale += LCB1.scale(s0);
                i0++;
                s0++;
            }
            while (i1 < AL1 and not A1.has_character2(i1))
            {
                assert(A1.has_character1(i1));
                double p_col = element_prod_sum(F.begin(), LCB2[s1], matrix_size );
                assert(std::isnan(p_col) or (0 <= p_col and p_col <= 1.00000000001));
                total *= p_col;
                total_scale += LCB2.scale(s1);
                i1++;
                s1++;
            }
            if (i1 >= AL1)
            {
                assert(i0 == AL0);
                break;
            }
            else
            {
                assert(i0 < AL0 and i1 < AL1);
                assert(A0.has_character2(i0) and A1.has_character2(i1));
            }

            int scale = 0;
            const double* C = S;
            bool not_gap0 = A0.has_character1(i0);
            bool not_gap1 = A1.has_character1(i1);
            i0++;
            i1++;
            if (not_gap0 and not_gap1)
            {
                element_prod_assign(S, LCB1[s0], LCB2[s1], matrix_size);
                scale = LCB1.scale(s0) + LCB2.scale(s1);
                s0++;
                s1++;
            }
            else if (not_gap0)
            {
                C = LCB1[s0];
                scale = LCB1.scale(s0);
                s0++;
            }
            else if (not_gap1)
            {
                C = LCB2[s1];
                scale = LCB2.scale(s1);
                s1++;
            }
            else
                C = ones.begin();  // Columns like this would not be in subA_index_leaf, but might be in subA_index_internal

            // propagate from the source distribution
            double* R = (*LCB3)[s2];            //name the result matrix
	    propagate(R, n_models, n_states, scale, transition_P, C);
            LCB3->scale(s2) = scale;
            s2++;
        }

        LCB3->other_subst = LCB1.other_subst * LCB2.other_subst * total;
        LCB3->other_subst.log() += total_scale*log_scale_min;
        return LCB3;
    }

    object_ptr<const Likelihood_Cache_Branch>
    peel_branch(const EVector& LCN,
		const EVector& LCB,
		const EVector& A_,
		const EVector& transition_P,
		const Matrix& F)
    {
        total_peel_internal_branches++;

        const int n_models = transition_P.size();
        const int n_states = transition_P[0].as_<Box<Matrix>>().size1();
        const int matrix_size = n_models * n_states;

        auto node_cache = [&](int i) -> auto& { return LCN[i].as_<Likelihood_Cache_Branch>(); };
        auto cache = [&](int i) -> auto& { return LCB[i].as_<Likelihood_Cache_Branch>(); };
        auto A = [&](int i) -> auto& { return A_[i].as_<Box<pairwise_alignment_t>>();};

	int n_sequences = LCN.size();
	int n_branches_in = LCB.size();
	assert(not LCN.empty() or not A_.empty());
	int L = (LCN.empty()) ? A(0).length2() : node_cache(0).n_columns();

        auto LCB_OUT = object_ptr<Likelihood_Cache_Branch>(new Likelihood_Cache_Branch(L, n_models, n_states));

#ifndef NDEBUG
	// Check that all the sequences have the right length.
	for(int i=0; i<n_sequences;i++)
	    assert(node_cache(i).n_columns() == L);

	// Check that all the alignments have the right length for both sequences.
	assert(A_.size() == n_branches_in);
	for(int i=0; i<n_branches_in; i++)
	{
	    assert(A(i).length2() == L);
	    assert(A(i).length1() == cache(i).n_columns());
	}
#endif

        // scratch matrix
        double* S = LCB_OUT->scratch(0);

        log_prod total;
        int total_scale = 0;

	vector<int> s(n_branches_in, 0);
	int s_out = 0;
	vector<int> i(n_branches_in, 0);
        for(;;)
        {
	    for(int j =0;j < n_branches_in; j++)
	    {
		auto& a = A(j);
		auto& lcb = cache(j);
		auto& ij = i[j];
		auto& sj = s[j];
		while (ij < a.size() and not a.has_character2(ij))
		{
		    assert(a.has_character1(ij));
		    double p_col = element_prod_sum(F.begin(), lcb[sj], matrix_size );
		    assert(std::isnan(p_col) or (0 <= p_col and p_col <= 1.00000000001));
		    total *= p_col;
		    total_scale += lcb.scale(sj);
		    ij++;
		    sj++;
		}
	    }
            if (s_out == L)
            {
		for(int j=0;j<n_branches_in;j++)
		    assert(i[j] == A(j).size());
                break;
            }
	    else
	    {
		for(int j=0;j<n_branches_in;j++)
		{
		    assert(i[j] < A(j).size());
		    assert(A(j).has_character2(i[j]));
		}
	    }

	    int scale = 0;
	    for(int k=0; k<matrix_size; k++)
		S[k] = 1.0;
	    for(int j=0;j<n_branches_in;j++)
	    {
		if (A(j).has_character1(i[j]))
		{
		    auto& lcb = cache(j);
		    element_prod_assign(S, lcb[s[j]], matrix_size);
		    scale += lcb.scale(s[j]);
		    s[j]++;
		}
		i[j]++;
	    }

	    // Handle observed sequences at the node.
	    for(int j=0;j<n_sequences;j++)
		element_prod_assign(S, node_cache(j)[s_out], matrix_size);

	    // propagate from the source distribution
	    double* R = (*LCB_OUT)[s_out];            //name the result matrix
	    propagate(R, n_models, n_states, scale, transition_P, S);
            LCB_OUT->scale(s_out++) = scale;
        }

	LCB_OUT->other_subst = total;
        LCB_OUT->other_subst.log() += total_scale*log_scale_min;
	for(int j=0;j<n_branches_in;j++)
	    LCB_OUT->other_subst *= cache(j).other_subst;
        return LCB_OUT;
    }


    object_ptr<const Likelihood_Cache_Branch>
    peel_branch2_at_root(const EVector& LCN,
			 const EVector& LCB,
			 const EVector& A_,
			 const EVector& transition_P,
			 const Matrix& F)
    {
        total_peel_internal_branches++;

        const int n_models = transition_P.size();
        const int n_states = transition_P[0].as_<Box<Matrix>>().size1();
        const int matrix_size = n_models * n_states;

        auto node_cache = [&](int i) -> auto& { return LCN[i].as_<Likelihood_Cache_Branch>(); };
        auto cache = [&](int i) -> auto& { return LCB[i].as_<Likelihood_Cache_Branch>(); };
        auto A = [&](int i) -> auto& { return A_[i].as_<Box<pairwise_alignment_t>>();};

	int n_sequences = LCN.size();
	int n_branches_in = LCB.size();
	assert(not LCN.empty() or not A_.empty());
	int L = (LCN.empty()) ? A(0).length2() : node_cache(0).n_columns();

        auto LCB_OUT = object_ptr<Likelihood_Cache_Branch>(new Likelihood_Cache_Branch(L, n_models, n_states));

#ifndef NDEBUG
	// Check that all the sequences have the right length.
	for(int i=0; i<n_sequences;i++)
	    assert(node_cache(i).n_columns() == L);

	// Check that all the alignments have the right length for both sequences.
	assert(A_.size() == n_branches_in);
	for(int i=0; i<n_branches_in; i++)
	{
	    assert(A(i).length2() == L);
	    assert(A(i).length1() == cache(i).n_columns());
	}
#endif

        // scratch matrix
        double* S = LCB_OUT->scratch(0);

        log_prod total;
        int total_scale = 0;

	vector<int> s(n_branches_in, 0);
	int s_out = 0;
	vector<int> i(n_branches_in, 0);
        for(;;)
        {
	    for(int j =0;j < n_branches_in; j++)
	    {
		auto& a = A(j);
		auto& lcb = cache(j);
		auto& ij = i[j];
		auto& sj = s[j];
		while (ij < a.size() and not a.has_character2(ij))
		{
		    assert(a.has_character1(ij));
		    double p_col = element_prod_sum(F.begin(), lcb[sj], matrix_size );
		    assert(std::isnan(p_col) or (0 <= p_col and p_col <= 1.00000000001));
		    total *= p_col;
		    total_scale += lcb.scale(sj);
		    ij++;
		    sj++;
		}
	    }
            if (s_out == L)
            {
		for(int j=0;j<n_branches_in;j++)
		    assert(i[j] == A(j).size());
                break;
            }
	    else
	    {
		for(int j=0;j<n_branches_in;j++)
		{
		    assert(i[j] < A(j).size());
		    assert(A(j).has_character2(i[j]));
		}
	    }

	    int scale = 0;
	    element_assign(S, F.begin(), matrix_size);
	    for(int j=0;j<n_branches_in;j++)
	    {
		if (A(j).has_character1(i[j]))
		{
		    auto& lcb = cache(j);
		    element_prod_assign(S, lcb[s[j]], matrix_size);
		    scale += lcb.scale(s[j]);
		    s[j]++;
		}
		i[j]++;
	    }

	    // Handle observed sequences at the node.
	    for(int j=0;j<n_sequences;j++)
		element_prod_assign(S, node_cache(j)[s_out], matrix_size);

	    // propagate from the source distribution
	    double* R = (*LCB_OUT)[s_out];            //name the result matrix
	    propagate(R, n_models, n_states, scale, transition_P, S);
            LCB_OUT->scale(s_out++) = scale;
        }

	LCB_OUT->other_subst = total;
        LCB_OUT->other_subst.log() += total_scale*log_scale_min;
	for(int j=0;j<n_branches_in;j++)
	    LCB_OUT->other_subst *= cache(j).other_subst;
	LCB_OUT->away_from_root_WF = Matrix(0,0);
        return LCB_OUT;
    }


	
    object_ptr<const Likelihood_Cache_Branch>
    peel_branch2_away_from_root(const EVector& LCN,
				const EVector& LCB,
				const EVector& A_,
				const EVector& transition_P,
				const Matrix& F)
    {
	total_peel_internal_branches++;

        const int n_models = transition_P.size();
        const int n_states = transition_P[0].as_<Box<Matrix>>().size1();
        const int matrix_size = n_models * n_states;

        auto node_cache = [&](int i) -> auto& { return LCN[i].as_<Likelihood_Cache_Branch>(); };
        auto cache = [&](int i) -> auto& { return LCB[i].as_<Likelihood_Cache_Branch>(); };
        auto A = [&](int i) -> auto& { return A_[i].as_<Box<pairwise_alignment_t>>();};

	int n_sequences = LCN.size();
	int n_branches_in = LCB.size();
	assert(not LCN.empty() or not A_.empty());
	int L = (LCN.empty()) ? A(0).length2() : node_cache(0).n_columns();

        auto LCB_OUT = object_ptr<Likelihood_Cache_Branch>(new Likelihood_Cache_Branch(L, n_models, n_states));

#ifndef NDEBUG
	// Check that all the sequences have the right length.
	for(int i=0; i<n_sequences;i++)
	    assert(node_cache(i).n_columns() == L);

	// Check that all the alignments have the right length for both sequences.
	assert(A_.size() == n_branches_in);
	for(int i=0; i<n_branches_in; i++)
	{
	    assert(A(i).length2() == L);
	    assert(A(i).length1() == cache(i).n_columns());
	}
#endif

        // scratch matrix
        double* S = LCB_OUT->scratch(0);

        log_prod total;
        int total_scale = 0;

	vector<int> s(n_branches_in, 0);
	int s_out = 0;
	vector<int> i(n_branches_in, 0);
        for(;;)
        {
	    for(int j =0;j < n_branches_in; j++)
	    {
		auto& a = A(j);
		auto& lcb = cache(j);
		auto& ij = i[j];
		auto& sj = s[j];
		while (ij < a.size() and not a.has_character2(ij))
		{
		    assert(a.has_character1(ij));
		    // Characters coming from the root have already have the root frequencies applied.
		    double p_col = (j==0) ? element_sum(lcb[sj], matrix_size ) : element_prod_sum(F.begin(), lcb[sj], matrix_size );
		    assert(std::isnan(p_col) or (0 <= p_col and p_col <= 1.00000000001));
		    total *= p_col;
		    total_scale += lcb.scale(sj);
		    ij++;
		    sj++;
		}
	    }
            if (s_out == L)
            {
		for(int j=0;j<n_branches_in;j++)
		    assert(i[j] == A(j).size());
                break;
            }
	    else
	    {
		for(int j=0;j<n_branches_in;j++)
		{
		    assert(i[j] < A(j).size());
		    assert(A(j).has_character2(i[j]));
		}
	    }

	    int scale = 0;
	    if (A(0).has_character2(i[0]))
		element_assign(S, 1.0, matrix_size);
	    else
		element_assign(S, F.begin(), matrix_size);

	    for(int j=0;j<n_branches_in;j++)
	    {
		if (A(j).has_character1(i[j]))
		{
		    auto& lcb = cache(j);
		    element_prod_assign(S, lcb[s[j]], matrix_size);
		    scale += lcb.scale(s[j]);
		    s[j]++;
		}
		i[j]++;
	    }

	    // Handle observed sequences at the node.
	    for(int j=0;j<n_sequences;j++)
		element_prod_assign(S, node_cache(j)[s_out], matrix_size);

	    // propagate from the source distribution
	    double* R = (*LCB_OUT)[s_out];            //name the result matrix
	    propagate(R, n_models, n_states, scale, transition_P, S);
            LCB_OUT->scale(s_out++) = scale;
        }

	LCB_OUT->other_subst = total;
        LCB_OUT->other_subst.log() += total_scale*log_scale_min;
	for(int j=0;j<n_branches_in;j++)
	    LCB_OUT->other_subst *= cache(j).other_subst;
	LCB_OUT->away_from_root_WF = Matrix(0,0);
        return LCB_OUT;
    }

    object_ptr<const Likelihood_Cache_Branch>
    peel_branch2(const EVector& LCN,
		 const EVector& LCB,
		 const EVector& A_,
		 const EVector& transition_P,
		 const Matrix& F,
		 bool away_from_root)
    {
	if (not away_from_root)
	    return peel_branch(LCN, LCB, A_, transition_P, F);
	else
	{
	    optional<int> away_from_root_index;
	    for(int j=0;j<LCB.size();j++)
		if (LCB[j].as_<Likelihood_Cache_Branch>().away_from_root())
		{
		    assert(not away_from_root_index.has_value());
		    away_from_root_index = j;
		}

	    if (not away_from_root_index)
		return peel_branch2_at_root(LCN, LCB, A_, transition_P, F);
	    else
	    {
		auto LCB2 = LCB;
		EVector A = A_;
		if (*away_from_root_index != 0)
		{
		    std::swap(LCB2[0], LCB2[*away_from_root_index]);
		    std::swap(A[0], A[*away_from_root_index]);
		}

		return peel_branch2_away_from_root(LCN, LCB2, A, transition_P, F);
	    }
	}
    }


    log_double_t calc_root_prob(const EVector& LCN,
				const EVector& LCB,
				const EVector& A_,
				const Matrix& F)
    {
        total_calc_root_prob++;

        const int n_models = F.size1();
        const int n_states = F.size2();
        const int matrix_size = n_models * n_states;

        auto node_cache = [&](int i) -> auto& { return LCN[i].as_<Likelihood_Cache_Branch>(); };
        auto cache = [&](int i) -> auto& { return LCB[i].as_<Likelihood_Cache_Branch>(); };
        auto A = [&](int i) -> auto& { return A_[i].as_<Box<pairwise_alignment_t>>();};

	int n_sequences = LCN.size();
	int n_branches_in = LCB.size();
	assert(not LCN.empty() or not A_.empty());
	int L = (LCN.empty()) ? A(0).length2() : node_cache(0).n_columns();

#ifndef NDEBUG
	// Check that all the sequences have the right length.
	for(int i=0; i<n_sequences; i++)
	    assert(node_cache(i).n_columns() == L);

	// Check that all the alignments have the right length for both sequences.
	assert(A_.size() == n_branches_in);
	for(int i=0; i<n_branches_in; i++)
	{
	    assert(A(i).length2() == L);
	    assert(A(i).length1() == cache(i).n_columns());

	    assert(n_models == cache(i).n_models());
	    assert(n_states == cache(i).n_states());
	}
#endif

        // scratch matrix
        Matrix SMAT(n_models,n_states);
        double* S = SMAT.begin();

        log_prod total;
        int total_scale = 0;

	vector<int> s(n_branches_in, 0);
	vector<int> i(n_branches_in, 0);
        for(int s_out=0;;s_out++)
        {
	    for(int j =0;j < n_branches_in; j++)
	    {
		auto& a = A(j);
		auto& lcb = LCB[j].as_<Likelihood_Cache_Branch>();
		auto& ij = i[j];
		auto& sj = s[j];
		while (ij < a.size() and not a.has_character2(ij))
		{
		    assert(a.has_character1(ij));
		    double p_col = element_prod_sum(F.begin(), lcb[sj], matrix_size );
		    assert(std::isnan(p_col) or (0 <= p_col and p_col <= 1.00000000001));
		    total *= p_col;
		    total_scale += lcb.scale(sj);
		    ij++;
		    sj++;
		}
	    }
            if (s_out == L)
            {
		for(int j=0;j<n_branches_in;j++)
		    assert(i[j] == A(j).size());
                break;
            }
	    else
	    {
		for(int j=0;j<n_branches_in;j++)
		{
		    assert(i[j] < A(j).size());
		    assert(A(j).has_character2(i[j]));
		}
	    }

	    element_assign(S, F.begin(), matrix_size);

	    for(int j=0;j<n_branches_in;j++)
	    {
		if (A(j).has_character1(i[j]))
		{
		    auto& lcb = cache(j);
		    element_prod_assign(S, lcb[s[j]], matrix_size);
		    total_scale += lcb.scale(s[j]);
		    s[j]++;
		}
		i[j]++;
	    }

	    // Handle observed sequences at the node.
	    for(int j=0;j<n_sequences;j++)
		element_prod_assign(S, node_cache(j)[s_out], matrix_size);

	    double p_col = element_sum(S, matrix_size);

	    total *= p_col;

	    total_root_clv_length++;
        }

        log_double_t Pr = total;
	for(int i=0;i<n_branches_in;i++)
	    Pr *= LCB[i].as_<Likelihood_Cache_Branch>().other_subst;

        Pr.log() += log_scale_min * total_scale;

        if (std::isnan(Pr.log()) and log_verbose > 0)
        {
            std::cerr<<"calc_root_probability: probability is NaN!\n";
            return log_double_t(0.0);
        }

        return Pr;
    }

    log_double_t calc_root_prob2_not_at_root(const EVector& LCN,
					     const EVector& LCB,
					     const EVector& A_,
					     const Matrix& F)
    {
        total_calc_root_prob++;

        const int n_models = F.size1();
        const int n_states = F.size2();
        const int matrix_size = n_models * n_states;

        auto node_cache = [&](int i) -> auto& { return LCN[i].as_<Likelihood_Cache_Branch>(); };
        auto cache = [&](int i) -> auto& { return LCB[i].as_<Likelihood_Cache_Branch>(); };
        auto A = [&](int i) -> auto& { return A_[i].as_<Box<pairwise_alignment_t>>();};

	int n_sequences = LCN.size();
	int n_branches_in = LCB.size();
	assert(not LCN.empty() or not A_.empty());
	int L = (LCN.empty()) ? A(0).length2() : node_cache(0).n_columns();

#ifndef NDEBUG
	// Check that all the sequences have the right length.
	for(int i=0; i<n_sequences; i++)
	    assert(node_cache(i).n_columns() == L);

	// Check that all the alignments have the right length for both sequences.
	assert(A_.size() == n_branches_in);
	for(int i=0; i<n_branches_in; i++)
	{
	    assert(A(i).length2() == L);
	    assert(A(i).length1() == cache(i).n_columns());

	    assert(n_models == cache(i).n_models());
	    assert(n_states == cache(i).n_states());
	}
#endif

        // scratch matrix
        Matrix SMAT(n_models,n_states);
        double* S = SMAT.begin();

        log_prod total;
        int total_scale = 0;

	vector<int> s(n_branches_in, 0);
	vector<int> i(n_branches_in, 0);
        for(int s_out=0;;s_out++)
        {
	    for(int j =0;j < n_branches_in; j++)
	    {
		auto& a = A(j);
		auto& lcb = LCB[j].as_<Likelihood_Cache_Branch>();
		auto& ij = i[j];
		auto& sj = s[j];
		while (ij < a.size() and not a.has_character2(ij))
		{
		    assert(a.has_character1(ij));
		    double p_col = (j == 0) ? element_sum(lcb[sj], matrix_size) : element_prod_sum(F.begin(), lcb[sj], matrix_size );
		    assert(std::isnan(p_col) or (0 <= p_col and p_col <= 1.00000000001));
		    total *= p_col;
		    total_scale += lcb.scale(sj);
		    ij++;
		    sj++;
		}
	    }
            if (s_out == L)
            {
		for(int j=0;j<n_branches_in;j++)
		    assert(i[j] == A(j).size());
                break;
            }
	    else
	    {
		for(int j=0;j<n_branches_in;j++)
		{
		    assert(i[j] < A(j).size());
		    assert(A(j).has_character2(i[j]));
		}
	    }

	    if (A(0).has_character2(i[0]))
		element_assign(S, 1.0, matrix_size);
	    else
		element_assign(S, F.begin(), matrix_size);

	    for(int j=0;j<n_branches_in;j++)
	    {
		if (A(j).has_character1(i[j]))
		{
		    auto& lcb = cache(j);
		    element_prod_assign(S, lcb[s[j]], matrix_size);
		    total_scale += lcb.scale(s[j]);
		    s[j]++;
		}
		i[j]++;
	    }

	    // Handle observed sequences at the node.
	    for(int j=0;j<n_sequences;j++)
		element_prod_assign(S, node_cache(j)[s_out], matrix_size);

	    double p_col = element_sum(S, matrix_size);

	    total *= p_col;

	    total_root_clv_length++;
        }

        log_double_t Pr = total;
	for(int i=0;i<n_branches_in;i++)
	    Pr *= LCB[i].as_<Likelihood_Cache_Branch>().other_subst;

        Pr.log() += log_scale_min * total_scale;

        if (std::isnan(Pr.log()) and log_verbose > 0)
        {
            std::cerr<<"calc_root_probability: probability is NaN!\n";
            return log_double_t(0.0);
        }

        return Pr;
    }

    log_double_t calc_root_prob2(const EVector& LCN,
				 const EVector& LCB,
				 const EVector& A_,
				 const Matrix& F)
    {
	optional<int> away_from_root_index;
	for(int j=0;j<LCB.size();j++)
	    if (LCB[j].as_<Likelihood_Cache_Branch>().away_from_root())
	    {
		assert(not away_from_root_index.has_value());
		away_from_root_index = j;
	    }

	if (not away_from_root_index)
	    return calc_root_prob(LCN, LCB, A_, F);
	else
	{
	    auto LCB2 = LCB;
	    if (*away_from_root_index != 0)
		std::swap(LCB2[0], LCB2[*away_from_root_index]);

	    return calc_root_prob2_not_at_root(LCN, LCB2, A_, F);
	}
    }

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

        // FIXME - its wasteful to do this for each letter.
        auto smap_ptr = P.state_letters();
        auto& smap = *smap_ptr;

        if (a.is_letter(l))
        {
            for(int m=0;m<n_models;m++)
                for(int s=0;s<n_states;s++)
                    if (l == smap[s].as_int())
                        R(m,s) = 1;
        }
        else if (a.is_letter_class(l))
        {
            for(int l2=0;l2<n_letters;l2++)
                if (a.matches(l,l2))
                    for(int m=0;m<n_models;m++)
                        for(int s=0;s<n_states;s++)
                            if (l2 == smap[s].as_int())
                                R(m,s) = 1;
        }
        else
            std::abort();

        return R;
    }


    Likelihood_Cache_Branch
    get_leaf_seq_likelihoods(const data_partition& P, int n, int delta)
    {
        auto node_CLV_ptr = P.get_node_CLV(n);
	auto& node_CLV = *node_CLV_ptr;

        const int n_models = node_CLV.n_models();
        const int n_states = node_CLV.n_states();

        // Compute the likelihood matrices for each letter in the sequence
        int L = node_CLV.n_columns();
        Likelihood_Cache_Branch LCB(L+delta, n_models, n_states);

        for(int i=0;i<delta;i++)
            LCB.set(i, 0);

        for(int i=0;i<L;i++)
	    LCB.set_ptr(i+delta, node_CLV[i]);

        return LCB;
    }

    /// Find the probabilities of each PRESENT letter at the root, given the data at the nodes in 'group'
    Likelihood_Cache_Branch
    get_column_likelihoods(const data_partition& P, const vector<int>& b, const matrix<int>& index, int delta)
    {
        auto t = P.t();

        //------ Check that all branches point to a 'root' node -----------//
        assert(b.size());

        int root = t.target(b[0]);
        for(int i=1;i<b.size();i++)
            assert(t.target(b[i]) == root);

        const int n_models = P.n_base_models();
        const int n_states = P.n_states();
        const int matrix_size = n_models * n_states;
        Likelihood_Cache_Branch LCB(index.size1() + delta, n_models, n_states);

        //Clear the padding matrices
        for(int i=0;i<delta;i++)
	{
            LCB.set(i,0);
	    LCB.scale(i) = 0;
	}

        vector<object_ptr<const Likelihood_Cache_Branch>> cache;
        for(int branch: b)
            cache.push_back(P.cache(branch));

        // For each column in the index (e.g. for each present character at node 'root')
        for(int i=0;i<index.size1();i++) 
        {
            LCB.set(i+delta, 1);

            // Note that it is possible that b.size() == 0, so that
	    // we do ZERO products, and stay at 1.0 for everything.
            auto m = LCB[i+delta];
            int scale = 0;
            for(int j=0;j<b.size();j++) 
            {
                int i0 = index(i,j);
                if (i0 == alphabet::gap) continue;

                element_prod_modify(m, (*cache[j])[i0], matrix_size);
                scale += cache[j]->scale(i0);
            }
            LCB.scale(i+delta) = scale;
        }

        return LCB;
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
            Pr3 *= P.cache(b)->other_subst;

        return Pr3;
    }

    Vector<pair<int,int>> sample_root_sequence(const EVector& LCN,
					       const EVector& LCB,
					       const EVector& A_,
                                               const Matrix& F)
    {

        const int n_models = F.size1();
        const int n_states = F.size2();
        const int matrix_size = n_models * n_states;

        auto node_cache = [&](int i) -> auto& { return LCN[i].as_<Likelihood_Cache_Branch>(); };
        auto cache = [&](int i) -> auto& { return LCB[i].as_<Likelihood_Cache_Branch>(); };
        auto A = [&](int i) -> auto& { return A_[i].as_<Box<pairwise_alignment_t>>();};

	int n_sequences = LCN.size();
	int n_branches_in = LCB.size();
	assert(not LCN.empty() or not A_.empty());
	int L = (LCN.empty()) ? A(0).length2() : node_cache(0).n_columns();

#ifndef NDEBUG
	// Check that all the sequences have the right length.
	for(int i=0; i<n_sequences; i++)
	    assert(node_cache(i).n_columns() == L);

	// Check that all the alignments have the right length for both sequences.
	assert(A_.size() == n_branches_in);
	for(int i=0; i<n_branches_in; i++)
	{
	    assert(A(i).length2() == L);
	    assert(A(i).length1() == cache(i).n_columns());

	    assert(n_models == cache(i).n_models());
	    assert(n_states == cache(i).n_states());
	}
#endif

        // scratch matrix
        Matrix S(n_models,n_states);

	vector<int> s(n_branches_in, 0);
	vector<int> i(n_branches_in, 0);
	Vector<pair<int,int>> ancestral_characters(L);
        for(int s_out=0;;s_out++)
        {
	    for(int j =0;j < n_branches_in; j++)
	    {
		auto& a = A(j);
		auto& ij = i[j];
		while (ij < a.size() and not a.has_character2(ij))
		{
		    assert(a.has_character1(ij));
		    ij++;
		    s[j]++;
		}
	    }
            if (s_out == L)
            {
		for(int j=0;j<n_branches_in;j++)
		    assert(i[j] == A(j).size());
                break;
            }
	    else
	    {
		for(int j=0;j<n_branches_in;j++)
		{
		    assert(i[j] < A(j).size());
		    assert(A(j).has_character2(i[j]));
		}
	    }

	    element_assign(S.begin(), F.begin(), matrix_size);

	    for(int j=0;j<n_branches_in;j++)
	    {
		if (A(j).has_character1(i[j]))
		{
		    element_prod_assign(S.begin(), cache(j)[s[j]], matrix_size);
		    s[j]++;
		}
		i[j]++;
	    }

	    // Handle observed sequences at the node.
	    for(int j=0;j<n_sequences;j++)
		element_prod_assign(S.begin(), node_cache(j)[s_out], matrix_size);

            ancestral_characters[s_out] = sample(S);
        }

        return ancestral_characters;
    }

    Vector<pair<int,int>> sample_branch_sequence(const Vector<pair<int,int>>& parent_seq,
						 const pairwise_alignment_t& parent_A,
						 const EVector& LCN,
						 const EVector& LCB,
						 const EVector& A_,
						 const EVector& transition_P,
						 const Matrix& F)
    {
        total_peel_internal_branches++;

        const int n_models = transition_P.size();
        const int n_states = transition_P[0].as_<Box<Matrix>>().size1();
        const int matrix_size = n_models * n_states;

        auto node_cache = [&](int i) -> auto& { return LCN[i].as_<Likelihood_Cache_Branch>(); };
        auto cache = [&](int i) -> auto& { return LCB[i].as_<Likelihood_Cache_Branch>(); };
        auto A = [&](int i) -> auto& { return A_[i].as_<Box<pairwise_alignment_t>>();};

        // Do this before accessing matrices or other_subst
	int n_sequences = LCN.size();
	int n_branches_in = LCB.size();
	assert(not LCN.empty() or not A_.empty());
	int L = (LCN.empty()) ? A(0).length2() : node_cache(0).n_columns();

#ifndef NDEBUG
	// Check that all the sequences have the right length.
	for(int i=0; i<n_sequences; i++)
	    assert(node_cache(i).n_columns() == L);

	// Check that all the alignments have the right length for both sequences.
	assert(A_.size() == n_branches_in);
	for(int i=0; i<n_branches_in; i++)
	{
	    assert(A(i).length2() == L);
	    assert(A(i).length1() == cache(i).n_columns());

	    assert(n_models == cache(i).n_models());
	    assert(n_states == cache(i).n_states());
	}
#endif

        // scratch matrix
	Matrix SMAT(n_models, n_states);
        double* S = SMAT.begin();

	// index into LCBs
	vector<int> s(n_branches_in, 0);
	// index into node sequence
	int s_node = 0;
	// index into parent sequence
	int s_parent = 0;
	// index into alignments
	vector<int> i(n_branches_in, 0);
	// index into parent_A
	int i_parent = 0;

	Vector<pair<int,int>> ancestral_characters(L);
        for(;;)
        {
	    for(int j =0;j < n_branches_in; j++)
	    {
		auto& a = A(j);
		auto& ij = i[j];
		while (ij < a.size() and not a.has_character2(ij))
		{
		    assert(a.has_character1(ij));
		    ij++;
		    s[j]++;
		}
	    }
	    {
		while (i_parent < parent_A.size() and not parent_A.has_character2(i_parent))
		{
		    assert(parent_A.has_character1(i_parent));
		    i_parent++;
		    s_parent++;
		}
	    }
            if (s_node == L)
            {
		for(int j=0;j<n_branches_in;j++)
		{
		    assert(i[j] == A(j).size());
		    assert(s[j] == A(j).length1());
		}
		assert(i_parent == parent_A.size());
		assert(s_parent == parent_A.length1());
                break;
            }
	    else
	    {
		for(int j=0;j<n_branches_in;j++)
		{
		    assert(i[j] < A(j).size());
		    assert(s[j] <= A(j).length1());
		    assert(A(j).has_character2(i[j]));
		}
		assert(i_parent < parent_A.size());
	    }

	    pair<int,int> state_model_parent(-1,-1);
	    if (i_parent < parent_A.size() and parent_A.has_character1(i_parent))
	    {
		state_model_parent = parent_seq[s_parent];
		s_parent++;
	    }
	    i_parent++;
	    calc_transition_prob_from_parent(SMAT, state_model_parent, transition_P, F);

	    for(int j=0;j<n_branches_in;j++)
	    {
		if (A(j).has_character1(i[j]))
		{
		    auto& lcb = cache(j);
		    element_prod_assign(S, lcb[s[j]], matrix_size);
		    s[j]++;
		}
		i[j]++;
	    }

	    // Handle observed sequences at the node.
	    for(int j=0;j<n_sequences;j++)
		element_prod_assign(S, node_cache(j)[s_node], matrix_size);

            ancestral_characters[s_node] = sample(SMAT);

	    s_node++;
        }

        return ancestral_characters;
    }

}
