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
#include "util/rng.H"
#include "util/range.H"
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
                                int size)
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

inline void element_prod_assign(double* __restrict__ M1,
                                const double* __restrict__ M2,
                                const double* __restrict__ M3,
                                const double* __restrict__ M4,
				int size)
{
    for(int i=0;i<size;i++)
        M1[i] = M2[i]*M3[i]*M4[i];
}

inline void element_prod_assign(double* __restrict__ M1,
                                const double* __restrict__ M2,
                                const double* __restrict__ M3,
                                const double* __restrict__ M4,
                                const double* __restrict__ M5,
				int size)
{
    for(int i=0;i<size;i++)
        M1[i] = M2[i]*M3[i]*M4[i]*M5[i];
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


    struct peeling_info: public vector<int> {
        peeling_info(const TreeInterface& t) { reserve(t.n_branches()); }
    };


    object_ptr<const Box<matrix<int>>>
    alignment_index3(const pairwise_alignment_t& A1, const pairwise_alignment_t& A2, const pairwise_alignment_t& A3)
    {
        auto a10 = convert_to_bits(A1,1,0);
        auto a20 = convert_to_bits(A2,2,0);
        auto a30 = convert_to_bits(A3,3,0);
        auto a0123 = Glue_A(a10, Glue_A(a20,a30));

        auto index = object_ptr<Box<matrix<int>>>(new Box<matrix<int>>);
        *index = get_indices_from_bitpath(a0123, {1,2,3});
        return index;
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

    log_double_t calc_root_probability_SEV(const Likelihood_Cache_Branch& LCB1,
                                           const Likelihood_Cache_Branch& LCB2,
                                           const Likelihood_Cache_Branch& LCB3,
                                           const Matrix& F,
                                           const EVector& counts)
    {
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

        const auto& bits1 = LCB1.bits;
        const auto& bits2 = LCB2.bits;
        const auto& bits3 = LCB3.bits;

        const int L = bits1.size();
        assert(L > 0);
        assert(L == bits2.size());
        assert(L == bits3.size());
        assert(L == counts.size());

        total_root_clv_length += L;

        log_prod total;
        int scale = 0;
        for(int c=0,i1=0,i2=0,i3=0;c<L;c++)
        {
            bool non_gap1 = bits1.test(c);
            bool non_gap2 = bits2.test(c);
            bool non_gap3 = bits3.test(c);

            if ((not non_gap1) and (not non_gap2) and (not non_gap3)) continue;

            const double* m[3];
            int mi=0;

            if (non_gap1)
            {
                m[mi++] = LCB1[i1];
                scale += LCB1.scale(i1);
            }
            if (non_gap2)
            {
                m[mi++] = LCB2[i2];
                scale += LCB2.scale(i2);
            }
            if (non_gap3)
            {
                m[mi++] = LCB3[i3];
                scale += LCB3.scale(i3);
            }

            double p_col = 1.0;
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
            if (non_gap1)
            if (non_gap2)
                element_prod_modify(S.begin(),LCB2[i2], matrix_size);
            if (non_gap3)
                element_prod_modify(S.begin(),LCB3[i3], matrix_size);

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

            if (non_gap1) i1++;
            if (non_gap2) i2++;
            if (non_gap3) i3++;

            total.mult_with_count(p_col, counts[c].as_int());
            //      std::clog<<" i = "<<i<<"   p = "<<p_col<<"  total = "<<total<<"\n";
        }

        log_double_t Pr = total;
        if (std::isnan(Pr.log()) and log_verbose > 0)
        {
            std::cerr<<"calc_root_probability_SEV: probability is NaN!\n";
            return log_double_t(0.0);
        }
        Pr.log() += log_scale_min * scale;
        return Pr;
    }


    // Generalize to degree n>=1?
    log_double_t calc_root_deg2_probability_SEV(const Likelihood_Cache_Branch& LCB1,
                                                const Likelihood_Cache_Branch& LCB2,
                                                const Matrix& F,
                                                const EVector& counts)
    {
        total_calc_root_prob++;

        const int n_models = F.size1();
        const int n_states = F.size2();
        const int matrix_size = n_models * n_states;

        assert(n_models == LCB1.n_models());
        assert(n_states == LCB1.n_states());

        assert(n_models == LCB2.n_models());
        assert(n_states == LCB2.n_states());

#ifdef DEBUG_SUBSTITUTION
        // scratch matrix
        Matrix S(n_models,n_states);
#endif

        const auto& bits1 = LCB1.bits;
        const auto& bits2 = LCB2.bits;

        const int L = bits1.size();
        assert(L > 0);
        assert(L == bits2.size());
        assert(L == counts.size());

        total_root_clv_length += L;

        log_prod total;
        int scale = 0;
        for(int c=0,i1=0,i2=0;c<L;c++)
        {
            bool non_gap1 = bits1.test(c);
            bool non_gap2 = bits2.test(c);

            if ((not non_gap1) and (not non_gap2)) continue;

            const double* m[2];
            int mi=0;

            if (non_gap1)
            {
                m[mi++] = LCB1[i1];
                scale += LCB1.scale(i1);
            }
            if (non_gap2)
            {
                m[mi++] = LCB2[i2];
                scale += LCB2.scale(i2);
            }

            double p_col = 1.0;
            if (mi==2)
                p_col = element_prod_sum(F.begin(), m[0], m[1], matrix_size);
            else if (mi==1)
                p_col = element_prod_sum(F.begin(), m[0], matrix_size);

#ifdef DEBUG_SUBSTITUTION
            //-------------- Set letter & model prior probabilities  ---------------//
            element_assign(S,F);

            //-------------- Propagate and collect information at 'root' -----------//
            if (non_gap1)
                element_prod_modify(S.begin(),LCB1[i1], matrix_size);
            if (non_gap2)
                element_prod_modify(S.begin(),LCB2[i2], matrix_size);

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

            if (non_gap1) i1++;
            if (non_gap2) i2++;

            total.mult_with_count(p_col,counts[c].as_int());
            //      std::clog<<" i = "<<i<<"   p = "<<p_col<<"  total = "<<total<<"\n";
        }

        log_double_t Pr = total;
        if (std::isnan(Pr.log()) and log_verbose > 0)
        {
            std::cerr<<"calc_root_deg2_probability_SEV: probability is NaN!";
            return log_double_t(0.0);
        }
        Pr.log() += log_scale_min * scale;
        return Pr;
    }


    object_ptr<const Likelihood_Cache_Branch>
    simple_sequence_likelihoods_SEV(const EPair& sequence_mask,
				    const alphabet& a,
				    const EVector& smap,
				    int n_models)
    {
	auto& sequence = sequence_mask.first.as_<EVector>();
	auto& mask = sequence_mask.second.as_<Box<boost::dynamic_bitset<>>>();

	int n_states = smap.size();
	int matrix_size = n_models * n_states;

	int L = mask.size();

	auto LCB = object_ptr<Likelihood_Cache_Branch>(new Likelihood_Cache_Branch(mask, n_models, n_states));

	int i=0;
        for(int c=0;c<L;c++)
	{
	    if (not mask.test(c)) continue;

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

	    i++;
	}

	return LCB;
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

    log_double_t calc_root_prob_SEV(const EVector& LCN,
				    const EVector& LCB,
				    const Matrix& F,
				    const EVector& counts)
    {
	total_calc_root_prob++;

        const int n_models = F.size1();
        const int n_states = F.size2();
        const int matrix_size = n_models * n_states;

	// If LCN or LCB is empty, maybe we could use it directly and avoid copying.
	// But then we'd be using a pointer, which is indirect.
	EVector LC;
	LC.reserve(LCN.size() + LCB.size());
	for(auto& lc: LCB)
	    LC.push_back(lc);
	for(auto& lc: LCN)
	    LC.push_back(lc);

	auto cache = [&](int i) -> auto& { return LC[i].as_<Likelihood_Cache_Branch>(); };

	int n_clvs = LC.size();

	assert(not LC.empty());

        int L = cache(0).bits.size();

#ifndef NDEBUG
	assert(L > 0);

        for(int i=0;i<n_clvs;i++)
        {
            assert(cache(i).bits.size() == L);
	    assert(n_models == cache(i).n_models());
	    assert(n_states == cache(i).n_states());
        }
#endif

        // scratch matrix
        Matrix SMAT(n_models,n_states);
        double* S = SMAT.begin();
        total_root_clv_length += L;

	boost::dynamic_bitset<> bits_out;
	bits_out.resize(L);
        for(int i=0;i<n_clvs;i++)
            bits_out |= cache(i).bits;

        // index into LCs
        vector<int> s(n_clvs, 0);

        int scale = 0;

        log_prod total;
        for(int c=0;c<L;c++)
        {
            if (not bits_out.test(c)) continue;

	    constexpr int mi_max = 3;
	    const double* m[mi_max];
	    int mi=0;

            // Handle branches in
	    int j=0;
            for(;j<n_clvs and mi < mi_max;j++)
            {
		auto& lcb = cache(j);
                if (lcb.bits.test(c))
                {
		    m[mi++] = lcb[s[j]];
                    scale += lcb.scale(s[j]);
                    s[j]++;
                }
            }

	    double p_col = 1;
	    if (j == n_clvs)
	    {
		if (mi==3)
		    p_col = element_prod_sum(F.begin(), m[0], m[1], m[2], matrix_size);
		else if (mi==2)
		    p_col = element_prod_sum(F.begin(), m[0], m[1], matrix_size);
		else if (mi==1)
		    p_col = element_prod_sum(F.begin(), m[0], matrix_size);
		else
		    p_col = 1;
	    }
	    else
	    {
		if (mi==3)
		    element_prod_assign(S, F.begin(), m[0], m[1], m[2], matrix_size);
		else if (mi==2)
		    element_prod_assign(S, F.begin(), m[0], m[1], matrix_size);
		else if (mi==1)
		    element_prod_assign(S, F.begin(), m[0], matrix_size);
		else
		    element_assign(S, F.begin(), matrix_size);

		for(;j<n_clvs;j++)
		{
		    auto& lcb = cache(j);
		    if (lcb.bits.test(c))
		    {
			element_prod_assign(S, lcb[s[j]], matrix_size);
			scale += lcb.scale(s[j]);
			s[j]++;
		    }
		}

		p_col = element_sum(S, matrix_size);
	    }

            // SOME model must be possible
            assert(std::isnan(p_col) or (0 <= p_col and p_col <= 1.00000000001));

            total.mult_with_count(p_col, counts[c].as_int());
            //      std::clog<<" i = "<<i<<"   p = "<<p_col<<"  total = "<<total<<"\n";
        }

        log_double_t Pr = total;

        Pr.log() += log_scale_min * scale;

        if (std::isnan(Pr.log()) and log_verbose > 0)
        {
            std::cerr<<"calc_root_probability_SEV: probability is NaN!\n";
            return log_double_t(0.0);
        }

        return Pr;
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
                element_assign(R, matrix_size, 1);
        }

        return LCB;
    }

    vector<double> f81_exp_a_t(const data_partition& P, int /*b0*/, double L)
    {
        const int n_models  = P.n_base_models();
        //    const vector<unsigned>& smap = P.state_letters();

        vector<object_ptr<const F81_Object> > SubModels(n_models);
        for(int m=0;m<n_models;m++) {
//          SubModels[m] = P.base_model(m,b0).as_ptr_to<F81_Object>();
//          assert(SubModels[m]);
        }
        //    const double L = t.branch_length(b0);

        vector<double> exp_a_t(n_models);
        for(int m=0;m<n_models;m++) 
            exp_a_t[m] = exp(-L * SubModels[m]->alpha_);

        return exp_a_t;
    }
  
    object_ptr<const Likelihood_Cache_Branch>
    peel_leaf_branch_F81(const EVector& sequence, const alphabet& a, const vector<double>& exp_a_t, const Matrix& FF)
    {
        total_peel_leaf_branches++;

        // Do this before accessing matrices or other_subst
        int L0 = sequence.size();

        const int n_models  = exp_a_t.size();
        const int n_states  = a.n_letters();
        const int matrix_size = n_models * n_states;

        auto LCB = object_ptr<Likelihood_Cache_Branch>(new Likelihood_Cache_Branch(L0, n_models, n_states));

        //    const vector<unsigned>& smap = P.state_letters();

        // This could be wrong, if the code below assumes row or column major incorrectly
        const double* F = FF.begin();

        for(int i=0;i<L0;i++)
        {
            double* R = (*LCB)[i];

            // compute the distribution at the parent node
            int l2 = sequence[i].as_int();

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
                element_assign(R, matrix_size, 1);
        }

        return LCB;
    }

    inline void propagate(double* R, int n_models, int n_states, int& scale, const EVector& transition_P, const double* S)
    {
	int matrix_size = n_models * n_states;
	bool need_scale = true;
	for(int m=0;m<n_models;m++)
	{
	    const Matrix& Q = transition_P[m].as_<Box<Matrix>>();

	    // compute the distribution at the target (parent) node - multiple letters
	    for(int s1=0;s1<n_states;s1++)
	    {
		double temp=0;
		for(int s2=0;s2<n_states;s2++)
		    temp += Q(s1,s2)*S[m*n_states + s2];
		R[m*n_states + s1] = temp;
		need_scale = need_scale and (temp < scale_min);
	    }
	}
	if (need_scale)
	{
	    scale++;
	    for(int j=0; j<matrix_size; j++)
		R[j] *= scale_factor;
	}
    }

    object_ptr<const Likelihood_Cache_Branch>
    peel_leaf_branch_SEV(const Likelihood_Cache_Branch& nodeCLV, const EVector& transition_P)
    {
        int L0 = nodeCLV.n_columns();

        const int n_models  = transition_P.size();
        const int n_states  = transition_P[0].as_<Box<Matrix>>().size1();

	assert(nodeCLV.n_models() == n_models);
	assert(nodeCLV.n_states() == n_states);

        total_peel_leaf_branches++;

        auto LCB = object_ptr<Likelihood_Cache_Branch>(new Likelihood_Cache_Branch(nodeCLV.bits, n_models, n_states));

        for(int i=0;i<L0;i++)
        {
	    const double* S = nodeCLV[i];

	    int scale = 0;
            double* R = (*LCB)[i];
	    propagate(R, n_models, n_states, scale, transition_P, S);
	    LCB->scale(i) = scale;
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
		const EVector& A,
		const EVector& transition_P,
		const Matrix& F)
    {
        total_peel_internal_branches++;

        const int n_models = transition_P.size();
        const int n_states = transition_P[0].as_<Box<Matrix>>().size1();
        const int matrix_size = n_models * n_states;

        // Do this before accessing matrices or other_subst
	int n_sequences = LCN.size();
	int n_branches_in = LCB.size();
	assert(not LCN.empty() or not A.empty());
	int L = (LCN.empty()) ? A[0].as_<Box<pairwise_alignment_t>>().length2() : LCN[0].as_<Likelihood_Cache_Branch>().n_columns();

        auto LCB_OUT = object_ptr<Likelihood_Cache_Branch>(new Likelihood_Cache_Branch(L, n_models, n_states));

#ifndef NDEBUG
	// Check that all the sequences have the right length.
	for(auto& lcn: LCN)
	    assert(lcn.as_<Likelihood_Cache_Branch>().n_columns() == L);

	// Check that all the alignments have the right length for both sequences.
	assert(A.size() == n_branches_in);
	for(int i=0; i<n_branches_in; i++)
	{
	    assert(A[i].as_<Box<pairwise_alignment_t>>().length2() == L);
	    assert(A[i].as_<Box<pairwise_alignment_t>>().length1() == LCB[i].as_<Likelihood_Cache_Branch>().n_columns());
	}
#endif

        // scratch matrix
        double* S = LCB_OUT->scratch(0);

        log_prod total;
        int total_scale = 0;
	vector<int> AL(n_branches_in);
	for(int j=0; j < n_branches_in;j++)
	    AL[j] = A[j].as_<Box<pairwise_alignment_t>>().size();

	vector<int> s(n_branches_in, 0);
	int s_out = 0;
	vector<int> i(n_branches_in, 0);
        for(;;)
        {
	    for(int j =0;j < n_branches_in; j++)
	    {
		auto& a = A[j].as_<Box<pairwise_alignment_t>>();
		auto& lcb = LCB[j].as_<Likelihood_Cache_Branch>();
		auto& ij = i[j];
		auto& sj = s[j];
		while (ij < AL[j] and not a.has_character2(ij))
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
		    assert(i[j] == AL[j]);
                break;
            }
	    else
	    {
		for(int j=0;j<n_branches_in;j++)
		{
		    assert(i[j] < AL[j]);
		    assert(A[j].as_<Box<pairwise_alignment_t>>().has_character2(i[j]));
		}
	    }

	    int scale = 0;
	    for(int k=0; k<matrix_size; k++)
		S[k] = 1.0;
	    for(int j=0;j<n_branches_in;j++)
	    {
		if (A[j].as_<Box<pairwise_alignment_t>>().has_character1(i[j]))
		{
		    auto& lcb = LCB[j].as_<Likelihood_Cache_Branch>();
		    element_prod_assign(S, lcb[s[j]], matrix_size);
		    scale += lcb.scale(s[j]);
		    s[j]++;
		}
		i[j]++;
	    }

	    // Handle observed sequences at the node.
	    for(int j=0;j<n_sequences;j++)
	    {
		auto& lcn = LCN[j].as_<Likelihood_Cache_Branch>();
		element_prod_assign(S, lcn[s_out], matrix_size);
	    }

	    // propagate from the source distribution
	    double* R = (*LCB_OUT)[s_out];            //name the result matrix
	    propagate(R, n_models, n_states, scale, transition_P, S);
            LCB_OUT->scale(s_out++) = scale;
        }

	LCB_OUT->other_subst = total;
        LCB_OUT->other_subst.log() += total_scale*log_scale_min;
	for(int j=0;j<n_branches_in;j++)
	    LCB_OUT->other_subst *= LCB[j].as_<Likelihood_Cache_Branch>().other_subst;
        return LCB_OUT;
    }


    log_double_t calc_root_prob(const EVector& sequences,
				const alphabet& a,
				const EVector& smap,
				const EVector& LCB,
				const EVector& A_,
				const Matrix& F)
    {
        total_calc_root_prob++;

        const int n_models = F.size1();
        const int n_states = F.size2();
        const int matrix_size = n_models * n_states;

        auto cache = [&](int i) -> auto& { return LCB[i].as_<Likelihood_Cache_Branch>(); };
        auto A = [&](int i) -> auto& { return A_[i].as_<Box<pairwise_alignment_t>>();};
        auto sequence = [&](int i) -> auto& { return sequences[i].as_<EVector>();};

        // Do this before accessing matrices or other_subst
	int n_branches_in = LCB.size();
	assert(not sequences.empty() or not A_.empty());
	int L = (sequences.empty()) ? A(0).length2() : sequence(0).size();

#ifndef NDEBUG
	// Check that all the sequences have the right length.
	for(auto& esequence: sequences)
	    assert(esequence.as_<EVector>().size() == L);

	// Check that all the alignments have the right length for both sequences.
	assert(A_.size() == n_branches_in);
	for(int i=0; i<n_branches_in; i++)
	{
	    assert(A(i).length2() == L);
	    assert(A(i).length1() == LCB[i].as_<Likelihood_Cache_Branch>().n_columns());

	    assert(n_models == LCB[i].as_<Likelihood_Cache_Branch>().n_models());
	    assert(n_states == LCB[i].as_<Likelihood_Cache_Branch>().n_states());
	}
#endif

        // scratch matrix
        Matrix SMAT(n_models,n_states);
        double* S = SMAT.begin();

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
	    for(auto& esequence: sequences)
	    {
		auto& sequence = esequence.as_<EVector>();
		int letter = sequence[s_out].as_int();

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


	    double p_col = element_sum(S, matrix_size);

	    total *= p_col;

	    total_root_clv_length++;

	    s_out++;
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

    object_ptr<const Likelihood_Cache_Branch>
    peel_internal_branch_SEV(const Likelihood_Cache_Branch& LCB1,
                             const Likelihood_Cache_Branch& LCB2,
                             const EVector& transition_P)
    {
        total_peel_internal_branches++;

        const int n_models = transition_P.size();
        const int n_states = transition_P[0].as_<Box<Matrix>>().size1();
        const int matrix_size = n_models * n_states;
    
        const auto& bits1 = LCB1.bits;
        const auto& bits2 = LCB2.bits;

        int L = bits1.size();
        assert(L > 0);
        assert(bits2.size() == L);

        // Do this before accessing matrices or other_subst
        auto LCB3 = object_ptr<Likelihood_Cache_Branch>(new Likelihood_Cache_Branch(LCB1.bits | LCB2.bits, n_models, n_states));
        const auto& bits3 = LCB3->bits;
        assert(bits3.size() == L);

        // scratch matrix
        double* S = LCB3->scratch(0);

        for(int c=0,i1=0,i2=0,i3=0;c<L;c++)
        {
            if (not bits3.test(c)) continue;

            bool nongap1 = bits1.test(c);
            bool nongap2 = bits2.test(c);

            int scale = 0;
            const double* C = S;
            if (nongap1 and nongap2)
            {
                element_prod_assign(S, LCB1[i1], LCB2[i2], matrix_size);
                scale = LCB1.scale(i1) + LCB2.scale(i2);
            }
            else if (nongap1)
            {
                C = LCB1[i1];
                scale = LCB1.scale(i1);
            }
            else if (nongap2)
            {
                C = LCB2[i2];
                scale = LCB2.scale(i2);
            }
            else
            {
                // columns like this should not be in the index
                std::abort();
            }

            // propagate from the source distribution
            double* R = (*LCB3)[i3];            //name the result matrix
	    propagate(R, n_models, n_states, scale, transition_P, C);
            LCB3->scale(i3) = scale;

            if (nongap1) i1++;
            if (nongap2) i2++;
            i3++;
        }

        return LCB3;
    }

    // Generalize to degree n>=1?
    object_ptr<const Likelihood_Cache_Branch>
    peel_deg2_branch_SEV(const Likelihood_Cache_Branch& LCB1,
                         const EVector& transition_P)
    {
        total_peel_internal_branches++;

        const int n_models = transition_P.size();
        const int n_states = transition_P[0].as_<Box<Matrix>>().size1();

        const auto& bits1 = LCB1.bits;

        int L = bits1.size();
        assert(L > 0);

        // Do this before accessing matrices or other_subst
        auto LCB2 = object_ptr<Likelihood_Cache_Branch>(new Likelihood_Cache_Branch(LCB1.bits, n_models, n_states));

        for(int c=0,i=0;c<L;c++)
        {
            if (not bits1.test(c)) continue;

            int scale = 0;

            const double* C = LCB1[i];
            scale = LCB1.scale(i);

            // propagate from the source distribution
            double* R = (*LCB2)[i];            //name the result matrix
	    propagate(R, n_models, n_states, scale, transition_P, C);
            LCB2->scale(i) = scale;

            i++;
        }

        return LCB2;
    }

    object_ptr<const Likelihood_Cache_Branch>
    peel_branch_SEV(const EVector& LCN,
                    const EVector& LCB,
                    const EVector& transition_P)
    {
        total_peel_internal_branches++;

	  if (LCN.empty() and LCB.size() == 2)
	    return peel_internal_branch_SEV(LCB[0].as_<Likelihood_Cache_Branch>(),
					    LCB[1].as_<Likelihood_Cache_Branch>(),
					    transition_P);
	else if (LCN.empty() and LCB.size() == 1)
	    return peel_deg2_branch_SEV(LCB[0].as_<Likelihood_Cache_Branch>(),
					transition_P);
	else if (LCN.size() == 1 and LCB.empty() and false)
	    return peel_leaf_branch_SEV(LCN[0].as_<Likelihood_Cache_Branch>(),
					transition_P);

        const int n_models = transition_P.size();
        const int n_states = transition_P[0].as_<Box<Matrix>>().size1();
        const int matrix_size = n_models * n_states;

	EVector LC;
	LC.reserve(LCB.size() + LCN.size());
	for(auto& lc: LCB)
	    LC.push_back(lc);
	for(auto& lc: LCN)
	    LC.push_back(lc);

	auto cache = [&](int i) -> auto& { return LC[i].as_<Likelihood_Cache_Branch>(); };

        int n_clvs = LC.size();
        assert(not LC.empty());
        int L = cache(0).bits.size();

#ifndef NDEBUG
        assert(L > 0);

        for(int i=0;i<n_clvs;i++)
        {
            assert(cache(i).bits.size() == L);
	    assert(n_models == cache(i).n_models());
	    assert(n_states == cache(i).n_states());
        }
#endif

        // Do this before accessing matrices or other_subst
	boost::dynamic_bitset<> bits;
	bits.resize(L);
        for(int i=0;i<n_clvs;i++)
            bits |= cache(i).bits;
        auto LCB_OUT = object_ptr<Likelihood_Cache_Branch>(new Likelihood_Cache_Branch( std::move(bits), n_models, n_states));
        const auto& bits_out = LCB_OUT->bits;
        assert(bits_out.size() == L);

        // scratch matrix
        double* S = LCB_OUT->scratch(0);

        // index into LCBs
        vector<int> s(n_clvs, 0);
        // index into LCB_OUT
        int s_out = 0;

        for(int c=0;c<L;c++)
        {
            if (not bits_out.test(c)) continue;

            int scale = 0;
            const double* C = S;
            for(int k=0; k<matrix_size; k++)
                S[k] = 1.0;

            // Handle branches in
            for(int j=0;j<n_clvs;j++)
            {
                if (cache(j).bits.test(c))
                {
                    auto& lcb = cache(j);
                    element_prod_assign(S, lcb[s[j]], matrix_size);
                    scale += lcb.scale(s[j]);
                    s[j]++;
                }
            }


            // propagate from the source distribution
            double* R = (*LCB_OUT)[s_out];            //name the result matrix
	    propagate(R, n_models, n_states, scale, transition_P, C);
            LCB_OUT->scale(s_out) = scale;

            s_out++;
        }

        return LCB_OUT;
    }

    object_ptr<const Box<matrix<int>>> alignment_index2(const pairwise_alignment_t& A0, const pairwise_alignment_t& A1)
    {
        auto a0 = convert_to_bits(A0, 0, 2);
        auto a1 = convert_to_bits(A1, 1, 2);
        auto a012 = Glue_A(a0, a1);

        // get the relationships with the sub-alignments for the (two) branches behind b0
        auto index = object_ptr<Box<matrix<int>>>(new Box<matrix<int>>);
        *index = get_indices_from_bitpath(a012, {0,1,2});
        return index;
    }

    object_ptr<const Likelihood_Cache_Branch>
    peel_internal_branch_F81(const Likelihood_Cache_Branch& LCB1,
                             const Likelihood_Cache_Branch& LCB2,
                             const pairwise_alignment_t& A0,
                             const pairwise_alignment_t& A1,
                             const vector<double>& exp_a_t,
                             const Matrix& FF,
                             const Matrix& /*WF*/)
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

        auto LCB3 = object_ptr<Likelihood_Cache_Branch>(new Likelihood_Cache_Branch(index.size1(), n_models, n_states));

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
                element_prod_assign(S, LCB1[i0], LCB2[i1], matrix_size);
            else if (i0 != alphabet::gap)
                C = LCB1[i0];
            else if (i1 != alphabet::gap)
                C = LCB2[i1];
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
//      LCB3->other_subst = collect_vanishing_internal(LCB1, LCB2, index_collect, WF);
        return LCB3;
    }

    Likelihood_Cache_Branch
    get_leaf_seq_likelihoods(const EVector& sequence, const alphabet& a, const data_partition& P, int delta);


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


    /// Get the likelihood matrix for each letter l of sequence 'sequence', where the likelihood matrix R(m,s) = Pr(observe letter l | model = m, state = 2)
    Likelihood_Cache_Branch
    get_leaf_seq_likelihoods(const EVector& sequence, const alphabet& a, const data_partition& P, int delta)
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
        Likelihood_Cache_Branch LCB(L+delta, n_models, n_states);

        for(int i=0;i<delta;i++)
            LCB.set(i,0);

        for(int i=0;i<L;i++)
        {
            int letter = sequence[i].as_int();
            if (a.is_letter(letter))
                LCB.set(i+delta, letter_likelihoods[letter]);
            else
                LCB.set(i+delta, get_letter_likelihoods(letter, a, P));
        }

        return LCB;
    }

    Likelihood_Cache_Branch
    get_leaf_seq_likelihoods(const data_partition& P, int n, int delta)
    {
        const auto sequence = P.get_sequence(n);
        auto a = P.get_alphabet();
        return get_leaf_seq_likelihoods(*sequence, *a, P, delta);
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

    void calc_transition_prob_from_parent(Matrix& S, const pair<int,int>& state_model_parent, const EVector& Ps)
    {
        auto [mp,lp] = state_model_parent;

        int n_states = S.size2();

        // If there IS no parent character, then we can sample from F
        assert(mp != -1);

        auto& Pr = Ps[mp].as_<Box<Matrix>>();
        assert(mp >= 0);
        element_assign(S,0);
        for(int l=0;l<n_states;l++)
            S(mp,l) = Pr(lp,l);
    }

    void calc_transition_prob_from_parent(Matrix& S, const pair<int,int>& state_model_parent, const EVector& Ps, const Matrix& WF)
    {
        auto [mp,lp] = state_model_parent;

        // If there IS no parent character, then we can sample from F
        if (mp == -1)
            S = WF;
        else
            calc_transition_prob_from_parent(S, state_model_parent, Ps);
    }

    Vector<pair<int,int>> sample_root_sequence(const EVector& sequences,
					       const alphabet& a,
					       const EVector& smap,
					       const EVector& LCB,
					       const EVector& A_,
                                               const Matrix& F)
    {

        const int n_models = F.size1();
        const int n_states = F.size2();
        const int matrix_size = n_models * n_states;

        auto cache = [&](int i) -> auto& { return LCB[i].as_<Likelihood_Cache_Branch>(); };
        auto A = [&](int i) -> auto& { return A_[i].as_<Box<pairwise_alignment_t>>();};
        auto sequence = [&](int i) -> auto& { return sequences[i].as_<EVector>();};

        // Do this before accessing matrices or other_subst
	int n_branches_in = LCB.size();
	assert(not sequences.empty() or not A_.empty());
	int L = (sequences.empty()) ? A(0).length2() : sequence(0).size();

#ifndef NDEBUG
	// Check that all the sequences have the right length.
	for(auto& esequence: sequences)
	    assert(esequence.as_<EVector>().size() == L);

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
	    for(auto& esequence: sequences)
	    {
		auto& sequence = esequence.as_<EVector>();
		int letter = sequence[s_out].as_int();

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
				(S.begin())[m*n_states + s1] = 0;
			    }
			}
		    }
		}
	    }

            ancestral_characters[s_out] = sample(S);
        }

        return ancestral_characters;
    }

    vector<optional<int>> get_index_for_column(const boost::dynamic_bitset<>& bits)
    {
        vector<optional<int>> index_for_column(bits.size());
        int index = 0;
        for(int c=0;c<bits.size();c++)
        {
            if (bits.test(c))
            {
                index_for_column[c] = index;
                index++;
            }
        }
        return index_for_column;
    }

    // Currently we are not treating N as a gap during compression.
    // So, we can assume that anything that's not in the mask will not have an ancestral letter.
    Vector<pair<int,int>> sample_root_sequence_SEV(const EVector& LCN,
						   const EVector& LCB,
                                                   const Matrix& F,
                                                   const EVector& compressed_col_for_col)
    {
        // 1. Construct a scratch CL matrix, and check that dimensions match inputs
        const int n_models = F.size1();
        const int n_states = F.size2();
        const int matrix_size = n_models * n_states;

	// If LCN or LCB is empty, maybe we could use it directly and avoid copying.
	// But then we'd be using a pointer, which is indirect.
	EVector LC;
	LC.reserve(LCN.size() + LCB.size());
	for(auto& lc: LCB)
	    LC.push_back(lc);
	for(auto& lc: LCN)
	    LC.push_back(lc);

	auto cache = [&](int i) -> auto& { return LC[i].as_<Likelihood_Cache_Branch>(); };

	int n_clvs = LC.size();

	assert(not LC.empty());

        int L_full = compressed_col_for_col.size();
        int L_compressed = cache(0).bits.size();

#ifndef NDEBUG
	assert(L_full >= L_compressed);

        for(int i=0;i<n_clvs;i++)
        {
            assert(cache(i).bits.size() == L_compressed);
	    assert(n_models == cache(i).n_models());
	    assert(n_states == cache(i).n_states());
        }
#endif

	// scratch matrix
        Matrix S(n_models,n_states);

	boost::dynamic_bitset<> allbits;
	allbits.resize(L_compressed);
        for(int i=0;i<n_clvs;i++)
            allbits |= cache(i).bits;

	vector<vector<optional<int>>> cache_index_for_compressed_column(n_clvs);
	for(int i=0;i<n_clvs;i++)
	    cache_index_for_compressed_column[i] = get_index_for_column( cache(i).bits );

	// Initially all the ancestral letters are missing.
        Vector<pair<int,int>> ancestral_characters(L_full,{-1,-1});

        // 3. Walk the alignment and sample (model,letter) for ancestral sequence
        for(int c = 0; c < L_full; c++)
        {
            int c2 = compressed_col_for_col[c].as_int();

            if (not allbits.test(c2)) continue;

            S = F;

	    for(int b=0;b<n_clvs;b++)
		if (auto i = cache_index_for_compressed_column[b][c2])
		    element_prod_modify(S.begin(), cache(b)[*i], matrix_size);

            ancestral_characters[c] = sample( S );
        }

        return ancestral_characters;
    }

    Vector<pair<int,int>> sample_branch_sequence(const Vector<pair<int,int>>& parent_seq,
						 const pairwise_alignment_t& parent_A,
						 const EVector& sequences,
						 const alphabet& a,
						 const EVector& smap,
						 const EVector& LCB,
						 const EVector& A,
						 const EVector& transition_P,
						 const Matrix& F)
    {
        total_peel_internal_branches++;

        const int n_models = transition_P.size();
        const int n_states = transition_P[0].as_<Box<Matrix>>().size1();
        const int matrix_size = n_models * n_states;

        // Do this before accessing matrices or other_subst
	int n_branches_in = LCB.size();
	assert(not sequences.empty() or not A.empty());
	int L = (sequences.empty())?A[0].as_<Box<pairwise_alignment_t>>().length2() : sequences[0].as_<EVector>().size();

#ifndef NDEBUG
	// Check that all the sequences have the right length.
	for(auto& esequence: sequences)
	    assert(esequence.as_<EVector>().size() == L);

	// Check that all the alignments have the right length for both sequences.
	assert(A.size() == n_branches_in);
	for(int i=0; i<n_branches_in; i++)
	{
	    assert(A[i].as_<Box<pairwise_alignment_t>>().length2() == L);
	    assert(A[i].as_<Box<pairwise_alignment_t>>().length1() == LCB[i].as_<Likelihood_Cache_Branch>().n_columns());
	}
#endif

        // scratch matrix
	Matrix SMAT(n_models, n_states);
        double* S = SMAT.begin();

	vector<int> AL(n_branches_in);
	for(int j=0; j < n_branches_in;j++)
	    AL[j] = A[j].as_<Box<pairwise_alignment_t>>().size();

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
		auto& a = A[j].as_<Box<pairwise_alignment_t>>();
		auto& ij = i[j];
		while (ij < AL[j] and not a.has_character2(ij))
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
		    assert(i[j] == AL[j]);
		    assert(s[j] == A[j].as_<Box<pairwise_alignment_t>>().length1());
		}
		assert(i_parent == parent_A.size());
		assert(s_parent == parent_A.length1());
                break;
            }
	    else
	    {
		for(int j=0;j<n_branches_in;j++)
		{
		    assert(i[j] < AL[j]);
		    assert(s[j] <= A[j].as_<Box<pairwise_alignment_t>>().length1());
		    assert(A[j].as_<Box<pairwise_alignment_t>>().has_character2(i[j]));
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
		if (A[j].as_<Box<pairwise_alignment_t>>().has_character1(i[j]))
		{
		    auto& lcb = LCB[j].as_<Likelihood_Cache_Branch>();
		    element_prod_assign(S, lcb[s[j]], matrix_size);
		    s[j]++;
		}
		i[j]++;
	    }

	    // Handle observed sequences at the node.
	    for(auto& esequence: sequences)
	    {
		auto& sequence = esequence.as_<EVector>();
		int letter = sequence[s_node].as_int();

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

            ancestral_characters[s_node] = sample(SMAT);

	    s_node++;
        }

        return ancestral_characters;
    }

    
    Vector<pair<int,int>> sample_sequence_SEV(const Vector<pair<int,int>>& parent_seq,
					      const EVector& LCN,
					      const EVector& transition_Ps,
					      const EVector& LCB,
					      const EVector& compressed_col_for_col)
    {
        // 1. Construct a scratch matrix and check that dimensions match inputs
        const int n_models  = transition_Ps.size();
        const int n_states  = transition_Ps[0].as_<Box<Matrix>>().size1();
        const int matrix_size = n_models * n_states;

	// If LCN or LCB is empty, maybe we could use it directly and avoid copying.
	// But then we'd be using a pointer, which is indirect.
	EVector LC;
	LC.reserve(LCN.size() + LCB.size());
	for(auto& lc: LCB)
	    LC.push_back(lc);
	for(auto& lc: LCN)
	    LC.push_back(lc);

	auto cache = [&](int i) -> auto& { return LC[i].as_<Likelihood_Cache_Branch>(); };

	int n_clvs = LC.size();

	assert(not LC.empty());

        int L_full = compressed_col_for_col.size();
        int L_compressed = cache(0).bits.size();

#ifndef NDEBUG
	assert(parent_seq.size() == L_full);
	assert(L_full >= L_compressed);

        for(int i=0;i<n_clvs;i++)
        {
            assert(cache(i).bits.size() == L_compressed);
	    assert(n_models == cache(i).n_models());
	    assert(n_states == cache(i).n_states());
        }
#endif

	Matrix S(n_models, n_states);

	boost::dynamic_bitset<> allbits;
	allbits.resize(L_compressed);
        for(int i=0;i<n_clvs;i++)
            allbits |= cache(i).bits;

	vector<vector<optional<int>>> cache_index_for_compressed_column(n_clvs);
	for(int i=0;i<n_clvs;i++)
	    cache_index_for_compressed_column[i] = get_index_for_column( cache(i).bits );

	// Initially all the ancestral letters are missing.
        Vector<pair<int,int>> ancestral_characters(L_full,{-1,-1});

        // 3. Walk the alignment and sample (model,letter) for ancestral sequence
        for(int c = 0; c < L_full; c++)
        {
            int c2 = compressed_col_for_col[c].as_int();

            if (not allbits.test(c2)) continue;

            calc_transition_prob_from_parent(S, parent_seq[c], transition_Ps);

	    for(int b=0;b<n_clvs;b++)
		if (auto i = cache_index_for_compressed_column[b][c2])
		    element_prod_modify(S.begin(), cache(b)[*i], matrix_size);

            ancestral_characters[c] = sample(S);
        }

        return ancestral_characters;
    }

}
