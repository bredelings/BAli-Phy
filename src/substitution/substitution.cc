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


    Box<matrix<int>>*
    alignment_index3(const pairwise_alignment_t& A1, const pairwise_alignment_t& A2, const pairwise_alignment_t& A3)
    {
        auto a10 = convert_to_bits(A1,1,0);
        auto a20 = convert_to_bits(A2,2,0);
        auto a30 = convert_to_bits(A3,3,0);
        auto a0123 = Glue_A(a10, Glue_A(a20,a30));

        auto index = new Box<matrix<int>>;
        *index = get_indices_from_bitpath(a0123, {1,2,3});
        return index;
    }


    log_double_t calc_root_probability(const Likelihood_Cache_Branch* LCB1,
                                       const Likelihood_Cache_Branch* LCB2,
                                       const Likelihood_Cache_Branch* LCB3,
                                       const pairwise_alignment_t& A0,
                                       const pairwise_alignment_t& A1,
                                       const pairwise_alignment_t& A2,
                                       const Matrix& F)
    {
        assert(LCB1->n_columns() == A0.length1());
        assert(LCB2->n_columns() == A1.length1());
        assert(LCB3->n_columns() == A2.length1());
        total_calc_root_prob++;

        const int n_models = F.size1();
        const int n_states = F.size2();
        const int matrix_size = n_models * n_states;

        assert(n_models == LCB1->n_models());
        assert(n_states == LCB1->n_states());

        assert(n_models == LCB2->n_models());
        assert(n_states == LCB2->n_states());

        assert(n_models == LCB3->n_models());
        assert(n_states == LCB3->n_states());

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
                double p_col = element_prod_sum(F.begin(), (*LCB1)[s0], matrix_size );
                assert(0 <= p_col and p_col <= 1.00000000001);
                total *= p_col;
                scale += LCB1->scale(s0);
                i0++;
                s0++;
                total_root_clv_length++;
            }
            while (i1 < AL1 and not A1.has_character2(i1))
            {
                assert(A1.has_character1(i1));
                double p_col = element_prod_sum(F.begin(), (*LCB2)[s1], matrix_size );
                assert(0 <= p_col and p_col <= 1.00000000001);
                total *= p_col;
                scale += LCB2->scale(s1);
                i1++;
                s1++;
                total_root_clv_length++;
            }
            while (i2 < AL2 and not A2.has_character2(i2))
            {
                assert(A2.has_character1(i2));
                double p_col = element_prod_sum(F.begin(), (*LCB3)[s2], matrix_size );
                assert(0 <= p_col and p_col <= 1.00000000001);
                total *= p_col;
                scale += LCB3->scale(s2);
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
                m[mi++] = ((*LCB1)[s0]);
                scale += (*LCB1).scale(s0);
                s0++;
            }
            if (not_gap1)
            {
                m[mi++] = ((*LCB2)[s1]);
                scale += (*LCB2).scale(s1);
                s1++;
            }
            if (not_gap2)
            {
                m[mi++] = ((*LCB3)[s2]);
                scale += (*LCB3).scale(s2);
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
                element_prod_modify(S.begin(),(*LCB1)[s0], matrix_size);
            if (not_gap1)
                element_prod_modify(S.begin(),(*LCB2)[s1], matrix_size);
            if (not_gap2)
                element_prod_modify(S.begin(),(*LCB3)[s2], matrix_size);

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

            // This might do a log( ) operation.
            total *= p_col;
            //      std::clog<<" i = "<<i<<"   p = "<<p_col<<"  total = "<<total<<"\n";

            s3++;
            total_root_clv_length++;
        }

        log_double_t Pr = total;
        Pr *= LCB1->other_subst;
        Pr *= LCB2->other_subst;
        Pr *= LCB3->other_subst;
        Pr.log() += log_scale_min * scale;
        return Pr;
    }

    log_double_t calc_root_probability_SEV(const Likelihood_Cache_Branch* LCB1,
                                           const Likelihood_Cache_Branch* LCB2,
                                           const Likelihood_Cache_Branch* LCB3,
                                           const Matrix& F,
                                           const EVector& counts)
    {
        total_calc_root_prob++;

        const int n_models = F.size1();
        const int n_states = F.size2();
        const int matrix_size = n_models * n_states;

        assert(n_models == LCB1->n_models());
        assert(n_states == LCB1->n_states());

        assert(n_models == LCB2->n_models());
        assert(n_states == LCB2->n_states());

        assert(n_models == LCB3->n_models());
        assert(n_states == LCB3->n_states());

#ifdef DEBUG_SUBSTITUTION
        // scratch matrix
        Matrix S(n_models,n_states);
#endif

        const auto& bits1 = LCB1->bits;
        const auto& bits2 = LCB2->bits;
        const auto& bits3 = LCB3->bits;

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
                m[mi++] = ((*LCB1)[i1]);
                scale += (*LCB1).scale(i1);
            }
            if (non_gap2)
            {
                m[mi++] = ((*LCB2)[i2]);
                scale += (*LCB2).scale(i2);
            }
            if (non_gap3)
            {
                m[mi++] = ((*LCB3)[i3]);
                scale += (*LCB3).scale(i3);
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
                element_prod_modify(S.begin(),(*LCB1)[i1], matrix_size);
            if (non_gap2)
                element_prod_modify(S.begin(),(*LCB2)[i2], matrix_size);
            if (non_gap3)
                element_prod_modify(S.begin(),(*LCB3)[i3], matrix_size);

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

            if (non_gap1) i1++;
            if (non_gap2) i2++;
            if (non_gap3) i3++;

            total.mult_with_count(p_col,counts[c].as_int());
            //      std::clog<<" i = "<<i<<"   p = "<<p_col<<"  total = "<<total<<"\n";
        }

        log_double_t Pr = total;
        if (std::isnan(Pr.log()))
        {
            std::cerr<<"calc_root_deg2_probability_SEV: probability is NaN!";
            return log_double_t(0.0);
        }
        Pr.log() += log_scale_min * scale;
        return Pr;
    }


    log_double_t calc_root_deg2_probability_SEV(const Likelihood_Cache_Branch* LCB1,
                                                const Likelihood_Cache_Branch* LCB2,
                                                const Matrix& F,
                                                const EVector& counts)
    {
        total_calc_root_prob++;

        const int n_models = F.size1();
        const int n_states = F.size2();
        const int matrix_size = n_models * n_states;

        assert(n_models == LCB1->n_models());
        assert(n_states == LCB1->n_states());

        assert(n_models == LCB2->n_models());
        assert(n_states == LCB2->n_states());

#ifdef DEBUG_SUBSTITUTION
        // scratch matrix
        Matrix S(n_models,n_states);
#endif

        const auto& bits1 = LCB1->bits;
        const auto& bits2 = LCB2->bits;

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
                m[mi++] = ((*LCB1)[i1]);
                scale += (*LCB1).scale(i1);
            }
            if (non_gap2)
            {
                m[mi++] = ((*LCB2)[i2]);
                scale += (*LCB2).scale(i2);
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
                element_prod_modify(S.begin(),(*LCB1)[i1], matrix_size);
            if (non_gap2)
                element_prod_modify(S.begin(),(*LCB2)[i2], matrix_size);

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

            if (non_gap1) i1++;
            if (non_gap2) i2++;

            total.mult_with_count(p_col,counts[c].as_int());
            //      std::clog<<" i = "<<i<<"   p = "<<p_col<<"  total = "<<total<<"\n";
        }

        log_double_t Pr = total;
        if (std::isnan(Pr.log()))
        {
            std::cerr<<"calc_root_deg2_probability_SEV: probability is NaN!";
            return log_double_t(0.0);
        }
        Pr.log() += log_scale_min * scale;
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


    Likelihood_Cache_Branch*
    peel_leaf_branch_simple(const EVector& sequence, const alphabet& a, const EVector& transition_P)
    {
        total_peel_leaf_branches++;

        int L0 = sequence.size();

        const int n_models  = transition_P.size();
        const int n_states  = transition_P[0].as_<Box<Matrix>>().size1();
        const int matrix_size = n_models * n_states;
        const int n_letters = a.n_letters();

        auto LCB = new Likelihood_Cache_Branch(L0, n_models, n_states);
    
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

        LCB->other_subst = 1;

        return LCB;
    }

    // the SEV version differs from the non-SEV version in two respectes:
    // * it doesn't need to propagate counts.
    // * it needs to set LCB->bits
    // they are otherwise identical.
    // how could we share the code?

    Likelihood_Cache_Branch*
    peel_leaf_branch_simple_SEV(const EVector& sequence, const alphabet& a, const EVector& transition_P, const boost::dynamic_bitset<>& mask)
    {
        total_peel_leaf_branches++;

        int L0 = sequence.size();

        const int n_models  = transition_P.size();
        const int n_states  = transition_P[0].as_<Box<Matrix>>().size1();
        const int matrix_size = n_models * n_states;
        const int n_letters = a.n_letters();

        auto LCB = new Likelihood_Cache_Branch(L0, n_models, n_states);
        LCB->bits = mask;
    
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

        LCB->other_subst = 1;

        return LCB;
    }

    vector<double> f81_exp_a_t(const data_partition& P, int /*b0*/, double L)
    {
        const int n_models  = P.n_base_models();
        //    const vector<unsigned>& smap = P.state_letters();

        vector<object_ptr<const F81_Object> > SubModels(n_models);
        for(int m=0;m<n_models;m++) {
//          SubModels[m] = P.base_model(m,b0).assert_is_a<F81_Object>();
//          assert(SubModels[m]);
        }
        //    const double L = t.branch_length(b0);

        vector<double> exp_a_t(n_models);
        for(int m=0;m<n_models;m++) 
            exp_a_t[m] = exp(-L * SubModels[m]->alpha_);

        return exp_a_t;
    }
  
    Likelihood_Cache_Branch*
    peel_leaf_branch_F81(const EVector& sequence, const alphabet& a, const vector<double>& exp_a_t, const Matrix& FF)
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

        LCB->other_subst = 1;

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
    Likelihood_Cache_Branch*
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

        auto LCB = new Likelihood_Cache_Branch(L0, n_models, n_states);

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

        LCB->other_subst = 1;

        return LCB;
    }

    Likelihood_Cache_Branch*
    peel_leaf_branch_SEV(const EVector& sequence, const alphabet& a, const EVector& transition_P, const boost::dynamic_bitset<>& mask, const EVector& smap)
    {
        // Do this before accessing matrices or other_subst
        int L0 = sequence.size();

        const int n_models  = transition_P.size();
        const int n_states  = transition_P[0].as_<Box<Matrix>>().size1();
        const int matrix_size = n_models * n_states;
        const int n_letters = a.n_letters();

        if (n_states == n_letters and is_iota(smap))
            return peel_leaf_branch_simple_SEV(sequence, a, transition_P, mask);

        total_peel_leaf_branches++;

        auto LCB = new Likelihood_Cache_Branch(L0, n_models, n_states);
        LCB->bits = mask;

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

        LCB->other_subst = 1;

        return LCB;
    }

    int sum_row(const matrix<int>& index, int row)
    {
        int total = 0;
        for(int i=0;i<index.size1();i++)
            if (index(i,row) >= 0)
                total++;
        return total;
    }
        

    Likelihood_Cache_Branch*
    peel_internal_branch(const Likelihood_Cache_Branch* LCB1,
                         const Likelihood_Cache_Branch* LCB2,
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
        auto* LCB3 = new Likelihood_Cache_Branch(A0.length2(), n_models, n_states);
        assert(A0.length2() == A1.length2());
        assert(A0.length1() == LCB1->n_columns());
        assert(A1.length1() == LCB2->n_columns());

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
                double p_col = element_prod_sum(F.begin(), (*LCB1)[s0], matrix_size );
                assert(0 <= p_col and p_col <= 1.00000000001);
                total *= p_col;
                total_scale += LCB1->scale(s0);
                i0++;
                s0++;
            }
            while (i1 < AL1 and not A1.has_character2(i1))
            {
                assert(A1.has_character1(i1));
                double p_col = element_prod_sum(F.begin(), (*LCB2)[s1], matrix_size );
                assert(0 <= p_col and p_col <= 1.00000000001);
                total *= p_col;
                total_scale += LCB2->scale(s1);
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
                element_prod_assign(S, (*LCB1)[s0], (*LCB2)[s1], matrix_size);
                scale = LCB1->scale(s0) + LCB2->scale(s1);
                s0++;
                s1++;
            }
            else if (not_gap0)
            {
                C = (*LCB1)[s0];
                scale = LCB1->scale(s0);
                s0++;
            }
            else if (not_gap1)
            {
                C = (*LCB2)[s1];
                scale = LCB2->scale(s1);
                s1++;
            }
            else
                C = ones.begin();  // Columns like this would not be in subA_index_leaf, but might be in subA_index_internal

            // propagate from the source distribution
            double* R = (*LCB3)[s2];            //name the result matrix
            bool need_scale = true;
            for(int m=0;m<n_models;m++)
            {
                const Matrix& Q = transition_P[m].as_<Box<Matrix>>();
        
                // compute the distribution at the target (parent) node - multiple letters
                for(int s1=0;s1<n_states;s1++) {
                    double temp=0;
                    for(int s2=0;s2<n_states;s2++)
                        temp += Q(s1,s2)*C[m*n_states + s2];
                    R[m*n_states + s1] = temp;
                    need_scale = need_scale and (temp < scale_min);
                }
            }
            if (need_scale) // and false)
            {
                scale++;
                for(int j=0; j<matrix_size; j++)
                    R[j] *= scale_factor;
            }
            LCB3->scale(s2) = scale;
            s2++;
        }

        LCB3->other_subst = LCB1->other_subst * LCB2->other_subst * total;
        LCB3->other_subst.log() += total_scale*log_scale_min;
        return LCB3;
    }

    Likelihood_Cache_Branch*
    peel_internal_branch_SEV(const Likelihood_Cache_Branch* LCB1,
                             const Likelihood_Cache_Branch* LCB2,
                             const EVector& transition_P,
                             const Matrix& /*F*/)
    {
        total_peel_internal_branches++;

        const int n_models = transition_P.size();
        const int n_states = transition_P[0].as_<Box<Matrix>>().size1();
        const int matrix_size = n_models * n_states;
    
        const auto& bits1 = LCB1->bits;
        const auto& bits2 = LCB2->bits;

        int L = bits1.size();
        assert(L > 0);
        assert(bits2.size() == L);

        // Do this before accessing matrices or other_subst
        auto* LCB3 = new Likelihood_Cache_Branch(L, n_models, n_states);
        LCB3->bits = LCB1->bits | LCB2->bits;
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
                element_prod_assign(S, (*LCB1)[i1], (*LCB2)[i2], matrix_size);
                scale = LCB1->scale(i1) + LCB2->scale(i2);
            }
            else if (nongap1)
            {
                C = (*LCB1)[i1];
                scale = LCB1->scale(i1);
            }
            else if (nongap2)
            {
                C = (*LCB2)[i2];
                scale = LCB2->scale(i2);
            }
            else
            {
                // columns like this should not be in the index
                std::abort();
            }

            // propagate from the source distribution
            double* R = (*LCB3)[i3];            //name the result matrix
            bool need_scale = true;
            for(int m=0;m<n_models;m++)
            {
                const Matrix& Q = transition_P[m].as_<Box<Matrix>>();
        
                // compute the distribution at the target (parent) node - multiple letters
                for(int s1=0;s1<n_states;s1++) {
                    double temp=0;
                    for(int s2=0;s2<n_states;s2++)
                        temp += Q(s1,s2)*C[m*n_states + s2];
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
            LCB3->scale(i3) = scale;

            if (nongap1) i1++;
            if (nongap2) i2++;
            i3++;
        }

        return LCB3;
    }

    Likelihood_Cache_Branch*
    peel_deg2_branch_SEV(const Likelihood_Cache_Branch* LCB1,
                         const EVector& transition_P,
                         const Matrix& /*F*/)
    {
        total_peel_internal_branches++;

        const int n_models = transition_P.size();
        const int n_states = transition_P[0].as_<Box<Matrix>>().size1();
        const int matrix_size = n_models * n_states;

        const auto& bits1 = LCB1->bits;

        int L = bits1.size();
        assert(L > 0);

        // Do this before accessing matrices or other_subst
        auto* LCB2 = new Likelihood_Cache_Branch(L, n_models, n_states);
        LCB2->bits = LCB1->bits;

        for(int c=0,i=0;c<L;c++)
        {
            if (not bits1.test(c)) continue;

            int scale = 0;

            const double* C = (*LCB1)[i];
            scale = LCB1->scale(i);

            // propagate from the source distribution
            double* R = (*LCB2)[i];            //name the result matrix
            bool need_scale = true;
            for(int m=0;m<n_models;m++)
            {
                const Matrix& Q = transition_P[m].as_<Box<Matrix>>();

                // compute the distribution at the target (parent) node - multiple letters
                for(int s1=0;s1<n_states;s1++)
                {
                    double temp=0;
                    for(int s2=0;s2<n_states;s2++)
                        temp += Q(s1,s2)*C[m*n_states + s2];
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
            LCB2->scale(i) = scale;

            i++;
        }

        return LCB2;
    }
  
    Box<matrix<int>>* alignment_index2(const pairwise_alignment_t& A0, const pairwise_alignment_t& A1)
    {
        auto a0 = convert_to_bits(A0, 0, 2);
        auto a1 = convert_to_bits(A1, 1, 2);
        auto a012 = Glue_A(a0, a1);

        // get the relationships with the sub-alignments for the (two) branches behind b0
        auto index = new Box<matrix<int>>;
        *index = get_indices_from_bitpath(a012, {0,1,2});
        return index;
    }

    Likelihood_Cache_Branch*
    peel_internal_branch_F81(const Likelihood_Cache_Branch* LCB1,
                             const Likelihood_Cache_Branch* LCB2,
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
        auto smap = P.state_letters();

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
        const auto& sequence = P.get_sequence(n);
        const alphabet& a = P.get_alphabet();
        return get_leaf_seq_likelihoods(sequence, a, P, delta);
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
            LCB.set(i,0);

        vector<const Likelihood_Cache_Branch*> cache;
        for(int branch: b)
            cache.push_back(&P.cache(branch));

        // For each column in the index (e.g. for each present character at node 'root')
        for(int i=0;i<index.size1();i++) 
        {
            LCB.set(i+delta, 1);

            // Note that we could do ZERO products in this loop
            auto m = LCB[i+delta];
            int scale = 0;
            for(int j=0;j<b.size();j++) 
            {
                int i0 = index(i,j);
                if (i0 == alphabet::gap) continue;

                element_prod_modify(m, (*cache[j])[i0], matrix_size);
                scale += cache[j]->scale(i0);
            }
            LCB.scale(i) = scale;
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

    void calc_transition_prob_from_parent(Matrix& S, const pair<int,int>& state_model_parent, const EVector& Ps, const Matrix& WF)
    {
        auto [mp,lp] = state_model_parent;

        int n_states = S.size2();

        // If there IS no parent character, then we can sample from F
        if (mp == -1)
            S = WF;
        else
        {
            auto& Pr = Ps[mp].as_<Box<Matrix>>();
            assert(mp != -1);
            element_assign(S,0);
            for(int l=0;l<n_states;l++)
                S(mp,l) = Pr(lp,l);
        }
    }

    void calc_transition_prob_from_parent(Matrix& S, const pair<int,int>& state_model_parent, const EVector& Ps)
    {
        auto [mp,lp] = state_model_parent;

        int n_states = S.size2();

        // If there IS no parent character, then we can sample from F
        assert(mp != -1);

        auto& Pr = Ps[mp].as_<Box<Matrix>>();
        assert(mp != -1);
        element_assign(S,0);
        for(int l=0;l<n_states;l++)
            S(mp,l) = Pr(lp,l);
    }

    void calc_leaf_likelihood(Matrix& S, int l, const alphabet& a, const EVector& smap)
    {
        int n_models = S.size1();
        int n_states = S.size2();

        if (l == alphabet::not_gap)
            ;
        else if (a.is_letter(l))
        {
            // Clear S(m,s) for every state s that doesn't map to the observed letter l
            for(int s=0;s<n_states;s++)
                if (smap[s].as_int() != l)
                    for(int m=0;m<n_models;m++)
                        S(m,s) = 0;
        }
        else
        {
            assert(a.is_letter_class(l));
            const auto& letters = a.letter_mask(l);
            for(int l=0;l<letters.size();l++)
                if (letters.test(l))
                    for(int s=0;s<n_states;s++)
                        if (smap[s].as_int() != l)
                            for(int m=0;m<n_models;m++)
                                S(m,s) = 0;
        }
    }

    Vector<pair<int,int>> sample_root_sequence(const Likelihood_Cache_Branch& cache0,
                                               const Likelihood_Cache_Branch& cache1,
                                               const Likelihood_Cache_Branch& cache2,
                                               const pairwise_alignment_t& A0,
                                               const pairwise_alignment_t& A1,
                                               const pairwise_alignment_t& A2,
                                               const Matrix& F)
    {
        // FIXME - this doesn't handle case where tree has only 2 leaves.

        // 1. Get and check length for the central node
        int L0 = A0.length2();
        assert(L0 == A1.length2());
        assert(L0 == A2.length2());

        // 2. Get the alignment of columns present at the internal node.
        auto a10 = convert_to_bits(A0,1,0);
        auto a20 = convert_to_bits(A1,2,0);
        auto a30 = convert_to_bits(A2,3,0);
        auto a0123 = Glue_A(a10, Glue_A(a20,a30));
        auto index = get_indices_from_bitpath_w(a0123, {1,2,3}, (1<<0));
        assert(L0 == index.size1());

        // 3. Construct a scratch matrix and check that dimensions match inputs
        int n_models = F.size1();
        int n_states = F.size2();
        assert(n_models == cache0.n_models());
        assert(n_states == cache0.n_states());
        assert(n_models == cache1.n_models());
        assert(n_states == cache1.n_states());
        assert(n_models == cache2.n_models());
        assert(n_states == cache2.n_states());
        Matrix S(n_models, n_states);
        const int matrix_size = n_models * n_states;


        // 4. Walk the alignment and sample (model,letter) for ancestral sequence
        Vector<pair<int,int>> ancestral_characters(L0);
        for(int i=0;i<L0;i++)
        {
            int i0 = index(i,0);
            int i1 = index(i,1);
            int i2 = index(i,2);

            S = F;

            if (i0 != -1) element_prod_modify(S.begin(), cache0[i0], matrix_size);
            if (i1 != -1) element_prod_modify(S.begin(), cache1[i1], matrix_size);
            if (i2 != -1) element_prod_modify(S.begin(), cache2[i2], matrix_size);

            ancestral_characters[i] = sample(S);
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

    Vector<pair<int,int>> sample_root_sequence_SEV(const Likelihood_Cache_Branch& cache1,
                                                   const Likelihood_Cache_Branch& cache2,
                                                   const Likelihood_Cache_Branch& cache3,
                                                   const Matrix& F,
                                                   const EVector& compressed_col_for_col)
    {
        // 1. Construct a scratch CL matrix, and check that dimensions match inputs
        int n_models = F.size1();
        int n_states = F.size2();
        assert(n_models == cache1.n_models());
        assert(n_states == cache1.n_states());
        assert(n_models == cache2.n_models());
        assert(n_states == cache2.n_states());
        assert(n_models == cache3.n_models());
        assert(n_states == cache3.n_states());
        Matrix S(n_models, n_states);
        const int matrix_size = n_models * n_states;

        // 2. Map each compressed column to an SEV index (or missing).
        auto allbits = cache1.bits | cache2.bits | cache3.bits;

        auto index_for_column1 = get_index_for_column(cache1.bits);
        auto index_for_column2 = get_index_for_column(cache2.bits);
        auto index_for_column3 = get_index_for_column(cache3.bits);

        // 3. Walk the alignment and sample (model,letter) for ancestral sequence
        int L = compressed_col_for_col.size();
        Vector<pair<int,int>> ancestral_characters(L,{-1,-1});
        for(int c = 0; c < L; c++)
        {
            int c2 = compressed_col_for_col[c].as_int();
            if (not allbits.test(c2)) continue;
            S = F;
            if (auto i1 = index_for_column1[c2])
                element_prod_modify(S.begin(), cache1[*i1], matrix_size);
            if (auto i2 = index_for_column2[c2])
                element_prod_modify(S.begin(), cache2[*i2], matrix_size);
            if (auto i3 = index_for_column3[c2])
                element_prod_modify(S.begin(), cache3[*i3], matrix_size);

            ancestral_characters[c] = sample( S );
        }
        return ancestral_characters;
    }

    Vector<pair<int,int>> sample_internal_node_sequence(const Vector<pair<int,int>>& parent_seq,
                                                        const EVector& transition_Ps,
                                                        const Likelihood_Cache_Branch& cache1,
                                                        const Likelihood_Cache_Branch& cache2,
                                                        const pairwise_alignment_t& A0,
                                                        const pairwise_alignment_t& A1,
                                                        const pairwise_alignment_t& A2,
                                                        const Matrix& F)
    {
        // 1. Get and check length for the central node
        int L0 = A0.length2();
        assert(L0 == A1.length2());
        assert(L0 == A2.length2());

        // 2. Get the alignment of columns present at the internal node.
        auto a10 = convert_to_bits(A0,1,0);
        auto a20 = convert_to_bits(A1,2,0);
        auto a30 = convert_to_bits(A2,3,0);
        auto a0123 = Glue_A(a10, Glue_A(a20,a30));
        auto index = get_indices_from_bitpath_w(a0123, {1,2,3}, (1<<0));
        assert(L0 == index.size1());

        // 3. Construct a scratch matrix and check that dimensions match inputs
        int n_models = F.size1();
        int n_states = F.size2();
        assert(n_models == cache1.n_models());
        assert(n_states == cache1.n_states());
        assert(n_models == cache2.n_models());
        assert(n_states == cache2.n_states());
        Matrix S(n_models, n_states);
        const int matrix_size = n_models * n_states;


        // 4. Walk the alignment and sample (model,letter) for ancestral sequence
        Vector<pair<int,int>> ancestral_characters(L0);
        for(int i=0;i<L0;i++)
        {
            int i0 = index(i,0);
            int i1 = index(i,1);
            int i2 = index(i,2);

            pair<int,int> parent_state(-1,-1);
            if (i0 != -1)
                parent_state = parent_seq[i0];

            calc_transition_prob_from_parent(S, parent_state, transition_Ps, F);

            if (i1 != -1) element_prod_modify(S.begin(), cache1[i1], matrix_size);
            if (i2 != -1) element_prod_modify(S.begin(), cache2[i2], matrix_size);

            ancestral_characters[i] = sample(S);
        }
        return ancestral_characters;
    }

    
    Vector<pair<int,int>> sample_internal_node_sequence_SEV(const Vector<pair<int,int>>& parent_seq,
                                                            const EVector& transition_Ps,
                                                            const Likelihood_Cache_Branch& cache1,
                                                            const Likelihood_Cache_Branch& cache2,
                                                            const EVector& compressed_col_for_col)
    {
        // 1. Construct a scratch matrix and check that dimensions match inputs
        const int n_models  = transition_Ps.size();
        const int n_states  = transition_Ps[0].as_<Box<Matrix>>().size1();
        assert(n_models == cache1.n_models());
        assert(n_states == cache1.n_states());
        assert(n_models == cache2.n_models());
        assert(n_states == cache2.n_states());
        Matrix S(n_models, n_states);
        const int matrix_size = n_models * n_states;

        // 2. Get the total length of the COMPRESSED matrix.
        auto allbits = cache1.bits | cache2.bits ;

        auto index_for_column1 = get_index_for_column(cache1.bits);
        auto index_for_column2 = get_index_for_column(cache2.bits);

        // 3. Walk the alignment and sample (model,letter) for ancestral sequence
        int L = compressed_col_for_col.size();
        Vector<pair<int,int>> ancestral_characters(L,{-1,-1});
        for(int c = 0; c < L; c++)
        {
            int c2 = compressed_col_for_col[c].as_int();
            if (not allbits.test(c2)) continue;

            calc_transition_prob_from_parent(S, parent_seq[c], transition_Ps);

            if (auto i1 = index_for_column1[c2])
                element_prod_modify(S.begin(), cache1[*i1], matrix_size);
            if (auto i2 = index_for_column2[c2])
                element_prod_modify(S.begin(), cache2[*i2], matrix_size);

            ancestral_characters[c] = sample(S);
        }
        return ancestral_characters;
    }

    
    Vector<pair<int,int>> sample_leaf_node_sequence(const Vector<pair<int,int>>& parent_seq,
                                                    const EVector& transition_Ps,
                                                    const EVector& sequence,
                                                    const alphabet& a,
                                                    const EVector& smap,
                                                    const pairwise_alignment_t& A0,
                                                    const Matrix& F)
    {
        // 1. Get and check length for the leaf node
        int L0 = A0.length2();

        // 2. Get the alignment of columns present at the leaf node.
        auto a10 = convert_to_bits(A0,1,0);
        auto index = get_indices_from_bitpath_w(a10, {1}, (1<<0));
        assert(L0 == index.size1());
        assert(L0 == sequence.size());

        // 3. Construct a scratch matrix and check that dimensions match inputs
        int n_models = F.size1();
        int n_states = F.size2();
        Matrix S(n_models, n_states);

        // 4. Walk the alignment and sample (model,letter) for leaf sequence
        Vector<pair<int,int>> ancestral_characters(L0);
        for(int i=0;i<L0;i++)
        {
            int i0 = index(i,0);

            pair<int,int> parent_state(-1,-1);
            if (i0 != -1)
                parent_state = parent_seq[i0];

            calc_transition_prob_from_parent(S, parent_state, transition_Ps, F);

            calc_leaf_likelihood(S, sequence[i].as_int(), a, smap);

            ancestral_characters[i] = sample(S);
        }
        return ancestral_characters;
    }


    Vector<pair<int,int>> sample_leaf_node_sequence_SEV(const Vector<pair<int,int>>& parent_seq,
                                                        const EVector& transition_Ps,
                                                        const EVector& sequence,
                                                        const Likelihood_Cache_Branch& cache,
                                                        const alphabet& a,
                                                        const EVector& smap,
                                                        const EVector& compressed_col_for_col)
    {
        // 1. Construct a scratch matrix and check that dimensions match inputs
        const int n_models  = transition_Ps.size();
        const int n_states  = transition_Ps[0].as_<Box<Matrix>>().size1();
        Matrix S(n_models, n_states);

        // 2. Walk the alignment and sample (model,letter) for leaf sequence
        auto index_for_column = get_index_for_column(cache.bits);

        int L = compressed_col_for_col.size();
        Vector<pair<int,int>> ancestral_characters(L,{-1,-1});
        for(int c = 0; c < L; c++)
        {
            int c2 = compressed_col_for_col[c].as_int();

            if (auto index = index_for_column[c2])
            {
                int letter = sequence[*index].as_int();

                calc_transition_prob_from_parent(S, parent_seq[c], transition_Ps);

                calc_leaf_likelihood(S, letter, a, smap);

                ancestral_characters[c] = sample(S);
            }
        }
        return ancestral_characters;
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
