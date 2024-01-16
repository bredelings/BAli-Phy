#include "likelihood.H"
#include "likelihoodSEV.H"
#include "ops.H"
#include "sequence/alphabet.H"
#include "util/rng.H"
#include "util/log-level.H"
#include <cmath>
#include <vector>
#include "math/logprod.H"
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

using std::vector;
using std::pair;
using std::optional;


namespace substitution
{
   log_double_t calc_probability_at_root_SEV(const Likelihood_Cache_Branch& LCB1,
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


    log_double_t calc_at_deg2_probability_SEV(const Likelihood_Cache_Branch& LCB1,
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


    log_double_t calc_prob_at_root_SEV(const EVector& LCN,
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


    log_double_t calc_prob_SEV(const EVector& LCN,
			       const EVector& LCB,
			       const Matrix& FF,
			       const EVector& counts)
    {
	const Likelihood_Cache_Branch* away_from_root_branch = nullptr;
	for(auto& lcb: LCB)
	    if (lcb.as_<Likelihood_Cache_Branch>().away_from_root())
	    {
		assert(not away_from_root_branch);
		away_from_root_branch = &lcb.as_<Likelihood_Cache_Branch>();
	    }

	bool at_root = not away_from_root_branch;

	if (at_root)
	    return calc_prob_at_root_SEV(LCN, LCB, FF, counts);

	total_calc_root_prob++;

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
        const int n_models = cache(0).n_models();
        const int n_states = cache(0).n_states();
        const int matrix_size = n_models * n_states;


#ifndef NDEBUG
	assert(L > 0);

        for(int i=0;i<n_clvs;i++)
        {
            assert(cache(i).bits.size() == L);
	    assert(n_models == cache(i).n_models());
	    assert(n_states == cache(i).n_states());
        }
#endif
	const Matrix& f = away_from_root_branch->away_from_root_WF.value();
	const boost::dynamic_bitset<>& prev_rootward_bits = away_from_root_branch->bits;

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

	    if (not prev_rootward_bits.test(c))
		m[mi++] = f.begin();

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
		    p_col = element_prod_sum(m[0], m[1], m[2], matrix_size);
		else if (mi==2)
		    p_col = element_prod_sum(m[0], m[1], matrix_size);
		else if (mi==1)
		    p_col = element_sum(m[0], matrix_size);
		else
		    p_col = 1;
	    }
	    else
	    {
		if (mi==3)
		    element_prod_assign(S, m[0], m[1], m[2], matrix_size);
		else if (mi==2)
		    element_prod_assign(S, m[0], m[1], matrix_size);
		else if (mi==1)
		    element_prod_assign(S, m[0], matrix_size);
		else
		    element_assign(S, 1.0, matrix_size);

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
	    propagate_toward_root(R, n_models, n_states, scale, transition_P, S);
	    LCB->scale(i) = scale;
        }

        return LCB;
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
	    propagate_toward_root(R, n_models, n_states, scale, transition_P, C);
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
	    propagate_toward_root(R, n_models, n_states, scale, transition_P, C);
            LCB2->scale(i) = scale;

            i++;
        }

        return LCB2;
    }

    object_ptr<const Likelihood_Cache_Branch>
    peel_branch_toward_root_SEV(const EVector& LCN,
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
	else if (LCN.size() == 1 and LCB.empty())
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
	    propagate_toward_root(R, n_models, n_states, scale, transition_P, C);
            LCB_OUT->scale(s_out) = scale;

            s_out++;
        }

        return LCB_OUT;
    }

    object_ptr<const Likelihood_Cache_Branch>
    peel_branch_SEV(const EVector& LCN,
		     const EVector& LCB,
		     const EVector& transition_P,
		     const Matrix& ff,
		     bool away_from_root)
    {
	if (not away_from_root)
	    return peel_branch_toward_root_SEV(LCN, LCB, transition_P);

	const Likelihood_Cache_Branch* away_from_root_branch = nullptr;
	for(auto& lcb: LCB)
	    if (lcb.as_<Likelihood_Cache_Branch>().away_from_root())
	    {
		assert(not away_from_root_branch);
		away_from_root_branch = &lcb.as_<Likelihood_Cache_Branch>();
	    }

	bool at_root = not away_from_root_branch;

	const Matrix& f = at_root ? ff : away_from_root_branch->away_from_root_WF.value();

        total_peel_internal_branches++;

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

	boost::dynamic_bitset<> prev_rootward_bits_;
	if (at_root)
	    prev_rootward_bits_.resize(L);
	const boost::dynamic_bitset<>& prev_rootward_bits = at_root ? prev_rootward_bits_ : away_from_root_branch->bits;

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
	    if (prev_rootward_bits.test(c))
		element_assign(S, 1, matrix_size);
	    else
		element_assign(S, f.begin(), matrix_size);

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
	    propagate_away_from_root(R, n_models, n_states, scale, transition_P, C);
            LCB_OUT->scale(s_out) = scale;

            s_out++;
        }

	LCB_OUT->away_from_root_WF = propagate_frequencies(f, transition_P);

        return LCB_OUT;
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
