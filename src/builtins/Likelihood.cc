#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
//#define DEBUG_RATE_MATRIX
#include "computation/machine/args.H"
#include "math/exponential.H"
#include "math/eigenvalue.H"
#include "sequence/alphabet.H"
#include "sequence/doublets.H"
#include "sequence/codons.H"
#include "util/io.H"
#include <valarray>
#include "dp/2way.H"
#include "util/range.H"
#include <unsupported/Eigen/MatrixFunctions>
#include "substitution/parsimony.H"

using std::vector;
using std::pair;
using std::istringstream;
using std::istream;
using std::valarray;

using std::cerr;
using std::endl;
using std::abs;

using Alphabet = PtrBox<alphabet>;

#include "substitution/cache.H"
#include "dp/hmm.H"
using boost::dynamic_bitset;

extern "C" closure builtin_function_bitmask_from_sequence(OperationArgs& Args)
{
    using boost::dynamic_bitset;

    auto arg0 = Args.evaluate(0);
    const auto& seq = arg0. as_<EVector>();

    int L = seq.size();

    object_ptr<Box<dynamic_bitset<>>> mask_(new Box<dynamic_bitset<>>(L));
    auto& mask = *mask_;

    for(int i=0;i<L;i++)
    {
        int c = seq[i].as_int();
	if (c != alphabet::gap and c != alphabet::unknown)
	    mask.flip(i);
    }

    return mask_;
}


extern "C" closure builtin_function_strip_gaps(OperationArgs& Args)
{
    using boost::dynamic_bitset;

    auto arg0 = Args.evaluate(0);
    const auto& seq1 = arg0. as_<EVector>();

    int L = seq1.size();

    object_ptr<EVector> Seq2(new EVector);
    auto& seq2 = *Seq2;

    for(int i=0;i<L;i++)
    {
        int c = seq1[i].as_int();
        if (c != alphabet::gap and c != alphabet::unknown)
            seq2.push_back(c);
    }

    return Seq2;
}


namespace substitution {
    object_ptr<const Likelihood_Cache_Branch>
    peel_leaf_branch(const EVector& sequence, const alphabet& a, const EVector& transition_P, const EVector& smap);

    object_ptr<const Likelihood_Cache_Branch>
    peel_leaf_branch_SEV(const EVector& sequence, const alphabet& a, const EVector& transition_P, const dynamic_bitset<>& mask, const EVector& smap);

    object_ptr<const Likelihood_Cache_Branch>
    peel_branch(const EVector& sequences,
		const alphabet& a,
		const EVector& smap,
		const EVector& LCB,
		const EVector& A,
		const EVector& transition_P,
		const Matrix& F);
}

extern "C" closure builtin_function_peel_leaf_branch(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);

    return substitution::peel_leaf_branch(arg0.as_<EVector>(), *arg1.as_<Alphabet>(), arg2.as_<EVector>(), arg3.as_<EVector>());
}


extern "C" closure builtin_function_peel_leaf_branch_SEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);

    return substitution::peel_leaf_branch_SEV(arg0.as_<EVector>(), *arg1.as_<Alphabet>(), arg2.as_<EVector>(), arg3.as_<Box<dynamic_bitset<>>>(), arg4.as_<EVector>());
}

extern "C" closure builtin_function_peelBranch(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& sequences = arg0.as_<EVector>();

    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);
    auto arg5 = Args.evaluate(5);
    auto arg6 = Args.evaluate(6);

    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);

    return substitution::peel_branch(sequences,                  // sequence
				     *arg1.as_<Alphabet>(),      // alphabet
				     arg2.as_<EVector>(),        // smap
				     arg3.as_<EVector>(),        // LCB
				     arg4.as_<EVector>(),        // A
				     arg5.as_<EVector>(),        // transition_P
				     arg6.as_<Box<Matrix>>()  ); // F
}



namespace substitution {
    object_ptr<const Box<matrix<int>>>
    alignment_index2(const pairwise_alignment_t&, const pairwise_alignment_t&);
    
    object_ptr<const Box<matrix<int>>>
    alignment_index3(const pairwise_alignment_t&, const pairwise_alignment_t&, const pairwise_alignment_t&);

    object_ptr<const Likelihood_Cache_Branch>
    peel_internal_branch(const Likelihood_Cache_Branch& LCB1,
			 const Likelihood_Cache_Branch& LCB2,
			 const pairwise_alignment_t&,
			 const pairwise_alignment_t&,
			 const EVector& transition_P,
			 const Matrix& F);

    object_ptr<const Likelihood_Cache_Branch>
    peel_internal_branch_SEV(const Likelihood_Cache_Branch& LCB1,
			     const Likelihood_Cache_Branch& LCB2,
			     const EVector& transition_P);

    object_ptr<const Likelihood_Cache_Branch>
    peel_deg2_branch_SEV(const Likelihood_Cache_Branch& LCB1,
                         const EVector& transition_P);

    object_ptr<const Likelihood_Cache_Branch>
    peel_branch_SEV(const EVector& sequences,
		    const alphabet& a,
		    const EVector& smap,
		    const EVector& LCB,
		    const EVector& transition_P);
}

extern "C" closure builtin_function_alignment_index2(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);

    return substitution::alignment_index2(arg0.as_<Box<pairwise_alignment_t>>(), arg1.as_<Box<pairwise_alignment_t>>());
}

extern "C" closure builtin_function_alignment_index3(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);

    return substitution::alignment_index3(arg0.as_<Box<pairwise_alignment_t>>(), arg1.as_<Box<pairwise_alignment_t>>(),  arg2.as_<Box<pairwise_alignment_t>>());
}

extern "C" closure builtin_function_peel_internal_branch(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);
    auto arg5 = Args.evaluate(5);

    return substitution::peel_internal_branch(arg0.as_<Likelihood_Cache_Branch>(),
					      arg1.as_<Likelihood_Cache_Branch>(),
					      arg2.as_<Box<pairwise_alignment_t>>(),
					      arg3.as_<Box<pairwise_alignment_t>>(),
					      arg4.as_<EVector>(),
					      arg5.as_<Box<Matrix>>());
}

extern "C" closure builtin_function_peel_internal_branch_SEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);

    return substitution::peel_internal_branch_SEV(arg0.as_<Likelihood_Cache_Branch>(),
						  arg1.as_<Likelihood_Cache_Branch>(),
						  arg2.as_<EVector>());
}

extern "C" closure builtin_function_peel_deg2_branch_SEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);

    return substitution::peel_deg2_branch_SEV(arg0.as_<Likelihood_Cache_Branch>(),
                                              arg1.as_<EVector>());
}

extern "C" closure builtin_function_peelBranchSEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);

    return substitution::peel_branch_SEV(arg0.as_<EVector>(),        // sequences
					 *arg1.as_<Alphabet>(),      // alphabet
					 arg2.as_<EVector>(),        // smap
					 arg3.as_<EVector>(),        // LCB
					 arg4.as_<EVector>());       // transition_P
}

namespace substitution {
    Vector<pair<int,int>> sample_root_sequence(const Likelihood_Cache_Branch& cache0,
                                               const Likelihood_Cache_Branch& cache1,
                                               const Likelihood_Cache_Branch& cache2,
                                               const pairwise_alignment_t& A0,
                                               const pairwise_alignment_t& A1,
                                               const pairwise_alignment_t& A2,
                                               const Matrix& F);

    Vector<pair<int,int>> sample_root_deg2_sequence_SEV(const Likelihood_Cache_Branch& cache1,
                                                        const Likelihood_Cache_Branch& cache2,
                                                        const Matrix& F,
                                                        const EVector& compressed_col_for_col);

    Vector<pair<int,int>> sample_internal_node_sequence(const Vector<pair<int,int>>& parent_seq,
                                                        const EVector& transition_Ps,
                                                        const Likelihood_Cache_Branch& cache1,
                                                        const Likelihood_Cache_Branch& cache2,
                                                        const pairwise_alignment_t& A0,
                                                        const pairwise_alignment_t& A1,
                                                        const pairwise_alignment_t& A2,
                                                        const Matrix& F);

    Vector<pair<int,int>> sample_leaf_node_sequence(const Vector<pair<int,int>>& parent_seq,
                                                    const EVector& transition_Ps,
                                                    const EVector& sequence,
                                                    const alphabet& a,
                                                    const EVector& smap1,
                                                    const pairwise_alignment_t& A0,
                                                    const Matrix& F);

    Vector<pair<int,int>> sample_root_sequence_SEV(const Likelihood_Cache_Branch& cache0,
                                                   const Likelihood_Cache_Branch& cache1,
                                                   const Likelihood_Cache_Branch& cache2,
                                                   const Matrix& F,
                                                   const EVector& compressed_col_for_col);

    Vector<pair<int,int>> sample_internal_node_sequence_SEV(const Vector<pair<int,int>>& parent_seq,
                                                            const EVector& transition_Ps,
                                                            const Likelihood_Cache_Branch& cache1,
                                                            const Likelihood_Cache_Branch& cache2,
                                                            const EVector& compressed_col_for_col);

    Vector<pair<int,int>> sample_deg2_node_sequence_SEV(const Vector<pair<int,int>>& parent_seq,
                                                        const EVector& transition_Ps,
                                                        const Likelihood_Cache_Branch& cache,
                                                        const EVector& compressed_col_for_col);

    Vector<pair<int,int>> sample_leaf_node_sequence_SEV(const Vector<pair<int,int>>& parent_seq,
                                                        const EVector& transition_Ps,
                                                        const EVector& sequence,
                                                        const Likelihood_Cache_Branch& cache1,  // just for the mask
                                                        const alphabet& a,
                                                        const EVector& smap1,
                                                        const EVector& compressed_col_for_col);

    log_double_t calc_root_probability(const Likelihood_Cache_Branch& LCB1,
				       const Likelihood_Cache_Branch& LCB2,
				       const Likelihood_Cache_Branch& LCB3,
				       const pairwise_alignment_t& A1,
				       const pairwise_alignment_t& A2,
				       const pairwise_alignment_t& A3,
				       const Matrix& F);

    log_double_t calc_root_prob(const EVector& sequences,
				const alphabet& a,
				const EVector& smap,
				const EVector& LCB,
				const EVector& A,
				const Matrix& F);

    log_double_t calc_root_prob_SEV(const EVector& sequences,
				    const alphabet& a,
				    const EVector& smap,
				    const EVector& LCB,
				    const Matrix& F,
				    const EVector& counts);

    log_double_t calc_root_probability_SEV(const Likelihood_Cache_Branch& LCB1,
					   const Likelihood_Cache_Branch& LCB2,
					   const Likelihood_Cache_Branch& LCB3,
					   const Matrix& F,
					   const EVector& counts);

    log_double_t calc_root_deg2_probability_SEV(const Likelihood_Cache_Branch& LCB1,
                                                const Likelihood_Cache_Branch& LCB2,
                                                const Matrix& F,
                                                const EVector& counts);
}


extern "C" closure builtin_function_calcRootProb(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);
    auto arg5 = Args.evaluate(5);

    log_double_t Pr = substitution::calc_root_prob(arg0.as_<EVector>(),       // sequences
						   *arg1.as_<Alphabet>(),     // a
						   arg2.as_<EVector>(),       // smap
						   arg3.as_<EVector>(),       // LCB
						   arg4.as_<EVector>(),       // A
						   arg5.as_<Box<Matrix>>());  // F
    return {Pr};
}

extern "C" closure builtin_function_calcRootProbSEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);
    auto arg5 = Args.evaluate(5);

    log_double_t Pr = substitution::calc_root_prob_SEV(arg0.as_<EVector>(),       // sequences
						       *arg1.as_<Alphabet>(),     // a
						       arg2.as_<EVector>(),       // smap
						       arg3.as_<EVector>(),       // LCB
						       arg4.as_<Box<Matrix>>(),   // F
						       arg5.as_<EVector>());  // counts
    return {Pr};
}

extern "C" closure builtin_function_calc_root_probability(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);
    auto arg5 = Args.evaluate(5);
    auto arg6 = Args.evaluate(6);

    log_double_t Pr = substitution::calc_root_probability(arg0.as_<Likelihood_Cache_Branch>(),
							  arg1.as_<Likelihood_Cache_Branch>(),
							  arg2.as_<Likelihood_Cache_Branch>(),
							  arg3.as_<Box<pairwise_alignment_t>>(),
							  arg4.as_<Box<pairwise_alignment_t>>(),
							  arg5.as_<Box<pairwise_alignment_t>>(),
							  arg6.as_<Box<Matrix>>());
    return {Pr};
}

extern "C" closure builtin_function_sample_root_sequence(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);
    auto arg5 = Args.evaluate(5);
    auto arg6 = Args.evaluate(6);

    return substitution::sample_root_sequence(arg0.as_<Likelihood_Cache_Branch>(),
                                              arg1.as_<Likelihood_Cache_Branch>(),
                                              arg2.as_<Likelihood_Cache_Branch>(),
                                              arg3.as_<Box<pairwise_alignment_t>>(),
                                              arg4.as_<Box<pairwise_alignment_t>>(),
                                              arg5.as_<Box<pairwise_alignment_t>>(),
                                              arg6.as_<Box<Matrix>>());
}

extern "C" closure builtin_function_sample_root_sequence_SEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);

    return substitution::sample_root_sequence_SEV(arg0.as_<Likelihood_Cache_Branch>(),
                                                  arg1.as_<Likelihood_Cache_Branch>(),
                                                  arg2.as_<Likelihood_Cache_Branch>(),
                                                  arg3.as_<Box<Matrix>>(),
                                                  arg4.as_<EVector>());
}

extern "C" closure builtin_function_sample_root_deg2_sequence_SEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);

    return substitution::sample_root_deg2_sequence_SEV(arg0.as_<Likelihood_Cache_Branch>(),
                                                       arg1.as_<Likelihood_Cache_Branch>(),
                                                       arg2.as_<Box<Matrix>>(),
                                                       arg3.as_<EVector>());
}

extern "C" closure builtin_function_sample_internal_sequence(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);
    auto arg5 = Args.evaluate(5);
    auto arg6 = Args.evaluate(6);
    auto arg7 = Args.evaluate(7);

    return substitution::sample_internal_node_sequence(arg0.as_<Vector<pair<int,int>>>(),
                                                       arg1.as_<EVector>(),
                                                       arg2.as_<Likelihood_Cache_Branch>(),
                                                       arg3.as_<Likelihood_Cache_Branch>(),
                                                       arg4.as_<Box<pairwise_alignment_t>>(),
                                                       arg5.as_<Box<pairwise_alignment_t>>(),
                                                       arg6.as_<Box<pairwise_alignment_t>>(),
                                                       arg7.as_<Box<Matrix>>());
}

extern "C" closure builtin_function_sample_internal_sequence_SEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);

    return substitution::sample_internal_node_sequence_SEV(arg0.as_<Vector<pair<int,int>>>(),
                                                           arg1.as_<EVector>(),
                                                           arg2.as_<Likelihood_Cache_Branch>(),
                                                           arg3.as_<Likelihood_Cache_Branch>(),
                                                           arg4.as_<EVector>());
}

extern "C" closure builtin_function_sample_deg2_sequence_SEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);

    return substitution::sample_deg2_node_sequence_SEV(arg0.as_<Vector<pair<int,int>>>(),
                                                       arg1.as_<EVector>(),
                                                       arg2.as_<Likelihood_Cache_Branch>(),
                                                       arg3.as_<EVector>());
}

extern "C" closure builtin_function_sample_leaf_sequence(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);
    auto arg5 = Args.evaluate(5);
    auto arg6 = Args.evaluate(6);

    return substitution::sample_leaf_node_sequence(arg0.as_<Vector<pair<int,int>>>(),
                                                   arg1.as_<EVector>(),
                                                   arg2.as_<EVector>(),
                                                   *arg3.as_<Alphabet>(),
                                                   arg4.as_<EVector>(),
                                                   arg5.as_<Box<pairwise_alignment_t>>(),
                                                   arg6.as_<Box<Matrix>>());
}

extern "C" closure builtin_function_sample_leaf_sequence_SEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);
    auto arg5 = Args.evaluate(5);
    auto arg6 = Args.evaluate(6);

    return substitution::sample_leaf_node_sequence_SEV(arg0.as_<Vector<pair<int,int>>>(),
                                                       arg1.as_<EVector>(),
                                                       arg2.as_<EVector>(),
                                                       arg3.as_<Likelihood_Cache_Branch>(),
                                                       *arg4.as_<Alphabet>(),
                                                       arg5.as_<EVector>(),
                                                       arg6.as_<EVector>());
}

extern "C" closure builtin_function_calc_root_probability_SEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);

    log_double_t Pr = substitution::calc_root_probability_SEV(arg0.as_<Likelihood_Cache_Branch>(),
							      arg1.as_<Likelihood_Cache_Branch>(),
							      arg2.as_<Likelihood_Cache_Branch>(),
							      arg3.as_<Box<Matrix>>(),
							      arg4.as_<EVector>());
    return {Pr};
}

extern "C" closure builtin_function_calc_root_deg2_probability_SEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);

    log_double_t Pr = substitution::calc_root_deg2_probability_SEV(arg0.as_<Likelihood_Cache_Branch>(),
                                                                   arg1.as_<Likelihood_Cache_Branch>(),
                                                                   arg2.as_<Box<Matrix>>(),
                                                                   arg3.as_<EVector>());
    return {Pr};
}

inline double letter_class_frequency(int l, const alphabet& a, const vector<double>& f)
{
    assert(a.is_letter_class(l));
    double p = 0;
    const auto& fmask = a.letter_fmask(l);
    for(int j=0; j<a.size(); j++)
	p += f[j] * fmask[j];
    return p;
}

inline log_double_t letter_frequency(int l, const alphabet& a, const vector<double>& f, const vector<log_double_t>& lf)
{
    if (a.is_letter(l))
	return lf[l];
    else if (a.is_letter_class(l))
	return letter_class_frequency(l,a,f);
    else
	return 1;
}

extern "C" closure builtin_function_peel_likelihood_1(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);

    const auto& seq  = arg0.as_<EVector>();
    const auto& a    = *arg1.as_<Alphabet>();
    const auto& WF   = arg2.as_<Box<Matrix>>();

    // Make frequency-vector AND log(frequency)-vector
    vector<double> F(a.size(),0);
    vector<log_double_t> LF(a.size());
    for(int l=0;l<F.size();l++)
    {
	for(int m=0;m<WF.size1();m++)
	    F[l] += WF(m,l);
	LF[l] = F[l];
    }

    log_double_t Pr = 1;

    for(auto l: seq)
	Pr *= letter_frequency(l.as_int(), a, F, LF);

    return {Pr};
}

extern "C" closure builtin_function_peel_likelihood_2(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);
    auto arg5 = Args.evaluate(5);

    const auto& seq1  = arg0.as_<EVector>();
    const auto& seq2  = arg1.as_<EVector>();
    const alphabet& alpha = *arg2.as_<Alphabet>();
    const auto& A     = arg3.as_<Box<pairwise_alignment_t>>();
    const auto& P     = arg4.as_<EVector>();
    const auto& WF    = arg5.as_<Box<Matrix>>();

    // Make frequency-vector AND log(frequency)-vector
    vector<double> F(alpha.size(),0);
    vector<log_double_t> LF(alpha.size());
    for(int l=0;l<F.size();l++)
    {
	for(int m=0;m<WF.size1();m++)
	    F[l] += WF(m,l);
	LF[l] = F[l];
    }

    assert(A.length1() == seq1.size());
    assert(A.length2() == seq2.size());

    log_double_t Pr = 1;

    int i=0;
    int j=0;
    for(int x=0;x<A.size();x++)
    {
	if (A.is_match(x))
	{
	    int l1 = seq1[i++].as_int();
	    int l2 = seq2[j++].as_int();

	    if (alpha.is_letter(l1))
	    {
		double p = 0;
		if (alpha.is_letter(l2))
		{
		    for(int m=0;m<WF.size1();m++)
			p += WF(m,l1) * P[m].as_<Box<Matrix>>()(l1,l2);
		}
		else if (alpha.is_letter_class(l2))
		{
		    const auto & fmask = alpha.letter_fmask(l2);
		    for(int m=0;m<WF.size1();m++)
			for(int j=0; j<alpha.size(); j++)
			    p += WF(m,l1) * P[m].as_<Box<Matrix>>()(l1,j) * fmask[j];
		}
		else
		    p += F[l1];
		Pr *= p;
	    }
	    else if (alpha.is_letter_class(l1))
	    {
		double p = 0;
		if (alpha.is_letter(l2))
		{
		    const auto & fmask = alpha.letter_fmask(l1); 
		    for(int m=0;m<WF.size1();m++)
			for(int j=0; j<alpha.size(); j++)
			    p += WF(m,l2) * P[m].as_<Box<Matrix>>()(l2,j) * fmask[j];
		}
		else if (alpha.is_letter_class(l2))
		{
		    const auto & mask1 = alpha.letter_mask(l1);
		    const auto & fmask2 = alpha.letter_fmask(l2);
		    for(int m=0;m<WF.size1();m++)
			for(int j=0; j<alpha.size(); j++)
			    if (mask1.test(j))
				for(int k=0; k<alpha.size(); k++)
				    p += WF(m,j) * P[m].as_<Box<Matrix>>()(j,k) * fmask2[k];
		}
		else
		    p = letter_class_frequency(l1, alpha, F);
		Pr *= p;
	    }
	    else
		Pr *= letter_frequency(l2, alpha, F, LF);
	}
	else if (A.is_delete(x))
	{
	    int l = seq1[i++].as_int();
	    Pr *= letter_frequency(l, alpha, F, LF);
	}
	else
	{
	    int l = seq2[j++].as_int();
	    Pr *= letter_frequency(l, alpha, F, LF);
	}
    }
    return {Pr};
}

extern "C" closure builtin_function_peel_likelihood_1_SEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);

    const auto& s1       = arg0.as_<EVector>();
    const alphabet& a    = *arg1.as_<Alphabet>();
    const auto& WF       = arg2.as_<Box<Matrix>>();
    const auto& counts   = arg3.as_<EVector>();

    // Make frequency-vector AND log(frequency)-vector
    vector<double> F(a.size(),0);
    vector<log_double_t> LF(a.size());
    for(int l=0;l<F.size();l++)
    {
	for(int m=0;m<WF.size1();m++)
	    F[l] += WF(m,l);
	LF[l] = F[l];
    }

    log_double_t Pr = 1;
    for(int i=0;i<s1.size();i++)
    {
        int l = s1[i].as_int();
        log_double_t p = letter_frequency(l, a, F, LF);
        int count = counts[i].as_int();
	Pr *= pow(p,count);
    }

    return {Pr};
}

extern "C" closure builtin_function_peel_likelihood_2_SEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);
    auto arg5 = Args.evaluate(5);

    const auto& s1       = arg0.as_<EVector>();
    const auto& s2       = arg1.as_<EVector>();
    const alphabet& alpha = *arg2.as_<Alphabet>();
    const auto& P         = arg3.as_<EVector>();
    const auto& WF        = arg4.as_<Box<Matrix>>();
    const auto& counts    = arg5.as_<EVector>();

    assert(s1.size() == s2.size());
    assert(s1.size() == counts.size());

    // Make frequency-vector AND log(frequency)-vector
    vector<double> F(alpha.size(),0);
    vector<log_double_t> LF(alpha.size());
    for(int l=0;l<F.size();l++)
    {
	for(int m=0;m<WF.size1();m++)
	    F[l] += WF(m,l);
	LF[l] = F[l];
    }

    log_double_t Pr = 1;

    for(int x=0;x<s1.size();x++)
    {
        int l1 = s1[x].as_int();
        int l2 = s2[x].as_int();

        if (l1 < 0 and l2 < 0) continue;

        double p = 0;

	if (l1 >= 0 and l2 >= 0)
	{
	    if (alpha.is_letter(l1))
	    {
		if (alpha.is_letter(l2))
		{
		    for(int m=0;m<WF.size1();m++)
			p += WF(m,l1) * P[m].as_<Box<Matrix>>()(l1,l2);
		}
		else if (alpha.is_letter_class(l2))
		{
		    const auto & fmask = alpha.letter_fmask(l2);
		    for(int m=0;m<WF.size1();m++)
			for(int j=0; j<alpha.size(); j++)
			    p += WF(m,l1) * P[m].as_<Box<Matrix>>()(l1,j) * fmask[j];
		}
                else
                    std::abort();
	    }
	    else if (alpha.is_letter_class(l1))
	    {
		if (alpha.is_letter(l2))
		{
		    const auto & fmask = alpha.letter_fmask(l1); 
		    for(int m=0;m<WF.size1();m++)
			for(int j=0; j<alpha.size(); j++)
			    p += WF(m,l2) * P[m].as_<Box<Matrix>>()(l2,j) * fmask[j];
		}
		else if (alpha.is_letter_class(l2))
		{
		    const auto & mask1 = alpha.letter_mask(l1);
		    const auto & fmask2 = alpha.letter_fmask(l2);
		    for(int m=0;m<WF.size1();m++)
			for(int j=0; j<alpha.size(); j++)
			    if (mask1.test(j))
				for(int k=0; k<alpha.size(); k++)
				    p += WF(m,j) * P[m].as_<Box<Matrix>>()(j,k) * fmask2[k];
		}
		else
                    std::abort();
	    }
	}
	else if (l1 >= 0)
            p = (double)letter_frequency(l1, alpha, F, LF);
	else if (l2 >= 0)
	    p = (double)letter_frequency(l2, alpha, F, LF);
        else
            std::abort();

        int count = counts[x].as_int();
        Pr *= pow(log_double_t(p), count);
    }
    return {Pr};
}

// maskSequenceRaw :: CBitVector -> EVector Int -> EVector Int
extern "C" closure builtin_function_maskSequenceRaw(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& mask = arg0.as_<Box<dynamic_bitset<>>>();

    auto arg1 = Args.evaluate(1);
    auto sequence = arg1.as_<EVector>();

    assert(mask.size() == sequence.size());
    for(int i=0;i<sequence.size();i++)
    {
        auto& C = sequence[i];
        int c = C.as_int();
        assert(i >= alphabet::unknown);
        if (mask.test(i))
        {
            if (c == alphabet::gap or c == alphabet::unknown)
                C = alphabet::not_gap;
        }
        else
            C = alphabet::gap;
    }

    return sequence;
}

pair<int,int> sample(const Matrix& M);

extern "C" closure builtin_function_simulateRootSequence(OperationArgs& Args)
{
    int L = Args.evaluate(0).as_int();
    auto arg1 = Args.evaluate(1);
    auto& F = arg1.as_<Box<Matrix>>();

    Vector<pair<int,int>> sequence(L);
    for(int i=0;i<L;i++)
        sequence[i] = sample(F);
    return sequence;
}

namespace substitution
{

void calc_transition_prob_from_parent(Matrix& S, const pair<int,int>& state_model_parent, const EVector& Ps);
};

extern "C" closure builtin_function_simulateSequenceFrom(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& parentSequence = arg0.as_<Vector<pair<int,int>>>();

    auto arg1 = Args.evaluate(1);
    auto& alignment = arg1.as_<Box<pairwise_alignment_t>>();

    auto& arg2 = Args.evaluate(2);
    auto& transition_ps = arg2.as_<EVector>();

    auto arg3 = Args.evaluate(3);
    auto& F = arg3.as_<Box<Matrix>>();

    Vector<pair<int,int>> sequence;
    auto S = F;
    for(int i=0,j=0;i<alignment.size();i++)
    {
        pair<int,int> parent_model_state(-2,-2);
        if (alignment.is_delete(i))
            continue;
        else if (alignment.is_match(i))
            parent_model_state = parentSequence[j++];
        else if (alignment.is_insert(i))
            parent_model_state = sample(F);

        substitution::calc_transition_prob_from_parent(S, parent_model_state, transition_ps);

        sequence.push_back(sample(S));
    }

    return sequence;
}

extern "C" closure builtin_function_simulateFixedSequenceFrom(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& parentSequence = arg0.as_<Vector<pair<int,int>>>();

    auto& arg1 = Args.evaluate(1);
    auto& transition_ps = arg1.as_<EVector>();

    auto arg2 = Args.evaluate(2);
    auto& F = arg2.as_<Box<Matrix>>();

    int L = parentSequence.size();

    Vector<pair<int,int>> sequence;
    auto S = F;
    for(int i=0; i<L; i++)
    {
        auto parent_model_state = parentSequence[i];

        substitution::calc_transition_prob_from_parent(S, parent_model_state, transition_ps);

        sequence.push_back(sample(S));
    }

    return sequence;
}

