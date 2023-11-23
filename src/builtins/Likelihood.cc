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


namespace substitution
{

    object_ptr<const Likelihood_Cache_Branch>
    peel_branch_SEV(const EVector& sequences,
		    const alphabet& a,
		    const EVector& smap,
		    const EVector& LCB,
		    const EVector& transition_P);

    object_ptr<const Likelihood_Cache_Branch>
    peel_branch(const EVector& sequences,
		const alphabet& a,
		const EVector& smap,
		const EVector& LCB,
		const EVector& A,
		const EVector& transition_P,
		const Matrix& F);

    object_ptr<const Likelihood_Cache_Branch>
    simple_sequence_likelihoods_SEV(const EPair& sequence_mask,
				    const alphabet& a,
				    const EVector& smap,
				    int n_models);
}

extern "C" closure builtin_function_simpleSequenceLikelihoodsSEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);

    return substitution::simple_sequence_likelihoods_SEV(arg3.as_<EPair>(),     // sequence/bits
							 *arg0.as_<Alphabet>(), // alphabet
							 arg1.as_<EVector>(),   // smap
							 arg2.as_int());             // n_models
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

    Vector<pair<int,int>> sample_root_sequence(const EVector& sequences,
					       const alphabet& a,
					       const EVector& smap,
					       const EVector& LCB,
					       const EVector& A,
                                               const Matrix& F);

    Vector<pair<int,int>> sample_root_sequence_SEV(const EVector& sequences,
						   const alphabet& a,
						   const EVector& smap,
						   const EVector& LCB,
						   const Matrix& F,
						   const EVector& compressed_col_for_col);

    Vector<pair<int,int>> sample_branch_sequence(const Vector<pair<int,int>>& parent_seq,
						 const pairwise_alignment_t& parent_A,
						 const EVector& sequences,
						 const alphabet& a,
						 const EVector& smap,
						 const EVector& LCB,
						 const EVector& A,
						 const EVector& transition_P,
						 const Matrix& F);

    Vector<pair<int,int>> sample_leaf_node_sequence(const Vector<pair<int,int>>& parent_seq,
                                                    const EVector& transition_Ps,
                                                    const EVector& sequence,
                                                    const alphabet& a,
                                                    const EVector& smap1,
                                                    const pairwise_alignment_t& A0,
                                                    const Matrix& F);


    Vector<pair<int,int>> sample_sequence_SEV(const Vector<pair<int,int>>& parent_seq,
					      const EVector& sequences,
					      const alphabet& a,
					      const EVector& smap,
					      const EVector& transition_Ps,
					      const EVector& LCB,
					      const EVector& compressed_col_for_col);

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

    log_double_t calc_root_prob_SEV(const EVector& LCN,
				    const EVector& LCB,
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

extern "C" closure builtin_function_sampleSequenceSEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);
    auto arg5 = Args.evaluate(5);
    auto arg6 = Args.evaluate(6);

    return substitution::sample_sequence_SEV(arg0.as_<Vector<pair<int,int>>>(), // parent_seq,
					     arg1.as_<EVector>(),               // sequences
					     *arg2.as_<Alphabet>(),             // a
					     arg3.as_<EVector>(),               // smap
					     arg4.as_<EVector>(),               // transition_ps
					     arg5.as_<EVector>(),               // LCB
					     arg6.as_<EVector>());              // compressed_col_for_col
}

extern "C" closure builtin_function_calcRootProbSEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);

    log_double_t Pr = substitution::calc_root_prob_SEV(arg0.as_<EVector>(),       // sequences
						       arg1.as_<EVector>(),       // LCB
						       arg2.as_<Box<Matrix>>(),   // F
						       arg3.as_<EVector>());  // counts
    return {Pr};
}

extern "C" closure builtin_function_sampleRootSequence(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);
    auto arg5 = Args.evaluate(5);

    return substitution::sample_root_sequence(arg0.as_<EVector>(),      // sequences
                                              *arg1.as_<Alphabet>(),    // alphabet
                                              arg2.as_<EVector>(),      // smap
                                              arg3.as_<EVector>(),      // LCB
                                              arg4.as_<EVector>(),      // As
                                              arg5.as_<Box<Matrix>>()); // F
}

extern "C" closure builtin_function_sampleRootSequenceSEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);
    auto arg5 = Args.evaluate(5);

    return substitution::sample_root_sequence_SEV(arg0.as_<EVector>(),
                                                  *arg1.as_<Alphabet>(),
                                                  arg2.as_<EVector>(),
                                                  arg3.as_<EVector>(),
                                                  arg4.as_<Box<Matrix>>(),
                                                  arg5.as_<EVector>());
}

extern "C" closure builtin_function_sampleBranchSequence(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);
    auto arg5 = Args.evaluate(5);
    auto arg6 = Args.evaluate(6);
    auto arg7 = Args.evaluate(7);
    auto arg8 = Args.evaluate(8);

    return substitution::sample_branch_sequence(arg0.as_<Vector<pair<int,int>>>(),     // parent_seq
						arg1.as_<Box<pairwise_alignment_t>>(), // parent_A
						arg2.as_<EVector>(),                   // sequences
						*arg3.as_<Alphabet>(),                 // alphabet
						arg4.as_<EVector>(),                   // smap
						arg5.as_<EVector>(),                   // LCB
						arg6.as_<EVector>(),                   // A
						arg7.as_<EVector>(),                   // transition_P
						arg8.as_<Box<Matrix>>());              // F
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

