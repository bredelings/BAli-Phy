#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
//#define DEBUG_RATE_MATRIX
#include "computation/machine/args.H"
#include "sequence/alphabet.H"
#include "dp/2way.H"
#include "substitution/ops.H"
#include "substitution/likelihood.H"
#include "computation/expression/bool.H"

using std::vector;
using std::pair;
using std::istringstream;
using std::istream;

using std::cerr;
using std::endl;
using std::abs;

using Alphabet = PtrBox<alphabet>;

#include "substitution/cache.H"
#include "dp/hmm.H"
using boost::dynamic_bitset;

extern "C" closure builtin_function_bitmaskFromSequence(OperationArgs& Args)
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


extern "C" closure builtin_function_stripGaps(OperationArgs& Args)
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

extern "C" closure builtin_function_simpleSequenceLikelihoods(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);

    return substitution::simple_sequence_likelihoods2(arg3.as_<EVector>(),   // sequence
                                                      *arg0.as_<Alphabet>(), // alphabet
                                                      arg1.as_<EVector>(),   // smap
                                                      arg2.as_int());        // n_models
}


extern "C" closure builtin_function_peelBranchTowardRoot(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);

    return substitution::peel_branch_toward_root(arg0.as_<EVector>(),        // LCN
						 arg1.as_<EVector>(),        // LCB
						 arg2.as_<EVector>(),        // A
						 arg3.as_<EVector>(),        // transition_P
						 arg4.as_<Box<Matrix>>()  ); // F
}

extern "C" closure builtin_function_calcProbAtRoot(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);

    log_double_t Pr = substitution::calc_prob_at_root(arg0.as_<EVector>(),       // LCN
						      arg1.as_<EVector>(),       // LCB
						      arg2.as_<EVector>(),       // A
						      arg3.as_<Box<Matrix>>());  // F
    return {Pr};
}

extern "C" closure builtin_function_peelBranchAwayFromRoot(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);

    return substitution::peel_branch_away_from_root(arg0.as_<EVector>(),        // LCN
						    arg1.as_<EVector>(),        // LCB
						    arg2.as_<EVector>(),        // A
						    arg3.as_<EVector>(),        // transition_P
						    arg4.as_<Box<Matrix>>());   // F
}

extern "C" closure builtin_function_calcProb(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);

    log_double_t Pr = substitution::calc_prob(arg0.as_<EVector>(),       // LCN
					      arg1.as_<EVector>(),       // LCB
					      arg2.as_<EVector>(),       // A
					      arg3.as_<Box<Matrix>>());  // F
    return {Pr};
}

extern "C" closure builtin_function_peelBranchTowardRootNonEq(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);

    return substitution::peel_branch_toward_root_non_eq(arg0,        // LCN
							arg1,        // LCB
							arg2,        // A
							arg3);       // transition_P
}

extern "C" closure builtin_function_peelBranchAwayFromRootNonEq(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);

    return substitution::peel_branch_away_from_root_non_eq(arg0.as_<EVector>(),        // LCN
							   arg1.as_<EVector>(),        // LCB
							   arg2.as_<EVector>(),        // A
							   arg3.as_<EVector>(),        // transition_P
							   arg4.as_<Box<Matrix>>());   // F
}

extern "C" closure builtin_function_calcProbNonEq(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);

    log_double_t Pr = substitution::calc_prob_non_eq(arg0.as_<EVector>(),       // LCN
						     arg1.as_<EVector>(),       // LCB
						     arg2.as_<EVector>(),       // A
						     arg3.as_<Box<Matrix>>());  // F
    return {Pr};
}

extern "C" closure builtin_function_propagateFrequencies(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);

    object_ptr<Box<Matrix>> F2 = new Box<Matrix>(propagate_frequencies(arg0.as_<Box<Matrix>>(), // F
								       arg1.as_<EVector>()));   // transition_P
    return F2;
}

extern "C" closure builtin_function_sampleRootSequence(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);

    return substitution::sample_root_sequence(arg0.as_<EVector>(),      // LCN
                                              arg1.as_<EVector>(),      // LCB
                                              arg2.as_<EVector>(),      // As
                                              arg3.as_<Box<Matrix>>()); // F
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

    return substitution::sample_branch_sequence(arg0.as_<Vector<pair<int,int>>>(),     // parent_seq
						arg1.as_<Box<pairwise_alignment_t>>(), // parent_A
						arg2.as_<EVector>(),                   // LCN
						arg3.as_<EVector>(),                   // LCB
						arg4.as_<EVector>(),                   // A
						arg5.as_<EVector>(),                   // transition_P
						arg6.as_<Box<Matrix>>());              // F
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

extern "C" closure builtin_function_simulateSequenceFrom(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& parentSequence = arg0.as_<Vector<pair<int,int>>>();

    auto arg1 = Args.evaluate(1);
    auto& alignment = arg1.as_<Box<pairwise_alignment_t>>();

    auto arg2 = Args.evaluate(2);
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

        calc_transition_prob_from_parent(S, parent_model_state, transition_ps);

        sequence.push_back(sample(S));
    }

    return sequence;
}

extern "C" closure builtin_function_simulateFixedSequenceFrom(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& parentSequence = arg0.as_<Vector<pair<int,int>>>();

    auto arg1 = Args.evaluate(1);
    auto& transition_ps = arg1.as_<EVector>();

    auto arg2 = Args.evaluate(2);
    auto& F = arg2.as_<Box<Matrix>>();

    int L = parentSequence.size();

    Vector<pair<int,int>> sequence;
    auto S = F;
    for(int i=0; i<L; i++)
    {
        auto parent_model_state = parentSequence[i];

        calc_transition_prob_from_parent(S, parent_model_state, transition_ps);

        sequence.push_back(sample(S));
    }

    return sequence;
}

