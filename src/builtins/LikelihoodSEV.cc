#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
//#define DEBUG_RATE_MATRIX
#include "computation/machine/args.H"
#include "sequence/alphabet.H"
#include "substitution/ops.H"
#include "substitution/likelihoodSEV.H"
#include "computation/expression/bool.H"

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

extern "C" closure builtin_function_simpleSequenceLikelihoodsSEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);

    return substitution::simple_sequence_likelihoods_SEV(arg3.as_<EPair>(),     // sequence/bits
							 *arg0.as_<Alphabet>(), // alphabet
							 arg1.as_<EVector>(),   // smap
							 arg2.as_int());        // n_models
}

extern "C" closure builtin_function_peelBranchTowardRootSEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);

    return substitution::peel_branch_toward_root_SEV(arg0.as_<EVector>(),        // LCN
						     arg1.as_<EVector>(),        // LCB
						     arg2.as_<EVector>());       // transition_P
}

extern "C" closure builtin_function_peelBranchSEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    bool arg4 = is_bool_true(Args.evaluate(4));

    return substitution::peel_branch_SEV(arg0.as_<EVector>(),        // LCN
					 arg1.as_<EVector>(),        // LCB
					 arg2.as_<EVector>(),        // transition_P
					 arg3.as_<Box<Matrix>>(),      
					 arg4);
}

extern "C" closure builtin_function_sampleSequenceSEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);

    return substitution::sample_sequence_SEV(arg0.as_<Vector<pair<int,int>>>(), // parent_seq,
					     arg1.as_<EVector>(),               // LCN
					     arg2.as_<EVector>(),               // transition_ps
					     arg3.as_<EVector>(),               // LCB
					     arg4.as_<EVector>());              // compressed_col_for_col
}

extern "C" closure builtin_function_calcProbSEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);

    log_double_t Pr = substitution::calc_prob_SEV(arg0.as_<EVector>(),       // sequences
						  arg1.as_<EVector>(),       // LCB
						  arg2.as_<Box<Matrix>>(),   // FF
						  arg3.as_<EVector>());      // counts
    return {Pr};
}

extern "C" closure builtin_function_calcProbAtRootSEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);

    log_double_t Pr = substitution::calc_prob_at_root_SEV(arg0.as_<EVector>(),       // sequences
							  arg1.as_<EVector>(),       // LCB
							  arg2.as_<Box<Matrix>>(),   // F
							  arg3.as_<EVector>());      // counts
    return {Pr};
}

extern "C" closure builtin_function_sampleRootSequenceSEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);

    return substitution::sample_root_sequence_SEV(arg0.as_<EVector>(),      // LCN
                                                  arg1.as_<EVector>(),      // LCB
                                                  arg2.as_<Box<Matrix>>(),  // F
                                                  arg3.as_<EVector>());     // compressed_col_for_col
}
