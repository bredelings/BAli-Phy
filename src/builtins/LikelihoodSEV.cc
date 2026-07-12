#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
//#define DEBUG_RATE_MATRIX
#include "computation/machine/args.H"
#include "sequence/alphabet.H"
#include "substitution/ops.H"
#include "substitution/likelihoodSEV.H"
#include "util/myexception.H"

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

extern "C" closure builtin_function_simpleSequenceLikelihoods(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    auto arg3 = Args.evaluate_slot_to_value(3);

    return substitution::simple_sequence_likelihoods2_SEV(arg3,                 // sequence/bits
							  *arg0.as_<Alphabet>(), // alphabet
							  arg1.as_<R::RVector>(),   // smap
							  arg2.as_int());        // n_models
}

extern "C" closure builtin_function_peelBranchTowardRoot(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);

    return substitution::peel_branch_toward_root_SEV(arg0.as_<R::RVector>(),        // LCN
						     arg1.as_<R::RVector>(),        // LCB
						     arg2.as_<R::RVector>());       // transition_P
}

extern "C" closure builtin_function_peelBranchAwayFromRoot(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    auto arg3 = Args.evaluate_slot_to_value(3);

    return substitution::peel_branch_away_from_root_SEV(arg0.as_<R::RVector>(),        // LCN
							arg1.as_<R::RVector>(),        // LCB
							arg2.as_<R::RVector>(),        // transition_P
							arg3.as_<Box<DenseMatrix<double>>>());   // WF
}

extern "C" closure builtin_function_sampleSequence(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    auto arg3 = Args.evaluate_slot_to_value(3);
    int offset = Args.evaluate_slot_to_value(4).as_int();
    int count = Args.evaluate_slot_to_value(5).as_int();
    auto owner_value = Args.evaluate_slot_to_value(6);
    const auto& owner = owner_value.as_<Box<DenseVector<int>>>();
    if (offset < 0 or count < 0 or offset > owner.size() or count > owner.size() - offset)
        throw myexception()<<"LikelihoodSEV.sampleSequence: invalid column-map view";
    auto columns = owner.segment(offset, count);

    return substitution::sample_sequence_SEV(arg0.as_<Vector<pair<int,int>>>(), // parent_seq,
					     arg1.as_<R::RVector>(),               // LCN
					     arg2.as_<R::RVector>(),               // transition_ps
					     arg3.as_<R::RVector>(),               // LCB
					     columns);                             // compressed_col_for_col
}

extern "C" closure builtin_function_calcProb(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    int offset = Args.evaluate_slot_to_value(3).as_int();
    int count = Args.evaluate_slot_to_value(4).as_int();
    auto owner_value = Args.evaluate_slot_to_value(5);
    const auto& owner = owner_value.as_<Box<DenseVector<int>>>();
    if (offset < 0 or count < 0 or offset > owner.size() or count > owner.size() - offset)
        throw myexception()<<"LikelihoodSEV.calcProb: invalid count-vector view";
    auto counts = owner.segment(offset, count);

    log_double_t Pr = substitution::calc_prob_SEV(arg0.as_<R::RVector>(),       // sequences
						  arg1.as_<R::RVector>(),       // LCB
						  arg2.as_<Box<DenseMatrix<double>>>(),   // FF
						  counts);                      // counts
    return {Pr};
}

extern "C" closure builtin_function_calcProbAtRoot(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    int offset = Args.evaluate_slot_to_value(3).as_int();
    int count = Args.evaluate_slot_to_value(4).as_int();
    auto owner_value = Args.evaluate_slot_to_value(5);
    const auto& owner = owner_value.as_<Box<DenseVector<int>>>();
    if (offset < 0 or count < 0 or offset > owner.size() or count > owner.size() - offset)
        throw myexception()<<"LikelihoodSEV.calcProbAtRoot: invalid count-vector view";
    auto counts = owner.segment(offset, count);

    log_double_t Pr = substitution::calc_prob_at_root_SEV(arg0.as_<R::RVector>(),       // sequences
							  arg1.as_<R::RVector>(),       // LCB
							  arg2.as_<Box<DenseMatrix<double>>>(),   // F
							  counts);                      // counts
    return {Pr};
}

extern "C" closure builtin_function_calcProbAtRootVariable(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    int offset = Args.evaluate_slot_to_value(3).as_int();
    int count = Args.evaluate_slot_to_value(4).as_int();
    auto owner_value = Args.evaluate_slot_to_value(5);
    const auto& owner = owner_value.as_<Box<DenseVector<int>>>();
    if (offset < 0 or count < 0 or offset > owner.size() or count > owner.size() - offset)
        throw myexception()<<"LikelihoodSEV.calcProbAtRootVariable: invalid count-vector view";
    auto counts = owner.segment(offset, count);

    log_double_t Pr = substitution::calc_prob_at_root_variable_SEV(arg0.as_<R::RVector>(),       // sequences
								   arg1.as_<R::RVector>(),       // LCB
								   arg2.as_<Box<DenseMatrix<double>>>(),   // F
								   counts);                      // counts
    return {Pr};
}

extern "C" closure builtin_function_sampleRootSequence(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    int offset = Args.evaluate_slot_to_value(3).as_int();
    int count = Args.evaluate_slot_to_value(4).as_int();
    auto owner_value = Args.evaluate_slot_to_value(5);
    const auto& owner = owner_value.as_<Box<DenseVector<int>>>();
    if (offset < 0 or count < 0 or offset > owner.size() or count > owner.size() - offset)
        throw myexception()<<"LikelihoodSEV.sampleRootSequence: invalid column-map view";
    auto columns = owner.segment(offset, count);

    return substitution::sample_root_sequence_SEV(arg0.as_<R::RVector>(),      // LCN
                                                  arg1.as_<R::RVector>(),      // LCB
                                                  arg2.as_<Box<DenseMatrix<double>>>(),  // F
						  columns);                    // compressed_col_for_col
}
