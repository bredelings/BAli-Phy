#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
//#define DEBUG_RATE_MATRIX
#include "computation/machine/args.H"
#include "sequence/alphabet.H"
#include "substitution/ops.H"
#include "substitution/likelihoodSEV.H"
#include "util/myexception.H"

using std::vector;
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

namespace
{

// Validate one Haskell primitive view after all arguments are retained and
// expose its contiguous logical range without per-element checks.
std::span<const int> native_int_view(const R::Exp& value, int offset, int count,
                                     const char* operation)
{
    const auto& owner = value.as_<Box<DenseVector<int>>>();
    if (offset < 0 or count < 0 or offset > owner.size() or count > owner.size() - offset)
        throw myexception()<<operation<<": invalid native Int vector view";
    std::span<const int> storage(owner.data(), static_cast<std::size_t>(owner.size()));
    return storage.subspan(static_cast<std::size_t>(offset),
                           static_cast<std::size_t>(count));
}

// Move a sampled structure-of-arrays result into two unboxed-vector owners
// without copying either primitive array.
closure component_state_result(ComponentStateVectors values)
{
    assert(values.components.size() == values.states.size());
    object_ptr<Box<DenseVector<int>>> components(
        new Box<DenseVector<int>>(std::move(values.components)));
    object_ptr<Box<DenseVector<int>>> states(
        new Box<DenseVector<int>>(std::move(values.states)));
    return R::RPair(components, states);
}

}

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
    int parent_count = Args.evaluate_slot_to_value(0).as_int();
    int component_offset = Args.evaluate_slot_to_value(1).as_int();
    auto component_value = Args.evaluate_slot_to_value(2);
    int state_offset = Args.evaluate_slot_to_value(3).as_int();
    auto state_value = Args.evaluate_slot_to_value(4);
    auto arg5 = Args.evaluate_slot_to_value(5);
    auto arg6 = Args.evaluate_slot_to_value(6);
    auto arg7 = Args.evaluate_slot_to_value(7);
    int column_offset = Args.evaluate_slot_to_value(8).as_int();
    int column_count = Args.evaluate_slot_to_value(9).as_int();
    auto column_value = Args.evaluate_slot_to_value(10);
    if (parent_count != column_count)
        throw myexception()<<"LikelihoodSEV.sampleSequence: parent and column-map lengths differ";
    auto parent_components = native_int_view(
        component_value, component_offset, parent_count,
        "LikelihoodSEV.sampleSequence components");
    auto parent_states = native_int_view(
        state_value, state_offset, parent_count,
        "LikelihoodSEV.sampleSequence states");
    auto columns = native_int_view(
        column_value, column_offset, column_count,
        "LikelihoodSEV.sampleSequence columns");

    auto result = substitution::sample_sequence_SEV(
        parent_components, parent_states,
	arg5.as_<R::RVector>(), // LCN
	arg6.as_<R::RVector>(), // transition_ps
	arg7.as_<R::RVector>(), // LCB
	columns);
    return component_state_result(std::move(result));
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
    auto columns = native_int_view(
        owner_value, offset, count,
        "LikelihoodSEV.sampleRootSequence columns");

    auto result = substitution::sample_root_sequence_SEV(
        arg0.as_<R::RVector>(),                    // LCN
        arg1.as_<R::RVector>(),                    // LCB
        arg2.as_<Box<DenseMatrix<double>>>(),       // F
        columns);                                   // compressed_col_for_col
    return component_state_result(std::move(result));
}
