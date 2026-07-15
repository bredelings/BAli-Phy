#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
//#define DEBUG_RATE_MATRIX
#include "builtins/native-vector-view.H"
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
    int component_offset = Args.evaluate_slot_to_value(0).as_int();
    int component_count = Args.evaluate_slot_to_value(1).as_int();
    auto component_value = Args.evaluate_slot_to_value(2);
    int state_offset = Args.evaluate_slot_to_value(3).as_int();
    int state_count = Args.evaluate_slot_to_value(4).as_int();
    auto state_value = Args.evaluate_slot_to_value(5);
    auto arg6 = Args.evaluate_slot_to_value(6);
    auto arg7 = Args.evaluate_slot_to_value(7);
    auto arg8 = Args.evaluate_slot_to_value(8);
    int column_offset = Args.evaluate_slot_to_value(9).as_int();
    int column_count = Args.evaluate_slot_to_value(10).as_int();
    auto column_value = Args.evaluate_slot_to_value(11);
    if (component_count != state_count)
        throw myexception()<<"LikelihoodSEV.sampleSequence: component and state lengths differ";
    if (component_count != column_count)
        throw myexception()<<"LikelihoodSEV.sampleSequence: parent and column-map lengths differ";
    const auto& component_owner = component_value.as_<Box<DenseVector<int>>>();
    const auto& state_owner = state_value.as_<Box<DenseVector<int>>>();
    const auto& column_owner = column_value.as_<Box<DenseVector<int>>>();
    auto parent_components = checked_native_vector_view(
        component_owner, component_offset, component_count,
        "LikelihoodSEV.sampleSequence components");
    auto parent_states = checked_native_vector_view(
        state_owner, state_offset, state_count,
        "LikelihoodSEV.sampleSequence states");
    auto columns = checked_native_vector_view(
        column_owner, column_offset, column_count,
        "LikelihoodSEV.sampleSequence columns");

    auto result = substitution::sample_sequence_SEV(
        parent_components, parent_states,
	arg6.as_<R::RVector>(), // LCN
	arg7.as_<R::RVector>(), // transition_ps
	arg8.as_<R::RVector>(), // LCB
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
    auto counts = checked_native_vector_view(
        owner, offset, count, "LikelihoodSEV.calcProb");

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
    auto count_view = checked_native_vector_view(
        owner, offset, count, "LikelihoodSEV.calcProbAtRoot");
    Eigen::Map<const DenseVector<int>> counts(
        count_view.data(), static_cast<Eigen::Index>(count_view.size()));

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
    auto counts = checked_native_vector_view(
        owner, offset, count, "LikelihoodSEV.calcProbAtRootVariable");

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
    auto columns = checked_native_vector_view(
        owner, offset, count,
        "LikelihoodSEV.sampleRootSequence columns");

    auto result = substitution::sample_root_sequence_SEV(
        arg0.as_<R::RVector>(),                    // LCN
        arg1.as_<R::RVector>(),                    // LCB
        arg2.as_<Box<DenseMatrix<double>>>(),       // F
        columns);                                   // compressed_col_for_col
    return component_state_result(std::move(result));
}
