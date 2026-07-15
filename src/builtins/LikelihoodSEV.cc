#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
//#define DEBUG_RATE_MATRIX
#include "builtins/native-vector-input.H"
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
    auto component_input =
        read_native_vector_input<int, ForeignDemand::use>(
            Args, 0, "LikelihoodSEV.sampleSequence components");
    auto state_input = read_native_vector_input<int, ForeignDemand::use>(
        Args, 3, "LikelihoodSEV.sampleSequence states");
    auto arg6 = Args.evaluate_slot_to_value(6);
    auto arg7 = Args.evaluate_slot_to_value(7);
    auto arg8 = Args.evaluate_slot_to_value(8);
    auto column_input = read_native_vector_input<int, ForeignDemand::use>(
        Args, 9, "LikelihoodSEV.sampleSequence columns");
    auto parent_components = component_input.view();
    auto parent_states = state_input.view();
    auto columns = column_input.view();
    if (parent_components.size() != parent_states.size())
        throw myexception()<<"LikelihoodSEV.sampleSequence: component and state lengths differ";
    if (parent_components.size() != columns.size())
        throw myexception()<<"LikelihoodSEV.sampleSequence: parent and column-map lengths differ";

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
    auto counts = read_native_vector_input<int, ForeignDemand::use>(
        Args, 3, "LikelihoodSEV.calcProb");

    log_double_t Pr = substitution::calc_prob_SEV(arg0.as_<R::RVector>(),       // sequences
						  arg1.as_<R::RVector>(),       // LCB
						  arg2.as_<Box<DenseMatrix<double>>>(),   // FF
						  counts.view());               // counts
    return {Pr};
}

extern "C" closure builtin_function_calcProbAtRoot(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    auto count_input = read_native_vector_input<int, ForeignDemand::use>(
        Args, 3, "LikelihoodSEV.calcProbAtRoot");
    auto count_view = count_input.view();
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
    auto counts = read_native_vector_input<int, ForeignDemand::use>(
        Args, 3, "LikelihoodSEV.calcProbAtRootVariable");

    log_double_t Pr = substitution::calc_prob_at_root_variable_SEV(arg0.as_<R::RVector>(),       // sequences
								   arg1.as_<R::RVector>(),       // LCB
								   arg2.as_<Box<DenseMatrix<double>>>(),   // F
								   counts.view());               // counts
    return {Pr};
}

extern "C" closure builtin_function_sampleRootSequence(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    auto columns = read_native_vector_input<int, ForeignDemand::use>(
        Args, 3, "LikelihoodSEV.sampleRootSequence columns");

    auto result = substitution::sample_root_sequence_SEV(
        arg0.as_<R::RVector>(),                    // LCN
        arg1.as_<R::RVector>(),                    // LCB
        arg2.as_<Box<DenseMatrix<double>>>(),       // F
        columns.view());                            // compressed_col_for_col
    return component_state_result(std::move(result));
}
