#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
//#define DEBUG_RATE_MATRIX
#include "builtins/native-vector-input.hh"
#include "computation/machine/args.hh"
#include "sequence/alphabet.hh"
#include "substitution/ops.hh"
#include "substitution/likelihoodSEV.hh"
#include "util/myexception.hh"

using std::vector;
using std::istringstream;
using std::istream;
using std::valarray;

using std::cerr;
using std::endl;
using std::abs;

using Alphabet = PtrBox<alphabet>;

#include "substitution/cache.hh"
#include "dp/hmm.hh"
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

// Decode the smap from raw slots 1..3 while retaining the legacy sequence/mask pair in slot 5.
// The alphabet remains in slot 0 and the intervening model count moves to slot 4.
extern "C" closure builtin_function_simpleSequenceLikelihoods(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto smap_input = read_native_vector_input<int, ForeignDemand::use>(
        Args, 1, "LikelihoodSEV.simpleSequenceLikelihoods smap");
    auto arg4 = Args.evaluate_slot_to_value(4);
    auto arg5 = Args.evaluate_slot_to_value(5);

    return substitution::simple_sequence_likelihoods2_SEV(arg5,                 // sequence/bits
							  *arg0.as_<Alphabet>(), // alphabet
							  smap_input.view(),     // smap
							  arg4.as_int());        // n_models
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

    ProbDensity Pr = substitution::calc_prob_SEV(arg0.as_<R::RVector>(),       // sequences
						  arg1.as_<R::RVector>(),       // LCB
						  arg2.as_<Box<DenseMatrix<double>>>(),   // FF
						  counts.view());               // counts
    return new Box<ProbDensity>(Pr);
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

    ProbDensity Pr = substitution::calc_prob_at_root_SEV(arg0.as_<R::RVector>(),       // sequences
							  arg1.as_<R::RVector>(),       // LCB
							  arg2.as_<Box<DenseMatrix<double>>>(),   // F
							  counts);                      // counts
    return new Box<ProbDensity>(Pr);
}

extern "C" closure builtin_function_calcProbAtRootVariable(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    auto counts = read_native_vector_input<int, ForeignDemand::use>(
        Args, 3, "LikelihoodSEV.calcProbAtRootVariable");

    ProbDensity Pr = substitution::calc_prob_at_root_variable_SEV(arg0.as_<R::RVector>(),       // sequences
								   arg1.as_<R::RVector>(),       // LCB
								   arg2.as_<Box<DenseMatrix<double>>>(),   // F
								   counts.view());               // counts
    return new Box<ProbDensity>(Pr);
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
