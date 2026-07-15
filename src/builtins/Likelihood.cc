#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
//#define DEBUG_RATE_MATRIX
#include "builtins/native-vector-input.H"
#include "computation/machine/args.H"
#include "sequence/alphabet.H"
#include "dp/2way.H"
#include "substitution/ops.H"
#include "substitution/likelihood.H"
#include "util/myexception.H"

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

namespace
{

// Move a sampled structure-of-arrays result into the two native owners used
// by Data.Vector.Unboxed without copying either array.
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

extern "C" closure builtin_function_bitmaskFromSequence(OperationArgs& Args)
{
    using boost::dynamic_bitset;

    auto arg0 = Args.evaluate_slot_to_value(0);
    const auto& seq = arg0. as_<R::RVector>();

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

    auto arg0 = Args.evaluate_slot_to_value(0);
    const auto& seq1 = arg0. as_<R::RVector>();

    int L = seq1.size();

    object_ptr<R::RVector> Seq2(new R::RVector);
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
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    auto arg3 = Args.evaluate_slot_to_value(3);

    return substitution::simple_sequence_likelihoods2(arg3.as_<R::RVector>(),   // sequence
                                                      *arg0.as_<Alphabet>(), // alphabet
                                                      arg1.as_<R::RVector>(),   // smap
                                                      arg2.as_int());        // n_models
}


extern "C" closure builtin_function_peelBranchTowardRoot(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    auto arg3 = Args.evaluate_slot_to_value(3);
    auto arg4 = Args.evaluate_slot_to_value(4);

    return substitution::peel_branch_toward_root(arg0.as_<R::RVector>(),        // LCN
						 arg1.as_<R::RVector>(),        // LCB
						 arg2.as_<R::RVector>(),        // A
						 arg3.as_<R::RVector>(),        // transition_P
						 arg4.as_<Box<DenseMatrix<double>>>()  ); // F
}

extern "C" closure builtin_function_calcProbAtRoot(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    auto arg3 = Args.evaluate_slot_to_value(3);

    log_double_t Pr = substitution::calc_prob_at_root(arg0.as_<R::RVector>(),       // LCN
						      arg1.as_<R::RVector>(),       // LCB
						      arg2.as_<R::RVector>(),       // A
						      arg3.as_<Box<DenseMatrix<double>>>());  // F
    return {Pr};
}

extern "C" closure builtin_function_peelBranchAwayFromRoot(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    auto arg3 = Args.evaluate_slot_to_value(3);
    auto arg4 = Args.evaluate_slot_to_value(4);

    return substitution::peel_branch_away_from_root(arg0.as_<R::RVector>(),        // LCN
						    arg1.as_<R::RVector>(),        // LCB
						    arg2.as_<R::RVector>(),        // A
						    arg3.as_<R::RVector>(),        // transition_P
						    arg4.as_<Box<DenseMatrix<double>>>());   // F
}

extern "C" closure builtin_function_calcProb(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    auto arg3 = Args.evaluate_slot_to_value(3);

    log_double_t Pr = substitution::calc_prob(arg0.as_<R::RVector>(),       // LCN
					      arg1.as_<R::RVector>(),       // LCB
					      arg2.as_<R::RVector>(),       // A
					      arg3.as_<Box<DenseMatrix<double>>>());  // F
    return {Pr};
}

extern "C" closure builtin_function_peelBranchTowardRootNonEq(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    auto arg3 = Args.evaluate_slot_to_value(3);

    return substitution::peel_branch_toward_root_non_eq(arg0,        // LCN
							arg1,        // LCB
							arg2,        // A
							arg3);       // transition_P
}

extern "C" closure builtin_function_peelBranchAwayFromRootNonEq(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    auto arg3 = Args.evaluate_slot_to_value(3);
    auto arg4 = Args.evaluate_slot_to_value(4);

    return substitution::peel_branch_away_from_root_non_eq(arg0.as_<R::RVector>(),        // LCN
							   arg1.as_<R::RVector>(),        // LCB
							   arg2.as_<R::RVector>(),        // A
							   arg3.as_<R::RVector>(),        // transition_P
							   arg4.as_<Box<DenseMatrix<double>>>());   // F
}

extern "C" closure builtin_function_calcProbNonEq(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    auto arg3 = Args.evaluate_slot_to_value(3);

    log_double_t Pr = substitution::calc_prob_non_eq(arg0.as_<R::RVector>(),       // LCN
						     arg1.as_<R::RVector>(),       // LCB
						     arg2.as_<R::RVector>(),       // A
						     arg3.as_<Box<DenseMatrix<double>>>());  // F
    return {Pr};
}

extern "C" closure builtin_function_propagateFrequencies(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);

    object_ptr<Box<DenseMatrix<double>>> F2 = new Box<DenseMatrix<double>>(propagate_frequencies(arg0.as_<Box<DenseMatrix<double>>>(), // F
								       arg1.as_<R::RVector>()));   // transition_P
    return F2;
}

extern "C" closure builtin_function_sampleRootSequence(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    auto arg3 = Args.evaluate_slot_to_value(3);

    auto result = substitution::sample_root_sequence(arg0.as_<R::RVector>(),      // LCN
                                                     arg1.as_<R::RVector>(),      // LCB
                                                     arg2.as_<R::RVector>(),      // As
                                                     arg3.as_<Box<DenseMatrix<double>>>()); // F
    return component_state_result(std::move(result));
}

extern "C" closure builtin_function_sampleBranchSequence(OperationArgs& Args)
{
    auto component_input =
        read_native_vector_input<int, ForeignDemand::use>(
            Args, 0, "Likelihood.sampleBranchSequence components");
    auto state_input = read_native_vector_input<int, ForeignDemand::use>(
        Args, 3, "Likelihood.sampleBranchSequence states");
    auto arg6 = Args.evaluate_slot_to_value(6);
    auto arg7 = Args.evaluate_slot_to_value(7);
    auto arg8 = Args.evaluate_slot_to_value(8);
    auto arg9 = Args.evaluate_slot_to_value(9);
    auto arg10 = Args.evaluate_slot_to_value(10);
    auto arg11 = Args.evaluate_slot_to_value(11);
    auto parent_components = component_input.view();
    auto parent_states = state_input.view();
    if (parent_components.size() != parent_states.size())
        throw myexception()<<"Likelihood.sampleBranchSequence: component and state lengths differ";
    const auto& parent_alignment = arg6.as_<Box<pairwise_alignment_t>>();
    if (parent_components.size() !=
        static_cast<std::size_t>(parent_alignment.length1()))
        throw myexception()<<"Likelihood.sampleBranchSequence: parent length does not match alignment";
    auto result = substitution::sample_branch_sequence(
        parent_components, parent_states,
	parent_alignment,                      // parent_A
	arg7.as_<R::RVector>(),                // LCN
	arg8.as_<R::RVector>(),                // LCB
	arg9.as_<R::RVector>(),                // A
	arg10.as_<R::RVector>(),               // transition_P
	arg11.as_<Box<DenseMatrix<double>>>()); // F
    return component_state_result(std::move(result));
}

// maskSequenceRaw :: CBitVector -> R::RVector Int -> R::RVector Int
extern "C" closure builtin_function_maskSequenceRaw(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& mask = arg0.as_<Box<dynamic_bitset<>>>();

    auto arg1 = Args.evaluate_slot_to_value(1);
    auto sequence = arg1.as_<R::RVector>();

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
    int L = Args.evaluate_slot_to_value(0).as_int();
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto& F = arg1.as_<Box<DenseMatrix<double>>>();

    ComponentStateVectors sequence(L);
    for(int i=0;i<L;i++)
    {
        auto [component, state] = sample(F);
        sequence.components[i] = component;
        sequence.states[i] = state;
    }
    return component_state_result(std::move(sequence));
}

extern "C" closure builtin_function_simulateSequenceFrom(OperationArgs& Args)
{
    auto component_input =
        read_native_vector_input<int, ForeignDemand::use>(
            Args, 0, "Likelihood.simulateSequenceFrom components");
    auto state_input = read_native_vector_input<int, ForeignDemand::use>(
        Args, 3, "Likelihood.simulateSequenceFrom states");
    auto alignment_value = Args.evaluate_slot_to_value(6);
    auto transition_value = Args.evaluate_slot_to_value(7);
    auto frequency_value = Args.evaluate_slot_to_value(8);
    auto parent_components = component_input.view();
    auto parent_states = state_input.view();
    if (parent_components.size() != parent_states.size())
        throw myexception()<<"Likelihood.simulateSequenceFrom: component and state lengths differ";
    const auto& alignment = alignment_value.as_<Box<pairwise_alignment_t>>();
    if (parent_components.size() !=
        static_cast<std::size_t>(alignment.length1()))
        throw myexception()<<"Likelihood.simulateSequenceFrom: parent length does not match alignment";
    const auto& transition_ps = transition_value.as_<R::RVector>();
    const auto& F = frequency_value.as_<Box<DenseMatrix<double>>>();

    ComponentStateVectors sequence(alignment.length2());
    auto S = F;
    int j = 0;
    int k = 0;
    for(int i=0;i<alignment.size();i++)
    {
        pair<int,int> parent_model_state(-2,-2);
        if (alignment.is_delete(i))
        {
            j++;
            continue;
        }
        else if (alignment.is_match(i))
        {
            parent_model_state = {parent_components[j], parent_states[j]};
            j++;
        }
        else if (alignment.is_insert(i))
            parent_model_state = sample(F);

        calc_transition_prob_from_parent(S, parent_model_state, transition_ps);

        auto [component, state] = sample(S);
        sequence.components[k] = component;
        sequence.states[k++] = state;
    }

    assert(static_cast<std::size_t>(j) == parent_components.size());
    assert(k == alignment.length2());

    return component_state_result(std::move(sequence));
}

extern "C" closure builtin_function_simulateFixedSequenceFrom(OperationArgs& Args)
{
    auto component_input =
        read_native_vector_input<int, ForeignDemand::use>(
            Args, 0, "Likelihood.simulateFixedSequenceFrom components");
    auto state_input = read_native_vector_input<int, ForeignDemand::use>(
        Args, 3, "Likelihood.simulateFixedSequenceFrom states");
    auto transition_value = Args.evaluate_slot_to_value(6);
    auto frequency_value = Args.evaluate_slot_to_value(7);
    auto parent_components = component_input.view();
    auto parent_states = state_input.view();
    if (parent_components.size() != parent_states.size())
        throw myexception()<<"Likelihood.simulateFixedSequenceFrom: component and state lengths differ";
    const auto& transition_ps = transition_value.as_<R::RVector>();
    const auto& F = frequency_value.as_<Box<DenseMatrix<double>>>();
    int parent_count = static_cast<int>(parent_components.size());

    ComponentStateVectors sequence(parent_count);
    auto S = F;
    for(int i=0; i<parent_count; i++)
    {
        pair<int,int> parent_model_state{parent_components[i], parent_states[i]};

        calc_transition_prob_from_parent(S, parent_model_state, transition_ps);

        auto [component, state] = sample(S);
        sequence.components[i] = component;
        sequence.states[i] = state;
    }

    return component_state_result(std::move(sequence));
}
