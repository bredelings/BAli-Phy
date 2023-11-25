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

namespace substitution
{

    object_ptr<const Likelihood_Cache_Branch>
    peel_branch_SEV(const EVector& LCN,
                    const EVector& LCB,
                    const EVector& transition_P);

    object_ptr<const Likelihood_Cache_Branch>
    peel_branch_SEV2(const EVector& LCN,
		     const EVector& LCB,
		     const EVector& transition_P,
		     const EMaybe& f);

    object_ptr<const Likelihood_Cache_Branch>
    simple_sequence_likelihoods_SEV(const EPair& sequence_mask,
				    const alphabet& a,
				    const EVector& smap,
				    int n_models);

    Vector<pair<int,int>> sample_root_sequence_SEV(const EVector& LCN,
						   const EVector& LCB,
						   const Matrix& F,
						   const EVector& compressed_col_for_col);

    Vector<pair<int,int>> sample_sequence_SEV(const Vector<pair<int,int>>& parent_seq,
					      const EVector& LCN,
					      const EVector& transition_Ps,
					      const EVector& LCB,
					      const EVector& compressed_col_for_col);

    log_double_t calc_root_prob_SEV(const EVector& LCN,
				    const EVector& LCB,
				    const Matrix& F,
				    const EVector& counts);

    log_double_t calc_root_prob_SEV2(const EVector& LCN,
				     const EVector& LCB,
				     const EMaybe& FF,
				     const EVector& counts);
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
							 arg2.as_int());        // n_models
}

extern "C" closure builtin_function_peelBranchSEV(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);

    return substitution::peel_branch_SEV(arg0.as_<EVector>(),        // LCN
					 arg1.as_<EVector>(),        // LCB
					 arg2.as_<EVector>());       // transition_P
}

extern "C" closure builtin_function_peelBranchSEV2(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);

    return substitution::peel_branch_SEV2(arg0.as_<EVector>(),        // LCN
					  arg1.as_<EVector>(),        // LCB
					  arg2.as_<EVector>(),        // transition_P
					  arg3.as_<EMaybe>());
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

extern "C" closure builtin_function_calcRootProbSEV2(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);

    log_double_t Pr = substitution::calc_root_prob_SEV2(arg0.as_<EVector>(),       // sequences
							arg1.as_<EVector>(),       // LCB
							arg2.as_<EMaybe>(),        // FF
							arg3.as_<EVector>());      // counts
    return {Pr};
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
