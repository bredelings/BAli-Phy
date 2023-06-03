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

extern "C" closure builtin_function_unitCostMatrix(OperationArgs& Args)
{
    int N = Args.evaluate(0).as_int();

    auto U = new Box<matrix<int>>(N, N, 1);
    auto& u = *U;
    for(int i=0; i<N; i++)
        u(i,i) = 0;

    return U;
}

// peel_muts_leaf_branch :: EVector Int -> Alphabet -> MutCosts -> CondPars
object_ptr<const ParsimonyCacheBranch> peel_muts_leaf_branch(const alphabet& a, const EVector& letters, const matrix<int>& cost);

extern "C" closure builtin_function_peel_muts_leaf_branch(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0); // EVector Int
    auto arg1 = Args.evaluate(1); // Alphabet
    auto arg2 = Args.evaluate(2); // MutCosts

    return peel_muts_leaf_branch(*arg1.as_<Alphabet>(),
                                 arg0.as_<EVector>(),
                                 arg2.as_<Box<matrix<int>>>());
}

// peel_muts_internal_branch :: CondPars -> CondPars -> PairwiseAlignment -> PairwiseAlignment -> MutCosts -> CondPars
object_ptr<ParsimonyCacheBranch>
peel_muts_internal_branch(const pairwise_alignment_t& A0,
                          const pairwise_alignment_t& A1,
                          const ParsimonyCacheBranch n_muts0,
                          const ParsimonyCacheBranch n_muts1,
                          const matrix<int>& cost);

extern "C" closure builtin_function_peel_muts_internal_branch(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0); // CondPars
    auto arg1 = Args.evaluate(1); // CondPars
    auto arg2 = Args.evaluate(2); // PairwiseAlignment
    auto arg3 = Args.evaluate(3); // PairwiseAlignment
    auto arg4 = Args.evaluate(4); // MutCosts

    return peel_muts_internal_branch(arg2.as_<Box<pairwise_alignment_t>>(),
                                     arg3.as_<Box<pairwise_alignment_t>>(),
                                     arg0.as_<ParsimonyCacheBranch>(),
                                     arg1.as_<ParsimonyCacheBranch>(),
                                     arg4.as_<Box<matrix<int>>>());
}

// calc_root_muts :: CondPars -> CondPars -> CondPars -> PairwiseAlignment -> PairwiseAlignment -> PairwiseAlignment -> MutCosts -> Int
int muts_root(const pairwise_alignment_t& A0,
              const pairwise_alignment_t& A1,
              const pairwise_alignment_t& A2,
              const ParsimonyCacheBranch& n_muts1,
              const ParsimonyCacheBranch& n_muts2,
              const ParsimonyCacheBranch& n_muts3,
              const matrix<int>& cost);

extern "C" closure builtin_function_calc_root_muts(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0); // CondPars
    auto arg1 = Args.evaluate(1); // CondPars
    auto arg2 = Args.evaluate(2); // CondPars
    auto arg3 = Args.evaluate(3); // PairwiseAlignment
    auto arg4 = Args.evaluate(4); // PairwiseAlignment
    auto arg5 = Args.evaluate(5); // PairwiseAlignment
    auto arg6 = Args.evaluate(6); // MutCosts

    int muts = muts_root(arg3.as_<Box<pairwise_alignment_t>>(),
                         arg4.as_<Box<pairwise_alignment_t>>(),
                         arg5.as_<Box<pairwise_alignment_t>>(),
                         arg0.as_<ParsimonyCacheBranch>(),
                         arg1.as_<ParsimonyCacheBranch>(),
                         arg2.as_<ParsimonyCacheBranch>(),
                         arg6.as_<Box<matrix<int>>>());

    return {muts};
}

// calc_leaf_muts :: Alphabet -> EVector Int -> PairwiseAlignment -> MutCosts -> CondPars -> Int
int accumulate_root_leaf(const alphabet& a, const EVector& letters, const pairwise_alignment_t& A, const matrix<int>& cost, const ParsimonyCacheBranch& n_muts);



extern "C" closure builtin_function_calc_leaf_muts(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0); // Alphabet
    auto arg1 = Args.evaluate(1); // EVector Int
    auto arg2 = Args.evaluate(2); // PairwiseAlignment
    auto arg3 = Args.evaluate(3); // MutCosts
    auto arg4 = Args.evaluate(4); // CondPars

    int muts = accumulate_root_leaf(*arg0.as_<Alphabet>(),
                                    arg1.as_<EVector>(),
                                    arg2.as_<Box<pairwise_alignment_t>>(),
                                    arg3.as_<Box<matrix<int>>>(),
                                    arg4.as_<ParsimonyCacheBranch>());

    return {muts};
}


