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
#include "tools/parsimony.H"
#include <boost/dynamic_bitset.hpp>

using std::vector;
using std::pair;
using std::istringstream;
using std::istream;
using std::valarray;

using std::cerr;
using std::endl;
using std::abs;

using boost::dynamic_bitset;

using Alphabet = PtrBox<alphabet>;

// the only one we're not using is `amino_acid_cost_matrix(*C)`.

extern "C" closure builtin_function_unitCostMatrix(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    const alphabet& a = *arg0.as_<Alphabet>();

    int N = a.size();
    auto U = new Box<matrix<int>>(N, N, 1);

    if (auto T = arg0.poly_cast<alphabet,Triplets>())
	*U = nucleotide_cost_matrix(*T);
    else if (auto D = arg0.poly_cast<alphabet,Doublets>())
	*U = nucleotide_cost_matrix(*D);
    else
        *U = unit_cost_matrix(a);

    return U;
}


object_ptr<ParsimonyCacheBranch>
peel_muts(const EVector& sequences, const alphabet& a, const EVector& A, const EVector& n_muts, const matrix<int>& cost);

extern "C" closure builtin_function_peelMuts(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0); // EVector Int
    auto arg1 = Args.evaluate(1); // Alphabet
    auto arg2 = Args.evaluate(2); // MutCosts
    auto arg3 = Args.evaluate(3); // n_
    auto arg4 = Args.evaluate(4); // MutCosts

    return peel_muts(arg0.as_<EVector>(),    // sequences
		     *arg1.as_<Alphabet>(),  // alphabet
		     arg2.as_<EVector>(),    // A
		     arg3.as_<EVector>(),    // n_muts
		     arg4.as_<Box<matrix<int>>>());
}

int muts_root(const EVector& sequences, const alphabet& a, const EVector& A, const EVector& n_muts, const matrix<int>& cost);

extern "C" closure builtin_function_mutsRoot(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0); // sequences
    auto arg1 = Args.evaluate(1); // alphabet
    auto arg2 = Args.evaluate(2); // A
    auto arg3 = Args.evaluate(3); // n_muts
    auto arg4 = Args.evaluate(4); // cost

    int muts = muts_root(arg0.as_<EVector>(),
			 *arg1.as_<Alphabet>(),
			 arg2.as_<EVector>(),
			 arg3.as_<EVector>(),
			 arg4.as_<Box<matrix<int>>>());

    return {muts};
}

object_ptr<const ParsimonyCacheBranch>
peel_muts_fixed_A(const EVector& sequences, const alphabet& a, const EVector& n_muts_, const matrix<int>& cost);

extern "C" closure builtin_function_peelMutsFixedA(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);

    return peel_muts_fixed_A(arg0.as_<EVector>(),            // sequences
			     *arg1.as_<Alphabet>(),          // a
			     arg2.as_<EVector>(),            // n_muts_
			     arg3.as_<Box<matrix<int>>>());  // cost
}


int muts_root_fixed_A(const EVector& sequences, const alphabet& a, const EVector& n_muts, const matrix<int>& costs, const EVector& counts);

extern "C" closure builtin_function_mutsRootFixedA(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto arg1 = Args.evaluate(1);
    auto arg2 = Args.evaluate(2);
    auto arg3 = Args.evaluate(3);
    auto arg4 = Args.evaluate(4);

    int muts = muts_root_fixed_A(arg0.as_<EVector>(),            // sequences
				 *arg1.as_<Alphabet>(),          // a
				 arg2.as_<EVector>(),            // n_muts_
				 arg3.as_<Box<matrix<int>>>(),   // cost
				 arg4.as_<EVector>());           // counts

    return { muts };
}


// peel_muts_leaf_branch_fixed_A :: EVector Int -> CBitVector -> Alphabet -> MutCosts -> CondPars
object_ptr<const ParsimonyCacheBranch>
peel_muts_leaf_branch_fixed_A(const alphabet& a, const EVector& seq, const dynamic_bitset<>& mask, const matrix<int>& cost);

extern "C" closure builtin_function_peel_muts_leaf_branch_fixed_A(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0); // EVector Int
    auto arg1 = Args.evaluate(1); // CBitVector
    auto arg2 = Args.evaluate(2); // Alphabet
    auto arg3 = Args.evaluate(3); // MutCosts

    return peel_muts_leaf_branch_fixed_A(*arg2.as_<Alphabet>(),
                                         arg0.as_<EVector>(),
                                         arg1.as_<Box<dynamic_bitset<>>>(),
                                         arg3.as_<Box<matrix<int>>>());
}



// peel_muts_internal_branch :: CondPars -> CondPars -> MutCosts -> CondPars
object_ptr<const ParsimonyCacheBranch> peel_muts_internal_branch_fixed_A(const ParsimonyCacheBranch& n_muts0, const ParsimonyCacheBranch& n_muts1, const matrix<int>& cost);

extern "C" closure builtin_function_peel_muts_internal_branch_fixed_A(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0); // CondPars
    auto arg1 = Args.evaluate(1); // CondPars
    auto arg2 = Args.evaluate(2); // MutCosts

    return peel_muts_internal_branch_fixed_A(arg0.as_<ParsimonyCacheBranch>(),
                                             arg1.as_<ParsimonyCacheBranch>(),
                                             arg2.as_<Box<matrix<int>>>());
}

// peel_muts_internal_branch :: CondPars -> CondPars -> CondPars -> MutCosts -> EVector Int -> Int
int muts_root_fixed_A(const ParsimonyCacheBranch& n_muts1, const ParsimonyCacheBranch& n_muts2, const ParsimonyCacheBranch& n_muts3, const matrix<int>& cost, const EVector& counts);

extern "C" closure builtin_function_calc_root_muts_fixed_A(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0); // CondPars
    auto arg1 = Args.evaluate(1); // CondPars
    auto arg2 = Args.evaluate(2); // CondPars
    auto arg3 = Args.evaluate(3); // MutCosts
    auto arg4 = Args.evaluate(4); // EVector Int

    int muts = muts_root_fixed_A(arg0.as_<ParsimonyCacheBranch>(),
                                 arg1.as_<ParsimonyCacheBranch>(),
                                 arg2.as_<ParsimonyCacheBranch>(),
                                 arg3.as_<Box<matrix<int>>>(),
                                 arg4.as_<EVector>());

    return {muts};
}

// calc_leaf_muts :: Alphabet -> EVector Int -> CBitVector -> CondPars -> MutCosts -> EVector Int -> Int
int accumulate_root_leaf_fixed_A(const alphabet& a, const EVector& root_seq, const dynamic_bitset<>& root_mask, const ParsimonyCacheBranch& n_muts, const matrix<int>& cost, const EVector& counts);

extern "C" closure builtin_function_calc_leaf_muts_fixed_A(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0); // Alphabet
    auto arg1 = Args.evaluate(1); // EVector Int
    auto arg2 = Args.evaluate(2); // CBitVector
    auto arg3 = Args.evaluate(3); // CondPars
    auto arg4 = Args.evaluate(4); // MutCosts
    auto arg5 = Args.evaluate(5); // EVector Int;

    int muts = accumulate_root_leaf_fixed_A(*arg0.as_<Alphabet>(),
                                            arg1.as_<EVector>(),
                                            arg2.as_<Box<dynamic_bitset<>>>(),
                                            arg3.as_<ParsimonyCacheBranch>(),
                                            arg4.as_<Box<matrix<int>>>(),
                                            arg5.as_<EVector>());

    return {muts};
}

