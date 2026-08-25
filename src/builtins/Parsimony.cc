#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
//#define DEBUG_RATE_MATRIX
#include "builtins/native-vector-input.hh"
#include "computation/machine/args.hh"
#include "math/exponential.hh"
#include "sequence/alphabet.hh"
#include "sequence/doublets.hh"
#include "sequence/RNAEdits.hh"
#include "sequence/codons.hh"
#include "util/io.hh"
#include <valarray>
#include "dp/2way.hh"
#include "util/range.hh"
#include <unsupported/Eigen/MatrixFunctions>
#include "substitution/parsimony.hh"
#include "tools/parsimony.hh"
#include "util/myexception.hh"
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
using Ambiguities = Box<ambiguity_database>;

// the only one we're not using is `amino_acid_cost_matrix(*C)`.

extern "C" closure builtin_function_unitCostMatrix(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    const alphabet& a = *arg0.as_<Alphabet>();

    int N = a.size();
    auto U = new Box<DenseMatrix<int>>(N, N);

    if (auto T = arg0.poly_cast<alphabet,Triplets>())
	*U = nucleotide_cost_matrix(*T);
    else if (auto D = arg0.poly_cast<alphabet,Doublets>())
	*U = nucleotide_cost_matrix(*D);
    else if (auto E = arg0.poly_cast<alphabet,RNAEdits>())
	*U = pos1_cost_matrix(*E);
    else
        *U = unit_cost_matrix(a);

    return U;
}

extern "C" closure builtin_function_aminoAcidCostMatrix(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    const alphabet& a = *arg0.as_<Alphabet>();

    int N = a.size();
    auto U = new Box<DenseMatrix<int>>(N, N);

    if (auto C = arg0.poly_cast<alphabet,Codons>())
	*U = amino_acid_cost_matrix(*C);
    else
	throw myexception()<<"Can't compute an amino-acid cost matrix for non-Codon alphabet '"<<a.name<<"'";

    return U;
}

extern "C" closure builtin_function_pos1CostMatrix(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    const alphabet& a = *arg0.as_<Alphabet>();

    int N = a.size();
    auto U = new Box<DenseMatrix<int>>(N, N);

    if (auto E = arg0.poly_cast<alphabet,RNAEdits>())
	*U = pos1_cost_matrix(*E);
    else
	throw myexception()<<"Can't compute a position-1 cost matrix for non-Doublets alphabet '"<<a.name<<"'";

    return U;
}

extern "C" closure builtin_function_pos2CostMatrix(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    const alphabet& a = *arg0.as_<Alphabet>();

    int N = a.size();
    auto U = new Box<DenseMatrix<int>>(N, N);

    if (auto E = arg0.poly_cast<alphabet,RNAEdits>())
	*U = pos2_cost_matrix(*E);
    else
	throw myexception()<<"Can't compute a position-2 cost matrix for non-Doublets alphabet '"<<a.name<<"'";

    return U;
}


object_ptr<ParsimonyCacheBranch>
peel_muts(const R::RVector& sequences, const alphabet& a, const ambiguity_database& ambiguities,
          const R::RVector& A, const R::RVector& n_muts, const DenseMatrix<int>& cost);

extern "C" closure builtin_function_peelMuts(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0); // R::RVector Int
    auto arg1 = Args.evaluate_slot_to_value(1); // Alphabet
    auto arg2 = Args.evaluate_slot_to_value(2); // Ambiguities
    auto arg3 = Args.evaluate_slot_to_value(3); // alignments
    auto arg4 = Args.evaluate_slot_to_value(4); // partials
    auto arg5 = Args.evaluate_slot_to_value(5); // costs

    return peel_muts(arg0.as_<R::RVector>(),    // sequences
		     *arg1.as_<Alphabet>(),  // alphabet
		     arg2.as_<Ambiguities>(), // ambiguities
		     arg3.as_<R::RVector>(),  // A
		     arg4.as_<R::RVector>(),  // n_muts
		     arg5.as_<Box<DenseMatrix<int>>>());
}

int muts_root(const R::RVector& sequences, const alphabet& a, const ambiguity_database& ambiguities,
              const R::RVector& A, const R::RVector& n_muts, const DenseMatrix<int>& cost);

extern "C" closure builtin_function_mutsRoot(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0); // sequences
    auto arg1 = Args.evaluate_slot_to_value(1); // alphabet
    auto arg2 = Args.evaluate_slot_to_value(2); // ambiguities
    auto arg3 = Args.evaluate_slot_to_value(3); // A
    auto arg4 = Args.evaluate_slot_to_value(4); // n_muts
    auto arg5 = Args.evaluate_slot_to_value(5); // cost

    int muts = muts_root(arg0.as_<R::RVector>(),
			 *arg1.as_<Alphabet>(),
			 arg2.as_<Ambiguities>(),
			 arg3.as_<R::RVector>(),
			 arg4.as_<R::RVector>(),
			 arg5.as_<Box<DenseMatrix<int>>>());

    return {muts};
}

object_ptr<const ParsimonyCacheBranch>
peel_muts_fixed_A(const R::RVector& sequences, const alphabet& a, const ambiguity_database& ambiguities,
                  const R::RVector& n_muts_, const DenseMatrix<int>& cost);

extern "C" closure builtin_function_peelMutsFixedA(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    auto arg3 = Args.evaluate_slot_to_value(3);
    auto arg4 = Args.evaluate_slot_to_value(4);

    return peel_muts_fixed_A(arg0.as_<R::RVector>(),            // sequences
			     *arg1.as_<Alphabet>(),          // a
			     arg2.as_<Ambiguities>(),         // ambiguities
			     arg3.as_<R::RVector>(),          // n_muts_
			     arg4.as_<Box<DenseMatrix<int>>>()); // cost
}


int muts_root_fixed_A(const R::RVector& sequences, const alphabet& a,
                      const ambiguity_database& ambiguities,
                      const R::RVector& n_muts, const DenseMatrix<int>& costs,
                      std::span<const int> counts);

extern "C" closure builtin_function_mutsRootFixedA(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto arg1 = Args.evaluate_slot_to_value(1);
    auto arg2 = Args.evaluate_slot_to_value(2);
    auto arg3 = Args.evaluate_slot_to_value(3);
    auto arg4 = Args.evaluate_slot_to_value(4);
    auto counts = read_native_vector_input<int, ForeignDemand::use>(
        Args, 5, "Parsimony.mutsRootFixedA");

    int muts = muts_root_fixed_A(arg0.as_<R::RVector>(),            // sequences
				 *arg1.as_<Alphabet>(),          // a
				 arg2.as_<Ambiguities>(),         // ambiguities
				 arg3.as_<R::RVector>(),          // n_muts_
				 arg4.as_<Box<DenseMatrix<int>>>(), // cost
				 counts.view());                     // counts

    return { muts };
}
