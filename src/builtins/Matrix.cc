#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
//#define DEBUG_RATE_MATRIX
#include "computation/machine/args.H"
#include "math/exponential.H"
#include "sequence/alphabet.H"
#include "sequence/doublets.H"
#include "sequence/codons.H"
#include "util/io.H"
#include "util/dense-matrix.H"
#include <valarray>
#include "dp/2way.H"
#include "util/range.H"
#include "substitution/parsimony.H"
#include <algorithm>
#include <limits>
#include <type_traits>

using std::vector;
using std::pair;
using std::istringstream;
using std::istream;
using std::valarray;

using std::cerr;
using std::endl;
using std::abs;

using Alphabet = PtrBox<alphabet>;

namespace
{

// Dispatch an operation to one of the native matrix representations supported
// by the Haskell Matrix type.
template <typename F>
decltype(auto) visit_matrix(const R::Exp& value, F&& operation)
{
    if (value.is_a<Box<matrix<double>>>() )
        return operation(value.as_<Box<matrix<double>>>());
    if (value.is_a<Box<matrix<int>>>() )
        return operation(value.as_<Box<matrix<int>>>());

    throw myexception()<<"Unsupported native matrix representation "<<value.print();
}

// Dispatch an operation to one of the native numeric vector representations
// supported by the Haskell Vector type.
template <typename F>
decltype(auto) visit_numeric_vector(const R::Exp& value, F&& operation)
{
    if (value.is_a<Box<DenseVector<double>>>() )
        return operation(value.as_<Box<DenseVector<double>>>());
    if (value.is_a<Box<DenseVector<int>>>() )
        return operation(value.as_<Box<DenseVector<int>>>());

    throw myexception()<<"Unsupported native vector representation "<<value.print();
}

// Dispatch a binary operation after verifying that both matrices have the
// same supported native element representation.
template <typename F>
decltype(auto) visit_same_matrix_pair(const R::Exp& value1, const R::Exp& value2, F&& operation)
{
    if (value1.is_a<Box<matrix<double>>>() )
    {
        if (not value2.is_a<Box<matrix<double>>>() )
            throw myexception()<<"Matrices have different native element representations";
        return operation(value1.as_<Box<matrix<double>>>(), value2.as_<Box<matrix<double>>>());
    }
    if (value1.is_a<Box<matrix<int>>>() )
    {
        if (not value2.is_a<Box<matrix<int>>>() )
            throw myexception()<<"Matrices have different native element representations";
        return operation(value1.as_<Box<matrix<int>>>(), value2.as_<Box<matrix<int>>>());
    }

    throw myexception()<<"Unsupported native matrix representation "<<value1.print();
}

// Read a runtime scalar using the representation selected by its matrix.
template <typename T>
T matrix_scalar(const R::Exp& value)
{
    if constexpr (std::is_same_v<T, int>)
        return value.as_int();
    else
        return value.as_double();
}

// Apply a scalar operation to every element while preserving the native
// matrix representation.
template <typename T, typename F>
closure map_matrix(const Box<matrix<T>>& matrix1, F&& operation)
{
    auto matrix2 = new Box<matrix<T>>(matrix1.size1(), matrix1.size2());
    for(int i=0; i<matrix1.size1(); i++)
        for(int j=0; j<matrix1.size2(); j++)
            (*matrix2)(i,j) = operation(matrix1(i,j));
    return matrix2;
}

// Apply an elementwise binary operation after checking that matrix dimensions
// agree.
template <typename T, typename F>
closure zip_matrices(const Box<matrix<T>>& matrix1, const Box<matrix<T>>& matrix2,
                     const std::string& operation_name, F&& operation)
{
    int rows = matrix1.size1();
    int cols = matrix1.size2();
    if (matrix2.size1() != rows or matrix2.size2() != cols)
        throw myexception()<<"Trying to "<<operation_name<<" matrices of unequal sizes ("
                           <<rows<<","<<cols<<") and ("<<matrix2.size1()<<","<<matrix2.size2()<<")";

    auto matrix3 = new Box<matrix<T>>(rows, cols);
    for(int i=0; i<rows; i++)
        for(int j=0; j<cols; j++)
            (*matrix3)(i,j) = operation(matrix1(i,j), matrix2(i,j));
    return matrix3;
}

// Multiply two compatible matrices without changing their native element
// representation.
template <typename T>
closure multiply_matrices(const Box<matrix<T>>& matrix1, const Box<matrix<T>>& matrix2)
{
    if (matrix1.size2() != matrix2.size1())
        throw myexception()<<"Trying to multiply matrices of incompatible sizes ("
                           <<matrix1.size1()<<","<<matrix1.size2()<<") and ("
                           <<matrix2.size1()<<","<<matrix2.size2()<<")";

    int rows = matrix1.size1();
    int inner = matrix1.size2();
    int cols = matrix2.size2();
    auto matrix3 = new Box<matrix<T>>(rows, cols);

    for(int i=0; i<rows; i++)
        for(int j=0; j<cols; j++)
        {
            T sum = 0;
            for(int k=0; k<inner; k++)
                sum += matrix1(i,k) * matrix2(k,j);
            (*matrix3)(i,j) = sum;
        }
    return matrix3;
}

// Transpose a native matrix while preserving its element representation.
template <typename T>
closure transpose_matrix(const Box<matrix<T>>& matrix1)
{
    auto matrix2 = new Box<matrix<T>>(matrix1.size2(), matrix1.size1());
    for(int i=0; i<matrix2->size1(); i++)
        for(int j=0; j<matrix2->size2(); j++)
            (*matrix2)(i,j) = matrix1(j,i);
    return matrix2;
}

// Flatten a native matrix into contiguous numeric values in row-major order.
template <typename T>
closure matrix_to_vector(const Box<matrix<T>>& native_matrix)
{
    object_ptr<Box<DenseVector<T>>> values = new Box<DenseVector<T>>(native_matrix.size());
    std::copy(native_matrix.begin(), native_matrix.end(), values->data());
    return values;
}

// Read a complete Haskell list through dependent USE edges and store its
// scalar values contiguously in a native numeric vector.
template <typename T>
closure vector_from_list(OperationArgs& Args)
{
    std::vector<T> values;
    int xs = Args.evaluate_slot_use(0);

    while(true)
    {
        const closure& xs_closure = Args.memory().closure_at(xs);
        auto list_cell = xs_closure.get_code().to<Runtime::ConstructorApp>();
        if (not list_cell)
            throw myexception()<<"vector fromList: expected a list constructor, but got "
                               <<xs_closure.get_code().print();

        const auto& tag = list_cell->head;
        if (tag.name() == "[]" and tag.n_args() == 0)
            break;
        if (tag.name() != ":" or tag.n_args() != 2)
            throw myexception()<<"vector fromList: expected ':' or '[]', but got "<<tag.print();
        if (values.size() == static_cast<std::size_t>(std::numeric_limits<int>::max()))
            throw myexception()<<"vector fromList: element count exceeds supported Int range";

        int element = xs_closure.reg_for_constructor_slot(0);
        int tail = xs_closure.reg_for_constructor_slot(1);
        int value = Args.evaluate_reg_dependent_use(element);
        values.push_back(matrix_scalar<T>(Args.memory().closure_at(value).get_code()));
        xs = Args.evaluate_reg_dependent_use(tail);
    }

    object_ptr<Box<DenseVector<T>>> result = new Box<DenseVector<T>>(values.size());
    std::copy(values.begin(), values.end(), result->data());
    return result;
}

// Read exactly the requested number of Haskell list cells, rejecting short
// input and leaving any excess tail unevaluated.
template <typename T>
closure sized_vector_from_list(OperationArgs& Args)
{
    int expected_size = Args.evaluate_slot_to_value(0).as_int();
    if (expected_size < 0)
        throw myexception()<<"vector (|>): size must be nonnegative, but got "<<expected_size;

    object_ptr<Box<DenseVector<T>>> result = new Box<DenseVector<T>>(expected_size);
    int xs = Args.evaluate_slot_use(1);
    for(int k=0; k<expected_size; k++)
    {
        const closure& xs_closure = Args.memory().closure_at(xs);
        auto list_cell = xs_closure.get_code().to<Runtime::ConstructorApp>();
        if (not list_cell)
            throw myexception()<<"vector (|>): expected a list constructor, but got "
                               <<xs_closure.get_code().print();

        const auto& tag = list_cell->head;
        if (tag.name() == "[]" and tag.n_args() == 0)
            throw myexception()<<"vector (|>): expected "<<expected_size<<" elements, but got "<<k;
        if (tag.name() != ":" or tag.n_args() != 2)
            throw myexception()<<"vector (|>): expected ':' or '[]', but got "<<tag.print();

        int element = xs_closure.reg_for_constructor_slot(0);
        int tail = xs_closure.reg_for_constructor_slot(1);
        int value = Args.evaluate_reg_dependent_use(element);
        (*result)(k) = matrix_scalar<T>(Args.memory().closure_at(value).get_code());
        xs = Args.evaluate_reg_dependent_use(tail);
    }
    return result;
}

// Copy a native vector into a row-major matrix having the requested column
// count, validating that the vector can be partitioned into complete rows.
template <typename T>
closure reshape_vector(const Box<DenseVector<T>>& values, int columns)
{
    if (columns < 0)
        throw myexception()<<"reshape: column count must be nonnegative, but got "<<columns;
    if (columns == 0)
    {
        if (values.size() != 0)
            throw myexception()<<"reshape: a nonempty vector cannot have zero columns";
        return new Box<matrix<T>>(0, 0);
    }
    if (values.size() % columns != 0)
        throw myexception()<<"reshape: vector length "<<values.size()
                           <<" is not divisible by column count "<<columns;

    int rows = values.size() / columns;
    auto result = new Box<matrix<T>>(rows, columns);
    std::copy(values.data(), values.data() + values.size(), result->begin());
    return result;
}

// Copy a native vector into either a single row or a single column matrix.
template <typename T>
closure vector_as_matrix(const Box<DenseVector<T>>& values, bool as_row)
{
    int rows = as_row ? 1 : values.size();
    int columns = as_row ? values.size() : 1;
    auto result = new Box<matrix<T>>(rows, columns);
    std::copy(values.data(), values.data() + values.size(), result->begin());
    return result;
}

// Build a native matrix while walking a Haskell list through dependent USE
// edges, validating dimensions and the exact element count before returning.
template <typename T>
closure matrix_from_list(OperationArgs& Args)
{
    int rows = Args.evaluate_slot_to_value(0).as_int();
    int columns = Args.evaluate_slot_to_value(1).as_int();

    if (rows < 0 or columns < 0)
        throw myexception()<<"matrix (><): dimensions must be nonnegative, but got ("
                           <<rows<<", "<<columns<<")";

    if (columns != 0 and rows > std::numeric_limits<int>::max() / columns)
        throw myexception()<<"matrix (><): dimensions ("<<rows<<", "<<columns
                           <<") exceed the supported element count";

    int expected_size = rows * columns;
    object_ptr<Box<matrix<T>>> native_matrix = new Box<matrix<T>>(rows, columns);
    int xs = Args.evaluate_slot_use(2);

    for(int k=0; k<expected_size; k++)
    {
        const closure& xs_closure = Args.memory().closure_at(xs);
        auto list_cell = xs_closure.get_code().to<Runtime::ConstructorApp>();
        if (not list_cell)
            throw myexception()<<"matrix (><): expected a list constructor, but got "
                               <<xs_closure.get_code().print();

        const auto& tag = list_cell->head;
        if (tag.name() == "[]" and tag.n_args() == 0)
            throw myexception()<<"matrix (><): expected "<<expected_size
                               <<" elements, but got "<<k;
        if (tag.name() != ":" or tag.n_args() != 2)
            throw myexception()<<"matrix (><): expected ':' or '[]', but got "<<tag.print();

        int element = xs_closure.reg_for_constructor_slot(0);
        int tail = xs_closure.reg_for_constructor_slot(1);
        int value = Args.evaluate_reg_dependent_use(element);
        native_matrix->begin()[k] = matrix_scalar<T>(Args.memory().closure_at(value).get_code());
        xs = Args.evaluate_reg_dependent_use(tail);
    }

    return native_matrix;
}

}

// Construct a matrix using the native Int representation.
extern "C" closure builtin_function_intMatrixFromList(OperationArgs& Args)
{
    return matrix_from_list<int>(Args);
}

// Construct a matrix using the native Double representation.
extern "C" closure builtin_function_doubleMatrixFromList(OperationArgs& Args)
{
    return matrix_from_list<double>(Args);
}

// Construct a native Int vector from a complete Haskell list.
extern "C" closure builtin_function_intVectorFromList(OperationArgs& Args)
{
    return vector_from_list<int>(Args);
}

// Construct a native Double vector from a complete Haskell list.
extern "C" closure builtin_function_doubleVectorFromList(OperationArgs& Args)
{
    return vector_from_list<double>(Args);
}

// Construct a fixed-length Int vector without evaluating an excess list tail.
extern "C" closure builtin_function_sizedIntVectorFromList(OperationArgs& Args)
{
    return sized_vector_from_list<int>(Args);
}

// Construct a fixed-length Double vector without evaluating an excess list tail.
extern "C" closure builtin_function_sizedDoubleVectorFromList(OperationArgs& Args)
{
    return sized_vector_from_list<double>(Args);
}

// Return the dimension of either supported native numeric vector.
extern "C" R::Exp simple_function_vectorSize(vector<R::Exp>& args)
{
    auto value = get_arg(args);
    return visit_numeric_vector(value, [](const auto& vector_value) {
        return static_cast<int>(vector_value.size());
    });
}

// Return one native vector element after checking its zero-based index.
extern "C" R::Exp simple_function_vectorAtIndex(vector<R::Exp>& args)
{
    auto value = get_arg(args);
    int index = get_arg(args).as_int();
    // Check the requested index before preserving the native scalar representation.
    return visit_numeric_vector(value, [index](const auto& vector_value) -> R::Exp {
        if (index < 0 or index >= vector_value.size())
            throw myexception()<<"vector atIndex: index "<<index
                               <<" is outside vector length "<<vector_value.size();
        return vector_value(index);
    });
}

// Compare two native vectors exactly after verifying that their element
// representations agree.
extern "C" R::Exp simple_function_vectorEqual(vector<R::Exp>& args)
{
    auto value1 = get_arg(args);
    auto value2 = get_arg(args);
    if (value1.is_a<Box<DenseVector<double>>>() and
        value2.is_a<Box<DenseVector<double>>>())
    {
        const auto& vector1 = value1.as_<Box<DenseVector<double>>>();
        const auto& vector2 = value2.as_<Box<DenseVector<double>>>();
        return vector1.size() == vector2.size() and
               (vector1.array() == vector2.array()).all();
    }
    if (value1.is_a<Box<DenseVector<int>>>() and
        value2.is_a<Box<DenseVector<int>>>())
    {
        const auto& vector1 = value1.as_<Box<DenseVector<int>>>();
        const auto& vector2 = value2.as_<Box<DenseVector<int>>>();
        return vector1.size() == vector2.size() and
               (vector1.array() == vector2.array()).all();
    }
    if (value1.is_a<Box<DenseVector<double>>>() or value1.is_a<Box<DenseVector<int>>>())
        throw myexception()<<"Native vectors have different element representations";

    throw myexception()<<"Unsupported native vector representation "<<value1.print();
}

// Sum a native vector while preserving its Int or Double scalar
// representation.
extern "C" R::Exp simple_function_vectorSumElements(vector<R::Exp>& args)
{
    auto value = get_arg(args);
    return visit_numeric_vector(value, [](const auto& vector_value) -> R::Exp {
        return vector_value.sum();
    });
}

// Reshape either supported native vector into a row-major matrix.
extern "C" closure builtin_function_reshapeVector(OperationArgs& Args)
{
    int columns = Args.evaluate_slot_to_value(0).as_int();
    auto value = Args.evaluate_slot_to_value(1);
    return visit_numeric_vector(value, [columns](const auto& vector_value) {
        return reshape_vector(vector_value, columns);
    });
}

// Convert either supported native vector into a one-row matrix.
extern "C" closure builtin_function_vectorAsRow(OperationArgs& Args)
{
    auto value = Args.evaluate_slot_to_value(0);
    return visit_numeric_vector(value, [](const auto& vector_value) {
        return vector_as_matrix(vector_value, true);
    });
}

// Convert either supported native vector into a one-column matrix.
extern "C" closure builtin_function_vectorAsColumn(OperationArgs& Args)
{
    auto value = Args.evaluate_slot_to_value(0);
    return visit_numeric_vector(value, [](const auto& vector_value) {
        return vector_as_matrix(vector_value, false);
    });
}

// Return the number of rows for either supported native matrix representation.
extern "C" R::Exp simple_function_nrows(vector<R::Exp>& args)
{
    auto arg0 = get_arg(args);
    return visit_matrix(arg0, [](const auto& native_matrix) { return native_matrix.size1(); });
}

// Return the number of columns for either supported native matrix representation.
extern "C" R::Exp simple_function_ncols(vector<R::Exp>& args)
{
    auto arg0 = get_arg(args);
    return visit_matrix(arg0, [](const auto& native_matrix) { return native_matrix.size2(); });
}

// Sum a native matrix while preserving its Int or Double scalar
// representation.
extern "C" R::Exp simple_function_matrixSumElements(vector<R::Exp>& args)
{
    auto value = get_arg(args);
    // Accumulate in the matrix's native scalar type without flattening it.
    return visit_matrix(value, [](const auto& native_matrix) -> R::Exp {
        using Scalar = std::remove_cvref_t<decltype(native_matrix(0,0))>;
        Scalar total = 0;
        for(int i = 0; i < native_matrix.size1(); i++)
            for(int j = 0; j < native_matrix.size2(); j++)
                total += native_matrix(i,j);
        return total;
    });
}

// scaleMatrix :: a -> Matrix a -> Matrix a
extern "C" closure builtin_function_scaleMatrix(OperationArgs& Args)
{
    auto factor = Args.evaluate_slot_to_value(0);
    auto matrix_value = Args.evaluate_slot_to_value(1);
    // Convert the factor according to the matrix representation selected at runtime.
    return visit_matrix(matrix_value, [&factor]<typename T>(const Box<matrix<T>>& native_matrix) {
        T typed_factor = matrix_scalar<T>(factor);
        return map_matrix(native_matrix, [typed_factor](T element) { return typed_factor * element; });
    });
}

extern "C" closure builtin_function_elementwise_multiply(OperationArgs& Args)
{
    auto matrix1 = Args.evaluate_slot_to_value(0);
    auto matrix2 = Args.evaluate_slot_to_value(1);
    return visit_same_matrix_pair(matrix1, matrix2, [](const auto& native1, const auto& native2) {
        return zip_matrices(native1, native2, "elementwise-multiply",
                            [](auto element1, auto element2) { return element1 * element2; });
    });
}

extern "C" closure builtin_function_mat_mult(OperationArgs& Args)
{
    auto matrix1 = Args.evaluate_slot_to_value(0);
    auto matrix2 = Args.evaluate_slot_to_value(1);
    return visit_same_matrix_pair(matrix1, matrix2, [](const auto& native1, const auto& native2) {
        return multiply_matrices(native1, native2);
    });
}

extern "C" closure builtin_function_elementwise_add(OperationArgs& Args)
{
    auto matrix1 = Args.evaluate_slot_to_value(0);
    auto matrix2 = Args.evaluate_slot_to_value(1);
    return visit_same_matrix_pair(matrix1, matrix2, [](const auto& native1, const auto& native2) {
        return zip_matrices(native1, native2, "add",
                            [](auto element1, auto element2) { return element1 + element2; });
    });
}


extern "C" closure builtin_function_elementwise_sub(OperationArgs& Args)
{
    auto matrix1 = Args.evaluate_slot_to_value(0);
    auto matrix2 = Args.evaluate_slot_to_value(1);
    return visit_same_matrix_pair(matrix1, matrix2, [](const auto& native1, const auto& native2) {
        return zip_matrices(native1, native2, "subtract",
                            [](auto element1, auto element2) { return element1 - element2; });
    });
}

extern "C" closure builtin_function_mat_negate(OperationArgs& Args)
{
    auto matrix_value = Args.evaluate_slot_to_value(0);
    return visit_matrix(matrix_value, [](const auto& native_matrix) {
        return map_matrix(native_matrix, [](auto element) { return -element; });
    });
}

extern "C" closure builtin_function_mat_abs(OperationArgs& Args)
{
    auto matrix_value = Args.evaluate_slot_to_value(0);
    return visit_matrix(matrix_value, [](const auto& native_matrix) {
        return map_matrix(native_matrix, [](auto element) { return std::abs(element); });
    });
}


extern "C" closure builtin_function_mat_signum(OperationArgs& Args)
{
    auto matrix_value = Args.evaluate_slot_to_value(0);
    // Preserve the native representation while mapping values to -1, 0, or 1.
    return visit_matrix(matrix_value, [](const auto& native_matrix) {
        return map_matrix(native_matrix, [](auto element) {
            return (element > 0 ? 1 : 0) - (element < 0 ? 1 : 0);
        });
    });
}


#include <unsupported/Eigen/MatrixFunctions>

extern "C" closure builtin_function_MatrixExp(OperationArgs& Args)
{
    using Eigen::Map;
    using Eigen::Dynamic;
    using Eigen::RowMajor;

    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& Q = arg0.as_<Box<Matrix>>();
    int n = Q.size1();
    assert(Q.size2() == n);

    double t = Args.evaluate_slot_to_value(1).as_double();

    auto P = new Box<Matrix>(n,n);

    // Using Map<.., Eigen::Aligned> gives a small (0.2%) speedup with codon alphabets.
    // But ensuring alignment is messy on windows.
    Map<const Eigen::Matrix<double, Dynamic, Dynamic, RowMajor>> EQ(Q.begin(), n, n);
    Map<Eigen::Matrix<double, Dynamic, Dynamic, RowMajor>> EP(P->begin(), n, n);

    EP = (t*EQ).exp();

    for(int i=0; i< n;i++)
    {
        double sum = 0;
        for(int j=0; j< n;j++)
        {
            EP(i,j) = std::max(EP(i,j),0.0);
            sum += EP(i,j);
        }
        for(int j=0; j< n;j++)
            EP(i,j)/= sum;
    }
    
    return P;
}



/*
 * 1. pi[i]*Q(i,j) = pi[j]*Q(j,i)         - Because Q is reversible
 * 2. Q(i,j)/pi[j] = Q(j,i)/pi[i] = S1(i,j)
 * 3. pi[i]^1/2 * Q(j,i) / pi[j]^1/2 = S2(i,j)
 * 4. exp(Q) = pi^-1.2 * exp(pi^1/2 * Q * pi^-1/2) * pi^1/2
 *           = pi^-1.2 * exp(S2) * pi^1/2
 */

extern "C" closure builtin_function_getEigensystemRaw(OperationArgs& Args)
{
    using namespace Eigen;

    auto arg0 = Args.evaluate_slot_to_value(0);
    const ::Matrix& Q = arg0.as_< Box<::Matrix> >();

    auto pi = vector<double>(Args.evaluate_slot_to_value(1).as_<R::RVector>() );

    const unsigned n = Q.size1();
    assert(Q.size2() == Q.size1());

#ifdef DEBUG_RATE_MATRIX
    assert(std::abs(sum(pi)-1.0) < 1.0e-6);
    for(int i=0;i<n;i++) {
	double sum = 0;
	for(int j=0;j<n;j++)
	    sum += Q(i,j);
	assert(abs(sum) < 1.0e-6);
    }
#endif

    //--------- Compute pi[i]**0.5 and pi[i]**-0.5 ----------//
    vector<double> sqrt_pi(n, 1.0);
    vector<double> inverse_sqrt_pi(n, 1.0);
    for(int i=0;i<n;i++)
    {
        // symmetrizing may be ill-conditioned -- fail
        if (not (pi[i]*n > 1.0e-13)) return {R::RMaybe()};

        sqrt_pi[i] = sqrt(pi[i]);
        inverse_sqrt_pi[i] = 1.0/sqrt_pi[i];

        // fail if we see Inf, -Inf or Nan.
        if (not std::isfinite(pi[i]) or not std::isfinite(inverse_sqrt_pi[i])) return {R::RMaybe()};
    }

    //--------------- Calculate eigensystem -----------------//
    ::Matrix S(n,n);
    for(int i=0;i<n;i++)
	for(int j=0;j<=i;j++) {
	    S(j,i) = S(i,j) = Q(i,j) * sqrt_pi[i] * inverse_sqrt_pi[j];

#ifdef DEBUG_RATE_MATRIX
	    // check reversibility of rate matrix
	    if (i != j) {
		assert (S(i,j) >= 0);
		double p12 = Q(i,j)*pi[i];
		double p21 = Q(j,i)*pi[j];
		assert (abs(p12-p21) < 1.0e-12*(1.0+abs(p12)));
		if (i > j)
		    assert( abs(S(i,j) - S(j,i)) < 1.0e-13 );
	    }
	    else
		assert (Q(i,j) <= 0);
#endif
            // fail if we see Inf, -Inf or Nan.
            if (not std::isfinite(S(i,j))) return {R::RMaybe()};
	}

    //---------------- Compute eigensystem ------------------//
    // 1. Make an eigen array from M
    Map<const Eigen::Matrix<double, Dynamic, Dynamic, RowMajor>> S2(S.begin(), n, n);

    object_ptr<Box<EigenValues>> eigensolver(new Box<EigenValues>(S2, ComputeEigenvectors));
    if (eigensolver->info() != Eigen::Success)
        return {R::RMaybe()};
    else if (std::abs(eigensolver->eigenvalues().maxCoeff()) > 1.0e-9)
    {
        // The largest eigenvalue should be exactly 0.
        return {R::RMaybe()};
    }
    else
        return {R::RMaybe(eigensolver)};
}

extern "C" closure builtin_function_lExpRaw(OperationArgs& Args)
{
    auto L = Args.evaluate_slot_to_value(0);
    auto pi = (vector<double>) Args.evaluate_slot_to_value(1).as_<R::RVector>();
    double t = Args.evaluate_slot_to_value(2).as_double();

    object_ptr<Box<Matrix>> Mptr = new Box<Matrix>;
    auto& M = *Mptr;
    M = exp(L.as_<Box<EigenValues>>(), pi, t);

    double error = positivize_and_renormalize_matrix(M);

    if (error > 1.0e-9)
        return {R::RMaybe()};
    else
        return {R::RMaybe(Mptr)};
}


extern "C" closure builtin_function_transpose(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    return visit_matrix(arg0, [](const auto& native_matrix) {
        return transpose_matrix(native_matrix);
    });
}

// Flatten either supported matrix representation into an EVector.
extern "C" closure builtin_function_matrixToVector(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    return visit_matrix(arg0, [](const auto& native_matrix) {
        return matrix_to_vector(native_matrix);
    });
}

// Read one element without converting its native scalar representation.
extern "C" R::Exp simple_function_getElem(vector<R::Exp>& args)
{
    int i = get_arg(args).as_int();
    int j = get_arg(args).as_int();
    auto arg2 = get_arg(args);
    return visit_matrix(arg2, [i,j](const auto& native_matrix) {
        return R::Exp(native_matrix(i,j));
    });
}
