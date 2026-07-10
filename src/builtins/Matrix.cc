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
    if (value.is_a<Box<DenseMatrix<double>>>() )
        return operation(value.as_<Box<DenseMatrix<double>>>());
    if (value.is_a<Box<DenseMatrix<int>>>() )
        return operation(value.as_<Box<DenseMatrix<int>>>());

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

// Dispatch a binary operation after verifying that both vectors have the
// same supported native element representation.
template <typename F>
decltype(auto) visit_same_numeric_vector_pair(const R::Exp& value1, const R::Exp& value2, F&& operation)
{
    if (value1.is_a<Box<DenseVector<double>>>() )
    {
        if (not value2.is_a<Box<DenseVector<double>>>() )
            throw myexception()<<"Vectors have different native element representations";
        return operation(value1.as_<Box<DenseVector<double>>>(), value2.as_<Box<DenseVector<double>>>());
    }
    if (value1.is_a<Box<DenseVector<int>>>() )
    {
        if (not value2.is_a<Box<DenseVector<int>>>() )
            throw myexception()<<"Vectors have different native element representations";
        return operation(value1.as_<Box<DenseVector<int>>>(), value2.as_<Box<DenseVector<int>>>());
    }

    throw myexception()<<"Unsupported native vector representation "<<value1.print();
}

// Dispatch a binary operation after verifying that both matrices have the
// same supported native element representation.
template <typename F>
decltype(auto) visit_same_matrix_pair(const R::Exp& value1, const R::Exp& value2, F&& operation)
{
    if (value1.is_a<Box<DenseMatrix<double>>>() )
    {
        if (not value2.is_a<Box<DenseMatrix<double>>>() )
            throw myexception()<<"Matrices have different native element representations";
        return operation(value1.as_<Box<DenseMatrix<double>>>(), value2.as_<Box<DenseMatrix<double>>>());
    }
    if (value1.is_a<Box<DenseMatrix<int>>>() )
    {
        if (not value2.is_a<Box<DenseMatrix<int>>>() )
            throw myexception()<<"Matrices have different native element representations";
        return operation(value1.as_<Box<DenseMatrix<int>>>(), value2.as_<Box<DenseMatrix<int>>>());
    }

    throw myexception()<<"Unsupported native matrix representation "<<value1.print();
}

// Dispatch a matrix/vector operation only when both values use the same
// supported native element representation.
template <typename F>
decltype(auto) visit_same_matrix_vector_pair(const R::Exp& matrix_value,
                                             const R::Exp& vector_value,
                                             F&& operation)
{
    if (matrix_value.is_a<Box<DenseMatrix<double>>>() )
    {
        if (not vector_value.is_a<Box<DenseVector<double>>>() )
            throw myexception()<<"Matrix and vector have different native element representations";
        return operation(matrix_value.as_<Box<DenseMatrix<double>>>(),
                         vector_value.as_<Box<DenseVector<double>>>());
    }
    if (matrix_value.is_a<Box<DenseMatrix<int>>>() )
    {
        if (not vector_value.is_a<Box<DenseVector<int>>>() )
            throw myexception()<<"Matrix and vector have different native element representations";
        return operation(matrix_value.as_<Box<DenseMatrix<int>>>(),
                         vector_value.as_<Box<DenseVector<int>>>());
    }

    throw myexception()<<"Unsupported native matrix representation "<<matrix_value.print();
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

// Return the broadcast result for two extents, rejecting unequal extents
// unless one operand is a singleton along this dimension.
Eigen::Index conform_dimension(Eigen::Index extent1, Eigen::Index extent2,
                               const std::string& operation_name,
                               const std::string& dimension_name)
{
    if (extent1 == extent2)
        return extent1;
    if (extent1 == 1)
        return extent2;
    if (extent2 == 1)
        return extent1;
    throw myexception()<<operation_name<<": incompatible "<<dimension_name
                       <<" extents "<<extent1<<" and "<<extent2;
}

// Apply a scalar operation to every vector element while preserving its
// native representation.
template <typename NativeVector, typename F>
closure map_vector(const Box<NativeVector>& vector1, F&& operation)
{
    auto vector2 = new Box<NativeVector>(vector1.size());
    for(Eigen::Index i=0; i<vector1.size(); i++)
        (*vector2)(i) = operation(vector1(i));
    return vector2;
}

// Apply an elementwise binary operation using singleton vector broadcasting.
template <typename NativeVector, typename F>
closure zip_vectors(const Box<NativeVector>& vector1, const Box<NativeVector>& vector2,
                    const std::string& operation_name, F&& operation)
{
    Eigen::Index size = conform_dimension(vector1.size(), vector2.size(),
                                          operation_name, "vector");
    auto vector3 = new Box<NativeVector>(size);
    for(Eigen::Index i=0; i<size; i++)
        (*vector3)(i) = operation(vector1(vector1.size() == 1 ? 0 : i),
                                  vector2(vector2.size() == 1 ? 0 : i));
    return vector3;
}

// Apply a scalar operation to every element while preserving the native
// matrix representation.
template <typename NativeMatrix, typename F>
closure map_matrix(const Box<NativeMatrix>& matrix1, F&& operation)
{
    auto matrix2 = new Box<NativeMatrix>(matrix1.rows(), matrix1.cols());
    for(Eigen::Index i=0; i<matrix1.rows(); i++)
        for(Eigen::Index j=0; j<matrix1.cols(); j++)
            (*matrix2)(i,j) = operation(matrix1(i,j));
    return matrix2;
}

// Apply an elementwise binary operation using independent singleton
// broadcasting for rows and columns.
template <typename NativeMatrix, typename F>
closure zip_matrices(const Box<NativeMatrix>& matrix1, const Box<NativeMatrix>& matrix2,
                     const std::string& operation_name, F&& operation)
{
    Eigen::Index rows = conform_dimension(matrix1.rows(), matrix2.rows(),
                                          operation_name, "row");
    Eigen::Index cols = conform_dimension(matrix1.cols(), matrix2.cols(),
                                          operation_name, "column");

    auto matrix3 = new Box<NativeMatrix>(rows, cols);
    for(Eigen::Index i=0; i<rows; i++)
        for(Eigen::Index j=0; j<cols; j++)
            (*matrix3)(i,j) = operation(
                matrix1(matrix1.rows() == 1 ? 0 : i, matrix1.cols() == 1 ? 0 : j),
                matrix2(matrix2.rows() == 1 ? 0 : i, matrix2.cols() == 1 ? 0 : j));
    return matrix3;
}

// Multiply two compatible matrices without changing their native element
// representation.
template <typename NativeMatrix>
closure multiply_matrices(const Box<NativeMatrix>& matrix1, const Box<NativeMatrix>& matrix2)
{
    if (matrix1.cols() != matrix2.rows())
        throw myexception()<<"Trying to multiply matrices of incompatible sizes ("
                           <<matrix1.rows()<<","<<matrix1.cols()<<") and ("
                           <<matrix2.rows()<<","<<matrix2.cols()<<")";

    auto matrix3 = new Box<NativeMatrix>(matrix1.rows(), matrix2.cols());
    matrix3->noalias() = matrix1 * matrix2;
    return matrix3;
}

// Multiply a matrix by a conformable column vector using Eigen evaluation.
template <typename NativeMatrix, typename NativeVector>
closure multiply_matrix_vector(const Box<NativeMatrix>& matrix,
                               const Box<NativeVector>& vector)
{
    if (matrix.cols() != vector.size())
        throw myexception()<<"matrix #> vector: incompatible extents "
                           <<matrix.cols()<<" and "<<vector.size();
    auto result = new Box<NativeVector>(matrix.rows());
    result->noalias() = matrix * vector;
    return result;
}

// Multiply a row vector by a conformable matrix, returning a column-stored
// vector containing the row-product values.
template <typename NativeVector, typename NativeMatrix>
closure multiply_vector_matrix(const Box<NativeVector>& vector,
                               const Box<NativeMatrix>& matrix)
{
    if (vector.size() != matrix.rows())
        throw myexception()<<"vector <# matrix: incompatible extents "
                           <<vector.size()<<" and "<<matrix.rows();
    auto result = new Box<NativeVector>(matrix.cols());
    result->noalias() = matrix.transpose() * vector;
    return result;
}

// Form the outer product of two vectors without materializing row-vector
// storage.
template <typename NativeVector>
closure outer_vectors(const Box<NativeVector>& vector1,
                      const Box<NativeVector>& vector2)
{
    using T = typename NativeVector::Scalar;
    auto result = new Box<DenseMatrix<T>>(vector1.size(), vector2.size());
    result->noalias() = vector1 * vector2.transpose();
    return result;
}

// Transpose a native matrix while preserving its element representation.
template <typename NativeMatrix>
closure transpose_matrix(const Box<NativeMatrix>& matrix1)
{
    auto matrix2 = new Box<NativeMatrix>(matrix1.cols(), matrix1.rows());
    for(Eigen::Index i=0; i<matrix2->rows(); i++)
        for(Eigen::Index j=0; j<matrix2->cols(); j++)
            (*matrix2)(i,j) = matrix1(j,i);
    return matrix2;
}

// Flatten a native matrix into contiguous numeric values in row-major order.
template <typename NativeMatrix>
closure matrix_to_vector(const Box<NativeMatrix>& native_matrix)
{
    using T = std::remove_cvref_t<decltype(native_matrix(0,0))>;
    object_ptr<Box<DenseVector<T>>> values = new Box<DenseVector<T>>(native_matrix.size());
    for(Eigen::Index i=0; i<native_matrix.rows(); i++)
        for(Eigen::Index j=0; j<native_matrix.cols(); j++)
            (*values)(i * native_matrix.cols() + j) = native_matrix(i,j);
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
        return new Box<DenseMatrix<T>>(0, 0);
    }
    if (values.size() % columns != 0)
        throw myexception()<<"reshape: vector length "<<values.size()
                           <<" is not divisible by column count "<<columns;

    int rows = values.size() / columns;
    auto result = new Box<DenseMatrix<T>>(rows, columns);
    for(int k=0; k<values.size(); k++)
        (*result)(k / columns, k % columns) = values(k);
    return result;
}

// Copy a native vector into either a single row or a single column matrix.
template <typename T>
closure vector_as_matrix(const Box<DenseVector<T>>& values, bool as_row)
{
    int rows = as_row ? 1 : values.size();
    int columns = as_row ? values.size() : 1;
    auto result = new Box<DenseMatrix<T>>(rows, columns);
    for(int k=0; k<values.size(); k++)
        (*result)(as_row ? 0 : k, as_row ? k : 0) = values(k);
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
    object_ptr<Box<DenseMatrix<T>>> native_matrix = new Box<DenseMatrix<T>>(rows, columns);
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
        (*native_matrix)(k / columns, k % columns) =
            matrix_scalar<T>(Args.memory().closure_at(value).get_code());
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

// Compute the dot product of two equal-length vectors.
extern "C" R::Exp simple_function_dotNative(vector<R::Exp>& args)
{
    auto value1 = get_arg(args);
    auto value2 = get_arg(args);
    // Evaluate the Eigen dot product after checking the shared representation.
    return visit_same_numeric_vector_pair(value1, value2, [](const auto& vector1, const auto& vector2) -> R::Exp {
        if (vector1.size() != vector2.size())
            throw myexception()<<"dot: incompatible vector lengths "
                               <<vector1.size()<<" and "<<vector2.size();
        using T = typename std::remove_cvref_t<decltype(vector1)>::Scalar;
        return static_cast<T>(vector1.dot(vector2));
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
extern "C" R::Exp simple_function_rows(vector<R::Exp>& args)
{
    auto arg0 = get_arg(args);
    return visit_matrix(arg0, [](const auto& native_matrix) { return static_cast<int>(native_matrix.rows()); });
}

// Return the number of columns for either supported native matrix representation.
extern "C" R::Exp simple_function_cols(vector<R::Exp>& args)
{
    auto arg0 = get_arg(args);
    return visit_matrix(arg0, [](const auto& native_matrix) { return static_cast<int>(native_matrix.cols()); });
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
        for(Eigen::Index i = 0; i < native_matrix.rows(); i++)
            for(Eigen::Index j = 0; j < native_matrix.cols(); j++)
                total += native_matrix(i,j);
        return total;
    });
}

// Scale every matrix element without changing its native representation.
extern "C" closure builtin_function_scale(OperationArgs& Args)
{
    auto factor = Args.evaluate_slot_to_value(0);
    auto matrix_value = Args.evaluate_slot_to_value(1);
    // Convert the factor according to the matrix representation selected at runtime.
    return visit_matrix(matrix_value, [&factor](const auto& native_matrix) {
        using T = std::remove_cvref_t<decltype(native_matrix(0,0))>;
        T typed_factor = matrix_scalar<T>(factor);
        return map_matrix(native_matrix, [typed_factor](T element) { return typed_factor * element; });
    });
}

// Multiply vectors elementwise with singleton broadcasting.
extern "C" closure builtin_function_vector_elementwise_multiply(OperationArgs& Args)
{
    auto vector1 = Args.evaluate_slot_to_value(0);
    auto vector2 = Args.evaluate_slot_to_value(1);
    // Apply multiplication after dispatching the common native representation.
    return visit_same_numeric_vector_pair(vector1, vector2, [](const auto& native1, const auto& native2) {
        return zip_vectors(native1, native2, "vector multiply",
                           [](auto element1, auto element2) { return element1 * element2; });
    });
}

// Add vectors elementwise with singleton broadcasting.
extern "C" closure builtin_function_vector_elementwise_add(OperationArgs& Args)
{
    auto vector1 = Args.evaluate_slot_to_value(0);
    auto vector2 = Args.evaluate_slot_to_value(1);
    // Apply addition after dispatching the common native representation.
    return visit_same_numeric_vector_pair(vector1, vector2, [](const auto& native1, const auto& native2) {
        return zip_vectors(native1, native2, "vector add",
                           [](auto element1, auto element2) { return element1 + element2; });
    });
}

// Subtract vectors elementwise with singleton broadcasting.
extern "C" closure builtin_function_vector_elementwise_sub(OperationArgs& Args)
{
    auto vector1 = Args.evaluate_slot_to_value(0);
    auto vector2 = Args.evaluate_slot_to_value(1);
    // Apply subtraction after dispatching the common native representation.
    return visit_same_numeric_vector_pair(vector1, vector2, [](const auto& native1, const auto& native2) {
        return zip_vectors(native1, native2, "vector subtract",
                           [](auto element1, auto element2) { return element1 - element2; });
    });
}

// Negate every vector element without changing its native representation.
extern "C" closure builtin_function_vector_negate(OperationArgs& Args)
{
    auto vector_value = Args.evaluate_slot_to_value(0);
    // Map the unary operation after dispatching the native representation.
    return visit_numeric_vector(vector_value, [](const auto& native_vector) {
        return map_vector(native_vector, [](auto element) { return -element; });
    });
}

// Take the absolute value of every vector element.
extern "C" closure builtin_function_vector_abs(OperationArgs& Args)
{
    auto vector_value = Args.evaluate_slot_to_value(0);
    // Map the unary operation after dispatching the native representation.
    return visit_numeric_vector(vector_value, [](const auto& native_vector) {
        return map_vector(native_vector, [](auto element) { return std::abs(element); });
    });
}

// Map vector elements to -1, 0, or 1 without changing representation.
extern "C" closure builtin_function_vector_signum(OperationArgs& Args)
{
    auto vector_value = Args.evaluate_slot_to_value(0);
    // Map the unary operation after dispatching the native representation.
    return visit_numeric_vector(vector_value, [](const auto& native_vector) {
        return map_vector(native_vector, [](auto element) {
            return (element > 0 ? 1 : 0) - (element < 0 ? 1 : 0);
        });
    });
}

// Multiply a matrix by a vector after dispatching their shared representation.
extern "C" closure builtin_function_matrixVectorNative(OperationArgs& Args)
{
    auto matrix_value = Args.evaluate_slot_to_value(0);
    auto vector_value = Args.evaluate_slot_to_value(1);
    // Evaluate the typed product after representation dispatch.
    return visit_same_matrix_vector_pair(matrix_value, vector_value,
        [](const auto& matrix, const auto& vector) {
            return multiply_matrix_vector(matrix, vector);
        });
}

// Multiply a vector by a matrix after dispatching their shared representation.
extern "C" closure builtin_function_vectorMatrixNative(OperationArgs& Args)
{
    auto vector_value = Args.evaluate_slot_to_value(0);
    auto matrix_value = Args.evaluate_slot_to_value(1);
    // Reverse the dispatch arguments while preserving the public operand order.
    return visit_same_matrix_vector_pair(matrix_value, vector_value,
        [](const auto& matrix, const auto& vector) {
            return multiply_vector_matrix(vector, matrix);
        });
}

// Form the outer product of two vectors with the same native scalar type.
extern "C" closure builtin_function_outerNative(OperationArgs& Args)
{
    auto vector1 = Args.evaluate_slot_to_value(0);
    auto vector2 = Args.evaluate_slot_to_value(1);
    // Evaluate the typed outer product after representation dispatch.
    return visit_same_numeric_vector_pair(vector1, vector2,
        [](const auto& native1, const auto& native2) {
            return outer_vectors(native1, native2);
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
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& Q = arg0.as_<Box<DenseMatrix<double>>>();
    Eigen::Index n = Q.rows();
    assert(Q.cols() == n);

    double t = Args.evaluate_slot_to_value(1).as_double();

    auto P = new Box<DenseMatrix<double>>((t*Q).exp());

    for(Eigen::Index i=0; i<n; i++)
    {
        double sum = 0;
        for(Eigen::Index j=0; j<n; j++)
        {
            (*P)(i,j) = std::max((*P)(i,j),0.0);
            sum += (*P)(i,j);
        }
        for(Eigen::Index j=0; j<n; j++)
            (*P)(i,j) /= sum;
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
    const auto& Q = arg0.as_<Box<DenseMatrix<double>>>();

    auto pi_value = Args.evaluate_slot_to_value(1);
    const auto& pi = pi_value.as_<Box<DenseVector<double>>>();

    const Eigen::Index n = Q.rows();
    assert(Q.cols() == Q.rows());
    assert(pi.size() == n);

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
    DenseVector<double> sqrt_pi(n);
    DenseVector<double> inverse_sqrt_pi(n);
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
    DenseMatrix<double> S(n,n);
    for(Eigen::Index i=0;i<n;i++)
        for(Eigen::Index j=0;j<=i;j++) {
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
    object_ptr<Box<EigenValues>> eigensolver(new Box<EigenValues>(S, ComputeEigenvectors));
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
    auto pi_value = Args.evaluate_slot_to_value(1);
    const auto& pi = pi_value.as_<Box<DenseVector<double>>>();
    double t = Args.evaluate_slot_to_value(2).as_double();

    object_ptr<Box<DenseMatrix<double>>> Mptr = new Box<DenseMatrix<double>>;
    auto& M = *Mptr;
    M = exp(L.as_<Box<EigenValues>>(), pi, t);

    double error = positivize_and_renormalize_matrix(M);

    if (error > 1.0e-9)
        return {R::RMaybe()};
    else
        return {R::RMaybe(Mptr)};
}


// Transpose either native matrix representation.
extern "C" closure builtin_function_tr(OperationArgs& Args)
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
extern "C" R::Exp simple_function_matrixAtIndex(vector<R::Exp>& args)
{
    int i = get_arg(args).as_int();
    int j = get_arg(args).as_int();
    auto arg2 = get_arg(args);
    return visit_matrix(arg2, [i,j](const auto& native_matrix) {
        return R::Exp(native_matrix(i,j));
    });
}
