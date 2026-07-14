#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "util/dense-matrix.H"
#include "util/myexception.H"

#include <limits>
#include <type_traits>
#include <vector>

using std::vector;

namespace
{

// Read a runtime scalar using the representation selected by its native
// vector element type.
template <typename T>
T native_scalar(const R::Exp& value)
{
    if constexpr (std::is_same_v<T, int>)
        return value.as_int();
    else
        return value.as_double();
}

// Allocate a native constant vector using the scalar's runtime representation.
template <typename T>
closure constant_vector(T value, int count)
{
    if (count < 0)
        throw myexception()<<"constant vector: negative extent "<<count;
    auto result = new Box<DenseVector<T>>(count);
    result->setConstant(value);
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
    if (expected_size == 0)
        return result;

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
        int tail = -1;
        if (k + 1 < expected_size)
            tail = xs_closure.reg_for_constructor_slot(1);
        int value = Args.evaluate_reg_dependent_use(element);
        (*result)(k) = native_scalar<T>(Args.memory().closure_at(value).get_code());
        if (k + 1 < expected_size)
            xs = Args.evaluate_reg_dependent_use(tail);
    }
    return result;
}

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

// Construct a constant vector without allocating a Haskell element list.
extern "C" closure builtin_function_vectorKonstNative(OperationArgs& Args)
{
    auto value = Args.evaluate_slot_to_value(0);
    int count = Args.evaluate_slot_to_value(1).as_int();
    if (value.is_int())
        return constant_vector(value.as_int(), count);
    if (value.is_double())
        return constant_vector(value.as_double(), count);
    throw myexception()<<"constant vector: unsupported scalar representation";
}

// Return one native vector element after checking its zero-based index.
extern "C" R::Exp simple_function_vectorAtIndex(vector<R::Exp>& args)
{
    auto value = get_arg(args);
    int index = get_arg(args).as_int();
    if (value.is_a<Box<DenseVector<int>>>() )
    {
        const auto& native = value.as_<Box<DenseVector<int>>>();
        if (index < 0 or index >= native.size())
            throw myexception()<<"vector atIndex: index "<<index
                               <<" is outside vector length "<<native.size();
        return native(index);
    }
    if (value.is_a<Box<DenseVector<double>>>() )
    {
        const auto& native = value.as_<Box<DenseVector<double>>>();
        if (index < 0 or index >= native.size())
            throw myexception()<<"vector atIndex: index "<<index
                               <<" is outside vector length "<<native.size();
        return native(index);
    }
    throw myexception()<<"Unsupported native vector representation "<<value.print();
}

// Report the physical extent of a complete native Int owner so Haskell can
// establish the initial logical length of an offset-zero unboxed vector.
extern "C" R::Exp simple_function_intVectorSize(vector<R::Exp>& args)
{
    auto value = get_arg(args);
    auto count = value.as_<Box<DenseVector<int>>>().size();
    if (count > std::numeric_limits<int>::max())
        throw myexception()<<"native Int vector length exceeds the Haskell Int range";
    return static_cast<int>(count);
}

// Report the physical extent of a complete native Double owner so Haskell can
// establish the initial logical length of an offset-zero unboxed vector.
extern "C" R::Exp simple_function_doubleVectorSize(vector<R::Exp>& args)
{
    auto value = get_arg(args);
    auto count = value.as_<Box<DenseVector<double>>>().size();
    if (count > std::numeric_limits<int>::max())
        throw myexception()<<"native Double vector length exceeds the Haskell Int range";
    return static_cast<int>(count);
}

// Read an Int element after the Haskell view has established its bounds.
extern "C" R::Exp simple_function_unsafeIntIndex(vector<R::Exp>& args)
{
    auto value = get_arg(args);
    int index = get_arg(args).as_int();
    return value.as_<Box<DenseVector<int>>>()(index);
}

// Read a Double element after the Haskell view has established its bounds.
extern "C" R::Exp simple_function_unsafeDoubleIndex(vector<R::Exp>& args)
{
    auto value = get_arg(args);
    int index = get_arg(args).as_int();
    return value.as_<Box<DenseVector<double>>>()(index);
}
