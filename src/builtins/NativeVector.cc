#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "builtins/haskell-list-input.hh"
#include "builtins/native-vector-input.hh"
#include "computation/haskell/ids.hh"
#include "computation/machine/args.hh"
#include "util/dense-matrix.hh"
#include "util/myexception.hh"

#include <algorithm>
#include <limits>
#include <string_view>
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
    auto size_arg = Args.evaluate_slot_to_value_with_contingency(0);
    int expected_size = size_arg.value.as_int();
    if (expected_size < 0)
        throw myexception()<<"vector (|>): size must be nonnegative, but got "<<expected_size;

    object_ptr<Box<DenseVector<T>>> result = new Box<DenseVector<T>>(expected_size);
    if (expected_size == 0)
        return result;

    auto xs = Args.evaluate_reg_use_with_contingency(
        Args.reg_for_slot(1), size_arg.edge_contingency);
    for(int k=0; k<expected_size; k++)
    {
        const closure& xs_closure = Args.memory().closure_at(xs.value_reg);
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
        auto value = Args.evaluate_reg_use(element, xs.edge_contingency);
        (*result)(k) = native_scalar<T>(Args.memory().closure_at(value).get_code());
        if (k + 1 < expected_size)
            xs = Args.evaluate_reg_use_with_contingency(tail, xs.edge_contingency);
    }
    return result;
}

// Decode one U.Vector numeric constructor through dependent USEs of its view fields.
// The returned input retains the native owner while exposing only the validated logical span.
template<typename T>
NativeVectorInput<T> read_unboxed_numeric_vector(OperationArgs& Args, int vector_reg,
                                                  EdgeContingency contingency,
                                                  std::string_view constructor_name)
{
    auto vector = Args.evaluate_reg_use_with_contingency(vector_reg, contingency);
    const closure& vector_closure = Args.memory().closure_at(vector.value_reg);
    auto constructor = vector_closure.get_code().to<Runtime::ConstructorApp>();
    if (not constructor or get_unqualified_name(constructor->head.name()) != constructor_name or
        constructor->head.n_args() != 3 or constructor->args.size() != 3)
        throw myexception()<<"Data.Vector.Unboxed.concat: expected "<<constructor_name
                           <<", but got "<<vector_closure.get_code().print();

    int offset_reg = vector_closure.reg_for_constructor_slot(0);
    int length_reg = vector_closure.reg_for_constructor_slot(1);
    int owner_reg = vector_closure.reg_for_constructor_slot(2);

    int offset_value_reg = Args.evaluate_reg_use(offset_reg, vector.edge_contingency);
    auto offset_value = Args.memory().closure_at(offset_value_reg).get_code();
    if (not offset_value.is_int())
        throw myexception()<<"Data.Vector.Unboxed.concat: "<<constructor_name<<" offset is not an Int";

    int length_value_reg = Args.evaluate_reg_use(length_reg, vector.edge_contingency);
    auto length_value = Args.memory().closure_at(length_value_reg).get_code();
    if (not length_value.is_int())
        throw myexception()<<"Data.Vector.Unboxed.concat: "<<constructor_name<<" length is not an Int";

    int owner_value_reg = Args.evaluate_reg_use(owner_reg, vector.edge_contingency);
    const auto& owner_value = Args.memory().closure_at(owner_value_reg).get_code();
    auto object_value = owner_value.template to<Runtime::ObjectValue>();
    object_ptr<const Box<DenseVector<T>>> owner;
    if (object_value)
        owner = boost::static_pointer_cast<const Box<DenseVector<T>>>(object_value->value);
    if (not owner)
        throw myexception()<<"Data.Vector.Unboxed.concat: "<<constructor_name
                           <<" owner has the wrong native representation";

    return NativeVectorInput<T>(std::move(owner), offset_value.as_int(), length_value.as_int(),
                                "Data.Vector.Unboxed.concat");
}

// Copy validated numeric views into the pre-sized result while walking the input list once.
// The supplied cached length is checked against the amount of data actually encountered.
template<typename T>
closure concat_unboxed_numeric_vectors(OperationArgs& Args, std::string_view constructor_name)
{
    auto count_arg = Args.evaluate_slot_to_value_with_contingency(0);
    if (not count_arg.value.is_int())
        throw myexception()<<"Data.Vector.Unboxed.concat: result length is not an Int";
    int expected_count = count_arg.value.as_int();
    if (expected_count < 0)
        throw myexception()<<"Data.Vector.Unboxed.concat: negative result length "<<expected_count;

    object_ptr<Box<DenseVector<T>>> result = new Box<DenseVector<T>>(expected_count);
    std::size_t total = static_cast<std::size_t>(expected_count);
    std::size_t offset = 0;
    auto xs = Args.evaluate_reg_use_with_contingency(Args.reg_for_slot(1), count_arg.edge_contingency);

    // Decode and copy one view at a time so only the current input owner must be retained.
    // Every constructor and owner USE still receives the list cell's controlling contingency.
    for_each_haskell_list_element(Args, xs, "Data.Vector.Unboxed.concat",
        [&](int vector_reg, EdgeContingency contingency)
        {
            auto input = read_unboxed_numeric_vector<T>(Args, vector_reg, contingency, constructor_name);
            auto values = input.view();
            if (values.size() > total - offset)
                throw myexception()<<"Data.Vector.Unboxed.concat: input lengths exceed cached result length";
            std::copy(values.begin(), values.end(), result->data() + offset);
            offset += values.size();
        });

    if (offset != total)
        throw myexception()<<"Data.Vector.Unboxed.concat: input lengths do not match cached result length";
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

// Concatenate sliced Int views without materializing either elements or descriptors in Haskell.
extern "C" closure builtin_function_concatIntVectors(OperationArgs& Args)
{
    return concat_unboxed_numeric_vectors<int>(Args, "V_Int");
}

// Concatenate sliced Double views through the same native numeric implementation.
extern "C" closure builtin_function_concatDoubleVectors(OperationArgs& Args)
{
    return concat_unboxed_numeric_vectors<double>(Args, "V_Double");
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
