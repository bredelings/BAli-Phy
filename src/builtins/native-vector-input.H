#ifndef BUILTINS_NATIVE_VECTOR_INPUT_H
#define BUILTINS_NATIVE_VECTOR_INPUT_H

#include <span>
#include <string_view>
#include <type_traits>
#include <utility>

#include "builtins/native-vector-view.H"
#include "computation/machine/args.H"
#include "computation/object.H"

enum class ForeignDemand
{
    use,
    force
};

namespace native_vector_input_detail
{
    template<typename T>
    inline constexpr bool supported_element =
        std::is_same_v<T, int> or std::is_same_v<T, double>;

    template<typename T>
    constexpr std::string_view element_name();

    template<>
    constexpr std::string_view element_name<int>()
    {
        return "Int";
    }

    template<>
    constexpr std::string_view element_name<double>()
    {
        return "Double";
    }

    template<ForeignDemand Demand>
    Runtime::Exp evaluate_slot(OperationArgs& args, int slot)
    {
        if constexpr (Demand == ForeignDemand::use)
            return args.evaluate_slot_to_value(slot);
        else
            return args.evaluate_slot_to_value_(slot);
    }
}

template<typename T>
class NativeVectorInput
{
    static_assert(native_vector_input_detail::supported_element<T>,
                  "NativeVectorInput supports only Int and Double vectors");

    using owner_type = object_ptr<const Box<DenseVector<T>>>;

    owner_type owner_;
    std::span<const T> view_;

public:
    NativeVectorInput(owner_type owner, int offset, int length,
                      std::string_view argument_name)
        : owner_(std::move(owner)),
          view_(checked_native_vector_view(*owner_, offset, length,
                                           argument_name))
    {
    }

    std::span<const T> view() const & noexcept
    {
        return view_;
    }

    std::span<const T> view() const && = delete;
};

template<typename T, ForeignDemand Demand>
NativeVectorInput<T>
read_native_vector_input(OperationArgs& args, int first_slot,
                         std::string_view argument_name)
{
    static_assert(native_vector_input_detail::supported_element<T>,
                  "read_native_vector_input supports only Int and Double vectors");

    if (first_slot < 0)
        throw myexception()<<argument_name<<": negative first raw slot "
                           <<first_slot;
    if (first_slot > args.n_args() or args.n_args() - first_slot < 3)
        throw myexception()<<argument_name<<" (raw group beginning at slot "
                           <<first_slot<<"): expected three slots, but operation has "
                           <<args.n_args();

    auto offset_value =
        native_vector_input_detail::evaluate_slot<Demand>(args, first_slot);
    if (not offset_value.is_int())
        throw myexception()<<argument_name<<" (raw slots "<<first_slot<<".."
                           <<first_slot + 2<<"): expected Int offset at slot "
                           <<first_slot;
    int offset = offset_value.as_int();

    auto length_value =
        native_vector_input_detail::evaluate_slot<Demand>(args, first_slot + 1);
    if (not length_value.is_int())
        throw myexception()<<argument_name<<" (raw slots "<<first_slot<<".."
                           <<first_slot + 2<<"): expected Int length at slot "
                           <<first_slot + 1;
    int length = length_value.as_int();

    auto owner_value =
        native_vector_input_detail::evaluate_slot<Demand>(args, first_slot + 2);
    auto object_value = owner_value.template to<Runtime::ObjectValue>();
    object_ptr<const Box<DenseVector<T>>> owner;
    if (object_value)
        owner = boost::dynamic_pointer_cast<const Box<DenseVector<T>>>(
            object_value->value);
    if (not owner)
        throw myexception()<<argument_name<<" (raw slots "<<first_slot<<".."
                           <<first_slot + 2<<"): expected NativeVector "
                           <<native_vector_input_detail::element_name<T>()
                           <<" owner at slot "<<first_slot + 2;

    return NativeVectorInput<T>(std::move(owner), offset, length, argument_name);
}

#endif
