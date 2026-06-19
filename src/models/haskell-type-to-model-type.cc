#include "models/haskell-type-to-model-type.H"

#include "computation/haskell/ids.H"
#include "util/myexception.H"

#include <array>
#include <optional>
#include <vector>

namespace
{

// Produces the command-line model variable names used for bridged Haskell
// variables, keeping short names stable for the common small signatures.
std::string canonical_model_type_var_name(int index)
{
    constexpr std::array<char, 26> names{
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
        'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
    };
    if (index < static_cast<int>(names.size()))
        return std::string(1, names[index]);
    return "a" + std::to_string(index);
}

// Returns the unqualified suffix of a Haskell type constructor name so bridge
// decisions can accept both Prelude-local and fully-qualified builtins.
std::string unqualified_type_con_name(const std::string& name)
{
    auto pos = name.rfind('.');
    if (pos == std::string::npos)
        return name;
    return name.substr(pos + 1);
}

// Bridges ordinary Haskell type constructors that have direct command-line
// model spelling in this first inferred-binding slice.
std::optional<CM::Type> bridge_builtin_type_con(const TypeCon& type_con)
{
    auto name = unqualified_type_con_name(type_con.name);
    if (name == "Int")
        return CM::type_con("Int");
    if (name == "Double")
        return CM::type_con("Double");
    if (name == "Bool")
        return CM::type_con("Bool");
    return {};
}

// Maps a small set of Haskell class names onto the command-line model
// constraint constructors used by existing JSON signatures.
std::optional<std::string> model_constraint_class_name(const TypeCon& type_con)
{
    auto unqualified_name = unqualified_type_con_name(type_con.name);
    if (type_con.name == eq_class_name or unqualified_name == "Eq")
        return "Eq";
    if (type_con.name == ord_class_name or unqualified_name == "Ord")
        return "Ord";
    if (type_con.name == "Compiler.Num.Num" or unqualified_name == "Num")
        return "Num";
    return {};
}

}

// Seeds model type-variable names from Haskell quantifier order so bridged
// signatures do not depend on result/argument traversal order.
void seed_haskell_type_bridge_vars(HaskellTypeBridgeState& state, const std::vector<TypeVar>& quantified_vars)
{
    for(const auto& type_var: quantified_vars)
    {
        auto [_, inserted] = state.type_vars.insert({type_var, canonical_model_type_var_name(state.next_type_var)});
        if (inserted)
            state.next_type_var++;
    }
}

// Converts one semantic Haskell Type into the existing command-line model type
// representation, rejecting unsupported shapes before they reach Rules.
CM::Type bridge_haskell_type_to_model_type(const Type& input_type, HaskellTypeBridgeState& state)
{
    auto type = follow_meta_type_var(input_type);

    if (auto type_var = type.to<TypeVar>())
    {
        auto [it, inserted] = state.type_vars.insert({*type_var, canonical_model_type_var_name(state.next_type_var)});
        if (inserted)
            state.next_type_var++;
        return CM::type_var(it->second);
    }

    if (auto meta_type_var = type.to<MetaTypeVar>())
        throw myexception()<<"Cannot bridge unresolved Haskell meta type variable '"<<meta_type_var->print()<<"' to a command-line model type";

    if (auto element_type = is_list_type(type))
        return CM::list_type(bridge_haskell_type_to_model_type(*element_type, state));

    if (auto element_types = is_tuple_type(type))
    {
        std::vector<CM::Type> bridged_elements;
        for(const auto& element_type: *element_types)
            bridged_elements.push_back(bridge_haskell_type_to_model_type(element_type, state));
        return CM::tuple_type(bridged_elements);
    }

    if (auto function = is_function_type(type))
        return CM::function_type(
            bridge_haskell_type_to_model_type(function->first, state),
            bridge_haskell_type_to_model_type(function->second, state)
        );

    if (auto type_con = type.to<TypeCon>())
        if (auto bridged = bridge_builtin_type_con(*type_con))
            return *bridged;

    throw myexception()<<"Cannot bridge unsupported Haskell type '"<<type.print()<<"' to a command-line model type";
}

// Converts a standalone semantic Haskell Type with a fresh canonical variable
// state, for tests and callers that do not need cross-type variable sharing.
CM::Type bridge_haskell_type_to_model_type(const Type& type)
{
    HaskellTypeBridgeState state;
    return bridge_haskell_type_to_model_type(type, state);
}

// Converts one semantic Haskell class predicate into an existing model-layer
// constraint, intentionally accepting only unary Eq/Ord/Num dictionaries.
CM::Type bridge_haskell_constraint_to_model_constraint(const Type& input_constraint, HaskellTypeBridgeState& state)
{
    auto constraint = follow_meta_type_var(input_constraint);

    if (is_role_equality_pred(constraint) or is_equality_pred(constraint))
        throw myexception()<<"Cannot bridge Haskell equality constraint '"<<constraint.print()<<"' to a command-line model constraint";

    auto dictionary = is_dictionary_pred(constraint);
    if (not dictionary)
        throw myexception()<<"Cannot bridge non-dictionary Haskell constraint '"<<constraint.print()<<"' to a command-line model constraint";

    auto& [head, args] = *dictionary;
    auto class_con = head.to<TypeCon>();
    if (not class_con)
        throw myexception()<<"Cannot bridge Haskell constraint '"<<constraint.print()<<"' with non-constructor class head";

    if (args.size() != 1)
        throw myexception()<<"Cannot bridge Haskell constraint '"<<constraint.print()<<"' with "<<args.size()<<" arguments; only unary class constraints are supported";

    auto class_name = model_constraint_class_name(*class_con);
    if (not class_name)
        throw myexception()<<"Cannot bridge unsupported Haskell class constraint '"<<constraint.print()<<"'";

    return CM::type_app(CM::type_con(*class_name), bridge_haskell_type_to_model_type(args[0], state));
}
