#include "models/model-type.H"

#include "util/string/join.H"

#include <algorithm>

using std::set;
using std::string;
using std::vector;

namespace CmdModel
{

// Constructs an atomic type from parser/rule spelling, preserving the old
// lowercase-name convention for type variables.
Type::Type(string name)
    :Type(type_atom(std::move(name)))
{}

Type::Type(const char* name)
    :Type(string(name))
{}

// Reports whether this type is the explicit empty type sentinel.
bool Type::is_null() const
{
    return is<NoType>();
}

// Preserves the command-line convention that lowercase type atoms are type
// variables, including freshened names like a#0.
bool is_type_variable_name(const string& name)
{
    if (name.empty()) return false;
    char first_letter = name[0];
    return first_letter >= 'a' and first_letter <= 'z';
}

// Reports whether a parsed or generated type is a type variable.
bool is_type_variable(const Type& type)
{
    return type.is<TypeVar>();
}

// Reports whether a type is the unification wildcard atom.
bool is_wildcard(const Type& type)
{
    if (auto tc = type.to<TypeCon>())
        return tc->name == "_";
    if (auto tv = type.to<TypeVar>())
        return tv->name == "_";
    return false;
}

// Builds a type constructor atom without applying lowercase variable rules.
Type type_con(string name)
{
    return TypeCon{std::move(name)};
}

// Builds a type variable atom without checking the spelling convention.
Type type_var(string name)
{
    return TypeVar{std::move(name)};
}

// Builds a command-line type atom, classifying lowercase names as variables.
Type type_atom(string name)
{
    if (is_type_variable_name(name))
        return type_var(std::move(name));
    else
        return type_con(std::move(name));
}

// Builds one type application node.
Type type_app(Type head, Type arg)
{
    return TypeApp{head, arg};
}

// Applies a type head to a sequence of arguments from left to right.
Type type_apps(Type type, const vector<Type>& args)
{
    for(auto& arg: args)
    {
        auto next = type_app(type, arg);
        type = next;
    }
    return type;
}

// Builds the command-line list type for one element type.
Type list_type(Type element)
{
    return type_app(type_con("List"), std::move(element));
}

// Builds the command-line tuple type for the supplied element types.
Type tuple_type(const vector<Type>& elements)
{
    return type_apps(type_con("Tuple"), elements);
}

// Builds the command-line function type for one argument and result.
Type function_type(Type arg, Type result)
{
    return type_apps(type_con("Function"), {std::move(arg), std::move(result)});
}

// Decomposes nested type applications into their head and argument list.
std::pair<Type, vector<Type>> get_type_apps(Type type)
{
    vector<Type> args;

    while(auto app = type.to<TypeApp>())
    {
        auto arg = app->arg;
        auto head = app->head;
        args.push_back(std::move(arg));
        type = std::move(head);
    }
    std::reverse(args.begin(), args.end());

    return {type, args};
}

// Returns only the head of a nested type application.
Type get_type_head(Type type)
{
    auto [head, args] = get_type_apps(std::move(type));
    return head;
}

// Collects every type variable name referenced inside a type.
set<string> find_variables_in_type(const Type& type)
{
    set<string> vars;
    // Visits the current type node and recurses into application children.
    type.visit([&](const auto& node)
    {
        using T = std::decay_t<decltype(node)>;

        if constexpr (std::is_same_v<T, TypeVar>)
            vars.insert(node.name);
        else if constexpr (std::is_same_v<T, TypeApp>)
        {
            auto head_vars = find_variables_in_type(node.head);
            auto arg_vars = find_variables_in_type(node.arg);
            vars.insert(head_vars.begin(), head_vars.end());
            vars.insert(arg_vars.begin(), arg_vars.end());
        }
    });
    return vars;
}

// Renders a native type using the command-line type syntax.
std::string unparse_type(const Type& type)
{
    if (type.is_null()) return "NOTYPE";

    auto [head, args] = get_type_apps(type);

    vector<string> sargs;
    for(const auto& arg: args)
        sargs.push_back(unparse_type(arg));

    if (auto tc = head.to<TypeCon>(); tc and tc->name == "Tuple")
        return "(" + join(sargs,",") + ")";
    else if (auto tc = head.to<TypeCon>(); tc and tc->name == "Function")
    {
        assert(sargs.size() == 2);
        return sargs[0] + " -> " + sargs[1];
    }
    else if (not args.empty())
        return unparse_type(head) + "<" + join(sargs, ',') + ">";
    else if (auto tc = head.to<TypeCon>())
        return tc->name;
    else if (auto tv = head.to<TypeVar>())
        return tv->name;
    else
        return "NOTYPE";
}

namespace
{

// Assigns variant alternatives a stable order for structural comparison.
int type_rank(const Type& type)
{
    // Maps each type node alternative to its ordering rank.
    return type.visit([](const auto& node)
    {
        using T = std::decay_t<decltype(node)>;

        if constexpr (std::is_same_v<T, NoType>) return 0;
        else if constexpr (std::is_same_v<T, TypeCon>) return 1;
        else if constexpr (std::is_same_v<T, TypeVar>) return 2;
        else return 3;
    });
}

}

// Compares two native types structurally.
bool operator==(const Type& t1, const Type& t2)
{
    if (type_rank(t1) != type_rank(t2))
        return false;

    // Compares the matching variant alternatives after the rank check.
    return t1.visit([&](const auto& n1)
    {
        using T = std::decay_t<decltype(n1)>;
        auto n2 = t2.to<T>();
        assert(n2);

        if constexpr (std::is_same_v<T, NoType>)
            return true;
        else if constexpr (std::is_same_v<T, TypeCon>)
            return n1.name == n2->name;
        else if constexpr (std::is_same_v<T, TypeVar>)
            return n1.name == n2->name;
        else
            return n1.head == n2->head and n1.arg == n2->arg;
    });
}

// Compares two native types for structural inequality.
bool operator!=(const Type& t1, const Type& t2)
{
    return not (t1 == t2);
}

// Orders native types structurally for use in std::set and std::map.
bool operator<(const Type& t1, const Type& t2)
{
    if (type_rank(t1) != type_rank(t2))
        return type_rank(t1) < type_rank(t2);

    // Compares the matching variant alternatives after the rank check.
    return t1.visit([&](const auto& n1)
    {
        using T = std::decay_t<decltype(n1)>;
        auto n2 = t2.to<T>();
        assert(n2);

        if constexpr (std::is_same_v<T, NoType>)
            return false;
        else if constexpr (std::is_same_v<T, TypeCon>)
            return n1.name < n2->name;
        else if constexpr (std::is_same_v<T, TypeVar>)
            return n1.name < n2->name;
        else if (n1.head != n2->head)
            return n1.head < n2->head;
        else
            return n1.arg < n2->arg;
    });
}

}
