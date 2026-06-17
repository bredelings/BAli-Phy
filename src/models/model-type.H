#ifndef MODELS_MODEL_TYPE_H
#define MODELS_MODEL_TYPE_H

#include <cassert>
#include <optional>
#include <set>
#include <string>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "models/model-box.H"

namespace CmdModel
{

struct Type;
struct TypeApp;

struct NoType
{};

struct TypeCon
{
    std::string name;
};

struct TypeVar
{
    std::string name;
};

template<class T>
struct is_boxed_type_node: std::false_type {};

template<>
struct is_boxed_type_node<TypeApp>: std::true_type {};

template<class T>
struct is_boxed_type_storage: std::false_type {};

template<class T>
struct is_boxed_type_storage<Box<T>>: is_boxed_type_node<T> {};

struct Type
{
    using Node = std::variant<
        NoType,
        TypeCon,
        TypeVar,
        Box<TypeApp>
    >;

    Node node;

    Type() = default;
    Type(const char*);
    Type(std::string);

    template<class T>
        requires (!std::is_same_v<std::decay_t<T>, Type> and !std::is_convertible_v<T, std::string>)
    Type(T);

    bool is_null() const;

    template<class T>       T* to();
    template<class T> const T* to() const;

    template<class T>       T& as();
    template<class T> const T& as() const;

    template<class T>
    bool is() const {return to<T>();}

    template<class F>
    decltype(auto) visit(F&&);

    template<class F>
    decltype(auto) visit(F&&) const;
};

struct TypeApp
{
    Type head;
    Type arg;
};

bool operator==(const Type&, const Type&);
bool operator!=(const Type&, const Type&);
bool operator<(const Type&, const Type&);

bool is_type_variable_name(const std::string&);
bool is_type_variable(const Type&);
bool is_wildcard(const Type&);

Type type_con(std::string);
Type type_var(std::string);
Type type_atom(std::string);
Type type_app(Type, Type);
Type type_apps(Type, const std::vector<Type>&);
Type list_type(Type);
Type tuple_type(const std::vector<Type>&);
Type function_type(Type, Type);

std::pair<Type, std::vector<Type>> get_type_apps(Type);
Type get_type_head(Type);

std::set<std::string> find_variables_in_type(const Type&);
std::string unparse_type(const Type&);

// Constructs a type and boxes recursive type node variants.
template<class T>
    requires (!std::is_same_v<std::decay_t<T>, Type> and !std::is_convertible_v<T, std::string>)
Type::Type(T node_)
{
    if constexpr (is_boxed_type_node<T>::value)
        node.template emplace<Box<T>>(new T(node_));
    else
        node = std::move(node_);
}

// Returns the requested type node, unwrapping boxed recursive nodes.
template<class T>
T* Type::to()
{
    if constexpr (is_boxed_type_node<T>::value)
    {
        if (auto box = std::get_if<Box<T>>(&node))
            return &box->get();
        else
            return nullptr;
    }
    else
        return std::get_if<T>(&node);
}

// Returns the requested const type node, unwrapping boxed recursive nodes.
template<class T>
const T* Type::to() const
{
    if constexpr (is_boxed_type_node<T>::value)
    {
        if (auto box = std::get_if<Box<T>>(&node))
            return &box->get();
        else
            return nullptr;
    }
    else
        return std::get_if<T>(&node);
}

// Returns the requested type node and asserts that the node type matches.
template<class T>
T& Type::as()
{
    auto ptr = to<T>();
    assert(ptr);
    return *ptr;
}

// Returns the requested const type node and asserts that the node type matches.
template<class T>
const T& Type::as() const
{
    auto ptr = to<T>();
    assert(ptr);
    return *ptr;
}

// Visits a type node, unwrapping boxed recursive nodes before dispatch.
template<class F>
decltype(auto) Type::visit(F&& f)
{
    // Unwrap boxed recursive storage before invoking the visitor.
    return std::visit([&](auto& node) -> decltype(auto)
    {
        using T = std::decay_t<decltype(node)>;

        if constexpr (is_boxed_type_storage<T>::value)
            return std::forward<F>(f)(node.get());
        else
            return std::forward<F>(f)(node);
    }, node);
}

// Visits a const type node, unwrapping boxed recursive nodes before dispatch.
template<class F>
decltype(auto) Type::visit(F&& f) const
{
    // Unwrap boxed recursive storage before invoking the visitor.
    return std::visit([&](const auto& node) -> decltype(auto)
    {
        using T = std::decay_t<decltype(node)>;

        if constexpr (is_boxed_type_storage<T>::value)
            return std::forward<F>(f)(node.get());
        else
            return std::forward<F>(f)(node);
    }, node);
}

}

namespace CM = CmdModel;

#endif
