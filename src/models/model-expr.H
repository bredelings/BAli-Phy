#ifndef MODELS_MODEL_EXPR_H
#define MODELS_MODEL_EXPR_H

#include <cassert>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "models/model-box.H"
#include "models/model-type.H"

namespace CmdModel
{

using NoAnn = std::monostate;

template<class A>
struct Expr;

template<class A>
struct Pattern;

template<class A>
struct TuplePattern;

template<class A>
struct Arg;

template<class A>
struct Call;

template<class A>
struct List;

template<class A>
struct Tuple;

template<class A>
struct Let;

template<class A>
struct Lambda;

template<class A>
struct Sample;

template<class A>
using Decls = std::vector<std::pair<std::string, Expr<A>>>;

using UntypedExpr = Expr<NoAnn>;

struct IntLiteral
{
    int value;
};

struct DoubleLiteral
{
    double value;
};

struct BoolLiteral
{
    bool value;
};

struct StringLiteral
{
    std::string value;
};

struct Var
{
    std::string name;
};

struct ArgRef
{
    std::string name;
};

struct Placeholder
{};

struct GetState
{
    std::string state_name;
};

struct Ann
{
    Type type;
    std::set<std::string> used_args;

    bool no_log = false;
    std::optional<std::string> extract;
};

struct VarPattern
{
    std::string name;
};

template<class T>
struct is_boxed_pattern_node: std::false_type {};

template<class A>
struct is_boxed_pattern_node<TuplePattern<A>>: std::true_type {};

template<class T>
struct is_boxed_pattern_storage: std::false_type {};

template<class T>
struct is_boxed_pattern_storage<Box<T>>: is_boxed_pattern_node<T> {};

template<class A>
struct Pattern
{
    [[no_unique_address]] A ann;

    using Node = std::variant<
        VarPattern,
        Box<TuplePattern<A>>
    >;

    Node node;

    Pattern() = default;

    template<class T>
    Pattern(A, T);

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

using UntypedPattern = Pattern<NoAnn>;
using TypedPattern = Pattern<Ann>;

template<class A>
struct TuplePattern
{
    std::vector<Pattern<A>> elements;
};

template<class T>
struct is_boxed_expr_node: std::false_type {};

template<class A>
struct is_boxed_expr_node<Call<A>>: std::true_type {};

template<class A>
struct is_boxed_expr_node<List<A>>: std::true_type {};

template<class A>
struct is_boxed_expr_node<Tuple<A>>: std::true_type {};

template<class A>
struct is_boxed_expr_node<Let<A>>: std::true_type {};

template<class A>
struct is_boxed_expr_node<Lambda<A>>: std::true_type {};

template<class A>
struct is_boxed_expr_node<Sample<A>>: std::true_type {};

template<class T>
struct is_boxed_expr_storage: std::false_type {};

template<class T>
struct is_boxed_expr_storage<Box<T>>: is_boxed_expr_node<T> {};

using TypedExpr = Expr<Ann>;
using TypedDecls = Decls<Ann>;

template<class A>
struct Expr
{
    [[no_unique_address]] A ann;

    using Node = std::variant<
        IntLiteral,
        DoubleLiteral,
        BoolLiteral,
        StringLiteral,
        Var,
        ArgRef,
        Placeholder,
        GetState,
        Box<Call<A>>,
        Box<List<A>>,
        Box<Tuple<A>>,
        Box<Let<A>>,
        Box<Lambda<A>>,
        Box<Sample<A>>
    >;

    Node node;

    Expr() = default;

    template<class T>
    Expr(A, T);

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

template<class A>
struct Arg
{
    std::string name;
    std::optional<Expr<A>> value;

    bool is_default_value = false;
    bool suppress_default = false;

    std::optional<Expr<A>> alphabet;
};

// Returns the expression stored on an argument edge, and fails clearly if the
// edge is only an absent-value placeholder.
template<class A>
Expr<A>& require_arg_value(Arg<A>& arg)
{
    if (not arg.value)
        throw std::logic_error("model argument has no expression value");
    return *arg.value;
}

// Returns the expression stored on a const argument edge, and fails clearly if
// the edge is only an absent-value placeholder.
template<class A>
const Expr<A>& require_arg_value(const Arg<A>& arg)
{
    if (not arg.value)
        throw std::logic_error("model argument has no expression value");
    return *arg.value;
}

template<class A>
struct Call
{
    std::string function;
    std::vector<Arg<A>> args;
};

template<class A>
struct List
{
    std::vector<Expr<A>> elements;
};

template<class A>
struct Tuple
{
    std::vector<Expr<A>> elements;
};

template<class A>
struct Let
{
    Decls<A> decls;
    Expr<A> body;
};

template<class A>
struct Lambda
{
    Pattern<A> pattern;
    Expr<A> body;
};

template<class A>
struct Sample
{
    Expr<A> dist;
};

// Constructs a pattern and boxes recursive pattern node variants.
template<class A>
template<class T>
Pattern<A>::Pattern(A ann_, T node_)
    :ann(std::move(ann_))
{
    if constexpr (is_boxed_pattern_node<T>::value)
        node = Box<T>(new T(std::move(node_)));
    else
        node = std::move(node_);
}

// Constructs an expression and boxes recursive expression node variants.
template<class A>
template<class T>
Expr<A>::Expr(A ann_, T node_)
    :ann(std::move(ann_))
{
    if constexpr (is_boxed_expr_node<T>::value)
        node = Box<T>(new T(std::move(node_)));
    else
        node = std::move(node_);
}

// Returns the requested pattern node, unwrapping boxed recursive nodes.
template<class A>
template<class T>
T* Pattern<A>::to()
{
    if constexpr (is_boxed_pattern_node<T>::value)
    {
        if (auto box = std::get_if<Box<T>>(&node))
            return &box->get();
        else
            return nullptr;
    }
    else
        return std::get_if<T>(&node);
}

// Returns the requested const pattern node, unwrapping boxed recursive nodes.
template<class A>
template<class T>
const T* Pattern<A>::to() const
{
    if constexpr (is_boxed_pattern_node<T>::value)
    {
        if (auto box = std::get_if<Box<T>>(&node))
            return &box->get();
        else
            return nullptr;
    }
    else
        return std::get_if<T>(&node);
}

// Returns the requested pattern node and asserts that the node type matches.
template<class A>
template<class T>
T& Pattern<A>::as()
{
    auto ptr = to<T>();
    assert(ptr);
    return *ptr;
}

// Returns the requested const pattern node and asserts that the node type matches.
template<class A>
template<class T>
const T& Pattern<A>::as() const
{
    auto ptr = to<T>();
    assert(ptr);
    return *ptr;
}

// Visits a pattern node, unwrapping boxed recursive nodes before dispatch.
template<class A>
template<class F>
decltype(auto) Pattern<A>::visit(F&& f)
{
    // Unwrap boxed recursive storage before invoking the visitor.
    return std::visit([&](auto& node) -> decltype(auto)
    {
        using T = std::decay_t<decltype(node)>;

        if constexpr (is_boxed_pattern_storage<T>::value)
            return std::forward<F>(f)(node.get());
        else
            return std::forward<F>(f)(node);
    }, node);
}

// Visits a const pattern node, unwrapping boxed recursive nodes before dispatch.
template<class A>
template<class F>
decltype(auto) Pattern<A>::visit(F&& f) const
{
    // Unwrap boxed recursive storage before invoking the visitor.
    return std::visit([&](const auto& node) -> decltype(auto)
    {
        using T = std::decay_t<decltype(node)>;

        if constexpr (is_boxed_pattern_storage<T>::value)
            return std::forward<F>(f)(node.get());
        else
            return std::forward<F>(f)(node);
    }, node);
}

// Returns the requested expression node, unwrapping boxed recursive nodes.
template<class A>
template<class T>
T* Expr<A>::to()
{
    if constexpr (is_boxed_expr_node<T>::value)
    {
        if (auto box = std::get_if<Box<T>>(&node))
            return &box->get();
        else
            return nullptr;
    }
    else
        return std::get_if<T>(&node);
}

// Returns the requested const expression node, unwrapping boxed recursive nodes.
template<class A>
template<class T>
const T* Expr<A>::to() const
{
    if constexpr (is_boxed_expr_node<T>::value)
    {
        if (auto box = std::get_if<Box<T>>(&node))
            return &box->get();
        else
            return nullptr;
    }
    else
        return std::get_if<T>(&node);
}

// Returns the requested expression node and asserts that the node type matches.
template<class A>
template<class T>
T& Expr<A>::as()
{
    auto ptr = to<T>();
    assert(ptr);
    return *ptr;
}

// Returns the requested const expression node and asserts that the node type matches.
template<class A>
template<class T>
const T& Expr<A>::as() const
{
    auto ptr = to<T>();
    assert(ptr);
    return *ptr;
}

// Visits an expression node, unwrapping boxed recursive nodes before dispatch.
template<class A>
template<class F>
decltype(auto) Expr<A>::visit(F&& f)
{
    // Unwrap boxed recursive storage before invoking the visitor.
    return std::visit([&](auto& node) -> decltype(auto)
    {
        using T = std::decay_t<decltype(node)>;

        if constexpr (is_boxed_expr_storage<T>::value)
            return std::forward<F>(f)(node.get());
        else
            return std::forward<F>(f)(node);
    }, node);
}

// Visits a const expression node, unwrapping boxed recursive nodes before dispatch.
template<class A>
template<class F>
decltype(auto) Expr<A>::visit(F&& f) const
{
    // Unwrap boxed recursive storage before invoking the visitor.
    return std::visit([&](const auto& node) -> decltype(auto)
    {
        using T = std::decay_t<decltype(node)>;

        if constexpr (is_boxed_expr_storage<T>::value)
            return std::forward<F>(f)(node.get());
        else
            return std::forward<F>(f)(node);
    }, node);
}

template<class... Ts>
struct overloaded: Ts...
{
    using Ts::operator()...;
};

template<class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

// Returns an owning copy of a model pattern.
template<class A>
Pattern<A> clone(const Pattern<A>& pattern)
{
    return pattern;
}

// Returns an owning copy of a model expression.
template<class A>
Expr<A> clone(const Expr<A>& expr)
{
    return expr;
}

// Returns an owning copy of a call argument edge.
template<class A>
Arg<A> clone(const Arg<A>& arg)
{
    return arg;
}

// Returns an owning copy of a model declaration list.
template<class A>
Decls<A> clone(const Decls<A>& decls)
{
    return decls;
}

// Reports whether an expression is a call to the requested function name.
template<class A>
bool is_call_named(const Expr<A>& expr, std::string_view name)
{
    if (auto call = expr.template to<Call<A>>())
        return call->function == name;
    else
        return false;
}

// Reports whether an expression is a list node.
template<class A>
bool is_list(const Expr<A>& expr)
{
    return expr.template is<List<A>>();
}

// Reports whether an expression is a tuple node.
template<class A>
bool is_tuple(const Expr<A>& expr)
{
    return expr.template is<Tuple<A>>();
}

// Reports whether an expression is a sample node.
template<class A>
bool is_sample(const Expr<A>& expr)
{
    return expr.template is<Sample<A>>();
}

// Reports whether an expression is a get_state node.
template<class A>
bool is_get_state(const Expr<A>& expr)
{
    return expr.template is<GetState>();
}

// Applies a visitor to each mutable child pattern stored under this pattern.
template<class A, class F>
void for_each_child(Pattern<A>& pattern, F&& f)
{
    pattern.visit(overloaded{
        // Visits each mutable tuple-pattern element.
        [&](TuplePattern<A>& tuple)
        {
            for(auto& element: tuple.elements)
                f(element);
        },
        [](auto&) {}
    });
}

// Applies a visitor to each const child pattern stored under this pattern.
template<class A, class F>
void for_each_child(const Pattern<A>& pattern, F&& f)
{
    pattern.visit(overloaded{
        // Visits each const tuple-pattern element.
        [&](const TuplePattern<A>& tuple)
        {
            for(auto& element: tuple.elements)
                f(element);
        },
        [](const auto&) {}
    });
}

// Applies a visitor to each mutable child expression stored under this node,
// including argument alphabets but skipping absent argument values.
template<class A, class F>
void for_each_child(Expr<A>& expr, F&& f)
{
    expr.visit(overloaded{
        // Visits call argument values and alphabet metadata stored on argument
        // edges.
        [&](Call<A>& call)
        {
            for(auto& arg: call.args)
            {
                if (arg.value)
                    f(*arg.value);
                if (arg.alphabet)
                    f(*arg.alphabet);
            }
        },
        // Visits each mutable list element.
        [&](List<A>& list)
        {
            for(auto& element: list.elements)
                f(element);
        },
        // Visits each mutable tuple element.
        [&](Tuple<A>& tuple)
        {
            for(auto& element: tuple.elements)
                f(element);
        },
        // Visits let declarations before the let body.
        [&](Let<A>& let)
        {
            for(auto& [name, value]: let.decls)
                f(value);
            f(let.body);
        },
        // Visits the lambda body.  Lambda patterns are traversed by the
        // pattern helpers because they are not expressions.
        [&](Lambda<A>& lambda)
        {
            f(lambda.body);
        },
        // Visits the sampled distribution expression.
        [&](Sample<A>& sample)
        {
            f(sample.dist);
        },
        [](auto&) {}
    });
}

// Applies a visitor to each const child expression stored under this node,
// including argument alphabets but skipping absent argument values.
template<class A, class F>
void for_each_child(const Expr<A>& expr, F&& f)
{
    expr.visit(overloaded{
        // Visits call argument values and alphabet metadata stored on const
        // argument edges.
        [&](const Call<A>& call)
        {
            for(auto& arg: call.args)
            {
                if (arg.value)
                    f(*arg.value);
                if (arg.alphabet)
                    f(*arg.alphabet);
            }
        },
        // Visits each const list element.
        [&](const List<A>& list)
        {
            for(auto& element: list.elements)
                f(element);
        },
        // Visits each const tuple element.
        [&](const Tuple<A>& tuple)
        {
            for(auto& element: tuple.elements)
                f(element);
        },
        // Visits const let declarations before the let body.
        [&](const Let<A>& let)
        {
            for(auto& [name, value]: let.decls)
                f(value);
            f(let.body);
        },
        // Visits the const lambda body.  Lambda patterns are traversed by the
        // pattern helpers because they are not expressions.
        [&](const Lambda<A>& lambda)
        {
            f(lambda.body);
        },
        // Visits the const sampled distribution expression.
        [&](const Sample<A>& sample)
        {
            f(sample.dist);
        },
        [](const auto&) {}
    });
}

}

namespace CM = CmdModel;

#endif
