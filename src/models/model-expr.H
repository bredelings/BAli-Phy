#ifndef MODELS_MODEL_EXPR_H
#define MODELS_MODEL_EXPR_H

#include <memory>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "util/ptree.H"

namespace CmdModel
{

using NoAnn = std::monostate;

template<class A>
struct Expr;

template<class A>
struct Arg;

template<class A>
using Decls = std::vector<std::pair<std::string, Expr<A>>>;

using UntypedExpr = Expr<NoAnn>;

template<class T>
class Box
{
    std::unique_ptr<T> value_;

public:
    Box() = delete;

    Box(const T& value)
        :value_(std::make_unique<T>(value))
    {}

    Box(T&& value)
        :value_(std::make_unique<T>(std::move(value)))
    {}

    Box(const Box& box)
        :value_(std::make_unique<T>(*box.value_))
    {}

    Box(Box&&) noexcept = default;

    Box& operator=(const Box& box)
    {
        if (this != &box)
            value_ = std::make_unique<T>(*box.value_);
        return *this;
    }

    Box& operator=(Box&&) noexcept = default;

          T& operator*()       {return *value_;}
    const T& operator*() const {return *value_;}

          T* operator->()       {return value_.get();}
    const T* operator->() const {return value_.get();}

          T& get()       {return *value_;}
    const T& get() const {return *value_;}
};

template<class A>
struct Arg
{
    std::string name;
    std::optional<Box<Expr<A>>> value;

    bool is_default_value = false;
    bool suppress_default = false;

    std::optional<Box<Expr<A>>> alphabet;
};

// Returns the expression stored on an argument edge, and fails clearly if the
// edge is only an absent-value placeholder.
template<class A>
Expr<A>& require_arg_value(Arg<A>& arg)
{
    if (not arg.value)
        throw std::logic_error("model argument has no expression value");
    return arg.value->get();
}

// Returns the expression stored on a const argument edge, and fails clearly if
// the edge is only an absent-value placeholder.
template<class A>
const Expr<A>& require_arg_value(const Arg<A>& arg)
{
    if (not arg.value)
        throw std::logic_error("model argument has no expression value");
    return arg.value->get();
}

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
    Box<Expr<A>> body;
};

template<class A>
struct Lambda
{
    Box<Expr<A>> pattern;
    Box<Expr<A>> body;
};

template<class A>
struct Sample
{
    Box<Expr<A>> dist;
};

struct Ann
{
    ptree type;
    std::set<std::string> used_args;

    bool no_log = false;
    std::optional<std::string> extract;
};

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
        Call<A>,
        List<A>,
        Tuple<A>,
        Let<A>,
        Lambda<A>,
        Sample<A>
    >;

    Node node;
};

template<class... Ts>
struct overloaded: Ts...
{
    using Ts::operator()...;
};

template<class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

template<class A>
Expr<A> clone(const Expr<A>& expr)
{
    return expr;
}

template<class A>
Arg<A> clone(const Arg<A>& arg)
{
    return arg;
}

template<class A>
Decls<A> clone(const Decls<A>& decls)
{
    return decls;
}

template<class A>
bool is_call_named(const Expr<A>& expr, std::string_view name)
{
    if (auto call = std::get_if<Call<A>>(&expr.node))
        return call->function == name;
    else
        return false;
}

template<class A>
bool is_list(const Expr<A>& expr)
{
    return std::holds_alternative<List<A>>(expr.node);
}

template<class A>
bool is_tuple(const Expr<A>& expr)
{
    return std::holds_alternative<Tuple<A>>(expr.node);
}

template<class A>
bool is_sample(const Expr<A>& expr)
{
    return std::holds_alternative<Sample<A>>(expr.node);
}

template<class A>
bool is_get_state(const Expr<A>& expr)
{
    return std::holds_alternative<GetState>(expr.node);
}

template<class A, class F>
void for_each_child(Expr<A>& expr, F&& f)
{
    std::visit(overloaded{
        [&](Call<A>& call)
        {
            for(auto& arg: call.args)
            {
                if (arg.value)
                    f(arg.value->get());
                if (arg.alphabet)
                    f(arg.alphabet->get());
            }
        },
        [&](List<A>& list)
        {
            for(auto& element: list.elements)
                f(element);
        },
        [&](Tuple<A>& tuple)
        {
            for(auto& element: tuple.elements)
                f(element);
        },
        [&](Let<A>& let)
        {
            for(auto& [name, value]: let.decls)
                f(value);
            f(let.body.get());
        },
        [&](Lambda<A>& lambda)
        {
            f(lambda.pattern.get());
            f(lambda.body.get());
        },
        [&](Sample<A>& sample)
        {
            f(sample.dist.get());
        },
        [](auto&) {}
    }, expr.node);
}

template<class A, class F>
void for_each_child(const Expr<A>& expr, F&& f)
{
    std::visit(overloaded{
        [&](const Call<A>& call)
        {
            for(auto& arg: call.args)
            {
                if (arg.value)
                    f(arg.value->get());
                if (arg.alphabet)
                    f(arg.alphabet->get());
            }
        },
        [&](const List<A>& list)
        {
            for(auto& element: list.elements)
                f(element);
        },
        [&](const Tuple<A>& tuple)
        {
            for(auto& element: tuple.elements)
                f(element);
        },
        [&](const Let<A>& let)
        {
            for(auto& [name, value]: let.decls)
                f(value);
            f(let.body.get());
        },
        [&](const Lambda<A>& lambda)
        {
            f(lambda.pattern.get());
            f(lambda.body.get());
        },
        [&](const Sample<A>& sample)
        {
            f(sample.dist.get());
        },
        [](const auto&) {}
    }, expr.node);
}

template<class A>
void check_invariants(const Expr<A>& expr)
{
    if constexpr(std::is_same_v<A, Ann>)
    {
        if (expr.ann.type.is_null())
            throw std::logic_error("typed model expression has null type annotation");
    }

    std::visit(overloaded{
        [](const Tuple<A>& tuple)
        {
            if (tuple.elements.size() < 2)
                throw std::logic_error("model tuple must have at least two elements");
        },
        [](const auto&) {}
    }, expr.node);

    for_each_child(expr, [](const Expr<A>& child) { check_invariants(child); });
}

template<class A>
void check_invariants(const Decls<A>& decls)
{
    for(auto& [name, expr]: decls)
        check_invariants(expr);
}

}

namespace CM = CmdModel;

#endif
