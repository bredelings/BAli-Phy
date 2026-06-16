#ifndef MODELS_MODEL_EXPR_H
#define MODELS_MODEL_EXPR_H

#include <memory>
#include <optional>
#include <set>
#include <string>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "util/ptree.H"

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

using UntypedAnn = std::monostate;

template<class A>
struct ModelExpr;

template<class A>
struct ModelArg
{
    std::string name;
    Box<ModelExpr<A>> value;

    bool is_default_value = false;
    bool suppress_default = false;

    std::optional<Box<ModelExpr<A>>> alphabet;
};

template<class A>
using ModelDecls = std::vector<std::pair<std::string, ModelExpr<A>>>;

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

struct MissingArg
{};

template<class A>
struct Call
{
    std::string function;
    std::vector<ModelArg<A>> args;
};

template<class A>
struct List
{
    std::vector<ModelExpr<A>> elements;
};

template<class A>
struct Tuple
{
    std::vector<ModelExpr<A>> elements;
};

template<class A>
struct Let
{
    ModelDecls<A> decls;
    Box<ModelExpr<A>> body;
};

template<class A>
struct Lambda
{
    Box<ModelExpr<A>> pattern;
    Box<ModelExpr<A>> body;
};

template<class A>
struct Sample
{
    Box<ModelExpr<A>> dist;
};

struct ModelAnn
{
    ptree type;
    std::set<std::string> used_args;

    bool no_log = false;
    std::optional<std::string> extract;
};

template<class A>
struct ModelExpr
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
        MissingArg,
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

using UntypedModelExpr = ModelExpr<UntypedAnn>;
using TypedModelExpr = ModelExpr<ModelAnn>;
using TypedModelDecls = ModelDecls<ModelAnn>;

template<class A>
ModelExpr<A> clone(const ModelExpr<A>& expr)
{
    return expr;
}

template<class A>
ModelArg<A> clone(const ModelArg<A>& arg)
{
    return arg;
}

template<class A>
ModelDecls<A> clone(const ModelDecls<A>& decls)
{
    return decls;
}

#endif
