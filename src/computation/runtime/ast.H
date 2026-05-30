#ifndef RUNTIME_AST_H
#define RUNTIME_AST_H

#include <memory>
#include <variant>
#include <vector>
#include "computation/expression/constructor.H"
#include "computation/expression/expression_ref.H"
#include "computation/expression/var.H"
#include "computation/operation.H"

namespace Runtime
{
    struct Exp;

    using ExpPtr = std::shared_ptr<const Exp>;

    struct Atom
    {
        expression_ref value;
    };

    struct IndexVar
    {
        int index;
    };

    struct GlobalVar
    {
        var name;
    };

    struct RegRef
    {
        int target;
    };

    struct Lambda
    {
        ExpPtr body;
    };

    struct Let
    {
        std::vector<ExpPtr> binds;
        ExpPtr body;
    };

    struct Alt
    {
        expression_ref pattern;
        ExpPtr body;
    };

    struct Case
    {
        ExpPtr object;
        std::vector<Alt> alts;
    };

    struct FunctionApply
    { };

    struct ConstructorApp
    {
        constructor head;
    };

    struct OperationApp
    {
        Operation head;
    };

    struct RawAppHead
    {
        expression_ref head;
    };

    using AppHead = std::variant<FunctionApply, ConstructorApp, OperationApp, RawAppHead>;

    struct App
    {
        AppHead head;
        std::vector<ExpPtr> args;
    };

    struct Trim
    {
        std::vector<int> indices;
        ExpPtr body;
    };

    struct Exp
    {
        std::variant<Atom, IndexVar, GlobalVar, RegRef, Lambda, Let, Case, App, Trim> value;

        explicit Exp(const Atom& a):value(a) {}
        explicit Exp(const IndexVar& i):value(i) {}
        explicit Exp(const GlobalVar& g):value(g) {}
        explicit Exp(const RegRef& r):value(r) {}
        explicit Exp(const Lambda& l):value(l) {}
        explicit Exp(const Let& l):value(l) {}
        explicit Exp(const Case& c):value(c) {}
        explicit Exp(const App& a):value(a) {}
        explicit Exp(const Trim& t):value(t) {}
    };

    ExpPtr make(const Atom&);
    ExpPtr make(const IndexVar&);
    ExpPtr make(const GlobalVar&);
    ExpPtr make(const RegRef&);
    ExpPtr make(const Lambda&);
    ExpPtr make(const Let&);
    ExpPtr make(const Case&);
    ExpPtr make(const App&);
    ExpPtr make(const Trim&);

    ExpPtr from_indexed_expression_ref(const expression_ref&);
    AppHead app_head_from_expression_ref(const expression_ref&);

    expression_ref to_expression_ref(const ExpPtr&);
    expression_ref to_expression_ref(const AppHead&);
}

#endif
