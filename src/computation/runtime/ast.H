#ifndef RUNTIME_AST_H
#define RUNTIME_AST_H

#include <memory>
#include <variant>
#include <vector>
#include "computation/expression/expression_ref.H"

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

    struct App
    {
        expression_ref head;
        std::vector<ExpPtr> args;
    };

    struct Trim
    {
        std::vector<int> indices;
        ExpPtr body;
    };

    struct Exp
    {
        std::variant<Atom, IndexVar, Lambda, Let, Case, App, Trim> value;

        explicit Exp(const Atom& a):value(a) {}
        explicit Exp(const IndexVar& i):value(i) {}
        explicit Exp(const Lambda& l):value(l) {}
        explicit Exp(const Let& l):value(l) {}
        explicit Exp(const Case& c):value(c) {}
        explicit Exp(const App& a):value(a) {}
        explicit Exp(const Trim& t):value(t) {}
    };

    ExpPtr make(const Atom&);
    ExpPtr make(const IndexVar&);
    ExpPtr make(const Lambda&);
    ExpPtr make(const Let&);
    ExpPtr make(const Case&);
    ExpPtr make(const App&);
    ExpPtr make(const Trim&);

    ExpPtr from_indexed_expression_ref(const expression_ref&);
    expression_ref to_expression_ref(const ExpPtr&);
}

#endif
