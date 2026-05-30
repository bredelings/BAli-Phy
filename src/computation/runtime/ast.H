#ifndef RUNTIME_AST_H
#define RUNTIME_AST_H

#include <memory>
#include <string>
#include <variant>
#include <vector>
#include "computation/expression/constructor.H"
#include "computation/expression/expression_ref.H"
#include "computation/expression/var.H"
#include "computation/haskell/integer_container.H"
#include "computation/operation.H"

namespace Runtime
{
    struct Exp;

    using ExpPtr = std::shared_ptr<const Exp>;

    struct IntLiteral
    {
        int value;
    };

    struct DoubleLiteral
    {
        double value;
    };

    struct LogDoubleLiteral
    {
        log_double_t value;
    };

    struct CharLiteral
    {
        char value;
    };

    struct StringLiteral
    {
        std::string value;
    };

    struct IntegerLiteral
    {
        integer value;
    };

    struct ConstructorValue
    {
        constructor value;
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

    struct WildcardPattern
    { };

    struct ConstructorPattern
    {
        constructor head;
    };

    struct RawPattern
    {
        expression_ref value;
    };

    using Pattern = std::variant<WildcardPattern, ConstructorPattern, RawPattern>;

    struct Alt
    {
        Pattern pattern;
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

    using AppHead = std::variant<FunctionApply, ConstructorApp, OperationApp>;

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
        std::variant<IntLiteral, DoubleLiteral, LogDoubleLiteral, CharLiteral, StringLiteral, IntegerLiteral,
                     ConstructorValue, IndexVar, GlobalVar, RegRef, Lambda, Let, Case, App, Trim> value;

        explicit Exp(const IntLiteral& l):value(l) {}
        explicit Exp(const DoubleLiteral& l):value(l) {}
        explicit Exp(const LogDoubleLiteral& l):value(l) {}
        explicit Exp(const CharLiteral& l):value(l) {}
        explicit Exp(const StringLiteral& l):value(l) {}
        explicit Exp(const IntegerLiteral& l):value(l) {}
        explicit Exp(const ConstructorValue& c):value(c) {}
        explicit Exp(const IndexVar& i):value(i) {}
        explicit Exp(const GlobalVar& g):value(g) {}
        explicit Exp(const RegRef& r):value(r) {}
        explicit Exp(const Lambda& l):value(l) {}
        explicit Exp(const Let& l):value(l) {}
        explicit Exp(const Case& c):value(c) {}
        explicit Exp(const App& a):value(a) {}
        explicit Exp(const Trim& t):value(t) {}
    };

    ExpPtr make(const IntLiteral&);
    ExpPtr make(const DoubleLiteral&);
    ExpPtr make(const LogDoubleLiteral&);
    ExpPtr make(const CharLiteral&);
    ExpPtr make(const StringLiteral&);
    ExpPtr make(const IntegerLiteral&);
    ExpPtr make(const ConstructorValue&);
    ExpPtr make(const IndexVar&);
    ExpPtr make(const GlobalVar&);
    ExpPtr make(const RegRef&);
    ExpPtr make(const Lambda&);
    ExpPtr make(const Let&);
    ExpPtr make(const Case&);
    ExpPtr make(const App&);
    ExpPtr make(const Trim&);

    ExpPtr from_indexed_expression_ref(const expression_ref&);
    ExpPtr atom_from_expression_ref(const expression_ref&);

    Pattern pattern_from_expression_ref(const expression_ref&);
    int pattern_arity(const Pattern&);

    AppHead app_head_from_expression_ref(const expression_ref&);

    expression_ref to_expression_ref(const ExpPtr&);
    expression_ref to_expression_ref(const Pattern&);
    expression_ref to_expression_ref(const AppHead&);
}

#endif
