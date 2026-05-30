#ifndef RUNTIME_AST_H
#define RUNTIME_AST_H

#include <cassert>
#include <cstdlib>
#include <memory>
#include <iosfwd>
#include <string>
#include <utility>
#include <variant>
#include <vector>
#include <cereal/types/memory.hpp>
#include <cereal/types/string.hpp>
#include <cereal/types/variant.hpp>
#include <cereal/types/vector.hpp>
#define CEREAL_FUTURE_EXPERIMENTAL 1
#include <cereal/archives/adapters.hpp>
#include "computation/expression/constructor.H"
#include "computation/expression/expression_ref.H"
#include "computation/expression/var.H"
#include "computation/haskell/integer_container.H"
#include "computation/loader.H"
#include "computation/operation.H"

namespace Runtime
{
    struct Exp;

    using ExpPtr = std::shared_ptr<const Exp>;

    struct IntLiteral
    {
        int value;

        template <class Archive>
        void serialize(Archive& ar) { ar(value); }
    };

    struct DoubleLiteral
    {
        double value;

        template <class Archive>
        void serialize(Archive& ar) { ar(value); }
    };

    struct LogDoubleLiteral
    {
        log_double_t value;

        template <class Archive>
        void save(Archive& ar) const
        {
            auto log_value = value.log();
            ar(log_value);
        }

        template <class Archive>
        void load(Archive& ar)
        {
            double log_value;
            ar(log_value);
            value = exp_to_log_space(log_value);
        }
    };

    struct CharLiteral
    {
        char value;

        template <class Archive>
        void serialize(Archive& ar) { ar(value); }
    };

    struct StringLiteral
    {
        std::string value;

        template <class Archive>
        void serialize(Archive& ar) { ar(value); }
    };

    struct IntegerLiteral
    {
        integer value;

        template <class Archive>
        void save(Archive& ar) const
        {
            auto text = value.str();
            ar(text);
        }

        template <class Archive>
        void load(Archive& ar)
        {
            std::string text;
            ar(text);
            value = integer(text);
        }
    };

    struct ConstructorValue
    {
        constructor value;

        ConstructorValue():value("", 0) {}
        ConstructorValue(constructor c):value(std::move(c)) {}

        template <class Archive>
        void save(Archive& ar) const
        {
            auto name = value.name();
            auto arity = value.n_args();
            ar(name, arity);
        }

        template <class Archive>
        void load(Archive& ar)
        {
            std::string name;
            int arity;
            ar(name, arity);
            value = constructor(name, arity);
        }
    };

    struct IndexVar
    {
        int index;

        template <class Archive>
        void serialize(Archive& ar) { ar(index); }
    };

    struct GlobalVar
    {
        var name;

        template <class Archive>
        void serialize(Archive& ar) { ar(name); }
    };

    struct RegRef
    {
        int target;

        template <class Archive>
        void serialize(Archive& ar) { ar(target); }
    };

    struct Lambda
    {
        ExpPtr body;

        template <class Archive>
        void serialize(Archive& ar) { ar(body); }
    };

    struct Let
    {
        std::vector<ExpPtr> binds;
        ExpPtr body;

        template <class Archive>
        void serialize(Archive& ar) { ar(binds, body); }
    };

    struct WildcardPattern
    {
        template <class Archive>
        void serialize(Archive&) {}
    };

    struct ConstructorPattern
    {
        constructor head;

        ConstructorPattern():head("", 0) {}
        ConstructorPattern(constructor c):head(std::move(c)) {}

        template <class Archive>
        void save(Archive& ar) const
        {
            auto name = head.name();
            auto arity = head.n_args();
            ar(name, arity);
        }

        template <class Archive>
        void load(Archive& ar)
        {
            std::string name;
            int arity;
            ar(name, arity);
            head = constructor(name, arity);
        }
    };

    // I might want to add literals here, especially a literal int pattern.
    using Pattern = std::variant<WildcardPattern, ConstructorPattern>;

    struct Alt
    {
        Pattern pattern;
        ExpPtr body;

        template <class Archive>
        void serialize(Archive& ar) { ar(pattern, body); }
    };

    struct Case
    {
        ExpPtr object;
        std::vector<Alt> alts;

        template <class Archive>
        void serialize(Archive& ar) { ar(object, alts); }
    };

    struct FunctionApply
    {
        template <class Archive>
        void serialize(Archive&) {}
    };

    Operation operation_from_builtin(void*, const std::string& lib_name, const std::string& func_name, const std::string& call_conv);

    struct ConstructorApp
    {
        constructor head;

        ConstructorApp():head("", 0) {}
        ConstructorApp(constructor c):head(std::move(c)) {}

        template <class Archive>
        void save(Archive& ar) const
        {
            auto name = head.name();
            auto arity = head.n_args();
            ar(name, arity);
        }

        template <class Archive>
        void load(Archive& ar)
        {
            std::string name;
            int arity;
            ar(name, arity);
            head = constructor(name, arity);
        }
    };

    struct OperationApp
    {
        Operation head;
        std::string lib_name;
        std::string func_name;
        std::string call_conv;

        OperationApp(Operation);
        OperationApp(Operation, std::string, std::string, std::string);
        OperationApp();

        template <class Archive>
        void save(Archive& ar) const
        {
            assert(not lib_name.empty());
            assert(not func_name.empty());
            assert(not call_conv.empty());
            ar(lib_name, func_name, call_conv);
        }

        template <class Archive>
        void load(Archive& ar)
        {
            ar(lib_name, func_name, call_conv);

            const module_loader& loader = get_user_data<const module_loader>(ar);
            void* op = loader.load_builtin_ptr(lib_name, func_name, call_conv);
            head = operation_from_builtin(op, lib_name, func_name, call_conv);
        }
    };

    using AppHead = std::variant<FunctionApply, ConstructorApp, OperationApp>;

    struct App
    {
        AppHead head;
        std::vector<ExpPtr> args;

        template <class Archive>
        void serialize(Archive& ar) { ar(head, args); }
    };

    struct Trim
    {
        std::vector<int> indices;
        ExpPtr body;

        template <class Archive>
        void serialize(Archive& ar) { ar(indices, body); }
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

        template <class Archive>
        void serialize(Archive& ar) { ar(value); }

        Exp():value(IntLiteral{0}) {}
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

    ExpPtr make_apply(ExpPtr, std::vector<ExpPtr>);
    ExpPtr make_apply_with_bound_args(int, std::vector<ExpPtr>);

    ExpPtr from_indexed_expression_ref(const expression_ref&);
    ExpPtr atom_from_expression_ref(const expression_ref&);

    Pattern pattern_from_expression_ref(const expression_ref&);
    int pattern_arity(const Pattern&);

    AppHead app_head_from_expression_ref(const expression_ref&);
    OperationApp builtin_operation_app(void*, const std::string& lib_name, const std::string& func_name, const std::string& call_conv);

    expression_ref to_expression_ref(const ExpPtr&);
    expression_ref to_expression_ref(const Pattern&);
    expression_ref to_expression_ref(const AppHead&);

    std::string print(const ExpPtr&);
    std::string print(const Pattern&);
    std::string print(const AppHead&);

    std::ostream& operator<<(std::ostream&, const Exp&);

    void check_invariants(const ExpPtr&);
    void check_no_reg_refs(const ExpPtr&);
    void check_translated(const ExpPtr&);

}

#endif
