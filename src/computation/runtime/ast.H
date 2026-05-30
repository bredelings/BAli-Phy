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

    // I might want to add literals here, especially a literal int pattern.
    using Pattern = std::variant<WildcardPattern, ConstructorPattern>;

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
        std::string lib_name;
        std::string func_name;
        std::string call_conv;

        OperationApp(Operation);
        OperationApp(Operation, std::string, std::string, std::string);
        OperationApp();
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

        template <class Archive>
        void save(Archive& ar) const;

        template <class Archive>
        void load(Archive& ar);

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

    template <class Archive>
    void save_constructor(Archive& ar, const constructor& c)
    {
        auto name = c.name();
        auto arity = c.n_args();
        ar(name, arity);
    }

    template <class Archive>
    constructor load_constructor(Archive& ar)
    {
        std::string name;
        int arity;
        ar(name, arity);
        return constructor(name, arity);
    }

    template <class Archive>
    void save_pattern(Archive& ar, const Pattern& pattern)
    {
        std::visit([&](const auto& p)
        {
            using T = std::decay_t<decltype(p)>;

            if constexpr (std::is_same_v<T, WildcardPattern>)
            {
                int tag = 0;
                ar(tag);
            }
            else if constexpr (std::is_same_v<T, ConstructorPattern>)
            {
                int tag = 1;
                ar(tag);
                save_constructor(ar, p.head);
            }
        }, pattern);
    }

    template <class Archive>
    Pattern load_pattern(Archive& ar)
    {
        int tag;
        ar(tag);

        if (tag == 0)
            return WildcardPattern{};
        else if (tag == 1)
            return ConstructorPattern{load_constructor(ar)};
        else
            std::abort();
    }

    template <class Archive>
    void save_operation_app(Archive& ar, const OperationApp& op)
    {
        assert(not op.lib_name.empty());
        assert(not op.func_name.empty());
        assert(not op.call_conv.empty());
        ar(op.lib_name, op.func_name, op.call_conv);
    }

    template <class Archive>
    OperationApp load_operation_app(Archive& ar)
    {
        std::string lib_name;
        std::string func_name;
        std::string call_conv;
        ar(lib_name, func_name, call_conv);

        const module_loader& loader = get_user_data<const module_loader>(ar);
        void* op = loader.load_builtin_ptr(lib_name, func_name, call_conv);
        return builtin_operation_app(op, lib_name, func_name, call_conv);
    }

    template <class Archive>
    void save_app_head(Archive& ar, const AppHead& head)
    {
        std::visit([&](const auto& h)
        {
            using T = std::decay_t<decltype(h)>;

            if constexpr (std::is_same_v<T, FunctionApply>)
            {
                int tag = 0;
                ar(tag);
            }
            else if constexpr (std::is_same_v<T, ConstructorApp>)
            {
                int tag = 1;
                ar(tag);
                save_constructor(ar, h.head);
            }
            else if constexpr (std::is_same_v<T, OperationApp>)
            {
                int tag = 2;
                ar(tag);
                save_operation_app(ar, h);
            }
        }, head);
    }

    template <class Archive>
    AppHead load_app_head(Archive& ar)
    {
        int tag;
        ar(tag);

        if (tag == 0)
            return FunctionApply{};
        else if (tag == 1)
            return ConstructorApp{load_constructor(ar)};
        else if (tag == 2)
            return load_operation_app(ar);
        else
            std::abort();
    }

    template <class Archive>
    void Exp::save(Archive& ar) const
    {
        std::visit([&](const auto& e)
        {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, IntLiteral>)
            {
                int tag = 0;
                ar(tag, e.value);
            }
            else if constexpr (std::is_same_v<T, DoubleLiteral>)
            {
                int tag = 1;
                ar(tag, e.value);
            }
            else if constexpr (std::is_same_v<T, LogDoubleLiteral>)
            {
                int tag = 2;
                auto log_value = e.value.log();
                ar(tag, log_value);
            }
            else if constexpr (std::is_same_v<T, CharLiteral>)
            {
                int tag = 3;
                ar(tag, e.value);
            }
            else if constexpr (std::is_same_v<T, StringLiteral>)
            {
                int tag = 4;
                ar(tag, e.value);
            }
            else if constexpr (std::is_same_v<T, IntegerLiteral>)
            {
                int tag = 5;
                auto value = e.value.str();
                ar(tag, value);
            }
            else if constexpr (std::is_same_v<T, ConstructorValue>)
            {
                int tag = 6;
                ar(tag);
                save_constructor(ar, e.value);
            }
            else if constexpr (std::is_same_v<T, IndexVar>)
            {
                int tag = 7;
                ar(tag, e.index);
            }
            else if constexpr (std::is_same_v<T, GlobalVar>)
            {
                int tag = 8;
                ar(tag, e.name);
            }
            else if constexpr (std::is_same_v<T, RegRef>)
            {
                int tag = 9;
                ar(tag, e.target);
            }
            else if constexpr (std::is_same_v<T, Lambda>)
            {
                int tag = 10;
                ar(tag, e.body);
            }
            else if constexpr (std::is_same_v<T, Let>)
            {
                int tag = 11;
                ar(tag, e.binds, e.body);
            }
            else if constexpr (std::is_same_v<T, Case>)
            {
                int tag = 12;
                ar(tag, e.object);
                int n_alts = e.alts.size();
                ar(n_alts);
                for(const auto& alt: e.alts)
                {
                    save_pattern(ar, alt.pattern);
                    ar(alt.body);
                }
            }
            else if constexpr (std::is_same_v<T, App>)
            {
                int tag = 13;
                ar(tag);
                save_app_head(ar, e.head);
                ar(e.args);
            }
            else if constexpr (std::is_same_v<T, Trim>)
            {
                int tag = 14;
                ar(tag, e.indices, e.body);
            }
        }, value);
    }

    template <class Archive>
    void Exp::load(Archive& ar)
    {
        int tag;
        ar(tag);

        if (tag == 0)
        {
            int x;
            ar(x);
            value = IntLiteral{x};
        }
        else if (tag == 1)
        {
            double x;
            ar(x);
            value = DoubleLiteral{x};
        }
        else if (tag == 2)
        {
            double x;
            ar(x);
            value = LogDoubleLiteral{exp_to_log_space(x)};
        }
        else if (tag == 3)
        {
            char x;
            ar(x);
            value = CharLiteral{x};
        }
        else if (tag == 4)
        {
            std::string x;
            ar(x);
            value = StringLiteral{x};
        }
        else if (tag == 5)
        {
            std::string x;
            ar(x);
            value = IntegerLiteral{integer(x)};
        }
        else if (tag == 6)
        {
            value = ConstructorValue{load_constructor(ar)};
        }
        else if (tag == 7)
        {
            int x;
            ar(x);
            value = IndexVar{x};
        }
        else if (tag == 8)
        {
            var x;
            ar(x);
            value = GlobalVar{x};
        }
        else if (tag == 9)
        {
            int x;
            ar(x);
            value = RegRef{x};
        }
        else if (tag == 10)
        {
            ExpPtr body;
            ar(body);
            value = Lambda{body};
        }
        else if (tag == 11)
        {
            std::vector<ExpPtr> binds;
            ExpPtr body;
            ar(binds, body);
            value = Let{binds, body};
        }
        else if (tag == 12)
        {
            ExpPtr object;
            int n_alts;
            ar(object, n_alts);

            std::vector<Alt> alts;
            for(int i=0; i<n_alts; i++)
            {
                auto pattern = load_pattern(ar);
                ExpPtr body;
                ar(body);
                alts.push_back({pattern, body});
            }
            value = Case{object, alts};
        }
        else if (tag == 13)
        {
            auto head = load_app_head(ar);
            std::vector<ExpPtr> args;
            ar(args);
            value = App{head, args};
        }
        else if (tag == 14)
        {
            std::vector<int> indices;
            ExpPtr body;
            ar(indices, body);
            value = Trim{indices, body};
        }
        else
            std::abort();
    }
}

#endif
