#ifndef RUNTIME_AST_H
#define RUNTIME_AST_H

#include <cassert>
#include <cstdlib>
#include <memory>
#include <iosfwd>
#include <stdexcept>
#include <string>
#include <type_traits>
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

struct Operation;

namespace Runtime
{
    struct Exp;

    struct Int;
    struct Double;
    struct LogDouble;
    struct Char;
    struct String;
    struct Integer;
    struct Constructor;
    struct ObjectValue;
    struct IndexVar;
    struct GlobalVar;
    struct RegRef;
    struct Lambda;
    struct Let;
    struct Case;
    struct App;
    struct Trim;

    struct Exp
    {
        using value_type = std::variant<
            std::shared_ptr<const Int>,
            std::shared_ptr<const Double>,
            std::shared_ptr<const LogDouble>,
            std::shared_ptr<const Char>,
            std::shared_ptr<const String>,
            std::shared_ptr<const Integer>,
            std::shared_ptr<const Constructor>,
            std::shared_ptr<const ObjectValue>,
            std::shared_ptr<const IndexVar>,
            std::shared_ptr<const GlobalVar>,
            std::shared_ptr<const RegRef>,
            std::shared_ptr<const Lambda>,
            std::shared_ptr<const Let>,
            std::shared_ptr<const Case>,
            std::shared_ptr<const App>,
            std::shared_ptr<const Trim>
        >;

        value_type value;

        Exp() = default;
        Exp(Int);
        Exp(Double);
        Exp(LogDouble);
        Exp(Char);
        Exp(String);
        Exp(Integer);
        Exp(Constructor);
        Exp(ObjectValue);
        Exp(IndexVar);
        Exp(GlobalVar);
        Exp(RegRef);
        Exp(Lambda);
        Exp(Let);
        Exp(Case);
        Exp(App);
        Exp(Trim);

        bool empty() const
        {
            return std::visit([](const auto& ptr) { return not ptr; }, value);
        }

        explicit operator bool() const { return not empty(); }

        template <typename T>
        const T* to() const
        {
            if (std::holds_alternative<std::shared_ptr<const T>>(value))
                return std::get<std::shared_ptr<const T>>(value).get();
            else
                return nullptr;
        }

        template <typename T>
        bool is_a() const { return to<T>(); }

        template <typename T>
        bool is() const { return to<T>(); }

        template <typename T>
        const T& as() const
        {
            auto ptr = to<T>();
            assert(ptr);
            return *ptr;
        }

        template <typename F>
        decltype(auto) visit(F&& f) const
        {
            return std::visit([&](const auto& ptr) -> decltype(auto)
            {
                assert(ptr);
                return std::forward<F>(f)(*ptr);
            }, value);
        }

        template <class Archive>
        void serialize(Archive& ar) { ar(value); }
    };

    struct Int
    {
        int value;

        Int() = default;
        Int(int x):value(x) {}

        template <class Archive>
        void serialize(Archive& ar) { ar(value); }
    };

    struct Double
    {
        double value;

        Double() = default;
        Double(double x):value(x) {}

        template <class Archive>
        void serialize(Archive& ar) { ar(value); }
    };

    struct LogDouble
    {
        log_double_t value;

        LogDouble() = default;
        LogDouble(log_double_t x):value(x) {}

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

    struct Char
    {
        char value;

        Char() = default;
        Char(char x):value(x) {}

        template <class Archive>
        void serialize(Archive& ar) { ar(value); }
    };

    struct String
    {
        std::string value;

        String() = default;
        String(std::string x):value(std::move(x)) {}

        template <class Archive>
        void serialize(Archive& ar) { ar(value); }
    };

    struct Integer
    {
        integer value;

        Integer() = default;
        Integer(integer x):value(std::move(x)) {}

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

    struct Constructor
    {
        constructor value;

        Constructor():value("()", 0) {}
        Constructor(constructor c):value(std::move(c)) {}

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

    struct ObjectValue
    {
        expression_ref value;

        ObjectValue() = default;
        ObjectValue(expression_ref v):value(std::move(v)) {}

        template <class Archive>
        void save(Archive&) const
        {
            throw std::runtime_error("Runtime::ObjectValue is not serializable");
        }

        template <class Archive>
        void load(Archive&)
        {
            throw std::runtime_error("Runtime::ObjectValue is not serializable");
        }
    };

    struct IndexVar
    {
        int index = 0;

        IndexVar() = default;
        IndexVar(int i):index(i) {}

        template <class Archive>
        void serialize(Archive& ar) { ar(index); }
    };

    struct GlobalVar
    {
        var name;

        GlobalVar() = default;
        GlobalVar(var x):name(std::move(x)) {}

        template <class Archive>
        void serialize(Archive& ar) { ar(name); }
    };

    struct RegRef
    {
        int target = 0;

        RegRef() = default;
        RegRef(int r):target(r) {}

        template <class Archive>
        void serialize(Archive& ar) { ar(target); }
    };

    struct Lambda
    {
        Exp body;

        Lambda() = default;
        Lambda(Exp b):body(std::move(b)) {}

        template <class Archive>
        void serialize(Archive& ar) { ar(body); }
    };

    struct Let
    {
        std::vector<Exp> binds;
        Exp body;

        Let() = default;
        Let(std::vector<Exp> bs, Exp b):binds(std::move(bs)), body(std::move(b)) {}

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

        ConstructorPattern():head("()", 0) {}
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
        Exp body;

        Alt() = default;
        Alt(Pattern p, Exp b):pattern(std::move(p)), body(std::move(b)) {}

        template <class Archive>
        void serialize(Archive& ar) { ar(pattern, body); }
    };

    struct Case
    {
        Exp object;
        std::vector<Alt> alts;

        Case() = default;
        Case(Exp e, std::vector<Alt> as):object(std::move(e)), alts(std::move(as)) {}

        template <class Archive>
        void serialize(Archive& ar) { ar(object, alts); }
    };

    struct FunctionApply
    {
        template <class Archive>
        void serialize(Archive&) {}
    };

    Operation operation_from_builtin(void*, const std::string& lib_name, const std::string& func_name, const std::string& call_conv);
    std::shared_ptr<const Operation> operation_ptr_from_builtin(void*, const std::string& lib_name, const std::string& func_name, const std::string& call_conv);

    struct ConstructorApp
    {
        constructor head;

        ConstructorApp():head("()", 0) {}
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
        std::shared_ptr<const Operation> head;
        std::string lib_name;
        std::string func_name;
        std::string call_conv;

        OperationApp(Operation);
        OperationApp(Operation, std::string, std::string, std::string);
        OperationApp();

        template <class Archive>
        void save(Archive& ar) const
        {
            if (lib_name.empty() or func_name.empty() or call_conv.empty())
                throw std::runtime_error("Runtime-only OperationApp is not serializable");
            ar(lib_name, func_name, call_conv);
        }

        template <class Archive>
        void load(Archive& ar)
        {
            ar(lib_name, func_name, call_conv);

            const module_loader& loader = get_user_data<const module_loader>(ar);
            void* op = loader.load_builtin_ptr(lib_name, func_name, call_conv);
            head = operation_ptr_from_builtin(op, lib_name, func_name, call_conv);
        }
    };

    using AppHead = std::variant<FunctionApply, ConstructorApp, OperationApp>;

    struct App
    {
        AppHead head;
        std::vector<Exp> args;

        App() = default;
        App(AppHead h, std::vector<Exp> as):head(std::move(h)), args(std::move(as)) {}

        template <class Archive>
        void serialize(Archive& ar) { ar(head, args); }
    };

    struct Trim
    {
        std::vector<int> indices;
        Exp body;

        Trim() = default;
        Trim(std::vector<int> is, Exp b):indices(std::move(is)), body(std::move(b)) {}

        template <class Archive>
        void serialize(Archive& ar) { ar(indices, body); }
    };

    Exp apply(Exp, std::vector<Exp>);
    Exp apply_env_function(int, std::vector<Exp>);
    Exp shift_free_indices(const Exp&, int amount, int depth = 0);

    int pattern_arity(const Pattern&);

    OperationApp builtin_operation_app(void*, const std::string& lib_name, const std::string& func_name, const std::string& call_conv);

    expression_ref to_expression_ref(const Exp&);
    expression_ref to_expression_ref(const Pattern&);
    expression_ref to_expression_ref(const AppHead&);

    std::string print(const Exp&);
    std::string print(const Pattern&);
    std::string print(const AppHead&);

    std::ostream& operator<<(std::ostream&, const Exp&);

    void check_invariants(const Exp&);
    void check_no_reg_refs(const Exp&);
    void check_translated(const Exp&);

}

#endif
