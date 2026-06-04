#ifndef RUNTIME_AST_H
#define RUNTIME_AST_H

#include <cassert>
#include <cstdlib>
#include <map>
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
#include "computation/haskell/Integer.H"
#include "computation/haskell/integer_container.H"
#include "computation/loader.H"
#include "util/OrderedDouble.H"

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
        Exp(int);
        Exp(double);
        Exp(log_double_t);
        Exp(bool);
        Exp(char);
        Exp(std::string);
        Exp(const char*);
        Exp(integer);
        Exp(constructor);
        Exp(const ::Integer&);
        Exp(const Object&);
        Exp(const Object*);
        Exp(object_ptr<const Object>);
        Exp(const ::String&);

        template <DerivedFromObject T>
        Exp(object_ptr<T> x):Exp(object_ptr<const Object>(std::move(x))) {}
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
        const T* to() const;

        template <typename T>
        bool is_a() const { return to<T>(); }

        template <typename T>
        bool is() const { return to<T>(); }

        bool is_int() const { return is<Int>(); }
        bool is_double() const { return is<Double>(); }
        bool is_log_double() const { return is<LogDouble>(); }
        bool is_char() const { return is<Char>(); }
        bool is_string() const { return is<String>(); }
        bool is_integer() const { return is<Integer>(); }
        bool is_constructor() const { return is<Constructor>(); }
        bool is_object_value() const { return is<ObjectValue>(); }

        template <typename T>
        const T& as() const
        {
            auto ptr = to<T>();
            assert(ptr);
            return *ptr;
        }

        bool is_atomic_value() const;
        bool is_value() const;
        bool is_whnf() const;
        bool is_gcable_object_value() const;
        int as_int() const;
        double as_double() const;
        log_double_t as_log_double() const;
        char as_char() const;
        const std::string& as_string() const;
        const integer& as_integer() const;
        const constructor& as_constructor() const;
        std::string print() const;

        template <DerivedFromObject T>
        const T& as_() const;

        template <DerivedFromObject T>
        const T& as_checked() const;

        template <DerivedFromObject T>
        object_ptr<const T> as_ptr_to() const;

        template <typename T1, typename T2>
        std::shared_ptr<const T2> poly_cast() const;

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

    bool operator==(const Exp&, const Exp&);

    using RVector = Box<std::vector<Exp>>;
    using RIntMap = Box<std::map<int, Exp>>;
    using RPair = Box<std::pair<Exp, Exp>>;

    struct Int
    {
        int value;

        Int() = default;
        Int(int x):value(x) {}

        bool operator==(const Int&) const = default;

        template <class Archive>
        void serialize(Archive& ar) { ar(value); }
    };

    struct Double
    {
        OrderedDouble value;

        Double() = default;
        Double(double x):value(x) {}

        bool operator==(const Double&) const = default;

        template <class Archive>
        void serialize(Archive& ar) { ar(value); }
    };

    struct LogDouble
    {
        log_double_t value;

        LogDouble() = default;
        LogDouble(log_double_t x):value(x) {}

        bool operator==(const LogDouble& x) const
        {
            return OrderedDouble(value.log()) == OrderedDouble(x.value.log());
        }

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

        bool operator==(const Char&) const = default;

        template <class Archive>
        void serialize(Archive& ar) { ar(value); }
    };

    struct String
    {
        std::string value;

        String() = default;
        String(std::string x):value(std::move(x)) {}

        bool operator==(const String&) const = default;

        template <class Archive>
        void serialize(Archive& ar) { ar(value); }
    };

    struct Integer
    {
        integer value;

        Integer() = default;
        Integer(integer x):value(std::move(x)) {}

        bool operator==(const Integer&) const = default;

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

        bool operator==(const Constructor& c) const
        {
            return value.name() == c.value.name() and value.n_args() == c.value.n_args();
        }

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
        object_ptr<const Object> value;

        ObjectValue() = default;
        ObjectValue(object_ptr<const Object> o):value(std::move(o))
        {
            assert(value->type() != type_constant::expression_type);
            assert(value->type() != type_constant::constructor_type);
            assert(not dynamic_pointer_cast<const Box<std::string>>(o));
            assert(not dynamic_pointer_cast<const Box<integer>>(o));
        }

        bool operator==(const ObjectValue&) const;

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

    template <DerivedFromObject T>
    const T& Exp::as_() const
    {
        auto ptr = to<ObjectValue>();
        if (not ptr)
            throw std::runtime_error("Runtime value is not an ObjectValue");
        return *convert<T>(ptr->value);
    }

    template <DerivedFromObject T>
    const T& Exp::as_checked() const
    {
        auto ptr = to<ObjectValue>();
        if (not ptr)
            throw std::runtime_error("Runtime value is not an ObjectValue");
        return *convert_and_check<const T>(ptr->value.get());
    }

    template <DerivedFromObject T>
    object_ptr<const T> Exp::as_ptr_to() const
    {
        auto ptr = to<ObjectValue>();
        if (not ptr)
            throw std::runtime_error("Runtime value is not an ObjectValue");
        return convert<const T>(ptr->value);
    }

    template <typename T1, typename T2>
    std::shared_ptr<const T2> Exp::poly_cast() const
    {
        auto object_value = to<ObjectValue>();
        if (not object_value)
            return nullptr;

        auto converted1 = dynamic_cast<const PtrBox<T1>*>(object_value->value.get());
        if (not converted1)
            return nullptr;

        return std::dynamic_pointer_cast<const T2>(*converted1);
    }

    template <typename T>
    const T* Exp::to() const
    {
        if constexpr (DerivedFromObject<T>)
        {
            if (auto object_value = to<ObjectValue>())
                return dynamic_cast<const T*>(object_value->value.get());
            else
                return nullptr;
        }
        else
        {
            if (std::holds_alternative<std::shared_ptr<const T>>(value))
                return std::get<std::shared_ptr<const T>>(value).get();
            else
                return nullptr;
        }
    }


    struct IndexVar
    {
        int index = 0;

        IndexVar() = default;
        IndexVar(int i):index(i) {}

        bool operator==(const IndexVar&) const = default;

        template <class Archive>
        void serialize(Archive& ar) { ar(index); }
    };

    struct GlobalVar
    {
        var name;

        GlobalVar() = default;
        GlobalVar(var x):name(std::move(x)) {}

        bool operator==(const GlobalVar&) const = default;

        template <class Archive>
        void serialize(Archive& ar) { ar(name); }
    };

    struct RegRef
    {
        int target = 0;

        RegRef() = default;
        RegRef(int r):target(r) {}

        bool operator==(const RegRef&) const = default;

        template <class Archive>
        void serialize(Archive& ar) { ar(target); }
    };

    struct Lambda
    {
        Exp body;

        Lambda() = default;
        Lambda(Exp b):body(std::move(b)) {}

        bool operator==(const Lambda&) const = default;

        template <class Archive>
        void serialize(Archive& ar) { ar(body); }
    };

    struct Let
    {
        std::vector<Exp> binds;
        Exp body;

        Let() = default;
        Let(std::vector<Exp> bs, Exp b):binds(std::move(bs)), body(std::move(b)) {}

        bool operator==(const Let&) const = default;

        template <class Archive>
        void serialize(Archive& ar) { ar(binds, body); }
    };

    struct WildcardPattern
    {
        bool operator==(const WildcardPattern&) const = default;

        template <class Archive>
        void serialize(Archive&) {}
    };

    struct ConstructorPattern
    {
        constructor head;

        ConstructorPattern():head("()", 0) {}
        ConstructorPattern(constructor c):head(std::move(c)) {}

        bool operator==(const ConstructorPattern& p) const
        {
            return head.name() == p.head.name() and head.n_args() == p.head.n_args();
        }

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

        bool operator==(const Alt&) const = default;

        template <class Archive>
        void serialize(Archive& ar) { ar(pattern, body); }
    };

    struct Case
    {
        Exp object;
        std::vector<Alt> alts;

        Case() = default;
        Case(Exp e, std::vector<Alt> as):object(std::move(e)), alts(std::move(as)) {}

        bool operator==(const Case&) const = default;

        template <class Archive>
        void serialize(Archive& ar) { ar(object, alts); }
    };

    struct FunctionApply
    {
        bool operator==(const FunctionApply&) const = default;

        template <class Archive>
        void serialize(Archive&) {}
    };

    std::shared_ptr<const Operation> operation_from_builtin(void*, const std::string& lib_name, const std::string& func_name, const std::string& call_conv);

    struct ConstructorApp
    {
        constructor head;

        ConstructorApp():head("()", 0) {}
        ConstructorApp(constructor c):head(std::move(c)) {}

        bool operator==(const ConstructorApp& app) const
        {
            return head.name() == app.head.name() and head.n_args() == app.head.n_args();
        }

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

        OperationApp(std::shared_ptr<const Operation>);
        OperationApp(std::shared_ptr<const Operation>, std::string, std::string, std::string);
        OperationApp();

        bool operator==(const OperationApp&) const;

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
            head = operation_from_builtin(op, lib_name, func_name, call_conv);
        }
    };

    using AppHead = std::variant<FunctionApply, ConstructorApp, OperationApp>;

    struct App
    {
        AppHead head;
        std::vector<Exp> args;

        App() = default;
        App(AppHead h, std::vector<Exp> as):head(std::move(h)), args(std::move(as)) {}

        bool operator==(const App&) const = default;

        template <class Archive>
        void serialize(Archive& ar) { ar(head, args); }
    };

    struct Trim
    {
        std::vector<int> indices;
        Exp body;

        Trim() = default;
        Trim(std::vector<int> is, Exp b):indices(std::move(is)), body(std::move(b)) {}

        bool operator==(const Trim&) const = default;

        template <class Archive>
        void serialize(Archive& ar) { ar(indices, body); }
    };

    Exp apply(Exp, std::vector<Exp>);
    Exp apply_env_function(int, std::vector<Exp>);
    Exp atomic_value(const expression_ref&);
    Exp e_op_value(const expression_ref&);
    const Exp& rpair_first(const Exp&);
    const Exp& rpair_first(const expression_ref&);
    const Exp& rpair_second(const Exp&);
    const Exp& rpair_second(const expression_ref&);
    int count_lambdas(const Exp&);
    Exp peel_lambdas(const Exp&, int);
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

namespace R = Runtime;

#endif
