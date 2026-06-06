#ifndef RUNTIME_AST_H
#define RUNTIME_AST_H

#include <cassert>
#include <cstdlib>
#include <map>
#include <memory>
#include <iosfwd>
#include <optional>
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
#include "computation/haskell/Integer.H"
#include "computation/haskell/integer_container.H"
#include "computation/loader.H"
#include "computation/object.H"
#include "util/OrderedDouble.H"
#include "util/cereal/intrusive_ptr.H"
#include "util/intrusive-counted.H"

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
    struct ConstructorTag;
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

    template <typename T>
    using ExpPtr = boost::intrusive_ptr<const T>;

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
        void serialize(Archive&) const
        {
            throw std::runtime_error("Runtime::ObjectValue is not serializable");
        }
    };

    struct IndexVar
    {
        int index = 0;

        IndexVar() = default;
        IndexVar(int i):index(i) {}

        bool operator==(const IndexVar&) const = default;

        template <class Archive>
        void serialize(Archive& ar) { ar(index); }
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

    template <typename T>
    struct is_boxed_exp_node: std::false_type {};

    template <> struct is_boxed_exp_node<String>: std::true_type {};
    template <> struct is_boxed_exp_node<Integer>: std::true_type {};
    template <> struct is_boxed_exp_node<Constructor>: std::true_type {};
    template <> struct is_boxed_exp_node<GlobalVar>: std::true_type {};
    template <> struct is_boxed_exp_node<Lambda>: std::true_type {};
    template <> struct is_boxed_exp_node<Let>: std::true_type {};
    template <> struct is_boxed_exp_node<Case>: std::true_type {};
    template <> struct is_boxed_exp_node<App>: std::true_type {};
    template <> struct is_boxed_exp_node<Trim>: std::true_type {};

    template <typename T>
    struct is_boxed_exp_storage: std::false_type {};

    template <typename T>
    struct is_boxed_exp_storage<ExpPtr<T>>: std::true_type {};
    
    struct Exp
    {
        using value_type = std::variant<
            std::monostate,
            Int,
            Double,
            LogDouble,
            Char,
            ObjectValue,
            IndexVar,
            RegRef,
            ExpPtr<String>,
            ExpPtr<Integer>,
            ExpPtr<Constructor>,
            ExpPtr<GlobalVar>,
            ExpPtr<Lambda>,
            ExpPtr<Let>,
            ExpPtr<Case>,
            ExpPtr<App>,
            ExpPtr<Trim>
        >;

        value_type value;

        Exp() = default;
        Exp(int x):Exp(Int(x)) {}
        Exp(double x):Exp(Double(x)) {}
        Exp(log_double_t x):Exp(LogDouble(x)) {}
        Exp(bool);
        Exp(char x):Exp(Char(x)) {}
        Exp(std::string);
        Exp(const char*);
        Exp(integer);
        Exp(const Box<integer>&) = delete;
        Exp(const Box<std::string>&) = delete;
        Exp(const Object& x):Exp(const_ptr(x)) {}
        Exp(const Object* x):Exp(object_ptr<const Object>(x)) {}
        Exp(object_ptr<const Object> x):Exp(ObjectValue(std::move(x))) {}

        template <DerivedFromObject T>
        Exp(object_ptr<T> x):Exp(object_ptr<const Object>(std::move(x))) {}
        Exp(Int node):value(std::move(node)) {}
        Exp(Double node):value(std::move(node)) {}
        Exp(LogDouble node):value(std::move(node)) {}
        Exp(Char node):value(std::move(node)) {}
        Exp(String);
        Exp(Integer);
        Exp(Constructor);
        Exp(ObjectValue node):value(std::move(node)) {}
        Exp(IndexVar node):value(std::move(node)) {}
        Exp(RegRef node):value(std::move(node)) {}
        Exp(GlobalVar);
        Exp(Lambda);
        Exp(Let);
        Exp(Case);
        Exp(App);
        Exp(Trim);

        bool empty() const
        {
            return std::holds_alternative<std::monostate>(value);
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
        const ConstructorTag& as_constructor() const;
        std::string print() const;

        const Exp& slot_ref(int i) const;

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
            return std::visit([&](const auto& node) -> decltype(auto)
            {
                using T = std::decay_t<decltype(node)>;

                if constexpr (std::is_same_v<T, std::monostate>)
                {
                    std::abort();
                    return std::forward<F>(f)(*static_cast<const Int*>(nullptr));
                }
                else if constexpr (is_boxed_exp_storage<T>::value)
                {
                    assert(node);
                    return std::forward<F>(f)(*node);
                }
                else
                    return std::forward<F>(f)(node);
            }, value);
        }

        template <class Archive>
        void serialize(Archive& ar) { ar(value); }
    };

    bool operator==(const Exp&, const Exp&);

    using RIntMap = Box<std::map<int, Exp>>;
    using RPair = Box<std::pair<Exp, Exp>>;

    struct RMaybe: public Box<std::optional<Exp>>
    {
        RMaybe() = default;

        using Box<std::optional<Exp>>::Box;

        RMaybe(Exp x):Box<std::optional<Exp>>(std::optional<Exp>(std::move(x))) {}

        RMaybe* clone() const override { return new RMaybe(*this); }
    };

    struct RVector: public Box<std::vector<Exp>>
    {
        RVector() = default;

        using Box<std::vector<Exp>>::Box;

        RVector* clone() const override { return new RVector(*this); }

        explicit RVector(const std::vector<double>&);
        explicit RVector(const std::vector<int>&);
        explicit RVector(const std::vector<char>&);

        explicit operator std::vector<double>() const;
        explicit operator std::vector<int>() const;
        explicit operator std::vector<char>() const;
    };

    struct String: intrusive_counted<String>
    {
        std::string value;

        String() = default;
        String(std::string x):value(std::move(x)) {}

        bool operator==(const String&) const = default;

        template <class Archive>
        void serialize(Archive& ar) { ar(value); }
    };

    struct Integer: intrusive_counted<Integer>
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

    struct ConstructorTag
    {
        std::string f_name = "()";
        int arity = 0;

        ConstructorTag() = default;
        ConstructorTag(std::string n, int a):f_name(std::move(n)), arity(a) {}

        const std::string& name() const { return f_name; }
        int n_args() const { return arity; }
        std::string print() const { return f_name; }

        bool operator==(const ConstructorTag&) const = default;

        template <class Archive>
        void serialize(Archive& ar) { ar(f_name, arity); }
    };

    struct Constructor: intrusive_counted<Constructor>
    {
        ConstructorTag value;

        Constructor() = default;
        Constructor(std::string name, int arity):value(std::move(name), arity) {}
        Constructor(ConstructorTag c):value(std::move(c)) {}

        bool operator==(const Constructor& c) const
        {
            return value == c.value;
        }

        template <class Archive>
        void serialize(Archive& ar) { ar(value); }
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
            if constexpr (is_boxed_exp_node<T>::value)
            {
                if (auto ptr = std::get_if<ExpPtr<T>>(&value))
                    return ptr->get();
                else
                    return nullptr;
            }
            else
                return std::get_if<T>(&value);
        }
    }

    struct GlobalVar: intrusive_counted<GlobalVar>
    {
        std::string name;
        int index = 0;

        GlobalVar() = default;
        GlobalVar(std::string x, int i=0):name(std::move(x)),index(i) {}

        bool operator==(const GlobalVar&) const = default;

        template <class Archive>
        void serialize(Archive& ar) { ar(name, index); }
    };

    struct Lambda: intrusive_counted<Lambda>
    {
        Exp body;

        Lambda() = default;
        Lambda(Exp b):body(std::move(b)) {}

        bool operator==(const Lambda&) const = default;

        template <class Archive>
        void serialize(Archive& ar) { ar(body); }
    };

    struct Let: intrusive_counted<Let>
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
        ConstructorTag head;

        ConstructorPattern() = default;
        ConstructorPattern(std::string name, int arity):head(std::move(name), arity) {}
        ConstructorPattern(ConstructorTag c):head(std::move(c)) {}

        bool operator==(const ConstructorPattern& p) const
        {
            return head == p.head;
        }

        template <class Archive>
        void serialize(Archive& ar) { ar(head); }
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

    struct Case: intrusive_counted<Case>
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
        ConstructorTag head;

        ConstructorApp() = default;
        ConstructorApp(std::string name, int arity):head(std::move(name), arity) {}
        ConstructorApp(ConstructorTag c):head(std::move(c)) {}

        bool operator==(const ConstructorApp& app) const
        {
            return head == app.head;
        }

        template <class Archive>
        void serialize(Archive& ar) { ar(head); }
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

    struct App: intrusive_counted<App>
    {
        AppHead head;
        std::vector<Exp> args;

        App() = default;
        App(AppHead h, std::vector<Exp> as):head(std::move(h)), args(std::move(as)) {}

        bool operator==(const App&) const = default;

        template <class Archive>
        void serialize(Archive& ar) { ar(head, args); }
    };

    struct Trim: intrusive_counted<Trim>
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
    const Exp& rpair_first(const Exp&);
    const Exp& rpair_second(const Exp&);
    int count_lambdas(const Exp&);
    Exp peel_lambdas(const Exp&, int);
    Exp shift_free_indices(const Exp&, int amount, int depth = 0);

    int pattern_arity(const Pattern&);
    bool has_constructor(const Exp&, const std::string&);

    OperationApp builtin_operation_app(void*, const std::string& lib_name, const std::string& func_name, const std::string& call_conv);

    std::string print(const Exp&);
    std::string print(const Pattern&);
    std::string print(const AppHead&);

    std::ostream& operator<<(std::ostream&, const Exp&);

    void check_invariants(const Exp&);
    void check_no_reg_refs(const Exp&);
    void check_translated(const Exp&);

    inline const Exp& Exp::slot_ref(int i) const
    {
        auto app = to<Runtime::App>();
        assert(app);
        return app->args[i];
    }
}

namespace R = Runtime;

#endif
