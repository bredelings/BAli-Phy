#include "ast.H"
#include <cassert>
#include <cstdlib>
#include <ostream>
#include "computation/expression/apply.H"
#include "computation/expression/bool.H"
#include "computation/expression/case.H"
#include "computation/expression/constructor.H"
#include "computation/expression/index_var.H"
#include "computation/expression/lambda.H"
#include "computation/expression/let.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/runtime_views.H"
#include "computation/expression/trim.H"
#include "computation/expression/var.H"
#include "computation/haskell/Integer.H"
#include "computation/haskell/ids.H"
#include "computation/operation.H"
#include "util/myexception.H"
#include "util/string/join.H"

using std::vector;

namespace Runtime
{

    namespace
    {
        template <typename T>
        Exp::value_type make_exp_value(T node)
        {
            return std::shared_ptr<const T>(new T(std::move(node)));
        }

        Exp object_exp(object_ptr<const Object> x)
        {
            if (auto s = boost::dynamic_pointer_cast<const ::String>(x))
                return String(s->value());
            else if (auto i = boost::dynamic_pointer_cast<const ::Integer>(x))
                return Integer(i->value());
            else if (auto c = boost::dynamic_pointer_cast<const constructor>(x))
                return Constructor(*c);
            else
                return ObjectValue(std::move(x));
        }
    }

    Exp::Exp(int x):Exp(Int(x)) {}
    Exp::Exp(double x):Exp(Double(x)) {}
    Exp::Exp(log_double_t x):Exp(LogDouble(x)) {}
    Exp::Exp(bool x):Exp(Constructor(constructor(x ? bool_true_name : bool_false_name, 0))) {}
    Exp::Exp(char x):Exp(Char(x)) {}
    Exp::Exp(std::string x):Exp(String(std::move(x))) {}
    Exp::Exp(const char* x):Exp(String(x)) {}
    Exp::Exp(integer x):Exp(Integer(std::move(x))) {}
    Exp::Exp(constructor x):Exp(Constructor(std::move(x))) {}
    Exp::Exp(const ::Integer& x):Exp(Integer(x.value())) {}
    Exp::Exp(const Object& x):Exp(object_exp(const_ptr(x))) {}
    Exp::Exp(const Object* x):Exp(object_exp(object_ptr<const Object>(x))) {}
    Exp::Exp(object_ptr<const Object> x):Exp(object_exp(std::move(x))) {}
    Exp::Exp(const ::String& x):Exp(String(x.value())) {}
    Exp::Exp(Int node):value(make_exp_value(std::move(node))) {}
    Exp::Exp(Double node):value(make_exp_value(std::move(node))) {}
    Exp::Exp(LogDouble node):value(make_exp_value(std::move(node))) {}
    Exp::Exp(Char node):value(make_exp_value(std::move(node))) {}
    Exp::Exp(String node):value(make_exp_value(std::move(node))) {}
    Exp::Exp(Integer node):value(make_exp_value(std::move(node))) {}
    Exp::Exp(Constructor node):value(make_exp_value(std::move(node))) {}
    Exp::Exp(ObjectValue node):value(make_exp_value(std::move(node))) {}
    Exp::Exp(IndexVar node):value(make_exp_value(std::move(node))) {}
    Exp::Exp(GlobalVar node):value(make_exp_value(std::move(node))) {}
    Exp::Exp(RegRef node):value(make_exp_value(std::move(node))) {}
    Exp::Exp(Lambda node):value(make_exp_value(std::move(node))) {}
    Exp::Exp(Let node):value(make_exp_value(std::move(node))) {}
    Exp::Exp(Case node):value(make_exp_value(std::move(node))) {}
    Exp::Exp(App node):value(make_exp_value(std::move(node))) {}
    Exp::Exp(Trim node):value(make_exp_value(std::move(node))) {}

    bool ObjectValue::operator==(const ObjectValue& object_value) const
    {
        if (value == object_value.value)
            return true;

        if (not value or not object_value.value)
            return false;

        return *value == *object_value.value;
    }

    bool OperationApp::operator==(const OperationApp& app) const
    {
        if (lib_name != app.lib_name or func_name != app.func_name or call_conv != app.call_conv)
            return false;

        if (head == app.head)
            return true;

        if (not head or not app.head)
            return false;

        return *head == static_cast<const Object&>(*app.head);
    }

    RVector::RVector(const vector<double>& values)
    {
        reserve(values.size());
        for(auto value: values)
            push_back(value);
    }

    RVector::RVector(const vector<int>& values)
    {
        reserve(values.size());
        for(auto value: values)
            push_back(value);
    }

    RVector::RVector(const vector<char>& values)
    {
        reserve(values.size());
        for(auto value: values)
            push_back(value);
    }

    RVector::operator vector<double>() const
    {
        vector<double> values;
        values.reserve(size());
        for(const auto& value: *this)
            values.push_back(value.as_double());
        return values;
    }

    RVector::operator vector<int>() const
    {
        vector<int> values;
        values.reserve(size());
        for(const auto& value: *this)
            values.push_back(value.as_int());
        return values;
    }

    RVector::operator vector<char>() const
    {
        vector<char> values;
        values.reserve(size());
        for(const auto& value: *this)
            values.push_back(value.as_char());
        return values;
    }

    bool operator==(const Exp& x, const Exp& y)
    {
        if (x.empty() or y.empty())
            return x.empty() == y.empty();

        if (x.value.index() != y.value.index())
            return false;

        return std::visit([](const auto& a, const auto& b) -> bool
        {
            using A = typename std::decay_t<decltype(a)>::element_type;
            using B = typename std::decay_t<decltype(b)>::element_type;

            if constexpr (std::is_same_v<A, B>)
                return *a == *b;
            else
                return false;
        }, x.value, y.value);
    }

    bool Exp::is_atomic_value() const
    {
        return visit([](const auto& e) -> bool
        {
            using T = std::decay_t<decltype(e)>;
            return std::is_same_v<T, Int> or
                   std::is_same_v<T, Double> or
                   std::is_same_v<T, LogDouble> or
                   std::is_same_v<T, Char> or
                   std::is_same_v<T, String> or
                   std::is_same_v<T, Integer> or
                   std::is_same_v<T, Constructor> or
                   std::is_same_v<T, ObjectValue>;
        });
    }

    bool Exp::is_value() const
    {
        return visit([](const auto& e) -> bool
        {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, Int> or
                          std::is_same_v<T, Double> or
                          std::is_same_v<T, LogDouble> or
                          std::is_same_v<T, Char> or
                          std::is_same_v<T, String> or
                          std::is_same_v<T, Integer> or
                          std::is_same_v<T, Constructor> or
                          std::is_same_v<T, ObjectValue>)
            {
                return true;
            }
            else if constexpr (std::is_same_v<T, App>)
            {
                if (not std::holds_alternative<ConstructorApp>(e.head))
                    return false;

                for(const auto& arg: e.args)
                    if (not arg.is_value())
                        return false;

                return true;
            }
            else
                return false;
        });
    }

    bool Exp::is_whnf() const
    {
        return visit([](const auto& e) -> bool
        {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, Int> or
                          std::is_same_v<T, Double> or
                          std::is_same_v<T, LogDouble> or
                          std::is_same_v<T, Char> or
                          std::is_same_v<T, String> or
                          std::is_same_v<T, Integer> or
                          std::is_same_v<T, Constructor> or
                          std::is_same_v<T, ObjectValue> or
                          std::is_same_v<T, Lambda>)
            {
                return true;
            }
            else if constexpr (std::is_same_v<T, App>)
            {
                return std::holds_alternative<ConstructorApp>(e.head);
            }
            else
                return false;
        });
    }

    bool Exp::is_gcable_object_value() const
    {
        if (auto object_value = to<ObjectValue>())
            return is_gcable_type(object_value->value->type());
        else
            return false;
    }

    int Exp::as_int() const
    {
        return as<Int>().value;
    }

    double Exp::as_double() const
    {
        return as<Double>().value;
    }

    log_double_t Exp::as_log_double() const
    {
        return as<LogDouble>().value;
    }

    char Exp::as_char() const
    {
        return as<Char>().value;
    }

    const std::string& Exp::as_string() const
    {
        return as<String>().value;
    }

    const integer& Exp::as_integer() const
    {
        return as<Integer>().value;
    }

    const constructor& Exp::as_constructor() const
    {
        return as<Constructor>().value;
    }

    Exp apply(Exp function, vector<Exp> args)
    {
        args.insert(args.begin(), std::move(function));
        return App(FunctionApply{}, std::move(args));
    }

    Exp apply_env_function(int function_index, vector<Exp> args)
    {
        vector<Exp> app_args;
        app_args.push_back(IndexVar(function_index + int(args.size())));

        for(int i = int(args.size()) - 1; i >= 0; --i)
            app_args.push_back(IndexVar(i));

        return Let(std::move(args), App(FunctionApply{}, std::move(app_args)));
    }

    int count_lambdas(const Exp& E)
    {
        const Exp* E2 = &E;
        int n = 0;
        while(const auto* lambda = E2->to<Lambda>())
        {
            E2 = &lambda->body;
            n++;
        }
        return n;
    }

    Exp peel_lambdas(const Exp& E, int n)
    {
        const Exp* E2 = &E;
        for(int i=0;i<n;i++)
        {
            const auto* lambda = E2->to<Lambda>();
            assert(lambda);
            E2 = &lambda->body;
        }
        return *E2;
    }

    Exp atomic_value(const expression_ref& E)
    {
        switch(E.type())
        {
        case type_constant::int_type:
            return Int(E.as_int());
        case type_constant::double_type:
            return Double(E.as_double());
        case type_constant::log_double_type:
            return LogDouble(E.as_log_double());
        case type_constant::char_type:
            return Char(E.as_char());
        case type_constant::constructor_type:
            return Constructor(E.as_<constructor>());
        default:
            break;
        }

        if (const auto* s = E.to<::String>())
            return String(s->value());

        if (const auto* i = E.to<::Integer>())
            return Integer(i->value());

        if (E.is_object_type() and not E.is_expression())
            return ObjectValue(E.ptr());

        throw myexception()<<"Cannot convert non-atomic expression '"<<E<<"' to Runtime::Exp";
    }

    Exp e_op_value(const expression_ref& E)
    {
        if (E.is_expression() and E.head().is_a<constructor>())
        {
            const auto& head = E.head().as_<constructor>();
            if (E.size() != head.n_args())
                throw myexception()<<"Cannot convert partial constructor application '"<<E<<"' to Runtime::Exp";

            vector<Exp> args;
            for(const auto& arg: E.sub())
                args.push_back(e_op_value(arg));

            return App(ConstructorApp(head), std::move(args));
        }

        return atomic_value(E);
    }

    const Exp& rpair_first(const Exp& E)
    {
        auto pair = E.to<RPair>();
        assert(pair);
        return pair->first;
    }

    const Exp& rpair_second(const Exp& E)
    {
        auto pair = E.to<RPair>();
        assert(pair);
        return pair->second;
    }

    Exp shift_free_indices(const Exp& E, int amount, int depth)
    {
        return E.visit([&](const auto& e) -> Exp
        {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, Int> or
                          std::is_same_v<T, Double> or
                          std::is_same_v<T, LogDouble> or
                          std::is_same_v<T, Char> or
                          std::is_same_v<T, String> or
                          std::is_same_v<T, Integer> or
                          std::is_same_v<T, Constructor> or
                          std::is_same_v<T, ObjectValue> or
                          std::is_same_v<T, GlobalVar> or
                          std::is_same_v<T, RegRef>)
            {
                return e;
            }
            else if constexpr (std::is_same_v<T, IndexVar>)
            {
                if (e.index >= depth)
                    return IndexVar(e.index + amount);
                else
                    return e;
            }
            else if constexpr (std::is_same_v<T, Lambda>)
            {
                return Lambda(shift_free_indices(e.body, amount, depth + 1));
            }
            else if constexpr (std::is_same_v<T, Let>)
            {
                int n = e.binds.size();

                vector<Exp> binds;
                for(const auto& bind: e.binds)
                    binds.push_back(shift_free_indices(bind, amount, depth + n));

                return Let(binds, shift_free_indices(e.body, amount, depth + n));
            }
            else if constexpr (std::is_same_v<T, Case>)
            {
                vector<Alt> alts;
                for(const auto& alt: e.alts)
                    alts.push_back(Alt(alt.pattern, shift_free_indices(alt.body, amount, depth + pattern_arity(alt.pattern))));

                return Case(shift_free_indices(e.object, amount, depth), alts);
            }
            else if constexpr (std::is_same_v<T, App>)
            {
                vector<Exp> args;
                for(const auto& arg: e.args)
                    args.push_back(shift_free_indices(arg, amount, depth));

                return App(e.head, args);
            }
            else if constexpr (std::is_same_v<T, Trim>)
            {
                auto indices = e.indices;
                for(int& index: indices)
                    if (index >= depth)
                        index += amount;

                return Trim(indices, e.body);
            }
            else
                std::abort();
        });
    }

    std::shared_ptr<const Operation> operation_from_builtin(void* op, const std::string& lib_name, const std::string& func_name, const std::string& call_conv)
    {
        if (call_conv == "bpcall" or call_conv == "trcall")
            return std::make_shared<Operation>((o_operation_fn)op, lib_name+":"+func_name);
        else if (call_conv == "ecall")
            return std::make_shared<Operation>((e_operation_fn)op, lib_name+":"+func_name);
        else
            throw myexception()<<"Unrecognized calling convention '"<<call_conv<<"'";
    }

    OperationApp::OperationApp(std::shared_ptr<const Operation> op)
        :head(std::move(op))
    {
    }

    OperationApp::OperationApp()
    {
    }

    OperationApp::OperationApp(std::shared_ptr<const Operation> op, std::string lib, std::string func, std::string conv)
        :head(std::move(op)), lib_name(std::move(lib)), func_name(std::move(func)), call_conv(std::move(conv))
    {
    }

    OperationApp builtin_operation_app(void* op, const std::string& lib_name, const std::string& func_name, const std::string& call_conv)
    {
        return OperationApp(operation_from_builtin(op, lib_name, func_name, call_conv), lib_name, func_name, call_conv);
    }

    expression_ref to_expression_ref(const AppHead& head)
    {
        return std::visit([](const auto& h) -> expression_ref
        {
            using T = std::decay_t<decltype(h)>;

            if constexpr (std::is_same_v<T, FunctionApply>)
                return Apply();
            else if constexpr (std::is_same_v<T, ConstructorApp>)
                return h.head;
            else if constexpr (std::is_same_v<T, OperationApp>)
                return *h.head;
        }, head);
    }

    int pattern_arity(const Pattern& pattern)
    {
        return std::visit([](const auto& p) -> int
        {
            using T = std::decay_t<decltype(p)>;

            if constexpr (std::is_same_v<T, WildcardPattern>)
                return 0;
            else if constexpr (std::is_same_v<T, ConstructorPattern>)
                return p.head.n_args();
        }, pattern);
    }

    expression_ref to_expression_ref(const Pattern& pattern)
    {
        return std::visit([](const auto& p) -> expression_ref
        {
            using T = std::decay_t<decltype(p)>;

            if constexpr (std::is_same_v<T, WildcardPattern>)
                return var(-1);
            else if constexpr (std::is_same_v<T, ConstructorPattern>)
                return p.head;
        }, pattern);
    }

    expression_ref to_expression_ref(const Exp& E)
    {
        return E.visit([](const auto& e) -> expression_ref
        {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, Int>)
            {
                return e.value;
            }
            else if constexpr (std::is_same_v<T, Double>)
            {
                return double(e.value);
            }
            else if constexpr (std::is_same_v<T, LogDouble>)
            {
                return e.value;
            }
            else if constexpr (std::is_same_v<T, Char>)
            {
                return e.value;
            }
            else if constexpr (std::is_same_v<T, String>)
            {
                return ::String(e.value);
            }
            else if constexpr (std::is_same_v<T, Integer>)
            {
                return ::Integer(e.value);
            }
            else if constexpr (std::is_same_v<T, Constructor>)
            {
                return e.value;
            }
            else if constexpr (std::is_same_v<T, ObjectValue>)
            {
                return e.value;
            }
            else if constexpr (std::is_same_v<T, IndexVar>)
            {
                return index_var(e.index);
            }
            else if constexpr (std::is_same_v<T, GlobalVar>)
            {
                return e.name;
            }
            else if constexpr (std::is_same_v<T, RegRef>)
            {
                return reg_var(e.target);
            }
            else if constexpr (std::is_same_v<T, Lambda>)
            {
                return expression_ref(lambda2(), {to_expression_ref(e.body)});
            }
            else if constexpr (std::is_same_v<T, Let>)
            {
                vector<expression_ref> binds;
                for(const auto& bind: e.binds)
                    binds.push_back(to_expression_ref(bind));

                return indexed_let_expression(binds, to_expression_ref(e.body));
            }
            else if constexpr (std::is_same_v<T, Case>)
            {
                Expression::CaseAlts alts;
                for(const auto& alt: e.alts)
                    alts.push_back({to_expression_ref(alt.pattern), to_expression_ref(alt.body)});

                return make_case_expression(to_expression_ref(e.object), alts);
            }
            else if constexpr (std::is_same_v<T, App>)
            {
                vector<expression_ref> args;
                for(const auto& arg: e.args)
                    args.push_back(to_expression_ref(arg));

                return expression_ref(to_expression_ref(e.head), args);
            }
            else if constexpr (std::is_same_v<T, Trim>)
            {
                expression* V = new expression(::Trim());
                V->sub.push_back(Vector<int>(e.indices));
                V->sub.push_back(to_expression_ref(e.body));
                return V;
            }
            else
                std::abort();
        });
    }

    static std::string parenthesize_if(bool b, const std::string& s)
    {
        if (b)
            return "(" + s + ")";
        else
            return s;
    }

    static bool prints_atomically(const Exp& E)
    {
        return E.visit([](const auto& e) -> bool
        {
            using T = std::decay_t<decltype(e)>;
            return std::is_same_v<T, Int> or
                   std::is_same_v<T, Double> or
                   std::is_same_v<T, LogDouble> or
                   std::is_same_v<T, Char> or
                   std::is_same_v<T, String> or
                   std::is_same_v<T, Integer> or
                   std::is_same_v<T, Constructor> or
                   std::is_same_v<T, ObjectValue> or
                   std::is_same_v<T, IndexVar> or
                   std::is_same_v<T, GlobalVar> or
                   std::is_same_v<T, RegRef>;
        });
    }

    std::string print(const Pattern& pattern)
    {
        return std::visit([](const auto& p) -> std::string
        {
            using T = std::decay_t<decltype(p)>;

            if constexpr (std::is_same_v<T, WildcardPattern>)
                return "_";
            else if constexpr (std::is_same_v<T, ConstructorPattern>)
                return p.head.print();
        }, pattern);
    }

    std::string print(const AppHead& head)
    {
        return std::visit([](const auto& h) -> std::string
        {
            using T = std::decay_t<decltype(h)>;

            if constexpr (std::is_same_v<T, FunctionApply>)
                return "@";
            else if constexpr (std::is_same_v<T, ConstructorApp>)
                return h.head.print();
            else if constexpr (std::is_same_v<T, OperationApp>)
                return h.head->print();
        }, head);
    }

    std::string print(const Exp& E)
    {
        if (not E)
            return "NOEXP";

        return E.visit([](const auto& e) -> std::string
        {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, Int>)
            {
                return std::to_string(e.value) + "#";
            }
            else if constexpr (std::is_same_v<T, Double>)
            {
                return std::to_string(e.value) + "##";
            }
            else if constexpr (std::is_same_v<T, LogDouble>)
            {
                return std::to_string(e.value.log()) + "L#";
            }
            else if constexpr (std::is_same_v<T, Char>)
            {
                return std::string("'") + e.value + "'";
            }
            else if constexpr (std::is_same_v<T, String>)
            {
                return std::string("\"") + e.value + "\"#";
            }
            else if constexpr (std::is_same_v<T, Integer>)
            {
                return e.value.str();
            }
            else if constexpr (std::is_same_v<T, Constructor>)
            {
                return e.value.print();
            }
            else if constexpr (std::is_same_v<T, ObjectValue>)
            {
                return e.value->print();
            }
            else if constexpr (std::is_same_v<T, IndexVar>)
            {
                return "#" + std::to_string(e.index);
            }
            else if constexpr (std::is_same_v<T, GlobalVar>)
            {
                return e.name.print();
            }
            else if constexpr (std::is_same_v<T, RegRef>)
            {
                return "$" + std::to_string(e.target);
            }
            else if constexpr (std::is_same_v<T, Lambda>)
            {
                return "\\ -> " + print(e.body);
            }
            else if constexpr (std::is_same_v<T, Let>)
            {
                vector<std::string> binds;
                for(const auto& bind: e.binds)
                    binds.push_back(print(bind));
                return "let {" + join(binds, "; ") + "} in " + print(e.body);
            }
            else if constexpr (std::is_same_v<T, Case>)
            {
                vector<std::string> alts;
                for(const auto& alt: e.alts)
                    alts.push_back(print(alt.pattern) + " -> " + print(alt.body));

                return "case " + parenthesize_if(not prints_atomically(e.object), print(e.object)) +
                       " of {" + join(alts, "; ") + "}";
            }
            else if constexpr (std::is_same_v<T, App>)
            {
                vector<std::string> args;
                for(const auto& arg: e.args)
                    args.push_back(parenthesize_if(not prints_atomically(arg), print(arg)));

                if (std::holds_alternative<FunctionApply>(e.head) and args.size() >= 2)
                    return args[0] + " " + join(vector<std::string>(args.begin()+1, args.end()), " ");
                else
                    return print(e.head) + " " + join(args, " ");
            }
            else if constexpr (std::is_same_v<T, Trim>)
            {
                vector<std::string> indices;
                for(auto i: e.indices)
                    indices.push_back(std::to_string(i));
                return "Trim {" + join(indices, ",") + "} " +
                       parenthesize_if(not prints_atomically(e.body), print(e.body));
            }
        });
    }

    std::ostream& operator<<(std::ostream& o, const Exp& E)
    {
        return o << print(E);
    }

    std::string Exp::print() const
    {
        return Runtime::print(*this);
    }

    static void check_pattern_invariants(const Pattern& pattern)
    {
#ifndef NDEBUG
        std::visit([](const auto& p)
        {
            using T = std::decay_t<decltype(p)>;

            if constexpr (std::is_same_v<T, WildcardPattern>)
            {
            }
            else if constexpr (std::is_same_v<T, ConstructorPattern>)
            {
                assert(p.head.n_args() >= 0);
            }
        }, pattern);
#endif
    }

    static void check_app_head_invariants(const AppHead& head, int n_args)
    {
#ifndef NDEBUG
        std::visit([&](const auto& h)
        {
            using T = std::decay_t<decltype(h)>;

            if constexpr (std::is_same_v<T, FunctionApply>)
            {
                assert(n_args >= 2);
            }
            else if constexpr (std::is_same_v<T, ConstructorApp>)
            {
                assert(h.head.n_args() == n_args);
            }
            else if constexpr (std::is_same_v<T, OperationApp>)
            {
                assert(n_args >= 0);
            }
        }, head);
#endif
    }

    void check_invariants(const Exp& E)
    {
#ifndef NDEBUG
        assert(E);

        E.visit([](const auto& e)
        {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, Int> or
                          std::is_same_v<T, Double> or
                          std::is_same_v<T, LogDouble> or
                          std::is_same_v<T, Char> or
                          std::is_same_v<T, String> or
                          std::is_same_v<T, Integer>)
            {
            }
            else if constexpr (std::is_same_v<T, Constructor>)
            {
                assert(e.value.n_args() >= 0);
            }
            else if constexpr (std::is_same_v<T, ObjectValue>)
            {
                assert(e.value->type() != type_constant::expression_type);
                assert(e.value->type() != type_constant::constructor_type);
                assert(not dynamic_pointer_cast<const Box<std::string>>(e.value));
                assert(not dynamic_pointer_cast<const Box<integer>>(e.value));
            }
            else if constexpr (std::is_same_v<T, IndexVar>)
            {
                assert(e.index >= 0);
            }
            else if constexpr (std::is_same_v<T, GlobalVar>)
            {
                assert(not e.name.name.empty());
            }
            else if constexpr (std::is_same_v<T, RegRef>)
            {
                assert(e.target > 0);
            }
            else if constexpr (std::is_same_v<T, Lambda>)
            {
                check_invariants(e.body);
            }
            else if constexpr (std::is_same_v<T, Let>)
            {
                for(const auto& bind: e.binds)
                    check_invariants(bind);
                check_invariants(e.body);
            }
            else if constexpr (std::is_same_v<T, Case>)
            {
                check_invariants(e.object);
                for(const auto& alt: e.alts)
                {
                    check_pattern_invariants(alt.pattern);
                    check_invariants(alt.body);
                }
            }
            else if constexpr (std::is_same_v<T, App>)
            {
                check_app_head_invariants(e.head, e.args.size());
                for(const auto& arg: e.args)
                    check_invariants(arg);
            }
            else if constexpr (std::is_same_v<T, Trim>)
            {
                assert(e.indices.size() == 0 or e.indices.front() >= 0);
                for(int i = 1; i < e.indices.size(); i++)
                    assert(e.indices[i-1] < e.indices[i]);
                check_invariants(e.body);
            }
        });
#endif
    }

    void check_no_reg_refs(const Exp& E)
    {
#ifndef NDEBUG
        check_invariants(E);

        E.visit([](const auto& e)
        {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, RegRef>)
            {
                std::abort();
            }
            else if constexpr (std::is_same_v<T, Lambda>)
            {
                check_no_reg_refs(e.body);
            }
            else if constexpr (std::is_same_v<T, Let>)
            {
                for(const auto& bind: e.binds)
                    check_no_reg_refs(bind);
                check_no_reg_refs(e.body);
            }
            else if constexpr (std::is_same_v<T, Case>)
            {
                check_no_reg_refs(e.object);
                for(const auto& alt: e.alts)
                    check_no_reg_refs(alt.body);
            }
            else if constexpr (std::is_same_v<T, App>)
            {
                for(const auto& arg: e.args)
                    check_no_reg_refs(arg);
            }
            else if constexpr (std::is_same_v<T, Trim>)
            {
                check_no_reg_refs(e.body);
            }
        });
#endif
    }

    void check_translated(const Exp& E)
    {
#ifndef NDEBUG
        check_invariants(E);

        E.visit([](const auto& e)
        {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, GlobalVar>)
            {
                std::abort();
            }
            else if constexpr (std::is_same_v<T, Lambda>)
            {
                check_translated(e.body);
            }
            else if constexpr (std::is_same_v<T, Let>)
            {
                for(const auto& bind: e.binds)
                    check_translated(bind);
                check_translated(e.body);
            }
            else if constexpr (std::is_same_v<T, Case>)
            {
                check_translated(e.object);
                for(const auto& alt: e.alts)
                    check_translated(alt.body);
            }
            else if constexpr (std::is_same_v<T, App>)
            {
                for(const auto& arg: e.args)
                    check_translated(arg);
            }
            else if constexpr (std::is_same_v<T, Trim>)
            {
                check_translated(e.body);
            }
        });
#endif
    }
}
