#include "ast.H"
#include <cassert>
#include <cstdlib>
#include <ostream>
#include "computation/haskell/Integer.H"
#include "computation/haskell/ids.H"
#include "computation/operation.H"
#include "util/myexception.H"
#include "util/string/join.H"
#include "util/string/convert.H"
#include "util/utf8.H"

using std::vector;

namespace Runtime
{

    // Reuse the immutable nullary Bool constructors instead of allocating a
    // ConstructorApp for every Bool result from simple interpreter calls.
    Exp::Exp(bool x)
    {
        static const ExpPtr<ConstructorApp> true_exp(new ConstructorApp(bool_true_name, 0, {}));
        static const ExpPtr<ConstructorApp> false_exp(new ConstructorApp(bool_false_name, 0, {}));
        value = x ? true_exp : false_exp;
    }
    Exp::Exp(std::string x):Exp(String(std::move(x))) {}
    Exp::Exp(const char* x):Exp(String(x)) {}
    Exp::Exp(integer x):Exp(Integer(std::move(x))) {}

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
        if (lib_name != app.lib_name or func_name != app.func_name or call_conv != app.call_conv or args != app.args)
            return false;

        if (head == app.head)
            return true;

        if (not head or not app.head)
            return false;

        return *head == static_cast<const Object&>(*app.head);
    }

    RVector::RVector(const vector<int>& values)
    {
        reserve(values.size());
        for(auto value: values)
            push_back(value);
    }

    RVector::RVector(const vector<char32_t>& values)
    {
        reserve(values.size());
        for(auto value: values)
            push_back(value);
    }

    RVector::operator vector<int>() const
    {
        vector<int> values;
        values.reserve(size());
        for(const auto& value: *this)
            values.push_back(value.as_int());
        return values;
    }

    RVector::operator vector<char32_t>() const
    {
        vector<char32_t> values;
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

        return exp_variant::visit([](const auto& a, const auto& b) -> bool
        {
            using A = std::decay_t<decltype(a)>;
            using B = std::decay_t<decltype(b)>;

            if constexpr (std::is_same_v<A, B>)
            {
                if constexpr (std::is_same_v<A, std::monostate>)
                    return true;
                else if constexpr (is_boxed_exp_storage<A>::value)
                    return *a == *b;
                else
                    return a == b;
            }
            else
                return false;
        }, x.value, y.value);
    }

    bool Exp::is_atomic_value() const
    {
        return visit([](const auto& e) -> bool
        {
            using T = std::decay_t<decltype(e)>;
            if constexpr (std::is_same_v<T, ConstructorApp>)
                return e.args.empty();
            else
                return std::is_same_v<T, Int> or
                       std::is_same_v<T, Double> or
                       std::is_same_v<T, LogDouble> or
                       std::is_same_v<T, Char> or
                       std::is_same_v<T, String> or
                       std::is_same_v<T, Integer> or
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
                          std::is_same_v<T, ObjectValue>)
            {
                return true;
            }
            else if constexpr (std::is_same_v<T, ConstructorApp>)
            {
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
                          std::is_same_v<T, ObjectValue> or
                          std::is_same_v<T, Lambda>)
            {
                return true;
            }
            else if constexpr (std::is_same_v<T, ConstructorApp>)
            {
                return true;
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

    Exp apply(Exp function, vector<Exp> args)
    {
        return FunctionApp(std::move(function), std::move(args));
    }

    Exp apply_env_function(int function_index, vector<Exp> args)
    {
        auto head = IndexVar(function_index + int(args.size()));

        vector<Exp> app_args;
        for(int i = int(args.size()) - 1; i >= 0; --i)
            app_args.push_back(IndexVar(i));

        // NOTE: This preserves the existing recursive indexing of the argument
        // batch.  Nested NonRec lets require removing and recalculating shifts.
        return Let({Rec(std::move(args))}, FunctionApp(std::move(head), std::move(app_args)));
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

    const Exp& rpair_first(const Exp& E)
    {
        return E.as_<RPair>().first;
    }

    const Exp& rpair_second(const Exp& E)
    {
        return E.as_<RPair>().second;
    }

    object_ptr<const Operation> operation_from_builtin(void* op, const std::string& lib_name, const std::string& func_name, const std::string& call_conv)
    {
        if (call_conv == "bpcall" or call_conv == "trcall")
            return new Operation((o_operation_fn)op, lib_name+":"+func_name);
        else if (call_conv == "ecall")
            return new Operation((e_operation_fn)op, lib_name+":"+func_name);
        else
            throw myexception()<<"Unrecognized calling convention '"<<call_conv<<"'";
    }

    OperationApp builtin_operation_app(void* op, const std::string& lib_name, const std::string& func_name, const std::string& call_conv)
    {
        return OperationApp(operation_from_builtin(op, lib_name, func_name, call_conv), lib_name, func_name, call_conv);
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

    bool has_constructor(const Exp& E, const std::string& name)
    {
        if (const auto* app = E.to<ConstructorApp>())
            return app->head.name() == name;
        else
            return false;
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
            if constexpr (std::is_same_v<T, ConstructorApp>)
                return e.args.empty();
            else
                return std::is_same_v<T, Int> or
                       std::is_same_v<T, Double> or
                       std::is_same_v<T, LogDouble> or
                       std::is_same_v<T, Char> or
                       std::is_same_v<T, String> or
                       std::is_same_v<T, Integer> or
                       std::is_same_v<T, ObjectValue> or
                       std::is_same_v<T, IndexVar> or
                       std::is_same_v<T, RegRef> or
                       std::is_same_v<T, GlobalVar>;
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

    static std::string print_operation_head(const OperationApp& app)
    {
        return app.head->print();
    }

    // Format a runtime Char as a Haskell-style character literal for debugging
    // and diagnostics.  Runtime Char now stores a Unicode scalar value.
    static std::string print_char_literal(char32_t c)
    {
        if (not utf8::is_scalar_value(c))
            throw myexception()<<"Invalid Runtime character code point: "<<static_cast<std::uint32_t>(c);

        std::string payload;
        if (c == U'\a') payload = "\\a";
        else if (c == U'\b') payload = "\\b";
        else if (c == U'\f') payload = "\\f";
        else if (c == U'\n') payload = "\\n";
        else if (c == U'\r') payload = "\\r";
        else if (c == U'\t') payload = "\\t";
        else if (c == U'\v') payload = "\\v";
        else if (c == U'\\') payload = "\\\\";
        else if (c == U'\'') payload = "\\'";
        else if (0x20 <= c and c <= 0x7E) payload = std::string(1, static_cast<char>(c));
        else payload = "\\" + std::to_string(static_cast<std::uint32_t>(c));

        return "'" + payload + "'";
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
                return print_char_literal(e.value);
            }
            else if constexpr (std::is_same_v<T, String>)
            {
                return std::string("\"") + e.value + "\"#";
            }
            else if constexpr (std::is_same_v<T, Integer>)
            {
                return e.value.str();
            }
            else if constexpr (std::is_same_v<T, ObjectValue>)
            {
                return e.value->print();
            }
            else if constexpr (std::is_same_v<T, IndexVar>)
            {
                return "#" + std::to_string(e.index);
            }
            else if constexpr (std::is_same_v<T, RegRef>)
            {
                return "$" + std::to_string(e.target);
            }
            else if constexpr (std::is_same_v<T, GlobalVar>)
            {
                std::string s = e.name;
                if (e.index != 0)
                    s += "#" + convertToString(e.index);
                return s;
            }
            else if constexpr (std::is_same_v<T, Lambda>)
            {
                return "\\ -> " + print(e.body);
            }
            else if constexpr (std::is_same_v<T, Let>)
            {
                vector<std::string> groups;
                for(const auto& bind: e.binds)
                {
                    if (auto nonrec = std::get_if<NonRec>(&bind))
                        groups.push_back("let " + print(nonrec->rhs));
                    else
                    {
                        vector<std::string> rhss;
                        for(const auto& rhs: std::get<Rec>(bind).rhss)
                            rhss.push_back(print(rhs));
                        groups.push_back("letrec {" + join(rhss, "; ") + "}");
                    }
                }
                return join(groups, "; ") + " in " + print(e.body);
            }
            else if constexpr (std::is_same_v<T, Case>)
            {
                vector<std::string> alts;
                for(const auto& alt: e.alts)
                    alts.push_back(print(alt.pattern) + " -> " + print(alt.body));

                return "case " + parenthesize_if(not prints_atomically(e.object), print(e.object)) +
                       " of {" + join(alts, "; ") + "}";
            }
            else if constexpr (std::is_same_v<T, FunctionApp>)
            {
                vector<std::string> args;
                args.push_back(parenthesize_if(not prints_atomically(e.head), print(e.head)));
                for(const auto& arg: e.args)
                    args.push_back(parenthesize_if(not prints_atomically(arg), print(arg)));

                if (args.size() >= 2)
                    return args[0] + " " + join(vector<std::string>(args.begin()+1, args.end()), " ");
                else
                    return "@ " + join(args, " ");
            }
            else if constexpr (std::is_same_v<T, ConstructorApp>)
            {
                if (e.args.empty())
                    return e.head.print();

                vector<std::string> args;
                for(const auto& arg: e.args)
                    args.push_back(parenthesize_if(not prints_atomically(arg), print(arg)));

                return e.head.print() + " " + join(args, " ");
            }
            else if constexpr (std::is_same_v<T, OperationApp>)
            {
                vector<std::string> args;
                for(const auto& arg: e.args)
                    args.push_back(parenthesize_if(not prints_atomically(arg), print(arg)));

                return print_operation_head(e) + " " + join(args, " ");
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

    static void check_app_invariants(const FunctionApp& app)
    {
#ifndef NDEBUG
        assert(app.head);
        assert(app.args.size() >= 1);
#endif
    }

    static void check_app_invariants(const ConstructorApp& app)
    {
#ifndef NDEBUG
        assert(app.head.n_args() == app.args.size());
#endif
    }

    static void check_app_invariants(const OperationApp& app)
    {
#ifndef NDEBUG
        assert(app.head);
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
            else if constexpr (std::is_same_v<T, ObjectValue>)
            {
                assert(e.value->type() != type_constant::constructor_type);
                assert(not dynamic_pointer_cast<const Box<std::string>>(e.value));
                assert(not dynamic_pointer_cast<const Box<integer>>(e.value));
            }
            else if constexpr (std::is_same_v<T, IndexVar>)
            {
                assert(e.index >= 0);
            }
            else if constexpr (std::is_same_v<T, RegRef>)
            {
                assert(e.target > 0);
            }
            else if constexpr (std::is_same_v<T, GlobalVar>)
            {
                assert(not e.name.empty());
            }
            else if constexpr (std::is_same_v<T, Lambda>)
            {
                check_invariants(e.body);
            }
            else if constexpr (std::is_same_v<T, Let>)
            {
                assert(not e.binds.empty());
                assert(not e.body.template to<Let>());
                for(const auto& bind: e.binds)
                    if (auto nonrec = std::get_if<NonRec>(&bind))
                        check_invariants(nonrec->rhs);
                    else
                        for(const auto& rhs: std::get<Rec>(bind).rhss)
                            check_invariants(rhs);
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
            else if constexpr (std::is_same_v<T, FunctionApp> or
                               std::is_same_v<T, ConstructorApp> or
                               std::is_same_v<T, OperationApp>)
            {
                check_app_invariants(e);
                if constexpr (std::is_same_v<T, FunctionApp>)
                    check_invariants(e.head);
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
                    if (auto nonrec = std::get_if<NonRec>(&bind))
                        check_no_reg_refs(nonrec->rhs);
                    else
                        for(const auto& rhs: std::get<Rec>(bind).rhss)
                            check_no_reg_refs(rhs);
                check_no_reg_refs(e.body);
            }
            else if constexpr (std::is_same_v<T, Case>)
            {
                check_no_reg_refs(e.object);
                for(const auto& alt: e.alts)
                    check_no_reg_refs(alt.body);
            }
            else if constexpr (std::is_same_v<T, FunctionApp> or
                               std::is_same_v<T, ConstructorApp> or
                               std::is_same_v<T, OperationApp>)
            {
                if constexpr (std::is_same_v<T, FunctionApp>)
                    check_no_reg_refs(e.head);
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
                    if (auto nonrec = std::get_if<NonRec>(&bind))
                        check_translated(nonrec->rhs);
                    else
                        for(const auto& rhs: std::get<Rec>(bind).rhss)
                            check_translated(rhs);
                check_translated(e.body);
            }
            else if constexpr (std::is_same_v<T, Case>)
            {
                check_translated(e.object);
                for(const auto& alt: e.alts)
                    check_translated(alt.body);
            }
            else if constexpr (std::is_same_v<T, FunctionApp> or
                               std::is_same_v<T, ConstructorApp> or
                               std::is_same_v<T, OperationApp>)
            {
                if constexpr (std::is_same_v<T, FunctionApp>)
                    check_translated(e.head);
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
