#include "ast.H"
#include <cstdlib>
#include <iostream>
#include <set>
#include <string>
#include <typeinfo>
#include "computation/expression/apply.H"
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

using std::vector;

namespace Runtime
{
    ExpPtr make(const IntLiteral& l) { return std::make_shared<Exp>(l); }
    ExpPtr make(const DoubleLiteral& l) { return std::make_shared<Exp>(l); }
    ExpPtr make(const LogDoubleLiteral& l) { return std::make_shared<Exp>(l); }
    ExpPtr make(const CharLiteral& l) { return std::make_shared<Exp>(l); }
    ExpPtr make(const StringLiteral& l) { return std::make_shared<Exp>(l); }
    ExpPtr make(const IntegerLiteral& l) { return std::make_shared<Exp>(l); }
    ExpPtr make(const ConstructorValue& c) { return std::make_shared<Exp>(c); }
    ExpPtr make(const IndexVar& i) { return std::make_shared<Exp>(i); }
    ExpPtr make(const GlobalVar& g) { return std::make_shared<Exp>(g); }
    ExpPtr make(const RegRef& r) { return std::make_shared<Exp>(r); }
    ExpPtr make(const Lambda& l) { return std::make_shared<Exp>(l); }
    ExpPtr make(const Let& l)    { return std::make_shared<Exp>(l); }
    ExpPtr make(const Case& c)   { return std::make_shared<Exp>(c); }
    ExpPtr make(const App& a)    { return std::make_shared<Exp>(a); }
    ExpPtr make(const Trim& t)   { return std::make_shared<Exp>(t); }

    AppHead app_head_from_expression_ref(const expression_ref& head)
    {
        if (head.is_a<Apply>())
            return FunctionApply{};
        else if (head.is_a<constructor>())
            return ConstructorApp{head.as_<constructor>()};
        else if (head.is_a<Operation>())
            return OperationApp{head.as_<Operation>()};
        else
            std::abort();
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
                return h.head;
        }, head);
    }

    Pattern pattern_from_expression_ref(const expression_ref& pattern)
    {
        if (is_wildcard(pattern))
            return WildcardPattern{};
        else if (pattern.head().is_a<constructor>())
            return ConstructorPattern{pattern.head().as_<constructor>()};
        else
            std::abort();
    }

    int expression_pattern_arity(const expression_ref& pattern)
    {
        if (pattern.head().is_a<constructor>())
            return pattern.head().as_<constructor>().n_args();
        else
            return 0;
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

    ExpPtr atom_from_expression_ref(const expression_ref& E)
    {
        if (E.is_int())
            return make(IntLiteral{E.as_int()});
        else if (E.is_double())
            return make(DoubleLiteral{E.as_double()});
        else if (E.is_log_double())
            return make(LogDoubleLiteral{E.as_log_double()});
        else if (E.is_char())
            return make(CharLiteral{E.as_char()});
        else if (E.is_a<String>())
            return make(StringLiteral{E.as_<String>().value()});
        else if (E.is_a<Integer>())
            return make(IntegerLiteral{E.as_<Integer>().value()});
        else if (is_constructor(E))
            return make(ConstructorValue{E.as_<constructor>()});
        else
        {
            std::abort();
        }
    }

    ExpPtr from_indexed_expression_ref(const expression_ref& E)
    {
        if (not E)
            throw myexception()<<"Cannot convert empty expression_ref to Runtime::Exp";

        if (auto L = RuntimeView::indexed_lambda(E))
            return make(Lambda{from_indexed_expression_ref(L->body)});

        if (auto L = RuntimeView::indexed_let(E))
        {
            vector<ExpPtr> binds;
            for(const auto& bind: L->value.binds)
                binds.push_back(from_indexed_expression_ref(bind));

            return make(Let{binds, from_indexed_expression_ref(L->value.body)});
        }

        if (auto C = RuntimeView::case_(E))
        {
            vector<Alt> alts;
            for(const auto& alt: C->alts)
                alts.push_back({pattern_from_expression_ref(alt.pattern), from_indexed_expression_ref(alt.body)});

            return make(Case{from_indexed_expression_ref(C->object), alts});
        }

        if (auto T = RuntimeView::trim(E))
        {
            assert(T->value->size() == 2);
            return make(Trim{T->value->sub()[0].as_<Vector<int>>(),
                             from_indexed_expression_ref(T->value->sub()[1])});
        }

        if (E.is_index_var())
            return make(IndexVar{E.as_index_var()});

        if (E.is_reg_var())
            return make(RegRef{E.as_reg_var()});

        if (is_qualified_var(E))
            return make(GlobalVar{E.as_<var>()});

        if (E.is_a<var>() and is_haskell_builtin_con_name(E.as_<var>().name))
            return make(GlobalVar{E.as_<var>()});

        if (E.is_atomic())
            return atom_from_expression_ref(E);

        if (RuntimeView::apply(E) or RuntimeView::constructor_app(E) or RuntimeView::operation_app(E))
        {
            vector<ExpPtr> args;
            for(const auto& arg: E.sub())
                args.push_back(from_indexed_expression_ref(arg));

            return make(App{app_head_from_expression_ref(E.head()), args});
        }

        throw myexception()<<"Cannot convert expression_ref '"<<E<<"' to Runtime::Exp";
    }

    expression_ref to_expression_ref(const ExpPtr& E)
    {
        return std::visit([](const auto& e) -> expression_ref
        {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, IntLiteral>)
            {
                return e.value;
            }
            else if constexpr (std::is_same_v<T, DoubleLiteral>)
            {
                return e.value;
            }
            else if constexpr (std::is_same_v<T, LogDoubleLiteral>)
            {
                return e.value;
            }
            else if constexpr (std::is_same_v<T, CharLiteral>)
            {
                return e.value;
            }
            else if constexpr (std::is_same_v<T, StringLiteral>)
            {
                return String(e.value);
            }
            else if constexpr (std::is_same_v<T, IntegerLiteral>)
            {
                return Integer(e.value);
            }
            else if constexpr (std::is_same_v<T, ConstructorValue>)
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
                Core::Alts alts;
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
        }, E->value);
    }
}
