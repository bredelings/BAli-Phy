#include "ast.H"
#include "computation/expression/case.H"
#include "computation/expression/constructor.H"
#include "computation/expression/lambda.H"
#include "computation/expression/let.H"
#include "computation/expression/runtime_views.H"
#include "computation/expression/trim.H"
#include "computation/operation.H"
#include "util/myexception.H"

using std::vector;

namespace Runtime
{
    ExpPtr make(const Atom& a)   { return std::make_shared<Exp>(a); }
    ExpPtr make(const Lambda& l) { return std::make_shared<Exp>(l); }
    ExpPtr make(const Let& l)    { return std::make_shared<Exp>(l); }
    ExpPtr make(const Case& c)   { return std::make_shared<Exp>(c); }
    ExpPtr make(const App& a)    { return std::make_shared<Exp>(a); }
    ExpPtr make(const Trim& t)   { return std::make_shared<Exp>(t); }

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
                alts.push_back({alt.pattern, from_indexed_expression_ref(alt.body)});

            return make(Case{from_indexed_expression_ref(C->object), alts});
        }

        if (auto T = RuntimeView::trim(E))
        {
            assert(T->value->size() == 2);
            return make(Trim{T->value->sub()[0].as_<Vector<int>>(),
                             from_indexed_expression_ref(T->value->sub()[1])});
        }

        if (E.is_atomic())
            return make(Atom{E});

        if (RuntimeView::apply(E) or RuntimeView::constructor_app(E) or RuntimeView::operation_app(E))
        {
            vector<ExpPtr> args;
            for(const auto& arg: E.sub())
                args.push_back(from_indexed_expression_ref(arg));

            return make(App{E.head(), args});
        }

        throw myexception()<<"Cannot convert expression_ref '"<<E<<"' to Runtime::Exp";
    }

    expression_ref to_expression_ref(const ExpPtr& E)
    {
        return std::visit([](const auto& e) -> expression_ref
        {
            using T = std::decay_t<decltype(e)>;

            if constexpr (std::is_same_v<T, Atom>)
            {
                return e.value;
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
                    alts.push_back({alt.pattern, to_expression_ref(alt.body)});

                return make_case_expression(to_expression_ref(e.object), alts);
            }
            else if constexpr (std::is_same_v<T, App>)
            {
                vector<expression_ref> args;
                for(const auto& arg: e.args)
                    args.push_back(to_expression_ref(arg));

                return expression_ref(e.head, args);
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
