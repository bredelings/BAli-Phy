#include <iostream>
#include <unordered_map>
#include "computation/machine/graph_register.H"
#include "computation/module.H"
#include "operations.H"
#include "computation/expression/let.H"
#include "computation/expression/case.H"
#include "computation/expression/var.H"
#include "computation/expression/lambda.H"
#include "computation/expression/indexify.H"
#include "computation/expression/constructor.H"
#include "computation/expression/expression.H" // for is_reglike( )
#include "computation/expression/convert.H" // for maybe_occ_to_expression_ref( )
#include "computation/expression/runtime_views.H"
#include "computation/runtime/ast.H"
#include "computation/runtime/trim.H"
#include "computation/fresh_vars.H"
#include "haskell/ids.H"
#include "util/variant.H"

using std::optional;
using std::string;
using std::vector;
using std::pair;
using std::set;
using std::map;

using std::cerr;
using std::endl;


CDecls graph_normalize(FreshVarSource& source, CDecls decls)
{
    // Just normalize the bound statements
    for(auto& [x,e]: decls)
        e = graph_normalize(source, e);

    return decls;
}

Core2::Decls<> graph_normalize(FreshVarSource& source, Core2::Decls<> decls);
Core2::Exp<> graph_normalize(FreshVarSource& source, const Core2::Exp<>& E);

Core2::Exp<> make_let(const Core2::Decls<>& decls, const Core2::Exp<>& body)
{
    if (decls.empty())
        return body;
    else
        return Core2::Let<>{decls, body};
}

bool is_simple_core_arg(const Core2::Exp<>& E, bool sub_exp_ok)
{
    if (E.to_var())
        return true;

    if (sub_exp_ok)
    {
        if (E.to_constant())
            return true;

        if (auto B = E.to_builtinOp(); B and B->call_conv == "ecall")
            return true;
    }

    return false;
}

std::tuple<Core2::Decls<>, Core2::Exp<>>
graph_normalize_lift(FreshVarSource& source, const Core2::Exp<>& E, bool sub_exp_ok)
{
    Core2::Decls<> decls;

    if (sub_exp_ok)
    {
        if (auto B = E.to_builtinOp(); B and B->call_conv == "ecall")
        {
            auto E2 = *B;

            for(auto& arg: E2.args)
            {
                auto [decls2, arg2] = graph_normalize_lift(source, arg, true);
                arg = arg2;
                std::ranges::move(decls2, std::back_inserter(decls));
            }

            return {decls, E2};
        }
    }

    auto E2 = graph_normalize(source, E);

    if (not is_simple_core_arg(E2, sub_exp_ok))
    {
        auto x = source.get_fresh_core_var("gn");
        decls.push_back({x, E2});
        E2 = x;
    }

    return {decls, E2};
}

Core2::Decls<> graph_normalize(FreshVarSource& source, Core2::Decls<> decls)
{
    for(auto& [_, e]: decls)
        e = graph_normalize(source, e);

    return decls;
}

Core2::Exp<> graph_normalize(FreshVarSource& source, const Core2::Exp<>& E)
{
    if (E.empty())
        return E;
    else if (E.to_var() or E.to_constant())
        return E;
    else if (auto L = E.to_lambda())
        return Core2::Lambda<>{L->x, graph_normalize(source, L->body)};
    else if (auto A = E.to_apply())
    {
        auto [head_decls, head] = graph_normalize_lift(source, A->head, false);
        auto [arg_decls, arg] = graph_normalize_lift(source, A->arg, false);

        head_decls.insert(head_decls.end(), arg_decls.begin(), arg_decls.end());
        return make_let(head_decls, Core2::Apply<>{head, arg});
    }
    else if (auto L = E.to_let())
    {
        auto decls = graph_normalize(source, L->decls);
        auto body = graph_normalize(source, L->body);
        return Core2::Let<>{decls, body};
    }
    else if (auto C = E.to_case())
    {
        auto [decls, object] = graph_normalize_lift(source, C->object, true);
        auto alts = C->alts;
        for(auto& [_, body]: alts)
            body = graph_normalize(source, body);

        return make_let(decls, Core2::Case<>{object, alts});
    }
    else if (auto C = E.to_conApp())
    {
        Core2::Decls<> decls;
        auto args = C->args;
        for(auto& arg: args)
        {
            auto [decls2, arg2] = graph_normalize_lift(source, arg, false);
            arg = arg2;
            std::ranges::move(decls2, std::back_inserter(decls));
        }

        return make_let(decls, Core2::ConApp<>{C->head, args});
    }
    else if (auto B = E.to_builtinOp())
    {
        Core2::Decls<> decls;
        auto args = B->args;
        bool sub_exp_ok = B->call_conv == "ecall";
        for(auto& arg: args)
        {
            auto [decls2, arg2] = graph_normalize_lift(source, arg, sub_exp_ok);
            arg = arg2;
            std::ranges::move(decls2, std::back_inserter(decls));
        }

        return make_let(decls, Core2::BuiltinOp<>{B->lib_name, B->func_name, B->call_conv, args, B->op});
    }
    else
        std::abort();
}

bool is_ok_arg(const expression_ref& arg, bool sub_exp_ok)
{
    if (is_reglike(arg)) return true;

    if (sub_exp_ok)
    {
        // This matches the condition for NOT lifting constants out of lambdas.
        if (arg.is_double() or arg.is_int() or arg.is_char() or arg.is_a<String>() or arg.is_a<Integer>())
            return true;

        if (auto O = arg.head().to<Operation>(); O and O->e_op)
            return true;
    }

    return false;
}


// If we have an expensive e_op that does something like ExpensiveOp1(1,ExpensiveOp2(2,x+y)), 
//  then we don't need to float out ExpensiveOp2, because ExpensiveOp1 can only be invalidated when
//  ExpensiveOp2 changes anyway.

std::tuple<CDecls,expression_ref> graph_normalize_lift(FreshVarSource& source, const expression_ref& E, bool sub_exp_ok)
{
    CDecls decls;
    if (sub_exp_ok)
    {
        // If we have something like 1 + (2*(4+factorial 5))) then we want to
        // (a) float the (factorial 5) out, and also
        // (b) treat the 2*_ as a cheap e_op.
        // So 
        if (auto O = E.head().to<Operation>(); O and O->e_op)
        {
            object_ptr<expression> E2 = E.as_expression().clone();

            for(auto& arg: E2->sub)
            {
                auto [decls2, arg2] = graph_normalize_lift(source, arg, true);

                arg = arg2;

                std::ranges::move(decls2, std::back_inserter(decls));
            }

            return {decls, *E2};
        }
    }

    auto E2 = graph_normalize(source, E);

    if (not is_ok_arg(E2, sub_exp_ok))
    {
        auto x = source.get_fresh_var();
        decls.push_back({x,E2});
        E2 = x;
    }

    return {decls, E2};
}

// PROBLEM: Ideally we want to normalize arguments and then analyze them.
// Right now, in order to handle e_ops, we have to analyze the e_ops before they are analyzed
//   in order to ensure that we only float vars that we have just created.
// SOLUTION: Do this in core and rename all the variables.
//   Then we can be sure that floating vars won't induce any aliasing.
expression_ref graph_normalize(FreshVarSource& source, const expression_ref& E)
{
    if (not E) return E;

    // 2. Lambda
    if (auto L = RuntimeView::lambda(E))
    {
	object_ptr<expression> V = E.as_expression().clone();
	V->sub[1] = graph_normalize(source, L->body);

	return V;
    }

    // 6. Case
    if (auto C = RuntimeView::case_(E))
    {
        auto object = C->object;
        auto alts = C->alts;

	// Just normalize the bodies
	for(auto& [pattern, body]: alts)
	    body = graph_normalize(source, body);

	// Normalize the object
	auto [decls2, object2] = graph_normalize_lift(source, object, true);

        return let_expression(decls2, make_case_expression(object2, alts));
    }

    // 5. Let
    if (auto Let = RuntimeView::let(E))
    {
        auto L = *Let->value;

	// Normalize the body
	L.body = graph_normalize(source, L.body);

	// Just normalize the bound statements
	L.binds = graph_normalize(source, L.binds);

	return L;
    }

    // 1. Var
    // 5. (partial) Literal constant.  Treat as 0-arg constructor.
    else if (not E.size()) return E;

    // 4. Constructor or Operation
    if (RuntimeView::constructor_app(E) or RuntimeView::operation_app(E))
    {
	object_ptr<expression> E2 = E.as_expression().clone();

        bool sub_exp_ok = false;
        if (auto O = E.head().to<Operation>(); O and O->e_op)
            sub_exp_ok = true;

	// Actually we probably just need x[i] not to be free in E.sub()[i]
	vector<pair<var, expression_ref>> decls;
	for(int i=0;i<E2->size();i++)
	{
            auto [decls2, arg2] = graph_normalize_lift(source, E.sub()[i], sub_exp_ok);

	    E2->sub[i] = arg2;

            std::ranges::move(decls2, std::back_inserter(decls));
	}

	return let_expression(decls, object_ptr<const expression>(E2));
    }

    throw myexception()<<"graph_normalize: I don't recognize expression '"+ E.print() + "'";
}

expression_ref graph_normalize(FreshVarState& state, const expression_ref& E)
{
    FreshVarSource source(state);
    return graph_normalize(source, E);
}

CDecls graph_normalize(FreshVarState& state, const CDecls& decls)
{
    FreshVarSource source(state);
    return graph_normalize(source, decls);
}

// Deleted: Fun_normalize( ).  Perhaps reintroduce later.
// Translate from language Basic to language Fun by introducing letrec expressions.  (arguments are already vars)
// See "From Natural Semantics to C: A Formal Derivation of two STG machines."
//      by Alberto de la Encina and Ricardo Pena.



closure graph_normalize(FreshVarState& state, closure&& C)
{
    C.exp = graph_normalize(state, expression_ref(C.exp));
    return std::move(C);
}

int pattern_arity(const expression_ref& pattern)
{
    if (pattern.head().is_a<constructor>())
        return pattern.head().as_<constructor>().n_args();
    else
        return 0;
}

Runtime::ExpPtr reg_heap::translate_refs(const Runtime::ExpPtr& E, closure::Env_t& Env, int depth)
{
    return std::visit([&](const auto& e) -> Runtime::ExpPtr
    {
        using T = std::decay_t<decltype(e)>;

        if constexpr (std::is_same_v<T, Runtime::GlobalVar>)
        {
            int index = depth + Env.size();
            Env.insert(Env.begin(), reg_for_id(e.name));
            return Runtime::make(Runtime::IndexVar{index});
        }
        else if constexpr (std::is_same_v<T, Runtime::RegRef>)
        {
            int index = depth + Env.size();
            Env.insert(Env.begin(), e.target);
            return Runtime::make(Runtime::IndexVar{index});
        }
        else if constexpr (std::is_same_v<T, Runtime::Atom> or std::is_same_v<T, Runtime::IndexVar>)
        {
            return Runtime::make(e);
        }
        else if constexpr (std::is_same_v<T, Runtime::Lambda>)
        {
            return Runtime::make(Runtime::Lambda{translate_refs(e.body, Env, depth + 1)});
        }
        else if constexpr (std::is_same_v<T, Runtime::Let>)
        {
            int n = e.binds.size();

            vector<Runtime::ExpPtr> binds;
            for(const auto& bind: e.binds)
                binds.push_back(translate_refs(bind, Env, depth + n));

            return Runtime::make(Runtime::Let{binds, translate_refs(e.body, Env, depth + n)});
        }
        else if constexpr (std::is_same_v<T, Runtime::Case>)
        {
            vector<Runtime::Alt> alts;
            for(const auto& alt: e.alts)
                alts.push_back({alt.pattern, translate_refs(alt.body, Env, depth + pattern_arity(alt.pattern))});

            return Runtime::make(Runtime::Case{translate_refs(e.object, Env, depth), alts});
        }
        else if constexpr (std::is_same_v<T, Runtime::App>)
        {
            vector<Runtime::ExpPtr> args;
            for(const auto& arg: e.args)
                args.push_back(translate_refs(arg, Env, depth));

            return Runtime::make(Runtime::App{e.head, args});
        }
        else if constexpr (std::is_same_v<T, Runtime::Trim>)
        {
            return Runtime::make(Runtime::Trim{e.indices, translate_refs(e.body, Env, depth)});
        }
        else
            std::abort();
    }, E->value);
}

closure indexify_translate_and_trim(reg_heap& heap, closure&& C)
{
    auto E = runtime_indexify(expression_ref(C.exp));
    E = heap.translate_refs(E, C.Env);
    E = Runtime::trim_normalize(E);
    C.exp = Runtime::to_expression_ref(E);
    return std::move(C);
}

closure reg_heap::preprocess(const Core2::Exp<>& E)
{
    FreshVarSource source(fresh_var_state);
    auto E2 = graph_normalize(source, E);
    return indexify_translate_and_trim(*this, closure(to_expression_ref(E2)));
}

closure reg_heap::preprocess(const closure& C)
{
    assert(C.exp);
    return indexify_translate_and_trim(*this, graph_normalize( fresh_var_state, closure(C) ) );
}

int reg_heap::reg_for_id(const var& x)
{
    auto& name = x.name;
    assert(is_qualified_symbol(name) or is_haskell_builtin_con_name(name));
    auto loc = identifiers.find( x.name );
    if (loc == identifiers.end())
    {
	if (is_haskell_builtin_con_name(name))
	{
            assert(x.index == 0);

            auto sym = lookup_builtin_symbol(name);
            auto code = maybe_occ_to_expression_ref(to<CoreUnfolding>(sym->unfolding)->expr);
            add_identifier(x.name);

	    // get the root for each identifier
	    loc = identifiers.find(x.name);
	    assert(loc != identifiers.end());

	    int R = loc->second;

	    assert(R != -1);
	    set_C(R, preprocess(code) );
	}
	else
	    throw myexception()<<"Can't translate undefined identifier '"<<x<<"' in expression!";
    }

    return loc->second;
}
