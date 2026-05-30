#include <iostream>
#include <unordered_map>
#include "computation/machine/graph_register.H"
#include "computation/module.H"
#include "operations.H"
#include "computation/expression/let.H"
#include "computation/expression/case.H"
#include "computation/expression/var.H"
#include "computation/expression/lambda.H"
#include "computation/expression/trim.H"
#include "computation/expression/index_var.H"
#include "computation/expression/indexify.H"
#include "computation/expression/constructor.H"
#include "computation/expression/reg_var.H"
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

closure indexify(closure&& C)
{
    auto E = runtime_indexify(expression_ref(C.exp));
    C.exp = Runtime::to_expression_ref(E);
    return std::move(C);
}

closure trim_normalize(closure&& C)
{
    auto E = Runtime::from_indexed_expression_ref(expression_ref(C.exp));
    E = Runtime::trim_normalize(E);
    C.exp = Runtime::to_expression_ref(E);
    return std::move(C);
}

closure reg_heap::preprocess(const Core2::Exp<>& E)
{
    FreshVarSource source(fresh_var_state);
    auto E2 = graph_normalize(source, E);
    return trim_normalize( indexify( translate_refs( closure(to_expression_ref(E2)) ) ) );
}

closure reg_heap::preprocess(const closure& C)
{
    assert(C.exp);
    //  return trim_normalize( indexify( Fun_normalize( graph_normalize( let_float( translate_refs( closure(C) ) ) ) ) ) );
    return trim_normalize( indexify( graph_normalize( fresh_var_state, translate_refs( closure(C) ) ) ) );
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

expression_ref reg_heap::translate_refs(const expression_ref& E, closure::Env_t& Env)
{
    optional<int> reg;

    // Replace dummies that are either qualified ids, or builtin constructor names
    if (is_qualified_var(E))
    {
	reg = reg_for_id( E.as_<var>() );
    }
    else if (E.is_a<var>() and is_haskell_builtin_con_name(E.as_<var>().name))
    {
	reg = reg_for_id( E.as_<var>() );
    }

    // Replace parameters with the appropriate reg_var: of value whatever
    else if (E.is_reg_var())
	reg = E.as_reg_var();

    if (reg)
    {
	int index = Env.size();
	Env.insert(Env.begin(), *reg);

	return index_var(index);
    }
    else if (is_let_expression(E))
    {
        auto L = E.as_<let_exp>();
        for(auto& [_,e]: L.binds)
            e = translate_refs(e, Env);
        L.body = translate_refs(L. body, Env);
        return L;
    }
    else if (is_case(E))
    {
        auto object = translate_refs(E.sub()[0], Env);
        auto alts = E.sub()[1].as_<Core::Alts>();
        for(auto& alt: alts)
            alt.body = translate_refs(alt.body, Env);
        return make_case_expression(object, alts);
    }
    
    // Other constants have no parts, and don't need to be translated
    else if (not E.size()) return E;
    else
    {
        // Translate the parts of the expression
        object_ptr<expression> V = E.as_expression().clone();
        for(int i=0;i<V->size();i++)
            V->sub[i] = translate_refs(V->sub[i], Env);

        return V;
    }
}

closure reg_heap::translate_refs(closure&& C)
{
    closure C2 = C;
    C2.exp = translate_refs(C2.exp, C2.Env);
    return C2;
}
