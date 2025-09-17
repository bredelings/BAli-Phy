#include "convert.H"
#include "computation/expression/var.H"
#include "computation/expression/lambda.H"
#include "computation/expression/apply.H"
#include "computation/expression/let.H"
#include "computation/expression/case.H"
#include "computation/expression/constructor.H"
#include "computation/haskell/Integer.H"
#include "util/variant.H"

#include <range/v3/all.hpp>

using std::set;
using std::vector;
using std::multiset;
using std::string;

//----------------------------------------------------------------------//

var to_var(const Core2::Var<>& V)
{
    return var(V.name, V.index, V.is_exported);
}

expression_ref var_to_expression_ref(const Core2::Var<>& V)
{
    return to_var(V);
}

expression_ref patarg_to_expression_ref(const Core2::Var<>& V)
{
    return to_var(V);
}

expression_ref to_expression_ref(const Core2::Lambda<>& L)
{
    return lambda_quantify(to_var(L.x), to_expression_ref(L.body));
}

expression_ref to_expression_ref(const Core2::Apply<>& A)
{
    // Note: we need to turn multiple single-arg applications into one multi-arg application!
    vector<expression_ref> args;

    Core2::Exp<> E = A;
    while(auto A2 = E.to_apply())
    {
        args.push_back( to_expression_ref(A2->arg) );
        E = A2->head;
    }

    args |= ranges::actions::reverse;
    
    return apply_expression(to_expression_ref(E), args );
}

vector<CDecls> decl_groups_to_expression_ref(const vector<Core2::Decls<>>& core_decl_groups)
{
    vector<CDecls> decl_groups;
    for(auto& core_decl_group: core_decl_groups)
	decl_groups.push_back(to_expression_ref(core_decl_group));
    return decl_groups;
}

CDecls to_expression_ref(const Core2::Decls<>& decls)
{
    CDecls decls2;
    for(auto& [x,E]: decls)
	decls2.push_back({to_var(x),to_expression_ref(E)});
    return decls2;
}

CDecl to_expression_ref(const Core2::Decl<>& decl)
{
    auto& [x,E] = decl;
    return {to_var(x),to_expression_ref(E)};
}

expression_ref to_expression_ref(const Core2::Let<>& L)
{
    auto decls = to_expression_ref(L.decls);
    auto body = to_expression_ref(L.body);
    return let_expression(decls, body);
}

expression_ref to_expression_ref(const Core2::Pattern<>& P)
{
    if (P.is_wildcard_pat())
	return var(-1);
    else
    {
	auto args = P.args | ranges::views::transform( patarg_to_expression_ref ) | ranges::to<vector>();
	return expression_ref(constructor(*P.head, args.size()), args);
    }
}

expression_ref to_expression_ref(const Core2::Var<>& P)
{
    return to_var(P);
}

Core::Alts to_expression_ref(const vector<Core2::Alt<>>& A)
{
    Core::Alts alts;
    for(auto [pat,body]: A)
	alts.push_back({to_expression_ref(pat), to_expression_ref(body)});
    return alts;
}

expression_ref to_expression_ref(const Core2::Case<>& C)
{
    auto object = to_expression_ref(C.object);
    auto alts = to_expression_ref(C.alts);
    return make_case_expression(object, alts);
}

expression_ref to_expression_ref(const Core2::ConApp<>& C)
{
    vector<expression_ref> args;
    for(auto& arg: C.args)
	args.push_back( to_expression_ref(arg) );
    return expression_ref(constructor(C.head,C.args.size()),args);
}

// How can we do this?
expression_ref to_expression_ref(const Core2::BuiltinOp<>& B)
{
    Operation O( (operation_fn)B.op, B.lib_name+":"+B.func_name);

    vector<expression_ref> args;
    for(auto& arg: B.args)
	args.push_back( to_expression_ref(arg) );
    return expression_ref(O, args);
}

expression_ref to_expression_ref(const Core2::Constant& C)
{
    if (auto c = to<char>(C.value))
	return *c;
    else if (auto i = to<int>(C.value))
	return *i;
    else if (auto i = to<integer_container>(C.value))
	return Integer(i->i);
    else if (auto s = to<std::string>(C.value))
	return String(*s);
    else
	std::abort();
}

expression_ref to_expression_ref(const Core2::Exp<>& E)
{
    if (auto v = E.to_var())
	return to_var(*v);
    else if (auto l = E.to_lambda())
	return to_expression_ref(*l);
    else if (auto a = E.to_apply())
	return to_expression_ref(*a);
    else if (auto l = E.to_let())
	return to_expression_ref(*l);
    else if (auto c = E.to_case())
	return to_expression_ref(*c);
    else if (auto c = E.to_conApp())
	return to_expression_ref(*c);
    else if (auto b = E.to_builtinOp())
	return to_expression_ref(*b);
    else if (auto c = E.to_constant())
	return to_expression_ref(*c);

    std::abort();
}

expression_ref maybe_to_expression_ref(const std::optional<Core2::Exp<>>& E)
{
    if (not E)
	return {};
    else
	return to_expression_ref(*E);
}

//------------------------------------------------------------------------------------------------------------

expression_ref occ_to_expression_ref(const Occ::Exp& E)
{
    return to_expression_ref(to_core_exp(E));
}

expression_ref maybe_occ_to_expression_ref(const std::optional<Occ::Exp>& E)
{
    if (not E)
	return {};
    else
	return occ_to_expression_ref(*E);
}

//----------------------------------------------------------------------//

Core2::Lambda<> load_builtins(const module_loader& loader, Core2::Lambda<> L)
{
    L.body = load_builtins(loader, L.body);
    return L;
}

Core2::Apply<> load_builtins(const module_loader& loader, Core2::Apply<> A)
{
    A.head = load_builtins(loader, A.head);
    return A;
}

Core2::Decls<> load_builtins(const module_loader& loader, Core2::Decls<> decls)
{
    for(auto& [x,E]: decls)
	E = load_builtins(loader, E);
    return decls;
}

Core2::Let<> load_builtins(const module_loader& loader, Core2::Let<> L)
{
    L.decls = load_builtins(loader, L.decls);
    L.body = load_builtins(loader, L.body);
    return L;
}

vector<Core2::Alt<>> load_builtins(const module_loader& loader, vector<Core2::Alt<>> alts)
{
    for(auto& [pat,body]: alts)
	body = load_builtins(loader, body);
    return alts;
}

Core2::Case<> load_builtins(const module_loader& loader, Core2::Case<> C)
{
    C.object = load_builtins(loader, C.object);
    C.alts = load_builtins(loader, C.alts);
    return C;
}

// How can we do this?
Core2::BuiltinOp<> load_builtins(const module_loader& loader, Core2::BuiltinOp<> B)
{
    B.op = loader.load_builtin_ptr(B.lib_name, B.func_name);

    return B;
}

Core2::Exp<> load_builtins(const module_loader& loader, const Core2::Exp<>& E)
{
    if (E.to_var())
	return E;
    else if (auto l = E.to_lambda())
	return load_builtins(loader, *l);
    else if (auto a = E.to_apply())
	return load_builtins(loader, *a);
    else if (auto l = E.to_let())
	return load_builtins(loader, *l);
    else if (auto c = E.to_case())
	return load_builtins(loader, *c);
    else if (E.to_conApp())
	return E;
    else if (auto b = E.to_builtinOp())
	return load_builtins(loader, *b);
    else if (E.to_constant())
	return E;

    std::abort();
}


//------------------------------------------------------------------------------------------------------

Core2::Var<> to_core_var(const Occ::Var& V)
{
    return Core2::Var<>{V.name, V.index, {}, V.is_exported};
}

Core2::Lambda<> to_core_lambda(const Occ::Lambda& L)
{
    return Core2::Lambda<>{to_core_var(L.x), to_core_exp(L.body)};
}

vector<Core2::Decls<>> decl_groups_to_core(const vector<Occ::Decls>& decl_groups)
{
    vector<Core2::Decls<>> decls2;
    for(auto& decl_group: decl_groups)
        decls2.push_back(to_core(decl_group));

    return decls2;
}

Core2::Decls<> to_core(const Occ::Decls& decls)
{
    Core2::Decls<> decls2;
    for(auto& [x,E]: decls)
	decls2.push_back(Core2::Decl<>{to_core_var(x), to_core_exp(E)});

    return decls2;
}

Core2::Apply<> to_core_apply(const Occ::Apply A)
{
    return Core2::Apply<>{to_core_exp(A.head), to_core_exp(A.arg)};
}

Core2::Let<> to_core_let(const Occ::Let& L)
{
    return {to_core(L.decls), to_core_exp(L.body)};
}

Core2::Var<> core_patarg_to_expression_ref(const Occ::Var& V)
{
    return to_core_var(V);
}

Core2::Pattern<> to_core_pattern(const Occ::Pattern& P)
{
    if (P.is_wildcard_pat())
        return {};
    else
    {
	auto args = P.args | ranges::views::transform( core_patarg_to_expression_ref ) | ranges::to<vector>();
	return {P.head, args};
    }
}

vector<Core2::Alt<>> to_core_alts(const vector<Occ::Alt>& alts1)
{
    vector<Core2::Alt<>> alts2;
    for(auto& [pattern,body]: alts1)
	alts2.push_back({to_core_pattern(pattern),to_core_exp(body)});
    return alts2;
}

Core2::Case<> to_core_case(const Occ::Case& C)
{
    return Core2::Case<>{to_core_exp(C.object), to_core_alts(C.alts)};
}

Core2::ConApp<> to_core_con_app(const Occ::ConApp& C)
{
    Core2::ConApp<> con_app;
    con_app.head = C.head;
    for(int i=0;i<C.args.size();i++)
	con_app.args.push_back(to_core_exp(C.args[i]));
    return con_app;
}

Core2::Constant to_core_constant(const Occ::Constant& C)
{
    return C;
}

Core2::BuiltinOp<> to_core_builtin_op(const Occ::BuiltinOp& B)
{
    Core2::BuiltinOp<> builtin;
    builtin.lib_name = B.lib_name;
    builtin.func_name = B.func_name;
    for(int i=0;i<B.args.size();i++)
	builtin.args.push_back(to_core_exp(B.args[i]));
    return builtin;
}

Core2::Exp<> to_core_exp(const Occ::Exp& E)
{
    if (auto v = E.to_var())
	return to_core_var(*v);
    else if (auto l = E.to_lambda())
	return to_core_lambda(*l);
    else if (auto a = E.to_apply())
	return to_core_apply(*a);
    else if (auto l = E.to_let())
	return to_core_let(*l);
    else if (auto c = E.to_case())
	return to_core_case(*c);
    else if (auto c = E.to_conApp())
	return to_core_con_app(*c);
    else if (auto b = E.to_builtinOp())
	return to_core_builtin_op(*b);
    else if (auto c = E.to_constant())
	return to_core_constant(*c);
    else
        std::abort();
}

