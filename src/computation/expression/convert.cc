#include "convert.H"
#include "computation/expression/var.H"
#include "computation/haskell/Integer.H"
#include "util/variant.H"

#include <range/v3/all.hpp>

using std::set;
using std::vector;
using std::multiset;
using std::string;

//----------------------------------------------------------------------//

var to_var(const Core::Var<>& V)
{
    return var(V.name, V.index, V.is_exported);
}

expression_ref to_expression_ref(const Core::Constant& C)
{
    if (auto c = to<char>(C.value))
	return *c;
    else if (auto i = to<int>(C.value))
	return *i;
    else if (auto i = to<integer_container>(C.value))
	return Integer(i->i);
    else if (auto d = to<double>(C.value))
	return *d;
    else if (auto s = to<std::string>(C.value))
	return String(*s);
    else
	std::abort();
}

std::optional<Core::Constant> to_core_constant(const expression_ref& E)
{
    if (E.is_char())
        return Core::Constant{{E.as_char()}};
    else if (E.is_int())
        return Core::Constant{{E.as_int()}};
    else if (E.is_double())
        return Core::Constant{{E.as_double()}};
    else if (E.is_a<Integer>())
        return Core::Constant{{integer_container(E.as_<Integer>())}};
    else if (E.is_a<String>())
        return Core::Constant{{E.as_<String>()}};
    else
	return {};
}

//------------------------------------------------------------------------------------------------------------

Core::Var<> to_core_var(const Occ::Var& V)
{
    return Core::Var<>{V.name, V.index, {}, V.is_exported};
}

Core::Lambda<> to_core_lambda(const Occ::Lambda& L)
{
    return Core::Lambda<>{to_core_var(L.x), to_core_exp(L.body)};
}

vector<Core::Decls<>> decl_groups_to_core(const vector<Occ::Decls>& decl_groups)
{
    vector<Core::Decls<>> decls2;
    for(auto& decl_group: decl_groups)
        decls2.push_back(to_core(decl_group));

    return decls2;
}

Core::Decls<> to_core(const Occ::Decls& decls)
{
    Core::Decls<> decls2;
    for(auto& [x,E]: decls)
	decls2.push_back(Core::Decl<>{to_core_var(x), to_core_exp(E)});

    return decls2;
}

Core::Apply<> to_core_apply(const Occ::Apply A)
{
    return Core::Apply<>{to_core_exp(A.head), to_core_exp(A.arg)};
}

Core::Let<> to_core_let(const Occ::Let& L)
{
    return {to_core(L.decls), to_core_exp(L.body)};
}

Core::Var<> core_patarg_to_expression_ref(const Occ::Var& V)
{
    return to_core_var(V);
}

Core::Pattern<> to_core_pattern(const Occ::Pattern& P)
{
    if (P.is_wildcard_pat())
        return {};
    else
    {
	auto args = P.args | ranges::views::transform( core_patarg_to_expression_ref ) | ranges::to<vector>();
	return {P.head, args};
    }
}

vector<Core::Alt<>> to_core_alts(const vector<Occ::Alt>& alts1)
{
    vector<Core::Alt<>> alts2;
    for(auto& [pattern,body]: alts1)
	alts2.push_back({to_core_pattern(pattern),to_core_exp(body)});
    return alts2;
}

Core::Case<> to_core_case(const Occ::Case& C)
{
    return Core::Case<>{to_core_exp(C.object), to_core_alts(C.alts)};
}

Core::ConApp<> to_core_con_app(const Occ::ConApp& C)
{
    Core::ConApp<> con_app;
    con_app.head = C.head;
    for(int i=0;i<C.args.size();i++)
	con_app.args.push_back(to_core_exp(C.args[i]));
    return con_app;
}

Core::Constant to_core_constant(const Occ::Constant& C)
{
    return C;
}

Core::BuiltinOp<> to_core_builtin_op(const Occ::BuiltinOp& B)
{
    vector<Core::Exp<>> args;
    for(int i=0;i<B.args.size();i++)
	args.push_back(to_core_exp(B.args[i]));

    return {B.lib_name, B.func_name, B.call_conv, args, B.op};
}

Core::Exp<> to_core_exp(const Occ::Exp& E)
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
