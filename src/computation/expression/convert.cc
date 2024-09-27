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

Core2::Var<> to_core(const var& V)
{
    return Core2::Var<>{V.name, V.index};
}

Core2::Lambda<> to_core_lambda(expression_ref L)
{
    assert(is_lambda_exp(L));

    std::vector<Core2::Var<>> vars;
    while(is_lambda_exp(L))
    {
	vars.push_back(to_core(L.sub()[0].as_<var>()));
	L = L.sub()[1];
    }
    assert(not vars.empty());
    return Core2::Lambda<>{std::move(vars), to_core_exp(L)};
}

Core2::Decls<> to_core(const CDecls& decls)
{
    Core2::Decls<> decls2;
    for(auto& [x,E]: decls)
	decls2.push_back(Core2::Decl<>{to_core(x), to_core_exp(E)});

    return decls2;
}

Core2::Apply<> to_core_apply(expression_ref A)
{
    assert(is_apply_exp(A));
    Core2::Apply<> A2;
    A2.head = to_core_exp(A.sub()[0]);
    for(int i=1;i<A.size();i++)
    {
	auto& arg = A.sub()[i];
	if (not arg.is_a<var>())
	    throw myexception()<<"to_core_apply: Argument "<<i<<" of apply is not a variable!";

	A2.args.push_back(to_core(arg.as_<var>()));
    }
    return A2;
}


Core2::Let<> to_core(const let_exp&  L)
{
    return {to_core(L.binds), to_core_exp(L.body)};
}

Core2::Let<> to_core_let(const expression_ref& E)
{
    assert(is_let_expression(E));
    return to_core(E.as_<let_exp>());
}

Core2::Pattern<> to_core_pattern(const expression_ref& P)
{
    if (auto v = P.to<var>())
    {
	if (v->is_wildcard()) return Core2::WildcardPat();
	else return Core2::VarPat<>{to_core(*v)};
    }
    else
    {
	Core2::ConPat<> con_app;
	con_app.head = P.head().as_<constructor>().f_name;

	// The arity on the constructor head must match the number of arguments.
	// We use the constructor arity to infer the number of pattern variables in indexified form.
	assert(P.head().as_<constructor>().n_args() == P.size());

	for(int i=0;i<P.size();i++)
	{
	    if (not is_var(P.sub()[i]))
		throw myexception()<<"to_core_pattern: constructor argument is not a var: "<<P;
	    con_app.args.push_back( to_core(P.sub()[i].as_<var>()) );
	}
	return con_app;
    }
}

Core2::Alts<> to_core(const Core::Alts& alts1)
{
    Core2::Alts<> alts2;
    for(auto& [pattern,body]: alts1)
	alts2.push_back({to_core_pattern(pattern),to_core_exp(body)});
    return alts2;
}

Core2::Alts<> to_core_alts(const expression_ref& E)
{
    assert(E.is_a<Core::Alts>());
    return to_core(E.as_<Core::Alts>());
}

Core2::Case<> to_core_case(const expression_ref& E)
{
    assert(is_case(E));
    return {to_core_exp(E.sub()[0]),to_core_alts(E.sub()[1])};
}

Core2::ConApp<> to_core_con_app(const expression_ref& E)
{
    assert(E.head().is_a<constructor>());

    Core2::ConApp<> con_app;
    con_app.head = E.head().as_<constructor>().f_name;
    for(int i=0;i<E.size();i++)
    {
	auto& arg = E.sub()[i];
	if (not arg.is_a<var>())
	    throw myexception()<<"to_core_con_app: Argument "<<i+1<<" is not a variable!";

	con_app.args.push_back(to_core(arg.as_<var>()));
    }
    return con_app;
}

Core2::Constant to_core_constant(const expression_ref& E)
{
    assert(E.is_atomic());

    if (E.is_char()) return {E.as_char()};
    else if (E.is_int()) return {E.as_int()};
    else if (E.is_double()) throw myexception()<<"to_core_constant: found double "<<E;
    else if (E.is_log_double()) throw myexception()<<"to_core_constant: found log-double "<<E;
    else if (E.is_index_var()) throw myexception()<<"to_core_constant: found index_var "<<E;
    else if (E.is_object_type())
    {
	if (auto s = E.to<Box<std::string>>()) 	return {*s};

	if (auto i = E.to<Integer>()) return { integer_container(*i) };

	throw myexception()<<"to_core_constant: found object "<<E;
    }
    else
	std::abort();
}

Core2::BuiltinOp<> to_core_builtin_op(const expression_ref& E)
{
    assert(E.head().is_a<Operation>());
    auto name = E.head().as_<Operation>().name();
    int delim = name.find(':');
    assert(delim != std::string::npos);
    string libname = name.substr(0,delim);
    string funcname = name.substr(delim+1);
    assert(not libname.empty());
    assert(not funcname.empty());

    Core2::BuiltinOp<> builtin_op{libname, funcname, {}, (void*)E.head().as_<Operation>().op};
    for(int i=0;i<E.size();i++)
    {
	auto& arg = E.sub()[i];
	if (not arg.is_a<var>())
	    throw myexception()<<"to_core_builtin_op: Argument "<<i+1<<" is not a variable!";

	builtin_op.args.push_back(to_core(arg.as_<var>()));
    }
    return builtin_op;
}

Core2::Exp<> to_core_exp(const expression_ref& E)
{
    if (auto v = E.to<var>())
	return to_core(*v);
    else if (is_lambda_exp(E))
	return to_core_lambda(E);
    else if (is_apply_exp(E))
	return to_core_apply(E);
    else if (is_let_expression(E))
	return to_core_let(E);
    else if (is_case(E))
	return to_core_case(E);
    else if (E.head().is_a<constructor>())
	return to_core_con_app(E);
    else if (E.head().is_a<Operation>())
	return to_core_builtin_op(E);
    else if (E.is_atomic())
	return to_core_constant(E);

    std::abort();
}

//----------------------------------------------------------------------//

var to_var(const Core2::Var<>& V)
{
    return var(V.name, V.index);
}

expression_ref var_to_expression_ref(const Core2::Var<>& V)
{
    return to_var(V);
}

expression_ref to_expression_ref(const Core2::Lambda<>& L)
{
    vector<var> vars = L.vars | ranges::views::transform( to_var ) | ranges::to<vector>();

    auto body = to_expression_ref(L.body);

    return lambda_quantify(vars, body);
}

expression_ref to_expression_ref(const Core2::Apply<>& A)
{
    vector<expression_ref> args = A.args | ranges::views::transform( var_to_expression_ref ) | ranges::to<vector>();
    return apply_expression(to_expression_ref(A.head), args);
}

CDecls to_expression_ref(const Core2::Decls<>& decls)
{
    CDecls decls2;
    for(auto& [x,E]: decls)
	decls2.push_back({to_var(x),to_expression_ref(E)});
    return decls2;
}

expression_ref to_expression_ref(const Core2::Let<>& L)
{
    auto decls = to_expression_ref(L.decls);
    auto body = to_expression_ref(L.body);
    return let_expression(decls, body);
}

expression_ref to_expression_ref(const Core2::Pattern<>& P)
{
    if (to<Core2::WildcardPat>(P))
	return var(-1);
    else if (auto v = to<Core2::VarPat<>>(P))
	return to_var(v->var);
    else if (auto c = to<Core2::ConPat<>>(P))
    {
	auto args = c->args | ranges::views::transform( var_to_expression_ref ) | ranges::to<vector>();
	return expression_ref(constructor(c->head, args.size()), args);
    }
    else
	std::abort();
}

Core::Alts to_expression_ref(const Core2::Alts<>& A)
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
    vector<expression_ref> vars;
    for(auto& v: C.args)
	vars.push_back( to_var(v) );
    return expression_ref(constructor(C.head,C.args.size()),vars);
}

// How can we do this?
expression_ref to_expression_ref(const Core2::BuiltinOp<>& B)
{
    Operation O( (operation_fn)B.op, B.lib_name+":"+B.func_name);

    vector<expression_ref> vars;
    for(auto& v: B.args)
	vars.push_back( to_var(v) );
    return expression_ref(O, vars);
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

expression_ref to_expression_ref(const std::optional<Core2::Exp<>>& E)
{
    if (not E)
	return {};
    else
	return to_expression_ref(*E);
}

//------------------------------------------------------------------------------------------------------------

Occ::Var to_occ(const var& V)
{
    Occ::Var OV;
    OV.name = V.name;
    OV.index = V.index;
    OV.info = V;

    return OV;
}

Occ::Lambda to_occ_lambda(expression_ref L)
{
    assert(is_lambda_exp(L));

    std::vector<Occ::Var> vars;
    while(is_lambda_exp(L))
    {
	vars.push_back(to_occ(L.sub()[0].as_<var>()));
	L = L.sub()[1];
    }
    assert(not vars.empty());
    return Occ::Lambda{std::move(vars), to_occ_exp(L)};
}

Occ::Decls to_occ(const CDecls& decls)
{
    Occ::Decls decls2;
    for(auto& [x,E]: decls)
    {
	decls2.push_back(Occ::Decl{to_occ(x), to_occ_exp(E)});
    }

    return decls2;
}

Occ::Apply to_occ_apply(expression_ref A)
{
    assert(is_apply_exp(A));
    Occ::Apply A2;
    A2.head = to_occ_exp(A.sub()[0]);
    for(int i=1;i<A.size();i++)
    {
	auto& arg = A.sub()[i];
	if (not arg.is_a<var>())
	    throw myexception()<<"to_occ_apply: Argument "<<i<<" of apply is not a variable!";

	A2.args.push_back(to_occ(arg.as_<var>()));
    }
    return A2;
}


Occ::Let to_occ(const let_exp&  L)
{
    return {to_occ(L.binds), to_occ_exp(L.body)};
}

Occ::Let to_occ_let(const expression_ref& E)
{
    assert(is_let_expression(E));
    return to_occ(E.as_<let_exp>());
}

Occ::Pattern to_occ_pattern(const expression_ref& P)
{
    if (auto v = P.to<var>())
    {
	if (v->is_wildcard()) return Occ::WildcardPat();
	else return Occ::VarPat{to_occ(*v)};
    }
    else
    {
	Occ::ConPat con_app;
	con_app.head = P.head().as_<constructor>().f_name;

	// The arity on the constructor head must match the number of arguments.
	// We use the constructor arity to infer the number of pattern variables in indexified form.
	assert(P.head().as_<constructor>().n_args() == P.size());

	for(int i=0;i<P.size();i++)
	{
	    if (not is_var(P.sub()[i]))
		throw myexception()<<"to_occ_pattern: constructor argument is not a var: "<<P;
	    con_app.args.push_back( to_occ(P.sub()[i].as_<var>()) );
	}
	return con_app;
    }
}

Occ::Alts to_occ(const Core::Alts& alts1)
{
    Occ::Alts alts2;
    for(auto& [pattern,body]: alts1)
	alts2.push_back({to_occ_pattern(pattern),to_occ_exp(body)});
    return alts2;
}

Occ::Alts to_occ_alts(const expression_ref& E)
{
    assert(E.is_a<Core::Alts>());
    return to_occ(E.as_<Core::Alts>());
}

Occ::Case to_occ_case(const expression_ref& E)
{
    assert(is_case(E));
    return {to_occ_exp(E.sub()[0]),to_occ_alts(E.sub()[1])};
}

Occ::ConApp to_occ_con_app(const expression_ref& E)
{
    assert(E.head().is_a<constructor>());

    Occ::ConApp con_app;
    con_app.head = E.head().as_<constructor>().f_name;
    for(int i=0;i<E.size();i++)
    {
	auto& arg = E.sub()[i];
	if (not arg.is_a<var>())
	    throw myexception()<<"to_occ_con_app: Argument "<<i+1<<" is not a variable!";

	con_app.args.push_back(to_occ(arg.as_<var>()));
    }
    return con_app;
}

Occ::Constant to_occ_constant(const expression_ref& E)
{
    assert(E.is_atomic());

    if (E.is_char()) return {E.as_char()};
    else if (E.is_int()) return {E.as_int()};
    else if (E.is_double()) throw myexception()<<"to_occ_constant: found double "<<E;
    else if (E.is_log_double()) throw myexception()<<"to_occ_constant: found log-double "<<E;
    else if (E.is_index_var()) throw myexception()<<"to_occ_constant: found index_var "<<E;
    else if (E.is_object_type())
    {
	if (auto s = E.to<Box<std::string>>()) 	return {*s};

	if (auto i = E.to<Integer>()) return { integer_container(*i) };

	throw myexception()<<"to_occ_constant: found object "<<E;
    }
    else
	std::abort();
}

Occ::BuiltinOp to_occ_builtin_op(const expression_ref& E)
{
    assert(E.head().is_a<Operation>());
    auto name = E.head().as_<Operation>().name();
    int delim = name.find(':');
    assert(delim != std::string::npos);
    string libname = name.substr(0,delim);
    string funcname = name.substr(delim+1);
    assert(not libname.empty());
    assert(not funcname.empty());

    Occ::BuiltinOp builtin_op{libname, funcname, {}, (void*)E.head().as_<Operation>().op};
    for(int i=0;i<E.size();i++)
    {
	auto& arg = E.sub()[i];
	if (not arg.is_a<var>())
	    throw myexception()<<"to_occ_builtin_op: Argument "<<i+1<<" is not a variable!";

	builtin_op.args.push_back(to_occ(arg.as_<var>()));
    }
    return builtin_op;
}

Occ::Exp to_occ_exp(const expression_ref& E)
{
    if (auto v = E.to<var>())
	return to_occ(*v);
    else if (is_lambda_exp(E))
	return to_occ_lambda(E);
    else if (is_apply_exp(E))
	return to_occ_apply(E);
    else if (is_let_expression(E))
	return to_occ_let(E);
    else if (is_case(E))
	return to_occ_case(E);
    else if (E.head().is_a<constructor>())
	return to_occ_con_app(E);
    else if (E.head().is_a<Operation>())
	return to_occ_builtin_op(E);
    else if (E.is_atomic())
	return to_occ_constant(E);

    std::abort();
}

//----------------------------------------------------------------------//

var occ_to_var(const Occ::Var& OV)
{
    var V(OV.name, OV.index);
    occurrence_info& O = V;
    O = OV.info;
    return V;
}

expression_ref occ_var_to_expression_ref(const Occ::Var& V)
{
    return occ_to_var(V);
}

expression_ref occ_to_expression_ref(const Occ::Lambda& L)
{
    vector<var> vars = L.vars | ranges::views::transform( occ_to_var ) | ranges::to<vector>();

    auto body = occ_to_expression_ref(L.body);

    return lambda_quantify(vars, body);
}

expression_ref occ_to_expression_ref(const Occ::Apply& A)
{
    vector<expression_ref> args = A.args | ranges::views::transform( occ_var_to_expression_ref ) | ranges::to<vector>();
    return apply_expression(occ_to_expression_ref(A.head), args);
}

CDecls occ_to_expression_ref(const Occ::Decls& decls)
{
    CDecls decls2;
    for(auto& [x,E]: decls)
	decls2.push_back({occ_to_var(x), occ_to_expression_ref(E)});
    return decls2;
}

expression_ref occ_to_expression_ref(const Occ::Let& L)
{
    auto decls = occ_to_expression_ref(L.decls);
    auto body = occ_to_expression_ref(L.body);
    return let_expression(decls, body);
}

expression_ref occ_to_expression_ref(const Occ::Pattern& P)
{
    if (to<Occ::WildcardPat>(P))
	return var(-1);
    else if (auto v = to<Occ::VarPat>(P))
	return occ_to_var(v->var);
    else if (auto c = to<Occ::ConPat>(P))
    {
	auto args = c->args | ranges::views::transform( occ_var_to_expression_ref ) | ranges::to<vector>();
	return expression_ref(constructor(c->head, args.size()), args);
    }
    else
	std::abort();
}

Core::Alts occ_to_expression_ref(const Occ::Alts& A)
{
    Core::Alts alts;
    for(auto [pat,body]: A)
	alts.push_back({occ_to_expression_ref(pat), occ_to_expression_ref(body)});
    return alts;
}

expression_ref occ_to_expression_ref(const Occ::Case& C)
{
    auto object = occ_to_expression_ref(C.object);
    auto alts = occ_to_expression_ref(C.alts);
    return make_case_expression(object, alts);
}

expression_ref occ_to_expression_ref(const Occ::ConApp& C)
{
    vector<expression_ref> vars;
    for(auto& v: C.args)
	vars.push_back( occ_to_var(v) );
    return expression_ref(constructor(C.head,C.args.size()),vars);
}

// How can we do this?
expression_ref occ_to_expression_ref(const Occ::BuiltinOp& B)
{
    Operation O( (operation_fn)B.op, B.lib_name+":"+B.func_name);

    vector<expression_ref> vars;
    for(auto& v: B.args)
	vars.push_back( occ_to_var(v) );
    return expression_ref(O, vars);
}

expression_ref occ_to_expression_ref(const Occ::Constant& C)
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

expression_ref occ_to_expression_ref(const Occ::Exp& E)
{
    if (auto v = E.to_var())
	return occ_to_var(*v);
    else if (auto l = E.to_lambda())
	return occ_to_expression_ref(*l);
    else if (auto a = E.to_apply())
	return occ_to_expression_ref(*a);
    else if (auto l = E.to_let())
	return occ_to_expression_ref(*l);
    else if (auto c = E.to_case())
	return occ_to_expression_ref(*c);
    else if (auto c = E.to_conApp())
	return occ_to_expression_ref(*c);
    else if (auto b = E.to_builtinOp())
	return occ_to_expression_ref(*b);
    else if (auto c = E.to_constant())
	return occ_to_expression_ref(*c);

    std::abort();
}

expression_ref occ_to_expression_ref(const std::optional<Occ::Exp>& E)
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

Core2::Alts<> load_builtins(const module_loader& loader, Core2::Alts<> alts)
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

