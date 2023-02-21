#include <iostream>
#include <unordered_map>
#include "computation/machine/graph_register.H"
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
#include "computation/fresh_vars.H"
#include "haskell/ids.H"

using std::optional;
using std::string;
using std::vector;
using std::pair;
using std::set;
using std::map;

using std::cerr;
using std::endl;


// This version is used in module.cc
expression_ref graph_normalize(FreshVarSource& source, const expression_ref& E)
{
    if (not E) return E;

    // 2. Lambda
    if (E.head().is_a<lambda>())
    {
	assert(E.size() == 2);
	object_ptr<expression> V = E.as_expression().clone();
	V->sub[1] = graph_normalize(source, E.sub()[1]);

	return V;
    }

    // 6. Case
    expression_ref object;
    vector<expression_ref> patterns;
    vector<expression_ref> bodies;
    if (parse_case_expression(E, object, patterns, bodies))
    {
	// Normalize the object
	object = graph_normalize(source, object);

	const int L = patterns.size();
	// Just normalize the bodies
	for(int i=0;i<L;i++)
	    bodies[i] = graph_normalize(source, bodies[i]);

	if (is_reglike(object))
	    return make_case_expression(object, patterns, bodies);
	else
	{
	    auto x = source.get_fresh_var();

	    return let_expression({{x,object}},make_case_expression(x, patterns, bodies));
	}
    }

    // 5. Let
    if (is_let_expression(E))
    {
        auto L = E.as_<let_exp>();

	// Normalize the body
	L.body = graph_normalize(source, L.body);

	// Just normalize the bound statements
	for(auto& [x,e]: L.binds)
	    e = graph_normalize(source, e);

	return L;
    }

    // 1. Var
    // 5. (partial) Literal constant.  Treat as 0-arg constructor.
    else if (not E.size()) return E;

    // 4. Constructor
    if (E.head().is_a<constructor>() or E.head().is_a<Operation>())
    {
	object_ptr<expression> E2 = E.as_expression().clone();

	// Actually we probably just need x[i] not to be free in E.sub()[i]
	vector<pair<var, expression_ref>> decls;
	for(int i=0;i<E2->size();i++)
	{
	    E2->sub[i] = graph_normalize(source, E.sub()[i]);

	    if (not is_reglike(E2->sub[i]))
	    {
		auto x = source.get_fresh_var();

		// 1. Let-bind the argument expression
		decls.push_back( {x, E2->sub[i]} );

		// 2. Replace the argument expression with the let var.
		E2->sub[i] = x;
	    }
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
    C.exp = indexify(expression_ref(C.exp));
    return std::move(C);
}

closure trim_normalize(closure&& C)
{
    C.exp = trim_normalize(expression_ref(C.exp));
    return std::move(C);
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
    auto loc = identifiers.find( x );
    if (loc == identifiers.end())
    {
	if (is_haskell_builtin_con_name(name))
	{
            assert(x.index == 0);

	    auto [sym,code] = Module::lookup_builtin_symbol(name);
	    add_identifier(x);

	    // get the root for each identifier
	    loc = identifiers.find(x);
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
    else if (E.is_a<reg_var>())
	reg = E.as_<reg_var>().target;

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

