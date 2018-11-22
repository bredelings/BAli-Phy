#include <iostream>
#include <unordered_map>
#include "computation/machine/graph_register.H"
#include "operations.H"
#include "computation/expression/expression.H"
#include "computation/expression/let.H"
#include "computation/expression/case.H"
#include "computation/expression/var.H"
#include "computation/expression/lambda.H"
#include "computation/expression/trim.H"
#include "computation/expression/indexify.H"
#include "computation/optimization/let-float.H"

#include <boost/optional.hpp>

using std::string;
using std::vector;
using std::pair;
using std::set;
using std::map;

using std::cerr;
using std::endl;


expression_ref graph_normalize(const expression_ref& E)
{
    if (not E) return E;

    // 1. Var
    // 5. (partial) Literal constant.  Treat as 0-arg constructor.
    if (not E.size()) return E;
  
    // 2. Lambda
    if (E.head().is_a<lambda>())
    {
	assert(E.size() == 2);
	object_ptr<expression> V = E.as_expression().clone();
	V->sub[1] = graph_normalize(E.sub()[1]);

	return V;
    }

    // 6. Case
    expression_ref object;
    vector<expression_ref> patterns;
    vector<expression_ref> bodies;
    if (parse_case_expression(E, object, patterns, bodies))
    {
	// Normalize the object
	object = graph_normalize(object);

	const int L = patterns.size();
	// Just normalize the bodies
	for(int i=0;i<L;i++)
	    bodies[i] = graph_normalize(bodies[i]);
    
	if (is_reglike(object))
	    return make_case_expression(object, patterns, bodies);
	else
	{
	    int var_index = get_safe_binder_index(E);
	    auto x = var(var_index);

	    return let_expression({{x,object}},make_case_expression(x, patterns, bodies));
	}
    }

    // 4. Constructor
    if (E.head().is_a<constructor>() or E.head().is_a<Operation>())
    {
	int var_index = get_safe_binder_index(E);

	object_ptr<expression> E2 = E.as_expression().clone();

	// Actually we probably just need x[i] not to be free in E.sub()[i]
	vector<pair<var, expression_ref>> decls;
	for(int i=0;i<E2->size();i++)
	{
	    E2->sub[i] = graph_normalize(E.sub()[i]);

	    if (not is_reglike(E2->sub[i]))
	    {
		auto x = var( var_index++ );

		// 1. Let-bind the argument expression
		decls.push_back( {x, E2->sub[i]} );

		// 2. Replace the argument expression with the let var.
		E2->sub[i] = x;
	    }
	}

	return let_expression(decls, object_ptr<const expression>(E2));
    }

    // 5. Let 
    if (is_let_expression(E))
    {
	auto decls = let_decls(E);
	auto body  = let_body(E);

	// Normalize the body
	body = graph_normalize(body);

	// Just normalize the bound statements
	for(auto& decl: decls)
	    decl.second = graph_normalize(decl.second);

	return let_expression(decls, body);
    }

    throw myexception()<<"graph_normalize: I don't recognize expression '"+ E.print() + "'";
}


// Deleted: Fun_normalize( ).  Perhaps reintroduce later.
// Translate from language Basic to language Fun by introducing letrec expressions.  (arguments are already vars)
// See "From Natural Semantics to C: A Formal Derivation of two STG machines."
//      by Alberto de la Encina and Ricardo Pena.



closure let_float(closure&& C)
{
    C.exp = let_float(expression_ref(C.exp));
    return std::move(C);
}

closure graph_normalize(closure&& C)
{
    C.exp = graph_normalize(expression_ref(C.exp));
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
    return trim_normalize( indexify( graph_normalize( translate_refs( closure(C) ) ) ) );
}

int reg_heap::reg_for_id(const string& name)
{
    assert(is_qualified_symbol(name) or is_haskell_builtin_con_name(name));
    auto loc = identifiers.find( name );
    if (loc == identifiers.end())
    {
	if (is_haskell_builtin_con_name(name))
	{
	    auto S = Module::lookup_builtin_symbol(name);
	    add_identifier(S.first.name);

	    // get the root for each identifier
	    loc = identifiers.find(S.first.name);
	    assert(loc != identifiers.end());

	    int R = loc->second;

	    assert(R != -1);
	    set_C(R, preprocess(S.second) );
	}
	else
	    throw myexception()<<"Can't translate undefined identifier '"<<name<<"' in expression!";
    }

    return loc->second;
}

expression_ref reg_heap::translate_refs(const expression_ref& E, closure::Env_t& Env)
{
    int reg = -1;

    // Replace parameters with the appropriate reg_var: of value parameter( )
    if (E.is_a<parameter>())
    {
	string name = E.as_<parameter>().parameter_name;

	int param_index = find_parameter(name);
    
	if (param_index == -1)
	    throw myexception()<<"Can't translate undefined parameter '"<<name<<"' in expression!";

	reg = parameters[param_index].second;
    }

    // Replace dummies that are either qualified ids, or builtin constructor names
    else if (is_qualified_var(E))
    {
	auto& name = E.as_<var>().name;
	reg = reg_for_id(name);
    }
    else if (E.is_a<var>() and is_haskell_builtin_con_name(E.as_<var>().name))
    {
	auto& name = E.as_<var>().name;
	reg = reg_for_id(name);
    }

    // Replace parameters with the appropriate reg_var: of value whatever
    else if (E.is_a<reg_var>())
	reg = E.as_<reg_var>().target;

    if (reg != -1)
    {
	int index = Env.size();
	Env.insert(Env.begin(), reg);

	return index_var(index);
    }

    // Other constants have no parts, and don't need to be translated
    if (not E.size()) return E;

    // Translate the parts of the expression
    object_ptr<expression> V = E.as_expression().clone();
    for(int i=0;i<V->size();i++)
	V->sub[i] = translate_refs(V->sub[i], Env);

    return V;
}

closure reg_heap::translate_refs(closure&& C)
{
    closure C2 = C;
    C2.exp = translate_refs(C2.exp, C2.Env);
    return C2;
}

