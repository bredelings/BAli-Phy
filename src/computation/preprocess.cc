#include <iostream>
#include <unordered_map>
#include "range/v3/all.hpp"
#include "computation/machine/graph_register.H"
#include "operations.H"
#include "computation/expression/let.H"
#include "computation/expression/case.H"
#include "computation/expression/var.H"
#include "computation/expression/resolved_symbol.H"
#include "computation/expression/lambda.H"
#include "computation/expression/trim.H"
#include "computation/expression/index_var.H"
#include "computation/expression/indexify.H"
#include "computation/expression/constructor.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/expression.H" // for is_reglike( )



using std::optional;
using std::string;
using std::vector;
using std::pair;
using std::set;
using std::map;

using std::cerr;
using std::endl;

namespace views = ranges::views;


expression_ref opt_normalize(const expression_ref& E)
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
	V->sub[1] = opt_normalize(E.sub()[1]);

	return V;
    }

    // 6. Case
    expression_ref object;
    vector<expression_ref> patterns;
    vector<expression_ref> bodies;
    if (parse_case_expression(E, object, patterns, bodies))
    {
	// Normalize the object
	object = opt_normalize(object);

	const int L = patterns.size();
	// Just normalize the bodies
	for(int i=0;i<L;i++)
	    bodies[i] = opt_normalize(bodies[i]);
    
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
	    E2->sub[i] = opt_normalize(E.sub()[i]);

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
	body = opt_normalize(body);

	// Just normalize the bound statements
	for(auto& decl: decls)
	    decl.second = opt_normalize(decl.second);

	return let_expression(decls, body);
    }

    throw myexception()<<"opt_normalize: I don't recognize expression '"+ E.print() + "'";
}


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
        const int L = patterns.size();

        // 1. Get a var that is not bound in the object AND isn't bound in any of the patterns.
        auto var_index = get_safe_binder_index(E);
	for(int i=0;i<L;i++)
            var_index = std::max(var_index, get_safe_binder_index(patterns[i]));

	// 2. Normalize the object
	object = graph_normalize(object);

        // 3. Make the object a variable
        CDecls decls;
        if (not is_reglike(object))
        {
            auto x = var(var_index++);
            decls.push_back({x,object});
            object = x;
        }

        // 4. Normalize the bodies AND let-bind all vars from the pattern binding.
	for(int i=0;i<L;i++)
        {
            // normalize the bodies
	    bodies[i] = graph_normalize(bodies[i]);

            // let-bind each pattern var to an object field
            if (patterns[i].is_expression())
            {
                CDecls alt_decls;
                object_ptr<expression> pattern2 = patterns[i].as_expression().clone();
                for(int j=0;j<patterns[i].size();j++)
                {
                    auto& v = patterns[i].sub()[j].as_<var>();
                    if (not v.is_wildcard())
                    {
                        expression_ref F= Get()+j+object;
                        alt_decls.push_back({v,F});
                        pattern2->sub[j] = var(-1);
                    }
                }
                patterns[i] = pattern2;
                bodies[i] = let_expression(alt_decls,bodies[i]);
            }

            // The body must be reglike AFTER we add let-binders for pattern vars.
            if (not is_reglike(bodies[i]))
            {
                auto b = var(var_index++);
                decls.push_back( {b, bodies[i] } );
                bodies[i] = b;
            }
        }
    
        // 5. Create the case expression
        auto E2 = make_case_expression(object, patterns, bodies);

        // 6. Create the let-bindings non-recursively, in reverse order
        for(auto& decl: decls | views::reverse)
            E2 = let_expression({decl}, E2);

        return E2;
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
    optional<int> reg;

    if (is_resolved_symbol(E))
    {
        auto& name = E.as_<resolved_symbol>().name;
	reg = reg_for_id(name);
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

    if (reg)
    {
	int index = Env.size();
	Env.insert(Env.begin(), *reg);

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

