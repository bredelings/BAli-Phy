#include <iostream>
#include "graph_register.H"
#include "operations.H"
#include "let-float.H"

using std::string;
using std::vector;
using std::pair;

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

	if (V->sub[1].ptr() == E.sub()[1].ptr())
	    return E;
	else
	    return V;
    }

    // 6. Case
    if (E.head().is_a<Case>())
    {
	object_ptr<expression> V = E.as_expression().clone();

	// Normalize the object
	V->sub[0] = graph_normalize(V->sub[0]);

	const int L = (V->sub.size()-1)/2;
	// Just normalize the bodies
	for(int i=0;i<L;i++)
	    V->sub[2+2*i] = graph_normalize(V->sub[2+2*i]);
    
	if (is_reglike(V->sub[0]))
	    return object_ptr<const expression>(V);
	else
	{
	    int var_index = get_safe_binder_index(E);
	    expression_ref x = dummy(var_index);
	    expression_ref obj = V->sub[0];
	    V->sub[0] = x;

	    return let_expression(x,obj,V);
	}
    }

    // 4. Constructor
    if (E.head().is_a<constructor>() or E.head().is_a<Operation>())
    {
	int var_index = get_safe_binder_index(E);

	object_ptr<expression> E2 = E.as_expression().clone();

	// Actually we probably just need x[i] not to be free in E.sub()[i]
	vector<expression_ref> vars;
	vector<expression_ref> bodies;
	for(int i=0;i<E2->size();i++)
	{
	    E2->sub[i] = graph_normalize(E.sub()[i]);

	    if (not is_reglike(E2->sub[i]))
	    {
		expression_ref var = dummy( var_index++ );

		// 1. Let-bind the argument expression
		vars.push_back( var );
		bodies.push_back( E2->sub[i] );

		// 2. Replace the argument expression with the let var.
		E2->sub[i] = var;
	    }
	}

	return let_expression(vars, bodies, object_ptr<const expression>(E2));
    }

    // 5. Let 
    if (E.head().is_a<let_obj>())
    {
	object_ptr<expression> V = E.as_expression().clone();

	// Normalize the object
	V->sub[0] = graph_normalize(V->sub[0]);

	const int L = (V->sub.size()-1)/2;

	// Just normalize the bodies, not the vars
	for(int i=0;i<L;i++)
	    V->sub[2 + 2*i] = graph_normalize(V->sub[2 + 2*i]);

	return V;
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
    C.exp = do_optimize_DCE(C.exp);
    return C;
}

closure graph_normalize(closure&& C)
{
    C.exp = graph_normalize(expression_ref(C.exp));
    return C;
}

closure indexify(closure&& C)
{
    C.exp = indexify(expression_ref(C.exp));
    return C;
}

closure trim_normalize(closure&& C)
{
    C.exp = trim_normalize(expression_ref(C.exp));
    return C;
}

closure resolve_refs(const vector<Module>& P, closure&& C)
{
    C.exp = resolve_refs(P, C.exp);
    return C;
}

closure reg_heap::preprocess(const closure& C)
{
    assert(C.exp);
    assert(let_float(C.exp).print() == let_float(let_float(C.exp)).print());
    //  return trim_normalize( indexify( Fun_normalize( graph_normalize( let_float( translate_refs( closure(C) ) ) ) ) ) );
    return trim_normalize( indexify( graph_normalize( let_float( translate_refs( resolve_refs(*P, closure(C) ) ) ) ) ) );
}

expression_ref reg_heap::translate_refs(const expression_ref& E, closure::Env_t& Env)
{
    int reg = -1;

    // Replace parameters with the appropriate reg_var: of value parameter( )
    if (E.is_a<parameter>())
    {
	string name = E.as_<parameter>().parameter_name;
	string qualified_name = name;

	int param_index = find_parameter(qualified_name);
    
	if (param_index == -1)
	    throw myexception()<<"Can't translate undefined parameter '"<<qualified_name<<"' ('"<<name<<"') in expression!";

	reg = parameters[param_index].second;
    }

    // Replace parameters with the appropriate reg_var: of value whatever
    if (E.is_a<identifier>())
    {
	string name = E.as_<identifier>().name;
	string qualified_name = name;
	assert(is_qualified_symbol(qualified_name) or is_haskell_builtin_con_name(qualified_name));
	auto loc = identifiers.find( qualified_name );
	if (loc == identifiers.end())
	{
	    if (is_haskell_builtin_con_name(name))
	    {
		symbol_info S = Module::lookup_builtin_symbol(name);
		add_identifier(S.name);
      
		// get the root for each identifier
		loc = identifiers.find(S.name);
		assert(loc != identifiers.end());
	
		int R = loc->second;
	
		assert(R != -1);
		set_C(R, preprocess(S.body) );
	    }
	    else
		throw myexception()<<"Can't translate undefined identifier '"<<name<<"' in expression!";
	}

	reg = loc->second;
    }

    // Replace parameters with the appropriate reg_var: of value whatever
    if (E.is_a<reg_var>())
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

