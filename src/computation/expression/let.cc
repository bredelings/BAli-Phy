#include "var.H"
#include "lambda.H"
#include "let.H"
#include "case.H"
#include "substitute.H"
#include "modifiable.H"
#include "AST_node.H"
#include "constructor.H"
#include "computation/operations.H"
#include "util/range.H" // for reverse( )

using std::vector;
using std::set;
using std::string;
using std::pair;

bool let_obj::operator==(const Object& o) const 
{
    return dynamic_cast<const let_obj*>(&o);
}

string let_obj::print() const 
{
    return "let";
}

expression_ref indexed_let_expression(const vector<expression_ref>& bodies, const expression_ref& T)
{
    return Let(bodies, T);
}


expression_ref make_decls(const CDecls& decls)
{
    if (decls.empty()) return {};

    object_ptr<expression> Decls = new expression(AST_node("Decls"));
    for(auto& decl: decls)
    {
	object_ptr<expression> Decl = new expression(AST_node("Decl"));
	Decl->sub.push_back(decl.first);
	assert(decl.second);
	Decl->sub.push_back(decl.second);
	Decls->sub.push_back(Decl);
    }
    return Decls;
}


expression_ref make_topdecls(const CDecls& decls)
{
    if (decls.empty()) return {};

    object_ptr<expression> Decls = new expression(AST_node("TopDecls"));
    for(auto& decl: decls)
    {
	object_ptr<expression> Decl = new expression(AST_node("Decl"));
	Decl->sub.push_back(decl.first);
	assert(decl.second);
	Decl->sub.push_back(decl.second);
	Decls->sub.push_back(Decl);
    }
    return Decls;
}


expression_ref let_expression(const CDecls& decls, const expression_ref& T)
{
    if (decls.size() == 0) return T;

    auto Decls = make_decls(decls);

    object_ptr<expression> E = new expression(let_obj());
    E->sub.push_back(Decls);
    E->sub.push_back(T);

    return E;
}

expression_ref let_expression(const vector<CDecls>& decl_groups, const expression_ref& T)
{
    expression_ref body = T;
    for(auto& decls: reverse(decl_groups))
	body = let_expression(decls,body);
    return body;
}

bool is_let_expression(const expression_ref& E)
{
    return (E.head().type() == let_type);
}

//let [(x[i], bodies[i])] T
bool parse_let_expression(const expression_ref& E, CDecls& decls, expression_ref& body)
{
    decls.clear();
    body = {};

    if (not is_let_expression(E)) return false;

    decls = let_decls(E);
    body = let_body(E);

    return true;
}

//let T bodies[i]
bool parse_indexed_let_expression(const expression_ref& E, vector<expression_ref>& bodies, expression_ref& T)
{
    bodies.clear();

    if (E.head().type() != let2_type) return false;

    auto& L = E.as_<Let>();
    T = L.body;
    bodies = L.binds;

    return true;
}

int n_free_occurrences(const expression_ref& E1, const expression_ref& D)
{
    assert(not is_wildcard(D));

    // If this is the relevant var, then substitute
    if (E1.size() == 0)
    {
	if (E1 == D)
	    return 1;
	// If this is any other constant, then it doesn't contain the var
	else
	    return 0;
    }

    // Handle case expressions differently
    {
	expression_ref T;
	vector<expression_ref> patterns;
	vector<expression_ref> bodies;
	if (parse_case_expression(E1,T,patterns,bodies))
	{
	    int count = n_free_occurrences(T, D);

	    const int L = (E1.size()-1)/2;

	    for(int i=0;i<L;i++)
	    {
		// don't substitute into subtree where this variable is bound
		std::set<var> bound = get_free_indices(patterns[i]);

		bool D_is_bound = false;
		for(const auto& b: bound)
		    if (D == b) D_is_bound=true;

		if (not D_is_bound)
		    count += n_free_occurrences(bodies[i], D);
	    }

	    return count;
	}
    }

    // What indices are bound at the top level?
    std::set<var> bound = get_bound_indices(E1);

    // Don't substitute into local variables
    for(const auto& b: bound)
	if (D == b) return 0;
    
    // Since this is an expression, count occurrences in sub-expressions
    int count = 0;
    for(int i=0;i<E1.size();i++)
	count += n_free_occurrences(E1.sub()[i], D);

    return count;
}

expression_ref unlet(const expression_ref& E)
{
    // 2. Lambda
    if (E.head().is_a<lambda>())
    {
	assert(E.size() == 2);
	expression* V = E.as_expression().clone();
	V->sub[1] = unlet(E.sub()[1]);

	if (V->sub[1].ptr() == E.sub()[1].ptr())
	    return E;
	else
	    return V;
    }

    // 6. Case
    expression_ref object;
    vector<expression_ref> patterns;
    vector<expression_ref> bodies;
    if (parse_case_expression(E, object, patterns, bodies))
    {
	// Unormalize the object
	object = unlet(object);

	const int L = patterns.size();
	// Just unnormalize the bodies
	for(int i=0;i<L;i++)
	    bodies[i] = unlet(bodies[i]);
    
	return make_case_expression(object, patterns, bodies);
    }

    // 5. Let 
    CDecls decls;
    expression_ref T;
    if (parse_let_expression(E, decls, T))
    {
	// unnormalize T and the bodies
	T = unlet(T);
	for(int i=0; i<decls.size(); i++)
	    decls[i].second = unlet(decls[i].second);

	// substitute for non-recursive lets
	bool changed = true;
	while(changed)
	{
	    changed = false;

	    for(int i=decls.size()-1; i>=0; i--)
	    {
		if (n_free_occurrences(decls[i].second, decls[i].first)) continue;

		int count = n_free_occurrences(T, decls[i].first);
		for(const auto& decl: decls)
		    count += n_free_occurrences(decl.second, decl.first);

		if (count != 1) continue;

		changed = true;
	
		auto decl = decls[i];
	
		decls.erase(decls.begin() + i);
	
		// substitute for the value of this variable in T and in the remaining bodies;
		for(int j=0;j<decls.size();j++)
		    decls[j].second = substitute(decls[j].second, decl.first, decl.second);
		T = substitute(T, decl.first, decl.second);
	    }
	}

	return let_expression(decls, T);
    }

    // 1. Var
    // 5. (partial) Literal constant.  Treat as 0-arg constructor.
    if (not E.size() or is_modifiable(E))
	return E;
  
    // 4. Constructor
    if (E.head().is_a<constructor>() or E.head().is_a<Operation>())
    {
	expression* V = E.as_expression().clone();
	for(int i=0;i<E.size();i++)
	    V->sub[i] = unlet(E.sub()[i]);
	return V;
    }

    std::cerr<<"I don't recognize expression '"+ E.print() + "'\n";
    return E;
}

void get_decls(const expression_ref& E, CDecls& decls)
{
    if (not E) return;

    assert(is_AST(E,"Decls") or is_AST(E,"TopDecls"));
    auto& Decls = E.sub();
    for(int i=0;i<Decls.size();i++)
    {
	assert(is_AST(Decls[i],"Decl"));
	auto& Decl = Decls[i].sub();
	decls.push_back({Decl[0].as_<var>(), Decl[1]});
    }
}

void let_decls(const expression_ref& E, CDecls& decls)
{
    assert(is_let_expression(E));
    get_decls(E.sub()[0], decls);
}

CDecls parse_decls(const expression_ref& E)
{
    CDecls decls;
    get_decls(E, decls);
    return decls;
}

CDecls let_decls(const expression_ref& E)
{
    CDecls decls;
    let_decls(E, decls);
    return decls;
}

expression_ref let_body(const expression_ref& E)
{
    assert(is_let_expression(E));
    return E.sub()[1];
}

expression_ref multi_let_body(expression_ref E)
{
    while(is_let_expression(E))
	E = let_body(E);
    return E;
}

std::vector<CDecls> strip_multi_let(expression_ref& E)
{
    std::vector<CDecls> decl_groups;
    while(is_let_expression(E))
    {
	decl_groups.push_back(let_decls(E));
	E = let_body(E);
    }
    return decl_groups;
}

std::optional<var> find_first_duplicate_var(const CDecls& decls)
{
    set<var> vars;
    for(auto& decl: decls)
    {
	const auto& x = decl.first;
	if (vars.count(x))
	    return x;
	vars.insert(x);
    }
    return {};
}

std::optional<var> find_first_duplicate_var(const expression_ref& decls)
{
    return find_first_duplicate_var(parse_decls(decls));
}

void check_duplicate_var(const CDecls& decls)
{
    auto var = find_first_duplicate_var(decls);
    if (var)
	throw myexception()<<"variable '"<<var->print()<<"' occurs twice!";
}

void check_duplicate_var(const expression_ref& decls)
{
    auto var = find_first_duplicate_var(decls);
    if (var)
	throw myexception()<<"variable '"<<var->print()<<"' occurs twice!";
}
