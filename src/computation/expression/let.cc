#include "var.H"
#include "reg_var.H"
#include "lambda.H"
#include "let.H"
#include "case.H"
#include "apply.H"
#include "substitute.H"
#include "modifiable.H"
#include "AST_node.H"
#include "constructor.H"
#include "computation/operations.H"
#include "util/range.H" // for reverse( )
#include "util/string/join.H" // for join( )

using std::vector;
using std::set;
using std::string;
using std::pair;

bool let_exp::operator==(const Object& o) const 
{
    if (this == &o) return true;

    if (typeid(*this) != typeid(o)) return false;

    auto& lo = static_cast<const let_exp&>(o);
    if (binds.size() != lo.binds.size()) return false;

    for(int i=0; i < binds.size();i++)
        if (binds[i] != lo.binds[i]) return false;

    return body == lo.body;
}

string let_exp::print() const 
{
    vector<string> bind_strings;
    for(auto& [x,e]: binds)
        bind_strings.push_back(x.print() + " = " + e.print());
    return "let {" + join(bind_strings,"; ") + "} in " + body.print();
}

Let indexed_let_expression(const vector<expression_ref>& bodies, const expression_ref& T)
{
    return Let(bodies, T);
}

expression_ref let_expression(const CDecls& decls, const expression_ref& T)
{
    if (decls.size() == 0) return T;

    return let_exp(decls, T);
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

    auto& L = E.as_<let_exp>();
    decls = L.binds;
    body = L.body;

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
    else if (E.is_a<var>() or is_reg_var(E))
        return E;

    // Constant or 0-arg constructor
    else if (is_literal_type(E.type()) or is_constructor(E))
        return E;
  
    // 4. Constructor
    else if (is_constructor_exp(E) or is_apply_exp(E) or E.head().is_a<Operation>())
    {
	expression* V = E.as_expression().clone();
	for(int i=0;i<E.size();i++)
	    V->sub[i] = unlet(E.sub()[i]);
	return V;
    }

    std::cerr<<"I don't recognize expression '"+ E.print() + "'\n";
    std::abort();
}

expression_ref multi_let_body(expression_ref E)
{
    while(is_let_expression(E))
	E = E.as_<let_exp>().body;
    return E;
}

std::vector<CDecls> strip_multi_let(expression_ref& E)
{
    std::vector<CDecls> decl_groups;
    while(is_let_expression(E))
    {
        auto& L = E.as_<let_exp>();
	decl_groups.push_back(L.binds);
	E = L.body;
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

void check_duplicate_var(const CDecls& decls)
{
    auto var = find_first_duplicate_var(decls);
    if (var)
	throw myexception()<<"variable '"<<var->print()<<"' occurs twice!";
}
