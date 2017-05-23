#include "dummy.H"
#include "lambda.H"
#include "let.H"
#include "case.H"
#include "substitute.H"
#include "expression.H"
#include "AST_node.H"
#include "computation/operations.H"

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
    expression* E = new expression( Let() );

    E->sub.push_back(T);

    for(const auto& body: bodies)
	E->sub.push_back(body);

    return E;
}


expression_ref make_decls(const vector<pair<dummy, expression_ref>>& decls)
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


expression_ref make_topdecls(const vector<pair<dummy, expression_ref>>& decls)
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


expression_ref let_expression(const vector<pair<dummy, expression_ref>>& decls, const expression_ref& T)
{
    if (decls.size() == 0) return T;

    auto Decls = make_decls(decls);

    object_ptr<expression> E = new expression(let_obj());
    E->sub.push_back(Decls);
    E->sub.push_back(T);

    return E;
}

bool is_let_expression(const expression_ref& E)
{
    return (E.head().type() == let_type);
}

//let [(x[i], bodies[i])] T
bool parse_let_expression(const expression_ref& E, vector<pair<dummy,expression_ref>>& decls, expression_ref& body)
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

    T = E.sub()[0];
    const int L = E.size()-1;
    bodies.resize(L);
    for(int i=0;i<L;i++)
	bodies[i] = E.sub()[i+1];

    return true;
}

int n_free_occurrences(const expression_ref& E1, const expression_ref& D)
{
    assert(not is_wildcard(D));

    // If this is the relevant dummy, then substitute
    if (E1.size() == 0)
    {
	if (E1 == D)
	    return 1;
	// If this is any other constant, then it doesn't contain the dummy
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
		std::set<dummy> bound = get_free_indices(patterns[i]);

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
    std::set<dummy> bound = get_bound_indices(E1);

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
    // 1. Var
    // 5. (partial) Literal constant.  Treat as 0-arg constructor.
    if (not E.size() or is_modifiable(E))
	return E;
  
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

    // 4. Constructor
    if (E.head().is_a<constructor>() or E.head().is_a<Operation>())
    {
	expression* V = E.as_expression().clone();
	for(int i=0;i<E.size();i++)
	    V->sub[i] = unlet(E.sub()[i]);
	return V;
    }

    // 5. Let 
    vector<pair<dummy,expression_ref>> decls;
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

    std::cerr<<"I don't recognize expression '"+ E.print() + "'\n";
    return E;
}

void get_decls(const expression_ref& E, vector<pair<dummy, expression_ref>>& decls)
{
    if (not E) return;

    assert(is_AST(E,"Decls") or is_AST(E,"TopDecls"));
    auto& Decls = E.sub();
    for(int i=0;i<Decls.size();i++)
    {
	assert(is_AST(Decls[i],"Decl"));
	auto& Decl = Decls[i].sub();
	decls.push_back({Decl[0].as_<dummy>(), Decl[1]});
    }
}

void get_decls_from_let(const expression_ref& E, vector<pair<dummy, expression_ref>>& decls)
{
    assert(is_let_expression(E));
    get_decls(E.sub()[0], decls);
}

vector<pair<dummy, expression_ref>> parse_decls(const expression_ref& E)
{
    vector<pair<dummy, expression_ref>> decls;
    get_decls(E, decls);
    return decls;
}

vector<pair<dummy, expression_ref>> let_decls(const expression_ref& E)
{
    vector<pair<dummy, expression_ref>> decls;
    get_decls_from_let(E, decls);
    return decls;
}

expression_ref let_body(expression_ref  E)
{
    return E.sub()[1];
}

void strip_let(expression_ref& E, vector<pair<dummy, expression_ref>>& decls)
{
    if (is_let_expression(E))
    {
	get_decls_from_let(E, decls);
	E = let_body(E);
    }
}

vector<pair<dummy, expression_ref>> strip_let(expression_ref& E)
{
    vector<pair<dummy, expression_ref>> decls;
    strip_let(E,decls);
    return decls;
}

boost::optional<dummy> find_first_duplicate_var(const vector<pair<dummy,expression_ref>>& decls)
{
    set<dummy> vars;
    for(auto& decl: decls)
    {
	const auto& x = decl.first;
	if (vars.count(x))
	    return x;
	vars.insert(x);
    }
    return boost::none;
}

boost::optional<dummy> find_first_duplicate_var(const expression_ref& decls)
{
    return find_first_duplicate_var(parse_decls(decls));
}

void check_duplicate_var(const vector<pair<dummy,expression_ref>>& decls)
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
