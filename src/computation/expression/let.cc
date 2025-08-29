#include "var.H"
#include "reg_var.H"
#include "lambda.H"
#include "let.H"
#include "case.H"
#include "apply.H"
#include "substitute.H"
#include "modifiable.H"
#include "constructor.H"
#include "computation/operations.H"
#include "util/range.H" // for reverse( )
#include "util/string/join.H" // for join( )

using std::vector;
using std::set;
using std::string;
using std::pair;

string print_cdecls(const CDecls& cdecls)
{
    vector<string> ds;
    for(auto& [x,E]: cdecls)
        ds.push_back(x.print() + " = " + E.print());

    return "{" + join(ds, "; ") +  "}";
}


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
    return "let " + print_cdecls(binds) + " in " + body.print();
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
    return (E.head().type() == type_constant::let_type);
}

std::optional<Let>
parse_indexed_let_expression(const expression_ref& E)
{
    if (E.head().type() == type_constant::let2_type)
        return E.as_<Let>();
    else
        return {};
}

int n_free_occurrences(const expression_ref& E1, const var& x)
{
    assert(not is_wildcard(x));

    // If this is the relevant var, then substitute
    if (E1.is_a<var>())
    {
        auto& y = E1.as_<var>();
        if (x == y)
            return 1;
        else
            return 0;
    }

    // Handle case expressions differently
    if (auto C = parse_case_expression(E1))
    {
        auto& [object, alts] = *C;

        int count = n_free_occurrences(object, x);

        for(auto& [pattern, body]: alts)
        {
            // don't substitute into subtree where this variable is bound
            std::set<var> bound = get_free_indices(pattern);

            bool x_is_bound = false;
            for(const auto& b: bound)
                if (x == b) x_is_bound=true;

            if (not x_is_bound)
                count += n_free_occurrences(body, x);
        }

        return count;
    }

    if (is_let_expression(E1))
    {
        auto& L = E1.as_<let_exp>();
        int count = n_free_occurrences(L.body, x);

        for(auto& [y,E]: L.binds)
        {
            if (x == y) return 0;
            count += n_free_occurrences(E, x);
        }

        return count;
    }

    // What indices are bound at the top level?
    std::set<var> bound = get_bound_indices(E1);

    // Don't substitute into local variables
    for(const auto& b: bound)
	if (x == b) return 0;
    
    // Since this is an expression, count occurrences in sub-expressions
    int count = 0;
    for(int i=0;i<E1.size();i++)
	count += n_free_occurrences(E1.sub()[i], x);

    return count;
}

expression_ref unlet(const expression_ref& E)
{
    // 2. Lambda
    if (E.head().is_a<lambda>())
    {
	assert(E.size() == 2);
	auto body = unlet(E.sub()[1]);

	if (body.is_object_type() and E.sub()[1].is_object_type() and body.ptr() == E.sub()[1].ptr())
	    return E;
	else
	{
	    object_ptr<expression> V = E.as_expression().clone();
	    V->sub[1] = body;
	    return V;
	}
    }

    // 6. Case
    if (auto C = parse_case_expression(E))
    {
        auto& [object,alts] = *C;

	// Unormalize the object
	object = unlet(object);

	// Just unnormalize the bodies
	for(auto& [pattern, body]: alts)
	    body = unlet(body);
    
	return make_case_expression(object, alts);
    }

    // 5. Let 
    if (is_let_expression(E))
    {
        auto L = E.as_<let_exp>();
	// unnormalize T and the bodies
	L.body = unlet(L.body);
	for(int i=0; i<L.binds.size(); i++)
	    L.binds[i].second = unlet(L.binds[i].second);

	// substitute for non-recursive lets
	bool changed = true;
	while(changed)
	{
	    changed = false;

	    for(int i=L.binds.size()-1; i>=0; i--)
	    {
                // If the variables binds to a case expression, then don't substitute.
                if (is_case(L.binds[i].second)) continue;

		if (n_free_occurrences(L.binds[i].second, L.binds[i].first)) continue;

		int count = n_free_occurrences(L.body, L.binds[i].first);
		for(const auto& decl: L.binds)
		    count += n_free_occurrences(decl.second, decl.first);

		if (count != 1) continue;

		changed = true;
	
		auto decl = L.binds[i];
	
		L.binds.erase(L.binds.begin() + i);
	
		// substitute for the value of this variable in T and in the remaining bodies;
		for(int j=0;j<L.binds.size();j++)
		    L.binds[j].second = substitute(L.binds[j].second, decl.first, decl.second);
		L.body = substitute(L.body, decl.first, decl.second);
	    }
	}

        if (L.binds.size() == 0)
            return L.body;
        else
            return L;
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
        // This handles (modifiable) with no arguments.
        if (E.is_atomic())
            return E;
        else
        {
            object_ptr<expression> V = E.as_expression().clone();
            for(int i=0;i<E.size();i++)
                V->sub[i] = unlet(E.sub()[i]);
            return V;
        }
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
