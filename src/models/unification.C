#include "unification.H"
#include <vector>
#include <set>
#include "util.H"

using std::vector;
using std::set;
using std::string;
using boost::property_tree::ptree;

bool is_variable(const ptree& p)
{
    if (not p.empty()) return false;
    const string& s = p.get_value<string>();
    if (s.empty()) return false;
    char first_letter = s[0];
    return (first_letter >= 97 and first_letter <= 122);
}

bool is_wildcard(const ptree& p)
{
    if (not p.empty()) return false;
    return (p.get_value<string>() == "_");
}

set<string> find_variables(const ptree& p)
{
    set<string> vars;
    if (is_variable(p))
	vars.insert(p.get_value<string>());
    else
	for(const auto& x: p)
	    add(vars,find_variables(x.second));
    return vars;
}

int find_unused_index(const set<string>& vars)
{
    int index = 0;
    for(const auto& var: vars)
    {
	if (var.size() > 3 and var.substr(0,3) == "var")
	{
	    int i;
	    if (can_be_converted_to<int>(var.substr(3),i))
		index = std::max(index,i);
	}
    }
    return index+1;
}

ptree alpha_rename(const set<string>& vars, const set<string>& vars_to_avoid)
{
    int index = std::max(find_unused_index(vars), find_unused_index(vars_to_avoid));

    equations_t equations;
    for(const auto& var: vars)
    {
	if (includes(vars_to_avoid, var))
	{
	    string new_var = string("var") + convertToString(index++);
	    equations.put(var,new_var);
	}
    }
    return equations;
}

bool can_unify(const ptree& p1, const ptree& p2)
{
    return unify(p1,p2).get_value<string>() != "fail";
}

// Given two sets of equations, what further equations do we need to unify them?
bool merge_equations(equations_t& p1, const equations_t& p2)
{
    if (p1.get_value<string>() == "fail") return false;
    if (p2.get_value<string>() == "fail") return false;

    for(const auto& x: p2)
    {
	assert(x.second.get_value<string>() != "");
	const auto& key = x.first;

	// If p1 has no equality for the variable, then just copy over p2's equality
	if (not p1.count(key))
	    p1.push_back(x);
        // If they BOTH have an equality, then we need to merge the equalities.
	else 
	    if (not merge_equations(p1, unify(p1.get_child(key),p2.get_child(key))))
	    {
		p1 = ptree("fail");
		return false;
	    }
    }

    return true;
}

void substitute(const equations_t& equations, ptree& p)
{
    if (is_variable(p))
    {
	for(auto& eq: equations)
	    if (eq.first == p.get_value<string>())
	    {
		p = eq.second;
		substitute(equations, p);
		break;
	    }
    }
    else
	for(auto& child: p)
	    substitute(equations, child.second);
}

equations_t unify(const ptree& p1, const ptree& p2)
{
    // 1. If either term is a variable, then we are good.
    string head1 = p1.get_value<string>();
    string head2 = p2.get_value<string>();
    if (is_wildcard(p1) or is_wildcard(p2))
	return {}; // Don't record equations for matching wildcards
    else if (is_variable(p1))
    {
	// Don't record equalities of the form a = a
	if (is_variable(p2) and head1 == head2)
	    return {};
	else
	{
	    equations_t equations;
	    equations.push_back({head1,p2});
	    return equations;
	}
    }
    else if (is_variable(p2))
    {
	equations_t equations;
	equations.push_back({head2,p1});
	return equations;
    }

    // 2. If the heads don't match then unification fails
    if (head1 != head2) return ptree("fail");

    // 3. If the arity doesn't match then unification fails
    if (p1.size() != p2.size()) return ptree("fail");

    // 4. If every argument unifies, then unification succeeds
    equations_t equations;

    auto x = p1.begin();
    auto y = p2.begin();
    for(int i=0; i<p1.size(); i++, x++, y++)
	if (not merge_equations(equations, unify(x->second, y->second)))
	    return ptree("fail");
    
    return equations;
}

