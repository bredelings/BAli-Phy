#include "unification.H"
#include <vector>
#include <set>
#include "util.H"

using std::vector;
using std::pair;
using std::map;
using std::set;
using std::string;
using boost::optional;
using boost::property_tree::ptree;


bool is_wildcard(const ptree& p)
{
    if (not p.empty()) return false;
    return (p.get_value<string>() == "_");
}

string show(const ptree& pt, int depth = 0);

/// Split a string of the form key=value into {key,value}
string show(const equations& E)
{
    string result;
    for(auto& e: E.get_values())
    {
	for(auto& var: e.first)
	    result += (var + " = ");
	if (e.second)
	    result += show(*e.second);
	else
	    result += "\n";
    }
    return result;
}

bool equations::has_record(const std::string& x) const
{
    for(auto& v: values)
	if (v.first.count(x)) return true;

    return false;
}

equations::operator bool() const
{
    return valid;
}

void equations::remove_record_for(const std::string& x)
{
    for(auto it = values.begin(); it != values.end(); it++)
    {
	if (it->first.count(x))
	{
	    values.erase(it);
	    return;
	}
    }
    std::abort();
}

const vector<pair<set<string>,optional<term_t>>>& equations::get_values() const
{
    return values;
}

vector<pair<set<string>,optional<term_t>>>::const_iterator equations::find_record(const std::string& x) const
{
    for(auto it = values.begin(); it != values.end(); it++)
	if (it->first.count(x))
	    return it;

    std::abort();
}

vector<pair<set<string>,optional<term_t>>>::iterator equations::find_record(const std::string& x)
{
    for(auto it = values.begin(); it != values.end(); it++)
	if (it->first.count(x))
	    return it;

    std::abort();
}

bool equations::add_condition(const string& x, const term_t& T)
{
    if (is_wildcard(T)) return valid;

    if (not has_record(x))
	// Add x = T
	values.push_back({set<string>{x},T});
    else
    {
	auto xrec = find_record(x);
	if (not xrec->second)
	    // Set x = T
	    xrec->second = T;
	else
	    // If x=U then unify(U,T)
	    unify(*xrec->second, T);
    }
#ifndef NDEBUG
    for(auto& eq: values)
	assert(eq.second or eq.first.size() > 1);
#endif
    return valid;
}

bool equations::add_var_condition(const string& x, const string& y)
{
    // 1. If we are adding x == y then quit.
    if (x == y) return valid;

    if (not has_record(y))
    {
	if (not has_record(x))
	    // 2. If neither x or y has a record, then add one for both of them.
	    values.push_back({set<string>{x,y},{}});
	else
	{
	    // 3. If x has a record by y does not, then add y to x's record;
	    find_record(x)->first.insert(y);
	}
    }
    else if (not has_record(x))
	// 4. If y has a record by x does not, then add x to y's record;
	find_record(y)->first.insert(x);
    else
    {
	auto xrec = find_record(x);
	auto yrec = find_record(y);
	// Add the variables in y's record to x's record.
	add(xrec->first, yrec->first);
	// If x doesn't have a value, then just use y's value;
	if (not xrec->second)
	    xrec->second = yrec->second;
	else if (yrec->second)
	    unify(*xrec->second, *yrec->second);
	remove_record_for(y);
    }
#ifndef NDEBUG
    for(auto& eq: values)
	assert(eq.second or eq.first.size() > 1);
#endif
    return valid;
}

optional<term_t> equations::value_of_var(const string& x) const
{
    if (has_record(x))
	return find_record(x)->second;
    else
	return boost::none;
}


void equations::eliminate_variable(const string& x)
{
    if (not has_record(x)) return;

    // find the record
    auto xrec = find_record(x);
    auto value = xrec->second;

    // remove x from the record
    xrec->first.erase(x);


    map<string,term_t>  S;
    // substitute the term if there is one
    if (value)
	S = {{x,*value}};
    // substitute an equivalent variable if there's no term in the equation
    else
    {
	// If there's no term in the equation, there should at least be another variable
	assert(not xrec->first.empty());
	auto y = *xrec->first.begin();
	S = {{x,term_t(y)}};
    }

    // remove the equation if it has no variables, or 1 variable and no term.
    if (xrec->first.empty() or (xrec->first.size() == 1 and not xrec->second))
	values.erase(xrec);

    // replace occurrences of x with the equivalent term or variable
    for(auto& equation: values)
	if (equation.second)
	    substitute(S, *equation.second);

    // Check invariant that we have either a term or >1 variable (#variables + #term >= 2)
#ifndef NDEBUG
    for(auto& eq: values)
	assert(eq.second or eq.first.size() > 1);
#endif
}

void equations::eliminate_except(const set<string>& keep)
{
    set<string> eliminate = referenced_vars();
    remove(eliminate, keep);
    for(auto& x: eliminate)
	eliminate_variable(x);
}

set<string> equations::referenced_vars() const
{
    set<string> vars;
    for(auto& v: values)
    {
	add(vars, v.first);
	if (v.second)
	    add(vars, find_variables_in_type(*v.second));
    }
    return vars;
}

bool is_variable(const ptree& p)
{
    if (not p.empty()) return false;
    const string& s = p.get_value<string>();
    if (s.empty()) return false;
    char first_letter = s[0];
    return (first_letter >= 97 and first_letter <= 122);
}

set<string> find_variables_in_type(const ptree& p)
{
    set<string> vars;
    if (is_variable(p))
	vars.insert(p.get_value<string>());
    else
	for(const auto& x: p)
	    add(vars,find_variables_in_type(x.second));
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

//

map<string,term_t> alpha_rename(const set<string>& vars, const set<string>& vars_to_avoid)
{
    int index = std::max(find_unused_index(vars), find_unused_index(vars_to_avoid));

    map<string,term_t> replace;
    for(const auto& var: vars)
    {
	if (includes(vars_to_avoid, var))
	{
	    string new_var = string("var") + convertToString(index++);
	    replace.insert({var, term_t(new_var)});
	}
    }
    return replace;
}


equations operator&&(const equations& E1, const equations& E2)
{
    if (not E1) return equations{false};
    if (not E2) return equations{false};

    equations solution = E1;

    for(const auto& value: E2.get_values())
    {
	string var0 = *value.first.begin();
	for(const auto& var: value.first)
	    solution.add_var_condition(var0,var);
	if (value.second)
	    solution.add_condition(var0, *value.second);
    }

    return solution;
}


void substitute(const equations& E, term_t& T)
{
    if (is_variable(T))
    {
	string name = T.get_value<string>();
	if (E.value_of_var(name))
	    T = ptree(*E.value_of_var(name));
    }
    else
	for(auto& child: T)
	    substitute(E, child.second);
}

void substitute(const map<string,term_t>& R, term_t& T)
{
    if (is_variable(T))
    {
	string name = T.get_value<string>();
	if (R.count(name))
	    T = R.at(name);
    }
    else
	for(auto& child: T)
	    substitute(R, child.second);
}

bool equations::unify(const term_t& T1, const term_t& T2)
{
    string head1 = T1.get_value<string>();
    string head2 = T2.get_value<string>();

    // 1. If either term is a wildcard, then we are done.
    if (is_wildcard(T1) or is_wildcard(T2))
	return valid;

    else if (is_variable(T1))
    {
	// 2. var1 = var2
	if (is_variable(T2))
	    return add_var_condition(head1, head2);
	// 3. var1 = T2
	else
	    return add_condition(head1, T2);
    }
    else if (is_variable(T2))
	// 4. var2 = T1
	return add_condition(head2, T1);

    // 5. If the heads don't match then unification fails
    if (head1 != head2)
	valid = false;

    // 6. If the arity doesn't match then unification fails
    if (T1.size() != T2.size())
	valid = false;

    // 7. Walk the arguments (children) of the T1 and T2 and unify them.
    auto x = T1.begin();
    auto y = T2.begin();
    for(int i=0; i<T1.size(); i++, x++, y++)
	if (not unify(x->second, y->second))
	    break;

    // 8. If we got here, then we succeeded!
    return valid;
}

equations unify(const term_t& T1, const term_t& T2)
{
    equations E;
    E.unify(T1, T2);
    return E;
}
