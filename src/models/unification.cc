#include "unification.H"
#include <set>
#include <regex>
#include "util/set.H"
#include "util/string/join.H"
#include "util/string/convert.H"
#include "parse.H"

using std::list;
using std::vector;
using std::pair;
using std::map;
using std::set;
using std::string;
using std::optional;


bool is_wildcard(const ptree& p)
{
    return p == "_";
}

/// Split a string of the form key=value into {key,value}
string show(const equations& E)
{
    string result;
    if (not E) result = "FAIL:\n";

    vector<string> constraints;
    for(auto& constraint: E.get_constraints())
	constraints.push_back(unparse_type(constraint));
    if (not constraints.empty())
	result += join(constraints,",") + "\n\n";

    vector<string> eqs;
    for(auto& [names,term]: E.get_values())
    {
	string e = join(names," = ");
	if (term)
	    e += " = " + unparse_type(*term);
	eqs.push_back(e);
    }
    result += join(eqs,"\n");

    return result;
}

bool equations::has_record(const std::string& x) const
{
    for(auto& [names,term]: values)
	if (names.count(x)) return true;

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

const list<pair<set<string>,optional<term_t>>>& equations::get_values() const
{
    return values;
}

const set<term_t>& equations::get_constraints() const
{
    return constraints;
}

void equations::add_constraint(const term_t& constraint)
{
    constraints.insert(constraint);
}

list<pair<set<string>,optional<term_t>>>::const_iterator equations::find_record(const std::string& x) const
{
    for(auto it = values.begin(); it != values.end(); it++)
	if (it->first.count(x))
	    return it;

    std::abort();
}

list<pair<set<string>,optional<term_t>>>::iterator equations::find_record(const std::string& x)
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
    for(auto& [names,term]: values)
	assert(term or names.size() > 1);
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

	// If the two variables are already equal then we are done
	if (xrec == yrec)
	    return valid;
	// Otherwise we need to merge the two records
	else
	{
	    // Add the variables in y's record to x's record.
	    add(xrec->first, yrec->first);
	    // Save the body in the y record.
	    auto y_body = yrec->second;
	    // Remove the y record
	    values.erase(yrec);

	    // If x doesn't have a value, then just use y's value;
	    if (not xrec->second)
		xrec->second = y_body;
	    // Otherwise unify the two values
	    else if (y_body)
		unify(*xrec->second, *y_body);

	}
    }
#ifndef NDEBUG
    for(auto& [names,term]: values)
	assert(term or names.size() > 1);
#endif
    return valid;
}

optional<term_t> equations::value_of_var(const string& x) const
{
    if (has_record(x))
	return find_record(x)->second;
    else
	return {};
}

bool compare(const ptree& a, const ptree& b)
{
    if (a.value < b.value) return true;
    if (a.value > b.value) return false;

    if (a.size() < b.size()) return true;
    if (a.size() > b.size()) return false;

    for(auto it1 = a.begin(), it2 = b.begin();it1 != a.end(); it1++, it2++)
    {
	if (it1->first < it2-> first) return true;
	if (it1->first > it2-> first) return false;

	if (compare(it1->second,it2->second)) return true;
	if (compare(it2->second,it1->second)) return false;
    }

    return false;
}

bool std::less<term_t>::operator()(const term_t& a, const term_t& b) const
{
    return compare(a,b);
}

map<string,term_t> equations::eliminate_variable(const string& x)
{
    map<string,term_t>  S;

    if (not has_record(x)) return S;

    // 1. find the record
    auto xrec = find_record(x);
    auto value = xrec->second;

    // 2. remove x from the record
    xrec->first.erase(x);

    // 3. Determine the substitution
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

    // 4. remove the equation if it has no variables, or 1 variable and no term.
    if (xrec->first.empty() or (xrec->first.size() == 1 and not xrec->second))
	values.erase(xrec);

    // 5. Apply the substitution to the equations.
    for(auto& equation: values)
	if (equation.second)
	    substitute(S, *equation.second);

    // 6. Apply the substitution to the constraints.
    set<term_t> new_constraints;
    for(auto& constraint: constraints)
    {
	auto new_constraint = constraint;
	substitute(S, new_constraint);
	new_constraints.insert(new_constraint);
    }
    std::swap(constraints, new_constraints);

    // Check invariant that we have either a term or >1 variable (#variables + #term >= 2)
#ifndef NDEBUG
    for(auto& [names,term]: values)
	assert(term or names.size() > 1);
#endif

    return S;
}

map<string,term_t> equations::eliminate_except(const set<string>& keep)
{
    map<string,term_t> S;

    // Find the vars to eliminate
    set<string> eliminate = referenced_vars();
    remove(eliminate, keep);

    // Eliminate each var
    for(auto& x: eliminate)
    {
	// Eliminate var x from equations and constraints, and return a substitution from x -> E
	auto Sx = eliminate_variable(x);

	// Eliminate var x from the cumulative substitution
	for(auto& y: S)
	    substitute(Sx,y.second);

	// Add the substitution for x -> E to the cumulative substitution
	for(auto& y: Sx)
	{
	    assert(S.count(y.first) == 0);
	    S[y.first] = y.second;
	}
    }
    return S;
}

set<string> equations::referenced_vars() const
{
    set<string> vars;

    // Get vars referenced by constraints
    for(auto& constraint: constraints)
	add(vars, find_variables_in_type(constraint));

    // Get vars referenced in equations
    for(auto& [names,term]: values)
    {
	add(vars, names);
	if (term)
	    add(vars, find_variables_in_type(*term));
    }
    return vars;
}

bool is_variable(const ptree& p)
{
    if (not p.has_value<string>()) return false;
    const string& s = p.get_value<string>();
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
	    if (auto i = can_be_converted_to<int>(var.substr(3)))
		index = std::max(index,*i);
	}
    }
    return index+1;
}

const std::regex rgx_uniqued( R"((.+)#[0-9]+)" );
string remove_rv_suffix(const std::string& s)
{
    std::smatch m;

    if (std::regex_match(s, m, rgx_uniqued))
	return m[1];
    else
	return s;
}

map<string,term_t> alpha_rename(const set<string>& vars, FVState& fresh_var_state)
{
    map<string,term_t> replace;
    for(const auto& var: vars)
    {
	auto new_var = fresh_var_state.get_fresh_type_var(remove_rv_suffix(var));
	replace.insert({var, new_var});
    }
    return replace;
}


equations operator&&(const equations& E1, const equations& E2)
{
    if (not E1) return equations{false};
    if (not E2) return equations{false};

    equations solution = E1;
    for(auto& constraint: E2.get_constraints())
	solution.add_constraint(constraint);

    for(const auto& [vars,value]: E2.get_values())
    {
	string var0 = *vars.begin();
	for(const auto& var: vars)
	    solution.add_var_condition(var0,var);
	if (value)
	    solution.add_condition(var0, *value);
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
    // 1. If either term is a wildcard, then we are done.
    if (is_wildcard(T1) or is_wildcard(T2))
	return valid;

    else if (is_variable(T1))
    {
	// 2. var1 = var2
	if (is_variable(T2))
	    return add_var_condition(T1, T2);
	// 3. var1 = T2
	else
	    return add_condition(T1, T2);
    }
    else if (is_variable(T2))
	// 4. var2 = T1
	return add_condition(T2, T1);

    // 5. If the heads don't match then unification fails
    if (T1.value != T2.value)
	valid = false;

    // 6. If the arity doesn't match then unification fails
    if (T1.size() != T2.size())
	valid = false;

    if (not valid) return false;

    // 7. Walk the arguments (children) of the T1 and T2 and unify them.
    //    -- but unifying things might result in reallocating the vector, and thus invalidating the entries.
    auto x = T1.begin();
    auto y = T2.begin();
    for(int i=0; i<T1.size(); i++, x++, y++)
	if (not unify(x->second, y->second))
	    break;

    // 8. If we got here, then we succeeded!
    return valid;
}

string equations::show() const
{
    return ::show(*this);
}

equations unify(const term_t& T1, const term_t& T2)
{
    equations E;
    E.unify(T1, T2);
    return E;
}
