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
string equations::show() const
{
    string result;
    if (not *this) {
        result = "FAIL:\n";
        for(auto& [t1,t2]: failed)
        {
            result += "  "+unparse_type(t1) + " !~ " + unparse_type(t2) + "\n";
        }
    }

    vector<string> constraints;
    for(auto& constraint: get_constraints())
	constraints.push_back(unparse_type(constraint));
    if (not constraints.empty())
	result += join(constraints,",") + "\n\n";

    vector<string> eqs;
    for(auto& [names,term]: get_values())
    {
	string e = join(names," = ");
	if (term)
	    e += " = " + unparse_type(*term);
	eqs.push_back(e);
    }
    result += join(eqs,"\n");

    return result;
}

void equations::clear()
{
    values.clear();
    constraints.clear();
}

bool equations::has_record(const std::string& x) const
{
    for(auto& [names,term]: values)
	if (names.count(x)) return true;

    return false;
}

bool equations::valid() const
{
    return failed.empty();
}

equations::operator bool() const
{
    return valid();
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

bool equations::occurs_check() const
{
    for(auto& [vars,value]: values)
    {
	if (value)
	    if (intersects(vars, find_variables_in_type(*value))) return true;
    }
    return false;
}


bool equations::add_condition(const string& x, const term_t& T)
{
    if (is_wildcard(T)) return valid();

    if (not has_record(x))
    {
	// Occurs check.
	auto fvs_T = find_variables_in_type(T);
	if (fvs_T.count(x))
            failed.push_back({ptree(x),T});
        else
            // Add x = T
            values.push_back({set<string>{x},T});
    }
    else
    {
	auto& [vars,value] = *find_record(x);
	if (not value)
	{
	    // Occurs check.
	    auto fvs_T = find_variables_in_type(T);
	    if (intersects(vars, fvs_T))
                failed.push_back({ptree(x),T});
            else
                // Set x = T
                value = T;
	}
	else
	    // If x=U then unify(U,T)
	    unify(*value, T);
    }
#ifndef NDEBUG
    for(auto& [names,term]: values)
	assert(term or names.size() > 1);
#endif
    assert(not valid() or not occurs_check());
    return valid();
}

bool equations::add_var_condition(const string& x, const string& y)
{
    // 1. If we are adding x == y then quit.
    if (x == y) return valid();

    if (not has_record(y))
    {
	if (not has_record(x))
	    // 2. If neither x or y has a record, then add one for both of them.
	    values.push_back({set<string>{x,y},{}});
	else
	{
	    auto& [vars,T] = *find_record(x);

	    // Occurs check.
	    if (T and find_variables_in_type(*T).count(y))
	    {
                failed.push_back({ptree(x),ptree(y)});
		return valid();
	    }

	    // 3. If x has a record by y does not, then add y to x's record;
	    vars.insert(y);
	}
    }
    else if (not has_record(x))
    {
	auto& [vars,T] = *find_record(y);

	// Occurs check.
	if (T and find_variables_in_type(*T).count(x))
	{
            failed.push_back({ptree(x),ptree(y)});
	    return valid();
	}

	// 4. If y has a record by x does not, then add x to y's record;
	vars.insert(x);
    }
    else
    {
	auto xrec = find_record(x);
	auto yrec = find_record(y);

	// If the two variables are already equal then we are done
	if (xrec == yrec) return valid();

	// Otherwise we need to merge the two records

	// Keep the one with the body
	if (yrec->second and not xrec->second)
	    std::swap(xrec,yrec);

	// Add the variables in y's record to x's record.
	add(xrec->first, yrec->first);
	// Do an occurs check.
	if (xrec->second and intersects(xrec->first, find_variables_in_type(*xrec->second)))
	{
	    failed.push_back({ptree(x),ptree(y)});
	    return valid();
	}

	// Save the body of the y record.
	auto y_body = yrec->second;
	// Remove the y record
	values.erase(yrec);

        // If both records have a body, then unify them.
	if (y_body)
	{
	    assert(xrec->second);
	    unify(*xrec->second, *y_body);
	}
    }
#ifndef NDEBUG
    for(auto& [names,term]: values)
	assert(term or names.size() > 1);
#endif
    assert(not valid() or not occurs_check());
    return valid();
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
    if (p.size()) return false;

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

map<string,term_t> alpha_rename(const set<string>& vars, const FVSource& fresh_var_state)
{
    map<string,term_t> replace;
    for(const auto& var: vars)
    {
	auto new_var = fresh_var_state.get_fresh_type_var(remove_rv_suffix(var));
	replace.insert({var, new_var});
    }
    return replace;
}

// add occurs check!

equations operator&&(const equations& E1, const equations& E2)
{
    if (not E1) return E1;
    if (not E2) return E2;

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
	{
	    T = ptree(*E.value_of_var(name));
	    substitute(E, T);
	}
	else if (E.has_record(name))
	{
	    auto& [vars,_] = *E.find_record(name);
	    T = ptree(*vars.begin());
	}
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
	return valid();

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
    else if (T1.size() == 0)
    {
	if (T2.size() != 0 or T1.value != T2.value)
            failed.push_back({T1,T2});
    }
    else if (T1.size() == 2 and T2.size() == 2)
    {
	unify(T1[0].second, T2[0].second) && unify(T1[1].second, T2[1].second);
    }
    else
    {
        failed.push_back({T1,T2});
    }
    return valid();
}

equations unify(const term_t& T1, const term_t& T2)
{
    equations E;
    E.unify(T1, T2);
    return E;
}

std::pair<term_t, std::vector<term_t>> get_type_apps(term_t type)
{
    std::vector<term_t> args;

    while(type.size() > 0)
    {
	args.push_back(type[1].second);
	auto next = type[0].second;
	type = next;
    }
    std::reverse(args.begin(), args.end());

    return {type, args};
}

term_t make_type_app(const term_t& t1, const term_t& t2)
{
    return ptree("@APP",{{"",t1},{"",t2}});
}

term_t make_type_apps(term_t type, const std::vector<term_t>& args)
{
    for(auto& arg: args)
    {
	auto tmp = type;
	type = make_type_app(tmp, arg);
    }
    return type;
}

ptree get_type_head(term_t type)
{
    auto [head,args] = get_type_apps(type);
    return head;
}
