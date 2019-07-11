#include "var.H"
#include "computation/expression/let.H"
#include "computation/expression/case.H"
#include "computation/module.H"

using std::set;
using std::vector;
using std::multiset;
using std::string;

bool occurrence_info::pre_inline() const
{
    if (work_dup != amount_t::Once) return false;
    if (code_dup != amount_t::Once) return false;
    if (context == var_context::argument) return false;
    if (is_loop_breaker) return false;
    return true;
}

bool var::operator==(const var& d) const
{
    return index == d.index and name == d.name;
}

bool var::operator==(const Object& o) const 
{
    const var* D = dynamic_cast<const var*>(&o);
    if (not D) 
	return false;

    return (*this) == *D;
}

string var::print() const {
    if (is_wildcard())
	return "_";
    else if (is_haskell_qsym(name)) // (:) or QVARSYM or QCONSYM
    {
	assert(index == 0);
	return string("(") + name + ")";
    }
    else if (name.size() and index == 0)
	return name;
    else
	return name+string("#")+convertToString(index);
}

bool var::operator<(const var& D) const 
{
    if (index < D.index) return true;
    if (index > D.index) return false;

    int cmp = name.compare(D.name);

    return (cmp < 0);
}

std::set<var> get_free_indices(const expression_ref& E);

/// Return the min of v
template<typename T>
T max(const std::set<T>& v)
{
    T t = *v.begin();
    for(const auto& i: v)
	t = std::max(t,i);

    return t;
}

int max_index(const std::set<var>& s)
{
    if (s.empty()) return -1;
    return max(s).index;
}

/// Return the min of v
template<typename T>
T min(const std::set<T>& v)
{
    T t = *v.begin();
    for(const auto& i: v)
	t = std::min(t,*i);

    return t;
}

// Return the list of variable indices that are bound at the top level of the expression
std::set<var> get_bound_indices(const expression_ref& E)
{
    std::set<var> bound;

    if (not E.size()) return bound;

    // Make sure we don't try to substitute for lambda-quantified dummies
    if (E.head().type() == lambda_type)
    {
	if (E.sub()[0].is_a<var>())
	    bound.insert(E.sub()[0].as_<var>());
    }
    else 
    {
	if (is_let_expression(E))
	{
	    auto decls = let_decls(E);
	    for(auto& decl: decls)
		bound.insert(decl.first);
	}
	assert(not is_case(E));
    }

    return bound;
}

void get_free_indices2(const expression_ref& E, multiset<var>& bound, set<var>& free)
{
    // fv x = { x }
    if (is_var(E))
    {
	var d = E.as_<var>();
	if (not is_wildcard(E) and (bound.find(d) == bound.end()))
	    free.insert(d);
	return;
    }

    // fv c = { }
    if (not E.size()) return;

    // for case expressions get_bound_indices doesn't work correctly.
    expression_ref object;
    vector<expression_ref> patterns;
    vector<expression_ref> bodies;
    if (parse_case_expression(E, object, patterns, bodies))
    {
	get_free_indices2(object, bound, free);

	const int L = patterns.size();

	for(int i=0;i<L;i++)
	{
	    std::set<var> bound_ = get_free_indices(patterns[i]);
	    for(const auto& d: bound_)
		bound.insert(d);
	    get_free_indices2(bodies[i], bound, free);
	    for(const auto& d: bound_)
	    {
		auto it = bound.find(d);
		bound.erase(it);
	    }
	}

	return;
    }

    std::set<var> bound_ = get_bound_indices(E);
    for(const auto& d: bound_)
	bound.insert(d);
    for(int i=0;i<E.size();i++)
	get_free_indices2(E.sub()[i], bound, free);
    for(const auto& d: bound_)
    {
	auto it = bound.find(d);
	bound.erase(it);
    }
}

std::set<var> get_free_indices(const expression_ref& E)
{
    multiset<var> bound;
    set<var> free;
    get_free_indices2(E, bound, free);
    return free;
}

int get_safe_binder_index(const expression_ref& E)
{
    std::set<var> free = get_free_indices(E);
    if (free.empty()) 
	return 0;
    else
	return max_index(free)+1;
}

bool is_var(const expression_ref& E)
{
    return (E.head().type() == var_type);
}

var qualified_var(const string& name)
{
    assert(name.size());
    assert(is_qualified_symbol(name));
    return var(name);
}

bool is_qualified_var(const expression_ref& E)
{
    if (not is_var(E)) return false;
    auto& x = E.as_<var>();
    if (x.is_wildcard()) return false;
    if (x.name.empty()) return false;
    return is_qualified_symbol(x.name);
}

bool is_wildcard(const var& d)
{
    return d.is_wildcard();
}

// Remove in favor of is_var?
bool is_wildcard(const expression_ref& E)
{
    if (is_var(E))
    {
	assert(not E.size());
	var d = E.as_<var>();
	return is_wildcard(d);
    }
    else
	return false;
}

