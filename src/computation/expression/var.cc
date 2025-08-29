#include "var.H"
#include "computation/expression/let.H"
#include "computation/expression/case.H"
#include "haskell/ids.H"
#include "haskell/coretype.H"

using std::set;
using std::vector;
using std::multiset;
using std::string;

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
    string s = name;
    if (name.size() and index == 0)
        ;
    else
	s += string("#")+convertToString(index);
    if (level)
        s += ":"+std::to_string(*level);
    return s;
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

    // Make sure we don't try to substitute for lambda-quantified dummies
    if (E.head().type() == type_constant::lambda_type)
    {
	if (E.sub()[0].is_a<var>())
	    bound.insert(E.sub()[0].as_<var>());
    }
    else if (is_let_expression(E))
    {
        auto& L = E.as_<let_exp>();
        for(auto& [x,_]: L.binds)
            bound.insert(x);

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

    // for case expressions get_bound_indices doesn't work correctly.
    // .. we need to handle each Alt separately.
    if (auto C = parse_case_expression(E))
    {
        auto& [object, alts] = *C;

	get_free_indices2(object, bound, free);

	for(auto& [pattern, body]: alts)
	{
	    std::set<var> bound_ = get_free_indices(pattern);
	    for(const auto& d: bound_)
		bound.insert(d);
	    get_free_indices2(body, bound, free);
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

    if (is_let_expression(E))
    {
        auto& L = E.as_<let_exp>();
        for(auto [_,e]: L.binds)
            get_free_indices2(e, bound, free);
        get_free_indices2(L.body, bound, free);
    }
    else
    {
        for(int i=0;i<E.size();i++)
            get_free_indices2(E.sub()[i], bound, free);
    }

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

bool is_var(const expression_ref& E)
{
    return (E.head().type() == type_constant::var_type);
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

expression_ref wildcard()
{
    return var(-1);
}

