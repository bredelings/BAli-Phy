#include "types.H"
#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"
#include "computation/expression/case.H"
#include "computation/module.H"

using std::set;
using std::vector;
using std::multiset;
using std::string;

// type var

bool type_var::operator==(const type_var& d) const
{
    return index == d.index and name == d.name;
}

bool type_var::operator==(const Object& o) const 
{
    const type_var* D = dynamic_cast<const type_var*>(&o);
    if (not D) 
	return false;

    return (*this) == *D;
}

string type_var::print() const {
    return name;
}

bool type_var::operator<(const type_var& D) const 
{
    if (index < D.index) return true;
    if (index > D.index) return false;

    int cmp = name.compare(D.name);

    return (cmp < 0);
}

bool is_type_var(const expression_ref& E)
{
    return (E.head().type() == type_var_type);
}

// type con

bool type_con::operator==(const type_con& d) const
{
    return index == d.index and name == d.name;
}

bool type_con::operator==(const Object& o) const 
{
    const type_con* D = dynamic_cast<const type_con*>(&o);
    if (not D) 
	return false;

    return (*this) == *D;
}

string type_con::print() const {
    string s = name;
    if (name.size() and index == 0)
        ;
    else
	s += string("#")+convertToString(index);

    return s;
}

bool type_con::operator<(const type_con& D) const 
{
    if (index < D.index) return true;
    if (index > D.index) return false;

    int cmp = name.compare(D.name);

    return (cmp < 0);
}

bool is_type_con(const expression_ref& E)
{
    return (E.head().type() == type_con_type);
}

// type apply

bool type_apply_node::operator==(const Object& o) const 
{
    return dynamic_cast<const type_apply_node*>(&o);
}

string type_apply_node::print() const {
    return "@";
}

bool is_type_apply_node(const expression_ref& E)
{
    return (E.head().type() == type_apply_node_type);
}

expression_ref type_apply(const expression_ref& t1, const expression_ref& t2)
{
    return expression_ref(type_apply_node(),{t1,t2});
}

bool is_type_apply(const expression_ref& E)
{
    bool pred = is_type_apply_node(E.head());
    if (pred)
        assert(E.size() == 2);
    return pred;
}

// type forall

bool type_forall_node::operator==(const Object& o) const 
{
    return dynamic_cast<const type_forall_node*>(&o);
}

string type_forall_node::print() const {
    return "forall";
}

bool is_type_forall_node(const expression_ref& E)
{
    return (E.head().type() == type_forall_node_type);
}


expression_ref type_forall(const expression_ref& tv, const expression_ref& E)
{
    assert(is_type_var(tv));

    return expression_ref(type_forall_node(),{tv,E});
}

bool is_type_forall(const expression_ref& E)
{
    bool pred = is_type_forall_node(E.head());
    if (pred)
        assert(E.size() == 2);
    return pred;
}

bool is_type(const expression_ref& E)
{
    if (is_type_forall(E)) return true;

    if (is_type_var(E)) return true;

    if (is_type_con(E)) return true;

    if (is_type_apply(E)) return true;

    return false;
}

// function types


type_con function_type_con_node()
{
    return type_con("(->)");
}

bool is_function_type_con_node(const expression_ref& E)
{
    if (not is_type_con(E)) return false;

    auto& tc = E.as_<type_con>();

    return tc == function_type_con_node();
}

expression_ref function_type(const expression_ref& T1, const expression_ref& T2)
{
    return type_apply(type_apply(function_type_con_node(),T1),T2);
}

bool is_function_type(const expression_ref& T1)
{
    // (((->) a) b)-> ((->) a)
    if (not is_type_apply(T1)) return false;

    auto T2 = T1.sub()[0];

    // (((->) a) b)-> (->)
    if (not is_type_apply(T2)) return false;

    auto T3 = T2.sub()[0];

    // (->) = tycon
    return is_function_type_con_node(T3);
}


void get_free_type_variables(const expression_ref& E, multiset<type_var>& bound, set<type_var>& free)
{
    // 1. fv x = { x }
    // However, if there are any binders called "x", then it won't be bound at the top level.
    if (is_type_var(E))
    {
	auto& tv = E.as_<type_var>();
	if (bound.find(tv) == bound.end())
	    free.insert(tv);
	return;
    }
    // 2. fv (t1 t2) = fv(t1) + fv(t2);
    else if (is_type_apply(E))
    {
        get_free_type_variables(E.sub()[0], bound, free);
        get_free_type_variables(E.sub()[1], bound, free);
    }
    // 3. fv (k) = {}
    else if (is_type_con(E.head()))
    {
    }
    // 4. fv (forall a.tau) = fv(tau) - a
    else if (is_type_forall(E))
    {
        assert(is_type_var(E.sub()[0]));
        auto& tv = E.sub()[0].as_<type_var>();
        bound.insert(tv);
        get_free_type_variables(E.sub()[1], bound, free);
        bound.erase(tv);
    }
    else
        std::abort();
}

void get_free_type_variables(const expression_ref& E, set<type_var>& free)
{
    multiset<type_var> bound;
    get_free_type_variables(E,bound,free);
}

std::set<type_var> free_type_variables(const expression_ref& E)
{
    set<type_var> free;
    get_free_type_variables(E, free);
    return free;
}

bool is_type_con_expression(const expression_ref& E)
{
    if (is_type_con(E))
        return true;
    else if (is_type_apply(E))
    {
        assert(is_type_con_expression(E.sub()[0]));
        return true;
    }
    return false;
}

expression_ref expand_type(const expression_ref& E)
{
    if (is_type_var(E)) return E;

    if (is_type_con(E)) return E;

    if (is_type_forall(E))
    {
        auto tv = E.sub()[0];
        auto subtype = E.sub()[1];
        subtype = expand_type( subtype );
        return type_forall_node() + tv + subtype;
    }

    assert(is_type_apply(E));
    assert(is_type_con_expression(E.sub()[0]));
    
    return expand_type(E.sub()[0]) + expand_type(E.sub()[1]);
}
