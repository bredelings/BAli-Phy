#include "constructor.H"
#include "computation/module.H" // for is_haskell_con_name( )
#include "expression.H" // for lambda_expression( )

using std::string;

tribool constructor::compare(const Object& o) const
{
    const constructor* E = dynamic_cast<const constructor*>(&o);
    if (not E) 
	return false;

    // Should we check that the arity matches also?

    return f_name == E->f_name;
}

constructor::constructor(const string& s, int n)
    :f_name(s), n_args_(n), assoc(assoc_none),prec(-1)
{
    assert(is_haskell_con_name(s) or (s=="*") or (s=="->"));
}

constructor left_assoc_constructor(const std::string& s,int prec)
{
    constructor f(s, 2);
    f.prec = prec;
    f.assoc = assoc_left;
    return f;
}

constructor right_assoc_constructor(const std::string& s,int prec)
{
    constructor f(s, 2);
    f.prec = prec;
    f.assoc = assoc_right;
    return f;
}

bool has_constructor(const expression_ref& E, const string& s)
{
    return E.head() == constructor(s,-1);
}

bool is_tuple_name(const string& s)
{
    if (s.size() < 3) return false;
    return s == tuple_name(s.size()-1);
}

string tuple_name(int n)
{
    if (n == 0)
	return "()";

    if (n == 1)
	std::abort();

    string s;
    s.resize(n+1);
    s[0] = '(';
    for(int i=1;i<n;i++)
	s[i] = ',';
    s[n] = ')';
    return s;
}

constructor tuple_head(int n)
{
    assert(n != 1);

    string s = tuple_name(n);
    return constructor(s,n);
}

expression_ref Tuple(int n)
{
    assert(n >= 0);
    return lambda_expression( tuple_head(n) );
}

