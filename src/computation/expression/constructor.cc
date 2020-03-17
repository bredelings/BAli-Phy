#include "constructor.H"
#include "computation/module.H" // for is_haskell_con_name( )

using std::string;

bool constructor::operator==(const Object& o) const
{
    const constructor* E = dynamic_cast<const constructor*>(&o);
    if (not E) 
	return false;

    // Should we check that the arity matches also?

    return f_name == E->f_name;
}
bool constructor::operator<(const constructor& c2) const
{
    return (f_name < c2.f_name);
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

