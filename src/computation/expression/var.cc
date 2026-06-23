#include "var.H"
#include "haskell/ids.H"

using std::string;

bool var::operator==(const var& d) const
{
    return name == d.name;
}

bool var::operator==(const Object& o) const 
{
    const var* D = dynamic_cast<const var*>(&o);
    if (not D) 
	return false;

    return (*this) == *D;
}

string var::print() const {
    if (is_haskell_qsym(name)) // (:) or QVARSYM or QCONSYM
    {
	return string("(") + name + ")";
    }
    string s = name;
    return s;
}

bool var::operator<(const var& D) const 
{
    int cmp = name.compare(D.name);

    return (cmp < 0);
}

bool is_var(const expression_ref& E)
{
    return (E.head().type() == type_constant::var_type);
}
