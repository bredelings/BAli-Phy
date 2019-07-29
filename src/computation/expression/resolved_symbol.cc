#include "resolved_symbol.H"
#include "module.H"
using std::set;
using std::vector;
using std::multiset;
using std::string;

bool resolved_symbol::operator==(const resolved_symbol& rs) const
{
    return name == rs.name;
}

bool resolved_symbol::operator==(const Object& o) const
{
    const resolved_symbol* rs = dynamic_cast<const resolved_symbol*>(&o);
    if (not rs)
	return false;

    return (*this) == *rs;
}

string resolved_symbol::print() const
{
    if (is_haskell_qsym(name)) // (:) or QRESOLVED_SYMBOLSYM or QCONSYM
    {
	return string("(") + name + ")";
    }
    else
	return name;
}

bool resolved_symbol::operator<(const resolved_symbol& rs) const
{
    int cmp = name.compare(rs.name);

    return (cmp < 0);
}

bool is_resolved_symbol(const expression_ref& E)
{
    return (E.is_a<resolved_symbol>());
}
