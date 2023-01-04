#include "var.H"
#include "ids.H"

using std::string;

namespace Haskell
{

bool Var::is_sym() const
{
    return is_haskell_sym(unloc(name));
}

string Var::print_without_parens() const
{
    string uname = unloc(name);
    if (index)
        uname = uname +"#"+std::to_string(*index);

    return uname;
}

string Var::print() const
{
    string uname = print_without_parens();

    if (is_haskell_sym(uname))
        uname = "(" + uname + ")";

    return uname;
}

string Var::print_with_type() const
{
    string uname = print();

    if (type)
        uname = "("+uname + " :: " + (*type).print()+")";

    return uname;
}

bool Var::operator==(const Object& o) const
{
    auto V = dynamic_cast<const Var*>(&o);
    if (not V)
        return false;

    return (*this) == *V;
}

bool Var::operator==(const Var& v) const
{
    return index == v.index and unloc(name) == unloc(v.name);
}

bool Var::operator<(const Var& v) const
{
    if (index < v.index)
        return true;

    if (index > v.index)
        return false;

    int cmp = unloc(name).compare(unloc(v.name));
    
    return (cmp < 0);
}

bool Con::is_sym() const
{
    return is_haskell_sym(unloc(name));
}

bool Con::operator==(const Object& o) const
{
    auto C = dynamic_cast<const Con*>(&o);
    if (not C)
        return false;

    return (*this) == *C;
}

bool Con::operator==(const Con& c) const
{
    return unloc(name) == unloc(c.name);
}

string Con::print() const
{
    return unloc(name);
}

} // namespace Haskell
