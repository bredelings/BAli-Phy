#include "var.H"
#include "ids.H"

using std::string;

namespace Haskell
{

bool Var::is_sym() const
{
    return is_haskell_sym(name);
}

string Var::print_without_parens() const
{
    return name;
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
    return name == v.name;
}

bool Var::operator<(const Var& v) const
{
    int cmp = name.compare(v.name);
    
    return (cmp < 0);
}

bool Con::is_sym() const
{
    return is_haskell_sym(name);
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
    return name == c.name;
}

string Con::print() const
{
    return name;
}

} // namespace Haskell
