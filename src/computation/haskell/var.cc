#include "var.H"

using std::string;

namespace Haskell
{

string Var::print() const
{
    return unloc(name);
}

string Var::print_with_type() const
{
    if (type)
        return "("+unloc(name) + " :: " + (*type).print()+")";
    else
        return unloc(name);
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
    return unloc(name) == unloc(v.name);
}

bool Var::operator<(const Var& v) const
{
    int cmp = unloc(name).compare(unloc(v.name));

    return cmp;
}

}
