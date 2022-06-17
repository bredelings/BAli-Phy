#include "var.H"

using std::string;

namespace Haskell
{

string Var::print() const
{
    string uname = unloc(name);
    if (index)
        uname = uname +"#"+std::to_string(*index);

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
    
    return (cmp < 0);;
}

string Con::print() const
{
    return unloc(name);
}

}
