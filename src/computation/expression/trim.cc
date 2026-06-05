#include "trim.H"

std::string Trim::print() const
{
    return "Trim";
}

bool Trim::operator==(const Object& o) const
{
    return dynamic_cast<const Trim*>(&o);
}
