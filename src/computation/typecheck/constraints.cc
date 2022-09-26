#include "constraints.H"


WantedConstraints::WantedConstraints(const LIE& l)
    : simple(l)
{
}


WantedConstraints& WantedConstraints::operator+=(const WantedConstraints& wc2)
{
    simple += wc2.simple;
    implications += wc2.implications;
    return *this;
}

WantedConstraints WantedConstraints::operator+(const WantedConstraints& wc2) const
{
    auto tmp = *this;
    tmp += wc2;
    return tmp;
}
