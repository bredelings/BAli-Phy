#include "effects.H"
#include "graph_register.H"
#include "util/log-level.H"

using std::string;

register_prob::register_prob(int r1, int r2, log_double_t p)
    :r_dist(r1), r_prob(r2), prob(p)
{
}

bool register_prob::operator==(const register_prob& e) const
{
    return r_dist == e.r_dist and r_prob == e.r_prob;
}

bool register_prior::operator==(const register_prior& e) const
{
    return register_prob::operator==(e);
}

bool register_prior::operator==(const Object& O) const
{
    if (this == &O) return true;

    if (typeid(*this) != typeid(O)) return false;

    auto* e = dynamic_cast<const register_prior*>(&O);

    return (*this) == *e;
}

string register_prior::print() const
{
    return string("register_prior[")+std::to_string(r_dist)+","+std::to_string(r_prob)+","+std::to_string(prob.log())+"]";
}

//--------------------------------------------------------------------

bool register_likelihood::operator==(const register_likelihood& e) const
{
    return register_prob::operator==(e);
}

bool register_likelihood::operator==(const Object& O) const
{
    if (this == &O) return true;

    if (typeid(*this) != typeid(O)) return false;

    auto* e = dynamic_cast<const register_likelihood*>(&O);

    return (*this) == *e;
}

string register_likelihood::print() const
{
    return string("register_likelihood[")+std::to_string(r_dist)+","+std::to_string(r_prob)+","+std::to_string(prob.log())+"]";
}
