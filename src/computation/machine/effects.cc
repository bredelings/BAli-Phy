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

//--------------------------------------------------------------------

bool register_transition_kernel::operator==(const register_transition_kernel& e) const
{
    return rate == e.rate and kernel_reg == e.kernel_reg;
}

bool register_transition_kernel::operator==(const Object& O) const
{
    if (this == &O) return true;

    if (typeid(*this) != typeid(O)) return false;

    auto* e = dynamic_cast<const register_transition_kernel*>(&O);

    return (*this) == *e;
}

string register_transition_kernel::print() const
{
    return string("register_transition_kernel[")+std::to_string(kernel_reg)+"]";
}

register_transition_kernel::register_transition_kernel(double r, int k)
    :rate(r), kernel_reg(k)
{ }

//--------------------------------------------------------------------

bool in_edge::operator==(const in_edge& e) const
{
    return r_from_node == e.r_from_node and r_to_dist == e.r_to_dist and arg_name == e.arg_name;
}

bool in_edge::operator==(const Object& O) const
{
    if (this == &O) return true;

    if (typeid(*this) != typeid(O)) return false;

    auto* e = dynamic_cast<const in_edge*>(&O);

    return (*this) == *e;
}

string in_edge::print() const
{
    std::ostringstream result;
    result<<"in_edge[from="<<r_from_node<<",to="<<r_to_dist<<",arg_name="<<arg_name<<"]";
    return result.str();
}

in_edge::in_edge(int i1, int i2, const string& s)
    :r_from_node(i1), r_to_dist(i2), arg_name(s)
{ }

//--------------------------------------------------------------------

bool out_edge::operator==(const out_edge& e) const
{
    return r_from_dist == e.r_from_dist and r_to_var == e.r_to_var;
}

bool out_edge::operator==(const Object& O) const
{
    if (this == &O) return true;

    if (typeid(*this) != typeid(O)) return false;

    auto* e = dynamic_cast<const out_edge*>(&O);

    return (*this) == *e;
}

string out_edge::print() const
{
    std::ostringstream result;
    result<<"out_edge[from="<<r_from_dist<<",to="<<r_to_var<<"]";
    return result.str();
}

out_edge::out_edge(int i1, int i2)
    :r_from_dist(i1), r_to_var(i2)
{ }

//--------------------------------------------------------------------

bool register_dist::operator==(const register_dist& e) const
{
    return name == e.name and r == e.r and observation == e.observation;
}

bool register_dist::operator==(const Object& O) const
{
    if (this == &O) return true;

    if (typeid(*this) != typeid(O)) return false;

    auto* e = dynamic_cast<const register_dist*>(&O);

    return (*this) == *e;
}

string register_dist::print() const
{
    std::ostringstream result;
    result<<"register_dist[name="<<name<<",r="<<r<<",observation="<<observation<<"]";
    return result.str();
}

register_dist::register_dist(const string& s, int i, bool b)
    :name(s),r(i),observation(b)
{ }

