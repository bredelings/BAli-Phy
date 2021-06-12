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

void register_prior::register_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 5)
        std::cerr<<print()<<":   REGISTER! ("<<M.prior_terms.size()<<" -> "<<M.prior_terms.size()+1<<")\n";
    M.register_prior(*this, s);
}

void register_prior::unregister_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 5)
        std::cerr<<print()<<": UNregister! ("<<M.prior_terms.size()<<" -> "<<M.prior_terms.size()-1<<")\n";

    M.unregister_prior(*this, s);
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

void register_likelihood::register_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 5)
        std::cerr<<print()<<":   REGISTER! ("<<M.likelihood_terms.size()<<" -> "<<M.likelihood_terms.size()+1<<")\n";
    M.register_likelihood_(*this, s);
}

void register_likelihood::unregister_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 5)
        std::cerr<<print()<<": UNregister! ("<<M.likelihood_terms.size()<<" -> "<<M.likelihood_terms.size()-1<<")\n";
    M.unregister_likelihood_(*this, s);
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

void register_transition_kernel::register_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 5)
        std::cerr<<"register_transition_kernel[rate="<<rate<<",kernel="<<kernel_reg<<"]: REGISTER!\n";
    M.register_transition_kernel(*this, s);
}

void register_transition_kernel::unregister_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 5)
        std::cerr<<"register_transition_kernel[rate="<<rate<<",kernel="<<kernel_reg<<"]: UNREGISTER!\n";
    M.unregister_transition_kernel(*this, s);
}

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

void in_edge::register_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 5) std::cerr<<(*this)<<": REGISTER!\n";
    M.register_in_edge(*this, s);
}

void in_edge::unregister_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 5)std::cerr<<(*this)<<": UNREGISTER!\n";
    M.unregister_in_edge(*this, s);
}

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

void out_edge::register_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 5) std::cerr<<(*this)<<": REGISTER!\n";
    M.register_out_edge(*this, s);
}

void out_edge::unregister_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 5)std::cerr<<(*this)<<": UNREGISTER!\n";
    M.unregister_out_edge(*this, s);
}

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

void register_dist::register_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 5) std::cerr<<(*this)<<":  REGISTER!\n";
    M.register_dist(*this, s);
}

void register_dist::unregister_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 5)std::cerr<<(*this)<<": UNREGISTER!\n";
    M.unregister_dist(*this, s);
}

//--------------------------------------------------------------------

bool dist_property::operator==(const dist_property& e) const
{
    return r_from_dist == e.r_from_dist and r_to_prop == e.r_to_prop and property == e.property;
}

bool dist_property::operator==(const Object& O) const
{
    if (this == &O) return true;

    if (typeid(*this) != typeid(O)) return false;

    auto* e = dynamic_cast<const dist_property*>(&O);

    return (*this) == *e;
}

string dist_property::print() const
{
    std::ostringstream result;
    result<<"dist_property[from="<<r_from_dist<<",to="<<r_to_prop<<",property="<<property<<"]";
    return result.str();
}

dist_property::dist_property(int i1, int i2, const string& s)
    :r_from_dist(i1), r_to_prop(i2), property(s)
{ }

void dist_property::register_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 5) std::cerr<<(*this)<<": REGISTER!\n";
    M.register_dist_property(*this, s);
}

void dist_property::unregister_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 5)std::cerr<<(*this)<<": UNREGISTER!\n";
    M.unregister_dist_property(*this, s);
}
