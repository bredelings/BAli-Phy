#include "effects.H"
#include "graph_register.H"
#include "util/log-level.H"

using std::string;

bool register_prior::operator==(const register_prior& e) const
{
    return variable_reg == e.variable_reg;
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
    return string("register_prior[")+std::to_string(variable_reg)+","+std::to_string(pdf)+"]";
}

register_prior::register_prior(int r, log_double_t pr)
    :variable_reg(r), pdf(pr)
{ }

void register_prior::register_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 5)
    {
        std::cerr<<"register_prior[var="<<variable_reg<<",pdf="<<pdf<<",step="<<s<<"]: ";
        std::cerr<<"  REGISTER! ("<<M.random_variables.size()<<" -> "<<M.random_variables.size()+1<<")\n";
    }
    M.register_prior(*this, s);
}

void register_prior::unregister_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 5)
    {
        std::cerr<<"register_prior[var="<<variable_reg<<",pdf="<<pdf<<",step="<<s<<"]: ";
        std::cerr<<"UNregister! ("<<M.random_variables.size()<<" -> "<<M.random_variables.size()-1<<")\n";
    }
    M.unregister_prior(*this, s);
}

//--------------------------------------------------------------------

bool register_likelihood::operator==(const register_likelihood& e) const
{
    return likelihood_reg == e.likelihood_reg;
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
    return string("register_likelihood[")+std::to_string(likelihood_reg)+","+std::to_string(likelihood)+"]";
}

register_likelihood::register_likelihood(int r, log_double_t lk)
    :likelihood_reg(r), likelihood(lk)
{ }

void register_likelihood::register_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 5)
    {
        std::cerr<<"register_likelihood["<<likelihood_reg<<", likelihood="<<likelihood<<", step="<<s<<"]: ";
        std::cerr<<"  REGISTER! ("<<M.likelihood_heads.size()<<" -> "<<M.likelihood_heads.size()+1<<")\n";
    }
    M.register_likelihood_(*this, s);
}

void register_likelihood::unregister_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 5)
    {
        std::cerr<<"register_likelihood["<<likelihood_reg<<", likelihood="<<likelihood<<", step="<<s<<"]: ";
        std::cerr<<"UNregister! ("<<M.likelihood_heads.size()<<" -> "<<M.likelihood_heads.size()-1<<")\n";
    }
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
    return r_from_var == e.r_from_var and s_to_dist == e.s_to_dist and role == e.role;
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
    result<<"in_edge[from="<<r_from_var<<",to="<<s_to_dist<<",role="<<role<<"]";
    return result.str();
}

in_edge::in_edge(int i1, int i2, const string& s)
    :r_from_var(i1), s_to_dist(i2), role(s)
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
    return s_from_dist == e.s_from_dist and r_to_var == e.r_to_var;
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
    result<<"out_edge[from="<<s_from_dist<<",to="<<r_to_var<<"]";
    return result.str();
}

out_edge::out_edge(int i1, int i2)
    :s_from_dist(i1), r_to_var(i2)
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
    return name == e.name;
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
    result<<"register_dist[name="<<name<<"]";
    return result.str();
}

register_dist::register_dist(const string& s)
    :name(s)
{ }

void register_dist::register_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 5) std::cerr<<(*this)<<": REGISTER!\n";
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
    return s_from_dist == e.s_from_dist and r_to_prop == e.r_to_prop and property == e.property;
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
    result<<"dist_property[from="<<s_from_dist<<",to="<<r_to_prop<<",property="<<property<<"]";
    return result.str();
}

dist_property::dist_property(int i1, int i2, const string& s)
    :s_from_dist(i1), r_to_prop(i2), property(s)
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
