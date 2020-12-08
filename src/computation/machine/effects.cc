#include "effects.H"
#include "graph_register.H"
#include "util/log-level.H"

using std::string;

bool register_random_variable::operator==(const register_random_variable& e) const
{
    return variable_reg == e.variable_reg;
}

bool register_random_variable::operator==(const Object& O) const
{
    if (this == &O) return true;

    if (typeid(*this) != typeid(O)) return false;

    auto* e = dynamic_cast<const register_random_variable*>(&O);

    return (*this) == *e;
}

string register_random_variable::print() const
{
    return string("register_random_variable[")+std::to_string(variable_reg)+","+std::to_string(pdf)+"]";
}

register_random_variable::register_random_variable(int r, log_double_t pr)
    :variable_reg(r), pdf(pr)
{ }

void register_random_variable::register_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 4)
    {
        std::cerr<<"register_random_variable[var="<<variable_reg<<",pdf="<<pdf<<",step="<<s<<"]: ";
        std::cerr<<"  REGISTER! ("<<M.random_variables.size()<<" -> "<<M.random_variables.size()+1<<")\n";
    }
    M.register_random_variable(*this, s);
}

void register_random_variable::unregister_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 4)
    {
        std::cerr<<"register_random_variable[var="<<variable_reg<<",pdf="<<pdf<<",step="<<s<<"]: ";
        std::cerr<<"UNregister! ("<<M.random_variables.size()<<" -> "<<M.random_variables.size()-1<<")\n";
    }
    M.unregister_random_variable(*this, s);
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
    if (log_verbose >= 4)
    {
        std::cerr<<"register_likelihood["<<likelihood_reg<<", likelihood="<<likelihood<<", step="<<s<<"]: ";
        std::cerr<<"  REGISTER! ("<<M.likelihood_heads.size()<<" -> "<<M.likelihood_heads.size()+1<<")\n";
    }
    M.register_likelihood_(*this, s);
}

void register_likelihood::unregister_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 4)
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
    if (log_verbose >= 4)
        std::cerr<<"register_transition_kernel[rate="<<rate<<",kernel="<<kernel_reg<<"]: REGISTER!\n";
    M.register_transition_kernel(*this, s);
}

void register_transition_kernel::unregister_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 4)
        std::cerr<<"register_transition_kernel[rate="<<rate<<",kernel="<<kernel_reg<<"]: UNREGISTER!\n";
    M.unregister_transition_kernel(*this, s);
}
