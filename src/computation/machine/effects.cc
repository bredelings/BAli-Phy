#include "effects.H"
#include "graph_register.H"
#include "util/log-level.H"

using std::string;

bool register_random_variable::operator==(const register_random_variable& e) const
{
    return pdf_reg == e.pdf_reg;
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
    return string("register_random_variable[")+std::to_string(pdf_reg)+"]";
}

register_random_variable::register_random_variable(int r)
    :pdf_reg(r)
{ }

void register_random_variable::register_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 2)
        std::cerr<<"register_random_variable["<<pdf_reg<<"]: REGISTER! ("<<M.random_variables.size()<<" -> "<<M.random_variables.size()+1<<")\n";
    M.register_random_variable(pdf_reg, s);
}

void register_random_variable::unregister_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 2)
    {
        std::cerr<<"register_random_variable["<<pdf_reg<<"]: UNregister! ("<<M.random_variables.size()<<" -> "<<M.random_variables.size()-1<<")\n";
    }
    M.unregister_random_variable(pdf_reg, s);
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
    return string("register_likelihood[")+std::to_string(likelihood_reg)+"]";
}

register_likelihood::register_likelihood(int r)
    :likelihood_reg(r)
{ }

void register_likelihood::register_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 2)
        std::cerr<<"register_likelihood["<<likelihood_reg<<"]: REGISTER! ("<<M.likelihood_heads.size()<<" -> "<<M.likelihood_heads.size()+1<<")\n";
    M.register_likelihood_(likelihood_reg, s);
}

void register_likelihood::unregister_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 2)
    {
        std::cerr<<"register_likelihood["<<likelihood_reg<<"]: UNregister! ("<<M.likelihood_heads.size()<<" -> "<<M.likelihood_heads.size()-1<<")\n";
    }
    M.unregister_likelihood_(likelihood_reg, s);
}

//--------------------------------------------------------------------

bool register_transition_kernel::operator==(const register_transition_kernel& e) const
{
    return rate_reg == e.rate_reg and kernel_reg == e.kernel_reg;
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

register_transition_kernel::register_transition_kernel(int r1, int r2)
    :rate_reg(r1), kernel_reg(r2)
{ }

void register_transition_kernel::register_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 2)
        std::cerr<<"register_transition_kernel["<<kernel_reg<<"]: REGISTER!\n";
    M.register_transition_kernel(rate_reg, kernel_reg, s);
}

void register_transition_kernel::unregister_effect(reg_heap& M, int s) const
{
    if (log_verbose >= 2)
        std::cerr<<"register_transition_kernel["<<kernel_reg<<"]: UNregister!\n";
    M.unregister_transition_kernel(kernel_reg, s);
}
