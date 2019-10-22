#include "effects.H"
#include "graph_register.H"

using std::string;

bool register_random_variable::operator==(const register_random_variable& e) const
{
    return random_variable_reg == e.random_variable_reg;
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
    return string("register_random_variable[")+std::to_string(random_variable_reg)+"]";
}

register_random_variable::register_random_variable(int r)
    :random_variable_reg(r)
{ }

void register_random_variable::register_effect(reg_heap& M, int) const
{
    M.register_random_variable(random_variable_reg);
}

void register_random_variable::unregister_effect(reg_heap& M, int) const
{
    M.unregister_random_variable(random_variable_reg);
}
