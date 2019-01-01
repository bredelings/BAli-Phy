#include "parameter.H"

bool parameter::operator==(const Object& o) const 
{
    const parameter* E = dynamic_cast<const parameter*>(&o);
    if (not E) 
	return false;

    return parameter_name == E->parameter_name;
}

parameter::parameter(const std::string& s)
    :parameter_name(s)
{
}

bool is_parameter(const expression_ref& E)
{
    return E.is_a<parameter>();
}

