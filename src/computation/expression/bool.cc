#include "bool.H"

std::string bool_true_name  = "Data.Bool.True";
std::string bool_false_name = "Data.Bool.False";

expression_ref bool_true  = constructor(bool_true_name,  0);
expression_ref bool_false = constructor(bool_false_name, 0);

bool is_bool_true(const constructor& c)
{
    return c.f_name == bool_true_name;
}

bool is_bool_false(const constructor& c)
{
    return c.f_name == bool_false_name;
}

bool is_bool_true(const expression_ref& e)
{
    return e.is_a<constructor>() and is_bool_true(e.as_<constructor>());
}

bool is_bool_false(const expression_ref& e)
{
    return e.is_a<constructor>() and is_bool_false(e.as_<constructor>());
}
