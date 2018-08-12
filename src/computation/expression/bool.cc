#include "bool.H"

expression_ref bool_true  = constructor("Prelude.True",  0);
expression_ref bool_false = constructor("Prelude.False", 0);

std::string bool_true_name  = "Prelude.True";
std::string bool_false_name = "Prelude.False";

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
