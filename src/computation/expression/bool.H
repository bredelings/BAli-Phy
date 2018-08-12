#include "expression_ref.H"
#include "constructor.H"

extern expression_ref bool_true;
extern expression_ref bool_false;

bool is_bool_true(const constructor& c);
bool is_bool_false(const constructor& c);
bool is_bool_true(const expression_ref& e);
bool is_bool_false(const expression_ref& e);
