#include "expression_ref.H"
#include "constructor.H"
#include "computation/haskell/ids.H"

namespace Runtime
{
    struct ConstructorTag;
}

extern expression_ref bool_true;
extern expression_ref bool_false;

bool is_bool(const constructor& c);
bool is_bool_true(const constructor& c);
bool is_bool_false(const constructor& c);

bool is_bool(const Runtime::ConstructorTag& c);
bool is_bool_true(const Runtime::ConstructorTag& c);
bool is_bool_false(const Runtime::ConstructorTag& c);

bool is_bool(const expression_ref& c);
bool is_bool_true(const expression_ref& e);
bool is_bool_false(const expression_ref& e);

bool is_otherwise(const expression_ref& e);
