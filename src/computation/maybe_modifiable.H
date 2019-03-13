#ifndef MAYBE_MODIFIABLE_H
#define MAYBE_MODIFIABLE_H

#include <optional>
#include "computation/context.H"

class maybe_modifiable
{
    std::optional<int> modifiable_reg;
    std::optional<expression_ref> value;
public:
    expression_ref get_value(const context* C) const
    {
        if (is_const())
            return *value;
        else
            return C->get_modifiable_value(*modifiable_reg);
    }
    void set_value(context* C, const expression_ref& v) { assert(not is_const()); C->set_modifiable_value(*modifiable_reg,v);}
    std::optional<expression_ref> is_const() const {return value;}
    maybe_modifiable(int p):modifiable_reg(p) {}
    maybe_modifiable(const expression_ref& v):value(v) {}
};

maybe_modifiable get_maybe_modifiable(const expression_ref& E);

#endif
