#ifndef MAYBE_MODIFIABLE_H
#define MAYBE_MODIFIABLE_H

#include <optional>
#include "computation/context.H"

class maybe_modifiable
{
    std::optional<int> modifiable_reg;
    std::optional<expression_ref> value;
public:
    expression_ref get_value(const context* C) const;
    void set_value(context* C, const expression_ref& v);

    std::optional<expression_ref> constant_value() const {return value;}

    maybe_modifiable(int p):modifiable_reg(p) {}
    maybe_modifiable(const expression_ref& v):value(v) {}
};

maybe_modifiable get_maybe_modifiable(const expression_ref& E);

#endif
