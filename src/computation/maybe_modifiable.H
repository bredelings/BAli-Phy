#ifndef MAYBE_MODIFIABLE_H
#define MAYBE_MODIFIABLE_H

#include <optional>
#include "computation/context.H"
#include "util/bounds.H"

class maybe_modifiable
{
    std::optional<int> head;
    std::optional<expression_ref> value;
public:
    expression_ref get_expression(const context& C) const;
    expression_ref get_value(const context& C) const;
    void set_value(context& C, const expression_ref& v);

    std::optional<expression_ref> constant_value() const {return value;}
    std::optional<int> is_modifiable(const context& C) const;
    std::optional<int> is_random_variable(const context& C) const;
    std::optional<bounds<double>> has_bounds(const context& C) const;

    maybe_modifiable(int h):head(h) {}
    maybe_modifiable(const expression_ref& v):value(v) {}
};

maybe_modifiable get_maybe_modifiable(context& C, const expression_ref& E);

#endif
