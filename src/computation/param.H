#ifndef PARAM_H
#define PARAM_H

#include <optional>
#include "computation/expression/expression_ref.H"
#include "util/bounds.H"

class context;

class param
{
    std::optional<int> head;
    std::optional<expression_ref> value;

public:
    expression_ref get_expression(const context& C) const;
    expression_ref get_value(const context& C) const;
    void set_value(context& C, const expression_ref& v) const;

    std::optional<expression_ref> constant_value() const {return value;}
    std::optional<int> is_modifiable(const context& C) const;
    std::optional<int> is_random_variable(const context& C) const;
    std::optional<bounds<double>> has_bounds(const context& C) const;

    param(int h):head(h) {}
    param(const expression_ref& v):value(v) {}
};

param get_param(context& C, const expression_ref& E);

#endif
