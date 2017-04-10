#ifndef APPLY_H
#define APPLY_H

#include "expression_ref.H"

// Single-arg version  (Is this a function at all?)";
expression_ref apply(const expression_ref& E,const expression_ref& arg);

// Multiple-arg version
expression_ref apply(const expression_ref& E, const std::vector< expression_ref > args);

// Single-arg version  (Is this a function at all?)";
expression_ref apply_expression(const expression_ref& E,const expression_ref& arg);

// Multiple-arg version
expression_ref apply_expression(const expression_ref& E, const std::vector< expression_ref > args);

inline bool is_apply(const expression_ref& E)
{
    return E.type() == apply_type;
}

#endif
