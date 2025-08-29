#ifndef APPLY_H
#define APPLY_H

#include "expression_ref.H"
#include "var.H"

// Single-arg version  (Is this a function at all?)";
expression_ref apply(const expression_ref& E,const expression_ref& arg);

// Multiple-arg version
expression_ref apply(const expression_ref& E, const std::vector< expression_ref >& args);

// Multiple-arg version
expression_ref apply(const expression_ref& E, const std::vector< var >& args);

// Single-arg version  (Is this a function at all?)";
expression_ref apply_expression(const expression_ref& E,const expression_ref& arg);

// Multiple-arg version
expression_ref apply_expression(const expression_ref& E, const std::vector< expression_ref >& args);

// Multiple-arg version
expression_ref apply_expression(const expression_ref& E, const std::vector< var >& args);

inline bool is_apply(const expression_ref& E)
{
    return E.type() == type_constant::apply_type;
}


inline bool is_apply_exp(const expression_ref& E)
{
    if (is_apply(E.head()))
    {
	assert(E.size() > 0);
	return true;
    }
    else
	return false;
}



#endif
