#include "instance_info.H"

Type InstanceInfo::type() const
{
    return add_forall_vars(tvs, add_constraints(constraints, type_apply(class_con, args)));
}

Type TypeFamEqnInfo::lhs() const
{
    return type_apply(family, lhs_args);
}

Type TypeFamEqnInfo::type() const
{
    return add_forall_vars(quantified_tvs, make_equality_pred(lhs(), rhs));
}

Type EqInstanceInfo::type() const
{
    return equation.type();
}
