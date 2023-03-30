#include "instance_info.H"

Type InstanceInfo::type() const
{
    return add_forall_vars(tvs, add_constraints(constraints, type_apply(class_con, args)));
}

Type EqInstanceInfo::type() const
{
    return add_forall_vars(tvs, make_equality_pred(lhs, rhs));
}
