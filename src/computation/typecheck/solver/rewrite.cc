#include "computation/typecheck/typecheck.H"
#include "computation/typecheck/kindcheck.H"
#include "computation/typecheck/solver.H"

#include "util/variant.H"

#include <range/v3/all.hpp>

namespace views = ranges::views;

using std::vector;
using std::string;

vector<Type> Solver::rewrite(ConstraintFlavor flavor, vector<Type> types)
{
    for(auto& type: types)
        type = rewrite(flavor, type);
    return types;
}

Type Solver::rewrite_mtv(ConstraintFlavor flavor, const MetaTypeVar& mtv) const
{
    for(auto& inert: inerts.mtv_eqs)
    {
        // Don't allow wanteds to rewrite givens
        if (inert.flavor() == Wanted and flavor == Given) continue;

        auto eq = to<CanonicalEquality>(inert);
        assert(eq);
        if (eq->role != Role::Nominal) continue;

        auto uv1 = follow_meta_type_var(eq->t1).to<MetaTypeVar>();
        assert(uv1);

        if (mtv == *uv1) return eq->t2;
    }

    return mtv;
}

Type Solver::rewrite_tv(ConstraintFlavor flavor, const TypeVar& tv) const
{
    for(auto& inert: inerts.tv_eqs)
    {
        // Don't allow wanteds to rewrite givens
        if (inert.flavor() == Wanted and flavor == Given) continue;

        auto eq = to<CanonicalEquality>(inert);
        assert(eq);
        if (eq->role != Role::Nominal) continue;

        auto tv1 = eq->t1.to<TypeVar>();
        assert(tv1);

        if (tv == *tv1) return eq->t2;
    }

    return tv;
}

Type Solver::rewrite_constrained_type(ConstraintFlavor flavor, const ConstrainedType& C)
{
    auto C2 = C;
    C2.context = rewrite(flavor, C.context);
    C2.type = rewrite(flavor, C.type);
    return C2;
}

Type Solver::rewrite_forall(ConstraintFlavor flavor, const ForallType& forall)
{
    ForallType forall2 = forall;
    forall2.type = rewrite(flavor, forall.type);
    return forall2;
}

std::optional<Type> Solver::reduce_open_type_family(const TypeCon& family, const vector<Type>& target_args)
{
    std::optional<Type> result;

    vector<const EqInstanceEnv*> eq_instance_envs({&this_mod().local_eq_instances});
    for(auto& [_, mod]: this_mod().transitively_imported_modules)
        eq_instance_envs.push_back(&mod->local_eq_instances());

    for(auto eq_instance_env: eq_instance_envs)
    {
        for(auto& [_, info_]: *eq_instance_env)
        {
            if (info_.equation.family != family) continue;
            auto equation = freshen(info_.equation);
            if (auto subst = maybe_match(equation.lhs_args, target_args))
            {
                auto rhs = apply_subst(*subst, equation.rhs);
                if (result)
                    assert(same_type(*result, rhs));
                else
                    result = rhs;
            }
        }
    }

    return result;
}

std::optional<Type> Solver::reduce_closed_type_family(const TypeCon& family,
                                                       const ClosedTypeFamInfo& closed,
                                                       const vector<Type>& target_args)
{
    for(int i = 0; i < closed.equations.size(); i++)
    {
        auto candidate = freshen(closed.equations[i].equation);
        assert(candidate.family == family);

        auto subst = maybe_match(candidate.lhs_args, target_args);
        if (not subst) continue;

        bool blocked = false;
        for(auto predecessor_index: closed.equations[i].incompatible_predecessors)
        {
            assert(predecessor_index < i);
            auto predecessor = freshen(closed.equations[predecessor_index].equation);
            if (apartness(target_args, predecessor.lhs_args) != Apartness::SurelyApart)
            {
                blocked = true;
                break;
            }
        }

        if (not blocked)
            return apply_subst(*subst, candidate.rhs);
    }

    return {};
}

std::optional<Type> Solver::reduce_type_family_app(const TypeCon& family, const vector<Type>& args)
{
    auto info = info_for_type_fam(family.name);
    assert(info);

    if (info->is_open())
        return reduce_open_type_family(family, args);

    assert(info->closed_family);
    return reduce_closed_type_family(family, *info->closed_family, args);
}

Type Solver::rewrite_type_con_app(ConstraintFlavor flavor, const TypeCon& tc, const vector<Type>& args)
{
    Type t = type_apply(tc, rewrite(flavor, args));
    if (auto t2 = expand_type_synonym(t))
        return rewrite(flavor, *t2);
    else if (auto tfam = is_type_fam_app(t))
    {
        auto& [family, family_args] = *tfam;
        if (auto rhs = reduce_type_family_app(family, family_args))
            return rewrite(flavor, *rhs);

        for(auto& inert: inerts.tyfam_eqs)
        {
            // Don't allow wanteds to rewrite givens
            if (inert.flavor() == Wanted and flavor == Given) continue;

            auto eq = to<CanonicalEquality>(inert);
            assert(eq);
            if (eq->role != Role::Nominal) continue;

            // FIXME: this doesn't handle forall types
            if (t == eq->t1) return eq->t2;
        }

        // What about top-level type family equalities?
        // Suppose we have (Result Normal ~ Double)?
        //   We might be able to solve that as an instance of  a top-level type fam instance.
        // How about Num (Result Normal)?
        //   Here we need to use the top-level type fam instance to rewrite (Result Normal) to Double.
    }

    return t;
}

Type Solver::rewrite_app(ConstraintFlavor flavor, const Type& fun, const Type& arg)
{
    Type t = TypeApp(rewrite(flavor, fun), rewrite(flavor, arg));

    if (auto tc = is_type_con_app(t))
    {
        auto& [tycon, args] = *tc;
        return rewrite_type_con_app(flavor, tycon, args);
    }
    else
        return t;
}

Type Solver::rewrite(ConstraintFlavor flavor, Type t)
{
    t = follow_meta_type_var(t);

    if (auto mtv = t.to<MetaTypeVar>())
    {
        return rewrite_mtv(flavor, *mtv);
    }
    else if (auto tv = t.to<TypeVar>())
    {
        return rewrite_tv(flavor, *tv);
    }
    else if (auto con = t.to<ConstrainedType>())
    {
        return rewrite_constrained_type(flavor, *con);
    }
    else if (auto forall = t.to<ForallType>())
    {
        return rewrite_forall(flavor, *forall);
    }
    else if (auto tc = is_type_con_app(t))
    {
        auto& [tycon, args] = *tc;
        return rewrite_type_con_app(flavor, tycon, args);
    }
    else if (auto app = t.to<TypeApp>())
        return rewrite_app(flavor, app->head, app->arg);
    else
        std::abort();
}
