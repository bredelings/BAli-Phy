#include "computation/typecheck/typecheck.H"
#include "computation/typecheck/kindcheck.H"
#include "computation/typecheck/solver.H"

#include "util/variant.H"

using std::vector;

vector<Type> Solver::rewrite(ConstraintFlavor flavor, vector<Type> types) const
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

        auto tv1 = eq->t1.to<TypeVar>();
        assert(tv1);

        if (tv == *tv1) return eq->t2;
    }

    return tv;
}

Type Solver::rewrite_constrained_type(ConstraintFlavor flavor, const ConstrainedType& C) const
{
    auto C2 = C;
    C2.context.constraints = rewrite(flavor, C.context.constraints);
    C2.type = rewrite(flavor, C.type);
    return C2;
}

Type Solver::rewrite_forall(ConstraintFlavor flavor, const ForallType& forall) const
{
    ForallType forall2 = forall;
    forall2.type = rewrite(flavor, forall.type);
    return forall2;
}

Type Solver::rewrite_type_con_app(ConstraintFlavor flavor, const TypeCon& tc, const vector<Type>& args) const
{
    Type t = make_tyapps(tc, rewrite(flavor, args));
    if (auto t2 = expand_type_synonym(t))
        return rewrite(flavor, *t2);
    else if (auto tfam = is_type_fam_app(t))
    {
        for(auto& inert: inerts.tyfam_eqs)
        {
            // Don't allow wanteds to rewrite givens
            if (inert.flavor() == Wanted and flavor == Given) continue;

            auto eq = to<CanonicalEquality>(inert);
            assert(eq);

            // FIXME: this doesn't handle forall types
            if (t == eq->t1) return eq->t2;
        }
    }

    return t;
}

Type Solver::rewrite_app(ConstraintFlavor flavor, const Type& fun, const Type& arg) const
{
    return TypeApp(rewrite(flavor, fun), rewrite(flavor, arg));
}

Type Solver::rewrite(ConstraintFlavor flavor, Type t) const
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
    else if (auto st = t.to<StrictType>())
    {
        auto ST = *st;
        ST.type = rewrite(flavor, ST.type);
        return ST;
    }
    else if (auto lt = t.to<LazyType>())
    {
        auto LT = *lt;
        LT.type = rewrite(flavor, LT.type);
        return LT;
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

