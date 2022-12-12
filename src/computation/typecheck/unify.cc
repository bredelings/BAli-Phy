#include "typecheck.H"

bool TypeChecker::occurs_check(const MetaTypeVar& tv, const Type& t) const
{
    assert(not tv.filled());

    if (auto tt = filled_meta_type_var(t))
        return occurs_check(tv, *tt);
    else if (auto x = t.to<MetaTypeVar>())
        return tv == *x;
    else if (t.is_a<TypeVar>())
        return false;
    else if (t.is_a<TypeCon>())
        return false;
    else if (auto syn = is_type_synonym(t))
        return occurs_check(tv,*syn);
    else if (auto tup = t.to<TupleType>())
    {
        for(auto& type: tup->element_types)
            if (occurs_check(tv, type))
                return true;
        return false;
    }
    else if (auto l = t.to<ListType>())
        return occurs_check(tv, l->element_type);
    else if (auto p_app = t.to<TypeApp>())
        return occurs_check(tv, p_app->head) or occurs_check(tv, p_app->arg);
    else if (auto f = t.to<ForallType>())
        return occurs_check(tv, f->type);
    else if (auto c = t.to<ConstrainedType>())
    {
        // The context may not contain vars that don't occur in the head;
        for(auto& constraint: c->context.constraints)
            if (occurs_check(tv, constraint))
                return true;

        return occurs_check(tv, c->type);
    }
    else if (auto sl = t.to<StrictLazyType>())
        return occurs_check(tv, sl->type);
    else
        std::abort();
}

bool TypeChecker::occurs_check(const TypeVar& tv, const Type& t) const
{
    if (auto tt = filled_meta_type_var(t))
        return occurs_check(tv, *tt);
    else if (t.is_a<MetaTypeVar>())
        return false;
    else if (auto x = t.to<TypeVar>())
        return tv == *x;
    else if (t.is_a<TypeCon>())
        return false;
    else if (auto syn = is_type_synonym(t))
        return occurs_check(tv,*syn);
    else if (auto tup = t.to<TupleType>())
    {
        for(auto& type: tup->element_types)
            if (occurs_check(tv, type))
                return true;
        return false;
    }
    else if (auto l = t.to<ListType>())
        return occurs_check(tv, l->element_type);
    else if (auto p_app = t.to<TypeApp>())
        return occurs_check(tv, p_app->head) or occurs_check(tv, p_app->arg);
    else if (auto f = t.to<ForallType>())
    {
        for(auto & qv: f->type_var_binders)
            if (qv == tv)
                return false;

        return occurs_check(tv, f->type);
    }
    else if (auto c = t.to<ConstrainedType>())
    {
        // The context may not contain vars that don't occur in the head;
        for(auto& constraint: c->context.constraints)
            if (occurs_check(tv, constraint))
                return true;

        return occurs_check(tv, c->type);
    }
    else if (auto sl = t.to<StrictLazyType>())
        return occurs_check(tv, sl->type);
    else
        std::abort();
}

bool TypeChecker::try_insert(const MetaTypeVar& tv, Type type) const
{
    // 1. We can't insert tv ~ type if we already have a substitution for tv.
    assert(not tv.filled());

    // 2. We can only bind meta type vars to tau types.
    assert(is_tau_type(type));

    // 3. Walk any meta-type-var indirections
    auto safe_type = type;
    while(auto t2 = filled_meta_type_var(safe_type))
        safe_type = *t2;

    // 4. tv ~ tv is already true, so in that case return success without doing anything.
    if (auto tv2 = safe_type.to<MetaTypeVar>(); tv2 and *tv2 == tv)
        return true;

    // 5. If safe_type contains tv, then we have a substitution loop for tv.
    //    Therefore return failure.  (This rules out infinite types.)
    if (occurs_check(tv, safe_type)) return false;

    // 6. It is safe to add tv -> safe_type
    tv.fill(type);

    return true;
}

// Is there a better way to implement this?
bool TypeChecker::maybe_unify_(bool eager_unification, bool both_ways, const unification_env& env, const Type& t1, const Type& t2)
{
    // Translate rigid type variables
    if (auto tv1 = t1.to<TypeVar>(); tv1 and env.mapping1.count(*tv1))
    {
        assert(not tv1->is_skolem_constant());
        auto tv1_remapped = env.mapping1.at(*tv1);
        assert(tv1_remapped.is_skolem_constant());
        assert(not env.mapping1.count(tv1_remapped));
        return maybe_unify_(eager_unification, both_ways, env, tv1_remapped, t2);
    }
    else if (auto tv2 = t2.to<TypeVar>(); tv2 and env.mapping2.count(*tv2))
    {
        assert(not tv2->is_skolem_constant());
        auto tv2_remapped = env.mapping2.at(*tv2);
        assert(tv2_remapped.is_skolem_constant());
        assert(not env.mapping2.count(tv2_remapped));
        return maybe_unify_(eager_unification, both_ways, env, t1, tv2_remapped);
    }
    else if (auto tt1 = filled_meta_type_var(t1))
        return maybe_unify_(eager_unification, both_ways, env, *tt1, t2);
    else if (auto tt2 = filled_meta_type_var(t2))
        return maybe_unify_(eager_unification, both_ways, env, t1, *tt2);

    else if (auto s1 = is_type_synonym(t1))
        return maybe_unify_(eager_unification, both_ways, env, *s1,  t2);
    else if (auto s2 = is_type_synonym(t2))
        return maybe_unify_(eager_unification, both_ways, env,  t1, *s2);

    else if (auto tv1 = t1.to<MetaTypeVar>())
    {
        if (eager_unification)
            return try_insert(*tv1, t2);
        else
        {
            add_dvar(make_equality_constraint(*tv1, t2));
            return true;
        }
    }
    else if (auto tv2 = t2.to<MetaTypeVar>(); tv2 and both_ways)
    {
        if (eager_unification)
            return try_insert(*tv2, t1);
        else
        {
            add_dvar(make_equality_constraint(*tv2, t1));
            return true;
        }
    }
    else if (auto tv1 = t1.to<TypeVar>())
    {
        if (eager_unification)
        {
            if (auto tv2 = t2.to<TypeVar>(); tv2 and *tv1 == *tv2)
                return true;
            else
                return false;
        }
        else
        {
            add_dvar(make_equality_constraint(t1, t2));
            return true;
        }
    }
    else if (auto tv2 = t2.to<TypeVar>())
    {
        if (eager_unification)
        {
            if (auto tv1 = t1.to<TypeVar>(); tv1 and *tv1 == *tv2)
                return true;
            else
                return false;
        }
        else
        {
            add_dvar(make_equality_constraint(t1, t2));
            return true;
        }
    }
    else if (t1.is_a<TypeApp>() and t2.is_a<TypeApp>())
    {
        auto& app1 = t1.as_<TypeApp>();
        auto& app2 = t2.as_<TypeApp>();

        return maybe_unify_(eager_unification, both_ways, env, app1.head, app2.head) and
               maybe_unify_(eager_unification, both_ways, env, app1.arg , app2.arg );
    }
    else if (t1.is_a<TypeCon>() and
             t2.is_a<TypeCon>() and
             t1.as_<TypeCon>() == t2.as_<TypeCon>())
    {
        return true;
    }
    else if (auto tup1 = t1.to<TupleType>())
    {
        return maybe_unify_(eager_unification, both_ways, env, canonicalize_type(*tup1), t2);
    }
    else if (auto tup2 = t2.to<TupleType>())
    {
        return maybe_unify_(eager_unification, both_ways, env, t1, canonicalize_type(*tup2));
    }
    else if (auto l1 = t1.to<ListType>())
    {
        return maybe_unify_(eager_unification, both_ways, env, canonicalize_type(*l1), t2);
    }
    else if (auto l2 = t2.to<ListType>())
    {
        return maybe_unify_(eager_unification, both_ways, env, t1, canonicalize_type(*l2));
    }
    else if (t1.is_a<ConstrainedType>() and t2.is_a<ConstrainedType>())
    {
        auto c1 = t1.to<ConstrainedType>();
        auto c2 = t2.to<ConstrainedType>();
        if (c1->context.constraints.size() != c2->context.constraints.size())
            return false;
        for(int i=0;i< c1->context.constraints.size();i++)
            if (not maybe_unify_(eager_unification, both_ways, env, c1->context.constraints[i], c2->context.constraints[i]))
                return false;
        return maybe_unify_(eager_unification, both_ways, env, c1->type, c2->type);
    }
    else if (t1.is_a<ForallType>() and t2.is_a<ForallType>())
    {
        auto fa1 = t1.to<ForallType>();
        auto fa2 = t2.to<ForallType>();

        if (fa1->type_var_binders.size() != fa2->type_var_binders.size())
            return false;

        auto env2 = env;
        for(int i=0;i < fa1->type_var_binders.size(); i++)
        {
            auto tv1 = fa1->type_var_binders[i];
            auto tv2 = fa2->type_var_binders[i];

            if (tv1.kind != tv2.kind) return false;

            auto v = env2.fresh_tyvar(tv1.kind);
            env2.mapping1 = env2.mapping1.insert({tv1,v});
            env2.mapping2 = env2.mapping2.insert({tv2,v});
        }

        return maybe_unify_(eager_unification, both_ways, env2, fa1->type, fa2->type);
    }
    else if (t1.is_a<StrictLazyType>() or t2.is_a<StrictLazyType>())
    {
        throw myexception()<<"maybe_unify "<<t1.print()<<" ~ "<<t2.print()<<": How should we handle unification for strict/lazy types?";
    }
    else
        return false;
}

bool TypeChecker::same_type(const Type& t1, const Type& t2) const
{
    if (auto type1 = filled_meta_type_var(t1))
        return same_type(*type1, t2);
    else if (auto type2 = filled_meta_type_var(t2))
        return same_type(t1, *type2);
    else if (t1.is_a<MetaTypeVar>())
        return (t1 == t2);
    else if (t1.is_a<TypeVar>())
        return (t1 == t2);
    else if (t1.is_a<TypeCon>())
        return (t1 == t2);
    else if (auto s1 = is_type_synonym(t1))
        return same_type(*s1, t2);
    else if (auto s2 = is_type_synonym(t2))
        return same_type(t1, *s2);
    else if (t1.is_a<TypeApp>() and t2.is_a<TypeApp>())
    {
        auto& app1 = t1.as_<TypeApp>();
        auto& app2 = t2.as_<TypeApp>();

        return same_type(app1.head, app2.head) and same_type(app1.arg, app2.arg);
    }
    else if (t1.is_a<TupleType>() and t2.is_a<TupleType>())
    {
        auto& tup1 = t1.as_<TupleType>();
        auto& tup2 = t2.as_<TupleType>();
        if (tup1.element_types.size() != tup2.element_types.size())
            return false;

        for(int i=0;i<tup1.element_types.size();i++)
            if (not same_type(tup1.element_types[i], tup2.element_types[i])) return false;

        return true;
    }
    else if (t1.is_a<ListType>() and t2.is_a<ListType>())
    {
        auto& L1 = t1.as_<ListType>();
        auto& L2 = t2.as_<ListType>();

        return same_type(L1.element_type, L2.element_type);
    }
    else if (t1.is_a<ForallType>() or t2.is_a<ForallType>())
    {
        throw myexception()<<"same_type "<<t1.print()<<" "<<t2.print()<<": How should we handle forall types?";
    }
    else if (t1.is_a<ConstrainedType>() or t2.is_a<ConstrainedType>())
    {
        throw myexception()<<"same_type "<<t1.print()<<" "<<t2.print()<<": How should we handle unification for constrained types?";
    }
    else if (t1.is_a<StrictLazyType>() or t2.is_a<StrictLazyType>())
    {
        throw myexception()<<"same_type "<<t1.print()<<" "<<t2.print()<<": How should we handle unification for strict/lazy types?";
    }
    else
        return false;
}


