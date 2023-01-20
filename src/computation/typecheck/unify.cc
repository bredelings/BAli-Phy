#include "typecheck.H"

using std::vector;

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
    else if (auto s = t.to<StrictType>())
        return occurs_check(tv, s->type);
    else if (auto l = t.to<LazyType>())
        return occurs_check(tv, l->type);
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
    else if (auto s = t.to<StrictType>())
        return occurs_check(tv, s->type);
    else if (auto l = t.to<LazyType>())
        return occurs_check(tv, l->type);
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

// It looks like GHC has two unifiers.
// * The "eager" one (in GHC/Tc/Utils.hs) takes a ConstraintOrigin, and doesn't do matching.
//   It returns a coercion.
// * The "pure" one (in GHC/Core/Unify.hs) takes a unifying-or-matching flag, but doesn't take a ConstraintOrigin.
//   It returns a Maybe Subst.
//   - A Subst maps from TyVars -> Types.
// 

void TypeChecker::unify_defer(const ConstraintOrigin& origin, const Type& t1, const Type& t2)
{
    add_wanted(origin, make_equality_pred(t1, t2));
}

// Is there a better way to implement this?
void TypeChecker::unify_solve_(const ConstraintOrigin& origin, const Type& t1, const Type& t2)
{
    if (auto tt1 = filled_meta_type_var(t1))
        unify_solve_(origin, *tt1, t2);
    else if (auto tt2 = filled_meta_type_var(t2))
        unify_solve_(origin, t1, *tt2);

    else if (auto tv1 = t1.to<MetaTypeVar>())
    {
        if (tv1->level() == level() and not tv1->cycle_breaker and check_type_equality(t1,t2) == ok_result)
            tv1->fill(t2);
        else
            unify_defer(origin, t1, t2);
    }
    else if (auto tv2 = t2.to<MetaTypeVar>())
    {
        if (tv2->level() == level() and not tv2->cycle_breaker and check_type_equality(t2,t1) == ok_result)
            tv2->fill(t1);
        else
            unify_defer(origin, t2, t1);
    }
    else if (t1.is_a<TypeVar>() or t2.is_a<TypeVar>())
        unify_defer(origin, t1, t2);
    // Handle type synonyms AFTER variables, so that we preserve more type synonyms.
    else if (auto s1 = is_type_synonym(t1))
        unify_solve_(origin, *s1,  t2);
    else if (auto s2 = is_type_synonym(t2))
        unify_solve_(origin,  t1, *s2);

    auto app1 = is_type_app(t1);
    auto app2 = is_type_app(t2);
    if (app1 and app2)
    {
        auto& [fun1, arg1] = *app1;
        auto& [fun2, arg2] = *app2;

        unify_solve_(origin, fun1, fun2);
        unify_solve_(origin, arg1, arg2);
    }
    else if (t1.is_a<TypeCon>() and
             t2.is_a<TypeCon>() and
             t1.as_<TypeCon>() == t2.as_<TypeCon>())
    { }
    else if (same_type(t1, t2))
    { }
    else
        unify_defer(origin, t1, t2);
}

// Is there a better way to implement this?
bool TypeChecker::maybe_unify_(bool both_ways, const unification_env& env, const Type& t1, const Type& t2)
{
    // Translate rigid type variables
    if (auto tv1 = t1.to<TypeVar>(); tv1 and env.mapping1.count(*tv1))
    {
        assert(not tv1->is_skolem_constant());
        auto tv1_remapped = env.mapping1.at(*tv1);
        assert(tv1_remapped.is_skolem_constant());
        assert(not env.mapping1.count(tv1_remapped));
        return maybe_unify_(both_ways, env, tv1_remapped, t2);
    }
    else if (auto tv2 = t2.to<TypeVar>(); tv2 and env.mapping2.count(*tv2))
    {
        assert(not tv2->is_skolem_constant());
        auto tv2_remapped = env.mapping2.at(*tv2);
        assert(tv2_remapped.is_skolem_constant());
        assert(not env.mapping2.count(tv2_remapped));
        return maybe_unify_(both_ways, env, t1, tv2_remapped);
    }
    else if (auto tt1 = filled_meta_type_var(t1))
        return maybe_unify_(both_ways, env, *tt1, t2);
    else if (auto tt2 = filled_meta_type_var(t2))
        return maybe_unify_(both_ways, env, t1, *tt2);

    else if (auto s1 = is_type_synonym(t1))
        return maybe_unify_(both_ways, env, *s1,  t2);
    else if (auto s2 = is_type_synonym(t2))
        return maybe_unify_(both_ways, env,  t1, *s2);

    else if (auto tv1 = t1.to<MetaTypeVar>())
    {
        return try_insert(*tv1, t2);
    }
    else if (auto tv2 = t2.to<MetaTypeVar>(); tv2 and both_ways)
    {
        return try_insert(*tv2, t1);
    }
    else if (auto tv1 = t1.to<TypeVar>())
    {
        if (auto tv2 = t2.to<TypeVar>(); tv2 and *tv1 == *tv2)
            return true;
        else
            return false;
    }
    else if (auto tv2 = t2.to<TypeVar>())
    {
        if (auto tv1 = t1.to<TypeVar>(); tv1 and *tv1 == *tv2)
            return true;
        else
            return false;
    }
    else if (t1.is_a<TypeApp>() and t2.is_a<TypeApp>())
    {
        auto& app1 = t1.as_<TypeApp>();
        auto& app2 = t2.as_<TypeApp>();

        return maybe_unify_(both_ways, env, app1.head, app2.head) and
               maybe_unify_(both_ways, env, app1.arg , app2.arg );
    }
    else if (t1.is_a<TypeCon>() and
             t2.is_a<TypeCon>() and
             t1.as_<TypeCon>() == t2.as_<TypeCon>())
    {
        return true;
    }
    else if (t1.is_a<ConstrainedType>() and t2.is_a<ConstrainedType>())
    {
        auto c1 = t1.to<ConstrainedType>();
        auto c2 = t2.to<ConstrainedType>();
        if (c1->context.constraints.size() != c2->context.constraints.size())
            return false;
        for(int i=0;i< c1->context.constraints.size();i++)
            if (not maybe_unify_(both_ways, env, c1->context.constraints[i], c2->context.constraints[i]))
                return false;
        return maybe_unify_(both_ways, env, c1->type, c2->type);
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

        return maybe_unify_(both_ways, env2, fa1->type, fa2->type);
    }
    else if (t1.is_a<StrictType>() or t1.is_a<LazyType>() or t2.is_a<StrictType>() or t2.is_a<LazyType>())
    {
        throw myexception()<<"maybe_unify "<<t1.print()<<" ~ "<<t2.print()<<": How should we handle unification for strict/lazy types?";
    }
    else
        return false;
}

bool TypeChecker::same_type_no_syns(const Type& t1, const Type& t2) const
{
    return same_type(true, t1, t2);
}

bool TypeChecker::same_type(const Type& t1, const Type& t2) const
{
    return same_type(false, t1, t2);
}

bool TypeChecker::same_type(bool keep_syns, const Type& t1, const Type& t2) const
{
    return same_type(keep_syns, {}, t1, t2);
}

bool TypeChecker::same_type(bool keep_syns, const RenameTyvarEnv2& env, const Type& t1, const Type& t2)const
{
    // 1. Follow filled meta type vars
    if (auto t1_new = filled_meta_type_var(t1))
        return same_type(keep_syns, env, *t1_new, t2);

    if (auto t2_new = filled_meta_type_var(t2))
        return same_type(keep_syns, env, t1, *t2_new);

    // 2. Handle plain type cons
    auto tc1 = t1.to<TypeCon>();
    auto tc2 = t2.to<TypeCon>();
    if (tc1 and tc2)
        return (*tc1 == *tc2);

    // 3. Maybe follow type synonyms
    if (auto ts1 = is_type_synonym(t1); ts1 and not keep_syns)
        return same_type(keep_syns, env, *ts1, t2);

    if (auto ts2 = is_type_synonym(t2); ts2 and not keep_syns)
        return same_type(keep_syns, env, t1, *ts2);

    // 4. Handle Type vars
    auto tv1 = t1.to<TypeVar>();
    auto tv2 = t2.to<TypeVar>();
    if (tv1 and tv2)
    {
        return env.map_left(*tv1) == env.map_right(*tv2);
    }

    // 5. Handle unfilled meta type vars
    auto mtv1 = t1.to<MetaTypeVar>();
    auto mtv2 = t2.to<MetaTypeVar>();
    if (mtv1 and mtv2)
        return *mtv1 == *mtv2;

    // 6. Handle forall types
    // Change the forall to one binder at a time!
    auto forall1 = t1.to<ForallType>();
    auto forall2 = t2.to<ForallType>();
    if (forall1 and forall2)
    {
        if (forall1->type_var_binders.size() != forall2->type_var_binders.size()) return false;

        auto env2 = rename_binders2(env, forall1->type_var_binders, forall2->type_var_binders);
        return same_type(keep_syns, env2, forall1->type, forall2->type);
    }

    // 7. Handle constrained types
    // Change the constrained type to be a special function type?
    auto c1 = t1.to<ConstrainedType>();
    auto c2 = t2.to<ConstrainedType>();
    if (c1 and c2)
    {
        if (c1->context.constraints.size() != c2->context.constraints.size()) return false;

        return same_types(keep_syns, env, c1->context.constraints, c2->context.constraints) and same_type(keep_syns, env, c1->type, c2->type);
    }

    // 8. Handle type apps
    auto app1 = t1.to<TypeApp>();
    auto app2 = t2.to<TypeApp>();
    if (app1 and app2)
        return same_type(keep_syns, env, app1->head, app2->head)
            and same_type(keep_syns, env, app1->arg, app2->arg);

    // 9. Handle Strict types
    auto s1 = t1.to<StrictType>();
    auto s2 = t2.to<StrictType>();
    if (s1 and s2)
        return same_type(keep_syns, env, s1->type, s2->type);

    // 10. Handle Strict types
    auto l1 = t1.to<LazyType>();
    auto l2 = t2.to<LazyType>();
    if (l1 and l2)
        return same_type(keep_syns, env, l1->type, l2->type);

    return false;
}

bool TypeChecker::same_types(bool keep_syns, const vector<Type>& ts1, const vector<Type>& ts2) const
{
    if (ts1.size() != ts2.size()) return false;

    for(int i=0;i<ts1.size();i++)
        if (not same_type(keep_syns, ts1[i], ts2[i])) return false;

    return true;
}

bool TypeChecker::same_types(bool keep_syns, const RenameTyvarEnv2& env, const vector<Type>& ts1, const vector<Type>& ts2) const
{
    if (ts1.size() != ts2.size()) return false;

    for(int i=0;i<ts1.size();i++)
        if (not same_type(keep_syns, env, ts1[i], ts2[i])) return false;

    return true;
}

