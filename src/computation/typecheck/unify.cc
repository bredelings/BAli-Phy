#include "typecheck.H"
#include "util/variant.H"

using std::vector;

bool same_forall_binder_kinds(const vector<TypeVar>& tvs1, const vector<TypeVar>& tvs2)
{
    if (tvs1.size() != tvs2.size())
        return false;

    for(int i=0; i<tvs1.size(); i++)
        if (tvs1[i].kind != tvs2[i].kind)
            return false;

    return true;
}

// Open two forall types by replacing corresponding binders with shared fresh variables.
std::optional<std::pair<Type, Type>> TypeChecker::open_forall_pair(const ForallType& forall1, const ForallType& forall2)
{
    if (not same_forall_binder_kinds(forall1.type_var_binders, forall2.type_var_binders))
        return {};

    substitution_t subst1;
    substitution_t subst2;
    for(int i=0; i<forall1.type_var_binders.size(); i++)
    {
        auto tv1 = forall1.type_var_binders[i];
        auto tv2 = forall2.type_var_binders[i];
        auto fresh_tv = fresh_rigid_type_var(tv1.name, tv1.kind);
        subst1 = subst1.insert({tv1, fresh_tv});
        subst2 = subst2.insert({tv2, fresh_tv});
    }

    return std::pair(apply_subst(subst1, forall1.type), apply_subst(subst2, forall2.type));
}

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
    else if (auto syn = expand_type_synonym(t))
        return occurs_check(tv,*syn);
    else if (auto p_app = t.to<TypeApp>())
        return occurs_check(tv, p_app->head) or occurs_check(tv, p_app->arg);
    else if (auto f = t.to<ForallType>())
        return occurs_check(tv, f->type);
    else if (auto c = t.to<ConstrainedType>())
    {
        // The context may not contain vars that don't occur in the head;
        for(auto& constraint: c->context)
            if (occurs_check(tv, constraint))
                return true;

        return occurs_check(tv, c->type);
    }
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
    else if (auto syn = expand_type_synonym(t))
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
        for(auto& constraint: c->context)
            if (occurs_check(tv, constraint))
                return true;

        return occurs_check(tv, c->type);
    }
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
    else if (auto s1 = expand_type_synonym(t1))
        unify_solve_(origin, *s1,  t2);
    else if (auto s2 = expand_type_synonym(t2))
        unify_solve_(origin,  t1, *s2);
    else
    {
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
}

namespace
{
void extend_unifier_substitution(bsubstitution_t& substitution,
                                 const std::variant<TypeVar,MetaTypeVar>& variable,
                                 const Type& replacement)
{
    assert(not substitution.count(variable));

    bsubstitution_t extension;
    extension = extension.insert({variable, replacement});

    // Compose the new binding after the existing substitution so that no
    // existing range mentions the newly solved variable.  The resulting
    // substitution is idempotent and can be applied simultaneously.
    bsubstitution_t composed = extension;
    for(const auto& [old_variable, old_replacement]: substitution)
        composed = composed.insert({old_variable,
                                    apply_subst(extension, old_replacement)});

    substitution = std::move(composed);
}
}

bool TypeChecker::maybe_unify_var_(bool both_ways, const unification_env& env, const std::variant<TypeVar,MetaTypeVar>& btv1, const Type& t2, bsubstitution_t& s) const
{
    // translate using env

    // Handle if tv1 is bound
    if (auto type1 = s.find(btv1))
    {
        if (both_ways)
            return maybe_unify_(both_ways, env, *type1, t2, s);
        else
            return same_type(*type1, t2);
    }

    if (auto s2 = expand_type_synonym(t2))
        return maybe_unify_var_(both_ways, env, btv1, *s2, s);

    // Keep the returned substitution idempotent.  This also resolves solved
    // variables occurring beneath a type constructor before the occurs check.
    if (auto resolved_t2 = check_apply_subst(s, t2))
        return maybe_unify_var_(both_ways, env, btv1, *resolved_t2, s);

    if (auto tv1 = to<TypeVar>(btv1))
    {
        if (auto tv2 = t2.to<TypeVar>())
        {
            assert(not env.mapping2.count(*tv2));

            if (*tv1 == *tv2) return true;

            if (auto type2 = s.find(*tv2))
                return maybe_unify_var_(both_ways, env, *tv1, *type2, s);
        }

        assert(not s.count(*tv1));

        // avoid unifying (forall a. b) ~ (forall a. [a])
        // we cannot unify b := [a] because a is local

        if (occurs_check(*tv1, t2)) return false;
    }

    if (auto mtv1 = to<MetaTypeVar>(btv1))
    {
        if (auto mtv2 = t2.to<MetaTypeVar>())
        {
            if (*mtv1 == *mtv2) return true;

            if (auto type2 = s.find(*mtv2))
                return maybe_unify_var_(both_ways, env, *mtv1, *type2, s);
        }

        assert(not s.count(*mtv1));

        // avoid unifying (forall a. b) ~ (forall a. [a])
        // we cannot unify b := [a] because a is local

        if (occurs_check(*mtv1, t2)) return false;
    }

    // ghc does not do an occurs check when matching
    extend_unifier_substitution(s, btv1, t2);

    return true;
}

// Is there a better way to implement this?
bool TypeChecker::maybe_unify_(bool both_ways, const unification_env& env, const Type& t1, const Type& t2, bsubstitution_t& s) const
{
    // Translate rigid type variables
    if (auto tv1 = t1.to<TypeVar>(); tv1 and env.mapping1.count(*tv1))
    {
        assert(not tv1->is_skolem_constant());
        auto tv1_remapped = env.mapping1.at(*tv1);
        assert(tv1_remapped.is_skolem_constant());
        assert(not env.mapping1.count(tv1_remapped));
        return maybe_unify_(both_ways, env, tv1_remapped, t2, s);
    }
    else if (auto tv2 = t2.to<TypeVar>(); tv2 and env.mapping2.count(*tv2))
    {
        assert(not tv2->is_skolem_constant());
        auto tv2_remapped = env.mapping2.at(*tv2);
        assert(tv2_remapped.is_skolem_constant());
        assert(not env.mapping2.count(tv2_remapped));
        return maybe_unify_(both_ways, env, t1, tv2_remapped, s);
    }
    else if (auto tt1 = filled_meta_type_var(t1))
        return maybe_unify_(both_ways, env, *tt1, t2, s);
    else if (auto tt2 = filled_meta_type_var(t2))
        return maybe_unify_(both_ways, env, t1, *tt2, s);

    else if (auto s1 = expand_type_synonym(t1))
        return maybe_unify_(both_ways, env, *s1,  t2, s);
    else if (auto s2 = expand_type_synonym(t2))
        return maybe_unify_(both_ways, env,  t1, *s2, s);

    else if (auto mtv1 = t1.to<MetaTypeVar>())
    {
        if (auto tv2 = t2.to<TypeVar>(); tv2 and *tv1 == *tv2)
            return true;

        return maybe_unify_var_(both_ways, env, *mtv1, t2, s);
    }
    else if (auto tv1 = t1.to<TypeVar>())
    {
        if (auto tv2 = t2.to<TypeVar>(); tv2 and *tv1 == *tv2)
            return true;

        return maybe_unify_var_(both_ways, env, *tv1, t2, s);
    }
    else if (auto tv2 = t2.to<TypeVar>(); tv2 and both_ways)
    {
        auto env2 = env;
        std::swap(env2.mapping1, env2.mapping2);
        return maybe_unify_var_(both_ways, env2, *tv2, t1, s);
    }
    else if (t1.is_a<TypeApp>() and t2.is_a<TypeApp>())
    {
        auto& app1 = t1.as_<TypeApp>();
        auto& app2 = t2.as_<TypeApp>();

        return maybe_unify_(both_ways, env, app1.head, app2.head, s) and
               maybe_unify_(both_ways, env, app1.arg , app2.arg, s);
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
        if (c1->context.size() != c2->context.size())
            return false;
        for(int i=0;i< c1->context.size();i++)
            if (not maybe_unify_(both_ways, env, c1->context[i], c2->context[i], s))
                return false;
        return maybe_unify_(both_ways, env, c1->type, c2->type, s);
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

        return maybe_unify_(both_ways, env2, fa1->type, fa2->type, s);
    }
    else
        return false;
}

Apartness combine_apartness(Apartness a1, Apartness a2)
{
    if (a1 == Apartness::SurelyApart or a2 == Apartness::SurelyApart)
        return Apartness::SurelyApart;
    else if (a1 == Apartness::MaybeApart or a2 == Apartness::MaybeApart)
        return Apartness::MaybeApart;
    else
        return Apartness::Unifiable;
}

Apartness TypeChecker::apartness(const Type& t1, const Type& t2) const
{
    // This is intentionally conservative.  It is used to decide whether data
    // family instance heads are disjoint, so MaybeApart is treated as an error
    // by the caller.
    if (maybe_unify(t1, t2))
        return Apartness::Unifiable;

    if (auto tt1 = filled_meta_type_var(t1))
        return apartness(*tt1, t2);
    else if (auto tt2 = filled_meta_type_var(t2))
        return apartness(t1, *tt2);

    if (auto s1 = expand_type_synonym(t1))
        return apartness(*s1, t2);
    else if (auto s2 = expand_type_synonym(t2))
        return apartness(t1, *s2);

    // A type family application may later reduce to the other type, so failure
    // to unify now is not proof of apartness.
    if (is_type_fam_app(t1) or is_type_fam_app(t2))
        return Apartness::MaybeApart;

    if (auto tv1 = t1.to<TypeVar>())
        return occurs_check(*tv1, t2) ? Apartness::MaybeApart : Apartness::Unifiable;
    else if (auto tv2 = t2.to<TypeVar>())
        return occurs_check(*tv2, t1) ? Apartness::MaybeApart : Apartness::Unifiable;
    else if (auto mtv1 = t1.to<MetaTypeVar>())
        return occurs_check(*mtv1, t2) ? Apartness::MaybeApart : Apartness::Unifiable;
    else if (auto mtv2 = t2.to<MetaTypeVar>())
        return occurs_check(*mtv2, t1) ? Apartness::MaybeApart : Apartness::Unifiable;

    if (auto tc1 = t1.to<TypeCon>(); tc1 and t2.is_a<TypeCon>())
        return Apartness::SurelyApart;

    if (auto app1 = t1.to<TypeApp>(); app1 and t2.is_a<TypeApp>())
    {
        auto& app2 = t2.as_<TypeApp>();
        return combine_apartness(apartness(app1->head, app2.head),
                                 apartness(app1->arg, app2.arg));
    }

    if (auto c1 = t1.to<ConstrainedType>(); c1 and t2.is_a<ConstrainedType>())
    {
        auto& c2 = t2.as_<ConstrainedType>();
        if (c1->context.size() != c2.context.size())
            return Apartness::SurelyApart;

        auto result = Apartness::Unifiable;
        for(int i=0;i<c1->context.size();i++)
            result = combine_apartness(result, apartness(c1->context[i], c2.context[i]));
        return combine_apartness(result, apartness(c1->type, c2.type));
    }

    if (t1.is_a<ForallType>() or t2.is_a<ForallType>())
        return Apartness::MaybeApart;

    return Apartness::SurelyApart;
}

Apartness TypeChecker::apartness(const vector<Type>& t1, const vector<Type>& t2) const
{
    if (t1.size() != t2.size()) return Apartness::SurelyApart;
    if (maybe_unify(t1, t2)) return Apartness::Unifiable;

    auto result = Apartness::Unifiable;
    for(int i = 0; i < t1.size(); i++)
        result = combine_apartness(result, apartness(t1[i], t2[i]));

    if (result != Apartness::Unifiable)
        return result;

    // Shared variables can make a vector pair apart even when every position
    // is independently unifiable, for example (a,a) and (Int,Bool).  A ground
    // side makes that failure definitive.  Otherwise remain conservative.
    bool t1_ground = free_type_variables(t1).empty() and free_meta_type_variables(t1).empty();
    bool t2_ground = free_type_variables(t2).empty() and free_meta_type_variables(t2).empty();
    if (t1_ground or t2_ground)
        return Apartness::SurelyApart;

    return Apartness::MaybeApart;
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
    if (auto ts1 = expand_type_synonym(t1); ts1 and not keep_syns)
        return same_type(keep_syns, env, *ts1, t2);

    if (auto ts2 = expand_type_synonym(t2); ts2 and not keep_syns)
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
        if (not same_forall_binder_kinds(forall1->type_var_binders, forall2->type_var_binders)) return false;

        auto env2 = rename_binders2(env, forall1->type_var_binders, forall2->type_var_binders);
        return same_type(keep_syns, env2, forall1->type, forall2->type);
    }

    // 7. Handle constrained types
    // Change the constrained type to be a special function type?
    auto c1 = t1.to<ConstrainedType>();
    auto c2 = t2.to<ConstrainedType>();
    if (c1 and c2)
    {
        if (c1->context.size() != c2->context.size()) return false;

        return same_types(keep_syns, env, c1->context, c2->context) and same_type(keep_syns, env, c1->type, c2->type);
    }

    // 8. Handle type apps
    auto app1 = t1.to<TypeApp>();
    auto app2 = t2.to<TypeApp>();
    if (app1 and app2)
        return same_type(keep_syns, env, app1->head, app2->head)
            and same_type(keep_syns, env, app1->arg, app2->arg);

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
