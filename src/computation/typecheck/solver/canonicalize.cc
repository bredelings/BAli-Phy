#include "computation/typecheck/typecheck.H"
#include "computation/typecheck/kindcheck.H"
#include "computation/typecheck/solver.H"

#include "util/set.H"
#include "util/variant.H"

using std::vector;
using std::optional;
using std::tuple;

std::bitset<8> set_occurs_check_maybe(std::bitset<8> result)
{
    if (result.test(occurs_definitely_bit))
    {
        result.reset(occurs_definitely_bit);
        result.set(occurs_maybe_bit);
    }
    return result;
}

bool has_occurs_check(std::bitset<8> result)
{
    return result.test(occurs_definitely_bit) or result.test(occurs_maybe_bit);
}

std::bitset<8> Solver::check_type_equality(const Type& lhs, const Type& rhs) const
{
    if (auto tt = filled_meta_type_var(rhs))
        return check_type_equality(lhs, *tt);
    else if (auto mtv = rhs.to<MetaTypeVar>())
    {
        if (lhs == *mtv)
            return occurs_definitely_result;
        else
            return ok_result;
    }
    else if (auto tv = rhs.to<TypeVar>())
    {
        if (lhs == *tv)
            return occurs_definitely_result;
        else
            return ok_result;
    }
    else if (auto app = is_type_app(rhs))
    {
        auto& [fun,arg] = *app;
        return check_type_equality(lhs, fun) | check_type_equality(lhs, arg);
    }
    else if (auto tfam = is_type_fam_app(rhs))
    {
        if (same_type(lhs,rhs))
            return occurs_definitely_result;
        else
        {
            auto [_,args] = *tfam;
            auto result = type_family_result;
            for(auto& arg: args)
                result |= check_type_equality(lhs,arg);
            return set_occurs_check_maybe(result);
        }
    }
    // We can record that type synonyms either do or do not expand to have
    // (i) type families
    // (ii) foralls and constraints
    else if (auto tsyn = is_type_synonym(rhs))
    {
        return check_type_equality(lhs, *tsyn);
    }
    else if (rhs.is_a<TypeCon>())
    {
        return ok_result;
    }
    else if (auto app = is_type_app(rhs))
    {
        auto& [fun,arg] = *app;
        return check_type_equality(lhs, fun) | check_type_equality(lhs, arg);
    }
    else if (auto forall = rhs.to<ForallType>())
    {
        return check_type_equality(lhs, forall->type) | impredicative_result;
    }
    else if (auto con = rhs.to<ConstrainedType>())
    {
        auto result = check_type_equality(lhs, con->type) | impredicative_result;
        for(auto& constraint: con->context.constraints)
            result |= check_type_equality(lhs, constraint);
        return result;
    }
    else if (auto sl = rhs.to<StrictLazyType>())
    {
        return check_type_equality(lhs, sl->type);
    }
    else
        std::abort();
}

std::optional<Predicate>
Solver::canonicalize_equality_type_apps(const Constraint& C,
                                        const Type& fun1, const Type& arg1, const Type& fun2, const Type& arg2)
{
    auto fun_constraint = make_equality_pred(fun1, fun2);
    auto fun_dvar = fresh_dvar(fun_constraint);
    work_list.push_back(NonCanonical({C.origin, C.flavor, fun_dvar, fun_constraint, C.tc_state}));

    auto arg_constraint = make_equality_pred(arg1, arg2);
    auto arg_dvar = fresh_dvar(arg_constraint);
    work_list.push_back(NonCanonical({C.origin, C.flavor, arg_dvar, arg_constraint, C.tc_state}));

    return {};
}

std::optional<Predicate> Solver::canonicalize_equality_type_cons(const CanonicalEquality& P, const TypeCon& tc1, const vector<Type>& args1, const TypeCon& tc2, const vector<Type>& args2)
{
    assert(not type_con_is_type_fam(tc1));
    assert(not type_con_is_type_fam(tc2));
    assert(not type_con_is_type_syn(tc1));
    assert(not type_con_is_type_syn(tc2));

    // if tc1 and tc2 and both non-type-family type cons...
    if (tc1 == tc2 and args1.size() == args2.size())
    {
        // If we've gotten here, the heads are both injective, and might be equal.
        for(int i=0;i<args1.size();i++)
        {
            auto constraint = make_equality_pred(args1[i], args2[i]);
            auto dvar = fresh_dvar(constraint);
            work_list.push_back(NonCanonical({P.origin(), P.flavor(), dvar, constraint, P.constraint.tc_state}));
        }
    }
    else
        inerts.failed.push_back(P);

    return {};
}

bool Solver::is_rewritable_lhs(Type t) const
{
    t = follow_meta_type_var(t);

    if (t.is_a<MetaTypeVar>())
        return true;
    else if (t.is_a<TypeVar>())
        return true;
    else if (is_type_fam_app(t))
        return true;
    else
        return false;
}

int lhs_priority(const Type& t)
{
    if (t.is_a<TypeVar>()) return 0;
    else if (auto mtv = t.to<MetaTypeVar>())
    {
        if (mtv->cycle_breaker)
            return 0;
        else
            return 1;
    }
    else
        std::abort();
}

bool flip_type_vars(bool is_given, const Type& t1, const Type& t2)
{
    int p1 = lhs_priority(t1);
    int p2 = lhs_priority(t2);

    // 1. We want unification variables on the left in wanteds.
    if (not is_given)
    {
        if (p1 == 0 and p2 > 0) return true;
        if (p2 == 0 and p1 > 0) return false;
    }

    // 2. We want the deepest level variable on the left.
    // * for wanteds, we want touchable meta-variables on the left.
    // * for givens, we want to eliminate deeper skolems to avoid skolem-escape.

    int l1 = max_level(t1);
    int l2 = max_level(t2);

    if (l1 > l2) return false;
    if (l2 > l1) return true;

    // 3. Otherwise we leave things the way they are.
    //    This avoid infinite cycles of flips.

    return false;
}

std::optional<Predicate>
Solver::canonicalize_equality_var_tyfam(CanonicalEquality P)
{
    assert(P.t1.is_a<TypeVar>() or P.t1.is_a<MetaTypeVar>());
    assert(is_type_fam_app(P.t2));

    // If mtv1 -> tyfam is touchable and has no problems, do that.
    auto mtv1 = P.t1.to<MetaTypeVar>();
    if (mtv1 and is_touchable(*mtv1, P.t2))
    {
        auto result = check_type_equality(P.t1, P.t2) & ~type_family_result;
        if (result == ok_result)
            return canonicalize_equality_lhs1(P);
    }

    // Otherwise prefer tyfam -> var
    return canonicalize_equality_lhs1(P.flip());
}


std::optional<Predicate>
Solver::canonicalize_equality_lhs2(CanonicalEquality P)
{
    assert(is_rewritable_lhs(P.t1) and is_rewritable_lhs(P.t2));

    auto tfam1 = is_type_fam_app(P.t1);
    auto tfam2 = is_type_fam_app(P.t2);
    
    if (tfam1 and tfam2)
    {
        // If both are type fam apps, then
        // 1. If only one has metatypevars as its arguments, then put that one on the left
        // 2. If the lhs occurs on the rhs, but not vice versa, then we want to swap.
        //    For example F a ~ F (F a) should be swapped.

        // Case 2: If we have F a = F (F a), then we want to flip.
        bool flip_for_occurs = check_type_equality(P.t1, P.t2).test(occurs_definitely_bit)
            and not check_type_equality(P.t2, P.t1).test(occurs_definitely_bit);

        if (flip_for_occurs)
            return canonicalize_equality_lhs1(P.flip());
        else
            return canonicalize_equality_lhs1(P);
    }
    else if (tfam1)
        return canonicalize_equality_var_tyfam(P.flip());
    else if (tfam2)
        return canonicalize_equality_var_tyfam(P);
    else
    {
        if (flip_type_vars(P.flavor() == Given, P.t1, P.t2))
            return canonicalize_equality_lhs1(P.flip());
        else
            return canonicalize_equality_lhs1(P);
    }
}

Type Solver::break_type_equality_cycle(const Constraint& C, const Type& type)
{
    if (auto t = filled_meta_type_var(type))
        return break_type_equality_cycle(C, *t);
    else if (type.is_a<MetaTypeVar>())
        return type;
    else if (type.is_a<TypeVar>())
        return type;
    else if (auto app = is_type_app(type))
    {
        auto& [head, arg] = *app;
        return TypeApp(break_type_equality_cycle(C, head), break_type_equality_cycle(C, arg));
    }
    // FIXME!
    // We should mark type synonyms for whether they contains type fams.
    // Then we wouldn't have to look through them as much.
    else if (auto syn = is_type_synonym(type))
        return break_type_equality_cycle(C, *syn);
    else if (auto tfam = is_type_fam_app(type))
    {
        auto& [tc,args] = *tfam;

        // Get the kind for type
        auto kind = tycon_info().at(unloc(tc.name)).kind;
        for(int i=0;i<args.size();i++)
        {
            auto arrow = kind.to<KindArrow>();
            assert(arrow);
            kind = arrow->result_kind;
        }

        auto new_tv = fresh_meta_type_var("cbv", kind);
        if (C.flavor == Given)
        {
            new_tv.cycle_breaker = true;
            inerts.cycle_breakers.push_back({new_tv, type});
        }

        // TODO: Mark these as coming from a cycle-breaking operation.

        auto constraint = make_equality_pred(new_tv, type);
        auto dvar = fresh_dvar(constraint);
        work_list.push_back(NonCanonical({CycleBreakerOrigin(), C.flavor, dvar, constraint, C.tc_state}));

        return new_tv;
    }
    else if (type.is_a<TypeCon>())
        return type;
    else if (type.is_a<ForallType>()) // We can't fix type families under a forall?
        return type;
    else if (auto c = type.to<ConstrainedType>())
    {
        auto CT = *c;
        for(auto& constraint: CT.context.constraints)
            constraint = break_type_equality_cycle(C, constraint);
        CT.type = break_type_equality_cycle(C, CT.type);
        return CT;
    }
    else if (auto sl = type.to<StrictLazyType>())
    {
        auto SL = *sl;
        SL.type = break_type_equality_cycle(C, SL.type);
        return SL;
    }
    else
        std::abort();
}

std::optional<Type> Solver::maybe_break_type_equality_cycle(const CanonicalEquality& P, std::bitset<8> result)
{
    assert(result.any());

    // 1. Only do this if we have an occurs check under a type family application.
    if (result != occurs_maybe_result) return {};

    // 2. Don't do this if we have a wanted cosntraint without a touchable mtv
    bool should_break_cycle = true;
    if (P.flavor() == Wanted)
    {
        should_break_cycle = false;
        auto lhs = follow_meta_type_var(P.t1);
        if (auto mtv = lhs.to<MetaTypeVar>(); mtv and is_touchable(*mtv, P.t2))
            should_break_cycle = true;
    }

    if (not should_break_cycle) return {};

    // 3. Check that the equality does not come from a cycle breaking operation.
    if (to<CycleBreakerOrigin>(P.constraint.origin)) return {};

    // 4. Actually do the cycle breaking
    return break_type_equality_cycle(P.constraint, P.t2);
}

std::optional<Predicate> Solver::canonicalize_equality_lhs1(const CanonicalEquality& P)
{
    assert(is_rewritable_lhs(P.t1));

    auto result0 = check_type_equality(P.t1, P.t2);

    // Don't worry about type families here...
    auto result1 = result0;
    result1.reset(type_family_bit);
    result1.reset(impredicative_bit); // FIXME!

    if (result1 == ok_result)
        return P;

    if (auto new_rhs = maybe_break_type_equality_cycle(P, result1))
    {
        auto P2 = P;
        P2.t2 = *new_rhs;
        auto result2 = check_type_equality(P2.t1, P2.t2);

        if (has_occurs_check(result2))
        {
            inerts.failed.push_back(P);
            return {};
        }
        else
            return P2;
    }
    else if (has_occurs_check(result1))
    {
        inerts.failed.push_back(P);
        return {};
    }
    else
    {
        inerts.irreducible.push_back(P);
        return {};
    }
}

std::optional<Predicate> Solver::canonicalize_equality_lhs(const CanonicalEquality& P)
{
    if (is_rewritable_lhs(P.t2))
        return canonicalize_equality_lhs2(P);
    else
        return canonicalize_equality_lhs1(P);

}


std::optional<Predicate> Solver::canonicalize_equality(CanonicalEquality P)
{
    P.t1 = rewrite(P.flavor(), P.t1);
    P.t2 = rewrite(P.flavor(), P.t2);

    P.t1 = follow_meta_type_var(P.t1);
    P.t2 = follow_meta_type_var(P.t2);

    // 1. Check if the types are identical -- not looking through type synonyms
    if (same_type_no_syns(P.t1, P.t2))
        return {}; // Solved!

    // 2. Check if we have two identical typecons with no arguments
    //    Right now, this is redundant with #1, but might not be if we start doing loops.
    //    Apparently this is a special case for handling nullary type synonyms before expansion.
    auto tc1 = P.t1.to<TypeCon>();
    auto tc2 = P.t2.to<TypeCon>();
    if (tc1 and tc2 and *tc1 == *tc2)
        return {}; // Solved!

    // 3. Look through type synonyms
    while(auto s1 = is_type_synonym(P.t1))
        P.t1 = *s1;
    while(auto s2 = is_type_synonym(P.t2))
        P.t2 = *s2;

    // 4. See if we have two tycons that aren't type family tycons
    auto tcapp1 = is_type_con_app(P.t1);
    auto tcapp2 = is_type_con_app(P.t2);
    if (tcapp1 and tcapp2)
    {
        auto& [tc1, args1] = *tcapp1;
        auto& [tc2, args2] = *tcapp2;
        if (not type_con_is_type_fam(tc1) and not type_con_is_type_fam(tc2))
            return canonicalize_equality_type_cons(P, tc1, args1, tc2, args2);
    }

    // 5. If both are ForallType

    // NOTE: missing!


    // 6. If both are type applications without type con heads
    auto tapp1 = is_type_app(P.t1);
    auto tapp2 = is_type_app(P.t2);
    if (tapp1 and tapp2)
    {
        auto& [fun1, arg1] = *tapp1;
        auto& [fun2, arg2] = *tapp2;
        return canonicalize_equality_type_apps(P.constraint, fun1, arg1, fun2, arg2);
    }

    // the lhs & rhs should be rewritten by the time we get here.
    // but what if we substitute for a type synonym?

    if (is_rewritable_lhs(P.t1))
        return canonicalize_equality_lhs(P);
    else if (is_rewritable_lhs(P.t2))
        return canonicalize_equality_lhs(P.flip());
    else
    {
        // This should end up in inerts.irreducible?
        return P;
    }

    std::abort();
}

optional<Predicate> Solver::canonicalize_dict(CanonicalDict P)
{
    for(auto& arg: P.args)
        arg = rewrite(P.flavor(), arg);

    P.constraint.pred = make_tyapps(P.klass, P.args);

    return P;
}

optional<Predicate> Solver::canonicalize(Predicate& P)
{
    if (auto NC = to<NonCanonical>(P))
    {
        if (auto eq = is_equality_pred(NC->constraint.pred))
        {
            auto& [t1, t2] = *eq;
            return canonicalize_equality({NC->constraint, t1, t2});
        }
        else
        {
            auto [head,args] = decompose_type_apps(NC->constraint.pred);
            auto klass = head.to<TypeCon>();
            assert(klass);
            return canonicalize_dict({NC->constraint, *klass, args});
        }
    }
    else if (auto D = to<CanonicalDict>(P))
        return canonicalize_dict(*D);
    else if (auto E = to<CanonicalEquality>(P))
        return canonicalize_equality(*E);
    else
        std::abort();
}

