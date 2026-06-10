#include "computation/typecheck/typecheck.H"
#include "computation/typecheck/kindcheck.H"
#include "computation/typecheck/solver.H"

#include "util/set.H"
#include "util/variant.H"

using std::vector;
using std::optional;
using std::tuple;

// Split equality between type applications into equality between heads and nominal arguments.
std::optional<Predicate>
Solver::canonicalize_equality_type_apps(const Constraint& C,
                                        Role role,
                                        const Type& fun1, const Type& arg1, const Type& fun2, const Type& arg2)
{
    auto fun_constraint = make_role_equality_pred(role, fun1, fun2);
    auto fun_dvar = fresh_dvar(fun_constraint);
    work_list.push_back(NonCanonical({C.origin, C.flavor, fun_dvar, fun_constraint, C.tc_state}));

    auto arg_constraint = make_role_equality_pred(Role::Nominal, arg1, arg2);
    auto arg_dvar = fresh_dvar(arg_constraint);
    work_list.push_back(NonCanonical({C.origin, C.flavor, arg_dvar, arg_constraint, C.tc_state}));

    return {};
}

// Decompose equality between saturated type constructors using their declared argument roles.
std::optional<Predicate> Solver::canonicalize_equality_type_cons(const CanonicalEquality& E, const TypeCon& tc1, const vector<Type>& args1, const TypeCon& tc2, const vector<Type>& args2)
{
    assert(not type_con_is_type_fam(tc1));
    assert(not type_con_is_type_fam(tc2));
    assert(not type_con_is_type_syn(tc1));
    assert(not type_con_is_type_syn(tc2));

    // if tc1 and tc2 and both non-type-family type cons...
    if (tc1 == tc2 and args1.size() == args2.size())
    {
        vector<Role> roles(args1.size(), Role::Nominal);
        if (E.role == Role::Representational)
        {
            if (auto T = this_mod().lookup_resolved_type(tc1.name); T and T->roles.size() >= args1.size())
                roles = T->roles;
            else
                roles.assign(args1.size(), Role::Nominal);
        }
        else if (E.role == Role::Phantom)
            roles.assign(args1.size(), Role::Phantom);

        // If we've gotten here, the heads are both injective, and might be equal.
        for(int i=0;i<args1.size();i++)
        {
            auto constraint = make_role_equality_pred(combine_roles(E.role, roles[i]), args1[i], args2[i]);
            auto dvar = fresh_dvar(constraint);
            work_list.push_back(NonCanonical({E.origin(), E.flavor(), dvar, constraint, E.constraint.tc_state}));
        }
    }
    else
    {
        if (E.role == Role::Representational)
            return canonicalize_equality_newtype(E);
        inerts.failed.push_back(E);
    }

    return {};
}

// Return the single field type that represents a saturated newtype application.
std::optional<Type> TypeChecker::unwrap_newtype_type(const Type& type) const
{
    auto tcapp = is_type_con_app(type);
    if (not tcapp) return {};

    auto& [tc, args] = *tcapp;
    auto T = this_mod().lookup_resolved_type(tc.name);
    if (not T) return {};

    auto data_info = T->is_data();
    if (not data_info or data_info->constructors.size() != 1) return {};

    auto C = this_mod().lookup_resolved_symbol(*data_info->constructors.begin());
    if (not C or not C->con_info or not C->con_info->is_newtype_constructor) return {};

    auto con_info = C->con_info;
    if (con_info->field_types.size() != 1 or con_info->uni_tvs.size() != args.size()) return {};

    substitution_t subst;
    for(int i=0;i<args.size();i++)
        subst = subst.insert({con_info->uni_tvs[i], args[i]});

    return apply_subst(subst, con_info->field_types[0]);
}

// Rewrite representational equality by unwrapping one newtype layer from either side.
std::optional<Predicate>
Solver::canonicalize_equality_newtype(const CanonicalEquality& E)
{
    if (E.role != Role::Representational) return E;

    if (auto t1 = unwrap_newtype_type(E.t1))
    {
        auto constraint = make_role_equality_pred(Role::Representational, *t1, E.t2);
        auto dvar = fresh_dvar(constraint);
        work_list.push_back(NonCanonical({E.origin(), E.flavor(), dvar, constraint, E.constraint.tc_state}));
        return {};
    }

    if (auto t2 = unwrap_newtype_type(E.t2))
    {
        auto constraint = make_role_equality_pred(Role::Representational, E.t1, *t2);
        auto dvar = fresh_dvar(constraint);
        work_list.push_back(NonCanonical({E.origin(), E.flavor(), dvar, constraint, E.constraint.tc_state}));
        return {};
    }

    return E;
}

// Decompose equality between forall types after opening both sides with shared binders.
std::optional<Predicate> canonicalize_equality_foralls(Solver& solver, const CanonicalEquality& E, const ForallType& forall1, const ForallType& forall2)
{
    auto opened = solver.open_forall_pair(forall1, forall2);
    if (not opened)
    {
        solver.inerts.failed.push_back(E);
        return {};
    }

    auto [type1, type2] = *opened;
    auto constraint = make_role_equality_pred(E.role, type1, type2);
    auto dvar = solver.fresh_dvar(constraint);
    solver.work_list.push_back(NonCanonical({E.origin(), E.flavor(), dvar, constraint, E.constraint.tc_state}));
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
Solver::canonicalize_equality_var_tyfam(CanonicalEquality E)
{
    assert(E.t1.is_a<TypeVar>() or E.t1.is_a<MetaTypeVar>());
    assert(is_type_fam_app(E.t2));

    // If mtv1 -> tyfam is touchable and has no problems, do that.
    auto mtv1 = E.t1.to<MetaTypeVar>();
    if (mtv1 and is_touchable(*mtv1, E.t2))
    {
        auto result = check_type_equality(E.t1, E.t2) & ~type_family_result;
        if (result == ok_result)
            return canonicalize_equality_lhs1(E);
    }

    // Otherwise prefer tyfam -> var
    return canonicalize_equality_lhs1(E.flip());
}


std::optional<Predicate>
Solver::canonicalize_equality_lhs2(CanonicalEquality E)
{
    assert(is_rewritable_lhs(E.t1) and is_rewritable_lhs(E.t2));

    auto tfam1 = is_type_fam_app(E.t1);
    auto tfam2 = is_type_fam_app(E.t2);
    
    if (tfam1 and tfam2)
    {
        // If both are type fam apps, then
        // 1. If only one has metatypevars as its arguments, then put that one on the left
        // 2. If the lhs occurs on the rhs, but not vice versa, then we want to swap.
        //    For example F a ~ F (F a) should be swapped.

        // Case 2: If we have F a = F (F a), then we want to flip.
        bool flip_for_occurs = check_type_equality(E.t1, E.t2).test(occurs_definitely_bit)
            and not check_type_equality(E.t2, E.t1).test(occurs_definitely_bit);

        if (flip_for_occurs)
            return canonicalize_equality_lhs1(E.flip());
        else
            return canonicalize_equality_lhs1(E);
    }
    else if (tfam1)
        return canonicalize_equality_var_tyfam(E.flip());
    else if (tfam2)
        return canonicalize_equality_var_tyfam(E);
    else
    {
        if (flip_type_vars(E.flavor() == Given, E.t1, E.t2))
            return canonicalize_equality_lhs1(E.flip());
        else
            return canonicalize_equality_lhs1(E);
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
    else if (auto syn = expand_type_synonym(type))
        return break_type_equality_cycle(C, *syn);
    else if (auto tfam = is_type_fam_app(type))
    {
        auto& [tc,args] = *tfam;

        // Get the kind for type
        auto kind = this_mod().lookup_local_type(tc.name)->kind;
        for(int i=0;i<args.size();i++)
        {
            auto [_, result_kind] = is_function_type(kind).value();
            kind = result_kind;
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
        for(auto& constraint: CT.context)
            constraint = break_type_equality_cycle(C, constraint);
        CT.type = break_type_equality_cycle(C, CT.type);
        return CT;
    }
    else
        std::abort();
}

std::optional<Type> Solver::maybe_break_type_equality_cycle(const CanonicalEquality& E, std::bitset<8> result)
{
    assert(result.any());

    // 1. Only do this if we have an occurs check under a type family application.
    if (result != occurs_maybe_result) return {};

    // 2. Don't do this if we have a wanted cosntraint without a touchable mtv
    bool should_break_cycle = true;
    if (E.flavor() == Wanted)
    {
        should_break_cycle = false;
        auto lhs = follow_meta_type_var(E.t1);
        if (auto mtv = lhs.to<MetaTypeVar>(); mtv and is_touchable(*mtv, E.t2))
            should_break_cycle = true;
    }

    if (not should_break_cycle) return {};

    // 3. Check that the equality does not come from a cycle breaking operation.
    if (to<CycleBreakerOrigin>(E.constraint.origin)) return {};

    // 4. Actually do the cycle breaking
    return break_type_equality_cycle(E.constraint, E.t2);
}

std::optional<Predicate> Solver::canonicalize_equality_lhs1(const CanonicalEquality& E)
{
    assert(is_rewritable_lhs(E.t1));

    auto result0 = check_type_equality(E.t1, E.t2);

    // Don't worry about type families here...
    auto result1 = result0;
    result1.reset(type_family_bit);

    if (result1 == ok_result)
        return E;

    if (auto new_rhs = maybe_break_type_equality_cycle(E, result1))
    {
        auto E2 = E;
        E2.t2 = *new_rhs;
        auto result2 = check_type_equality(E2.t1, E2.t2);

        if (has_occurs_check(result2))
        {
            inerts.failed.push_back(E);
            return {};
        }
        else
            return E2;
    }
    else if (has_occurs_check(result1))
    {
        inerts.failed.push_back(E);
        return {};
    }
    else
    {
        inerts.irreducible.push_back(E);
        return {};
    }
}

std::optional<Predicate> Solver::canonicalize_equality_lhs(const CanonicalEquality& E)
{
    if (is_rewritable_lhs(E.t2))
        return canonicalize_equality_lhs2(E);
    else
        return canonicalize_equality_lhs1(E);

}


std::optional<Predicate> Solver::canonicalize_equality(CanonicalEquality E)
{
    if (E.role == Role::Phantom)
        return {};

    E.t1 = rewrite(E.flavor(), E.t1);
    E.t2 = rewrite(E.flavor(), E.t2);

    E.t1 = follow_meta_type_var(E.t1);
    E.t2 = follow_meta_type_var(E.t2);

    // 1. Check if the types are identical -- not looking through type synonyms
    if (same_type_no_syns(E.t1, E.t2))
        return {}; // Solved!

    // 2. Check if we have two identical typecons with no arguments
    //    Right now, this is redundant with #1, but might not be if we start doing loops.
    //    Apparently this is a special case for handling nullary type synonyms before expansion.
    auto tc1 = E.t1.to<TypeCon>();
    auto tc2 = E.t2.to<TypeCon>();
    if (tc1 and tc2 and *tc1 == *tc2)
        return {}; // Solved!

    // 3. Look through type synonyms
    while(auto s1 = expand_type_synonym(E.t1))
        E.t1 = *s1;
    while(auto s2 = expand_type_synonym(E.t2))
        E.t2 = *s2;

    // 4. See if we have two tycons that aren't type family tycons
    auto tcapp1 = is_type_con_app(E.t1);
    auto tcapp2 = is_type_con_app(E.t2);
    if (tcapp1 and tcapp2)
    {
        auto& [tc1, args1] = *tcapp1;
        auto& [tc2, args2] = *tcapp2;
        if (not type_con_is_type_fam(tc1) and not type_con_is_type_fam(tc2))
            return canonicalize_equality_type_cons(E, tc1, args1, tc2, args2);
    }

    if (E.role == Role::Representational)
    {
        if (auto unwrapped = canonicalize_equality_newtype(E))
        {
            if (not std::holds_alternative<CanonicalEquality>(*unwrapped))
                return unwrapped;
        }
        else
            return {};
    }

    // 5. If both are ForallType
    auto forall1 = E.t1.to<ForallType>();
    auto forall2 = E.t2.to<ForallType>();
    if (forall1 and forall2)
        return canonicalize_equality_foralls(*this, E, *forall1, *forall2);


    // 6. If both are type applications without type con heads
    auto tapp1 = is_type_app(E.t1);
    auto tapp2 = is_type_app(E.t2);
    if (tapp1 and tapp2)
    {
        auto& [fun1, arg1] = *tapp1;
        auto& [fun2, arg2] = *tapp2;
        return canonicalize_equality_type_apps(E.constraint, E.role, fun1, arg1, fun2, arg2);
    }

    // the lhs & rhs should be rewritten by the time we get here.
    // but what if we substitute for a type synonym?

    if (is_rewritable_lhs(E.t1))
        return canonicalize_equality_lhs(E);
    else if (is_rewritable_lhs(E.t2))
        return canonicalize_equality_lhs(E.flip());
    else
    {
        // This should end up in inerts.irreducible?
        return E;
    }

    std::abort();
}

optional<Predicate> Solver::canonicalize_dict(CanonicalDict P)
{
    for(auto& arg: P.args)
        arg = rewrite(P.flavor(), arg);

    P.constraint.pred = type_apply(P.klass, P.args);

    return P;
}

optional<Predicate> Solver::canonicalize(Predicate& P)
{
    if (auto NC = to<NonCanonical>(P))
    {
        if (auto eq = is_role_equality_pred(NC->constraint.pred))
        {
            return canonicalize_equality({NC->constraint, eq->role, eq->lhs, eq->rhs});
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
