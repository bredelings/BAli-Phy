#include "typecheck.H"
#include "kindcheck.H"
#include "solver.H"

#include "util/set.H"
#include "util/string/join.H"

#include <range/v3/all.hpp>

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;
using std::shared_ptr;

namespace views = ranges::views;

template <typename T, typename U>
const T* to(const U& u)
{
    if (std::holds_alternative<T>(u))
        return &std::get<T>(u);
    else
        return nullptr;
}

template <typename T, typename U>
T* to(U& u)
{
    if (std::holds_alternative<T>(u))
        return &std::get<T>(u);
    else
        return nullptr;
}

string NonCanonical::print() const
{
    return constraint.print();
}

string CanonicalDict::print() const
{
    return constraint.print();
}

string CanonicalEquality::print() const
{
    return constraint.print();
}

string Predicate::print() const
{
    if (std::holds_alternative<NonCanonical>(*this))
        return std::get<NonCanonical>(*this).print();
    else if (std::holds_alternative<CanonicalDict>(*this))
        return std::get<CanonicalDict>(*this).print();
    else if (std::holds_alternative<CanonicalEquality>(*this))
        return std::get<CanonicalEquality>(*this).print();
    else
        std::abort();
}

const Constraint& Predicate::constraint() const
{
    if (auto nc = to<NonCanonical>(*this))
        return nc->constraint;
    else if (auto dict = to<CanonicalDict>(*this))
        return dict->constraint;
    else if (auto eq = to<CanonicalEquality>(*this))
        return eq->constraint;
    else
        std::abort();
}

ConstraintFlavor Predicate::flavor() const
{
    return constraint().flavor;
}

// FIXME: there should be a `const` way of getting these.
// FIXME: instantiate is not constant though.
// FIXME: we shouldn't need fresh type vars if the type is unambiguous though.
vector<pair<Core::Var, Type>> TypeChecker::superclass_constraints(const Type& constraint)
{
    vector<pair<Core::Var, Type>> constraints;

    auto class_name = get_full_class_name_from_constraint(constraint);

    if (class_name == "~") return {};

    // Fixme... since we know the class name, can we simplify superclass_extractors???
    for(auto& [dvar, type]: class_env().at(class_name).superclass_extractors)
    {
        // forall a.Klass a => Superklass a
        auto [_, wanteds, superclass_constraint] = instantiate(type);

        // Constraints like a ~ (Arg a -> Result a) violate this.
        // assert(constraint_is_hnf(superclass_constraint));

        assert(wanteds.size() == 1);

        auto class_constraint = wanteds[0].pred;

        // The premise doesn't match the current class;
        if (not maybe_match(class_constraint, constraint)) continue;

        constraints.push_back( { dvar, superclass_constraint } );
    }

    return constraints;
}

// We are trying to eliminate the *first* argument.
optional<vector<Core::Var>> TypeChecker::is_superclass_of(const Type& constraint1, const Type& constraint2)
{
    vector<Core::Var> extractors;
    if (same_type(constraint1, constraint2))
        return extractors;
    else
    {
        // dvar1 :: constraint1 => dvar3 :: constraint3 => dvar2 :: constraint2
        for(auto& [dvar, constraint3]: superclass_constraints(constraint2))
        {
            if (auto extractors2 = is_superclass_of(constraint1, constraint3))
            {
                extractors = std::move(*extractors2);
                extractors.push_back(dvar);
                return extractors;
            }
        }
        return {};
    }
}

optional<Core::Decls> TypeChecker::entails_by_superclass(const Constraint& given, const Constraint& wanted)
{
    if (auto extractors = is_superclass_of(wanted.pred, given.pred))
    {
        Core::Exp dict_exp = given.ev_var;
        for(auto& extractor: *extractors | views::reverse)
            dict_exp = {extractor, dict_exp};

        // dvar_wanted = extractor[n] extractor[n-1] ... extractor[0] dvar_given
        return Core::Decls( { pair(wanted.ev_var, dict_exp) } );
    }
    else
        return {};
}

// 1. constraint solving loop: first process simple constraints, then go under implications.
//    1a. Run the constraint solving loop on the simpled Wanted constraints.
//        - This can create new implication constraints. (how?)
//    1b. Process each implication constraint
//        - Enter an implication.
//        - Save the outer inert set.
//        - Simplify the givens - add them to the inert set when this makes sense.
//        - Solve the inner simple wanteds - also add them to the inert set?
//          - does this have the effect of making simple wanteds affect implication constraints?
//        - Recurse into nested implications.
//        - Restore the saved inert set to what it was before entering the implication.
//        - Then continue processing the next implication constraint.

// 2. The interaction pipeline.
//    2a. Pick an item from the work list.
//    2b. If its not canonical, canonicalize it and goto 2a.
//    2c. If we have an inert reaction, do it and goto 2a.
//    2d. If we have a top-level reaction, do it and goto 2a.
//    2e. If its canonical and there are no possible reactions left, add it to the inert set.


template <typename T>
std::optional<Core::Decls> TypeChecker::entails(const T& givens, const std::pair<Core::Var, Type>& wanted_pred)
{
    // 1. First check if the wanted pred is a superclass of any of the givens.
    //
    //    A class implies all of its superclasses separately, but a single superclass of K may not imply K.
    for(auto& given: givens)
    {
        if (auto decls = entails_by_superclass(given, wanted_pred))
            return *decls;
    }

    // 2. Then check if there is an instance dfun :: (K1 a, K2 a) => K a
    auto [wanted_dvar, wanted_constraint] = wanted_pred;

    if (auto inst = lookup_instance(wanted_constraint))
    {
        auto [dfun_exp, super_wanteds] = *inst;

        Core::Decls decls;
        decls.push_back( { wanted_dvar, dfun_exp} );

        // If we can get (dvar1 :: K1 a) and (dvar2 :: K2 a) and a dfun so that dvar = dfun dvar1 dvar2
        for(auto& super_wanted: super_wanteds)
        {
            if (auto edecls = entails(givens, super_wanted))
                decls = *edecls + decls;
            else
                return {};
        }

        return decls;
    }

    return {};
}

CanonicalEquality CanonicalEquality::flip() const
{
    CanonicalEquality E = *this;
    E.constraint.pred = make_equality_constraint(t2,t1);
    std::swap(E.t1, E.t2);
    return E;
}

std::optional<Predicate>
Solver::canonicalize_equality_type_apps(ConstraintFlavor flavor,
                                        const Type& fun1, const Type& arg1, const Type& fun2, const Type& arg2)
{
    auto fun_constraint = make_equality_constraint(fun1, fun2);
    auto fun_dvar = fresh_dvar(fun_constraint);
    work_list.push_back(NonCanonical({flavor, fun_dvar, fun_constraint}));

    auto arg_constraint = make_equality_constraint(arg1, arg2);
    auto arg_dvar = fresh_dvar(arg_constraint);
    work_list.push_back(NonCanonical({flavor, arg_dvar, arg_constraint}));

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
            auto constraint = make_equality_constraint(args1[i], args2[i]);
            auto dvar = fresh_dvar(constraint);
            work_list.push_back(NonCanonical({P.flavor(), dvar, constraint}));
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

    auto mtv1 = P.t1.to<MetaTypeVar>();
    if (mtv1 and is_touchable(*mtv1, P.t2))
        return canonicalize_equality_lhs1(P);
    else
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

void Solver::unbreak_type_equality_cycles()
{
    for(auto& [mtv,type]: inerts.cycle_breakers)
    {
        mtv.fill(type);
    }
}

Type Solver::break_type_equality_cycle(ConstraintFlavor flavor, const Type& type)
{
    if (auto t = filled_meta_type_var(type))
        return break_type_equality_cycle(flavor, *t);
    else if (type.is_a<MetaTypeVar>())
        return type;
    else if (type.is_a<TypeVar>())
        return type;
    else if (auto app = is_type_app(type))
    {
        auto& [head, arg] = *app;
        return TypeApp(break_type_equality_cycle(flavor, head), break_type_equality_cycle(flavor, arg));
    }
    // FIXME!
    // We should mark type synonyms for whether they contains type fams.
    // Then we wouldn't have to look through them as much.
    else if (auto syn = is_type_synonym(type))
        return break_type_equality_cycle(flavor, *syn);
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
        if (flavor == Given)
        {
            new_tv.cycle_breaker = true;
            inerts.cycle_breakers.push_back({new_tv, type});
        }

        // TODO: Mark these as coming from a cycle-breaking operation.

        auto constraint = make_equality_constraint(new_tv, type);
        auto dvar = fresh_dvar(constraint);
        work_list.push_back(NonCanonical({flavor, dvar, constraint}));

        return new_tv;
    }
    else if (type.is_a<TypeCon>())
        return type;
    else if (type.is_a<ForallType>()) // We can't fix type families under a forall?
        return type;
    else if (auto c = type.to<ConstrainedType>())
    {
        auto C = *c;
        for(auto& constraint: C.context.constraints)
            constraint = break_type_equality_cycle(flavor, constraint);
        C.type = break_type_equality_cycle(flavor, C.type);
        return C;
    }
    else if (auto sl = type.to<StrictLazyType>())
    {
        auto SL = *sl;
        SL.type = break_type_equality_cycle(flavor, SL.type);
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

    // 4. Actually do the cycle breaking
    return break_type_equality_cycle(P.flavor(), P.t2);
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
        return canonicalize_equality_type_apps(P.flavor(), fun1, arg1, fun2, arg2);
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

bool is_canonical(const Predicate& p)
{
    return std::holds_alternative<CanonicalDict>(p) or
        std::holds_alternative<CanonicalEquality>(p);
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
        if (auto eq = is_equality_constraint(NC->constraint.pred))
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
    if (auto t2 = is_type_synonym(t))
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
    else if (auto sl = t.to<StrictLazyType>())
    {
        auto SL = *sl;
        SL.type = rewrite(flavor, SL.type);
        return SL;
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

Change Solver::interact(const Predicate& P1, const Predicate& P2)
{
    assert(is_canonical(P1));
    assert(is_canonical(P2));

    // Don't allow wanteds to rewrite givens
    if (P1.flavor() == Wanted and P2.flavor() == Given) return Unchanged();

    auto dict1 = to<CanonicalDict>(P1);
    auto dict2 = to<CanonicalDict>(P2);

    if (dict1 and dict2)
    {
        // DDICT:  (D xs)     + (D xs)     -> (D xs)
        if (same_type(dict1->constraint.pred, dict2->constraint.pred))
        {
            decls.push_back({dict2->constraint.ev_var, dict1->constraint.ev_var});
            return Solved();
        }
        // SUPER - not in the paper.
        else if (auto sdecls = entails_by_superclass(dict1->constraint, dict2->constraint))
        {
            decls += *sdecls;
            return Solved();
        }
    }
    // EQFEQ:  (tv1 ~ X1) + (F xs ~ x) -> (tv1 ~ X1) && (F [tv1->X1]xs ~ [tv1 -> X1]x) if tv1 in ftv(xs,x)
    // FEQFEQ: (F xs ~ X1) + (F xs ~ X2) -> (F xs ~ X1) && (X1 ~ X2)

    return Unchanged();
}

// we need to have three results: No react, React<vector<Predicates>>, ReactFail
std::optional<Reaction> Solver::top_react(const Predicate& P)
{
    assert(is_canonical(P));

    if (auto dict = to<CanonicalDict>(P))
    {
        // We DO get givens like Eq Ordering that have instances.
        // Should we be preventing such things from becoming givens, since we could
        //   derive them from an instance instead?

        // We don't use instances for givens.
        if (P.flavor() == Given) return {};

        if (auto inst = lookup_instance(dict->constraint.pred))
        {
            auto [dfun_exp, super_wanteds] = *inst;

            decls.push_back( { dict->constraint.ev_var, dfun_exp } );
            for(auto& pred: super_wanteds)
                work_list.push_back( pred );

            return ReactSuccess();
        }
    }
    else if (auto eq = to<CanonicalEquality>(P))
    {
        // We don't use instances for givens.
        if (P.flavor() == Given) return {};

        auto constraint = make_equality_constraint(eq->t1,eq->t2);

        if (auto inst = lookup_instance(constraint))
        {
            auto [dfun_exp, super_wanteds] = *inst;

//            What is the evidence for type family instances?
//            decls.push_back( { dvar, dfun_exp } );

            if (super_wanteds.size())
                throw err_context_exception()<<"type family instances can't have constraints!";

            return ReactSuccess();
        }

        // How similar is this to looking up a class instance for (~) t1 t2?

        // I guess if we find a match then we are just done.
    }
    
    return {};
}

bool Solver::is_touchable(const MetaTypeVar& mtv, const Type& rhs) const
{
    // We need to have done follow_meta_type_var( ) already.
    assert(not mtv.filled());

    assert(mtv.level() <= level);

    assert(inerts.given_eq_level.value_or(0) < level);

    if (mtv.cycle_breaker) return false;

    // 1. Check for intervening given equalities
    bool intervening_given_eqs = inerts.given_eq_level and mtv.level() <= *inerts.given_eq_level;
    if (intervening_given_eqs) return false;

    // 2. Check for skolem escapes
    for(auto& tv: free_type_variables(rhs))
    {
        if (mtv.level() < tv.level())
            return false;
    }

    return true;
}

bool affected_by_mtv(const Predicate& P, const MetaTypeVar& mtv)
{
    return affected_by_mtv(P.constraint().pred, mtv);
}

void Solver::add_to_work_list(const std::vector<Predicate>& ps)
{
    for(auto& p: ps)
        if (p.flavor() == Wanted)
            work_list.push_back(p);

    for(auto& p: ps)
        if (p.flavor() == Given)
            work_list.push_back(p);
}

vector<Predicate> kickout_after_unification2(const MetaTypeVar& mtv, std::vector<Predicate>& preds)
{
    vector<Predicate> work_list;
    // kick-out for inerts that are rewritten by p
    for(int i=0;i<preds.size();i++)
    {
        if (affected_by_mtv(preds[i], mtv))
        {
            // FIXME: put givens last, so that we process them first
            work_list.push_back(preds[i]);
            if (i+1 < preds.size())
                std::swap(preds[i], preds.back());

            preds.pop_back();
            i--;
        }
    }

    return work_list;
}

void Solver::kickout_after_unification(const MetaTypeVar& mtv)
{
    // kick-out for inerts containing mtv
    kickout_after_unification2(mtv, inerts.tv_eqs);
    kickout_after_unification2(mtv, inerts.mtv_eqs);
    kickout_after_unification2(mtv, inerts.tyfam_eqs);
    kickout_after_unification2(mtv, inerts.dicts);
    kickout_after_unification2(mtv, inerts.irreducible);
}

void Solver::add_inert(const Predicate& p)
{
    if (auto E = to<CanonicalEquality>(p))
    {
        int eq_level = std::max( max_level(E->t1), max_level(E->t2));
        if (eq_level < level and p.flavor() == Given)
        {
            inerts.given_eq_level = std::max( inerts.given_eq_level.value_or(0), eq_level );
        }
    }

    if (auto E = to<CanonicalEquality>(p))
    {
        auto t1 = follow_meta_type_var(E->t1);
        if (t1.is_a<TypeVar>())
            inerts.tv_eqs.push_back(p);
        else if (t1.is_a<MetaTypeVar>())
            inerts.mtv_eqs.push_back(p);
        else if (is_type_fam_app(t1))
            inerts.tyfam_eqs.push_back(p);
        else
            inerts.irreducible.push_back(p);
    }
    else if (to<CanonicalDict>(p))
    {
        inerts.dicts.push_back(p);
    }
    else
        std::abort();
}

    // The simplifier corresponds to the relation |->[simp] from Figure 19 of the OutsideIn(X) paper:
    //    \mathcal{Q}; Q[given] ; alpha[touchable] |->[simp] Q[wanted] ~~> Q[residual]; theta
    // where theta is the substitution -- which we should actually perform here instead of returning as an object.

    // So, we need to to know the set of touchable variables, either through the TypeChecker, or
    // as a function argument.

    // This has a few steps (rule SIMPLES from Figure 19):
    // 1. Run the the rewrite rules until no more are applicable.
    //    We start out with no flattening substitution.
    // 2. Apply the flattening transformation on the wanteds -> Q[rr].
    // 3. \mathcal{E} = the set of (beta ~ tau) or (tau ~ beta) in Q[rr] where
    //    * beta is a touchable unification variable
    //    * beta is not in the free unification variables of tau
    // 4. theta = substitutions (beta -> theta(tau)) from \mathcal{E}, where
    //    * we choose only one substitution for beta if we have multiple equations on beta
    //    * for example, if we have (beta ~ tau1) and (beta ~ tau2), we pick one.
    //    * presumably, we can pick either one -> they would need to agree!
    //    * if they don't, do we do Irresolvable (beta ~ tau1) and Irresolvable (beta ~ tau2)?
    //    * we need to substitute into the taus using the theta that contains the taus!
    //      - that probably works fine if we use meta-vars.
    // 5. Q[r] = Q[rr] - \mathcal{E}
    // 6. Return theta(Q[r])
    //    * we can do this by just performing theta and returning Q[r].

    // The rewrite rules (also Figure 19) include:
    // a. replace a given/wanted by a canonicalized given/wanted (and maybe add a flattening substitution)
    // b. interact a given/wanted Q1 with a given/wanted Q2 and to produce a given/wanted Q3.
    //    - do we REPLACE Q1 and Q2 by Q3, or ADD Q3 to the set?
    // c. react a given Q1 and a wanted Q2 to produce a new wanted Q3 = simplifies(Q1,Q2)
    //    - replace the wanted Q2 by Q3.
    // d. react a given Q1 with axioms in \mathcal{Q} to produce Q2.
    //    - replace Q1 by Q2.
    // e. react a wanted Q1 with axioms in \mathcal{Q} to produce Q2 and new touchable variables beta.
    //    - replace Q1 by Q2 and add in the new touchable variables beta.


bool Solver::contains_type(const Type& t1_, const Type& t2) const
{
    assert(is_rewritable_lhs(t2));

    Type t1 = follow_meta_type_var(t1_);

    if (auto mtv = t1.to<MetaTypeVar>())
        return *mtv == t2;
    else if (auto tv = t1.to<TypeVar>())
        return *tv == t2;
    else if (auto con = t1.to<ConstrainedType>())
        return contains_type(con->context.constraints, t2) or contains_type(con->type, t2);
    else if (auto forall = t1.to<ForallType>())
        return contains_type(forall->type, t2);
    else if (auto sl = t1.to<StrictLazyType>())
        return contains_type(sl->type, t2);
    else if (auto syn = is_type_synonym(t1))
        return contains_type(*syn, t2);
    else if (auto app = t1.to<TypeApp>())
        return contains_type(app->head, t2) or contains_type(app->arg, t2);
    else if (t1.is_a<TypeCon>())
        return false;
    else
        std::abort();
}

bool Solver::contains_type(const vector<Type>& ts1, const Type& t2) const
{
    for(auto& t1: ts1)
        if (contains_type(t1, t2)) return true;
    return false;
}

bool Solver::can_rewrite(const Predicate& p1, const Predicate& p2) const
{
    // 1. Check the flavor
    if (p1.flavor() == Wanted and p2.flavor() == Given) return false;

    // 2. Check if p1 can be used for rewriting.
    auto eq1 = to<CanonicalEquality>(p1);
    if (not eq1 or not is_rewritable_lhs(eq1->t1)) return false;

    auto lhs = follow_meta_type_var(eq1->t1);

    if (auto dict2 = to<CanonicalDict>(p2))
        return contains_type(dict2->args, lhs);
    else if (auto eq2 = to<CanonicalEquality>(p2))
        return contains_type(eq2->t1, lhs) or contains_type(eq2->t2, lhs);
    else
        std::abort();

    return false;
}

void Solver::kickout_rewritten(const Predicate& p, std::vector<Predicate>& ps)
{
    // kick-out for inerts that are rewritten by p
    for(int i=0;i<ps.size();i++)
    {
        if (can_rewrite(p, ps[i]))
        {
            work_list.push_back(ps[i]);

            if (i+1 < ps.size())
                std::swap(ps[i], ps.back());

            ps.pop_back();
            i--;
            continue;
        }
    }
}

Core::Decls Solver::simplify(const LIE& givens, LIE& wanteds)
{
    if (wanteds.empty()) return {{}, {}};

    for(auto& wanted: wanteds)
    {
        assert(wanted.flavor == Wanted);
        work_list.push_back( NonCanonical(wanted) );
    }
    // Givens must be processed first!
    for(auto& given: givens)
    {
        assert(given.flavor == Given);
        work_list.push_back( NonCanonical(given) );
    }

//    std::cerr<<"---------------\n";
//    for(auto& w: work_list)
//        std::cerr<<"  "<<w.print()<<"\n";

    while(not work_list.empty())
    {
        auto p = work_list.back(); work_list.pop_back();

        // canonicalize
        if (auto cp = canonicalize(p))
            p = *cp;
        else
            continue;

        // interact
        bool done = false;
        for(auto& inert: views::concat(inerts.tv_eqs, inerts.mtv_eqs, inerts.tyfam_eqs, inerts.dicts, inerts.irreducible, inerts.failed))
        {
            auto I = interact(inert, p);
            if (auto C = to<Changed>(I))
            {
                p = C->P;
            }
            else if (to<Solved>(I) or to<NonCanon>(I))
            {
                done = true;
                break;
            }
        }
        if (done) continue;

        // kick-out for inerts that are rewritten by p
        kickout_rewritten(p, inerts.tv_eqs);
        kickout_rewritten(p, inerts.mtv_eqs);
        kickout_rewritten(p, inerts.tyfam_eqs);
        kickout_rewritten(p, inerts.dicts);
        kickout_rewritten(p, inerts.irreducible);
        kickout_rewritten(p, inerts.failed);

        // top-level reactions
        if (top_react(p))
            continue;


        // perform same-level substitutions
        if (auto E = to<CanonicalEquality>(p); E and p.flavor() == Wanted)
        {
            auto t1 = follow_meta_type_var(E->t1);
            if (auto mtv = t1.to<MetaTypeVar>())
            {
                if (mtv->level() == level)
                {
                    mtv->fill(E->t2);
                    kickout_after_unification(*mtv);
                    continue;
                }
                else if (mtv->level() < level and is_touchable(*mtv, E->t2))
                {
                    promote(E->t2, mtv->level());
                    set_unification_level(mtv->level());
                    mtv->fill(E->t2);
                    kickout_after_unification(*mtv);
                    // Iterating at the level of mtv reconstructs the inert set from scratch, so ...
                    // we don't need to do kick-out at that level?
                    continue;

                    // 3. maybe track which level the constraints are added on?  that way wanteds on a higher
                    //    level become givens automatically...

                    // 4. create a CtLoc object and set it when we first create constraints...

                    // 5. create a TcLclEnv object, and give one to implications also

                    // 6. what to do with ic_given_eqs on implications?
                }
            }
        }

        // we should only get this far if p is closed under rewriting, and unsolved.
        add_inert(p);
    }

    if (not inerts.failed.empty())
    {
        myexception e("Unsolvable equations:\n");
        for(auto& f: inerts.failed)
            e<<"  "<<f.print()<<"\n";
        throw e;
    }

    // Split inert into substitution and remaining constraints
    wanteds.clear();
    for(auto& P: views::concat(inerts.tv_eqs, inerts.mtv_eqs, inerts.tyfam_eqs, inerts.dicts, inerts.irreducible, inerts.failed))
    {
        assert(is_canonical(P));

        if (P.flavor() == Wanted)
            wanteds.push_back(P.constraint());
    }

//    std::cerr<<" residual wanteds = \n";
//    for(auto& [v,c]: residual_wanteds)
//        std::cerr<<"  "<<v.print()<<" :  "<<c.print()<<"\n";

    return decls;
}

bool contains_equality_constraints(const LIE& constraints)
{
    for(auto& constraint: constraints)
        if (is_equality_constraint(constraint.pred))
            return true;
    return false;
}

Core::Decls TypeChecker::entails(const LIE& givens, WantedConstraints& wanteds)
{
    Core::Decls decls;
    bool update = false;
    do
    {
        // 1. Simplify the simple wanteds.
        Solver solver(*this);
        decls += solver.simplify(givens, wanteds.simple);
        update = false;

        // 2. Handle implications
        vector<shared_ptr<Implication>> wanted_implics;
        LIE new_wanteds;
        std::swap(wanted_implics, wanteds.implications);
        for(auto& implic: wanted_implics)
        {
            // 3. construct sub-givens
            LIE sub_givens = implic->givens;
            sub_givens += givens;
            sub_givens += wanteds.simple;

            // 4. try and sub-wanteds
            auto tcs2 = copy_clear_wanteds();
            tcs2.level = implic->level;
            *implic->evidence_binds += tcs2.entails(sub_givens, implic->wanteds);

            // 5. Promote any level+1 meta-vars and complain about level+1 skolem vars.
            LIE lie_residual_keep;
            if (not contains_equality_constraints(implic->givens))
            {
                for(auto& constraint: implic->wanteds.simple)
                {
                    promote(constraint.pred, level);

                    if (intersects(free_type_variables(constraint.pred), implic->tvs))
                        lie_residual_keep.push_back(constraint);
                    else
                    {
                        // If we've discovered a new simple wanted, then we need to run simplification again.
                        update = true;
                        new_wanteds.push_back(constraint);
                    }
                }
                implic->wanteds.simple.clear();
            }

            // 6. Issue collected warnings constraints that couldn't be derived from the givens.
            if (not lie_residual_keep.empty())
                throw myexception()<<"Can't derive constraints '"<<print(lie_residual_keep)<<"' from specified constraints '"<<print(givens)<<"'";

            // 7. Keep implication if not empty.
            if (not implic->wanteds.empty())
                wanteds.implications.push_back( implic );

            // 8. If there was a unification, that affected this level, then we have to iterate.
            if (unification_level() and *unification_level() <= level)
                break;
        }

        wanteds.simple += new_wanteds;

        // If there was a unification, that affected this level, then we have to iterate.
        if (unification_level() and *unification_level() == level)
        {
            update = true;
            clear_unification_level();
        }

        solver.unbreak_type_equality_cycles();
    }
    while(update);

    // This should implement |->[solv] from Figure 14 of the OutsideIn(X) paper:
    //   \mathcal{Q}; Q[given]; alpha[touchable] |->[solv] C[wanted] ~~> Q[residual]; theta
    // where theta is a substitution.

    // This has a few steps (rule SOLVE from Figure 14):
    // 1. Run the simplifier (|->[simp]) on the non-implication constraints to produce Q[r]; theta
    // 2. For each implication constraint alpha[i];Q[i] => C[i]
    //    - apply theta to Q[i] and C[i] (which would happen automatically if we use metavars)
    //    - run solve(\mathcal{Q}, Q[given] + Q[r] + Q[i]; alpha[i], C[i]) |->[solv] empty;theta[i]
    //    - the theta[i] shouldn't affect anything outside the implication we are working on.
    // 3. we return Q[r];theta
    //    - interestingly, the implication constraints don't contribute here.

    return decls;
}

Solver::Solver(const TypeChecker& tc)
    :TypeChecker(tc)
{
}

string InertSet::print() const
{
    vector<string> ps;
    for(auto& pred: views::concat(tv_eqs, mtv_eqs, tyfam_eqs, dicts, irreducible, failed))
    {
        ps.push_back(pred.print());
    }
    return join(ps, "\n");
}
