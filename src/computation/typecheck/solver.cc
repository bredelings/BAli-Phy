#include "typecheck.H"
#include "kindcheck.H"
#include "solver.H"

#include "util/set.H"

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

string NonCanonicalPred::print() const
{
    return dvar.print() + " :: " + constraint.print();
}

string CanonicalDictPred::print() const
{
    auto constraint = Hs::make_tyapps(klass, args);
    return dvar.print() + " :: " + constraint.print();
}

string CanonicalEqualityPred::print() const
{
    auto constraint = Hs::make_equality_constraint(a, b);
    return co.print() + " :: " + constraint.print();
}

string Pred::print() const
{
    if (std::holds_alternative<NonCanonicalPred>(*this))
        return std::get<NonCanonicalPred>(*this).print();
    else if (std::holds_alternative<CanonicalDictPred>(*this))
        return std::get<CanonicalDictPred>(*this).print();
    else if (std::holds_alternative<CanonicalEqualityPred>(*this))
        return std::get<CanonicalEqualityPred>(*this).print();
    else
        std::abort();
}

string Predicate::print()  const
{
    string s = (flavor==Given)?"[G] ":"[W] ";
    s += pred.print();
    return s;
}
template <typename T, typename U>
const T* to(const U& u)
{
    if (std::holds_alternative<T>(u))
        return &std::get<T>(u);
    else
        return nullptr;
}

// FIXME: there should be a `const` way of getting these.
// FIXME: instantiate is not constant though.
// FIXME: we shouldn't need fresh type vars if the type is unambiguous though.
vector<pair<Core::Var, Hs::Type>> TypeChecker::superclass_constraints(const Hs::Type& constraint)
{
    vector<pair<Core::Var, Hs::Type>> constraints;

    auto class_name = get_full_class_name_from_constraint(constraint);

    // Fixme... since we know the class name, can we simplify superclass_extractors???
    for(auto& [dvar, type]: class_env().at(class_name).superclass_extractors)
    {
        // forall a.Klass a => Superklass a
        auto [_, wanteds, superclass_constraint] = instantiate(type);

        assert(constraint_is_hnf(superclass_constraint));

        assert(wanteds.size() == 1);

        auto class_constraint = wanteds[0].second;

        // The premise doesn't match the current class;
        if (not maybe_match(class_constraint, constraint)) continue;

        constraints.push_back( { dvar, superclass_constraint } );
    }

    return constraints;
}

// We are trying to eliminate the *first* argument.
optional<vector<Core::Var>> TypeChecker::is_superclass_of(const Hs::Type& constraint1, const Hs::Type& constraint2)
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

optional<Core::Decls> TypeChecker::entails_by_superclass(const pair<Core::Var, Hs::Type>& given, const pair<Core::Var, Hs::Type>& wanted)
{
    auto& [dvar_given, given_constraint] = given;
    auto& [dvar_wanted, wanted_constraint] = wanted;

    if (auto extractors = is_superclass_of(wanted_constraint, given_constraint))
    {
        Core::Exp dict_exp = dvar_given;
        for(auto& extractor: *extractors | views::reverse)
            dict_exp = {extractor, dict_exp};

        // dvar_wanted = extractor[n] extractor[n-1] ... extractor[0] dvar_given
        return Core::Decls( { pair(dvar_wanted, dict_exp) } );
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
std::optional<Core::Decls> TypeChecker::entails(const T& givens, const std::pair<Core::Var, Hs::Type>& wanted_pred)
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

bool is_canonical(const Predicate& p)
{
    return std::holds_alternative<CanonicalDictPred>(p.pred) or
        std::holds_alternative<CanonicalEqualityPred>(p.pred);
}

const Hs::MetaTypeVar* unfilled_meta_type_var(Hs::Type& t)
{
    t = follow_meta_type_var(t);
    return t.to<Hs::MetaTypeVar>();
}

bool cmp_less(const Hs::MetaTypeVar& uv1, const Hs::MetaTypeVar& uv2)
{
    assert(not uv1.filled());
    assert(not uv2.filled());

    if (uv1.level() > uv2.level())
        return true;
    if (uv1.level() < uv2.level())
        return false;

    return (uv1 < uv2);
}


std::optional<Predicate> Solver::canonicalize_equality(Core::Var& co_var, ConstraintFlavor flavor, Hs::Type t1, Hs::Type t2)
{
    Predicate P{flavor,CanonicalEqualityPred(co_var, t1, t2)};

    auto uv1 = unfilled_meta_type_var(t1);
    auto uv2 = unfilled_meta_type_var(t2);

    // REFL: tau ~ tau
    // NOTE: this does not currently handle foralls or constraints!
    if (same_type(t1,t2)) return {};

    auto tv1 = t1.to<Hs::TypeVar>();
    auto tv2 = t2.to<Hs::TypeVar>();

    if (uv1 and uv2)
    {
        if (cmp_less(*uv2,*uv1))
            return canonicalize_equality(co_var, flavor, t2, t1);
        else
        {
            return P;
        }
    }
    else if (uv1)
    {
        if (occurs_check(*uv1, t2))
        {
            failed.push_back(P);
            return {};
        }
        else
        {
            return P;
        }
    }
    else if (uv2)
    {
        return canonicalize_equality(co_var, flavor, t2, t1);
    }
    else if (tv1 and tv2)
    {
        if (*tv2 < *tv1)
            return canonicalize_equality(co_var, flavor, t2, t1);
        else
        {
            return P;
        }
    }
    else if (tv1)
    {
        if (occurs_check(*tv1, t2))
        {
            failed.push_back(P);
            return {};
        }
        else
        {
            return P;
        }
    }
    else if (tv2)
    {
        return canonicalize_equality(co_var, flavor, t2, t1);
    }
    else
    {
        // Right now the only TypeCon heads are data constructors,
        //   type synonyms, and variables.
        // Unlike GHC, we don't consider representational equality.
        // When we add type families, this will become more complicated.
        // See [Decomposing equality] in Tc/Solver/Canonical.hs
        while(auto s1 = is_type_synonym(t1))
            t1 = *s1;
        while(auto s2 = is_type_synonym(t2))
            t2 = *s2;

        if (auto tuple = t1.to<Hs::TupleType>())
            t1 = canonicalize_type(*tuple);
        else if (auto list = t1.to<Hs::ListType>())
            t1 = canonicalize_type(*list);

        if (auto tuple = t2.to<Hs::TupleType>())
            t2 = canonicalize_type(*tuple);
        else if (auto list = t2.to<Hs::ListType>())
            t2 = canonicalize_type(*list);

        auto [head1,args1] = decompose_type_apps(t1);
        auto [head2,args2] = decompose_type_apps(t2);

        if (args1.size() != args2.size())
        {
            // Is this too eager?
            failed.push_back(P);
            return {};
        }

        auto h1 = head1.to<Hs::TypeCon>();
        auto h2 = head2.to<Hs::TypeCon>();

        vector<Predicate> preds;
        if (h1 and h2)
        {
            if (*h1 == *h2)
            {
                // TDEC: T x1 y1 z1 = T x2 y2 z2
                // Fall through.
            }
            else
            {
                // FAILDEC: T x1 y1 z1 = S x2 y2 z2
                failed.push_back(P);
                return {};
            }
        }
        // We will need to do extra stuff here if either of the heads is a type family.
        else
        {
            work_list.push_back(Predicate(flavor,NonCanonicalPred(flavor, make_equality_constraint(head1, head2))));
        }

        // If we've gotten here, the heads are both injective, and might be equal.
        for(int i=0;i<args1.size();i++)
            work_list.push_back(Predicate(flavor,NonCanonicalPred(flavor, make_equality_constraint(args1[i],args2[i]))));

        return {};
    }

    std::abort();
}

bool Solver::canonicalize(Predicate& P)
{
    if (is_canonical(P)) return true;

    auto NCP = std::get<NonCanonicalPred>(P.pred);
    auto flavor = P.flavor;

    if (auto eq = Hs::is_equality_constraint(NCP.constraint))
    {
        auto& [t1, t2] = *eq;
        if (auto C = canonicalize_equality(NCP.dvar, flavor, t1, t2))
        {
            P = *C;
            return true;
        }
        else
            return false;
    }
    else
    {
        auto [head,args] = decompose_type_apps(NCP.constraint);
        assert(head.is_a<Hs::TypeCon>());
        auto klass = head.as_<Hs::TypeCon>();
        P = Predicate{flavor, CanonicalDictPred{NCP.dvar, klass, args}};

        return true;
    }
}

Change Solver::interact(const Predicate& P1, const Predicate& P2)
{
    assert(is_canonical(P1));
    assert(is_canonical(P2));

    // Don't allow wanteds to rewrite givens
    if (P1.flavor == Wanted and P2.flavor == Given) return Unchanged();

    auto eq1 = to<CanonicalEqualityPred>(P1.pred);
    auto eq2 = to<CanonicalEqualityPred>(P2.pred);

    auto dict1 = to<CanonicalDictPred>(P1.pred);
    auto dict2 = to<CanonicalDictPred>(P2.pred);

    if (eq1 and eq2)
    {
        auto [v1, t1a, t1b] = *eq1;
        auto uv1 = unfilled_meta_type_var(t1a);
        auto tv1 = t1a.to<Hs::TypeVar>();

        auto [v2, t2a, t2b] = *eq2;
        auto uv2 = unfilled_meta_type_var(t2a);
        auto tv2 = t2a.to<Hs::TypeVar>();

        // EQSAME: (tv1 ~ X1) + (tv1 ~ X2) -> (tv1 ~ X1) && (X1 ~ X2)
        if ((tv1 and tv2 and *tv1 == *tv2) or (uv1 and uv2 and *uv1 == *uv2))
        {
            work_list.push_back(Predicate{P2.flavor, NonCanonicalPred(eq2->co, make_equality_constraint(t1b, t2b))});
            return NonCanon();
        }
        // EQDIFF: (tv1 ~ X1) + (tv2 ~ X2) -> (tv1 ~ X1) && (tv2 ~ [tv1->X1]X2) if tv1 in ftv(X2)
        else if (tv1 or uv1)
        {
            if (auto t2b_subst = tv1?check_apply_subst({{*tv1, t1b}}, t2b):check_apply_subst({{*uv1, t1b}}, t2b))
            {
                return Changed{Predicate{P2.flavor, CanonicalEqualityPred(eq2->co, t2a, *t2b_subst)}};
            }
        }
    }
    // EQDICT: (tv1 ~ X1) + (D xs)     -> (tv1 ~ X1) && (D [tv1->X1]xs) if tv1 in ftv(xs)
    else if (eq1 and dict2)
    {
        auto [v1, t1a, t1b] = *eq1;
        auto uv1 = unfilled_meta_type_var(t1a);
        auto tv1 = t1a.to<Hs::TypeVar>();

        if (tv1 or uv1)
        {
            bool changed = false;
            CanonicalDictPred dict2_subst = *dict2;
            for(auto& class_arg: dict2_subst.args)
            {
                if (auto maybe_class_arg = tv1?check_apply_subst({{*tv1,t1b}}, class_arg):check_apply_subst({{*uv1,t1b}},class_arg))
                {
                    changed = true;
                    class_arg = *maybe_class_arg;
                }
            }
            if (changed)
                return Changed{Predicate(P2.flavor, dict2_subst)};
        }
    }
    else if (dict1 and dict2)
    {
        auto constraint1 = Hs::make_tyapps(dict1->klass, dict1->args);
        auto constraint2 = Hs::make_tyapps(dict2->klass, dict2->args);

        // DDICT:  (D xs)     + (D xs)     -> (D xs)
        if (same_type(constraint1, constraint2))
        {
            decls.push_back({dict2->dvar, dict1->dvar});
            return Solved();
        }
        // SUPER - not in the paper.
        else if (auto sdecls = entails_by_superclass({dict1->dvar,constraint1}, {dict2->dvar,constraint2}))
        {
            decls += *sdecls;
            return Solved();
        }
    }
    // EQFEQ:  (tv1 ~ X1) + (F xs ~ x) -> (tv1 ~ X1) && (F [tv1->X1]xs ~ [tv1 -> X1]x) if tv1 in ftv(xs,x)
    // FEQFEQ: (F xs ~ X1) + (F xs ~ X2) -> (F xs ~ X1) && (X1 ~ X2)

    return Unchanged();
}

std::optional<Reaction> Solver::interact_same(const Predicate& P1, const Predicate& P2)
{
    assert(is_canonical(P1));
    assert(is_canonical(P2));

    if (P1.flavor != P2.flavor) return {};
    auto flavor = P1.flavor;

    auto eq1 = to<CanonicalEqualityPred>(P1.pred);
    auto eq2 = to<CanonicalEqualityPred>(P2.pred);

    auto dict1 = to<CanonicalDictPred>(P1.pred);
    auto dict2 = to<CanonicalDictPred>(P2.pred);

    if (eq2 and not eq1)
        return interact_same(P2,P1);

    else if (eq1 and eq2)
    {
        auto [v1, t1a, t1b] = *eq1;
        auto uv1 = unfilled_meta_type_var(t1a);
        auto tv1 = t1a.to<Hs::TypeVar>();

        auto [v2, t2a, t2b] = *eq2;
        auto uv2 = unfilled_meta_type_var(t2a);
        auto tv2 = t2a.to<Hs::TypeVar>();

        // EQSAME: (tv1 ~ X1) + (tv1 ~ X2) -> (tv1 ~ X1) && (X1 ~ X2) 
        if ((tv1 and tv2 and *tv1 == *tv2) or (uv1 and uv2 and *uv1 == *uv2))
        {
            Predicate P3(flavor, NonCanonicalPred(eq2->co, make_equality_constraint(t1b, t2b)));
            work_list.push_back(P1);
            work_list.push_back(P3);
            return ReactSuccess();
        }
        // EQDIFF: (tv1 ~ X1) + (tv2 ~ X2) -> (tv1 ~ X1) && (tv2 ~ [tv1->X1]X2) if tv1 in ftv(X2)
        else if ((tv1 or uv1) and (tv2 or uv2))
        {
            if (auto t2b_subst = tv1?check_apply_subst({{*tv1, t1b}}, t2b):check_apply_subst({{*uv1, t1b}}, t2b))
            {
                Predicate P3(flavor, CanonicalEqualityPred(eq2->co, t2a, *t2b_subst));
                work_list.push_back(P1);
                work_list.push_back(P3);
                return ReactSuccess();
            }
            else if (auto t1b_subst = tv2?check_apply_subst({{*tv2, t2b}}, t1b):check_apply_subst({{*uv2, t2b}}, t1b))
            {
                Predicate P3(flavor, CanonicalEqualityPred(eq2->co, t1a, *t1b_subst));
                work_list.push_back(P3);
                work_list.push_back(P2);
                return ReactSuccess();
            }
        }
    }
    // EQDICT: (tv1 ~ X1) + (D xs)     -> (tv1 ~ X1) && (D [tv1->X1]xs) if tv1 in ftv(xs)
    else if (eq1 and dict2)
    {
        auto [v1, t1a, t1b] = *eq1;
        auto uv1 = unfilled_meta_type_var(t1a);
        auto tv1 = t1a.to<Hs::TypeVar>();

        if (tv1 or uv1)
        {
            bool changed = false;
            CanonicalDictPred dict2_subst = *dict2;
            for(auto& class_arg: dict2_subst.args)
            {
                if (auto maybe_class_arg = tv1?check_apply_subst({{*tv1,t1b}}, class_arg):check_apply_subst({{*uv1,t1b}},class_arg))
                {
                    changed = true;
                    class_arg = *maybe_class_arg;
                }
            }
            if (changed)
            {
                auto P3 = Predicate(flavor, dict2_subst);
                work_list.push_back(P1);
                work_list.push_back(P3);
                return ReactSuccess();
            }
        }
    }
    else if (dict1 and dict2)
    {
        auto constraint1 = Hs::make_tyapps(dict1->klass,dict1->args);
        auto constraint2 = Hs::make_tyapps(dict2->klass,dict2->args);

        // DDICT:  (D xs)     + (D xs)     -> (D xs)
        if (same_type(constraint1, constraint2))
        {
            decls.push_back({dict2->dvar, dict1->dvar});
            work_list.push_back(P1);
            return ReactSuccess();
        }
        // SUPER - not in the paper.
        else if (auto sdecls = entails_by_superclass({dict1->dvar,constraint1}, {dict2->dvar,constraint2}))
        {
            decls += *sdecls;
            work_list.push_back(P1);
            return ReactSuccess();
        }
        // SUPER - not in the paper.
        else if (auto sdecls = entails_by_superclass({dict2->dvar,constraint2}, {dict1->dvar,constraint1}))
        {
            decls += *sdecls;
            work_list.push_back(P2);
            return ReactSuccess();
        }
    }
    // EQFEQ:  (tv1 ~ X1) + (F xs ~ x) -> (tv1 ~ X1) && (F [tv1->X1]xs ~ [tv1 -> X1]x) if tv1 in ftv(xs,x)
    // FEQFEQ: (F xs ~ X1) + (F xs ~ X2) -> (F xs ~ X1) && (X1 ~ X2)
    
    // No reaction
    return {};
}

std::optional<Reaction> Solver::interact_g_w(const Predicate& P1, const Predicate& P2)
{
    assert(is_canonical(P1));
    assert(is_canonical(P2));

    if (P1.flavor == P2.flavor) return {};
    if (P1.flavor != Given) return interact_g_w(P2, P1);

    auto eq1 = to<CanonicalEqualityPred>(P1.pred);
    auto eq2 = to<CanonicalEqualityPred>(P2.pred);

    auto dict1 = to<CanonicalDictPred>(P1.pred);
    auto dict2 = to<CanonicalDictPred>(P2.pred);

    if (eq1 and eq2)
    {
        auto [v1, t1a, t1b] = *eq1;
        auto uv1 = unfilled_meta_type_var(t1a);
        auto tv1 = t1a.to<Hs::TypeVar>();

        auto [v2, t2a, t2b] = *eq2;
        auto uv2 = unfilled_meta_type_var(t2a);
        auto tv2 = t2a.to<Hs::TypeVar>();

        // SEQSAME: (tv1 ~ X1) simplifies (tv1 ~ X2) -> (X1 ~ X2) 
        if ((tv1 and tv2 and *tv1 == *tv2) or (uv1 and uv2 and *uv1 == *uv2))
        {
            Predicate P3(Wanted, NonCanonicalPred(eq2->co, make_equality_constraint(t1b, t2b)));
            work_list.push_back(P1);
            work_list.push_back(P3);
            return ReactSuccess();
        }
        // SEQDIFF: (tv1 ~ X1) simplifies (tv2 ~ X2) -> (tv2 ~ [tv1->X1]X2) if tv1 in ftv(X2)
        else if ((tv1 or uv1) and (tv2 or uv2))
        {
            if (auto t2b_subst = tv1?check_apply_subst({{*tv1, t1b}}, t2b):check_apply_subst({{*uv1, t1b}}, t2b))
            {
                Predicate P3(Wanted, NonCanonicalPred(eq2->co, make_equality_constraint(t2a, *t2b_subst)));
                work_list.push_back(P1);
                work_list.push_back(P3);
                return ReactSuccess();
            }
        }
    }
    // SEQDICT: (tv1 ~ X1) simplifies (D xs) -> (D [tv1->X1]xs) if tv1 in ftv(xs)
    else if (eq1 and dict2)
    {
        auto [v1, t1a, t1b] = *eq1;
        auto uv1 = unfilled_meta_type_var(t1a);
        auto tv1 = t1a.to<Hs::TypeVar>();

        bool changed = false;
        CanonicalDictPred dict2_subst = *dict2;
        for(auto& class_arg: dict2_subst.args)
        {
            if (auto maybe_class_arg = tv1?check_apply_subst({{*tv1,t1b}}, class_arg):check_apply_subst({{*uv1,t1b}},class_arg))
            {
                changed = true;
                class_arg = *maybe_class_arg;
            }
        }
        if (changed)
        {
            work_list.push_back(Predicate(Wanted, dict2_subst));
            return ReactSuccess();
        }
    }
    else if (dict1 and dict2)
    {
        auto constraint1 = Hs::make_tyapps(dict1->klass,dict1->args);
        auto constraint2 = Hs::make_tyapps(dict2->klass,dict2->args);

        // SDDICTG:  (D xs) simplifies (D xs)     -> empty
        if (same_type(constraint1, constraint2))
        {
            decls.push_back({dict2->dvar, dict1->dvar});
            work_list.push_back(P1);
            return ReactSuccess();
        }
        // SSUPER - not in the paper.
        else if (auto sdecls = entails_by_superclass({dict1->dvar,constraint1}, {dict2->dvar,constraint2}))
        {
            decls += *sdecls;
            work_list.push_back(P1);
            return ReactSuccess();
        }
    }
    // SEQFEQ:  (tv1 ~ X1) simplifies (F xs ~ x) -> (F [tv1->X1]xs ~ [tv1 -> X1]x) if tv1 in ftv(xs,x)
    // SFEQFEQ: (F xs ~ X1) simplifies (F xs ~ X2) -> (X1 ~ X2)

    return {};
}

// we need to have three results: No react, React<vector<Predicates>>, ReactFail
std::optional<Reaction> Solver::top_react(const Predicate& P)
{
    assert(is_canonical(P));

    if (auto dict = to<CanonicalDictPred>(P.pred))
    {
        // We DO get givens like Eq Ordering that have instances.
        // Should we be preventing such things from becoming givens, since we could
        //   derive them from an instance instead?

        // We don't use instances for givens.
        if (P.flavor == Given) return {};

        auto [dvar, klass, args] = *dict;
        auto constraint = Hs::make_tyapps(klass,args);

        if (auto inst = lookup_instance(constraint))
        {
            auto [dfun_exp, super_wanteds] = *inst;

            decls.push_back( { dvar, dfun_exp } );
            for(auto& pred: make_predicates(Wanted, super_wanteds))
                work_list.push_back( pred );

            return ReactSuccess();
        }
    }
    
    return {};
}

bool TypeChecker::is_touchable(const Hs::MetaTypeVar& mtv)
{
    assert(mtv.filled() or mtv.level() <= level);

    return not mtv.filled() and mtv.level() == level;
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

Core::Decls Solver::simplify(const LIE& givens, LIE& wanteds)
{
    if (wanteds.empty()) return {{}, {}};

    for(auto& [evar, constraint]: givens)
        work_list.push_back({Given, NonCanonicalPred(evar, constraint)});
    for(auto& [evar, constraint]: wanteds)
        work_list.push_back({Wanted, NonCanonicalPred(evar, constraint)});

//    std::cerr<<"---------------\n";
//    for(auto& w: work_list)
//        std::cerr<<"  "<<w.print()<<"\n";

    while(not work_list.empty())
    {
        auto p = work_list.back(); work_list.pop_back();

        // canonicalize
        if (not canonicalize(p)) continue;

        // rewrite and interact
        bool done = false;
        bool changed = true;
        while (changed and not done)
        {
            changed = false;
            for(auto& inert: inerts)
            {
                auto I = interact(inert, p);
                if (auto C = to<Changed>(I))
                {
                    p = C->P;
                    changed = true;
                }
                else if (to<Solved>(I) or to<NonCanon>(I))
                {
                    done = true;
                    break;
                }
            }
        }
        if (done) continue;

        // kick-out for inerts that are rewritten by p
        for(int i=0;i<inerts.size();i++)
        {
            auto I1 = interact(inerts[i], p);
            assert(to<Unchanged>(I1));

            auto I2 = interact(p, inerts[i]);
            if (not to<Unchanged>(I2))
            {
                // If its noncanon or solved, we don't want to put it back!
                if (auto C = to<Changed>(I2))
                    work_list.push_back(C->P);

                if (i+1 < inerts.size())
                    std::swap(inerts[i], inerts.back());

                inerts.pop_back();
                i--;
                continue;
            }
        }

        // top-level reactions
        if (top_react(p))
            continue;

        // we should only get this far if p is closed under rewriting, and unsolved.
        inerts.push_back(p);
    }

    if (not failed.empty())
    {
        myexception e("Unsolvable equations:\n");
        for(auto& f: failed)
            e<<"  "<<f.print()<<"\n";
        throw e;
    }

    // Split inert into substitution and remaining constraints
    wanteds.clear();
    vector<pair<Hs::MetaTypeVar,Hs::Type>> equations;
    for(auto& P: inerts)
    {
        assert(is_canonical(P));

        if (P.flavor == Given) continue;

        if (auto eq = to<CanonicalEqualityPred>(P.pred))
        {
            auto [_, t_a, t_b] = *eq;
            auto uv = unfilled_meta_type_var(t_a);
            if (uv and is_touchable(*uv) and not occurs_check(*uv, t_b))
                equations.push_back({*uv,t_b});
            else
                wanteds.push_back({eq->co, Hs::make_equality_constraint(t_a, t_b)});
        }
        else if (auto dict = to<CanonicalDictPred>(P.pred))
        {
            auto constraint = Hs::make_tyapps(dict->klass, dict->args);
            wanteds.push_back({dict->dvar, constraint});
        }
    }

    // Actually perform the substitution.
    for(auto& [uv,type]: equations)
    {
        if (not uv.filled())
            uv.fill(type);
    }

//    std::cerr<<" residual wanteds = \n";
//    for(auto& [v,c]: residual_wanteds)
//        std::cerr<<"  "<<v.print()<<" :  "<<c.print()<<"\n";

    return decls;
}

bool contains_equality_constraints(const LIE& givens)
{
    for(auto& [_,constraint]: givens)
        if (is_equality_constraint(constraint))
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
            auto sub_decls = tcs2.entails(sub_givens, implic->wanteds);

            // 5. Promote any level+1 meta-vars and complain about level+1 skolem vars.
            LIE lie_residual_keep;
            if (not contains_equality_constraints(implic->givens))
            {
                for(auto& [var, constraint]: implic->wanteds.simple)
                {
                    promote(constraint);
                    if (max_level(constraint) > level)
                        throw myexception()<<"skolem-escape in "<<constraint;
                    else if (intersects(free_type_variables(constraint), implic->tvs))
                        lie_residual_keep.push_back({var,constraint});
                    else
                    {
                        // If we've discovered a new simple wanted, then we need to run simplification again.
                        update = true;
                        new_wanteds.push_back({var,constraint});
                    }
                }
                implic->wanteds.simple.clear();
            }

            // 6. Issue collected warnings constraints that couldn't be derived from the givens.
            if (not lie_residual_keep.empty())
                throw myexception()<<"Can't derive constraints '"<<print(lie_residual_keep)<<"' from specified constraints '"<<print(givens)<<"'";

            // 7. Write newly discovered evidence bidings.
            *implic->evidence_binds += sub_decls;

            // 8. Keep implication if not empty.
            if (not implic->wanteds.empty())
                wanteds.implications.push_back( implic );
        }

        wanteds.simple += new_wanteds;
    } while(update);

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

std::vector<Predicate> make_predicates(ConstraintFlavor f, const std::vector<std::pair<Core::Var, Hs::Type>>& ps)
{
    vector<Predicate> predicates;
    for(auto& [cvar, constraint]: ps)
        predicates.push_back(Predicate(f,NonCanonicalPred(cvar,constraint)));
    return predicates;
}

Solver::Solver(const TypeChecker& tc)
    :TypeChecker(tc)
{
}
