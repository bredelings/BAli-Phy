#include "typecheck.H"
#include "kindcheck.H"

#include "util/set.H"

#include <range/v3/all.hpp>

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;

namespace views = ranges::views;

bool type_is_hnf(const Hs::Type& type)
{
    auto [head,args] = Hs::decompose_type_apps(type);

    if (head.is_a<Hs::TypeVar>())
        return true;
    else if (head.is_a<Hs::MetaTypeVar>())
        return true;
    else if (head.is_a<Hs::TypeCon>())
        return false;
    else if (head.is_a<Hs::ListType>())
        return false;
    else if (head.is_a<Hs::TupleType>())
        return false;
    else
        std::abort();
}

// OK:     K a, K (a b), K (a [b]), etc. OK
// NOT OK: K [a], K (a,b), etc. NOT OK.
// Question: for multiparameter type classes, how about i.e. `K Int a`?
bool constraint_is_hnf(const Hs::Type& constraint)
{
    auto [class_con, args] = Hs::decompose_type_apps(constraint);
    for(auto& arg: args)
        if (not type_is_hnf(arg))
            return false;
    return true;
}


// 1. An instance looks like (forall as.Q1(as) => Q2(as))
// 2. We have some constraints Q3.
// 3. We instantiate the instance with substitutions [as->gs].
// 4. We'd like to check if Q3 ~ [as->gs]Q2, where only the variables gs are allowed to be unified.
// 5. If we give the unification variables gs a higher level, can we guarantee that only
//    gs will be constrained?
// 6. Actually, I don't think so... Suppose that the instance is (Eq Int) and the constraint is
//    Eq a.
// 7. Unless we actually FORBID unification of variables at any higher level, then this won't work.
// 8. Simply forbidding substitution to a deeper depth won't cut it.

optional<pair<Core::Exp,LIE>> typechecker_state::lookup_instance(const Hs::Type& constraint)
{
    for(auto& [dfun, type]: instance_env() )
    {
        auto [_, wanteds, instance_head] = instantiate(type);

        assert(not constraint_is_hnf(instance_head));

        if (not maybe_match(instance_head, constraint)) continue;

        auto dfun_exp = Core::Apply(dfun, vars_from_lie<Core::Exp>(wanteds));

        return {{dfun_exp, wanteds}};
    }
    return {};
}

pair<Core::Decls, LIE> typechecker_state::toHnf(const Core::Var& dvar, const Hs::Type& constraint)
{
    Core::Decls decls;
    LIE lie;
    if (constraint_is_hnf(constraint))
    {
        lie.push_back({dvar, constraint});
    }
    else
    {
        LIE lie2;

        // 1. find the (single) matching instance or fail.
        // The instance will be of the form
        //     dfun :: forall  a1 a2 a3. (constraint, constraint, constraint) => K (T b1 b2 b3)
        // We need to substitute into this definition to make K (T b1 b2 b3) == constraint.
        auto instance = lookup_instance(constraint);
        if (not instance)
            throw myexception()<<"No instance for '"<<constraint<<"'";

        auto [dfun_exp, wanteds] = *instance;

        // 2. We need to make up new dvar names for all the input constraints.
        // Then we would add to the decls:
        //     dvar_orig = dfun dvar_new1 dvar_new2 dvar_new3
        // And we would get a NEW lie:
        //     dvar_new1 :: constraint1
        //     dvar_new2 :: constraint2
        //     dvar_new3 :: constraint3
        for(auto& [wdvar, wconstraint]: wanteds)
            lie2.push_back({wdvar, wconstraint});


        // 3. Finally, we may need to further simplify the new LIE
        auto [decls2, lie3] = toHnfs(lie2);

        lie = lie3;
        decls = decls2;
        decls.push_back( {dvar, dfun_exp} );
    }
    return {decls,lie};
}

pair<Core::Decls, LIE>
typechecker_state::toHnfs(const LIE& lie_in)
{
    Core::Decls decls_out;
    LIE lie_out;
    for(auto& [name, constraint]: lie_in)
    {
        auto [decls2, lie2] = toHnf(name, constraint);

        decls_out += decls2;
        lie_out += lie2;
    }
    return {decls_out, lie_out};
}

// FIXME: there should be a `const` way of getting these.
// FIXME: instantiate is not constant though.
// FIXME: we shouldn't need fresh type vars if the type is unambiguous though.
vector<pair<Core::Var, Hs::Type>> typechecker_state::superclass_constraints(const Hs::Type& constraint)
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
optional<vector<Core::Var>> typechecker_state::is_superclass_of(const Hs::Type& constraint1, const Hs::Type& constraint2)
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

optional<Core::Decls> typechecker_state::entails_by_superclass(const pair<Core::Var, Hs::Type>& to_keep, const pair<Core::Var, Hs::Type>& to_remove)
{
    auto& [dvar_to_keep, constraint_to_keep] = to_keep;
    auto& [dvar_to_remove, constraint_to_remove] = to_remove;

    if (auto extractors = is_superclass_of(constraint_to_remove, constraint_to_keep))
    {
        Core::Exp dict_exp = dvar_to_keep;
        for(auto& extractor: *extractors | views::reverse)
            dict_exp = {extractor, dict_exp};

        // dvar_to_remove = extractor[n] extractor[n-1] ... extractor[0] dvar_to_keep
        return Core::Decls( { pair(dvar_to_remove, dict_exp) } );
    }
    else
        return {};
}

// How does this relate to simplifying constraints?
tuple<Core::Decls, LIE, LIE> typechecker_state::entails(const LIE& lie1, const LIE& lie2)
{
    Core::Decls decls;
    LIE failed_constraints;
    LIE entailed_constraints;

    for(auto& constraint: lie2)
    {
        auto decls1 = entails(lie1, constraint);
        if (not decls1)
            failed_constraints.push_back(constraint);
        else
        {
            entailed_constraints.push_back(constraint);
            decls = *decls1 + decls;
        }
    }
    return {decls, entailed_constraints, failed_constraints};
}

pair<Core::Decls, LIE> typechecker_state::simplify(const LIE& lie)
{
    Core::Decls decls_out;
    LIE lie_out;
    LIE checked;

    for(int i=0;i<lie.size();i++)
    {
        auto& pred = lie[i];
        auto preds = views::concat(lie | views::drop(i+1), checked);
        if (auto edecls = entails(preds, pred))
            decls_out += *edecls;
        else
            lie_out.push_back(pred);
    }

    return {decls_out, lie_out};
}

pair<Core::Decls, LIE> typechecker_state::reduce(const LIE& lie)
{
    auto [decls1, lie1] = toHnfs(lie);

    auto [decls2, lie2] = simplify(lie1);

    return {decls2 + decls1, lie2};
}

Core::Decls typechecker_state::reduce_current_lie()
{
    auto& lie = current_lie();

    auto [decls, new_lie] = reduce( lie );

    lie = new_lie;

    return decls;
}


