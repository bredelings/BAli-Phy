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

optional<Core::Decls> typechecker_state::entails_by_superclass(const pair<Core::Var, Hs::Type>& given, const pair<Core::Var, Hs::Type>& wanted)
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

template <typename T>
std::optional<Core::Decls> typechecker_state::entails(const T& givens, const std::pair<Core::Var, Hs::Type>& wanted_pred)
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

// How does this relate to simplifying constraints?
pair<Core::Decls, LIE> typechecker_state::entails(const LIE& givens, const LIE& wanteds)
{
    Core::Decls decls;
    LIE residual_wanteds;

    for(int i=0;i<wanteds.size();i++)
    {
        auto& wanted = wanteds[i];

        auto preds = views::concat(wanteds | views::drop(i+1), residual_wanteds, givens);

        if (auto edecls = entails(preds, wanted))
            decls += *edecls;
        else
            residual_wanteds.push_back(wanted);
    }

    return {decls, residual_wanteds};
}

pair<Core::Decls, LIE> typechecker_state::simplify(const LIE& wanteds)
{
    return entails({}, wanteds);
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


