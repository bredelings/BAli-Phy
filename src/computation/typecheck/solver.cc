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
    for(auto& [name, type]: instance_env() )
    {
        auto [_, wanteds, instance_head] = instantiate(type);

        assert(not constraint_is_hnf(instance_head));

        auto s = ::maybe_match(instance_head, constraint);

        // This instance doesn't match.
        if (not s) continue;

        for(auto& [dvar, instance_constraint]: wanteds)
            instance_constraint = apply_subst(*s, instance_constraint);

        auto dfun = Core::Var({noloc, name});

        Hs::Expression dfun_exp = dfun;
        if (wanteds.size())
            dfun_exp = Hs::ApplyExp(dfun, vars_from_lie<Hs::Expression>(wanteds));

        return {{dfun_exp, wanteds}};
    }
    return {};
}

pair<Core::Decls,local_instance_env> typechecker_state::toHnf(const ID& name, const Hs::Type& constraint)
{
    Core::Decls decls;
    local_instance_env lie;
    if (constraint_is_hnf(constraint))
    {
        lie = lie.insert({name, constraint});
    }
    else
    {
        local_instance_env lie2;

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
        for(auto& [dvar, constraint]: wanteds)
            lie2 = lie2.insert({unloc(dvar.name), constraint});

        Hs::Var dvar({noloc, name});


        // 3. Finally, we may need to further simplify the new LIE
        auto [decls2, lie3] = toHnfs(lie2);

        lie = lie3;
        decls = decls2;
        decls.push_back(Hs::simple_decl(dvar, dfun_exp));
    }
    return {decls,lie};
}

pair<Core::Decls, local_instance_env>
typechecker_state::toHnfs(const local_instance_env& lie_in)
{
    Core::Decls decls_out;
    local_instance_env lie_out;
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
vector<pair<string, Hs::Type>> typechecker_state::superclass_constraints(const Hs::Type& constraint)
{
    vector<pair<string, Hs::Type>> constraints;

    auto class_name = get_full_class_name_from_constraint(constraint);

    // Fixme... since we know the class name, can we simplify superclass_extractors???
    for(auto& [name, type]: class_env().at(class_name).superclass_extractors)
    {
        // forall a.Klass a => Superklass a
        auto [_, wanteds, superclass_constraint] = instantiate(type);

        assert(constraint_is_hnf(superclass_constraint));

        assert(wanteds.size() == 1);

        auto class_constraint = wanteds[0].second;
        auto s = ::maybe_match(class_constraint, constraint);

        // The premise doesn't match the current class;
        if (not s) continue;

        superclass_constraint = apply_subst(*s, superclass_constraint);

        constraints.push_back( { name, superclass_constraint } );
    }

    return constraints;
}

// We are trying to eliminate the *first* argument.
optional<vector<string>> typechecker_state::is_superclass_of(const Hs::Type& constraint1, const Hs::Type& constraint2)
{
    vector<string> extractors;
    if (same_type(constraint1, constraint2))
        return extractors;
    else
    {
        // dvar1 :: constraint1 => dvar3 :: constraint3 => dvar2 :: constraint2
        for(auto& [name, constraint3]: superclass_constraints(constraint2))
        {
            if (auto extractors2 = is_superclass_of(constraint1, constraint3))
            {
                extractors = std::move(*extractors2);
                extractors.push_back(name);
                return extractors;
            }
        }
        return {};
    }
}

optional<Core::Decls> typechecker_state::entails_by_superclass(const pair<string, Hs::Type>& to_keep, const pair<string, Hs::Type>& to_remove)
{
    auto& [dvar_to_keep_name, constraint_to_keep] = to_keep;
    auto& [dvar_to_remove_name, constraint_to_remove] = to_remove;

    Hs::Var dvar_to_keep({noloc, dvar_to_keep_name});
    Hs::Var dvar_to_remove({noloc, dvar_to_remove_name});

    return entails_by_superclass({dvar_to_keep, constraint_to_keep}, {dvar_to_remove, constraint_to_remove});
}

optional<Core::Decls> typechecker_state::entails_by_superclass(const pair<Core::Var, Hs::Type>& to_keep, const pair<Core::Var, Hs::Type>& to_remove)
{
    auto& [dvar_to_keep, constraint_to_keep] = to_keep;
    auto& [dvar_to_remove, constraint_to_remove] = to_remove;

    if (auto extractors = is_superclass_of(constraint_to_remove, constraint_to_keep))
    {
        Core::Exp dict_exp = dvar_to_keep;
        for(auto& extractor: *extractors | views::reverse)
        {
            Core::Var get_dict({noloc, extractor});
            dict_exp = {get_dict, dict_exp};
        }

        // dvar_to_remove = extractor[n] extractor[n-1] ... extractor[0] dvar_to_keep
        return Hs::Decls( { Hs::simple_decl(dvar_to_remove, dict_exp) } );
    }
    else
        return {};
}

// How does this relate to simplifying constraints?
tuple<Hs::Binds, LIE, LIE> typechecker_state::entails(const LIE& lie1, const LIE& lie2)
{
    Hs::Binds binds;
    LIE failed_constraints;
    LIE entailed_constraints;

    for(auto& constraint: lie2)
    {
        auto binds1 = entails(lie1, constraint);
        if (not binds1)
            failed_constraints.push_back(constraint);
        else
        {
            entailed_constraints.push_back(constraint);
            ranges::insert(binds, binds.begin(), *binds1);
        }
    }
    return {binds, entailed_constraints, failed_constraints};
}

// How does this relate to simplifying constraints?
tuple<Hs::Binds, local_instance_env, local_instance_env> typechecker_state::entails(const local_instance_env& lie1, const local_instance_env& lie2)
{
    Hs::Binds binds;
    local_instance_env failed_constraints;
    local_instance_env entailed_constraints;

    for(auto& constraint: lie2)
    {
        auto binds1 = entails(lie1, constraint);
        if (not binds1)
            failed_constraints = failed_constraints.insert(constraint);
        else
        {
            entailed_constraints = entailed_constraints.insert(constraint);
            ranges::insert(binds, binds.begin(), *binds1);
        }
    }
    return {binds, entailed_constraints, failed_constraints};
}

pair<Hs::Binds, local_instance_env> typechecker_state::simplify(const local_instance_env& lie)
{
    Hs::Binds binds_out;
    local_instance_env lie_out;
    vector<pair<string,Hs::Type>> lie_vec;
    for(auto& entry: lie)
       lie_vec.push_back(entry);
    vector<pair<string,Hs::Type>> checked;

    for(int i=0;i<lie_vec.size();i++)
    {
        auto& pred = lie_vec[i];
        auto preds = views::concat(lie_vec | views::drop(i+1), checked);
        if (auto new_binds = entails(preds, pred))
            ranges::insert(binds_out, binds_out.begin(), *new_binds);
        else
            checked.push_back(lie_vec[i]);
    }

    for(auto& var_equals_constraint: checked)
        lie_out = lie_out.insert(var_equals_constraint);

    return {binds_out, lie_out};
}

pair<Hs::Binds, local_instance_env> typechecker_state::reduce(const local_instance_env& lie)
{
    auto [decls1, lie1] = toHnfs(lie);

    auto [binds2, lie2] = simplify(lie1);

    auto binds = binds2;
    binds.push_back(decls1);

    return {binds, lie2};
}

Hs::Binds typechecker_state::reduce_current_lie()
{
    auto& lie = current_lie();

    lie = apply_current_subst( lie );

    auto [binds, new_lie] = reduce( lie );

    lie = new_lie;

    return binds;
}


