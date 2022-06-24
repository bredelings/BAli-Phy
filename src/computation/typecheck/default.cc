#include "typecheck.H"
#include "kindcheck.H"

#include "util/set.H"   // for add( , )

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;

// Constraints for defaulting must be of the form K a (e.g. Num a)
optional<Hs::TypeCon> simple_constraint_class(const Hs::Type& constraint)
{
    auto [tycon, args] = Hs::decompose_type_apps(constraint);

    // Only one constrained type.
    if (args.size() != 1) return {};

    // The type is a typevar
    if (not args[0].is_a<Hs::TypeVar>()) return {};

    // The constraint should be a TyCon, not (say) a variable.
    auto tc = tycon.to<Hs::TypeCon>();
    if (not tc) return {};

    return *tc;
}

// The defaulting criteria for an ambiguous type variable v are:
// 1. v appears only in constraints of the form C v , where C is a class
// 2. at least one of these classes is a numeric class, (that is, Num or a subclass of Num)
// 3. all of these classes are defined in the Prelude or a standard library (Figures 6.2–6.3 show the numeric classes, and Figure 6.1 shows the classes defined in the Prelude.)

optional<tuple<u_substitution_t, Hs::Binds>>
typechecker_state::candidates(const Hs::MetaTypeVar& tv, const local_instance_env& tv_lie)
{
    set<string> num_classes_ = {"Num", "Integral", "Floating", "Fractional", "Real", "RealFloat", "RealFrac"};
    set<string> std_classes_ = {"Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix", "Functor", "Monad", "MonadPlus"};
    add(std_classes_, num_classes_);

    set<string> num_classes;
    set<string> std_classes;
    for(auto& cls: num_classes_)
        num_classes.insert( find_prelude_tycon_name(cls) );

    for(auto& cls: std_classes_)
        std_classes.insert( find_prelude_tycon_name(cls) );

    bool any_num = false;
    for(auto& [dvar,constraint]: tv_lie)
    {
        // Fail if any of the predicates is not a simple constraint.
        auto tycon = simple_constraint_class(constraint);
        if (not tycon) return {};

        auto& name = unloc(tycon->name);
        if (num_classes.count(name))
            any_num = true;

        // Fail if any of the predicates are not in the standard prelude.
        if (not std_classes.count(name)) return {};
    }

    // Fail if none of the predicates is a numerical constraint
    if (not any_num) return {};

    for(auto& type: defaults() )
    {
        u_substitution_t s;
        s = s.insert({tv, type});
        auto [decls, _, failed_constraints] = entails({}, apply_subst(s, tv_lie));
        if (failed_constraints.empty())
            return pair(s, Hs::Binds({decls}));
    }

    return {};
}

pair<local_instance_env, map<Hs::MetaTypeVar, local_instance_env>>
ambiguities(const set<Hs::MetaTypeVar>& tvs1, const set<Hs::MetaTypeVar>& tvs2, local_instance_env lie)
{
    // The input lie MUST be substituted to find its free type vars!
    // lie = apply_current_subst(lie);
    auto ambiguous_tvs = free_meta_type_variables(lie) - tvs1 - tvs2;

    // 1. Record the constraints WITH ambiguous type vars, by type var
    map<Hs::MetaTypeVar,local_instance_env> ambiguities;
    for(auto& ambiguous_tv: ambiguous_tvs)
    {
        local_instance_env lie_for_tv;
        for(auto& [dvar,constraint]: lie)
        {
            if (free_meta_type_variables(constraint).count(ambiguous_tv))
                lie_for_tv = lie_for_tv.insert({dvar,constraint});
        }
        if (not lie_for_tv.empty())
            ambiguities.insert({ambiguous_tv, lie_for_tv});
    }

    // 2. Find the constraints WITHOUT ambiguous type vars
    local_instance_env unambiguous_preds;

    for(auto& [dvar, constraint]: lie)
    {
        auto ftvs = free_meta_type_variables(constraint);
        if (not intersects(ftvs, ambiguous_tvs))
            unambiguous_preds = unambiguous_preds.insert({dvar, constraint});
    }

    return {unambiguous_preds, ambiguities};
}


tuple<u_substitution_t, Hs::Binds, local_instance_env>
typechecker_state::default_preds( const set<Hs::MetaTypeVar>& fixed_tvs,
                                  const set<Hs::MetaTypeVar>& referenced_tvs,
                                  const local_instance_env& lie)
{
    u_substitution_t s;
    Hs::Binds binds;
    auto [unambiguous_preds, ambiguous_preds_by_var] = ambiguities(fixed_tvs, referenced_tvs, lie);

    for(auto& [tv, preds]: ambiguous_preds_by_var)
    {
        auto result = candidates(tv, preds);

        if (not result)
        {
            auto e = myexception()<<"Ambiguous type variable '"<<tv<<"' in classes: ";
            for(auto& [dvar,constraint]: preds)
                e<<constraint<<" ";
            throw e;
        }
        auto& [s1, binds1] = *result;

        auto tmp = combine(s, s1);
        assert(tmp);
        s = *tmp; // These constraints should be on separate variables, and should not interact.

        // Each binds should be independent of the others, so order should not matter.
        ranges::insert(binds, binds.end(), binds1);
    }

    return {s, binds, unambiguous_preds};
}

Hs::Binds typechecker_state::simplify_and_default_top_level()
{
    auto top_simplify_binds = reduce_current_lie();

    auto [s, default_binds, unambiguous_preds] = default_preds({}, {}, current_lie());
    assert(unambiguous_preds.empty());

    // Record the substitution, since it can affect types.
    bool ok = add_substitution(s);
    assert(ok);

    // Clear the LIE, which should now be empty.
    current_lie() = {};

    gve = apply_current_subst(gve);

//    std::cerr<<"GVE (all after defaulting):\n";
//    for(auto& [x,t]: state.gve)
//    {
//        std::cerr<<x<<" :: "<<alphabetize_type(t)<<"\n";
//        std::cerr<<x<<" = "<<e<<"\n\n\n";
//    }
//    std::cerr<<"\n";

    ranges::insert(top_simplify_binds, top_simplify_binds.end(), default_binds);
    return top_simplify_binds;
}

