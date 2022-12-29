#include "typecheck.H"
#include "kindcheck.H"

#include "util/text.H"      // for ANSI colors
#include "util/set.H"       // for add( , )
#include "util/variant.H"   // for to< >()
#include "haskell/ids.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;

// Constraints for defaulting must be of the form K a (e.g. Num a) where a is a MetaTypeVar.
optional<TypeCon> simple_constraint_class_meta(const Type& constraint)
{
    auto [tycon, args] = decompose_type_apps(constraint);

    // Only one constrained type.
    if (args.size() != 1) return {};

    // The type is a typevar
    if (not args[0].is_a<MetaTypeVar>()) return {};

    // The constraint should be a TyCon, not (say) a variable.
    auto tc = tycon.to<TypeCon>();
    if (not tc) return {};

    return *tc;
}

// The defaulting criteria for an ambiguous type variable v are:
// 1. v appears only in constraints of the form C v , where C is a class
// 2. at least one of these classes is a numeric class, (that is, Num or a subclass of Num)
// 3. all of these classes are defined in the Prelude or a standard library (Figures 6.2–6.3 show the numeric classes, and Figure 6.1 shows the classes defined in the Prelude.)

bool
TypeChecker::candidates(const MetaTypeVar& tv, const LIE& tv_wanteds)
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
    for(auto& constraint: tv_wanteds)
    {
        // Fail if any of the predicates is not a simple constraint.
        auto tycon = simple_constraint_class_meta(constraint.pred);
        if (not tycon) return false;

        auto& name = unloc(tycon->name);
        if (num_classes.count(name))
            any_num = true;

        // Fail if any of the predicates are not in the standard prelude.
        if (not std_classes.count(name)) return false;
    }

    // Fail if none of the predicates is a numerical constraint
    if (not any_num) return false;

    for(auto& type: defaults() )
    {
        tv.fill(type);
        auto wanteds = WantedConstraints(tv_wanteds);
        entails({}, wanteds);
        if (wanteds.empty())
            return true;
        else
            tv.clear();
    }

    return false;
}

pair<LIE, map<MetaTypeVar, LIE>>
ambiguities(const LIE& lie)
{
    auto ambiguous_tvs = free_meta_type_variables(lie);

    // 1. Record the constraints WITH ambiguous type vars, by type var
    map<MetaTypeVar, LIE> ambiguities;
    for(auto& ambiguous_tv: ambiguous_tvs)
    {
        LIE lie_for_tv;
        for(auto& constraint: lie)
        {
            if (free_meta_type_variables(constraint.pred).count(ambiguous_tv))
                lie_for_tv.push_back(constraint);
        }
        if (not lie_for_tv.empty())
            ambiguities.insert({ambiguous_tv, lie_for_tv});
    }

    // 2. Find the constraints WITHOUT ambiguous type vars
    LIE unambiguous_preds;

    for(auto& constraint: lie)
    {
        auto ftvs = free_meta_type_variables(constraint.pred);
        if (not intersects(ftvs, ambiguous_tvs))
            unambiguous_preds.push_back(constraint);
    }

    return {unambiguous_preds, ambiguities};
}


// This is used both when generalizing let's in bind.cc, and in defaulting.
// When we are generalizing, we don't want to float out of implications with given equalities.
//   (float_past_equalities = false)
// But when defaulting we do.
//   (float_past_equalities = true)
LIE float_wanteds(bool float_past_equalities, const WantedConstraints& wanteds, const std::set<TypeVar>& trapping_tvs)
{
    // GHC doesn't float insoluble wanteds?
    // Then insoluble wanted must be marked...

    LIE floated;

    // 1. Float simple wanteds that don't contain any trapping tvs
    for(auto& simple: wanteds.simple)
        if (not intersects(free_type_variables(simple.pred), trapping_tvs))
            floated.push_back(simple);

    // 2. Try and float wanteds out of implications
    for(auto& implication: wanteds.implications)
    {
        // 2a. Any vars in the implication block floating too.
        auto trapping_tvs2 = trapping_tvs;
        for(auto& tv: implication->tvs)
            trapping_tvs2.insert(tv);

        // 2b. And if there's a given equality, we can't float past that.
        if (not float_past_equalities and contains_equality_constraints(implication->givens)) continue;

        // 2c. What to float?
        auto i_floated = float_wanteds(float_past_equalities, implication->wanteds, trapping_tvs2);

        // 2d. Append
        for(auto& wanted: i_floated)
            floated.push_back(wanted);
    }

    return floated;
}

bool TypeChecker::default_preds( WantedConstraints& wanted )
{
    Core::Decls decls;
    auto simple_wanteds = float_wanteds(true, wanted);
    auto [unambiguous_preds, ambiguous_preds_by_var] = ambiguities( simple_wanteds );

    bool progress = false;
    for(auto& [tv, preds]: ambiguous_preds_by_var)
        progress = progress or candidates(tv, preds);

    return progress;
}

string print_unqualified_id(const string& s)
{
    auto s2 = get_unqualified_name(s);
    if (is_haskell_sym(s2))
        s2 = "("+s2+")";
    return s2;
}

string print_unqualified_id(const Located<string>& ls)
{
    return print_unqualified_id(unloc(ls));
}

void TypeChecker::check_wanteds(const WantedConstraints& wanteds, const TypeCheckerContext& context)
{
    for(auto& wanted: wanteds.simple)
    {
        std::optional<yy::location> loc;
        Note e;
        e<<"Could not derive `"<<bold_green(print_unqualified(wanted.pred))<<ANSI::bold<<"`";
        if (auto occ = to<OccurrenceOrigin>(wanted.origin))
        {
            e<<" arising from a use of `"<<cyan(print_unqualified_id(occ->name))<<ANSI::bold<<"`";
            if (occ->name.loc)
            {
                loc = occ->name.loc;
                e<<" at "<<(*loc);
            }
        }
        record_error(loc, context, e);
    }

    for(auto& implic: wanteds.implications)
        check_wanteds(implic->wanteds,implic->context);
}

Core::Decls TypeChecker::simplify_and_default_top_level()
{
    auto top_simplify_decls = entails( {}, current_wanteds() );

    // Defaulting just sets the metatypevar, it doesn't actually move or remove the constraints.
    while(default_preds( current_wanteds() ))
        top_simplify_decls += entails( {}, current_wanteds() );
        
    // Here we should complain about unsolved abiguities...
    check_wanteds(current_wanteds(),{});

//    std::cerr<<"GVE (all after defaulting):\n";
//    for(auto& [x,t]: state.gve)
//    {
//        std::cerr<<x<<" :: "<<alphabetize_type(t)<<"\n";
//        std::cerr<<x<<" = "<<e<<"\n\n\n";
//    }
//    std::cerr<<"\n";

    return top_simplify_decls;
}

