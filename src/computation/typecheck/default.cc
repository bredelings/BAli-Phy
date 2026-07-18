#include "typecheck.H"
#include "kindcheck.H"

#include "util/variant.H"   // for to< >()
#include "haskell/ids.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;

void TypeChecker::get_defaults(const Hs::ModuleDecls& M)
{
    auto Num = find_prelude_tycon("Num");
    auto IsString = TypeCon("Data.String.IsString");
    bool extended = this_mod().language_extensions.has_extension(LangExt::ExtendedDefaultRules);
    bool ovl_string = this_mod().language_extensions.has_extension(LangExt::OverloadedStrings);

    vector<TypeCon> unnamed_default_classes = {Num};
    if (ovl_string)
        unnamed_default_classes.push_back(IsString);
    if (extended)
    {
        unnamed_default_classes.push_back(TypeCon("Text.Show.Show"));
        unnamed_default_classes.push_back(TypeCon("Data.Eq.Eq"));
        unnamed_default_classes.push_back(TypeCon("Data.Ord.Ord"));
        unnamed_default_classes.push_back(TypeCon("Data.Foldable.Foldable"));
        unnamed_default_classes.push_back(TypeCon("Data.Traversable.Traversable"));
    }

    for(auto& [loc,default_decl]: M.default_decls)
    {
        vector<Type> default_types;
        for(auto& type: default_decl.types)
            default_types.push_back(check_type(type));

        vector<TypeCon> default_classes;

        if (default_decl.maybe_class)
        {
            bool named_defaults = this_mod().language_extensions.has_extension(LangExt::NamedDefaults);
            if (not named_defaults)
            {
                record_error(default_decl.maybe_class->loc,
                             Note() <<"Class-specific defaults only allowed with extension NamedDefaults" );
                continue;
            }
            else if (auto d = find_tycon(unloc(*default_decl.maybe_class)))
                default_classes.push_back(*d);
            else
            {
                record_error(default_decl.maybe_class->loc, Note()<<"Unknown data type");
                continue;
            }
        }
        else
            default_classes = unnamed_default_classes;

        for(const auto& dclass: default_classes)
        {
            if (default_env().count(dclass))
                record_error(loc, Note() <<"Duplicate default declaration." );
            else
            {
                // Check that the data types are all data types and instances of the class?
                default_env().insert({dclass, default_types});
            }
        }
    }

    if (not default_env().count(Num))
    {
        auto Integer = find_prelude_tycon("Integer");
        auto Double = find_prelude_tycon("Double");
        default_env().insert({Num, {Integer, Double}});
    }

    if (not default_env().count(IsString) and ovl_string)
    {
        Type String = list_type( char_type() );
        default_env().insert({IsString, {String}});
    }

    if (extended)
    {
        auto Show = TypeCon("Text.Show.Show");
        auto Eq = TypeCon("Data.Eq.Eq");
        auto Foldable = TypeCon("Data.Foldable.Foldable");
        auto Integer = find_prelude_tycon("Integer");
        auto Double = find_prelude_tycon("Double");

        if (not default_env().count(Show))
            default_env().insert({Show, {tuple_type({}), Integer, Double}});
        if (not default_env().count(Eq))
            default_env().insert({Eq, {tuple_type({}), Integer, Double}});
        if (not default_env().count(Foldable))
            default_env().insert({Foldable, {list_tycon()}});
    }
}

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

// Default an ambiguous variable from the declarations attached to its classes. Without ExtendedDefaultRules,
// every constraint must also be unary and belong to a standard or explicitly defaulted class.

bool
TypeChecker::candidates(const MetaTypeVar& tv, const LIE& tv_wanteds)
{
    bool extended = this_mod().language_extensions.has_extension(LangExt::ExtendedDefaultRules);

    set<string> std_classes = {"Compiler.Num.Num", "Compiler.Integral.Integral", "Compiler.Floating.Floating",
                               "Compiler.Fractional.Fractional", "Compiler.Real.Real",
                               "Compiler.RealFloat.RealFloat",
                               "Compiler.RealFrac.RealFrac", "Data.Eq.Eq", "Data.Ord.Ord", "Text.Show.Show",
                               "Text.Read.Read", "Compiler.Enum.Bounded", "Compiler.Enum.Enum", "Data.Ix.Ix",
                               "Data.Functor.Functor", "Control.Applicative.Applicative",
                               "Control.Applicative.Alternative", "Control.Monad.Monad",
                               "Control.Monad.MonadFail",
                               "Data.Semigroup.Semigroup", "Data.Monoid.Monoid", "Data.String.IsString",
                               "Data.Foldable.Foldable", "Data.Traversable.Traversable"};

    set<TypeCon> proposal_classes;
    vector<Type> pending_superclasses;
    for(auto& constraint: tv_wanteds)
    {
        if (auto tycon = simple_constraint_class_meta(constraint.pred))
        {
            proposal_classes.insert(*tycon);
            pending_superclasses.push_back(constraint.pred);
            bool has_defaults = default_env().contains(*tycon);

            if (not extended and not has_defaults and not std_classes.count(tycon->name))
                return false;
        }
        else if (not extended)
            return false;
    }

    // Recover unary superclass constraints that Bali-Phy's solver may already have discharged.
    while(not pending_superclasses.empty())
    {
        auto constraint = pending_superclasses.back();
        pending_superclasses.pop_back();
        for(const auto& [_, superclass]: superclass_constraints(constraint))
        {
            auto superclass_tycon = simple_constraint_class_meta(superclass);
            if (superclass_tycon and proposal_classes.insert(*superclass_tycon).second)
                pending_superclasses.push_back(superclass);
        }
    }

    optional<Type> proposal;
    for(const auto& dclass: proposal_classes)
    {
        auto defaults = default_env().find(dclass);
        if (defaults == default_env().end())
            continue;

        optional<Type> class_proposal;
        for(const auto& type: defaults->second)
        {
            tv.fill(type);
            auto wanteds = WantedConstraints(tv_wanteds);
            entails({}, wanteds);
            bool accepted = wanteds.empty();
            tv.clear();

            if (accepted)
            {
                class_proposal = type;
                break;
            }
        }

        if (not class_proposal)
            continue;
        if (proposal and not same_type(*proposal, *class_proposal))
            return false;
        proposal = *class_proposal;
    }

    if (proposal)
    {
        tv.fill(*proposal);
        return true;
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
    auto simple_wanteds = float_wanteds(true, wanted);
    auto [unambiguous_preds, ambiguous_preds_by_var] = ambiguities( simple_wanteds );

    bool progress = false;
    for(auto& [tv, preds]: ambiguous_preds_by_var)
        progress = progress or candidates(tv, preds);

    return progress;
}

Core::Decls<> TypeChecker::simplify_and_default_top_level()
{
    auto top_simplify_decls = entails( {}, current_wanteds() );

    // Defaulting just sets the metatypevar, it doesn't actually move or remove the constraints.
    while(default_preds( current_wanteds() ))
        top_simplify_decls += entails( {}, current_wanteds() );
        
    // Here we should complain about unsolved abiguities...
    check_wanteds(current_wanteds());

    return top_simplify_decls;
}
