#include "typecheck.H"
#include "kindcheck.H"
#include "parser/rename.H"

#include <set>

#include <range/v3/all.hpp>

#include "parser/haskell.H"

#include "immer/map.hpp" // for immer::map

#include "util/set.H"

#include "util/graph.H" // for get_ordered_strong_components( )

#include "computation/expression/apply.H"
#include "computation/expression/tuple.H" // for is_tuple_name( )
#include "computation/operation.H" // for is_non_apply_op( )
#include "computation/module.H"    // for is_qualified_symbol( ), get_module_name( )

#include "unify.H"

#include "alphabetize.H" // for alphabetize_type( ).

namespace views = ranges::views;

using std::vector;
using std::string;
using std::optional;
using std::map;
using std::set;
using std::pair;
using std::shared_ptr;
using std::tuple;

/*
  NOTE:
  - Type signatures with constraints are forbidden for variables declared in PatBind's!!!
    This might mean that we can simply instantiate the signatures inside the pattern -- and we 
       can issue an error message if there are any constraints.

  NOTE: It also looks like GHC DOES use 1-way matching -- but it implements it as a bool flag.

  Done: 
  * Check that constraints in classes only mention type vars.
  * Check that constraints in instance heads are of the form Class (Tycon a1 a2 a3..)
  * Switch from substitutions to constraints (i.e. from compose( ) to combine( ) ).
  * Add a substitution to the typechecker_state, instead of returning substitutions from every call.
  * We no longer need to keep substituting into the type.
  * Handle exp :: type expressions.
  * Monomorphism restriction.
  * Defaulting.
  * Make defaulting happen at every binding.
  * Make a version of unification that returns optional instead of throwing.
  * Make a stack of LIEs.
  * Don't substitute into LIEs / LVEs / GVEs until we need to.
  * Handle a :: Num a => Char in (a,b) = ('a',1)
  * Partially handle polymorphic recursion.
  * Split decls into pieces.
  * Handle expression :: type
  * Handle explicit signatures in fundecls / simple-pattern bindings.
  * Handle explicit signatures in pattern bindings.
  * Kind-check explicit type signatures.

  TODO:
  1. Reject unification of variables, tycons, etc with different kinds.
     - Ensure that all ForallType binders have kinds.
     - Assign kinds to all TypeCons.... OR look it up in the symbol table when we need to!
  2. Process type signatures for ambiguity and type synonyms.
  3. Change the type of class methods to forall a.C a => (forall b. ctxt => body)
  4. Record impedance-matching info on GenBind
     - unused types
     - defaulted types
     - defaulted dictionaries.
  5. Write more-general impedance-matching code.
  6. Avoid a space leak with polymorphic recursion in cases like factorial.
     Do not create new dictionaries for each call at the same type.
  7. Check that constraints in instance contexts satisfy the "paterson conditions"
  8. How do we EXPORT stuff?
     - We already handle exporting of names and stuff in add_local_symbols( ).  We need it for renaming.
     - This includes classes, methods, data types, constructors, type synonyms, etc.
     - Can we just paste on the type info for the variables, and the kind info for the classes?
     - How do you export instances, though?
  9. Make functions to handle instance declarations from Figure 12.
     - infer_type_for_instance1 creates a dfun with the type: forall a.Constaints(a) => Class a
     - the dfun definition would look like:
       + dfun dict1 .. dictn = ClassConstructor dict1 ... dictn method1 method2 ... methodn
                where method[1] = body[1]; method[2] = body[2]; ... method[n] = body[n]
       + how do we efficiently implement recursive class methods?
  10. Handle export subspecs Datatype(constructor...) and Class(method...)
  11. Make AST nodes for dictionary abstraction and dictionary application.
     - \(dicts::theta) -> monobinds in binds
     - exp <dicts>
     - (superdicts, methods)
      We can then desugar these expressions to EITHER multiple dictionaries OR a tuple of dictionaries.
      Can we desugar the dictionary for class K to a data type K?
  12. Implement fromInt and fromRational
  13. Implement literal strings.  Given them type [Char] and turn them into lists during desugaring.
      Do I need to handle LiteralString patterns, then?
  14. Check that there are no cycles in the class hierarchy.
  15. Emit code for instances and check if there are instances for the context.
  16. Handle constraints on constructors.
  17. Remove the constraint from EmptySet
  18. Add basic error reporting.
  19. How do we handle things like Prelude.Num, Prelude.Enum, Prelude.fromInt, etc.
      Right now, maybe we can pick a Num / fromInt from the local scope, instead?
      This might require passing some information from the renamer into the typechecker...
  20. Handle literal constant patterns.  We need a Num or Fractional dictionary for
      Int or Double constants.  I guess we need an Eq Char, or Eq [Char] dictionary for
      characters or strings?

  Questions:
  1. How do we handle predicates that make it to the top level?
    - If they are tautological, we can remove them and infer the dictionary.
    - Otherwise we try to default them, if they have a type variable.
  2. Is THIH correct about binding groups in Haskell 2010?
     - It seems that basically all the explicitly-typed things should be at the END.
     - In that case, every explicitly-typed thing would be in its OWN group.
     - We need the explicit types BEFORE we type the thing.
     - So they should be attached to Hs::Binds, not Hs::Decls.
  3. Does normal Haskell avoid context reduction before generalization?
     - choice 2d in "Type classes - an exploration of the design space" suggests
       we should avoid using instances to simplify constraints.
     - but then wouldn't we end up with constraints like Eq [a]?
     - maybe this describes a non-standard extension.

  4. What does entails(x,y) return inside GHC?  If not Binds, then what?  It seems like we
     actually want a formula for all the dvars in y in terms of x.

  5. If the LHS type for a var is more general than its explicit type, how to we do the
     impedance matching?

  6. When a var has an explicit type, on the right hand side, but not on the LHS, how
     do we define the actual exported var?  And how do we ensure that the resulting definition
     is visible on the RHS?  Maybe define the tmp-tuple and the exported var in the same let-rec 
     block?

  7. If we are checking the RHS of a FunDecl with a signature, it seems that we could
     create constraints that refer to type variables from higher scopes, or perhaps
     variables from this scope that do not occur in the type.

     When we check to see that the "wanted" constraints are implied by the "given"
     constraints, we shouldn't need to check these constraints should we?

  Cleanups:
  1. Implement kinds as Hs::Type

  Unification:
  1. If I somehow defer "zonking" to the end, can I avoid manually applying all these
     substitutions?
  2. Can we get better error messages by listing an expected type?
  3. Can we prefix with "heralds" for better error messages?
 */

/*
  Points about contexts in instances and classes:

  1. Each class declaration must have the form class (C1,C2) => K a1 a2 a3 where
  - CLASS ARGUMENTS must be type variables.
  - CONSTRAINT ARGUMENTS must be type variables, unless FlexibleContexts is enabled.

  2. Each instance declaration must have the form instance (C1,C2) => K (X1 a1 a2) (X2 b1 b2) (X3 c1 c2)
  - CLASS ARGUMENTS must have a single type constructor applied to type variables.
  - CONSTAINTS must satisfy the instance termination rules:

    See https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/instances.html#instance-termination

    We can ignore the functional dependencies stuff.  Thus, we just have:

      The Paterson Conditions: for each class constraint (C t1 ... tn) in the context

      1. No type variable has more occurrences in the constraint than in the head
      2. The constraint has fewer constructors and variables (taken together and counting repetitions) than the head
      3. The constraint mentions no type functions. A type function application can in principle expand to a type of arbitrary size, and so are rejected out of hand

  3. We can therefore look up an instance by (K,X1,X2,X3).
  - The more complete form would simply scan ALL instance declarations to find the ONE (or ZERO) matching instances.
  - The slow implementation can extract the constraint from the type.

  Questions about instances:

  Q1. How do we name dictionary extractor & dictionary creator functions?
  A1. Just make up names and record them at the appropriate place.

  Q2. How do we handle mutual recursion between instance methods and value declarations?
  A2. We can process instances before values, but we output instances in the same recursive block as values.

  Q3: Should we translate => to -> during typechecking, OR do we want to do this during desugaring?
  A3: Well.. the typechecker IS making up some variable names... so maybe we do this during type checking?
      Can we tell what GHC is doing?
  A3: We should do it during translation to "Core".

  Q4: How do we process the instances in the correct order?
  A4: Can we put them into the type groups, and then REVISIT the groups,
      analyzing the instances last in each group?

  Q5. How do we type-check the method bindings, if their types depend on the VALUE bindings?
  A5. We know the types of the instance methods BEFORE we know the function bodies, right?
      So, we should be able to type-check the instance bodies LAST, if we generate the types
      for the value_decls first.
  A5. First record the method types, then type-check values -- including values for the method bindings!
      Then create values for the instances.

  Q6. How do we check that there are no super-class -> class cycles.
  A5. Just record all classes that occur in the constraints of each class as a directed edge, and look for cycles.

  Q7. Should we generate a special syntax node for dictionary arguments, dictionary applications, etc,
      in order to delay converting => to ->, and converting dictionary applications, etc?
      See Section 4.3:
      - exp <dict>                                       Dictionary application
      - \dicts:theta -> exp                              Dictionary abstraction
      - \\a1..an -> \dicts:theta -> binds in monobinds   Dictionary abstraction (generalized)
      - <dicts,methods>                                  Dictionary record data type
      - \<dicts:theta,methods:GVE> -> exp                Selecting entries from a dictionary
 */

typedef Hs::Type monotype;
typedef Hs::Type overtype;
typedef Hs::Type polytype;
typedef Hs::Type constraint;

// A = out typevar
// T = out monotype
// K = class typecon.  Also a kind of data declaration for dictionaries.

// E = (TCE, TVE, VE = (CVE, GVE, LVE), CE, IE = (GIE, LIE))

// TCE = type constructor environment = tycon -> /\A1 A2 .. An.T

// TVE = type variable environment = tyvar -> A (destination language typevar)

// CE = class environment = class name -> (K,(forall A (context, methods: GVE)))

// IE = instance environment = (GIE, LIE)

// GIE = global instance environment = dfun -> forall A1 A2 .. An. context => K T

// LIE = dvar -> K T

// VE = (CVE, GVE, LVE)
// CVE = constructor value environment = con -> polytype
// GVE = global value environment      = var -> polytype
// LVE = local  value environment      = var -> monotype

template <typename T>
std::set<T> operator-(const std::set<T>& s1, const std::set<T>& s2)
{
    return minus(s1,s2);
}

// The GIE does NOT allow free type variables.
struct instance_info
{
    // How do we get the kind into the type vars?
    vector<Hs::TypeVar> type_vars;
    Hs::Context context;
    string class_name;
    std::vector<Hs::Type> argument_types;

    string dfun_name;

    // forall <type_vars> . context => class_name argument_types[0] arguments[1] .. argument_types[n01]
    Hs::Type dfun_type() const
    {
        Hs::TypeCon class_con({noloc, class_name}); // whats the kind?
        return Hs::ForallType(type_vars, Hs::ConstrainedType(context, make_tyapps(class_con, argument_types)));
    }
};

global_value_env apply_subst(const substitution_t& s, const value_env& env1)
{
    global_value_env env2;
    for(auto& [x,type]: env1)
        env2 = env2.insert({x, apply_subst(s,type)});
    return env2;
}

std::set<Hs::TypeVar> free_type_variables(const Hs::Type& t)
{
    return free_type_VARS(t);
}

std::set<Hs::TypeVar> free_type_variables(const value_env& env)
{
    std::set<Hs::TypeVar> free;
    for(auto& [x,type]: env)
        add(free, free_type_variables(type));
    return free;
}

optional<string> maybe_get_class_name_from_constraint(const Hs::Type& constraint)
{
    auto [tycon, args] = decompose_type_apps(constraint);
    if (auto tc = tycon.to<Hs::TypeCon>())
        return get_unqualified_name(unloc(tc->name));
    else
        return {};
}

string get_class_name_from_constraint(const Hs::Type& constraint)
{
    if (auto name = maybe_get_class_name_from_constraint(constraint))
        return *name;
    else
        return "Constraint";
}

// Wait, actually don't we assume that the value decls are divided into self-referencing binding groups, along with explicit signatures?
// We would also need: infix declarations, default declarations, ???
// I guess this is AFTER rename, so declarations have been un-infixed, and we could (theoretically) represent each function as something like [([pat],grhs)]
// SHOULD we actually translate each function to (say) a list of ([pat],ghrs)?  How do we store 
//
// typecheck_module(vector<ClassDecl>, vector<DataDecl>, vector<TypeSyonymDecl>, vector<InstanceDecl>, vector<ValueDecl>)
// {
//    Kindcheck(classdecls, data_decls, type_decls);
//
//
// }

local_instance_env& typechecker_state::current_lie() {
    return lie_stack.back();
}

void typechecker_state::add_dvar(const string& name, const Hs::Type& constraint)
{
    auto& lie = current_lie();
    assert(not lie.count(name));
    lie = lie.insert( {name, constraint} );
}

void typechecker_state::add_dvar(const Hs::Var& dvar, const Hs::Type& constraint)
{
    add_dvar(unloc(dvar.name), constraint);
}

Hs::Var typechecker_state::fresh_dvar(const Hs::Type& constraint)
{
    string name = "dvar";
    if (auto cname = maybe_get_class_name_from_constraint(constraint))
        name = "d" + *cname;
    return fresh_var(name, false);
}

Hs::Var typechecker_state::add_dvar(const Hs::Type& constraint)
{
    auto dvar = fresh_dvar(constraint);

    add_dvar(dvar, constraint);

    return dvar;
}

void typechecker_state::push_lie() {
    lie_stack.push_back( {} );
}

local_instance_env typechecker_state::pop_lie()
{
    auto lie = current_lie();
    lie_stack.pop_back();
    return lie;
}

void typechecker_state::pop_and_add_lie()
{
    auto lie = pop_lie();
    current_lie() += lie;
}

typechecker_state::typechecker_state(const string& s, const Module& m, const Hs::ModuleDecls& M, const type_con_env& tce_, const constr_env& ce)
    :tce(tce_),
     con_info(ce),
     mod_name(s),
     this_mod(m)
{
    push_lie();

    if (M.default_decl)
        defaults = M.default_decl->types;
    else
        defaults = { Hs::TypeCon({noloc,"Int"}), Hs::TypeCon({noloc,"Double"}) };
}

Hs::Var typechecker_state::find_prelude_var(string name) const
{
    if (this_mod.is_declared(name))
        name = this_mod.lookup_symbol(name).name;
    return Hs::Var({noloc, name});
}

string typechecker_state::find_prelude_tycon_name(const string& name) const
{
    if (this_mod.type_is_declared(name))
        return this_mod.lookup_type(name).name;
    else
        return name;
}

Hs::TypeCon typechecker_state::find_prelude_tycon(const string& name) const
{
    auto prelude_name = find_prelude_tycon_name(name);
    return Hs::TypeCon({noloc, prelude_name });
}

Hs::Type typechecker_state::bool_type() const
{
    return find_prelude_tycon("Bool");
}

Hs::Type typechecker_state::char_type() const
{
    return find_prelude_tycon("Char");
}

Hs::Type typechecker_state::enum_class(const Hs::Type& arg) const
{
    auto Enum = find_prelude_tycon("Enum");

    return Hs::TypeApp( Enum, arg);
}

Hs::Type typechecker_state::num_class(const Hs::Type& arg) const
{
    auto Num = find_prelude_tycon("Num");

    return Hs::TypeApp( Num, arg);
}

Hs::Type typechecker_state::fractional_class(const Hs::Type& arg) const
{
    auto Fractional = find_prelude_tycon("Fractional");

    return Hs::TypeApp( Fractional, arg);
}

tuple<Hs::Var, Hs::Type> typechecker_state::fresh_enum_type(bool meta)
{
    Hs::Type a = fresh_type_var(meta, kind_star());
    Hs::Type enum_a = enum_class(a);
    auto dvar = add_dvar(enum_a);
    return {dvar, a};
}

tuple<Hs::Var, Hs::Type> typechecker_state::fresh_num_type(bool meta)
{
    Hs::Type a = fresh_type_var(meta, kind_star());
    Hs::Type num_a = num_class(a);
    auto dvar = add_dvar(num_a);
    return {dvar, a};
}

tuple<Hs::Var, Hs::Type> typechecker_state::fresh_fractional_type(bool meta)
{
    Hs::Type a = fresh_type_var(meta, kind_star());
    Hs::Type fractional_a = fractional_class(a);
    auto dvar = add_dvar(fractional_a);
    return {dvar, a};
}

bool typechecker_state::add_substitution(const substitution_t& s)
{
    if (auto s2 = combine(type_var_to_type, s))
    {
        type_var_to_type = *s2;
        return true;
    }
    else
        return false;
}

bool typechecker_state::add_substitution(const Hs::TypeVar& a, const Hs::Type& type)
{
    if (auto s = try_insert({}, a, type))
        return add_substitution(*s);
    else
        return false;
}

bool typechecker_state::maybe_unify(const Hs::Type& t1, const Hs::Type& t2)
{
    if (auto s = ::maybe_unify(t1, t2))
        return add_substitution(*s);
    else
        return false;
}

void typechecker_state::unify(const Hs::Type& t1, const Hs::Type& t2)
{
    if (not maybe_unify(t1,t2))
        throw myexception()<<"Unification failed: "<<apply_current_subst(t1)<<" !~ "<<apply_current_subst(t2);
}

void typechecker_state::unify(const Hs::Type& t1, const Hs::Type& t2, const myexception& e)
{
    if (not maybe_unify(t1,t2))
        throw e;
}

Hs::Var typechecker_state::fresh_var(const std::string& s, bool qualified)
{
    string name = "$"+s+std::to_string(next_var_index);
    if (qualified)
        name = mod_name + "." + name;
    Hs::Var x({noloc, name});
    next_var_index++;
    return x;
}

// "Rigid" type vars come from forall-quantified variables.
// "Wobbly" type vars come from existentially-quantified variables (I think).  We don't have any.
// "Meta" type vars are unification type vars.
Hs::TypeVar typechecker_state::fresh_rigid_type_var(const Hs::Kind& k) {
    Hs::TypeVar tv({noloc, "t"+std::to_string(next_tvar_index)});
    next_tvar_index++;
    tv.info = Hs::typevar_info::rigid;
    tv.kind = k;
    return tv;
}

Hs::TypeVar typechecker_state::fresh_meta_type_var(const Hs::Kind& k) {
    Hs::TypeVar tv({noloc, "t"+std::to_string(next_tvar_index)});
    next_tvar_index++;
    tv.info = Hs::typevar_info::meta;
    tv.kind = k;
    return tv;
}

Hs::TypeVar typechecker_state::fresh_type_var(bool meta, const Hs::Kind& k)
{
    if (meta)
        return fresh_meta_type_var(k);
    else
        return fresh_rigid_type_var(k);
}

pair<local_instance_env, map<Hs::TypeVar, local_instance_env>>
ambiguities(const set<Hs::TypeVar>& tvs1, const set<Hs::TypeVar>& tvs2, local_instance_env lie)
{
    // The input lie MUST be substituted to find its free type vars!
    // lie = apply_current_subst(lie);
    auto ambiguous_tvs = free_type_variables(lie) - tvs1 - tvs2;

    // 1. Record the constraints WITH ambiguous type vars, by type var
    map<Hs::TypeVar,local_instance_env> ambiguities;
    for(auto& ambiguous_tv: ambiguous_tvs)
    {
        local_instance_env lie_for_tv;
        for(auto& [dvar,constraint]: lie)
        {
            if (free_type_variables(constraint).count(ambiguous_tv))
                lie_for_tv = lie_for_tv.insert({dvar,constraint});
        }
        if (not lie_for_tv.empty())
            ambiguities.insert({ambiguous_tv, lie_for_tv});
    }

    // 2. Find the constraints WITHOUT ambiguous type vars
    local_instance_env unambiguous_preds;

    for(auto& [dvar, constraint]: lie)
    {
        auto ftvs = free_type_variables(constraint);
        if (not intersects(ftvs, ambiguous_tvs))
            unambiguous_preds = unambiguous_preds.insert({dvar, constraint});
    }

    return {unambiguous_preds, ambiguities};
}


// Constraints for defaulting must be of the form K a (e.g. Num a)
optional<Hs::TypeCon> simple_constraint_class(const Hs::Type& constraint)
{
    auto [tycon, args] = decompose_type_apps(constraint);

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

optional<tuple<substitution_t, Hs::Binds>>
typechecker_state::candidates(const Hs::TypeVar& tv, const local_instance_env& tv_lie)
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

    for(auto& type: defaults)
    {
        substitution_t s;
        s = s.insert({tv, type});
        if (auto binds = entails({}, apply_subst(s, tv_lie)))
            return pair(s, *binds);
    }

    return {};
}

Hs::Binds typechecker_state::default_subst()
{
    auto [s, binds, unambiguous_preds] = default_preds({}, {}, current_lie());
    assert(unambiguous_preds.empty());

    // Record the substitution, since it can affect types.
    bool ok = add_substitution(s);
    assert(ok);

    // Clear the LIE, which should now be empty.
    current_lie() = {};

    return binds;
}

set<Hs::TypeVar> free_type_variables(const Hs::Type& t);

pair<Hs::Type, vector<Hs::Type>> typechecker_state::constr_types(const Hs::Con& con)
{
    auto& con_name = unloc(con.name);

    if (con_name == ":")
    {
        Hs::Type a = fresh_meta_type_var( kind_star() );
        return {Hs::ListType(a),{a,Hs::ListType(a)}};
    }
    else if (con_name == "[]")
    {
        Hs::Type a = fresh_meta_type_var( kind_star() );
        return {Hs::ListType(a),{}};
    }
    else if (is_tuple_name(con_name))
    {
        int n = tuple_arity(con_name);
        vector<Hs::Type> types;
        for(int i=0;i<n;i++)
            types.push_back(fresh_meta_type_var( kind_star() ));
        return {Hs::TupleType(types),types};
    }

    // 1. Find the data type
    if (not con_info.count(con_name))
        throw myexception()<<"Unrecognized constructor: "<<con;
    auto [_, constraints, con_type] = instantiate(con_info.at(con_name));
    vector<Hs::Type> field_types;

    while(auto f = is_function_type(con_type))
    {
        auto [t1,t2] = *f;
        field_types.push_back(t1);
        con_type = t2;
    }
    auto object_type = con_type;

    return {object_type, field_types};
}

value_env add_constraints(const std::vector<Haskell::Type>& constraints, const value_env& env1)
{
    value_env env2;
    for(auto& [name, monotype]: env1)
        env2 = env2.insert( {name, Hs::add_constraints(constraints, monotype)} );
    return env2;
}

template <typename T>
Hs::Type quantify(const T& tvs, const Hs::Type& monotype)
{
    if (tvs.empty())
        return monotype;
    else
    {
        for(auto& tv: tvs)
            assert(tv.kind);
        return Hs::ForallType(tvs | ranges::to<vector>, monotype);
    }
}

template <typename T>
value_env quantify(const T& tvs, const value_env& env1)
{
    value_env env2;
    for(auto& [name, monotype]: env1)
        env2 = env2.insert( {name, quantify(tvs, monotype)} );
    return env2;
}

Hs::Type generalize(const global_value_env& env, const Hs::Type& monotype)
{
    auto ftv1 = free_type_variables(monotype);
    auto ftv2 = free_type_variables(env);
    for(auto tv: ftv2)
        ftv1.erase(tv);

    for(auto& tv: ftv1)
        assert(tv.kind);
    return Hs::ForallType(ftv1 | ranges::to<vector>, monotype);
}

tuple<vector<Hs::TypeVar>, vector<Hs::Type>, Hs::Type> typechecker_state::instantiate(const Hs::Type& t, bool meta)
{
    // 1. Handle foralls
    vector<Hs::TypeVar> tvs;
    vector<Hs::Type> constraints;
    Hs::Type type = t;

    if (auto fa = type.to<Hs::ForallType>())
    {
        substitution_t s;
        for(auto& tv: fa->type_var_binders)
        {
            assert(tv.kind);
            auto new_tv = fresh_type_var(meta, *tv.kind);
            s = s.insert({tv,new_tv});

            tvs.push_back(new_tv);
        }
        type = fa->type;
        type = apply_subst(s,type);
    }

    // 2. Handle constraints
    if (auto ct = type.to<Hs::ConstrainedType>())
    {
        constraints = ct->context.constraints;
        type = ct->type;
    }

    // 3. Handle the exposed type being a polytype
    if (not tvs.empty() or not constraints.empty())
    {
        auto [tvs2, constraints2, type2] = instantiate(type, meta);

        for(auto& tv2: tvs2)
            tvs.push_back(tv2);

        for(auto& constraint2:  constraints2)
            constraints.push_back(constraint2);

        type = type2;
    }

    return {tvs, constraints, type};
}

vector<Hs::Type> constraints_from_lie(const local_instance_env& lie)
{
    vector<Hs::Type> constraints;
    for(auto& [_, constraint]: lie)
        constraints.push_back(constraint);
    return constraints;
}

vector<Hs::Var> vars_from_lie(const local_instance_env& lie)
{
    vector<Hs::Var> dict_vars;
    for(auto& [name, constraint]: lie)
    {
        Hs::Var dict_var({noloc,name});
        dict_var.type = constraint;
        dict_vars.push_back( dict_var );
    }
    return dict_vars;
}

vector<Hs::Var> vars_from_lie(const vector<pair<Hs::Var, Hs::Type>>& lie)
{
    vector<Hs::Var> vars;
    for(auto& [var, constraint]: lie)
        vars.push_back( var );
    return vars;
}

global_value_env sig_env(const map<string, Hs::Type>& signatures)
{
    global_value_env sig_env;
    for(auto& [name, type]: signatures)
        sig_env = sig_env.insert({name, type});
    return sig_env;
}

tuple<Hs::Binds, global_value_env>
typechecker_state::infer_type_for_binds(const global_value_env& env, Hs::Binds binds)
{
    kindchecker_state K(tce);
    for(auto& [name,type]: binds.signatures)
        type = K.kind_and_type_check_type(type);
    auto env2 = plus_prefer_right(env, sig_env(binds.signatures));

    global_value_env binders;
    for(auto& decls: binds)
    {
        auto [decls1, binders1] = infer_type_for_decls(env2, binds.signatures, decls);
        decls = decls1;
        // We could remove the binders with sigs
        env2 = plus_prefer_right(env2, binders1);
        binders += binders1;
    }

    return {binds, binders};
}

bool type_is_hnf(const Hs::Type& type)
{
    auto [head,args] = decompose_type_apps(type);

    if (head.is_a<Hs::TypeVar>())
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
    auto [class_con, args] = decompose_type_apps(constraint);
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

optional<pair<Hs::Var,vector<Hs::Type>>> typechecker_state::lookup_instance(const Hs::Type& constraint)
{
    for(auto& [name, type]: gie)
    {
        auto [_, instance_constraints, instance_head] = instantiate(type);

        // Skip if this is not an instance.
        if (constraint_is_hnf(instance_head)) continue;

        auto s = ::maybe_match(instance_head, constraint);

        // This instance doesn't match.
        if (not s) continue;

        for(auto& instance_constraint: instance_constraints)
            instance_constraint = apply_subst(*s, instance_constraint);

        auto dfun = Hs::Var({noloc, name});

        return {{dfun, instance_constraints}};
    }
    return {};
}

pair<Hs::Binds,local_instance_env> typechecker_state::toHnf(const string& name, const Hs::Type& constraint)
{
    Hs::Binds binds;
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

        auto [dfun,new_constraints] = *instance;

        // 2. We need to make up new dvar names for all the input constraints.
        // Then we would add to the decls:
        //     dvar_orig = dfun dvar_new1 dvar_new2 dvar_new3
        // And we would get a NEW lie:
        //     dvar_new1 :: constraint1
        //     dvar_new2 :: constraint2
        //     dvar_new3 :: constraint3
        expression_ref rhs = dfun;
        for(auto& new_constraint: new_constraints)
        {
            auto dvar = fresh_var("dvar", false);
            lie2 = lie2.insert({unloc(dvar.name), new_constraint});
            rhs = {rhs,dvar};
        }

        Hs::Var dvar({noloc, name});

        Hs::Decls decls;
        decls.push_back(Hs::simple_decl(dvar, rhs));

        // 3. Finally, we may need to further simplify the new LIE
        auto [binds2, lie3] = toHnfs(lie2);

        lie = lie3;
        binds = binds2;
        binds.push_back(decls);
    }
    return {binds,lie};
}

pair<Hs::Binds, local_instance_env>
typechecker_state::toHnfs(const local_instance_env& lie_in)
{
    Hs::Binds binds_out;
    local_instance_env lie_out;
    for(auto& [name, constraint]: lie_in)
    {
        auto [binds2, lie2] = toHnf(name, constraint);
        for(auto& bind: binds2)
            binds_out.push_back(bind);
        lie_out += lie2;
    }
    return {binds_out, lie_out};
}

// FIXME: there should be a `const` way of getting these.
// FIXME: instantiate is not constant though.
// FIXME: we shouldn't need fresh type vars if the type is unambiguous though.
vector<pair<string, Hs::Type>> typechecker_state::superclass_constraints(const Hs::Type& constraint)
{
    vector<pair<string, Hs::Type>> constraints;

    for(auto& [name, type]: gie)
    {
        // Klass a => Superklass a
        auto [_, class_constraints, superclass_constraint] = instantiate(type, false);

        // Skip if this is not a method of extracting superclass dictionaries
        if (not constraint_is_hnf(superclass_constraint)) continue;

        assert(class_constraints.size() == 1);

        auto class_constraint = class_constraints[0];
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

optional<Hs::Binds> typechecker_state::entails_by_superclass(const pair<string, Hs::Type>& to_keep, const pair<string, Hs::Type>& to_remove)
{
    auto& [dvar_to_keep_name, constraint_to_keep] = to_keep;
    auto& [dvar_to_remove_name, constraint_to_remove] = to_remove;

    if (auto extractors = is_superclass_of(constraint_to_remove, constraint_to_keep))
    {
        Hs::Var dvar_to_keep({noloc, dvar_to_keep_name});
        Hs::Var dvar_to_remove({noloc, dvar_to_remove_name});

        Hs::Exp dict_exp = dvar_to_keep;
        for(auto& extractor: *extractors | views::reverse)
        {
            Hs::Var get_dict({noloc, extractor});
            dict_exp = {get_dict, dict_exp};
        }

        Hs::Decls decls;
        // dvar_to_remove = extractor[n] extractor[n-1] ... extractor[0] dvar_to_keep
        decls.push_back( Hs::simple_decl(dvar_to_remove, dict_exp) );
        Hs::Binds binds;
        binds.push_back(decls);
        return binds;
    }
    else
        return {};
}

vector<pair<Hs::Var,Hs::Type>> typechecker_state::constraints_to_lie(const vector<Hs::Type>& constraints)
{
    vector<pair<Hs::Var, Hs::Type>> ordered_lie;
    for(auto& constraint:constraints)
    {
        auto dvar = fresh_var("dvar", false);
        dvar.type = constraint;
        ordered_lie.push_back({dvar, constraint});
    }
    return ordered_lie;
}

local_instance_env unordered_lie(const vector<pair<Hs::Var, Hs::Type>>& lie1)
{
    local_instance_env lie2;
    for(auto& [var,constraint]: lie1)
        lie2 = lie2.insert({unloc(var.name), constraint});
    return lie2;
}

// How does this relate to simplifying constraints?
optional<Hs::Binds> typechecker_state::entails(const local_instance_env& lie1, const local_instance_env& lie2)
{
    Hs::Binds binds;
    for(auto& constraint: lie2)
    {
        auto binds1 = entails(lie1, constraint);
        if (not binds1)
            return {};
        ranges::insert(binds, binds.begin(), *binds1);
    }
    return binds;
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
    auto [binds1, lie1] = toHnfs(lie);

    auto [binds2, lie2] = simplify(lie1);

    auto binds = binds2;
    for(auto& bind: binds1)
        binds.push_back(bind);

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


tuple<substitution_t, Hs::Binds, local_instance_env>
typechecker_state::default_preds( const set<Hs::TypeVar>& fixed_tvs,
                                  const set<Hs::TypeVar>& referenced_tvs,
                                  const local_instance_env& lie)
{
    substitution_t s;
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

// Why aren't we using `fixed_type_vars`?
// I guess the deferred constraints that do not mention fixed_type_vars are ambiguous?
pair<local_instance_env, local_instance_env>
classify_constraints(const local_instance_env& lie,
                     const set<Hs::TypeVar>& fixed_type_vars)
{
    local_instance_env lie_deferred;
    local_instance_env lie_retained;

    for(auto& [name, constraint]: lie)
    {
        auto constraint_type_vars = free_type_VARS(constraint);

        // Does the constraint contain any ambiguous vars?
        bool all_fixed = true;
        for(auto& type_var: constraint_type_vars)
            if (not fixed_type_vars.count(type_var))
                all_fixed = false;

        if (all_fixed)
            lie_deferred = lie_deferred.insert({name,constraint});
        else
            lie_retained = lie_retained.insert({name,constraint});
    }
    return {lie_deferred, lie_retained};
}

bool is_restricted(const map<string, Hs::Type>& signatures, const Hs::Decls& decls)
{
    for(auto& decl: decls)
    {
        if (decl.is_a<Hs::PatDecl>())
            return true;
        else if (auto fd = decl.to<Hs::FunDecl>())
        {
            // Simple pattern declaration
            if (fd->match.rules[0].patterns.size() == 0)
            {
                auto& name = unloc(fd->v.name);
                if (not signatures.count(name)) return true;
            }
        }
    }
    return false;
};

tuple<Hs::Var, Hs::Type, local_value_env>
typechecker_state::infer_lhs_var_type(Hs::Var v)
{
    auto& name = unloc(v.name);

    Hs::Type type = fresh_meta_type_var( kind_star() );
    v.type = type;

    // Check that this is a NEW name.
    local_value_env lve;
    lve = lve.insert({name,type});
    return {v, type, lve};
}

tuple<expression_ref, Hs::Type, local_value_env>
typechecker_state::infer_lhs_type(const expression_ref& decl, const map<string, Hs::Type>& signatures)
{
    if (auto fd = decl.to<Hs::FunDecl>())
    {
        auto FD = *fd;
        // If there was a signature, we would have called infer_type_for_single_fundecl_with_sig
        assert(not signatures.count(unloc(FD.v.name)));

        auto [v2, type, lve] = infer_lhs_var_type(FD.v);
        FD.v.type = type;
        return {FD, type, lve};
    }
    else if (auto pd = decl.to<Hs::PatDecl>())
    {
        auto PD = *pd;
        auto [lhs, type, lve] = infer_pattern_type(PD.lhs, signatures);
        PD.lhs = lhs;
        return {PD, type, lve};
    }
    else
        std::abort();
}

tuple<expression_ref, Hs::Type>
typechecker_state::infer_rhs_type(const global_value_env& env, const expression_ref& decl)
{
    if (auto fd = decl.to<Hs::FunDecl>())
    {
        auto FD = *fd;
        auto [match, rhs_type] = infer_type(env, FD.match);
        FD.match = match;

        return {FD, rhs_type};
    }
    else if (auto pd = decl.to<Hs::PatDecl>())
    {
        auto PD = *pd;
        auto [rhs, rhs_type] = infer_type(env, PD.rhs);
        PD.rhs = rhs;

        return {PD, rhs_type};
    }
    else
        std::abort();
}

vector<Hs::Decls> split_decls_by_signatures(const Hs::Decls& decls, const map<string, Hs::Type>& signatures)
{
    // 1. Map names to indices
    map<string,int> index_for_name = get_indices_for_names(decls);

    // 2. Figure out which indices reference each other
    vector<vector<int>> referenced_decls;
    for(int i=0;i<decls.size();i++)
    {
        vector<int> refs;
        for(auto& name: get_rhs_free_vars(decls[i]))
        {
            auto it = index_for_name.find(name);

            // Skip if this name isn't one of the ids being defined.
            if (it == index_for_name.end()) continue;

            // Skip if this name has a signature
            if (signatures.count(name)) continue;

            refs.push_back(it->second);
        }
        referenced_decls.push_back( std::move(refs) );
    }

    // 3. Compute strongly-connected components and split
    return split_decls(decls, referenced_decls);
}

bool single_fundecl_with_sig(const Hs::Decls& decls, const signature_env& signatures)
{
    if (decls.size() != 1) return false;

    auto& decl = decls[0];

    if (not decl.is_a<Hs::FunDecl>()) return false;

    auto& FD = decl.as_<Hs::FunDecl>();

    auto& name = unloc(FD.v.name);

    return signatures.count(name) > 0;
}

value_env remove_sig_binders(value_env binder_env, const signature_env& signatures)
{
    auto no_sig_binder_env = binder_env;
    for(auto& [name,_]: binder_env)
        if (signatures.count(name))
            no_sig_binder_env = no_sig_binder_env.erase(name);
    return no_sig_binder_env;
}

tuple<Hs::Decls, global_value_env>
typechecker_state::infer_type_for_decls(const global_value_env& env, const signature_env& signatures, const Hs::Decls& decls)
{
    // The signatures for the binders should already be in the environment.

    auto bind_groups = split_decls_by_signatures(decls, signatures);

    auto env2 = env;
    Hs::Decls decls2;
    local_value_env binders;
    for(auto& group: bind_groups)
    {
        auto [group_decls, group_binders] = infer_type_for_decls_groups(env2, signatures, group);

        for(auto& decl: group_decls)
            decls2.push_back(decl);

        binders += group_binders;

        env2 += remove_sig_binders(group_binders, signatures);
    }
    return {decls2, binders};
}

tuple<expression_ref, string, Hs::Type>
typechecker_state::infer_type_for_single_fundecl_with_sig(const global_value_env& env, Hs::FunDecl FD)
{
    auto& name = unloc(FD.v.name);

    auto sig_type = env.at(name);

    // OK, so what we want to do is:

    // 1. instantiate the type -> (tvs, givens, rho-type)
    auto [tvs, given_constraints, given_type] = instantiate(sig_type, false);
    auto ordered_lie_given = constraints_to_lie(given_constraints);
    auto lie_given = unordered_lie(ordered_lie_given);
    
    // 2. typecheck the rhs -> (rhs_type, wanted, body)
    push_lie();
    auto [decl2, most_general_type] = infer_rhs_type(env, FD);
    auto lie_wanted = pop_lie();

    // 3. alpha[i] in most_general_type but not in env
    auto ftv_mgt = free_type_variables(most_general_type) - free_type_variables(env);
    // FIXME -- what if the instantiated type contains variables that are free in the environment?

    // 4. match(given_type <= most_general_type)
    unify(most_general_type, given_type);

    // 5. check if the given => wanted ~ EvBinds
    lie_wanted = apply_current_subst( lie_wanted );
    auto evbinds = entails(lie_given, lie_wanted);
    if (not evbinds)
        throw myexception()<<"Can't derive constraints '"<<print(lie_wanted)<<"' from specified constraints '"<<print(lie_given)<<"'";

    // 5. return GenBind with tvs, givens, body
    auto dict_vars = vars_from_lie( ordered_lie_given );

    Hs::Decls decls;
    decls.push_back(decl2);

    auto decl = Hs::GenBind( tvs, dict_vars, *evbinds, decls );
    return {decl, name, sig_type};
}

tuple<Hs::Decls, global_value_env>
typechecker_state::infer_type_for_decls_groups(const global_value_env& env, const map<string, Hs::Type>& signatures, Hs::Decls decls)
{
    if (single_fundecl_with_sig(decls, signatures))
    {
        auto& FD = decls[0].as_<Hs::FunDecl>();

        auto [decl, name, sig_type] = infer_type_for_single_fundecl_with_sig(env, FD);

        Hs::Decls decls({decl});

        global_value_env binders;;
        binders = binders.insert({name, sig_type});
        return {decls, binders};
    }

// How & when do we complain if there are predicates on signatures with the monomorphism restriction?

    push_lie();

    // 1. Add each let-binder to the environment with a fresh type variable
    value_env binder_env;

    vector<Hs::Type> lhs_types;
    for(int i=0;i<decls.size();i++)
    {
        auto [decl, type, lve] = infer_lhs_type( decls[i], signatures );
        decls[i] = decl;

        binder_env += lve;
        lhs_types.push_back(type);
    }

    auto env2 = env + remove_sig_binders(binder_env, signatures);

    // 2. Infer the types of each of the x[i]
    for(int i=0;i<decls.size();i++)
    {
        auto [decl, rhs_type] = infer_rhs_type(env2, decls[i]);
        decls[i] = decl;

        unify(lhs_types[i], rhs_type);
    }

    // We need to substitute before looking for free type variables!
    // We also need to substitute before we quantify below.
    binder_env = apply_current_subst(binder_env);

    auto fixed_tvs = free_type_variables(env);
    set<Hs::TypeVar> tvs_in_any_type;  // type variables in ANY of the definitions
    set<Hs::TypeVar> tvs_in_all_types;  // type variables in ALL of the definitions
    {
        // FIXME - should we be looping over binder vars, or over definitions?
        optional<set<Hs::TypeVar>> tvs_in_all_types_;
        for(auto& [_, type]: binder_env)
        {
            auto tvs = free_type_variables(type);
            add(tvs_in_any_type, tvs);
            if (tvs_in_all_types_)
                tvs_in_all_types_ = intersection(*tvs_in_all_types_, tvs);
            else
                tvs_in_all_types_ = tvs;
        }
        assert(tvs_in_all_types_);
        tvs_in_all_types = *tvs_in_all_types_;
    }

    // OK, we've got to do defaulting before we consider what variables to quantify over.

    vector< Hs::Var > dict_vars;

    // A. First, REDUCE the lie by
    //    (i)  converting to Hnf
    //     -- when do we do this?  Always?
    //    (ii) representing some constraints in terms of others.
    // This also substitutes into the current LIE, which we need to do 
    //    before finding free type vars in the LIE below.
    Hs::Binds binds = reduce_current_lie();

    // B. Second, extract the "retained" predicates can be added without causing abiguity.
    auto [lie_deferred, lie_retained] = classify_constraints( current_lie(), fixed_tvs );

    /* NOTE: Constraints can reference variables that are in
     *        (i) ALL types in a recursive group
     *       (ii) SOME-BUT-NOT-ALL types
     *      (iii) NO types.
     *
     * For unrestricted bindings, classes (ii) and (iii) need defaults.
     * For restricted bindings, only class (iii) (I think) needs defaults.
     */


    // FIXME: return {dvar = expression} as a mapping, instead of a sequence of binds?
    // If we want to substitute an expression for an argument in the wrapper,
    
    // For the COMPLETELY ambiguous constraints, we should be able to just discard the constraints,
    //   after generating definitions of their 
    auto [s1, binds1, lie_not_completely_ambiguous] = default_preds( fixed_tvs, tvs_in_any_type, lie_retained );
    binds = binds1 + binds;

    set<Hs::TypeVar> qtvs = tvs_in_any_type - fixed_tvs;

    map<string, Hs::BindInfo> bind_infos;

    if (is_restricted(signatures, decls))
    {
        // 1. Remove defaulted constraints from LIE?
        current_lie() = lie_deferred + lie_not_completely_ambiguous;

        // 2. Quantify only over variables that are "unconstrained" (not part of the LIE)
        // -- after defaulting!

        // NOTE: in theory, we should be able to subtract just ftvs(lie_retained_not_defaulted),
        //       since lie_deferred should contain only fixed type variables that have already been
        //       removed from qtvs.
        qtvs = qtvs - free_type_variables(current_lie());

        // 3. We have already substituted for types above.
        binder_env = quantify( qtvs, binder_env );
    }
    else
    {
        // For the SOMEWHAT ambiguous constraints, we don't need the defaults to define the recursive group,
        // but we do need the defaults to define individual symbols.

        // 1. Quantify over variables in ANY type that are not fixed -- doesn't depend on defaulting.
        // Never quantify over variables that are only in a LIE -- those must be defaulted.

        // 2. Only the constraints with all fixed tvs are going to be visible outside this declaration group.
        current_lie() = lie_deferred;

        dict_vars = vars_from_lie( lie_not_completely_ambiguous );

        global_value_env binder_env2;
        for(auto& [name,type]: binder_env)
        {
            Hs::BindInfo info;
            auto tvs_in_this_type = free_type_variables(type);

            // Default any constraints that do not occur in THIS type.
            auto [s2, binds2, lie_for_this_type] = default_preds( fixed_tvs, tvs_in_this_type, lie_not_completely_ambiguous );

            Hs::Type constrained_type = Hs::add_constraints( constraints_from_lie(lie_for_this_type), type );

            // Only quantify over type variables that occur in THIS type.
            Hs::Type qualified_type = quantify( tvs_in_this_type, constrained_type );

            // How can we generate a wrapper between qualified_type and lie_deferred => (type1, unrestricted_type, type3)?

            binder_env2 = binder_env2.insert( {name, qualified_type} );

            info.monotype = type;
            info.binds = binds2;
            bind_infos.insert({name, info});
        }
        binder_env = binder_env2;
    }

    Hs::Decls decls2 = decls;
    if (qtvs.size() or binds.size() or dict_vars.size())
    {
        decls2 = {};
        Hs::GenBind gen_bind( qtvs | ranges::to<vector>, dict_vars, binds, decls );
        gen_bind.bind_infos = bind_infos;
        decls2.push_back( gen_bind );
    }

    pop_and_add_lie();

    return {decls2, binder_env};
}

// Figure 24. Rules for patterns
tuple<Hs::Pattern, Hs::Type, local_value_env>
typechecker_state::infer_pattern_type(const Hs::Pattern& pat, const map<string, Hs::Type>& sigs)
{
    // TAUT-PAT
    if (auto v = pat.to<Hs::Var>())
    {
        auto V = *v;
        auto& name = unloc(V.name);
        local_value_env lve;
        Hs::Type type;
        if (sigs.count(name))
        {
            auto sig_type = sigs.at(name);
            auto [tvs, constraints, monotype] = instantiate(sig_type);
            if (constraints.size())
                throw myexception()<<"variable '"<<name<<"' cannot have constrained type '"<<sig_type<<"' due to monomorphism restriction";
            type = monotype;
        }
        else
        {
            auto tv = fresh_meta_type_var( kind_star() );
            type = tv;
        }
        V.type = type;
        lve = lve.insert({name, type});
	return { V, type , lve };
    }
    // CONSTR-PAT
    else if (auto con = pat.head().to<Hs::Con>())
    {
        local_value_env lve;
        local_instance_env lie;
        vector<Hs::Type> types;
        auto pats = pat.copy_sub();

        for(auto& pat: pats)
        {
            auto [p, t1, lve1] = infer_pattern_type(pat, sigs);
            pat = p;
            types.push_back(t1);
            lve += lve1;
        }
        substitution_t s;
        auto [type,field_types] = constr_types(*con);

        assert(field_types.size() == pat.size());

        // Unify constructor field types with discovered types.
        for(int i=0;i<types.size();i++)
            unify(types[i], field_types[i]);

        return { pat, type, lve };
    }
    // AS-PAT
    else if (auto ap = pat.to<Hs::AsPattern>())
    {
        auto [pat, t, lve] = infer_pattern_type(ap->pattern, sigs);
        auto& name = unloc(ap->var.as_<Hs::Var>().name);
        lve = lve.insert({name, t});
        return {pat, t, lve};
    }
    // LAZY-PAT
    else if (auto lp = pat.to<Hs::LazyPattern>())
    {
        auto [p, t, lve] = infer_pattern_type(lp->pattern, sigs);
        return {Hs::LazyPattern(p), t, lve};
    }
    // not in paper (STRICT-PAT)
    else if (auto sp = pat.to<Hs::StrictPattern>())
    {
        auto [p, t, lve] = infer_pattern_type(sp->pattern, sigs);
        return {Hs::StrictPattern(p), t, lve};
    }
    // WILD-PAT
    else if (pat.is_a<Hs::WildcardPattern>())
    {
        auto tv = fresh_meta_type_var( kind_star() );
        return {pat, tv, {}};
    }
    // LIST-PAT
    else if (auto l = pat.to<Hs::List>())
    {
        auto L = *l;

        local_value_env lve;
        Hs::Type t = fresh_meta_type_var( kind_star() );
        for(auto& element: L.elements)
        {
            auto [p1, t1, lve1] = infer_pattern_type(element, sigs);
            element = p1;

            unify(t, t1);
            lve += lve1;
        }

        return {L, Hs::ListType(t), lve};
    }
    // TUPLE-PAT
    else if (auto t = pat.to<Hs::Tuple>())
    {
        auto T = *t;
        vector<Hs::Type> types;
        local_value_env lve;
        for(auto& element: T.elements)
        {
            auto [p, t1, lve1] = infer_pattern_type(element, sigs);
            element = p;
            types.push_back(t1);
            lve += lve1;
        }
        return {T, Hs::TupleType(types), lve};
    }
    // ???
    else if (pat.is_int())
    {
        auto [dvar, type] = fresh_num_type();
        return {pat, type, {}};
    }
    else if (pat.is_double())
    {
        auto [dvar, type] = fresh_fractional_type();
        return {pat, type, {}};
    }
    else if (pat.is_char())
    {
        return {pat, char_type(), {}};
    }
    else if (false) // Literal string
    {
        return {pat, Hs::ListType(char_type()), {}};
    }
    else if (pat.is_log_double())
        throw myexception()<<"log_double literatal should be impossible: '"<<pat<<"'!";
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
}


// Figure 22. Rules for quals
//
// The implementation is rather... different?
// * the original figure doesn't have let quals.
// * the original figure seems to assume that quals only occur in list comprehensions?

tuple<vector<Hs::Qual>, value_env>
typechecker_state::infer_quals_type(const global_value_env& env, vector<Hs::Qual> quals)
{
    auto env2 = env;
    local_value_env binders;
    for(auto& qual: quals)
    {
        auto [qual_exp, qual_binders] = infer_qual_type(env2, qual);

        qual = qual_exp;
        env2 = plus_prefer_right(env2, qual_binders);
        binders = plus_prefer_right(binders, qual_binders);
    }
    return {quals, binders};
}

tuple<Hs::Qual, value_env>
typechecker_state::infer_qual_type(const global_value_env& env, const Hs::Qual& qual)
{
    // FILTER
    if (auto sq = qual.to<Hs::SimpleQual>())
    {
        auto SQ = *sq;
        auto [exp, cond_type] = infer_type(env, SQ.exp);
        SQ.exp = exp;
        unify( cond_type, bool_type() );
        return {SQ, {}};
    }
    // GENERATOR.
    else if (auto pq = qual.to<Hs::PatQual>())
    {
        auto PQ = *pq;
        // pat <- exp
        auto [bindpat, pat_type, lve] = infer_pattern_type(PQ.bindpat);
        auto [exp, exp_type] = infer_type(env, PQ.exp);

        PQ.bindpat = bindpat;
        PQ.exp = exp;

        // type(pat) = type(exp)
        unify(Hs::ListType(pat_type), exp_type);

        return {PQ, lve};
    }
    else if (auto lq = qual.to<Hs::LetQual>())
    {
        auto LQ = *lq;
        auto [binds, t] = infer_type_for_binds(env, unloc(LQ.binds));
        unloc(LQ.binds) = binds;
        return {LQ, t};
    }
    else
        std::abort();
}


tuple<Hs::Qual, value_env>
typechecker_state::infer_guard_type(const global_value_env& env, const Hs::Qual& guard)
{
    if (auto sq = guard.to<Hs::SimpleQual>())
    {
        auto SQ = *sq;
        auto [cond_exp, cond_type] = infer_type(env, SQ.exp);
        SQ.exp = cond_exp;
        unify( cond_type, bool_type() );
        return {SQ, {}};
    }
    else if (auto pq = guard.to<Hs::PatQual>())
    {
        auto PQ = *pq;
        // pat <- exp
        auto [bindpat, pat_type, lve] = infer_pattern_type(PQ.bindpat);
        auto [exp, exp_type] = infer_type(env, PQ.exp);

        PQ.bindpat = bindpat;
        PQ.exp = exp;
        
        // type(pat) = type(exp)
        unify(pat_type,exp_type);

        return {PQ, lve};
    }
    else if (auto lq = guard.to<Hs::LetQual>())
    {
        auto LQ = *lq;
        auto [binds, t] = infer_type_for_binds(env, unloc(LQ.binds));
        unloc(LQ.binds) = binds;
        return {LQ, t};
    }
    else
        std::abort();
}


// Figure 25. Rules for match, mrule, and grhs
tuple<Hs::GuardedRHS, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, Hs::GuardedRHS rhs)
{
    // Fig 25. GUARD-DEFAULT
    if (rhs.guards.empty())
    {
        auto [body, type] = infer_type(env, rhs.body);
        rhs.body = body;
        return {rhs, type};
    }

    // Fig 25. GUARD
    auto guard = rhs.guards[0];
    auto [guard1, env1] = infer_guard_type(env, guard);
    auto env2 = plus_prefer_right(env, env1);

    rhs.guards.erase(rhs.guards.begin());
    auto [rhs2, t2] = infer_type(env2, rhs);
    
    rhs2.guards.insert(rhs2.guards.begin(), guard1);

    Hs::Type type = t2;
    return {rhs2, type};
}

// Fig 25. GUARD-OR
tuple<Hs::MultiGuardedRHS, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, Hs::MultiGuardedRHS rhs)
{
    substitution_t s;
    Hs::Type type = fresh_meta_type_var( kind_star() );

    auto env2 = env;
    if (rhs.decls)
    {
        auto [decls1, binders] = infer_type_for_binds(env, unloc(*rhs.decls));
        unloc(*rhs.decls) = decls1;
        env2 = plus_prefer_right(env, binders);
    }

    for(auto& guarded_rhs: rhs.guarded_rhss)
    {
        auto [guarded_rhs2, t1] = infer_type(env2, guarded_rhs);
        guarded_rhs = guarded_rhs2;
        unify(t1,type);
    }

    return {rhs, type};
};

tuple<Hs::MRule, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, Hs::MRule rule)
{

    if (rule.patterns.empty())
    {
        auto [rhs, type] = infer_type(env, rule.rhs);
        rule.rhs = rhs;
        return {rule, type};
    }
    else
    {
        auto [pat, t1, lve1] = infer_pattern_type(rule.patterns.front());
        auto env2 = plus_prefer_right(env, lve1);

        // Remove the first pattern in the rule
        rule.patterns.erase(rule.patterns.begin());

        auto [rule2, t2] = infer_type(env2, rule);

        rule2.patterns.insert(rule2.patterns.begin(), pat);

        Hs::Type type = make_arrow_type(t1,t2);

        return {rule2, type};
    }
}

tuple<Hs::Match, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, Hs::Match m)
{
    Hs::Type result_type = fresh_meta_type_var( kind_star() );

    for(auto& rule: m.rules)
    {
        auto [rule1, t1] = infer_type(env, rule);
        rule = rule1;
        unify(result_type, t1);
    }

    return {m, result_type};
}



tuple<expression_ref, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, expression_ref E)
{
    if (auto x = E.to<Hs::Var>())
    {
        auto& x_name = unloc(x->name);
        auto sigma = env.find( x_name );

        // x should be in the type environment
        if (not sigma)
            throw myexception()<<"infer_type: can't find type of variable '"<<x->print()<<"'";

        auto [_, constraints, type] = instantiate(*sigma);

        for(auto& constraint: constraints)
        {
            auto dvar = add_dvar(constraint);
            E = {E, dvar};
        }

        return {E, type};
    }
    else if (E.is_int())
    {
        auto [dvar, type] = fresh_num_type();
        E = { find_prelude_var("fromInteger"), dvar, E };
        return { E, type };
    }
    else if (E.is_double())
    {
        auto [dvar, type] = fresh_fractional_type();
        E = { find_prelude_var("fromRational"), dvar, E };
        return { E, type };
    }
    else if (E.is_char())
    {
        return { E, char_type() };
    }
    else if (E.is_log_double())
    {
        auto [dvar, type] = fresh_fractional_type();
        return { E, type };
    }
    else if (auto texp = E.to<Hs::TypedExp>())
    {
        // So, ( e :: tau ) should be equivalent to ( let x :: tau ; x = e in x )
        // according to the 2010 report.

        // Example: (\x -> x) :: Num a => a -> a
        // In this example, we should rewrite this to \dNum -> \x -> x

        // texp->exp;
        // texp->type

        // FIXME: For better error messages, we should inline the code for inferring types of LetExp.
        //        We will know we will call infer_type_for_single_fundecl_with_sig

        auto x = fresh_var("tmp", false);
        Hs::Decls decls;
        decls.push_back(simple_decl(x,texp->exp));
        Hs::Binds binds;
        // By making a LetExp, we rely on the Let code to handle the type here.
        binds.signatures.insert({unloc(x.name), texp->type});
        binds.push_back(decls);
        expression_ref E2 = Hs::LetExp({noloc,binds},{noloc,x});

        return infer_type(env, E2);
    }
    else if (auto l = E.to<Hs::List>())
    {
        Hs::Type element_type = fresh_meta_type_var( kind_star() );
        auto L = *l;
        for(auto& element: L.elements)
        {
            auto [element1, t1] = infer_type(env, element);
            element = element1;
            unify(t1, element_type);
        }
        return { L, Hs::ListType(element_type) };
    }
    else if (auto tup = E.to<Hs::Tuple>())
    {
        auto T = *tup;

        vector<Hs::Type> element_types;
        for(auto& element: T.elements)
        {
            auto [element1, element_type] = infer_type(env, element);
            element = element1;
            element_types.push_back( element_type );
        }
        Hs::Type result_type = Hs::TupleType(element_types);
        return {T, result_type};
    }
    // COMB
    else if (is_apply_exp(E))
    {
        assert(E.size() >= 2);

        auto e1 = E.sub()[0];

        auto [f, t1] = infer_type(env,e1);

        vector<expression_ref> args;
        for(int i=1;i<E.size();i++)
        {
            auto e2 = E.sub()[i];

            // tv <- fresh
            auto tv = fresh_meta_type_var( kind_star() );

            auto [arg2, t2] = infer_type(env, e2);
            args.push_back(arg2);

            unify (t1, make_arrow_type(t2,tv));

            t1 = tv;
        }
        E = apply_expression(f, args);

        return {E, t1};
    }
    // LAMBDA
    else if (auto lam = E.to<Hs::LambdaExp>())
    {
        auto Lam = *lam;
        auto rule = Hs::MRule{Lam.args, Lam.body};
        auto [rule2, t] = infer_type(env, rule);
        Lam.args = rule.patterns;
        Lam.body = rule.rhs;
        return {Lam, t};
    }
    // LET
    else if (auto let = E.to<Hs::LetExp>())
    {
        auto Let = *let;

        // 1. Extend environment with types for decls, get any substitutions
        auto [binds, binders] = infer_type_for_binds(env, unloc(Let.binds));
        unloc(Let.binds) = binds;
        auto env2 = plus_prefer_right(env, binders);

        // 2. Compute type of let body
        auto [body, t_body] = infer_type(env2, unloc(Let.body));
        unloc(Let.body) = body;

        // return (s1 `compose` s2, t2)
        return {Let, t_body};
    }
    else if (auto con = E.head().to<Hs::Con>())
    {
        auto [type, field_types] = constr_types(*con);

        vector<Hs::Type> arg_types;
        vector<Hs::Exp> args = E.copy_sub();
        for(int i=0; i < args.size(); i++)
        {
            auto& arg = args[i];
            auto [arg_i, t_i] = infer_type(env, arg);
            arg = arg_i;
            arg_types.push_back(t_i);

            // REQUIRE that i-th argument matches the type for the i-th field.
            unify( field_types[i], t_i);
        }
        E = expression_ref(*con, args);
        
        return { E, type };
    }
    else if (is_non_apply_op_exp(E))
    {
        std::abort();
        // this includes builtins like Prelude::add
    }
    // CASE
    else if (auto case_exp = E.to<Hs::CaseExp>())
    {
        auto Case = *case_exp;

        // 1. Determine object type
        auto [object, object_type] = infer_type(env, Case.object);
        Case.object = object;
        
        // 2. Determine data type for object from patterns.
        Hs::Match match;
        for(auto& alt: Case.alts)
        {
            auto& [pattern, body] = unloc(alt);
            match.rules.push_back(Hs::MRule{{pattern},body});
        }

        auto [match2, match_type] = infer_type(env, match);

        for(int i=0;i<Case.alts.size();i++)
        {
            unloc(Case.alts[i]) = {match2.rules[i].patterns[0], match2.rules[i].rhs};
        }

        Hs::Type result_type = fresh_meta_type_var( kind_star() );

        unify( make_arrow_type(object_type,result_type), match_type );

        return { Case, result_type };
    }
    // IF
    else if (auto if_exp = E.to<Hs::IfExp>())
    {
        auto If = *if_exp;
        auto [cond, cond_type ] = infer_type(env, unloc(If.condition));
        auto [tbranch, tbranch_type] = infer_type(env, unloc(If.true_branch));
        auto [fbranch, fbranch_type] = infer_type(env, unloc(If.false_branch));
        unloc(If.condition) = If;
        unloc(If.true_branch) = tbranch;
        unloc(If.false_branch) = fbranch;

        unify(cond_type, bool_type());
        unify(tbranch_type, fbranch_type);

        return {If, tbranch_type};
    }
    // LISTCOMP
    else if (auto lcomp = E.to<Hs::ListComprehension>())
    {
        auto LComp = *lcomp;
        auto [quals, quals_binders] = infer_quals_type(env, LComp.quals);
        auto [body, exp_type] = infer_type(plus_prefer_right(env, quals_binders), LComp.body);
        LComp.quals = quals;
        LComp.body = body;

        Hs::Type result_type = Hs::ListType(exp_type);

        return { LComp, result_type };
    }
    // ENUM-FROM
    else if (auto l = E.to<Hs::ListFrom>())
    {
        auto L = *l;
        auto [dvar, t] = fresh_enum_type();

        // PROBLEM: Do we need to desugar these here, in order to plug in the dictionary?
        auto [from, t_from] = infer_type(env, L.from);
        L.from = from;
        unify(t,t_from);

        return {L, Hs::ListType(t) };
    }
    // ENUM-FROM-THEN
    else if (auto l = E.to<Hs::ListFromThen>())
    {
        auto L = *l;
        auto [dvar, t] = fresh_enum_type();
        auto [from, t_from] = infer_type(env, L.from);
        unify(t,t_from);

        auto [then, t_then] = infer_type(env, L.then);
        L.from = from;
        L.then = then;
        
        return {L, Hs::ListType(t)};
    }
    // ENUM-FROM-TO
    else if (auto l = E.to<Hs::ListFromTo>())
    {
        auto L = *l;
        auto [dvar, t] = fresh_enum_type();
        auto [from, t_from] = infer_type(env, l->from);
        unify(t,t_from);

        auto [to, t_to] = infer_type(env, l->to);
        L.from = from;
        L.to = to;
        
        return {L, Hs::ListType(t)};
    }
    // ENUM-FROM-THEN-TO
    else if (auto l = E.to<Hs::ListFromThenTo>())
    {
        auto L = *l;
        auto [dvar, t] = fresh_enum_type();
        auto [from, t_from] = infer_type(env, L.from);
        unify(t,t_from);

        auto [then, t_then] = infer_type(env, L.then);
        unify(t,t_then);

        auto [to, t_to] = infer_type(env, l->to);
        unify(t,t_to);

        L.from = from;
        L.then = then;
        L.to = to;

        return {L, Hs::ListType(t)};
    }
    else
        throw myexception()<<"type check expression: I don't recognize expression '"<<E<<"'";
}

Hs::Type remove_top_level_foralls(Hs::Type t)
{
    while(auto fa = t.to<Hs::ForallType>())
        t = fa->type;
    return t;
}

constr_env get_constructor_info(const Hs::Decls& decls, const type_con_env& tce)
{
    constr_env cve;

    kindchecker_state ks(tce);

    for(auto& decl: decls)
    {
        auto d = decl.to<Hs::DataOrNewtypeDecl>();
        if (not d) continue;

        auto constr_map = ks.type_check_data_type(*d);
        for(auto& [name, type]: constr_map)
            cve = cve.insert({name,type});
    }

    return cve;
}

tuple<global_value_env, global_instance_env, class_env, Hs::Binds> typechecker_state::infer_type_for_classes(const Hs::Decls& decls)
{
    global_value_env gve;
    global_instance_env gie;
    class_env ce;
    Hs::Binds binds;

    for(auto& decl: decls)
    {
        auto c = decl.to<Hs::ClassDecl>();
        if (not c) continue;

        auto [gve1, gie1, class_info, class_decls] = infer_type_for_class(*c);

        gve += gve1;
        gie += gie1;
        ce.insert({class_info.name, class_info});
        binds.push_back(class_decls);
    }

    return {gve, gie, ce, binds};
}

// OK, so
// * global_value_env    = name         :: forall a: class var => signature (i.e. a-> b -> a)
// * global_instance_env = made-up-name :: forall a: class var => superclass var
// * Hs::Decls           = { name         = \dict -> case dict of (_,_,method,_,_) -> method }
//                       = { made-up-name = \dict -> case dict of (superdict,_,_,_,_) -> superdict }

tuple<global_value_env,global_instance_env,class_info,Hs::Decls>
typechecker_state::infer_type_for_class(const Hs::ClassDecl& class_decl)
{
    kindchecker_state K(tce);

    auto& name = class_decl.name;

    class_info cinfo;
    cinfo.type_vars = class_decl.type_vars;
    cinfo.name = name;
    cinfo.emitted_name = "class$"+name; // FIXME: only modify name after qualifier?  Just prefix d?
    cinfo.context = class_decl.context;

    // 1. Bind type parameters for class
    K. push_type_var_scope();

    // 1a. Look up kind for this data type.
    auto class_kind = K.kind_for_type_con(name);

    // 1b. Record the kind for each type variable.
    for(auto& tv: class_decl.type_vars)
        K.bind_type_var(tv, *tv.kind);

    // 2. construct the constraint that represent the class
    Hs::Type class_constraint = Hs::TypeCon(Unlocated(class_decl.name)); // put class_kind into TypeCon?
    for(auto& tv: class_decl.type_vars)
        class_constraint = Hs::TypeApp(class_constraint, tv);

    // 3. make global types for class methods
    global_value_env gve;
    if (class_decl.binds)
    {
        for(auto& [qname, type]: unloc(*class_decl.binds).signatures)
        {
            auto method_type = K.kind_and_type_check_type(type);

            Hs::Type method_value_type = add_forall_vars(class_decl.type_vars,
                                                         Hs::ConstrainedType(Hs::Context({class_constraint}), method_type));
            gve = gve.insert({qname, method_value_type});

            auto name = get_unqualified_name(qname);

            Hs::Type plain_method_body_type = add_forall_vars(class_decl.type_vars, method_type);
            cinfo.plain_members = cinfo.plain_members.insert({name, plain_method_body_type});

            Hs::Type method_body_type = add_forall_vars(class_decl.type_vars,
                                                        Hs::ConstrainedType(cinfo.context, method_type));
            cinfo.members = cinfo.members.insert({name, method_body_type});
        }
    }

    K.pop_type_var_scope();

    // OK, so now we need to
    //   (a) determine names for dictionary extractors.
    //   (b) determine an order for all the fields.
    //   (c) synthesize field accessors and put them in decls

    global_instance_env gie;
    for(auto& superclass_constraint: cinfo.context.constraints)
    {
        string cname1 = get_class_name_from_constraint(superclass_constraint);
        string cname2 = get_class_name_from_constraint(class_constraint);
        string extractor_name = cname1+"From"+cname2;

        auto get_dict = fresh_var(extractor_name, true);
        // Should this be a function arrow?
        Hs::Type type = Hs::add_constraints({class_constraint}, superclass_constraint);
        type = apply_current_subst(type);
        // Maybe intersect the forall_vars with USED vars?
        type = add_forall_vars(class_decl.type_vars, type);
        gie = gie.insert({unloc(get_dict.name), type});
    }
    for(auto& [name,type]: gie + gve)
        cinfo.fields.push_back({get_unqualified_name(name),type});

    Hs::Decls decls;

    vector<Hs::Type> types;
    for(auto& [name,type]: cinfo.fields)
        types.push_back(type);
    Hs::Type dict_type = Hs::tuple_type(types);

    int i = 0;
    for(auto& [name,type]: cinfo.fields)
    {
        // body = \dict -> case dict of (_,field,_,_) -> field

        // dict
        Hs::Var dict({noloc,"dict"});
        // field
        Hs::Var field({noloc,"field"});

        // (_,field,_,_)
        vector<Hs::Pattern> pats(cinfo.fields.size(), Hs::WildcardPattern());
        pats[i] = field;

        // (,field,_,_) -> field
        Hs::Alt alt{Hs::tuple(pats),Hs::SimpleRHS({noloc,field})};

        // case dict of (_,field,_,_) -> field
        Hs::CaseExp case_exp(dict,Hs::Alts({{noloc,alt}}));

        // dict -> case dict of (_,field,_,_) -> field
        Hs::MRule rule{{dict},Hs::SimpleRHS({noloc,case_exp})};
        Hs::Match m{{rule}};

        // f = dict -> case dict of (_,field,_,_) -> field
        Hs::Var f({noloc,name});
        decls.push_back( Hs::FunDecl(f,m) );

        i++;
    }

    return {gve,gie,cinfo,decls};
}

pair<Hs::Var, Hs::Type>
typechecker_state::infer_type_for_instance1(const Hs::InstanceDecl& inst_decl, const class_env& ce)
{
    // -- old -- //
    auto [class_head, class_args] = decompose_type_apps(inst_decl.constraint);

    // Premise #1: Look up the info for the class
    optional<class_info> cinfo;
    if (auto tc = class_head.to<Hs::TypeCon>())
    {
        // Check that this is a class, and not a data or type?
        auto class_name = unloc(tc->name);
        if (not ce.count(class_name))
            throw myexception()<<"In instance '"<<inst_decl.constraint<<"': no class '"<<class_name<<"'!";
        cinfo = ce.at(class_name);
    }
    else
        throw myexception()<<"In instance for '"<<inst_decl.constraint<<"': "<<class_head<<" is not a class!";


    // Premise #2: Find the type vars mentioned in the constraint.
    set<Hs::TypeVar> type_vars;
    // Premise #4: the class_arg must be a type constructor applied to simple, distinct type variables.
    vector<Hs::TypeCon> types;
    for(auto& class_arg: class_args)
    {
        auto [a_head, a_args] = decompose_type_apps(class_arg);
        auto tc = a_head.to<Hs::TypeCon>();
        if (not tc)
            throw myexception()<<"In instance for '"<<inst_decl.constraint<<"': "<<a_head<<" is not a type constructor!";

        types.push_back(*tc);

        // Add distinct type variables
        for(auto& a_arg: a_args)
        {
            auto tv = a_arg.to<Hs::TypeVar>();

            if (not tv)
                throw myexception()<<"In instance for '"<<inst_decl.constraint<<"' for type '"<<class_arg<<"': "<<a_arg<<" is not a type variable!";

            if (type_vars.count(*tv))
                throw myexception()<<"Type variable '"<<tv->print()<<"' occurs twice in constraint '"<<inst_decl.constraint<<"'";

            type_vars.insert(*tv);
        }
    }

    // Premise 5: Check that the context contains no variables not mentioned in `class_arg`
    for(auto& tv: free_type_VARS(inst_decl.context))
    {
        if (not type_vars.count(tv))
            throw myexception()<<"Constraint context '"<<inst_decl.context.print()<<"' contains type variable '"<<tv.print()<<"' that is not mentioned in '"<<inst_decl.constraint<<"'";
    }

    auto dfun = fresh_var("dfun", true);

    //  -- new -- //
    kindchecker_state K(tce);
    Hs::Type inst_type = Hs::add_constraints(inst_decl.context, inst_decl.constraint);
    inst_type = K.kind_and_type_check_constraint(inst_type);
    return {dfun, inst_type};
}


// See Tc/TyCl/Instance.hs
// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
pair<global_instance_env,vector<pair<Hs::Var,Hs::InstanceDecl>>>
typechecker_state::infer_type_for_instances1(const Hs::Decls& decls, const class_env& ce)
{
    global_instance_env gie_inst;

    vector<pair<Hs::Var, Hs::InstanceDecl>> named_instances;

    for(auto& decl: decls)
    {
        if (auto I = decl.to<Hs::InstanceDecl>())
        {
            auto [dfun, inst_type] = infer_type_for_instance1(*I, ce);

            named_instances.push_back({dfun, *I});
            gie_inst = gie_inst.insert({unloc(dfun.name), inst_type});
        }
    }
    return {gie_inst, named_instances};
}

template <typename T>
Hs::Expression tuple_from_value_env(const T& venv)
{
    vector<expression_ref> elements;
    for(auto& [name,type]: venv)
        elements.push_back( Hs::Var({noloc, name}) );

    return Hs::tuple(elements);
}

Hs::Decls
typechecker_state::infer_type_for_instance2(const Hs::Var& dfun, const Hs::InstanceDecl& inst_decl, const class_env& ce)
{

    // PROBLEM: we also need to know all the instance types to check the entails.
    //          so, that part needs to come after a first pass over all instances...
    // PROBLEM: we need to types for functions defined in the module...
    //          so, typechecking the method bodies needs to come after typechecking the rest of the module.
    // FIXME: What stuff do we want to know from infer_type_for_instance1( )?
    //        * the dvar name
    
    // Construct superclass dictionary entries from instance constraints

    // Construct member function entries.

    /* dfun idvar1:instance_constraint1 ... idvar[N]:instance_constraint[N] =
              let dvar1 = <construct superdict1>
                  dvar2 = <construct superdictN>

              in let var1 = <body1>
                     varM = <bodyM>
                 in <dvar1, ..., dvarN, var1, ..., varM>
    */

    auto [class_head, monotypes] = decompose_type_apps(inst_decl.constraint);

    // 1. Look up the info for the main class
    auto tc = class_head.to<Hs::TypeCon>();
    assert(tc); // already checked.
    string class_name = unloc(tc->name);
    class_info cinfo = ce.at(class_name);

    // 2. build a local instance environment from the instance constraints
    auto ordered_lie_instance = constraints_to_lie(inst_decl.context.constraints);
    auto lie_instance = unordered_lie(ordered_lie_instance);

    // 3. Construct binds_super
    auto ordered_lie_super = constraints_to_lie(cinfo.context.constraints);
    auto lie_super = unordered_lie(ordered_lie_super);
    auto binds_super = entails(lie_instance, lie_super);
    if (not binds_super)
        throw myexception()<<"Missing instances!";

    // 5. make some intermediates
    auto instance_constraint_dvars = vars_from_lie(ordered_lie_instance);
    vector<Hs::Pattern> lambda_vars;
    vector<Hs::Expression> dict_entries;
    for(auto dv: instance_constraint_dvars)
    {
        lambda_vars.push_back(dv);
        dict_entries.push_back(dv);
    }


    // 4. Construct binds_methods
    Hs::Binds binds_methods;
    std::map<string,Hs::Match> method_matches;
    for(auto& decl: unloc(*inst_decl.binds)[0])
    {
        auto& fd = decl.as_<Hs::FunDecl>();
        string name = unloc(fd.v.name);
        if (not cinfo.members.count(name))
            throw myexception()<<"'"<<name<<"' is not a member of class '"<<class_name<<"'";
        if (method_matches.count(name))
            throw myexception()<<"method '"<<name<<"' defined twice!";
        method_matches.insert({name,fd.match});
    }

    for(int i=(int)cinfo.fields.size()-(int)cinfo.members.size();i<cinfo.fields.size();i++)
    {
        auto& [name,type] = cinfo.fields[i];

        auto it = method_matches.find(name);
        if (it == method_matches.end())
            throw myexception()<<"instance "<<inst_decl.constraint<<" is missing method '"<<name<<"'";

        auto method = fresh_var(name,false);
        dict_entries.push_back(method);
        
        Hs::Decls decls;
        decls.push_back(Hs::FunDecl(method,it->second));
        binds_methods.push_back(decls);
    }
    
    // dfun = /\a1..an -> \dicts:theta -> let binds_super in let_binds_methods in <superdict_vars,method_vars>
    expression_ref dict = Hs::tuple(dict_entries);

    expression_ref E = Hs::LetExp( {noloc, binds_methods}, {noloc, dict} );
    E = Hs::LetExp( {noloc,*binds_super}, {noloc,E} );

    E = Hs::LambdaExp( lambda_vars, E);

    Hs::Decls decls ({ simple_decl(dfun,E) });
    return decls;
}

// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
Hs::Decls
typechecker_state::infer_type_for_instances2(const vector<pair<Hs::Var, Hs::InstanceDecl>>& named_instances, const class_env& ce)
{
    Hs::Decls out_decls;
    global_instance_env gie_inst;
    for(auto& [dfun, instance_decl]: named_instances)
    {
        auto decls_ = infer_type_for_instance2(dfun, instance_decl, ce);

        for(auto& d: decls_)
            out_decls.push_back(d);
    }
    return out_decls;
}

Hs::Kind result_kind_for_type_vars(vector<Hs::TypeVar>& type_vars, Hs::Kind k)
{
    for(auto& tv: type_vars)
    {
        // the kind should be an arrow kind.
        auto ka = k.to<KindArrow>();
        assert(ka);

        // record a version of the var with that contains its kind
        tv.kind = ka->k1;

        // set up the next iteration
        k = ka->k2;
    }
    // This is the result kind.
    return k;
}

Hs::Decls add_type_var_kinds(Hs::Decls type_decls, const type_con_env& tce)
{
    for(auto& type_decl: type_decls)
    {
        if (type_decl.is_a<Hs::DataOrNewtypeDecl>())
        {
            auto D = type_decl.as_<Hs::DataOrNewtypeDecl>();
            auto kind = tce.at(D.name).kind;
            result_kind_for_type_vars(D.type_vars, kind);
            type_decl = D;
        }
        else if (type_decl.is_a<Hs::ClassDecl>())
        {
            auto C = type_decl.as_<Hs::ClassDecl>();
            auto kind = tce.at(C.name).kind;
            result_kind_for_type_vars(C.type_vars, kind);
            type_decl = C;
        }
        else if (type_decl.is_a<Hs::TypeSynonymDecl>())
        {
            auto T = type_decl.as_<Hs::TypeSynonymDecl>();
            auto kind = tce.at(T.name).kind;
            result_kind_for_type_vars(T.type_vars, kind);
            type_decl = T;
        }
    }

    return type_decls;
}

Hs::ModuleDecls Module::typecheck( Hs::ModuleDecls M )
{
    // 1. Check the module's type declarations, and derives a Type Environment TE_T:(TCE_T, CVE_T)
    //    OK, so datatypes produce a
    //    * Type Constructor Environment (TCE) = tycon -> (kind, arity, method of applying the tycon?)
    //    * Constructor Value Environment (CVE)
    //
    // 2. Check the module's class declarations, produce some translated bindings -> binds_C ( GVE_C, CE_C, GIE_C )
    //
    // 3. We need to import/export:
    //    - TCE_T: type_con_env& tce                type -> kind
    //    - CVE_T: constr_env& state.con_info       constructor id -> type
    //    - GIE_C: global_instance_env state.gie    ??
    //      * superclass extractors?
    //      * instance functions to create dictionaries...
    //      * 
    //    - GVE_C: global_value_environment gve     id -> type (for class methods)
    //    - CE_C: class_info                        id -> class_info

    //    - GVE:   global_value_environment env     id -> type (for other values)
    //    - class_binds                             id -> body        = unused?
    //
    // 4. Should imports/export only affect what NAMES are in scope, or also things like the instance environment?

    // TCE_T = type con info, part1
    auto tce = get_tycon_info( M.type_decls );
    for(auto& [tycon,ka]: tce)
    {
        auto& [k,arity] = ka;
        std::cerr<<tycon<<" :: "<<k.print()<<"\n";
    }
    M.type_decls = add_type_var_kinds(M.type_decls, tce);
    std::cerr<<"\n";

    // CVE_T = constructor types :: map<string, polytype> = global_value_env
    auto constr_info = get_constructor_info(M.type_decls, tce);

    for(auto& [con,type]: constr_info)
    {
        std::cerr<<con<<" :: "<<type.print()<<"\n";
    }
    std::cerr<<"\n";

    //   CE_C  = class name -> class info
    typechecker_state state( name, *this, M, tce, constr_info );
    auto [gve, class_gie, class_info, class_binds] = state.infer_type_for_classes(M.type_decls);
    // GVE_C = {method -> type map} :: map<string, polytype> = global_value_env

    for(auto& [method,type]: gve)
    {
        std::cerr<<method<<" :: "<<type.print()<<"\n";
    }
    std::cerr<<"\n";

    for(auto& [method,type]: class_gie)
    {
        std::cerr<<method<<" :: "<<type.print()<<"\n";
    }
    std::cerr<<"\n";

    std::cerr<<class_binds.print()<<"\n";
    std::cerr<<"\n";

    state.gie = class_gie;
    auto [inst_gie, named_instances] = state.infer_type_for_instances1(M.type_decls, class_info);

    state.gie += inst_gie;

    for(auto& [method,type]: inst_gie)
    {
        std::cerr<<method<<" :: "<<type.print()<<"\n";
    }
    std::cerr<<"\n\n";

    // 3. E' = (TCE_T, (CVE_T, GVE_C, LVE={}), CE_C, (GIE_C, LIE={}))

    auto [value_decls, env] = state.infer_type_for_binds(gve, M.value_decls);
    M.value_decls = value_decls;

//    auto inst_decls = state.infer_type_for_instances2(M.type_decls, class_info);
//    std::cerr<<inst_decls.print();
//    std::cerr<<"\n\n";
    
    auto simpl_binds = state.reduce_current_lie();
    
    ranges::insert(simpl_binds, simpl_binds.end(), M.value_decls);
    M.value_decls = simpl_binds;

    auto default_binds = state.default_subst();
    env = state.apply_current_subst(env);
    ranges::insert(M.value_decls, M.value_decls.begin(), default_binds);

    for(auto& [x,t]: env)
    {
        std::cerr<<x<<" :: "<<alphabetize_type(t)<<"\n";
//        std::cerr<<x<<" = "<<e<<"\n\n\n";
    }
    std::cerr<<"\n";

    std::cerr<<M.value_decls.print();
    std::cerr<<"\n\n";

    // Record kinds on the type symbol table
    for(auto& [typecon,info]: state.tce)
    {
        if (get_module_name(typecon) == name)
        {
            auto& T = types.at(typecon);
            assert(not T.kind);
            T.kind = info.kind;
        }
    }

    // Record types on the value symbol table
    for(auto& [value,type]: env)
    {
        if (get_module_name(value) == name)
        {
            auto& V = symbols.at(value);
            V.type = type;
        }
    }

    // how about instances?
    // how about type synonyms?
    // how about class -> methods?
    // how about datatype -> constructors?

    return M;
}

    // GIE_C = functions to extract sub-dictionaries from containing dictionaries?
    // NOT IMPLEMENTED YET.

    // 3. E' = (TCE_T, (CVE_T, GVE_C, {}), CE_C, (GIE_C,{}))
    //
    // 4. Check the module's instance declarations -> monobinds : GIE_I
    //    These are mutually recursive with the value declarations. ?!?
    //
    // 5. Check the module's value declarations.

    // FIXME: Handle instances.

    // Instances: an instance is a function from dictionaries a dictionaries.
    //    instance (A a, B b) => A (b a) is a function of the form \dict_A_a dict_B_b -> dict_A_(b_a)

    // Q: How are instances grouped?
    // A: Each instance needs to be at-or-after all the types/classes referenced,
    //    Do instances depend on other instances?  Maybe this is check in the context...
    //    e.g. instance Eq a => Eq [a] where

    // See equivalents in GHC Rename/Module.hs
    // We are trying to find strongly connected components of
    //  types, type classes, and instances.

    // Shouldn't instances be AFTER everything?
    // * We only have type class instances (ClsInstDecl), but GHC
    //   also has data family instances and type family instances.

    // GHC looks at types and classes first, then adds instances to the SCCs later.


    // 5. Compute types for functions.

    //   Does the type-checker need to augment all bound variables with their type?

    //   Does the type-checker need to add type lambdas?

    //   Does the type-checker need to specify type arguments to type lambdas?

    //   So, let, lambda, and case would need to specify the type

    // 6. Compute types for class default methods?

    // Q: How are default method declarations handled here?
    //    Do they affect type class resolution?
    //    Do we need to do more work on them when handling value decls?
    // A: I think default methods do not affect the type.

    // See function `rnTyClDecls`, which calls `depAnalTyClDecls`.
    // * Dependency analysis on values can be done by name, since instances are not included.
    // * Code is in GHC.Data.Graph.Directed.

    // I don't think we need to look up "parents" during typechecking unless we are promoting data constructors
    // to types or kinds.

    // For values, each value can have a body decl, a fixity decl, and a signature decl.
    // So we can't use the decl itself as the key -- we have to use something like the name.

    // It looks like GHC rename extracts the "free variables" from everything.
    // For example: rnSrcInstDecl operates on ClsInstD, which wraps ClsInstDecl from Hs/Decl.hs

    // FreeVars = OccEnv ID.  See Core/Opt/OccurAnal.hs.

    // Looks like code for determining inlining


/*
 Examples #1:
(i,j) = (\x ->x, \y -> y)

i_mono :: a -> a
j_mono :: b -> b 

i = /\a. i' (@a) Any
j = /\b. j' Any (@b)


AbsBinds [p_a2MR, p_a2MW] []
  {Exports: [i <= i_a2ML
               wrap: /\(@ p_a2N4). <> @ p_a2N4 @ GHC.Types.Any,
             j <= j_a2MN
               wrap: /\(@ p_a2Nd). <> @ GHC.Types.Any @ p_a2Nd]
   Exported types: i :: forall p. p -> p
                   [LclId]
                   j :: forall p. p -> p
                   [LclId]
   Binds: (i_a2ML, j_a2MN) = (\ x_a1Zd -> x_a1Zd, \ y_a1Ze -> y_a1Ze)
   Evidence: [EvBinds{}]}

 Example #2:

i :: Int -> Int
(i,j) = (\x ->x, \y -> y)

i = case tup Any of (i,j) -> i
j = /\a.case tup a of (i,j) -> j

AbsBinds [p_a2MV] []
  {Exports: [i <= i_a2MK
               wrap: <> @ GHC.Types.Any,
             j <= j_a2MR
               wrap: <>]
   Exported types: i :: Int -> Int
                   [LclId]
                   j :: forall p. p -> p
                   [LclId]
   Binds: (i_a2MK, j_a2MR) = (\ x_a1Z9 -> x_a1Z9, \ y_a1Za -> y_a1Za)
   Evidence: [EvBinds{}]}

 */
