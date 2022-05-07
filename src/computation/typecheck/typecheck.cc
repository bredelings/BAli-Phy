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
  *. Change the type of class methods to forall a.C a => (forall b. ctxt => body)

  TODO:
  1. Reject unification of variables, tycons, etc with different kinds.
     - Ensure that all ForallType binders have kinds.
     - Assign kinds to all TypeCons.... OR look it up in the symbol table when we need to!
  2. Process type signatures for ambiguity and type synonyms.
  3. Emit code for instances and check if there are instances for the context.
     - 3a. Emit code for default methods
           + Out of line, $dm<op>#
           + Take (C a) argument.
           + Typecheck with C a + method constraints in scope...
             - Do we use the single_fundecl case?
             - Or do we steal code from that to set up a series of givens?
     - 3b. Emit code for instance methods
          + Out of line, $i<op>$
          + Take instance constraint dictionaries as arguments
          + Construct the dfun for the instance
          + Typecheck with C a + method constraints in scope...
     - 3c. Emit code for the dfun
          + Take instance constraint dictionaries as arguments
          + Construct superclass dictionaries in a simpler way?
          + Construct a record that references the superclass dictionary variables bound in binds_super?
          + The record should then call methodA d1 d2 d3, methodB d1 d2 d3, etc.
     - 3d. There is still the question of how to optimize recursion between the method ops and the dfun.
          + For example, if we have $i==# = \ (x:xs) (y:ys) -> (x == y) && (xs == ys) then

(==) (x,y) = x

dfun da = (i$== da, i$\= da)

i$== = \ da -> let dLista = dfun da 
                   f (x:xs) (y:ys) -> ((==) da x y) && ((==) dLista xs ys) 
               in f

i$== = \ da -> let dLista = ($i== da, $i\= da)
                   f (x:xs) (y:ys) -> ((==) da x y) && ((\(x,y)->x) dLista xs ys) 
               in f

i$== = \ da -> let 
                   f (x:xs) (y:ys) -> ((==) da x y) && ($== da xs ys) 
               in f

i$== = \ da -> let tmp1 = (==) da
                   tmp2 = $i== da
                   f (x:xs) (y:ys) -> (tmp1 x y) && (tmp2 xs ys) 
               in f

So... whenever we have a method selector applied to a dfun, then if we inline BOTH, we
will get a direct reference to the method operation?

And we want to do this regardless of how many times method selector and the dfun appear?

  4. Record impedance-matching info on GenBind
     - unused types
     - defaulted types
     - defaulted dictionaries.
  5. Write more-general impedance-matching code.
  5. Record types on let, lambda, and case binders.
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

    // Instances, pass1
    state.gie = class_gie;
    auto [inst_gie, named_instances] = state.infer_type_for_instances1(M.type_decls, class_info);

    state.gie += inst_gie;

    for(auto& [method,type]: inst_gie)
    {
        std::cerr<<method<<" :: "<<type.print()<<"\n";
    }
    std::cerr<<"\n\n";

    // 3. E' = (TCE_T, (CVE_T, GVE_C, LVE={}), CE_C, (GIE_C, LIE={}))

    // Value decls
    auto [value_decls, env] = state.infer_type_for_binds(gve, M.value_decls);
    M.value_decls = value_decls;

    // Default methods
    // auto default_method_decls = state.infer_type_for_default_methods(gve, class_info);

    // Instances, pass2
    auto inst_decls = state.infer_type_for_instances2(named_instances, class_info);
    std::cerr<<inst_decls.print();
    std::cerr<<"\n\n";

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
