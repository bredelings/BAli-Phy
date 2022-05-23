#include "typecheck.H"
#include "kindcheck.H"

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
  * Change the type of class methods to forall a.C a => (forall b. ctxt => body)
  * Typecheck default instance methods in class declarations.
    + Out of line, take a (C a) argument.
    + Typecheck as FunDecl with signature forall a. C a => method_type
  * Typecheck instance methods instance declarations.
    + Out of line, take instance constraint dictionaries as arguments.
    + Substitute into class variables to get method type.
    + Typecheck as FunDecl with signature forall tvs.(instance constraints) => method_type
  * Handle different defaults for each entry of a GenBind by recording impedance matching info on BindInfo

  TODO:
  0. Exporting and importing types and instances between modules:
     - Some things have symbols -- we can add the type or kind to the symbol in the symbol table.
     - This includes classes, methods, data types, constructors, type synonyms, etc.
     - The small_decls won't have symbols though...  maybe we need annotated type info for that?
     - How do you export instances?
  1. Reject unification of variables, tycons, etc with different kinds.
     - Ensure that all ForallType binders have kinds.
     - Assign kinds to all TypeCons.... OR look it up in the symbol table when we need to!
  2. Process type signatures for ambiguity and type synonyms.
  3. Efficient handling of recursion between instance method and dictionaries.
       + For example, if we have i==@1 = \(x:xs) (y:ys) -> (x == y) && (xs == ys) then
         it would be nice to make the call for (xs == ys) call i==@1 directly, instead of
         allocating an Eq [a] dictionary and extracting its (==) element.
       + We should end up with something like (==) (dEqList dEqa) xs ys where
         dEqList dEqa = (i==@1 dEqa, i=\@2 dEq a).
       + If we could inline BOTH (==) and dEqList, then I think we'd get the result that we want.
       + Maybe we could inform the inliner that i==@1 is an method selector, and dEqList is a dfun?
       + And we want to do this regardless of how many times the method selector and the dfun appear?
       + GHC does something else...  see DFunUnfolding?
  4. Avoid a space leak with polymorphic recursion in cases like factorial.
  5. Record impedance-matching info for types on GenBind
     - Need to pass type/dict arguments in correct order for (forall a. C1 a => forall b. C2 b => a -> b -> a)
     - Instantiated type would otherwise be treated as (forall a b. (C1 a, C2 b) => a -> b -> a)
     - For defaulted defaulted types and defaulted dictionaries.
  6. Check that constraints in instance contexts satisfy the "paterson conditions"
  7. Handle export subspecs Datatype(constructor...) and Class(method...)
  8. AST nodes for type and evidence bindings?
  9. Factor generalization code out of typecheck/binds.cc
  10. Check that there are no cycles in the class hierarchy.

  11. Annotate let, lambda, and case binders with variable's type.
  12. Should we make AST for: 
     - \(dicts::theta) -> monobinds in binds    -> This is GenBinds
     - exp <dicts>
     - (superdicts, methods)
      We can then desugar these expressions to EITHER multiple dictionaries OR a tuple of dictionaries.
      Can we desugar the dictionary for class K to a data type K?
  12. Implement fromInt and fromRational
  13. Implement literal strings.  Given them type [Char] and turn them into lists during desugaring.
      Do I need to handle LiteralString patterns, then?
  16. Handle constraints on constructors.
  17. Remove the constraint from EmptySet
  18. Add basic error reporting.
  19. How do we handle things like Prelude.Num, Prelude.Enum, Prelude.fromInt, etc.
      Right now, maybe we can pick a Num / fromInt from the local scope, instead?
      This might require passing some information from the renamer into the typechecker...
  20. Handle literal constant patterns.  We need a Num or Fractional dictionary for
      Int or Double constants.  I guess we need an Eq Char, or Eq [Char] dictionary for
      characters or strings?

  Optimization:
  - If we have ((case x of alts) y z), should we sink the arguments?
    This could expose a case-of-case, for example.

  - If we have ((case x of alts) y z), can we float out the case?
    It seems that we are not doing so...

  - See note [ClassOp/DFun selection] in Tc/TyCl/Instance.hs
    + We often see op2 (df d1 d2)
    + dfuns (df) and class ops (op2) NEVER inline
    + always define dfuns in stylized form: df d1 d2 = MkD ($cop d1 d2) ($cop2 d1 d2) ...
    + df inlines to the magical form DFunUnfolding [$cop1, $cop2, ...]
    + Use GHC.Core.Unfold.exprIsConApp_maybe inlines DFunUnfoldings? ... how?
    + Each class op has a BuiltinRule that extracts the right piece whenever the argument is a ConApp..
    + Make 'df' "CONLIKE" so that shared uses still match:
      let d = df d1 d2
      in ...(op2 d) ... (op1 d) ...

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


void typechecker_state::get_tycon_info(const Hs::Decls& type_decls)
{
    type_con_env new_tycons;

    auto type_decl_groups = find_type_groups(type_decls);

    // Compute kinds for type/class constructors.
    for(auto& type_decl_group: type_decl_groups)
    {
        kindchecker_state K(tce);

        auto new_tycons_for_group = K.infer_kinds(type_decl_group);

        new_tycons += new_tycons_for_group;

        tce += new_tycons;
    }

//    for(auto& [tycon,ka]: new_tycons)
//    {
//        auto& [k,arity] = ka;
//        std::cerr<<tycon<<" :: "<<k.print()<<"\n";
//    }
//    std::cerr<<"\n";
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
    return get_fresh_Var(name, false);
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

typechecker_state::typechecker_state(FreshVarState& fvs, const string& s, const Module& m, const Hs::ModuleDecls& M)
    :FreshVarSource(fvs, s),
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


// FIXME:: Wrappers:
//
// Note that instantiate collapses any structure in leading foralls/contexts.
//
//    forall a. Eq a => forall b. Ord b => a -> b -> a --> [a,b]  + [Eq a, Ord b] + (a -> b -> a)
//
// So we may need a wrapper to create a
//
//    forall a. Eq a => forall b. Ord b => type
//
// from
//
//    forall a b. (Eq a, Ord b) => type
//
// Maybe something like:
//
//    /\(a::*).\dict (d1::Eq a) -> /\(b::*).\dict (d2::Ord b) -> <RESULT> a b d1 d2
//
// This would take the code for <RESULT> and add the other code around it.
//
// Now, actually, we may NOT need this until add type /\s, because the dictionary arguments should be in the right order.

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

vector<pair<Hs::Var,Hs::Type>> typechecker_state::constraints_to_lie(const vector<Hs::Type>& constraints)
{
    vector<pair<Hs::Var, Hs::Type>> ordered_lie;
    for(auto& constraint:constraints)
    {
        auto dvar = get_fresh_Var("dvar", false);
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

Hs::Type remove_top_level_foralls(Hs::Type t)
{
    while(auto fa = t.to<Hs::ForallType>())
        t = fa->type;
    return t;
}

void typechecker_state::get_constructor_info(const Hs::Decls& decls)
{
    kindchecker_state ks(tce);

    for(auto& decl: decls)
    {
        auto d = decl.to<Hs::DataOrNewtypeDecl>();
        if (not d) continue;

        auto constr_map = ks.type_check_data_type(*d);
        for(auto& [name, type]: constr_map)
            con_info = con_info.insert({name,type});
    }

//     for(auto& [con,type]: con_info)
//     {
//         std::cerr<<con<<" :: "<<type.print()<<"\n";
//     }
//     std::cerr<<"\n";
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

Hs::Decls typechecker_state::add_type_var_kinds(Hs::Decls type_decls)
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

Hs::ModuleDecls Module::typecheck( FreshVarState& fvs, Hs::ModuleDecls M )
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


    typechecker_state state( fvs, name, *this, M);

    // 1. Find the kind and arity of type constructors declared in this module ( TCE_T = type con info, part1 )
    state.get_tycon_info( M.type_decls );

    // 2. Annotate tyvars in types with their kind.
    M.type_decls = state.add_type_var_kinds( M.type_decls );

    // 3. Get types for value constructors  (CVE_T = constructor types)
    state.get_constructor_info(M.type_decls);

    // 4. Get types and values for class method selectors and superclass selectors (CE_C  = class name -> class info)
    auto class_binds = state.infer_type_for_classes(M.type_decls);

    // 5. Get types and names for instances (pass 1)
    auto named_instances = state.infer_type_for_instances1(M.type_decls);

    // 6. Typecheck value decls
    auto [value_decls, env] = state.infer_type_for_binds(state.gve, M.value_decls);
    state.gve += env;
    M.value_decls = value_decls;

    // 7. Typecheck default methods
    auto default_method_decls = state.infer_type_for_default_methods(M.type_decls);
    for(auto& dm_decl: default_method_decls)
        M.value_decls.push_back(dm_decl);

    // 8. Typecheck instance methods and generate dfuns (pass 2)
    auto inst_decls = state.infer_type_for_instances2(named_instances);
    M.value_decls.push_back(inst_decls);

    std::cerr<<"\nInstance ops and dfuns:\n";
    std::cerr<<inst_decls.print();
    std::cerr<<"\n\n";

    auto simpl_binds = state.reduce_current_lie();
    
    ranges::insert(simpl_binds, simpl_binds.end(), M.value_decls);
    M.value_decls = simpl_binds;

    auto default_binds = state.default_subst();
    state.gve = state.apply_current_subst(state.gve);
    ranges::insert(M.value_decls, M.value_decls.begin(), default_binds);

    // Insert class_defs
    ranges::insert(M.value_decls, M.value_decls.begin(), class_binds);

    std::cerr<<"GVE (all after defaulting):\n";
    for(auto& [x,t]: state.gve)
    {
        std::cerr<<x<<" :: "<<alphabetize_type(t)<<"\n";
//        std::cerr<<x<<" = "<<e<<"\n\n\n";
    }
    std::cerr<<"\n";

    std::cerr<<"All decls:\n";
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
    for(auto& [value,type]: state.gve)
    {
        if (get_module_name(value) == name)
        {
            if (not symbols.count(value))
                std::cerr<<"'"<<value<<"' is not a symbol!\n";
            else
            {
                auto& V = symbols.at(value);
                V.type = type;
            }
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
