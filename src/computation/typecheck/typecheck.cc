#include "typecheck.H"
#include "kindcheck.H"

#include <set>

#include <range/v3/all.hpp>

#include "haskell/haskell.H"

#include "immer/map.hpp" // for immer::map

#include "util/set.H"
#include "util/string/join.H"

#include "util/graph.H" // for get_ordered_strong_components( )

#include "computation/expression/apply.H"
#include "computation/expression/tuple.H" // for is_tuple_name( )
#include "computation/operation.H" // for is_non_apply_op( )
#include "haskell/ids.H"

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
  * Mark instances, default methods, and superclass extractors as exported.
  * Export ALL typechecker state between modules.
  * Make a version of the string-to-[Char] routine that makes sense type-wise.
  * Always generalize at top level.
  * Rewrite builtin decls to look like `foreign import mycall "lib:fname" name :: type`
  * Typecheck do-expression, left-sections, and right sections.
  * Typecheck syntax operations directly: fromInteger, fromRational, (>>=), (>>), enumFrom, etc...
  * Record impedance-matching info in GenBind, but only for passing dictionaries.
  * Implement literals, including literal strings.
  * Replace let-bound vars w/o signatures with local monomorphic ids.
  * Handle type synonyms.
  * assert in try_insert that we are inserting a tau type with no foralls.
  * Make a different struct type for MetaTypeVar.
  * Make a constructor expression that must be fully applied
  * add tcRho( ).
  * skolemize in single_fundecl_with_sig
  * make a version of instantiation that invents and applied dictionaries by creating a wrapper.
  * make wrappers operate on Core
  * separate Core::Apply, Core::Let, Core::Lambda, Core::Tuple
  * add a wrapper to checkSigma( ).
  * record a result wrapper on ApplyExp
  * Don't desugar Core::Decls, just the Hs::Binds.  But pass them both to the optimizer.
  * Implement type classes Num, Enum, and Ix.
  * Make MetaTypeVar's point directly to their target.

  0. Speed up: add type variable levels.
    - where do we increment the level?
      + let-bindings
      + lambda-bindings?
  0. Speed up: don't import EVERYTHING
    - Also put the stuff from other modules in a different environment that we know has no meta-type-variables.
  0. Speed up: make types into a ref-counted type?
  0. Add Functor instance for Ix i => Array i.
     - We need to make another type (Array# e) that is indexed by Int.
     - We can then declare Array i e = Array i i (Array# e)
     - The Array then contains its own bounds.
     - The downside is that we need to subtract 0 from every index before we look it up.
  0. merge rename_pattern_from_bindinfo into tcPat( ) -> pass in (LetCtxt signatures (\id->monoid))
  0. remove desugar_pattern( )
  0. Make a quals class.
  0. Make a Pattern class.
     - we would have to re-implement infix expression handling -> infix pattern handling
     - does GHC do that?
  0. Make environments not just take a string ... make them take a Var/Con or a (String,Int) pair?
  0. Make an Expression class, similar to the Type class, that allows the object to be modified.
     - It can hold things in a cow_ptr
     - We probably need to also make a Pattern class
     - We probably need to make an ExpressionOrPattern class that we parse to, and then fix up.
  0. Cleanup: convert ListType, TupleType, to applied TypeCon earlier.
     - We need to alter TypeApp printing, then!
     - How about StrictLazyType?  Is this part of the type, or part of the declaration?
  0. Cleanup: eliminate dependencies on expression_ref:
     - Make Pattern into a Type that doesn't depend on expression_ref.
       - Make a LitPattern that compares Int, Double, String, Char by equality.
  0. Cleanup: move generalization code out of binds.cc to generalize.cc?
  0. Should/Can we get default_method_decls and superclass extractor decls from a class decl?
  0. Exporting and importing types and instances between modules:
     - Should we have separate maps for variables and constructors?
       - This includes classes, methods, data types, constructors, type synonyms, etc.
     - The small_decls won't have symbols though...  maybe we need annotated type info for that?
     - How do you export instances?
  0. Update importing/exporting of constructors and class methods -- in rename???
  0. Typecheck Rec, MDo, etc.
    * Rec is currently desugared in rename.
    * MDo is ignored.
  1. Reject unification of variables, tycons, etc with different kinds.
     - Ensure that all ForallType binders have kinds.
     - Assign kinds to all TypeCons.... OR look it up in the symbol table when we need to!
  2. Process type signatures for ambiguity (?)
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
    + When we generate a polymorphic call, sometimes the call is at the same type as the monomorphic call.
    + In those cases, we want to replace the polymorphic call with a monomorphic call... how?
  5. Record impedance-matching info for types on GenBind
     - Need to pass type/dict arguments in correct order for (forall a. C1 a => forall b. C2 b => a -> b -> a)
     - Instantiated type would otherwise be treated as (forall a b. (C1 a, C2 b) => a -> b -> a)
     - For defaulted defaulted types and defaulted dictionaries.
  0. Punt all defaulting to top level?
  6. Check that constraints in instance contexts satisfy the "paterson conditions"
  7. Handle export subspecs Datatype(constructor...) and Class(method...)
  8. AST nodes for type and evidence bindings?
  9. Factor generalization code out of typecheck/binds.cc
  10. Check that there are no cycles in the class hierarchy.
  11. Allow dictionaries as constructor entries.
     - How do we treat such entries as givens when we handle case alternatives?
     - Handle constraints on constructors and data types.
     - Remove the constraint from EmptySet
        - I guess we remove type-level constraints if they would be ambiguous.
        - I guess we COMPLAIN about user-specified constructor-level constraints if they would be ambiguous.
  0. Use UniqueString = {string,optional<int>} ids after rename.
    * Ideally, allow comparing just the integers.
    * If the integers are the same, the strings had better be the same too!
    * Should we record original names during rename?  What does GHC do?
    * Currently, I guess the ids must be globally unique within an entire program.
    * Later, if we allow separate compilation, we could allow ids to be unique within a module.
  11. Annotate let, lambda, and case binders with variable's type.
  12. Should we make AST for: 
     - \(dicts::theta) -> monobinds in binds    -> This is GenBinds
     - exp <dicts>
     - (superdicts, methods)
      We can then desugar these expressions to EITHER multiple dictionaries OR a tuple of dictionaries.
      Can we desugar the dictionary for class K to a data type K?
  18. Add basic error reporting.
  20. Handle literal constant patterns.  
      - we need to typecheck something like (==) (fromInteger x :: a) (case_object :: b)
      - then we would report the type b
      - this requires an Eq dictionary for the (==) and a Num dictionary (eventually) for the integer
      - we don't create an actual case_object expression, just unify the type of (==) with (a -> b -> c)
        and extract the b.
      - should we use an operator == for strings as well?
      - I guess (case x of 1 -> y; _ -> rest) changes to (case (x == 1) of True -> y; _ -> rest)
        + this is NOT a very efficient switch statement!
      - what if I add a switch x of {1 -> x, 2 -> y, etc.} operation for cases where x :: Int#?
      - we know that you can do (<) comparison on Int#...

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


Hs::TypeVar unification_env::fresh_tyvar(const std::optional<Hs::Kind>& kind) const
{
    int level = 0;
    Hs::TypeVar ftv(level, {noloc,"utv"+std::to_string(next_index)});
    ftv.index = next_index++;
    ftv.kind = kind;
    return ftv;
}

// Is there a better way to implement this?
bool typechecker_state::maybe_unify_(bool allow_unification, bool both_ways, const unification_env& env, const Hs::Type& t1, const Hs::Type& t2)
{
    // Translate rigid type variables
    if (auto tv1 = t1.to<Hs::TypeVar>(); tv1 and env.mapping1.count(*tv1))
    {
        assert(not tv1->is_skolem_constant());
        auto tv1_remapped = env.mapping1.at(*tv1);
        assert(tv1_remapped.is_skolem_constant());
        assert(not env.mapping1.count(tv1_remapped));
        return maybe_unify_(allow_unification, both_ways, env, tv1_remapped, t2);
    }
    else if (auto tv2 = t2.to<Hs::TypeVar>(); tv2 and env.mapping2.count(*tv2))
    {
        assert(not tv2->is_skolem_constant());
        auto tv2_remapped = env.mapping2.at(*tv2);
        assert(tv2_remapped.is_skolem_constant());
        assert(not env.mapping2.count(tv2_remapped));
        return maybe_unify_(allow_unification, both_ways, env, t1, tv2_remapped);
    }
    else if (auto tt1 = filled_meta_type_var(t1))
        return maybe_unify_(allow_unification, both_ways, env, *tt1, t2);
    else if (auto tt2 = filled_meta_type_var(t2))
        return maybe_unify_(allow_unification, both_ways, env, t1, *tt2);
    else if (auto tv1 = t1.to<Hs::MetaTypeVar>())
    {
        if (allow_unification)
            return try_insert(*tv1, t2);
        else
        {
            add_dvar(make_equality_constraint(*tv1, t2));
            return true;
        }
    }
    else if (auto tv2 = t2.to<Hs::MetaTypeVar>(); tv2 and both_ways)
    {
        if (allow_unification)
            return try_insert(*tv2, t1);
        else
        {
            add_dvar(make_equality_constraint(*tv2, t1));
            return true;
        }
    }
    else if (auto tv1 = t1.to<Hs::TypeVar>())
    {
        if (auto tv2 = t2.to<Hs::TypeVar>(); tv2 and *tv1 == *tv2)
            return true;
        else
            return false;
    }
    else if (t1.is_a<Hs::TypeApp>() and t2.is_a<Hs::TypeApp>())
    {
        auto& app1 = t1.as_<Hs::TypeApp>();
        auto& app2 = t2.as_<Hs::TypeApp>();

        return maybe_unify_(allow_unification, both_ways, env, app1.head, app2.head) and
               maybe_unify_(allow_unification, both_ways, env, app1.arg , app2.arg );
    }
    else if (t1.is_a<Hs::TypeCon>() and
             t2.is_a<Hs::TypeCon>() and
             t1.as_<Hs::TypeCon>() == t2.as_<Hs::TypeCon>())
    {
        return true;
    }
    else if (auto tup1 = t1.to<Hs::TupleType>())
    {
        return maybe_unify_(allow_unification, both_ways, env, canonicalize_type(*tup1), t2);
    }
    else if (auto tup2 = t2.to<Hs::TupleType>())
    {
        return maybe_unify_(allow_unification, both_ways, env, t1, canonicalize_type(*tup2));
    }
    else if (auto l1 = t1.to<Hs::ListType>())
    {
        return maybe_unify_(allow_unification, both_ways, env, canonicalize_type(*l1), t2);
    }
    else if (auto l2 = t2.to<Hs::ListType>())
    {
        return maybe_unify_(allow_unification, both_ways, env, t1, canonicalize_type(*l2));
    }
    else if (t1.is_a<Hs::ConstrainedType>() and t2.is_a<Hs::ConstrainedType>())
    {
        auto c1 = t1.to<Hs::ConstrainedType>();
        auto c2 = t2.to<Hs::ConstrainedType>();
        if (c1->context.constraints.size() != c2->context.constraints.size())
            return false;
        for(int i=0;i< c1->context.constraints.size();i++)
            if (not maybe_unify_(allow_unification, both_ways, env, c1->context.constraints[i], c2->context.constraints[i]))
                return false;
        return maybe_unify_(allow_unification, both_ways, env, c1->type, c2->type);
    }
    else if (t1.is_a<Hs::ForallType>() and t2.is_a<Hs::ForallType>())
    {
        auto fa1 = t1.to<Hs::ForallType>();
        auto fa2 = t2.to<Hs::ForallType>();

        if (fa1->type_var_binders.size() != fa2->type_var_binders.size())
            return false;

        auto env2 = env;
        for(int i=0;i < fa1->type_var_binders.size(); i++)
        {
            auto tv1 = fa1->type_var_binders[i];
            auto tv2 = fa2->type_var_binders[i];

            if (tv1.kind != tv2.kind) return false;

            auto v = env2.fresh_tyvar(tv1.kind);
            env2.mapping1 = env2.mapping1.insert({tv1,v});
            env2.mapping2 = env2.mapping2.insert({tv2,v});
        }

        return maybe_unify_(allow_unification, both_ways, env2, fa1->type, fa2->type);
    }
    else if (t1.is_a<Hs::StrictLazyType>() or t2.is_a<Hs::StrictLazyType>())
    {
        throw myexception()<<"maybe_unify "<<t1.print()<<" ~ "<<t2.print()<<": How should we handle unification for strict/lazy types?";
    }
    else
        return false;
}

global_tc_state::global_tc_state(const Module& m)
    :this_mod(m)
{ }


Hs::Type typechecker_state::expTypeToType(const Expected& E)
{
    if (E.infer())
        return inferResultToType(E);
    else
        return E.check_type();
}

Hs::Type typechecker_state::inferResultToType(const Expected& E)
{
    if (not E.infer())
        throw myexception()<<"inferResultToType: expected Infer, but got "<<E.print();

    if (auto T = E.inferred_type())
    {
        if (not is_rho_type(*T))
            throw myexception()<<"inferResultToType: expected monotype, but got Infer("<<T->print()<<")";
        return *T;
    }
    else
    {
        // This can now only be a monotype
        auto tv = fresh_meta_type_var( kind_star() );
        E.infer_type(tv);
        return tv;
    }
}

void typechecker_state::set_expected_type(const Expected& E, const Hs::Type& type)
{
    if (E.infer())
        E.infer_type(type);
    else
    {
        try {
            unify(E.check_type(), type);
        }
        catch (myexception& ex)
        {
            std::ostringstream header;
            header<<"Expected type\n\n";
            header<<"   "<<E.check_type()<<"\n\n";
            header<<"but got type\n\n";
            header<<"   "<<type<<"\n\n";

            ex.prepend(header.str());
            throw;
        }
    }
}

void typechecker_state::get_tycon_info(const Hs::Decls& type_decls)
{
    type_con_env new_tycons;

    auto type_decl_groups = find_type_groups(type_decls);

    // Compute kinds for type/class constructors.
    for(auto& type_decl_group: type_decl_groups)
    {
        kindchecker_state K( tycon_info() );

        auto new_tycons_for_group = K.infer_kinds(type_decl_group);

        new_tycons += new_tycons_for_group;

        // Later groups could refer to tycons from this group
        tycon_info() += new_tycons_for_group;
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
    ID class_name;
    std::vector<Hs::Type> argument_types;

    ID dfun_name;

    // forall <type_vars> . context => class_name argument_types[0] arguments[1] .. argument_types[n01]
    Hs::Type dfun_type() const
    {
        Hs::TypeCon class_con({noloc, class_name}); // whats the kind?
        return Hs::ForallType(type_vars, Hs::ConstrainedType(context, make_tyapps(class_con, argument_types)));
    }
};

LIE apply_subst(const substitution_t& s, const LIE& env1)
{
    LIE env2;
    for(auto& [x,type]: env1)
        env2.push_back({x, apply_subst(s,type)});
    return env2;
}

optional<ID> maybe_get_class_name_from_constraint(const Hs::Type& constraint)
{
    auto [tycon, args] = Hs::decompose_type_apps(constraint);
    if (auto tc = tycon.to<Hs::TypeCon>())
        return get_unqualified_name(unloc(tc->name));
    else
        return {};
}

ID get_full_class_name_from_constraint(const Hs::Type& constraint)
{
    auto [tycon, args] = Hs::decompose_type_apps(constraint);
    if (auto tc = tycon.to<Hs::TypeCon>())
        return unloc(tc->name);
    else
        throw myexception()<<"Can't get class name for constraint '"<<constraint<<"'";
}

ID get_class_name_from_constraint(const Hs::Type& constraint)
{
    if (auto name = maybe_get_class_name_from_constraint(constraint))
        return *name;
    else
        return "Constraint";
}

Hs::MetaTypeVar typechecker_state::fresh_meta_type_var(const std::string& name, const Hs::Kind& k)
{
    return FreshVarSource::fresh_meta_type_var(level, name, k);
}

Hs::MetaTypeVar typechecker_state::fresh_meta_type_var(const Hs::Kind& k)
{
    return FreshVarSource::fresh_meta_type_var(level, k);
}

Hs::TypeVar typechecker_state::fresh_rigid_type_var(const std::string& name, const Hs::Kind& k)
{
    return FreshVarSource::fresh_rigid_type_var(level, name, k);
}

Hs::TypeVar typechecker_state::fresh_rigid_type_var(const Hs::Kind& k)
{
    return FreshVarSource::fresh_rigid_type_var(level, k);
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

const TypeSynonymInfo* typechecker_state::maybe_find_type_synonym(const Hs::Type& type) const
{
    if (auto tycon = type.to<Hs::TypeCon>())
    {
        auto iter = type_syn_info().find( unloc(tycon->name) );
        if (iter != type_syn_info().end())
            return (&iter->second);
    }
    return nullptr;
}

std::optional<Hs::Type> typechecker_state::is_type_synonym(const Hs::Type& type) const
{
    if (type.is_a<Hs::TypeCon>() or type.is_a<Hs::TypeApp>())
    {
        auto [head, args] = Hs::decompose_type_apps(type);

        if (auto tsyn = maybe_find_type_synonym(head))
            return tsyn->expand(args);
    }

    return {};
}

void typechecker_state::expand_type_synonyms(Hs::Type& type) const
{
    if (type.is_a<Hs::TypeVar>())
        ;
    else if (auto l = type.to_modifiable<Hs::ListType>())
        expand_type_synonyms(l->element_type);
    else if (auto tup = type.to_modifiable<Hs::TupleType>())
    {
        for(auto& element_type: tup->element_types)
            expand_type_synonyms(element_type);
    }
    else if (auto c = type.to_modifiable<Hs::ConstrainedType>())
        expand_type_synonyms(c->type);
    else if (auto fa = type.to_modifiable<Hs::ForallType>())
        expand_type_synonyms(fa->type);
    else if (type.is_a<Hs::TypeCon>() or type.is_a<Hs::TypeApp>())
    {
        auto [head, args] = Hs::decompose_type_apps(type);

        while (auto tsyn = maybe_find_type_synonym(head))
        {
            auto type2 = tsyn->expand(args);
            std::tie(head,args) = Hs::decompose_type_apps(type2);
        }

        for(auto& arg: args)
            expand_type_synonyms(arg);

        type = Hs::make_tyapps(head,args);
    }
    else
        throw myexception()<<"expand_type_synonyms: I don't recognize type '"<<type<<"'";
}

Hs::Type typechecker_state::check_type(const Hs::Type& type, kindchecker_state& K) const
{
    // So, currently, we
    // (1) infer kinds for all the variables.
    // (2) then we all foralls.
    // Should we be doing synonym substitution FIRST?

    auto type2 = K.kind_and_type_check_type( type );
    expand_type_synonyms(type2);
    return type2;
}

Hs::Type typechecker_state::check_type(const Hs::Type& type) const
{
    // This should be rather wasteful... can we use a reference?
    kindchecker_state K( tycon_info() );

    return check_type(type, K);
}

Hs::Type typechecker_state::check_constraint(const Hs::Type& type) const
{
    // This should be rather wasteful... can we use a reference?
    kindchecker_state K( tycon_info() );

    return K.kind_and_type_check_constraint( type );

}

typechecker_state typechecker_state::copy_clear_wanteds() const
{
    auto tc2 = *this;
    tc2.current_wanteds() = {};
    return tc2;
}

typechecker_state typechecker_state::copy_inc_level_clear_wanteds() const
{
    auto tc2 = *this;
    tc2.current_wanteds() = {};
    tc2.level++;
    return tc2;
}

void typechecker_state::promote_mtv(const Hs::MetaTypeVar& mtv)
{
    assert(mtv.level() > level);
    mtv.fill( fresh_meta_type_var(unloc(mtv.name), *mtv.kind) );
}

bool typechecker_state::maybe_promote_mtv(const Hs::MetaTypeVar& mtv)
{
    if (mtv.level() > level)
    {
        assert(mtv.level() == level + 1);
        promote_mtv(mtv);
        return true;
    }
    else
        return false;
}

void typechecker_state::promote(Hs::Type t)
{
    for(auto& mtv: free_meta_type_variables(t))
        maybe_promote_mtv(mtv);
}

void typechecker_state::add_binders(const local_value_env& binders)
{
    gve = plus_prefer_right( gve, binders );
}

typechecker_state
typechecker_state::copy_add_binders(const local_value_env& binders) const
{
    auto new_state = copy_clear_wanteds();
    new_state.add_binders( binders );
    return new_state;
}

WantedConstraints& typechecker_state::current_wanteds()
{
    return collected_wanteds;
}

void typechecker_state::add_dvar(const Core::Var& dvar, const Hs::Type& constraint)
{
    current_wanteds().simple.push_back( {dvar, constraint} );
}

Core::Var typechecker_state::fresh_dvar(const Hs::Type& constraint)
{
    ID name = "dvar";
    if (auto cname = maybe_get_class_name_from_constraint(constraint))
        name = "d" + *cname;
    auto dvar = get_fresh_var(name, false);
    dvar.type_ = std::make_shared<Hs::Type>(constraint);
    return dvar;
}

Core::Var typechecker_state::add_dvar(const Hs::Type& constraint)
{
    auto dvar = fresh_dvar(constraint);

    add_dvar(dvar, constraint);

    return dvar;
}

void typechecker_state::get_defaults(const Hs::ModuleDecls& M)
{
    if (M.default_decl)
        defaults() = M.default_decl->types;
    else
        defaults() = { Hs::TypeCon({noloc,"Int"}), Hs::TypeCon({noloc,"Double"}) };
}

typechecker_state::typechecker_state(FreshVarState& fvs, const string& s, const Module& m)
    :FreshVarSource(fvs, s)
{
    global_state = std::make_shared<global_tc_state>(m);
}

Hs::Var typechecker_state::find_prelude_var(string name) const
{
    if (this_mod().is_declared(name))
        name = this_mod().lookup_symbol(name).name;
    return Hs::Var({noloc, name});
}

ID typechecker_state::find_prelude_tycon_name(const string& name) const
{
    if (this_mod().type_is_declared(name))
        return this_mod().lookup_type(name).name;
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

Hs::Type typechecker_state::int_type() const
{
    return find_prelude_tycon("Int");
}

Hs::Type typechecker_state::double_type() const
{
    return find_prelude_tycon("Double");
}

bool typechecker_state::add_substitution(const Hs::MetaTypeVar& a, const Hs::Type& type)
{
    return try_insert(a, type);
}


void typechecker_state::unify(const Hs::Type& t1, const Hs::Type& t2)
{
    myexception e;
    e<<"Unification failed: "<<t1<<" !~ "<<t2;
    unify(t1, t2, e);
}

void typechecker_state::unify(const Hs::Type& t1, const Hs::Type& t2, const myexception& e)
{
    unification_env env;

    if (not maybe_unify_(false, true, env, t1,t2))
        throw e;
}

bool typechecker_state::maybe_match(const Hs::Type& t1, const Hs::Type& t2)
{
    unification_env env;

    return maybe_unify_(true, false, env, t1, t2);
}

void typechecker_state::match(const Hs::Type& t1, const Hs::Type& t2, const myexception& e)
{
    if (not maybe_match(t1,t2))
        throw e;
}

void typechecker_state::match(const Hs::Type& t1, const Hs::Type& t2)
{
    auto e = myexception()<<"match failed: "<<t1<<" !~ "<<t2;
    match(t1,t2,e);
}


std::pair<Hs::Type, Hs::Type> typechecker_state::unify_function(const Hs::Type& t)
{
    auto e = myexception()<<"Not a function type:\n\n   "<<t;
    return unify_function(t, e);
}


std::pair<Hs::Type, Hs::Type> typechecker_state::unify_function(const Hs::Type& t, const myexception& e)
{
    assert(is_rho_type(t));
    if (auto arg_and_result = Hs::is_function_type(t))
        return *arg_and_result;
    else
    {
        auto a = fresh_meta_type_var( kind_star() );
        auto b = fresh_meta_type_var( kind_star() );
        unify(t, make_arrow_type(a,b), e);
        return {a,b};
    }
}


Hs::Type typechecker_state::constructor_type(const Hs::Con& con)
{
    auto& con_name = unloc(con.name);

    if (con_name == ":")
    {
        auto a = fresh_other_type_var( kind_star() );
        return Hs::add_forall_vars({a},Hs::function_type({a, Hs::ListType(a)}, Hs::ListType(a)));
    }
    else if (con_name == "[]")
    {
        auto a = fresh_other_type_var( kind_star() );
        return Hs::add_forall_vars({a},Hs::function_type({}, Hs::ListType(a)));
    }
    else if (is_tuple_name(con_name) or con_name == "()")
    {
        int n = tuple_arity(con_name);
        vector<Hs::Type> types;
        vector<Hs::TypeVar> tvs;
        for(int i=0;i<n;i++)
        {
            auto tv = fresh_other_type_var( kind_star() );
            types.push_back( tv );
            tvs.push_back( tv );
        }

        return Hs::add_forall_vars(tvs, Hs::function_type(types, Hs::TupleType(types)));
    }

    if (not con_info().count(con_name))
        throw myexception()<<"Unrecognized constructor: "<<con;

    return con_info().at(con_name);
}


value_env add_constraints(const std::vector<Haskell::Type>& constraints, const value_env& env1)
{
    value_env env2;
    for(auto& [name, monotype]: env1)
        env2 = env2.insert( {name, Hs::add_constraints(constraints, monotype)} );
    return env2;
}

// should return a wrapper!

// OK, so this returns something of type exp_sigma
Core::wrapper typechecker_state::checkSigma(Hs::Expression& E, const Hs::SigmaType& sigma_type)
{
    // 1. skolemize the type
    auto [wrap_gen, tvs, givens, rho_type] = skolemize(sigma_type, true);

    // 2. typecheck
    auto tcs2 = copy_clear_wanteds();
    tcs2.tcRho(E, Check(rho_type));
    auto new_wanteds = tcs2.current_wanteds();

    // 3. Check for escaped skolem variables
    auto s2 = sigma_type;
    auto free_tvs = free_type_variables(gve);
    add(free_tvs, free_type_variables(s2));
    for(auto ftv: free_tvs)
        if (includes(tvs, ftv))
            throw myexception()<<"Type not polymorphic enough";

    // 4. check that the remaining constraints are satisfied by the constraints in the type signature
    auto [ev_decls, non_entailed_wanteds] = entails( givens, new_wanteds );

    // 5. put wanteds without skolem variables into the current LIE
    LIE lie_failed;
    for(auto& [dvar, constraint]: non_entailed_wanteds)
    {
        if (intersects(free_type_variables(constraint), tvs))
            lie_failed.push_back({dvar,constraint});
        else
            current_wanteds().simple.push_back({dvar, constraint});
    }

    if (not lie_failed.empty())
        throw myexception()<<"Can't derive constraints '"<<print(lie_failed)<<"' from specified constraints '"<<print(givens)<<"'";

    // 6. modify E, which is of type rho_type, to be of type sigma_type
    return wrap_gen * Core::WrapLet(ev_decls);
}

// The idea is that we need an e2, but we have a t1.
// So, if we can make an e2 from the t1, then we are good.
// The wrapper is evidence that we can, and also converts a value of type t1 to a value of type e2.
Core::wrapper typechecker_state::subsumptionCheck(const Hs::Type& t1, const Expected& e2)
{
    if (auto t2 = e2.read_type_maybe())
        return subsumptionCheck(t1, *t2);
    else
    {
        e2.infer_type(t1);
        return Core::wrapper_id;
    }
}

Core::wrapper typechecker_state::subsumptionCheck(const Hs::Type& t1, const Hs::Type& t2)
{
    /*
      If we can make y :: t2 out of x :: t1 then it is OK.  (without doing eta reduction, according to QL).
      We return a wrapper that perform this conversion.

      Q: How do we handle dictionary constraints?

      A: When doing this conversion, we are allowed to make up dictionary variables for any constraints that
      don't contain skolem variables, and emit them into the current LIE.

      A: It is very much possible for t2 to be a simple meta-type-variable, in which case ALL
      wanted constraints will be emitted into the current LIE.

      Example 1. If t1 = (forall a b . a -> b) and t2 = (forall c. c -> c) then
           y = x @c @c

      Example 2. If t1 = (forall a. Eq a => a)  and t2 = (forall b. Ord b => b), then
           y = /\b.\dictOrd -> x b (eqFromOrd dictOrd)

      Example 3. If t1 = (forall a. Eq a => a) and t2 = Int then
           y = x Int dictEqInt
      where entails checks that there is actually an instance for (Eq Int)?

      Example 4. If t1 = (forall a. Eq a => a -> a) and t2 = b (meta type var) then
           b ~ a -> a
      and constraints1 = {Eq a}.  Currently this fails, because we can't infer Eq a.
      However, perhaps we should add Eq a to the environment?
    */

    auto [wrap_gen, tvs2, givens, type2] = skolemize(t2, true);

    auto tcs2 = copy_clear_wanteds();
    auto [wrap_apply, type1] = tcs2.instantiate_emit(t1);
    unify(type1, type2);
    auto wanteds = tcs2.current_wanteds();

    // We are looking for type variables in t1, not type1.
    // This will only contain skolem variables if t1 contains a meta-variable BEFORE instantiation.
    for(auto ftv: free_type_variables(t1))
        if (includes(tvs2, ftv))
            throw myexception()<<"Type not polymorphic enough";

    // I *think* that we can just issue some of the wanteds into the environment.
    // This is what would happen if t2 is just a meta-type-variable.
    //
    // But if t2 contains a skolem variable from tvs2, then we can't issue it into the environment.
    // How about if it contains a meta-type variable

    auto [decls, non_entailed_wanteds] = entails(givens, wanteds);

    for(auto& [dvar, constraint]: non_entailed_wanteds)
    {
        for(auto& ftv: free_type_variables(constraint))
            if (includes(tvs2, ftv))
                throw myexception()<<"Type\n\n  "<<t1<<"\n\n  does not subsume\n\n  "<<t2;
    }

    collected_wanteds += non_entailed_wanteds;

    return wrap_gen * Core::WrapLet(decls) * wrap_apply;
}

std::tuple<Core::wrapper, Hs::Type>
typechecker_state::instantiate_emit(const Hs::Type& polytype)
{
    auto [_, wanteds, rho_type] = instantiate(polytype);

    collected_wanteds += wanteds;

    auto dict_args = vars_from_lie( dictionary_constraints( wanteds ) );

    return {Core::WrapApply(dict_args), rho_type};
}

Core::wrapper
typechecker_state::instantiateSigma(const Hs::Type& polytype, const Expected& exp_type)
{
    if (exp_type.infer())
    {
        auto [wrap, rho_type] = instantiate_emit(polytype);
        exp_type.infer_type(rho_type);
        return wrap;
    }
    else
    {
        // why would this be a rho?
        // assert(is_rho_type(exp_type.check_type()));
        try {
            return subsumptionCheck(polytype, exp_type.check_type());
        }
        catch (myexception& ex)
        {
            std::ostringstream header;
            header<<"Expected type\n\n";
            header<<"   "<<exp_type.check_type()<<"\n\n";
            header<<"but got type\n\n";
            header<<"   "<<polytype<<"\n\n";

            ex.prepend(header.str());
            throw;
        }
    }
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

tuple<vector<Hs::MetaTypeVar>, LIE, Hs::Type> typechecker_state::instantiate(const Hs::Type& t)
{
    // 1. Handle foralls
    vector<Hs::MetaTypeVar> tvs;
    LIE wanteds;
    Hs::Type type = t;

    if (auto fa = type.to<Hs::ForallType>())
    {
        substitution_t s;
        for(auto& tv: fa->type_var_binders)
        {
            assert(tv.kind);
            auto new_tv = fresh_meta_type_var(*tv.kind);
            s = s.insert({tv,new_tv});

            tvs.push_back(new_tv);
        }
        type = fa->type;
        type = apply_subst(s,type);
    }

    // 2. Handle constraints
    if (auto ct = type.to<Hs::ConstrainedType>())
    {
        wanteds = constraints_to_lie(ct->context.constraints);
        type = ct->type;
    }

    // 3. Handle the exposed type being a polytype
    if (not tvs.empty() or not wanteds.empty())
    {
        auto [tvs2, wanteds2, type2] = instantiate(type);

        for(auto& tv2: tvs2)
            tvs.push_back(tv2);

        for(auto& wanted: wanteds2)
            wanteds.push_back(wanted);

        type = type2;
    }

    return {tvs, wanteds, type};
}

tuple<Core::wrapper, vector<Hs::TypeVar>, LIE, Hs::Type> typechecker_state::skolemize(const Hs::Type& t, bool skolem)
{
    // 1. Handle foralls
    vector<Hs::TypeVar> tvs;
    LIE givens;
    Hs::Type type = t;
    Core::wrapper wrap = Core::wrapper_id;
    
    if (auto fa = type.to<Hs::ForallType>())
    {
        substitution_t s;
        for(auto& tv: fa->type_var_binders)
        {
            assert(tv.kind);
            Hs::TypeVar new_tv;
            if (skolem)
                new_tv = fresh_rigid_type_var(*tv.kind);
            else
                new_tv = fresh_other_type_var(*tv.kind);
            s = s.insert({tv,new_tv});

            tvs.push_back(new_tv);
        }
        type = fa->type;
        type = apply_subst(s,type);

        // TODO: wrap = Core::WrapLambdaTypes(fa->type_var_binders) * wrap;
    }

    // 2. Handle constraints
    if (auto ct = type.to<Hs::ConstrainedType>())
    {
        givens = constraints_to_lie(ct->context.constraints);

        type = ct->type;

        auto dict_vars = vars_from_lie(givens);
        
        wrap = Core::WrapLambda( dict_vars );
    }

    // 3. Handle the exposed type being a polytype
    if (not tvs.empty() or not givens.empty())
    {
        auto [wrap2, tvs2, givens2, type2] = skolemize(type, skolem);

        wrap = wrap * wrap2;

        for(auto& tv2: tvs2)
            tvs.push_back(tv2);

        for(auto& given:  givens2)
            givens.push_back(given);

        type = type2;
    }

    return {wrap, tvs, givens, type};
}

LIE typechecker_state::constraints_to_lie(const vector<Hs::Type>& constraints)
{
    LIE ordered_lie;
    for(auto& constraint:constraints)
    {
        auto dvar = fresh_dvar(constraint);
        ordered_lie.push_back({dvar, constraint});
    }
    return ordered_lie;
}

Hs::Type remove_top_level_foralls(Hs::Type t)
{
    while(auto fa = t.to<Hs::ForallType>())
        t = fa->type;
    return t;
}

void typechecker_state::get_constructor_info(const Hs::Decls& decls)
{
    kindchecker_state ks( tycon_info() );

    for(auto& decl: decls)
    {
        auto d = decl.to<Hs::DataOrNewtypeDecl>();
        if (not d) continue;

        auto constr_map = ks.type_check_data_type(*d);
        for(auto& [name, type]: constr_map)
            con_info() = con_info().insert({name, check_type(type)});
    }

//     for(auto& [con,type]: con_info())
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
        tv.kind = ka->arg_kind;

        // set up the next iteration
        k = ka->result_kind;
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
            auto kind = tycon_info().at(D.name).kind;
            result_kind_for_type_vars(D.type_vars, kind);
            type_decl = D;
        }
        else if (type_decl.is_a<Hs::ClassDecl>())
        {
            auto C = type_decl.as_<Hs::ClassDecl>();
            auto kind = tycon_info().at(C.name).kind;
            result_kind_for_type_vars(C.type_vars, kind);
            type_decl = C;
        }
        else if (type_decl.is_a<Hs::TypeSynonymDecl>())
        {
            auto T = type_decl.as_<Hs::TypeSynonymDecl>();
            auto kind = tycon_info().at(T.name).kind;
            result_kind_for_type_vars(T.type_vars, kind);
            type_decl = T;
        }
    }

    return type_decls;
}

pair<Hs::Binds,Core::Decls> typechecker_result::all_binds() const
{
    Hs::Binds all = value_decls;
    all.signatures = {};

    ranges::insert(all, all.end(), default_method_decls);
    ranges::insert(all, all.end(), instance_method_decls);
    ranges::insert(all, all.end(), class_binds);

//    std::cerr<<"Haskell decls:\n";
//    std::cerr<<all.print();

    Core::Decls all2 = top_simplify_decls;
    all2 += dfun_decls;

//    std::cerr<<"\n\nCore decls:\n";
//    std::cerr<<print_cdecls(all2);
//    std::cerr<<"\n\n";

    return {all, all2};
}


typechecker_result Module::typecheck( Hs::ModuleDecls M )
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


    // 1. Get the types for defaulting.
    tc_state->get_defaults( M );

    // 2. Find the kind and arity of type constructors declared in this module ( TCE_T = type con info, part1 )
    tc_state->get_tycon_info( M.type_decls );

    // 3. Annotate tyvars in types with their kind.
    // Do we need this, could we do it in infer_type_for_classes/synonyms/data?
    M.type_decls = tc_state->add_type_var_kinds( M.type_decls );

    // 4. Get type synonyms
    tc_state->check_type_synonyms(M.type_decls);

    // 5. Get types for value constructors  (CVE_T = constructor types)
    tc_state->get_constructor_info(M.type_decls);

    // 6. Get types and values for class method selectors and superclass selectors (CE_C  = class name -> class info)
    auto class_binds = tc_state->infer_type_for_classes(M.type_decls);

    // 7. Get types and names for instances (pass 1)
    auto named_instances = tc_state->infer_type_for_instances1(M.type_decls);

    // 8. Get types for foreign imports
    tc_state->infer_type_for_foreign_imports(M.foreign_decls);

    // 9. Typecheck value decls
    auto value_decls = tc_state->infer_type_for_binds_top(M.value_decls);

    // 10. Typecheck default methods
    auto dm_decls = tc_state->infer_type_for_default_methods(M.type_decls);

    // 11. Typecheck instance methods and generate dfuns (pass 2)
    auto [instance_method_binds, dfun_decls] = tc_state->infer_type_for_instances2(named_instances);

    // 12. Default top-level ambiguous type vars.
    auto top_simplify_decls = tc_state->simplify_and_default_top_level();

    return {class_binds, value_decls, dm_decls, instance_method_binds, dfun_decls, top_simplify_decls};
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
