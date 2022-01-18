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
  Done: 
  * Check that constraints in classes only mention type vars.
  * Check that constraints in instance heads are of the form Class (Tycon a1 a2 a3..)
  * Switch from substitutions to constraints (i.e. from compose( ) to combine( ) ).
  * Add a substitution to the typechecker_state, instead of returning substitutions from every call.
  * We no longer need to keep substituting into the type.
  * Handle exp :: type expressions.
  *. Monomorphism restriction.

  TODO:
  9. Create DictionaryLambda, or whatever we need to show the dictionary bindings.
  10. Defaulting.
    - Can we solve the problem with (a,b) = ('a,1) via defaulting?
  1. Check that constraints in instance contexts satisfy the "paterson conditions"
  2. How do we export stuff?
  3. Make functions to handle instance declarations from Figure 12.
  4. Handle instances in two passes:
     - Can we first the the NAME and TYPE for all the instance variables,
       and second generate the instance dfun bodies?
     - Possibly generating the dfun bodies AFTER the value declarations are done?
     - How do we figure out if the instance contexts can be satisfied in a mutally recursive manner?
  5. Make AST nodes for dictionary abstraction and dictionary application.
     - \(dicts::theta) -> monobinds in binds
     - exp <dicts>
     - (superdicts, methods)
      We can then desugar these expressions to EITHER multiple dictionaries OR a tuple of dictionaries.
      Can we desugar the dictionary for class K to a data type K?
  6. Implement explicitly typed bindings
  7. Implement fromInt and fromRational
  8. Implement literal strings.  Given them type [Char] and turn them into lists during desugaring.
      Do I need to handle LiteralString patterns, then?
  11. Emit code for instances and check if there are instances for the context.
  12. Handle a :: Num a => Char in (a,b) = ('a',1)
  13. Handle constraints on constructors.
  14. Remove the constraint from EmptySet
  15. Don't substitution into LIEs / LVEs / GVEs until we need to.
  16. Make unification stop throwing.
  17. Replace types with type synonyms.
  18. Reject unification of variables, tycons, etc with different kinds.
  19. Add basic error reporting.
  20. Make a stack of lies / lves?  Can we then automatically accumulate LIEs to the
      LIE on the top of the LIE stack?
  21. Should we consider having infer_type( ) modify expressions in-place instace of 
      returning a new one?
  22. How do we handle things like Prelude.Num, Prelude.Enum, Prelude.fromInt, etc.
      Right now, maybe we can pick a Num / fromInt from the local scope, instead?
      This might require passing some information from the renamer into the typechecker...
  23. Handle literal constant patterns.  We need a Num or Fractional dictionary for
      Int or Double constants.  I guess we need an Eq Char, or Eq [Char] dictionary for
      characters or strings?

  Questions:
  1. How do we handle predicates that make it to the top level?
  2. Is THIH correct about binding groups in Haskell 2010?
     - It seems that basically all the explicitly-typed things should be at the END.
     - In that case, every explicitly-typed thing would be in its OWN group.
     - We need the explicit types BEFORE we type the thing.
     - So they should be attached to Hs::Binds, not Hs::Decls.

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

  Q4: How do we process the instances in the correct order?
  A4: Can we put them into the type groups, and then REVISIT the groups,
      analyzing the instances last in each group?

  Q5. How do we type-check the method bindings, if their types depend on the VALUE bindings?
  A5. We know the types of the instance methods BEFORE we know the function bodies, right?
      So, we should be able to type-check the instance bodies LAST, if we generate the types
      for the value_decls first.

  Q6. How do we check that there are no super-class -> class cycles.

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

typedef value_env constr_env;

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

struct typechecker_state
{
    int next_var_index = 1;

    int next_tvar_index = 1;

    constr_env con_info;

    global_instance_env gie;

    std::string mod_name;

    const Module& this_mod; // for name lookups like Bool, Num, etc.

    substitution_t type_var_to_type;

    vector<local_instance_env> lie_stack;

    local_instance_env& current_lie() {return lie_stack.back();}

    void add_dvar(const string& name, const Hs::Type& constraint)
    {
        auto& lie = current_lie();
        assert(not lie.count(name));
        lie = lie.insert( {name, constraint} );
    }

    void add_dvar(const Hs::Var& dvar, const Hs::Type& constraint)
    {
        add_dvar(unloc(dvar.name), constraint);
    }

    Hs::Var add_dvar(const Hs::Type& constraint)
    {
        auto [tycon, args] = decompose_type_apps(constraint);
        string namebase = "dvar";
        if (auto tc = tycon.to<Hs::TypeCon>())
            namebase = "d" + get_unqualified_name(unloc(tc->name));
        Hs::Var dvar = fresh_var(namebase, false);

        add_dvar(dvar, constraint);

        return dvar;
    }

    void push_lie() {
        lie_stack.push_back( {} );
    }

    local_instance_env pop_lie()
    {
        auto lie = current_lie();
        lie_stack.pop_back();
        return lie;
    }

    void pop_and_add_lie()
    {
        auto lie = pop_lie();
        current_lie() += lie;
    }

    typechecker_state(const string& s, const Module& m, const constr_env& ce)
        :con_info(ce),
         mod_name(s),
         this_mod(m)
        {
            push_lie();
        }

    Hs::Var find_prelude_var(string name) const
    {
        if (this_mod.is_declared(name))
            name = this_mod.lookup_symbol(name).name;
        return Hs::Var({noloc, name});
    }

    Hs::TypeCon find_prelude_tycon(string name) const
    {
        if (this_mod.type_is_declared(name))
            name = this_mod.lookup_type(name).name;
        return Hs::TypeCon({noloc, name });
    }

    Hs::Type bool_type() const
    {
        return find_prelude_tycon("Bool");
    }

    Hs::Type char_type() const
    {
        return find_prelude_tycon("Char");
    }

    Hs::Type enum_class(const Hs::Type& arg) const
    {
        auto Enum = find_prelude_tycon("Enum");

        return Hs::TypeApp( Enum, arg);
    }

    Hs::Type num_class(const Hs::Type& arg) const
    {
        auto Num = find_prelude_tycon("Num");

        return Hs::TypeApp( Num, arg);
    }

    Hs::Type fractional_class(const Hs::Type& arg) const
    {
        auto Fractional = find_prelude_tycon("Fractional");

        return Hs::TypeApp( Fractional, arg);
    }

    tuple<Hs::Var, Hs::Type> fresh_enum_type()
    {
        Hs::Type a = fresh_type_var();
        Hs::Type enum_a = enum_class(a);
        auto dvar = add_dvar(enum_a);
        return {dvar, a};
    }

    tuple<Hs::Var, Hs::Type> fresh_num_type()
    {
        Hs::Type a = fresh_type_var();
        Hs::Type num_a = num_class(a);
        auto dvar = add_dvar(num_a);
        return {dvar, a};
    }

    tuple<Hs::Var, Hs::Type> fresh_fractional_type()
    {
        Hs::Type a = fresh_type_var();
        Hs::Type fractional_a = fractional_class(a);
        auto dvar = add_dvar(fractional_a);
        return {dvar, a};
    }

    template <typename T>
    T apply_current_subst(const T& thing) const
    {
        return apply_subst(type_var_to_type, thing);
    }

    bool add_substitution(const substitution_t& s)
    {
        if (auto s2 = combine(type_var_to_type, s))
        {
            type_var_to_type = *s2;
            return true;
        }
        else
            return false;
    }

    bool add_substitution(const Hs::TypeVar& a, const Hs::Type& type)
    {
        if (auto s = try_insert({}, a, type))
            return add_substitution(*s);
        else
            return false;
    }

    bool maybe_unify(const Hs::Type& t1, const Hs::Type& t2)
    {
        if (auto s = ::maybe_unify(t1,t2))
            return add_substitution(*s);
        else
            return false;
    }

    void unify(const Hs::Type& t1, const Hs::Type& t2)
    {
        if (not maybe_unify(t1,t2))
            throw myexception()<<"Unification failed: "<<t1<<" !~ "<<t2;
    }

    void match(const Hs::Type& t1, const Hs::Type& t2)
    {
        if (not ::match(t1,t2))
            throw myexception()<<"Matching failed: "<<t1<<" !~ "<<t2;
    }

    void match(const Hs::Type& t1, const Hs::Type& t2, const myexception& e)
    {
        if (not ::match(t1,t2))
            throw e;
    }

    pair<Hs::Type, vector<Hs::Type>> constr_types(const Hs::Con&);

    Hs::Var fresh_var(const std::string& s, bool qualified)
    {
        string name = "$"+s+std::to_string(next_var_index);
        if (qualified)
            name = mod_name + "." + name;
        Hs::Var x({noloc, name});
        next_var_index++;
        return x;
    }

    Hs::TypeVar fresh_type_var() {
        Hs::TypeVar tv({noloc, "t"+std::to_string(next_tvar_index)});
        next_tvar_index++;
        return tv;
    }

    Hs::TypeVar named_type_var(const string& name)
    {
        Hs::TypeVar tv({noloc, name+"_"+std::to_string(next_tvar_index)});
        next_tvar_index++;
        return tv;
    }

    pair<vector<Hs::Type>,Hs::Type> instantiate(const Hs::Type& t);

    // Figure 22.
    tuple<vector<Hs::Qual>, local_value_env>
    infer_quals_type(const global_value_env& env, vector<Hs::Qual> quals);

    // Figure 22.
    tuple<Hs::Qual, local_value_env>
    infer_qual_type(const global_value_env& env, const Hs::Qual& qual);

    tuple<Hs::Qual, local_value_env>
    infer_guard_type(const global_value_env& env, const Hs::Qual& guard);

    // Figure 24.
    tuple<Hs::Pattern, Hs::Type, local_value_env>
    infer_pattern_type(const Hs::Pattern& pat);

    tuple<expression_ref, Hs::Type>
    infer_type(const global_value_env& env, expression_ref exp);

    tuple<Hs::GuardedRHS, Hs::Type>
    infer_type(const global_value_env& env, Hs::GuardedRHS);

    tuple<Hs::MultiGuardedRHS, Hs::Type>
    infer_type(const global_value_env& env, Hs::MultiGuardedRHS);

    tuple<Hs::MRule, Hs::Type>
    infer_type(const global_value_env& env, Hs::MRule);

    tuple<Hs::Match, Hs::Type>
    infer_type(const global_value_env& env, Hs::Match);

    // Figures 13, 14, 15?
    tuple<Hs::Decls, global_value_env>
    infer_type_for_decls(const global_value_env& env, const map<string, Hs::Type>&, Hs::Decls E);

    // Figures 13, 14, 15?
    tuple<Hs::Binds, global_value_env>
    infer_type_for_binds(const global_value_env& env, Hs::Binds binds);

    tuple<global_value_env, global_instance_env, class_env, Hs::Binds>
    infer_type_for_classes(const Hs::Decls& decls, const type_con_env& tce);

    tuple<global_value_env,global_instance_env,class_info,Hs::Decls>
    infer_type_for_class(const type_con_env& tce, const Hs::ClassDecl& class_decl);

    // Figure 12
    global_instance_env
    infer_type_for_instances1(const Hs::Decls& decls, const class_env& ce);

    // Figure 12
    global_instance_env
    infer_type_for_instance1(const Hs::InstanceDecl& instance_del, const class_env& ce);

    // Figure 12
    Hs::Decls
    infer_type_for_instances2(const Hs::Decls& decls, const class_env& ce);

    // Figure 12
    Hs::Decls
    infer_type_for_instance2(const Hs::InstanceDecl& instance_del, const class_env& ce);

    // Figure 26
    // Express lie2 in terms of gie (functions) and lie1 (arguments to this dfun, I think).
    optional<Hs::Binds> get_dicts(const local_instance_env& lie1, const local_instance_env& lie2);

    local_instance_env constraints_to_lie(const vector<Hs::Type>&);

    vector<pair<string, Hs::Type>> superclass_constraints(const Hs::Type& constraint);

    std::optional<vector<string>>
    is_superclass_of(const Hs::Type&, const Hs::Type&);

    std::optional<Hs::Binds>
    entails_by_superclass(const pair<string, Hs::Type>& to_keep,
                          const pair<string, Hs::Type>& to_remove);

    template <typename T>
    std::optional<Hs::Binds> entails(const T& to_keep, const pair<string, Hs::Type>& to_check);

    optional<pair<Hs::Var,vector<Hs::Type>>> lookup_instance(const Hs::Type& constraint);

    pair<Hs::Binds,local_instance_env> toHnf(const string& name, const Hs::Type& constraint);
    pair<Hs::Binds, local_instance_env> toHnfs(const local_instance_env& lie_in);
    pair<Hs::Binds, local_instance_env> simplify(const local_instance_env& lie_in);
    pair<Hs::Binds, local_instance_env> reduce(const local_instance_env& lie_in);
};

set<Hs::TypeVar> free_type_variables(const Hs::Type& t);

pair<Hs::Type, vector<Hs::Type>> typechecker_state::constr_types(const Hs::Con& con)
{
    auto& con_name = unloc(con.name);

    if (con_name == ":")
    {
        Hs::Type a = fresh_type_var();
        return {Hs::ListType(a),{a,Hs::ListType(a)}};
    }
    else if (con_name == "[]")
    {
        Hs::Type a = fresh_type_var();
        return {Hs::ListType(a),{}};
    }
    else if (is_tuple_name(con_name))
    {
        int n = tuple_arity(con_name);
        vector<Hs::Type> types;
        for(int i=0;i<n;i++)
            types.push_back(fresh_type_var());
        return {Hs::TupleType(types),types};
    }

    // 1. Find the data type
    if (not con_info.count(con_name))
        throw myexception()<<"Unrecognized constructor: "<<con;
    auto [constraints, con_type] = instantiate(con_info.at(con_name));
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

std::set<Hs::TypeVar> free_type_variables(const Hs::Type& t)
{
    return free_type_VARS(t);
}

std::set<Hs::TypeVar> free_type_vars(const value_env& env)
{
    set<Hs::TypeVar> tvs;
    for(auto [_,type]: env)
        add(tvs, free_type_VARS(type));
    return tvs;
}

std::set<Hs::TypeVar> free_type_variables(const value_env& env)
{
    std::set<Hs::TypeVar> free;
    for(auto& [x,type]: env)
        add(free, free_type_variables(type));
    return free;
}

value_env add_constraints(const std::vector<Haskell::Type>& constraints, const value_env& env1)
{
    value_env env2;
    for(auto& [name, monotype]: env1)
        env2 = env2.insert( {name, add_constraints(constraints, monotype)} );
    return env2;
}

template <typename T>
Hs::Type quantify(const T& tvs, const Hs::Type& monotype)
{
    return Hs::ForallType(tvs | ranges::to<vector>, monotype);
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

    return Hs::ForallType(ftv1 | ranges::to<vector>, monotype);
}

pair<vector<Hs::Type>, Hs::Type> typechecker_state::instantiate(const Hs::Type& t)
{
    // 1. Handle foralls
    substitution_t s;
    auto type = t;
    while(auto fa = type.to<Hs::ForallType>())
    {
        for(auto& tv: fa->type_var_binders)
        {
            auto new_tv = fresh_type_var();
            new_tv.kind = tv.kind;
            s = s.insert({tv,new_tv});
        }
        type = fa->type;
    }
    type = apply_subst(s,type);

    // 2. Handle constraints
    vector<Hs::Type> constraints;
    if (auto ct = type.to<Hs::ConstrainedType>())
    {
        constraints = ct->context.constraints;
        type = ct->type;
    }
    return {constraints,type};
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

tuple<Hs::Binds, global_value_env>
typechecker_state::infer_type_for_binds(const global_value_env& env, Hs::Binds binds)
{
    global_value_env sig_env;
    for(auto& [name, type]: binds.signatures)
    {
        // FIXME!
        // We need to translate type synonyms and add foralls.
        Hs::Type type2 = apply_current_subst(type);
        type2 = add_forall_vars(free_type_variables(type) | ranges::to<vector>, type);
        sig_env = sig_env.insert({name, type2});
    }

    auto env2 = plus_prefer_right(env, sig_env);

    global_value_env binders;
    for(auto& decls: binds)
    {
        auto [decls1, binders1] = infer_type_for_decls(env2, binds.signatures, decls);
        decls = decls1;
        env2 = plus_prefer_right(env2, binders1);
        binders += binders1;
    }

    return {binds, binders};
}

bool type_is_hnf(const Hs::Type& type)
{
    auto [head,args] = decompose_type_apps(type);

    if (type.is_a<Hs::TypeVar>())
        return true;
    else if (type.is_a<Hs::TypeCon>())
        return false;
    else if (type.is_a<Hs::ListType>())
        return false;
    else if (type.is_a<Hs::TupleType>())
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

optional<pair<Hs::Var,vector<Hs::Type>>> typechecker_state::lookup_instance(const Hs::Type& constraint)
{
    for(auto& [name, type]: gie)
    {
        auto [instance_constraints, instance_head] = instantiate(type);

        // Skip if this is not an instance.
        if (constraint_is_hnf(instance_head)) continue;

        auto s = ::match(instance_head, constraint);

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

vector<pair<string, Hs::Type>> typechecker_state::superclass_constraints(const Hs::Type& constraint)
{
    vector<pair<string, Hs::Type>> constraints;

    for(auto& [name, type]: gie)
    {
        // Klass a => Superklass a
        auto [class_constraints, superclass_constraint] = instantiate(type);

        // Skip if this is not a method of extracting superclass dictionaries
        if (not constraint_is_hnf(superclass_constraint)) continue;

        assert(class_constraints.size() == 1);

        auto class_constraint = class_constraints[0];
        auto s = ::match(class_constraint, constraint);

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

template <typename T>
optional<Hs::Binds> typechecker_state::entails(const T& to_keep, const pair<string, Hs::Type>& to_remove)
{
    // 1. First check if the relevant constraints are superclasses of the current constraint.
    for(auto& constraint2: to_keep)
    {
        if (auto binds = entails_by_superclass(constraint2, to_remove))
            return *binds;
    }

    // 2. Then check if there is an instance dfun :: (K1 a, K2 a) => K a
    if (auto inst = lookup_instance(to_remove.second))
    {
        auto [dfun, constraints] = *inst;

        Hs::Binds binds;
        // If we can get (dvar1 :: K1 a) and (dvar2 :: K2 a) and a dfun so that dvar = dfun dvar1 dvar2
        vector<Hs::Exp> args;
        for(auto& constraint: constraints)
        {
            Hs::Decls decls;
            auto dvar = fresh_var("dvar", false);
            args.push_back(dvar);
            if (auto cbinds = entails(to_keep, {unloc(dvar.name),constraint}))
            {
                ranges::insert(binds, binds.end(), *cbinds);
            }
            else
                return {};
        }

        Hs::Decls decls;
        Hs::Var dvar( {noloc, to_remove.first} );
        decls.push_back( Hs::simple_decl( dvar, apply_expression(dfun, args)) );
        binds.push_back( decls );

        return binds;
    }

    return {};
}

pair<Hs::Binds, local_instance_env> typechecker_state::simplify(const local_instance_env& lie)
{
    Hs::Binds binds_out;
    local_instance_env lie_out;
    auto lie_vec = lie | ranges::to<vector>;
    vector<pair<string,Hs::Type>> checked;

    for(int i=0;i<lie_vec.size();i++)
    {
        auto& pred = lie_vec[i];
        auto preds = views::concat(lie_vec | views::drop(i+1), checked);
        if (auto new_binds = entails(preds, pred))
            ranges::insert(binds_out, binds_out.end(), *new_binds);
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

    auto binds = binds1;
    for(auto& bind: binds2)
        binds.push_back(bind);

    return {binds, lie2};
}


// Why aren't we using `fixed_type_vars`?
// I guess the deferred constraints that do not mention fixed_type_vars are ambiguous?
pair<local_instance_env, local_instance_env>
classify_constraints(const local_instance_env& lie,
                     const set<Hs::TypeVar>& fixed_type_vars,
                     const set<Hs::TypeVar>& target_type_vars)
{
    local_instance_env lie_deferred;
    local_instance_env lie_retained;

    for(auto& [name, constraint]: lie)
    {
        auto constraint_type_vars = free_type_VARS(constraint);

        // Does the constraint contain any ambiguous vars?
        bool any_ambiguous = false;
        // Does the constraint reference any variables from the target set?
        bool any_referenced = false;
        for(auto& type_var: constraint_type_vars)
        {
            if (target_type_vars.count(type_var))
                any_referenced = true;
            else if (not fixed_type_vars.count(type_var))
                any_ambiguous = true;
        }

        if (any_referenced and not any_ambiguous)
            lie_retained = lie_retained.insert({name,constraint});
        else
            lie_deferred = lie_deferred.insert({name,constraint});
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

tuple<Hs::Decls, global_value_env>
typechecker_state::infer_type_for_decls(const global_value_env& env, const map<string, Hs::Type>& signatures, Hs::Decls decls)
{
    push_lie();

    // 1. Add each let-binder to the environment with a fresh type variable
    value_env binder_env;

    vector<Hs::Type> lhs_types;
    for(int i=0;i<decls.size();i++)
    {
        auto& decl = decls[i];
        if (auto fd = decl.to<Hs::FunDecl>())
        {
            auto FD = *fd;
            auto& name = unloc(FD.v.name);

            Hs::Type type = fresh_type_var();
            FD.v.type = type;
            decl = FD;

            // Check that this is a NEW name.
            local_value_env lve;
            lve = lve.insert({name,type});
            binder_env += lve;

            lhs_types.push_back(type);
        }
        else if (auto pd = decl.to<Hs::PatDecl>())
        {
            auto PD = *pd;
            auto [lhs, type, lve] = infer_pattern_type(PD.lhs);
            PD.lhs = lhs;
            decl = PD;

            binder_env += lve;

            lhs_types.push_back(type);
        }
    }
    auto env2 = plus_prefer_right(env, binder_env);

    // 2. Infer the types of each of the x[i]
    for(int i=0;i<decls.size();i++)
    {
        auto& decl = decls[i];
        auto& lhs_type = lhs_types[i];
        if (auto fd = decl.to<Hs::FunDecl>())
        {
            auto FD = *fd;
            auto [match, rhs_type] = infer_type(env2, FD.match);
            FD.match = match;
            decl = FD;

            unify(lhs_type, rhs_type);
        }
        else if (auto pd = decl.to<Hs::PatDecl>())
        {
            auto PD = *pd;
            auto [rhs, rhs_type] = infer_type(env2, PD.rhs);
            PD.rhs = rhs;
            decl = PD;

            unify(lhs_type, rhs_type);
        }
    }

    // We need to substitute before looking for free type variables!
    // We also need to substitute before we quantify below.
    binder_env = apply_current_subst(binder_env);
    // We need to substitute before we try and reduce the LIE.
    current_lie() = apply_current_subst( current_lie() );

    auto fs = free_type_variables(env);
    set<Hs::TypeVar> any_tvs;  // type variables in ANY of the definitions
    set<Hs::TypeVar> all_tvs;  // type variables in ALL of the definitions
    {
        // FIXME - should we be looping over binder vars, or over definitions?
        optional<set<Hs::TypeVar>> all_tvs_;
        for(auto& [_, type]: binder_env)
        {
            auto tvs = free_type_variables(type);
            add(any_tvs, tvs);
            if (all_tvs_)
                all_tvs_ = intersection(*all_tvs_, tvs);
            else
                all_tvs_ = tvs;
        }
        assert(all_tvs_);
        all_tvs = *all_tvs_;
    }

    set<Hs::TypeVar> qtvs = minus(any_tvs, fs);
    vector< Hs::Var > dict_vars;
    Hs::Binds binds;
    if (is_restricted(signatures, decls))
    {
        // lie == lie_deferred + lie_retained;
        qtvs = minus(qtvs, free_type_vars(current_lie()));
    }
    else
    {
        // A. First, REDUCE the lie by
        //    (i)  converting to Hnf
        //    (ii) representing some constraints in terms of others.

        auto [binds1, lie1] = reduce( current_lie() );
        current_lie() = lie1;
        binds = binds1;

        // B. Second, extract the "retained" predicates can be added without causing abiguity.
        auto [lie_deferred, lie_retained] = classify_constraints(lie1, fs, all_tvs);
        current_lie() = lie_deferred;

        auto dict_vars = vars_from_lie( lie_retained );

        binder_env = add_constraints( constraints_from_lie(lie_retained), binder_env );
    }

    // We have already substituted for types above.
    binder_env = quantify( qtvs, binder_env );

    Hs::Decls decls2 = decls;
    if (qtvs.size() or binds.size() or dict_vars.size())
    {
        decls2 = {};
        decls2.push_back( Hs::DictionaryLambda( qtvs | ranges::to<vector>, dict_vars, binds, decls ) );
    }

    pop_and_add_lie();

    return {decls2, binder_env};
}

// Figure 24. Rules for patterns
tuple<Hs::Pattern, Hs::Type, local_value_env>
typechecker_state::infer_pattern_type(const Hs::Pattern& pat)
{
    // TAUT-PAT
    if (auto v = pat.to<Hs::Var>())
    {
        auto V = *v;
        Hs::Type type = fresh_type_var();
        local_value_env lve;
        auto& name = unloc(V.name);
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
            auto [p, t1, lve1] = infer_pattern_type(pat);
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
        auto [pat, t, lve] = infer_pattern_type(ap->pattern);
        auto& name = unloc(ap->var.as_<Hs::Var>().name);
        lve = lve.insert({name, t});
        return {pat, t, lve};
    }
    // LAZY-PAT
    else if (auto lp = pat.to<Hs::LazyPattern>())
    {
        auto [p, t, lve] = infer_pattern_type(lp->pattern);
        return {Hs::LazyPattern(p), t, lve};
    }
    // not in paper (STRICT-PAT)
    else if (auto sp = pat.to<Hs::StrictPattern>())
    {
        auto [p, t, lve] = infer_pattern_type(sp->pattern);
        return {Hs::StrictPattern(p), t, lve};
    }
    // WILD-PAT
    else if (pat.is_a<Hs::WildcardPattern>())
    {
        auto tv = fresh_type_var();
        return {pat, tv, {}};
    }
    // LIST-PAT
    else if (auto l = pat.to<Hs::List>())
    {
        auto L = *l;

        local_value_env lve;
        Hs::Type t = fresh_type_var();
        for(auto& element: L.elements)
        {
            auto [p1, t1, lve1] = infer_pattern_type(element);
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
            auto [p, t1, lve1] = infer_pattern_type(element);
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
    Hs::Type type = fresh_type_var();

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
        auto env2 = env + lve1;

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
    Hs::Type result_type = fresh_type_var();

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

        auto [constraints, type] = instantiate(*sigma);

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
        // t->exp
        // t->type

        // 1. Get the most general type
        push_lie();
        auto [exp1, most_general_type] = infer_type(env, texp->exp);
        auto lie_most_general = pop_lie();

        // 2. alpha[i] in most_general_type but not in env
        auto ftv_mgt = free_type_variables(most_general_type);
        auto ftv_env = free_type_variables(env);
        for(auto tv: ftv_env)
            ftv_mgt.erase(tv);
        // 3. instantiate the given type...
        // 4. ... with fresh variables gamma[i].
        auto [given_constraints, given_type] = instantiate(texp->type);
        // 5. Find a way to constrain the most general type to the constrained type.

        auto s2 = ::match(most_general_type, given_type);
        if (not s2 or not add_substitution(*s2))
            throw myexception()<<"Type '"<<texp->type<<"' does not match type '"<<most_general_type<<"' of expression '"<<texp->exp<<"'";

        for(auto& [a,type]: *s2)
            if (not ftv_mgt.count(a))
                throw myexception()<<"Type '"<<texp->type<<"' does not match type '"<<most_general_type<<"' of expression '"<<texp->exp<<"' because it tries to constraint type variable '"<<a.print()<<"'";
        // 6. Convert the given_constraints into a LIE

        lie_most_general = apply_current_subst(lie_most_general);

        auto lie_given = constraints_to_lie(given_constraints);
        // 7. Express lie2 in terms of lie1
        auto binds = get_dicts(lie_given, lie_most_general);
        if (not binds)
            throw myexception()<<"Can't derive constraints '"<<print(lie_most_general)<<"' from specified constraints '"<<print(lie_given)<<"'";

        current_lie() += lie_given;
        return {Hs::LetExp({noloc,*binds}, {noloc,exp1}), given_type};
    }
    else if (auto l = E.to<Hs::List>())
    {
        Hs::Type element_type = fresh_type_var();
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
            auto tv = fresh_type_var();

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
        auto [binds, env_decls] = infer_type_for_binds(env, unloc(Let.binds));
        unloc(Let.binds) = binds;
        auto env2 = env_decls +  env;

        // 2. Compute type of let body
        auto [body, t_body] = infer_type(env2, unloc(Let.body));
        unloc(Let.body) = body;

        // return (s1 `compose` s2, t2)
        return {Let, t_body};
    }
    else if (auto con = E.head().to<Hs::Con>())
    {
        auto [type, field_types] = constr_types(*con);

        auto env2 = env;
        vector<Hs::Type> arg_types;
        vector<Hs::Exp> args = E.copy_sub();
        for(int i=0; i < args.size(); i++)
        {
            auto& arg = args[i];
            auto [arg_i, t_i] = infer_type(env2, arg);
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

        Hs::Type result_type = fresh_type_var();

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

tuple<global_value_env, global_instance_env, class_env, Hs::Binds> typechecker_state::infer_type_for_classes(const Hs::Decls& decls, const type_con_env& tce)
{
    global_value_env gve;
    global_instance_env gie;
    class_env ce;
    Hs::Binds binds;

    for(auto& decl: decls)
    {
        auto c = decl.to<Hs::ClassDecl>();
        if (not c) continue;

        auto [gve1, gie1, class_info, class_decls] = infer_type_for_class(tce, *c);

        gve += gve1;
        gie += gie1;
        ce.insert({class_info.name, class_info});
        binds.push_back(class_decls);
    }

    return {gve, gie, ce, binds};
}

Hs::Type type_check_class_method_type(kindchecker_state& K, Hs::Type type, const Hs::Type& constraint)
{
    // 1. Bind type parameters for type declaration
    K.push_type_var_scope();

    std::optional<Hs::Context> context;

    // 2. Find the unconstrained type
    auto unconstrained_type = type;
    if (unconstrained_type.is_a<Hs::ConstrainedType>())
    {
        auto& ct = unconstrained_type.as_<Hs::ConstrainedType>();
        context = ct.context;
        unconstrained_type = ct.type;
    }

    // 3. Find the NEW free type variables
    auto new_ftvs = free_type_VARS(unconstrained_type);
    vector<Hs::TypeVar> to_erase;
    for(auto& type_var: new_ftvs)
        if (K.type_var_in_scope(type_var))
            to_erase.push_back(type_var);
    for(auto& type_var: to_erase)
        new_ftvs.erase(type_var);

    // 4. Bind fresh kind vars to new type variables
    for(auto& ftv: new_ftvs)
    {
        auto a = K.fresh_kind_var();
        K.bind_type_var(ftv,a);
    }

    // 5. Check the context
    if (context)
        K.kind_check_context(*context);

    // 6. Check the unconstrained type and infer kinds.
    K.kind_check_type_of_kind(unconstrained_type, make_kind_star());

    // 7. Bind fresh kind vars to new type variables
    vector<Hs::TypeVar> new_type_vars;
    for(auto& type_var: new_ftvs)
    {
        auto type_var_with_kind = type_var;
        type_var_with_kind.kind = replace_kvar_with_star( K.kind_for_type_var(type_var) );
        new_type_vars.push_back( type_var_with_kind );
    }

    // Don't we need to simplify constraints if we do this?
    type = add_constraints({constraint}, type);

    type = add_forall_vars(new_type_vars, type);
    
    // 6. Unbind type parameters
    K.pop_type_var_scope();

    return type;
}

// OK, so
// * global_value_env    = name         :: forall a: class var => signature (i.e. a-> b -> a)
// * global_instance_env = made-up-name :: forall a: class var => superclass var
// * Hs::Decls           = { name         = \dict -> case dict of (_,_,method,_,_) -> method }
//                       = { made-up-name = \dict -> case dict of (superdict,_,_,_,_) -> superdict }

tuple<global_value_env,global_instance_env,class_info,Hs::Decls>
typechecker_state::infer_type_for_class(const type_con_env& tce, const Hs::ClassDecl& class_decl)
{
    kindchecker_state K(tce);

    auto& name = class_decl.name;

    class_info cinfo;
    cinfo.type_vars = class_decl.type_vars;
    cinfo.name = name;
    cinfo.emitted_name = "class$"+name; // FIXME: only modify name after qualifier?  Just prefix d?
    cinfo.context = class_decl.context;

    // Bind type parameters for class
    K. push_type_var_scope();

    // a. Look up kind for this data type.
    kind k = K. kind_for_type_con(name);  // FIXME -- check that this is a class?

    // b. Put each type variable into the kind.
    vector<Hs::TypeVar> class_typevars;
    for(auto& tv: class_decl.type_vars)
    {
        // the kind should be an arrow kind.
        assert(k->is_karrow());
        auto& ka = dynamic_cast<const KindArrow&>(*k);

        // map the name to its kind
        K.bind_type_var(tv, ka.k1);

        // record a version of the var with that contains its kind
        auto tv2 = tv;
        tv2.kind = ka.k1;
        class_typevars.push_back(tv2);

        // set up the next iteration
        k = ka.k2;
    }
    assert(k->is_kconstraint());

    // d. construct the constraint
    Hs::Type constraint = Hs::TypeCon(Unlocated(class_decl.name));
    for(auto& tv: class_typevars)
        constraint = Hs::TypeApp(constraint, tv);

    // e. handle the class methods
    global_value_env gve;
    if (class_decl.binds)
    {
        for(auto& [name, type]: unloc(*class_decl.binds).signatures)
        {
            Hs::Type method_type = type_check_class_method_type(K, type, constraint);

            method_type = apply_current_subst(method_type);
            method_type = add_forall_vars(class_typevars, method_type);

            gve = gve.insert({name, method_type});
        }
    }

    K.pop_type_var_scope();

    // OK, so now we need to
    //   (a) determine names for dictionary extractors.
    //   (b) determine an order for all the fields.
    //   (c) synthesize field accessors and put them in decls

    global_instance_env gie;
    for(auto& constraint_: cinfo.context.constraints)
    {
        auto get_dict = fresh_var("get_dict", true);
        // Should this be a function arrow?
        Hs::Type type = add_constraints({constraint}, constraint_);
        // Could we be adding too many forall vars?
        type = apply_current_subst(type);
        type = add_forall_vars(class_typevars, type);
        gie = gie.insert({unloc(get_dict.name), type});
    }
    cinfo.fields = gve + gie;

    Hs::Decls decls;

    vector<Hs::Type> types;
    for(auto& [name,type]: cinfo.fields)
        types.push_back(type);
    Hs::Type dict_type = Hs::TupleType(types);

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
        Hs::Alt alt{Hs::Tuple(pats),Hs::SimpleRHS({noloc,field})};

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

Hs::Type extract_class_constraint(Hs::Type type)
{
    // This should only be called on LIEs
    assert(not type.is_a<Hs::ForallType>());

    if (auto c = type.to<Hs::ConstrainedType>())
        return c->type;
    else
        return type;
}

local_instance_env typechecker_state::constraints_to_lie(const vector<Hs::Type>& constraints)
{
    local_instance_env lie;
    for(auto& constraint: constraints)
    {
        auto dvar = fresh_var("dvar", false);
        lie = lie.insert({unloc(dvar.name), constraint});
    }
    return lie;
}

// How does this relate to simplifying constraints?
optional<Hs::Binds> typechecker_state::get_dicts(const local_instance_env& lie1, const local_instance_env& lie2)
{
    Hs::Binds binds;
    for(auto& constraint: lie2)
    {
        auto binds1 = entails(lie1, constraint);
        if (not binds1)
            return {};
        ranges::insert(binds, binds.end(), *binds1);
    }
    return binds;
}

global_instance_env
typechecker_state::infer_type_for_instance1(const Hs::InstanceDecl& inst_decl, const class_env& ce)
{
    auto [class_head, monotypes] = decompose_type_apps(inst_decl.constraint);

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
    // Premise #4: the monotype must be a type constructor applied to simple, distinct type variables.
    vector<Hs::TypeCon> types;
    for(auto& monotype: monotypes)
    {
        auto [a_head, a_args] = decompose_type_apps(monotype);
        auto tc = a_head.to<Hs::TypeCon>();
        if (not tc)
            throw myexception()<<"In instance for '"<<inst_decl.constraint<<"': "<<a_head<<" is not a type constructor!";

        types.push_back(*tc);

        // Add distinct type variables
        for(auto& a_arg: a_args)
        {
            auto tv = a_arg.to<Hs::TypeVar>();

            if (not tv)
                throw myexception()<<"In instance for '"<<inst_decl.constraint<<"' for type '"<<monotype<<"': "<<a_arg<<" is not a type variable!";

            if (type_vars.count(*tv))
                throw myexception()<<"Type variable '"<<tv->print()<<"' occurs twice in constraint '"<<inst_decl.constraint<<"'";

            type_vars.insert(*tv);
        }
    }

    // Premise 5: Check that the context contains no variables not mentioned in `monotype`
    for(auto& tv: free_type_VARS(inst_decl.context))
    {
        if (not type_vars.count(tv))
            throw myexception()<<"Constraint context '"<<inst_decl.context.print()<<"' contains type variable '"<<tv.print()<<"' that is not mentioned in '"<<inst_decl.constraint<<"'";
    }

    auto dfun = fresh_var("dfun", true);
    Hs::Type inst_type = add_constraints(inst_decl.context.constraints, inst_decl.constraint);
    inst_type = apply_current_subst(inst_type);
    inst_type = add_forall_vars( free_type_VARS(inst_type) | ranges::to<vector>, inst_type);
    global_instance_env gie_out;
    gie_out = gie_out.insert( { unloc(dfun.name), inst_type } );
    return gie_out;
}

// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
global_instance_env
typechecker_state::infer_type_for_instances1(const Hs::Decls& decls, const class_env& ce)
{
    global_instance_env gie_inst;
    for(auto& decl: decls)
    {
        if (auto I = decl.to<Hs::InstanceDecl>())
        {
            auto gie1 = infer_type_for_instance1(*I, ce);

            gie_inst += gie1;
        }
    }
    return gie_inst;
}

expression_ref tuple_from_value_env(const value_env& venv)
{
    vector<expression_ref> elements;
    for(auto& [name,type]: venv)
        elements.push_back( Hs::Var({noloc, name}) );

    return Hs::Tuple(elements);
}

Hs::Decls
typechecker_state::infer_type_for_instance2(const Hs::InstanceDecl& inst_decl, const class_env& ce)
{
    auto [class_head, monotypes] = decompose_type_apps(inst_decl.constraint);

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
    // Premise #4: the monotype must be a type constructor applied to simple, distinct type variables.
    vector<Hs::TypeCon> types;
    for(auto& monotype: monotypes)
    {
        auto [a_head, a_args] = decompose_type_apps(monotype);
        auto tc = a_head.to<Hs::TypeCon>();
        if (not tc)
            throw myexception()<<"In instance for '"<<inst_decl.constraint<<"': "<<a_head<<" is not a type!";

        types.push_back(*tc);

        // Add distinct type variables
        for(auto& a_arg: a_args)
        {
            auto tv = a_arg.to<Hs::TypeVar>();

            if (not tv)
                throw myexception()<<"In instance for '"<<inst_decl.constraint<<"' for type '"<<monotype<<"': "<<a_arg<<" is not a type variable!";

            if (type_vars.count(*tv))
                throw myexception()<<"Type variable '"<<tv->print()<<"' occurs twice in constraint '"<<inst_decl.constraint<<"'";

            type_vars.insert(*tv);
        }
    }

    // Premise 5: Check that the context contains no variables not mentioned in `monotype`
    for(auto& tv: free_type_VARS(inst_decl.context))
    {
        if (not type_vars.count(tv))
            throw myexception()<<"Constraint context '"<<inst_decl.context.print()<<"' contains type variable '"<<tv.print()<<"' that is not mentioned in '"<<inst_decl.constraint<<"'";
    }

    // Premise 6: build a local instance environment from the context
    local_instance_env lie;
    int i=0;
    for(auto& constraint: inst_decl.context.constraints)
    {
        string dict_name = "dict"+std::to_string(i+1);
        Hs::Var dict({noloc, dict_name});
        lie = lie.insert({dict_name,constraint});
    }

    // Premise 7:
    local_instance_env lie_super;
    auto binds_super = get_dicts(lie, lie_super);
    if (not binds_super)
        throw myexception()<<"Missing instances!";

    // Premise 8:
    Hs::Binds binds_methods;
    auto dfun = fresh_var("dfun", true);

    // dfun = /\a1..an -> \dicts:theta -> let binds_super in let_binds_methods in <superdicts,methods>
    expression_ref dict1 = tuple_from_value_env(cinfo->fields);
    expression_ref dict2 = tuple_from_value_env(lie);

    expression_ref E = Hs::LetExp( {noloc,binds_methods}, {noloc, dict1} );
    E = Hs::LetExp( {noloc,*binds_super}, {noloc,E} );
    E = Hs::LambdaExp({dict2}, E);

    Hs::Decls decls ({ simple_decl(dfun,E) });
    return decls;
}

// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
Hs::Decls
typechecker_state::infer_type_for_instances2(const Hs::Decls& decls, const class_env& ce)
{
    Hs::Decls out_decls;
    global_instance_env gie_inst;
    for(auto& decl: decls)
    {
        if (auto I = decl.to<Hs::InstanceDecl>())
        {
            auto decls_ = infer_type_for_instance2(*I, ce);

            for(auto& d: decls_)
                out_decls.push_back(d);
        }
    }
    return out_decls;
}

Hs::ModuleDecls typecheck( const string& mod_name, const Module& m, Hs::ModuleDecls M )
{
    // 1. Check the module's type declarations, and derives a Type Environment TE_T:(TCE_T, CVE_T)
    //    OK, so datatypes produce a
    //    * Type Constructor Environment (TCE) = tycon -> (kind, arity, method of applying the tycon?)
    //    * Constructor Value Environment (CVE)
    //
    // 2. Check the module's class declarations, produce some translated bindings -> binds_C ( GVE_C, CE_C, GIE_C )

    // TCE_T = type con info, part1
    auto tce = get_tycon_info( M.type_decls );
    for(auto& [tycon,ka]: tce)
    {
        auto& [k,arity] = ka;
        std::cerr<<tycon<<" :: "<<k->print()<<"\n";
    }
    std::cerr<<"\n";

    // CVE_T = constructor types :: map<string, polytype> = global_value_env
    auto constr_info = get_constructor_info(M.type_decls, tce);

    for(auto& [con,type]: constr_info)
    {
        std::cerr<<con<<" :: "<<type.print()<<"\n";
    }
    std::cerr<<"\n";

    //   CE_C  = class name -> class info
    typechecker_state state( mod_name, m, constr_info );
    auto [gve, class_gie, class_info, class_binds] = state.infer_type_for_classes(M.type_decls, tce);
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
    auto inst_gie = state.infer_type_for_instances1(M.type_decls, class_info);

    state.gie += inst_gie;

    for(auto& [method,type]: inst_gie)
    {
        std::cerr<<method<<" :: "<<type.print()<<"\n";
    }
    std::cerr<<"\n";

    // 3. E' = (TCE_T, (CVE_T, GVE_C, LVE={}), CE_C, (GIE_C, LIE={}))

    auto [value_decls, env] = state.infer_type_for_binds(gve, M.value_decls);
    M.value_decls = value_decls;

    for(auto& [x,t]: env)
    {
        std::cerr<<x<<" :: "<<remove_top_level_foralls(alphabetize_type(t))<<"\n";
//        std::cerr<<x<<" = "<<e<<"\n\n\n";
    }
    std::cerr<<"\n";

    std::cerr<<M.value_decls.print();
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


