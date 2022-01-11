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

  TODO:
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
  9. Monomorphism restriction.
  10. Defaulting.
  11. Emit code for instances and check if there are instances for the context.

  Cleanups:
  1. Implement kinds as Hs::Type

  Unification:
  0. Can we avoid explicitly returning a substitution?
  1. If I somehow defer "zonking" to the end, can I avoid manually applying all these
     substitutions?
  2. Unification SHOULD be accumulating a set of constraints that can be solved later.
     But right now it seems that by failing to substitute into a term before unifying
     that term with another term, we can lose constraints.
     How can we avoid this?
  3. Can we get better error messages by listing an expected type?
  4. Can we prefix with "heralds" for better error messages?

  5. I'd like to replace substitutions with constraints.
     Hmmm... perhaps the difference is that COMPOSITION of substitutions never fails,
     but COMBINING constraints can fail?

     Example: a ~ Int , a ~ b
     * for substitutions, later ones have no effect
     * for constraints, I guess you do a ~ Int, b ~ a, changin a ~ b into b ~ a in order to
       "define" each variable only once.
     * if you later add b ~ Int, then you need to unify b ~ Int and b ~ a,
       which then unifies Int ~ a, which succeeds.
     * So, perhaps a second definition of a variable must either fail or succeed -- it doesn't
       get added.
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

    typechecker_state(const string& s, const constr_env& ce)
        :con_info(ce),
         mod_name(s)
        { }

    Hs::Type bool_type() const { return Hs::TypeCon({noloc,"Data.Bool.True"}); }

    Hs::Type char_type() const { return Hs::TypeCon({noloc,"Char"}); }

    Hs::Type enum_class(const Hs::Type& arg) const
    {
        return Hs::TypeApp(Hs::TypeCon({noloc,"Enum"}),arg);
    }

    Hs::Type num_class(const Hs::Type& arg) const
    {
        return Hs::TypeApp(Hs::TypeCon({noloc,"Num"}),arg);
    }

    Hs::Type fractional_class(const Hs::Type& arg) const
    {
        return Hs::TypeApp(Hs::TypeCon({noloc,"Fractional"}),arg);
    }

    pair<local_instance_env, Hs::Type> fresh_enum_type()
    {
        local_instance_env lie;
        Hs::Type a = fresh_type_var();
        Hs::Type num_a = enum_class(a);
        auto dvar = fresh_var("dvar", false);
        lie = lie.insert({unloc(dvar.name), num_a});
        return {lie, a};
    }

    pair<local_instance_env, Hs::Type> fresh_num_type()
    {
        local_instance_env lie;
        Hs::Type a = fresh_type_var();
        Hs::Type num_a = num_class(a);
        auto dvar = fresh_var("dvar", false);
        lie = lie.insert({unloc(dvar.name), num_a});
        return {lie, a};
    }

    pair<local_instance_env, Hs::Type> fresh_fractional_type()
    {
        local_instance_env lie;
        Hs::Type a = fresh_type_var();
        Hs::Type fractional_a = fractional_class(a);
        auto dvar = fresh_var("dvar", false);
        lie = lie.insert({unloc(dvar.name), fractional_a});
        return {lie, a};
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
    tuple<substitution_t, vector<Hs::Qual>, local_instance_env, local_value_env>
    infer_quals_type(const global_value_env& env, vector<Hs::Qual> quals);

    // Figure 22.
    tuple<substitution_t, Hs::Qual, local_instance_env, local_value_env>
    infer_qual_type(const global_value_env& env, const Hs::Qual& qual);

    tuple<substitution_t, Hs::Qual, local_instance_env, local_value_env>
    infer_guard_type(const global_value_env& env, const Hs::Qual& guard);

    // Figure 24.
    tuple<Hs::Pattern, Hs::Type, local_instance_env, local_value_env>
    infer_pattern_type(const Hs::Pattern& pat);

    tuple<substitution_t, expression_ref, local_instance_env, Hs::Type>
    infer_type(const global_value_env& env, expression_ref exp);

    tuple<substitution_t, Hs::GuardedRHS, local_instance_env, Hs::Type>
    infer_type(const global_value_env& env, Hs::GuardedRHS);

    tuple<substitution_t, Hs::MultiGuardedRHS, local_instance_env, Hs::Type>
    infer_type(const global_value_env& env, Hs::MultiGuardedRHS);

    tuple<substitution_t, Hs::MRule, local_instance_env, Hs::Type>
    infer_type(const global_value_env& env, Hs::MRule);

    tuple<substitution_t, Hs::Match, local_instance_env, Hs::Type>
    infer_type(const global_value_env& env, Hs::Match);

    // Figures 13, 14, 15?
    tuple<substitution_t, Hs::Decls, local_instance_env, global_value_env>
    infer_type_for_decls(const global_value_env& env, Hs::Decls E);

    // Figures 13, 14, 15?
    tuple<substitution_t, Hs::Binds, local_instance_env, global_value_env>
    infer_type_for_decls(const global_value_env& env, Hs::Binds binds);

    tuple<global_value_env, global_instance_env, class_env, Hs::Binds>
    infer_type_for_classes(const Hs::Decls& decls, const type_con_env& tce);

    tuple<global_value_env,global_instance_env,class_info,Hs::Decls>
    infer_type_for_class(const type_con_env& tce, const Hs::ClassDecl& class_decl);

    // Figure 12
    global_instance_env
    infer_type_for_instances1(const Hs::Decls& decls, const class_env& ce, const global_instance_env& gie_in);

    // Figure 12
    global_instance_env
    infer_type_for_instance1(const Hs::InstanceDecl& instance_del, const class_env& ce, const global_instance_env& gie_in);

    // Figure 12
    Hs::Decls
    infer_type_for_instances2(const Hs::Decls& decls, const class_env& ce, const global_instance_env& gie_in);

    // Figure 12
    Hs::Decls
    infer_type_for_instance2(const Hs::InstanceDecl& instance_del, const class_env& ce, const global_instance_env& gie_in);

    // Figure 26
    // Express lie2 in terms of gie (functions) and lie1 (arguments to this dfun, I think).
    Hs::Binds get_dicts(const global_instance_env& gie, const local_instance_env& lie1, const local_instance_env& lie2);

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

std::set<Hs::TypeVar> free_type_variables(const global_value_env& env)
{
    std::set<Hs::TypeVar> free;
    for(auto& [x,type]: env)
        add(free, free_type_variables(type));
    return free;
}

expression_ref generalize(const global_value_env& env, const expression_ref& monotype)
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

tuple<substitution_t, Hs::Binds, local_instance_env, global_value_env>
typechecker_state::infer_type_for_decls(const global_value_env& env, Hs::Binds binds)
{
    substitution_t s;
    auto env2 = env;
    local_instance_env lie;
    global_value_env binders;
    for(auto& decls: binds)
    {
        auto [s1, decls1, lie1, binders1] = infer_type_for_decls(env2, decls);
        decls = decls1;
        lie += lie1;
        env2 = plus_prefer_right(env2, binders1);
        binders += binders1;
        s = compose(s1, s);
    }
    binders = apply_subst(s, binders);
    lie = apply_subst(s, lie);
    return {s, binds, lie, binders};
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

        auto s = match(instance_head, constraint);

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

        Hs::Decls decls;
        decls.push_back(Hs::simple_decl(dfun, rhs));

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
        auto s = match(class_constraint, constraint);

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
        bool keep = false;
        for(auto& target_var: target_type_vars)
            if (constraint_type_vars.count(target_var))
                keep = true;

        if (keep)
            lie_retained = lie_retained.insert({name,constraint});
        else
            lie_deferred = lie_deferred.insert({name,constraint});
    }

    return {lie_deferred, lie_retained};
}

tuple<substitution_t, Hs::Decls, local_instance_env, global_value_env>
typechecker_state::infer_type_for_decls(const global_value_env& env, Hs::Decls decls)
{
    // 1. Add each let-binder to the environment with a fresh type variable
    local_instance_env lie;
    value_env binder_env;

    vector<pair<Hs::Type,global_value_env>> decl_types;
    for(int i=0;i<decls.size();i++)
    {
        auto& decl = decls[i];
        if (auto fd = decl.to<Hs::FunDecl>())
        {
            Hs::Type type = fresh_type_var();
            local_value_env lve;
            auto& name = unloc(fd->v.name);
            lve = lve.insert({name,type});
            decl_types.push_back({type, lve});
        }
        else if (auto pd = decl.to<Hs::PatDecl>())
        {
            auto [p, type, lie_p, lve] = infer_pattern_type(pd->lhs);
            decl_types.push_back({type,lve});
            lie += lie_p;
        }
        auto& [type, lve] = decl_types.back();
        binder_env += lve;
    }
    auto env2 = plus_prefer_right(env, binder_env);

    // 2. Infer the types of each of the x[i]
    substitution_t s;
    for(int i=0;i<decls.size();i++)
    {
        auto& decl = decls[i];
        if (auto fd = decl.to<Hs::FunDecl>())
        {
            auto FD = *fd;
            auto& name = unloc(FD.v.name);
            auto lhs_type = env2.at(name);
            auto [s2, match, rhs_lie, rhs_type] = infer_type(env2, FD.match);
            FD.match = match;
            decl = FD;
            
            lie += rhs_lie;
            s = compose(s2, compose(unify(lhs_type, rhs_type), s));
        }
        else if (auto pd = decl.to<Hs::PatDecl>())
        {
            auto PD = *pd;
            auto [lhs, lhs_type, lhs_lie, lve] = infer_pattern_type(PD.lhs);
            auto [s2, rhs, rhs_lie, rhs_type] = infer_type(env2, PD.rhs);
            PD.lhs = lhs;
            PD.rhs = rhs;
            decl = PD;
            
            lie += lhs_lie;
            lie += rhs_lie;
            s = compose(s2, compose(unify(lhs_type, rhs_type), s));
        }

        binder_env = apply_subst(s, binder_env);
        env2 = apply_subst(s, env2);
        lie = apply_subst(s, lie);
    }
    
    // FIXME: Deal with constraints in the LIE here...
    auto fs = free_type_variables(env);
    auto gs = free_type_variables(binder_env);
    // fs = free_type_variables(env);
    // gs = free_type_variables(type) - fs;
    // ??? = information to reduce LIEs and also handle defaults.

    // A. First, REDUCE the lie
    auto [binds1, lie1] = reduce(lie);

    // % (decls1,lie') = reduce ??? lie
    // B. Second, remove the "deferred" predicates,
    auto [lie_deferred, lie_retained] = classify_constraints(lie1, fs, gs);
    //     where all the variables are fixed / constrained => they are a subset of `fs`.
    // % (lie_deferred, lie_retained) = partition lie'
    // C. Find retained predicates that are defaulted.
    //    (decls2, lie_retained') = resolveDefaultedPreds ??? (fs++gs) lie_retained
    //    Example: show (read x + 1)

    vector<Hs::Type> constraints;
    for(auto& [name, constraint]: lie_retained)
        constraints.push_back(constraint);

    value_env predicated_binder_env;
    for(auto& [var,type]: binder_env)
    {
        auto predicated_type = add_constraints(constraints,type);
        predicated_binder_env = predicated_binder_env.insert({var, predicated_type});
    }

    // 3. Generalize each type over variables not in the *original* environment
    value_env generalized_binder_env;
    for(auto& [var,type]: predicated_binder_env)
        generalized_binder_env = generalized_binder_env.insert({var,generalize(env, type)});

    return {s, decls, lie_deferred, generalized_binder_env};
}

// Figure 24. Rules for patterns
tuple<Hs::Pattern, Hs::Type, local_instance_env, local_value_env>
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
	return { V, type , local_instance_env(), lve };
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
            auto [p, t1, lie1, lve1] = infer_pattern_type(pat);
            pat = p;
            types.push_back(t1);
            lve += lve1;
            lie += lie1;
        }
        substitution_t s;
        auto [type,field_types] = constr_types(*con);

        assert(field_types.size() == pat.size());

        // Unify constructor field types with discovered types.
        for(int i=0;i<types.size();i++)
        {
            auto s1 = unify(types[i], field_types[i]);
            s = compose(s1, s);
        }

        lie = apply_subst(s, lie);
        lve = apply_subst(s, lve);
        type = apply_subst(s, type);

        return { pat, type, lie, lve };
    }
    // AS-PAT
    else if (auto ap = pat.to<Hs::AsPattern>())
    {
        auto [pat, t, lie, lve] = infer_pattern_type(ap->pattern);
        auto& name = unloc(ap->var.as_<Hs::Var>().name);
        lve = lve.insert({name, t});
        return {pat, t, lie, lve};
    }
    // LAZY-PAT
    else if (auto lp = pat.to<Hs::LazyPattern>())
    {
        auto [p, t, lie, lve] = infer_pattern_type(lp->pattern);
        return {Hs::LazyPattern(p), t, lie, lve};
    }
    // not in paper (STRICT-PAT)
    else if (auto sp = pat.to<Hs::StrictPattern>())
    {
        auto [p, t, lie, lve] = infer_pattern_type(sp->pattern);
        return {Hs::StrictPattern(p), t, lie, lve};
    }
    // WILD-PAT
    else if (pat.is_a<Hs::WildcardPattern>())
    {
        auto tv = fresh_type_var();
        return {pat, tv, local_instance_env(), {}};
    }
    // LIST-PAT
    else if (auto l = pat.to<Hs::List>())
    {
        auto L = *l;

        local_instance_env lie;
        local_value_env lve;
        Hs::Type t = fresh_type_var();
        substitution_t s;
        for(auto& element: L.elements)
        {
            auto [p1, t1, lie1, lve1] = infer_pattern_type(element);
            element = p1;

            auto s1 = unify(t, t1);
            s = compose(s1,s);
            lve += lve1;
            lie += lie1;
        }

        lie = apply_subst(s, lie);
        lve = apply_subst(s, lve);
        t = apply_subst(s, t);

        return {L, t, lie, lve};
    }
    // TUPLE-PAT
    else if (auto t = pat.to<Hs::Tuple>())
    {
        auto T = *t;
        vector<Hs::Type> types;
        local_value_env lve;
        local_instance_env lie;
        for(auto& element: T.elements)
        {
            auto [p, t1, lie1, lve1] = infer_pattern_type(element);
            element = p;
            types.push_back(t1);
            lve += lve1;
            lie += lie1;
        }
        return {T, Hs::TupleType(types), lie, lve};
    }
    // ???
    else if (pat.is_int())
    {
        auto [lie, type] = fresh_num_type();
        return {pat, type, lie, {}};
    }
    else if (pat.is_double())
    {
        auto [lie, type] = fresh_fractional_type();
        return {pat, type, lie, {}};
    }
    else if (pat.is_char())
    {
        return {pat, char_type(), local_instance_env(), {}};
    }
    else if (false) // Literal string
    {
        return {pat, Hs::ListType(char_type()), local_instance_env(), {}};
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

tuple<substitution_t, vector<Hs::Qual>, local_instance_env, value_env>
typechecker_state::infer_quals_type(const global_value_env& env, vector<Hs::Qual> quals)
{
    substitution_t s;
    auto env2 = env;
    local_instance_env lie;
    local_value_env binders;
    for(auto& qual: quals)
    {
        auto [qual_s, qual_exp, qual_lie, qual_binders] = infer_qual_type(env2, qual);

        qual = qual_exp;
        lie += qual_lie;
        s = compose(qual_s, s);
        env2 = plus_prefer_right(env2, qual_binders);
        binders = plus_prefer_right(binders, qual_binders);
    }
    binders = apply_subst(s, binders);
    return {s, quals, lie, binders};
}

tuple<substitution_t, Hs::Qual, local_instance_env, value_env>
typechecker_state::infer_qual_type(const global_value_env& env, const Hs::Qual& qual)
{
    // FILTER
    if (auto sq = qual.to<Hs::SimpleQual>())
    {
        auto SQ = *sq;
        auto [cond_s, exp, cond_lie, cond_type] = infer_type(env, SQ.exp);
        SQ.exp = exp;
        auto s2 = unify( cond_type, bool_type() );
        auto s = compose(s2, cond_s);
        return {s, SQ, cond_lie, {}};
    }
    // GENERATOR.
    else if (auto pq = qual.to<Hs::PatQual>())
    {
        auto PQ = *pq;
        // pat <- exp
        auto [bindpat, pat_type, pat_lie, lve] = infer_pattern_type(PQ.bindpat);
        auto [exp_s, exp, exp_lie, exp_type] = infer_type(env, PQ.exp);

        PQ.bindpat = bindpat;
        PQ.exp = exp;

        // type(pat) = type(exp)
        auto s3 = unify(Hs::ListType(pat_type), exp_type);
        auto s = compose(s3, exp_s);
        local_instance_env lie = pat_lie + exp_lie;

        lie = apply_subst(s, lie);
        lve = apply_subst(s, lve);

        return {s, PQ, lie, lve};
    }
    else if (auto lq = qual.to<Hs::LetQual>())
    {
        auto LQ = *lq;
        auto [s, binds, lie, t] = infer_type_for_decls(env, unloc(LQ.binds));
        unloc(LQ.binds) = binds;
        return {s, LQ, lie, t};
    }
    else
        std::abort();
}


tuple<substitution_t, Hs::Qual, local_instance_env, value_env>
typechecker_state::infer_guard_type(const global_value_env& env, const Hs::Qual& guard)
{
    if (auto sq = guard.to<Hs::SimpleQual>())
    {
        auto SQ = *sq;
        auto [cond_s, cond_exp, cond_lie, cond_type] = infer_type(env, SQ.exp);
        SQ.exp = cond_exp;
        auto s2 = unify( cond_type, bool_type() );
        auto s = compose(s2, cond_s);
        return {s, SQ, cond_lie, {}};
    }
    else if (auto pq = guard.to<Hs::PatQual>())
    {
        auto PQ = *pq;
        // pat <- exp
        auto [bindpat, pat_type, pat_lie, lve] = infer_pattern_type(PQ.bindpat);
        auto [exp_s, exp, exp_lie, exp_type] = infer_type(env, PQ.exp);

        PQ.bindpat = bindpat;
        PQ.exp = exp;
        
        // type(pat) = type(exp)
        auto s3 = unify(pat_type,exp_type);
        auto s = compose(s3, exp_s);
        local_instance_env lie = pat_lie + exp_lie;

        lve = apply_subst(s, lve);
        lie = apply_subst(s, lie);

        return {s, PQ, exp_lie, lve};
    }
    else if (auto lq = guard.to<Hs::LetQual>())
    {
        auto LQ = *lq;
        auto [s, binds, lie, t] = infer_type_for_decls(env, unloc(LQ.binds));
        unloc(LQ.binds) = binds;
        return {s, LQ, lie, t};
    }
    else
        std::abort();
}


// Figure 25. Rules for match, mrule, and grhs
tuple<substitution_t, Hs::GuardedRHS, local_instance_env, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, Hs::GuardedRHS rhs)
{
    // Fig 25. GUARD-DEFAULT
    if (rhs.guards.empty())
    {
        auto [s, body, lie, type] = infer_type(env, rhs.body);
        rhs.body = body;
        return {s, rhs, lie, type};
    }

    // Fig 25. GUARD
    auto guard = rhs.guards[0];
    auto [s1, guard1, lie1, env1] = infer_guard_type(env, guard);
    auto env2 = plus_prefer_right(env, env1);

    rhs.guards.erase(rhs.guards.begin());
    auto [s2, rhs2, lie2, t2] = infer_type(env2, rhs);
    
    rhs2.guards.insert(rhs2.guards.begin(), guard1);
    auto lie = lie1 + lie2;

    auto s = compose(s2, s1);

    Hs::Type type = apply_subst(s, t2);
    lie = apply_subst(s, lie);
    return {s, rhs2, lie, type};
}

// Fig 25. GUARD-OR
tuple<substitution_t, Hs::MultiGuardedRHS, local_instance_env, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, Hs::MultiGuardedRHS rhs)
{
    substitution_t s;
    local_instance_env lie;
    Hs::Type type = fresh_type_var();

    auto env2 = env;
    if (rhs.decls)
    {
        auto [s1, decls1, lie1, binders] = infer_type_for_decls(env, unloc(*rhs.decls));
        unloc(*rhs.decls) = decls1;
        lie += lie1;
        env2 = plus_prefer_right(env, binders);
        s = compose(s1, s);
    }

    for(auto& guarded_rhs: rhs.guarded_rhss)
    {
        auto [s1, guarded_rhs2, lie1, t1] = infer_type(env2, guarded_rhs);
        guarded_rhs = guarded_rhs2;
        auto s2 = unify(t1,type);
        lie += lie1;
        s = compose(s2,compose(s1,s));
    }
    lie = apply_subst(s, lie);
    type = apply_subst(s, type);
    return {s, rhs, lie, type};
};

tuple<substitution_t, Hs::MRule, local_instance_env, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, Hs::MRule rule)
{

    if (rule.patterns.empty())
    {
        auto [s, rhs, lie, type] = infer_type(env, rule.rhs);
        rule.rhs = rhs;
        return {s, rule, lie, type};
    }
    else
    {
        auto [pat, t1, lie1, lve1] = infer_pattern_type(rule.patterns.front());
        auto env2 = plus_no_overlap(env, lve1);

        // Remove the first pattern in the rule
        rule.patterns.erase(rule.patterns.begin());

        auto [s, rule2, lie2, t2] = infer_type(env2, rule);

        rule2.patterns.insert(rule2.patterns.begin(), pat);

        Hs::Type type = make_arrow_type(t1,t2);

        local_instance_env lie = lie1 + lie2;

        lie = apply_subst(s, lie);
        type = apply_subst(s, type);

        return {s, rule2, lie, type};
    }
}

tuple<substitution_t, Hs::Match, local_instance_env, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, Hs::Match m)
{
    substitution_t s;
    local_instance_env lie;
    Hs::Type result_type = fresh_type_var();

    for(auto& rule: m.rules)
    {
        auto [s1, rule1, lie1, t1] = infer_type(env, rule);
        rule = rule1;
        auto s2 = unify(result_type, t1);
        lie += lie1;
        s = compose(s2,compose(s1,s));
        result_type = apply_subst(s, result_type);
    }

    lie = apply_subst(s, lie);
    return {s, m, lie, result_type};
}



tuple<substitution_t, expression_ref, local_instance_env, Hs::Type>
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

        local_instance_env lie;
        for(auto& constraint: constraints)
        {
            auto dvar = fresh_var("dvar", false);
            lie = lie.insert({unloc(dvar.name), constraint});
            E = {E, dvar};
        }

        return {{}, E, lie, type};
    }
    else if (E.is_int())
    {
        auto [lie, type] = fresh_num_type();
        return { {}, E, lie, type };
    }
    else if (E.is_double())
    {
        auto [lie, type] = fresh_fractional_type();
        return { {}, E, lie, type };
    }
    else if (E.is_char())
    {
        local_instance_env lie;
        return { {}, E, lie, char_type() };
    }
    else if (E.is_log_double())
    {
        auto [lie, type] = fresh_fractional_type();
        return { {}, E, lie, type };
    }
    else if (auto l = E.to<Hs::List>())
    {
        local_instance_env lie;
        Hs::Type element_type = fresh_type_var();
        auto L = *l;
        substitution_t s;
        for(auto& element: L.elements)
        {
            auto [s1, element1, lie1, t1] = infer_type(env, element);
            element = element1;
            auto s2 = unify(t1, element_type);
            s = compose(s2, compose(s1, s));

            lie += lie1;
        }
        element_type = apply_subst(s, element_type);
        lie = apply_subst(s, lie);
        return { s, L, lie, Hs::ListType(element_type) };
    }
    else if (auto tup = E.to<Hs::Tuple>())
    {
        auto T = *tup;

        local_instance_env lie;

        substitution_t s;
        vector<Hs::Type> element_types;
        for(auto& element: T.elements)
        {
            auto [s1, element1, element_lie, element_type] = infer_type(env, element);
            element = element1;
            s = compose(s1, s);
            element_types.push_back( element_type );

            lie += element_lie;
        }
        Hs::Type result_type = Hs::TupleType(element_types);
        result_type = apply_subst(s, result_type);
        lie = apply_subst(s, lie);
        return {s, T, lie, result_type};
    }
    // COMB
    else if (is_apply_exp(E))
    {
        assert(E.size() >= 2);

        auto e1 = E.sub()[0];

        auto [s1, f, lie,t1] = infer_type(env,e1);
        substitution_t s = s1;

        vector<expression_ref> args;
        for(int i=1;i<E.size();i++)
        {
            auto e2 = E.sub()[i];

            // tv <- fresh
            auto tv = fresh_type_var();

            // This is now done by the previous iteration of the loop!
            // (s1, t1) <- infer env e1
            // auto [s1,t1] = infer_type(env, e1);

            // (s2, t2) <- infer (apply s1 env) e2
            auto [s2, arg2, lie2,t2] = infer_type(apply_subst(s1,env), e2);
            args.push_back(arg2);

            // s3       <- unify (apply s2 t1) (TArr t2 tv)
            auto s3 = unify (apply_subst(s2,t1), make_arrow_type(t2,tv));

            s = compose(s3,compose(s2,s));
            t1 = apply_subst(s3,tv);
            lie += lie2;
        }
        E = apply_expression(f, args);

        // This is now done by the setup for the next loop iteration.
        // return {compose(s3,compose(s2,s1)), apply_subst(s3,tv)};
        lie = apply_subst(s, lie);
        return {s, E, lie, t1};
    }
    // LAMBDA
    else if (auto lam = E.to<Hs::LambdaExp>())
    {
        auto Lam = *lam;
        auto rule = Hs::MRule{Lam.args, Lam.body};
        auto [s, rule2, lie, t] = infer_type(env, rule);
        Lam.args = rule.patterns;
        Lam.body = rule.rhs;
        return {s, Lam, lie, t};
    }
    // LET
    else if (auto let = E.to<Hs::LetExp>())
    {
        auto Let = *let;

        // 1. Extend environment with types for decls, get any substitutions
        auto [s_decls, binds, lie_decls, env_decls] = infer_type_for_decls(env, unloc(Let.binds));
        unloc(Let.binds) = binds;
        auto env2 = env_decls +  env;

        // 2. Compute type of let body
        auto [s_body, body, lie_body, t_body] = infer_type(env2, unloc(Let.body));
        unloc(Let.body) = body;

        // return (s1 `compose` s2, t2)
        auto s = compose(s_body, s_decls);
        auto lie = lie_decls + lie_body;
        lie = apply_subst(s, lie);
        return {s, Let, lie, t_body};
    }
    else if (auto con = E.head().to<Hs::Con>())
    {
        local_instance_env lie;
        auto [object_type, field_types] = constr_types(*con);

        substitution_t s;
        auto env2 = env;
        vector<Hs::Type> arg_types;
        vector<Hs::Exp> args = E.copy_sub();
        for(int i=0; i < args.size(); i++)
        {
            auto& arg = args[i];
            auto [s_i, arg_i, lie_i, t_i] = infer_type(env2, arg);
            arg = arg_i;
            arg_types.push_back(t_i);

            // REQUIRE that i-th argument matches the type for the i-th field.
            auto s2_i = unify( field_types[i], t_i);

            lie += lie_i;
            s = compose(compose(s2_i,s_i), s);
            env2 = apply_subst(s_i, env2);
        }
        E = expression_ref(*con, args);
        
        lie = apply_subst(s, lie);
        return { s, E, lie, apply_subst(s, object_type) };
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
        auto [s1, object, lie1, object_type] = infer_type(env, Case.object);
        Case.object = object;
        auto env2 = apply_subst(s1, env);
        
        // 2. Determine data type for object from patterns.
        Hs::Match match;
        for(auto& alt: Case.alts)
        {
            auto& [pattern, body] = unloc(alt);
            match.rules.push_back(Hs::MRule{{pattern},body});
        }

        auto [s2, match2, lie2, match_type] = infer_type(env2, match);

        for(int i=0;i<Case.alts.size();i++)
        {
            unloc(Case.alts[i]) = {match2.rules[i].patterns[0], match2.rules[i].rhs};
        }

        Hs::Type result_type = fresh_type_var();

        auto s3 = unify( make_arrow_type(object_type,result_type), match_type );

        auto s = compose(s3, compose(s2, s1));

        result_type = apply_subst(s, result_type);

        auto lie = lie1 + lie2;
        lie = apply_subst(s, lie);
        return { s, Case, lie, result_type };
    }
    // IF
    else if (auto if_exp = E.to<Hs::IfExp>())
    {
        auto If = *if_exp;
        auto [cond_s, cond, cond_lie, cond_type ] = infer_type(env, unloc(If.condition));
        auto [tbranch_s, tbranch, tbranch_lie, tbranch_type] = infer_type(env, unloc(If.true_branch));
        auto [fbranch_s, fbranch, fbranch_lie, fbranch_type] = infer_type(env, unloc(If.false_branch));
        unloc(If.condition) = If;
        unloc(If.true_branch) = tbranch;
        unloc(If.false_branch) = fbranch;

        auto s2 = unify(cond_type, bool_type());
        auto s3 = unify(tbranch_type, fbranch_type);

        auto s = compose(s3, compose(s2, compose(fbranch_s, compose(tbranch_s, cond_s))));

        auto result_type = apply_subst(s, tbranch_type);
        auto lie = cond_lie + tbranch_lie + fbranch_lie;
        lie = apply_subst(s, lie);
        return {s, If, lie, result_type};
    }
    // LISTCOMP
    else if (auto lcomp = E.to<Hs::ListComprehension>())
    {
        auto LComp = *lcomp;
        auto [quals_s, quals, quals_lie, quals_binders] = infer_quals_type(env, LComp.quals);
        auto [exp_s, body, exp_lie, exp_type] = infer_type(plus_prefer_right(env, quals_binders), LComp.body);
        LComp.quals = quals;
        LComp.body = body;
        auto s = compose(exp_s, quals_s);
        Hs::Type result_type = apply_subst(s, Hs::ListType(exp_type));

        auto lie = quals_lie + exp_lie;
        lie = apply_subst(s, lie);
        return { s, LComp, lie, result_type };
    }
    // ENUM-FROM
    else if (auto l = E.to<Hs::ListFrom>())
    {
        auto L = *l;
        auto [lie, t] = fresh_enum_type();

        // PROBLEM: Do we need to desugar these here, in order to plug in the dictionary?
        auto [s_from, from, lie_from, t_from] = infer_type(env, L.from);
        L.from = from;
        auto s1 = unify(t,t_from);
        auto s = compose(s1, s_from);
        t = apply_subst(s,t);

        lie += lie_from;
        lie = apply_subst(s, lie);
        return {s, L, lie, Hs::ListType(t) };
    }
    // ENUM-FROM-THEN
    else if (auto l = E.to<Hs::ListFromThen>())
    {
        auto L = *l;
        auto [lie, t] = fresh_enum_type();
        auto [s_from, from, lie_from, t_from] = infer_type(env, L.from);
        auto s1 = unify(t,t_from);
        auto s = compose(s1, s_from);
        t = apply_subst(s,t);

        auto [s_then, then, lie_then, t_then] = infer_type(env, L.then);
        auto s2 = unify(t,t_then);
        s = compose(s2, compose(s_then,s));
        t = apply_subst(s,t);
        L.from = from;
        L.then = then;
        
        lie += lie_from + lie_then;
        lie = apply_subst(s, lie);
        return {s, L, lie, Hs::ListType(t)};
    }
    // ENUM-FROM-TO
    else if (auto l = E.to<Hs::ListFromTo>())
    {
        auto L = *l;
        auto [lie, t] = fresh_enum_type();
        auto [s_from, from, lie_from, t_from] = infer_type(env, l->from);
        auto s1 = unify(t,t_from);
        auto s = compose(s1, s_from);
        t = apply_subst(s,t);

        auto [s_to, to, lie_to, t_to] = infer_type(env, l->to);
        auto s2 = unify(t,t_to);
        s = compose(s2, compose(s_to,s));
        t = apply_subst(s,t);
        L.from = from;
        L.to = to;
        
        lie += lie_from + lie_to;
        lie = apply_subst(s, lie);
        return {s, L, lie, Hs::ListType(t)};
    }
    // ENUM-FROM-THEN-TO
    else if (auto l = E.to<Hs::ListFromThenTo>())
    {
        auto L = *l;
        auto [lie, t] = fresh_enum_type();
        auto [s_from, from, lie_from, t_from] = infer_type(env, L.from);
        auto s1 = unify(t,t_from);
        auto s = compose(s1, s_from);
        t = apply_subst(s,t);

        auto [s_then, then, lie_then, t_then] = infer_type(env, L.then);
        auto s2 = unify(t,t_then);
        s = compose(s2, compose(s_then,s));
        t = apply_subst(s,t);

        auto [s_to, to, lie_to, t_to] = infer_type(env, l->to);
        auto s3 = unify(t,t_to);
        s = compose(s3, compose(s_to,s));
        t = apply_subst(s,t);
        L.from = from;
        L.then = then;
        L.to = to;

        lie += lie_from + lie_then + lie_to;
        lie = apply_subst(s, lie);
        return {s, L, lie, Hs::ListType(t)};
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

        gve = plus_no_overlap(gve, gve1);
        gie = plus_no_overlap(gie, gie1);
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
    if (class_decl.decls)
    {
        for(auto& [name, type]: unloc(*class_decl.decls).signatures)
        {
            Hs::Type method_type = type_check_class_method_type(K, type, constraint);

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
        type = add_forall_vars(class_typevars, type);
        gie = gie.insert({unloc(get_dict.name), type});
    }
    cinfo.fields = plus_no_overlap(gve, gie);

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

optional<Hs::Var> contains_constraint(const local_instance_env& lie)
{
    return {};
}

// How does this relate to simplifying constraints?
Hs::Binds typechecker_state::get_dicts(const global_instance_env& gie, const local_instance_env& lie1, const local_instance_env& lie2)
{
    Hs::Binds binds;
    for(auto& [name,constraint]: lie2)
    {
        auto [class_con, args] = decompose_type_apps(extract_class_constraint(constraint));
        for(auto& arg: args)
        {
            // name = dvar
            auto [head, type_args] = decompose_type_apps(arg);

            if (auto dvar = contains_constraint(lie1))
            {
                auto decl = Hs::simple_decl(Hs::Var({noloc,name}), *dvar);
                // binds[0].push_back( decl ) ?
            }
            else if (auto k = head.to<Hs::TypeCon>())
            {
                // Look up instance for K (head a1 a2) in gie -- there should be an instance defined for it.
            }
            else if (auto a = constraint.is_a<Hs::TypeVar>())
            {
                // There might be another assumption in LIE1 (or LIE2?) of the form dvar' :: k' a
                // Then we could extract a as a superclass for k' a.

                // Uh.... Is this basically simplifying the context?
            }
            else
                std::abort();
        }
    }
    return binds;
}

global_instance_env
typechecker_state::infer_type_for_instance1(const Hs::InstanceDecl& inst_decl, const class_env& ce, const global_instance_env& gie_in)
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
    inst_type = add_forall_vars( free_type_VARS(inst_type) | ranges::to<vector>, inst_type);
    global_instance_env gie;
    gie = gie.insert( { unloc(dfun.name), inst_type } );
    return gie;
}

// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
global_instance_env
typechecker_state::infer_type_for_instances1(const Hs::Decls& decls, const class_env& ce, const global_instance_env& gie_in)
{
    global_instance_env gie_inst;
    for(auto& decl: decls)
    {
        if (auto I = decl.to<Hs::InstanceDecl>())
        {
            auto gie = infer_type_for_instance1(*I, ce, gie_in);

            gie_inst = plus_no_overlap(gie_inst, gie);
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
typechecker_state::infer_type_for_instance2(const Hs::InstanceDecl& inst_decl, const class_env& ce, const global_instance_env& gie_in)
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
    Hs::Binds binds_super = get_dicts(gie_in, lie, lie_super);

    // Premise 8:
    Hs::Binds binds_methods;
    auto dfun = fresh_var("dfun", true);

    // dfun = /\a1..an -> \dicts:theta -> let binds_super in let_binds_methods in <superdicts,methods>
    expression_ref dict1 = tuple_from_value_env(cinfo->fields);
    expression_ref dict2 = tuple_from_value_env(lie);

    expression_ref E = Hs::LetExp( {noloc,binds_methods}, {noloc, dict1} );
    E = Hs::LetExp( {noloc,binds_super}, {noloc,E} );
    E = Hs::LambdaExp({dict2}, E);

    Hs::Decls decls ({ simple_decl(dfun,E) });
    return decls;
}

// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
Hs::Decls
typechecker_state::infer_type_for_instances2(const Hs::Decls& decls, const class_env& ce, const global_instance_env& gie_in)
{
    Hs::Decls out_decls;
    global_instance_env gie_inst;
    for(auto& decl: decls)
    {
        if (auto I = decl.to<Hs::InstanceDecl>())
        {
            auto decls_ = infer_type_for_instance2(*I, ce, gie_in);

            for(auto& d: decls_)
                out_decls.push_back(d);
        }
    }
    return out_decls;
}

Hs::ModuleDecls typecheck( const string& mod_name, Hs::ModuleDecls M )
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
    typechecker_state state( mod_name, constr_info );
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

    auto inst_gie = state.infer_type_for_instances1(M.type_decls, class_info, class_gie);

    auto gie = plus_no_overlap(class_gie, inst_gie);
    state.gie = gie;

    for(auto& [method,type]: inst_gie)
    {
        std::cerr<<method<<" :: "<<type.print()<<"\n";
    }
    std::cerr<<"\n";

    // 3. E' = (TCE_T, (CVE_T, GVE_C, LVE={}), CE_C, (GIE_C, LIE={}))

    auto [s, value_decls, lie,env] = state.infer_type_for_decls(gve, M.value_decls);
    M.value_decls = value_decls;

    for(auto& [x,t]: env)
    {
        std::cerr<<x<<" :: "<<remove_top_level_foralls(alphabetize_type(t))<<"\n";
//        std::cerr<<x<<" = "<<e<<"\n\n\n";
    }
    std::cerr<<"\n";

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


