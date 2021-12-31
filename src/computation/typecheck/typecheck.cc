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

namespace views = ranges::views;

using std::vector;
using std::string;
using std::map;
using std::set;
using std::pair;
using std::shared_ptr;
using std::tuple;

/*
  TODO:
  1. Check that constraints in classes only mention type vars.
  2. Check that constraints in instance heads are of the form Class (Tycon a1 a2 a3..)
  3. Check that constraints in instance contexts satisfy the "paterson conditions"
  4. Construct the list of class -> [tycon] -> instance to record which instances exist
     (like in THIH -- no actual code generation yet)
  5. How are we actually supposed to store the GIE?
  6. Put class methods into global namespace WITH their type -> how?
  7. How do we export stuff?

  Cleanups:
  1. Implement kinds as Hs::Type
  2. Use Hs::decompose_type_apps( ) to simplify Hs::is_function_type( ),
     like in parser.y:check_kind( ).

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

string print(const value_env& env)
{
    std::ostringstream oss;
    for(auto& [value,type]: env)
    {
        oss<<value<<" :: "<<type.print()<<"\n";
    }
    return oss.str();
}

string print(const substitution_t& s)
{
    std::ostringstream oss;
    for(auto& [var,type]: s)
    {
        oss<<var.print()<<" :: "<<type.print()<<"\n";
    }
    return oss.str();
}

typedef value_env global_value_env;

typedef value_env local_value_env;

void add_prefer_right(value_env& e1, const value_env& e2)
{
    if (e1.empty())
        e1 = e2;
    else
        for(auto& [x,t]: e2)
            e1 = e1.insert({x,t});
}

value_env plus_prefer_right(const value_env& e1, const value_env& e2)
{
    auto e3 = e1;
    add_prefer_right(e3,e2);
    return e3;
}

void add_no_overlap(value_env& e1, const value_env& e2)
{
    if (e1.empty())
        e1 = e2;
    else
        for(auto& [x,t]: e2)
        {
            if (e1.count(x))
                throw myexception()<<"Both environments contain variable "<<x<<"!";
            e1 = e1.insert({x,t});
        }
}

value_env plus_no_overlap(const value_env& e1, const value_env& e2)
{
    auto e3 = e1;
    add_no_overlap(e3,e2);
    return e3;
}

void add_no_overlap(type_con_env& e1, const type_con_env& e2)
{
    if (e1.empty())
        e1 = e2;
    else
        for(auto& [x,t]: e2)
        {
            if (e1.count(x))
                throw myexception()<<"Both environments contain variable "<<x<<"!";
            e1.insert({x,t});
        }
}

type_con_env plus_no_overlap(const type_con_env& e1, const type_con_env& e2)
{
    auto e3 = e1;
    add_no_overlap(e3,e2);
    return e3;
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

string alphabetized_type_var_name(int i)
{
    string s;
    while (true)
    {
        s.push_back( char('a'+(i%26)) );
        if (i < 26) break;
        i /= 26;
    }
    return s;
}

Haskell::TypeVar alphabetized_type_var(int i)
{
    auto s = alphabetized_type_var_name(i);
    auto v = Haskell::TypeVar({noloc,s});
    v.index = i;
    return v;
}

expression_ref alphabetize_type(const expression_ref& type, map<Haskell::TypeVar,Haskell::TypeVar>& s, int& index);

Hs::Context alphabetize(Hs::Context context, map<Haskell::TypeVar,Haskell::TypeVar>& s, int& index)
{
    for(auto& constraint: context.constraints)
        constraint = alphabetize_type(constraint, s, index);
    return context;
}

expression_ref alphabetize_type(const expression_ref& type, map<Haskell::TypeVar,Haskell::TypeVar>& s, int& index)
{
    // Lets just assume that there is no shadowing.

    if (auto tv = type.to<Haskell::TypeVar>())
    {
        auto rec = s.find(*tv);
        if (rec == s.end())
        {
            rec = s.insert({*tv, alphabetized_type_var(index++)}).first;
        }
        return expression_ref(rec->second);
    }
    else if (type.is_a<Haskell::TypeCon>())
        return type;
    else if (type.is_a<Haskell::ForallType>())
    {
        auto forall = type.as_<Haskell::ForallType>();

        // 2a. Ensure that we see each of the type var binders in the order they are used.
        for(auto& tv: forall.type_var_binders)
        {
            alphabetize_type(tv, s, index);
            tv = s.at(tv);
        }

        // 2b. Alphabetize the type body.
        forall.type = alphabetize_type(forall.type, s, index);

        // 2c. Return the type
        return forall;
    }
    else if (type.is_a<Haskell::TypeApp>())
    {
        auto app = type.as_<Haskell::TypeApp>();
        app.head = alphabetize_type(app.head, s, index);
        app.arg  = alphabetize_type(app.arg , s, index);
        return app;
    }
    else if (auto l = type.to<Hs::ListType>())
    {
        auto L = *l;
        L.element_type = alphabetize_type(L.element_type, s, index);
        return L;
    }
    else if (auto tup = type.to<Hs::TupleType>())
    {
        auto T = *tup;
        for(auto& type: T.element_types)
            type = alphabetize_type(type, s, index);
        return T;;
    }
    else if (auto c = type.to<Hs::ConstrainedType>())
    {
        auto C = *c;
        C.type = alphabetize_type(C.type, s, index);
        C.context = alphabetize(C.context, s, index);
        return C;
    }
    else if (auto sl = type.to<Hs::StrictLazyType>())
    {
        auto SL = *sl;
        SL.type = alphabetize_type(SL.type, s, index);
        return SL;
    }
    else
        std::abort();
}

expression_ref alphabetize_type(const expression_ref& type)
{
    map<Haskell::TypeVar, Haskell::TypeVar> s;
    int index = 0;
    return alphabetize_type(type, s, index);
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
    typechecker_state(const constr_env& ce)
        :con_info(ce)
        { }

    Hs::Type bool_type() const { return Hs::TypeCon({noloc,"Data.Bool.True"}); }

    Hs::Type num_type() const { return Hs::TypeCon({noloc,"Num#"}); }

    Hs::Type char_type() const { return Hs::TypeCon({noloc,"Char#"}); }

    constr_env con_info;

    pair<Hs::Type, vector<Hs::Type>> constr_types(const Hs::Con&);

    int next_var_index = 1;

    Hs::TypeVar fresh_type_var() {
        Hs::TypeVar tv({noloc, "t"+std::to_string(next_var_index)});
        tv.index = next_var_index;
        next_var_index++;
        return tv;
    }

    Hs::TypeVar named_type_var(const string& name)
    {
        Hs::TypeVar tv({noloc, name+"_"+std::to_string(next_var_index)});
        tv.index = next_var_index;
        next_var_index++;
        return tv;
    }

    Hs::Type instantiate(const Hs::Type& t);

    pair<substitution_t, local_value_env>
    infer_quals_type(const global_value_env& env, const vector<Hs::Qual>& quals);

    pair<substitution_t, local_value_env>
    infer_qual_type(const global_value_env& env, const Hs::Qual& qual);

    pair<substitution_t, local_value_env>
    infer_guard_type(const global_value_env& env, const Hs::Qual& guard);

    pair<Hs::Type, local_value_env>
    infer_pattern_type(const Hs::Pattern& pat);

    pair<substitution_t, Hs::Type>
    infer_type(const global_value_env& env, const expression_ref& exp);

    pair<substitution_t, Hs::Type>
    infer_type(const global_value_env& env, const Hs::GuardedRHS&);

    pair<substitution_t, Hs::Type>
    infer_type(const global_value_env& env, const Hs::MultiGuardedRHS&);

    pair<substitution_t, Hs::Type>
    infer_type(const global_value_env& env, const Hs::MRule&);

    pair<substitution_t, Hs::Type>
    infer_type(const global_value_env& env, const Hs::Match&);

    pair<substitution_t, global_value_env>
    infer_type_for_decls(const global_value_env& env, const Hs::Decls& E);

    pair<substitution_t, global_value_env>
    infer_type_for_decls(const global_value_env& env, const Hs::Binds& binds);
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
        return {Hs::ListType(a),{a,Hs::ListType(a)}};
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
    auto con_type = instantiate(con_info.at(con_name));
    vector<Hs::Type> field_types;

    while(auto f = Hs::is_function_type(con_type))
    {
        auto [t1,t2] = *f;
        field_types.push_back(t1);
        con_type = t2;
    }
    auto object_type = con_type;

    return {object_type, field_types};
}

void get_free_type_variables(const Hs::Type& T, std::multiset<Hs::TypeVar>& bound, set<Hs::TypeVar>& free)
{
    // 1. fv x = { x }
    // However, if there are any binders called "x", then it won't be bound at the top level.
    if (auto tv = T.to<Hs::TypeVar>())
    {
	if (bound.find(*tv) == bound.end())
	    free.insert(*tv);
	return;
    }
    // 2. fv (t1 t2) = fv(t1) + fv(t2);
    else if (auto a = T.to<Hs::TypeApp>())
    {
        get_free_type_variables(a->head, bound, free);
        get_free_type_variables(a->arg, bound, free);
    }
    // 3. fv (k) = {}
    else if (T.is_a<Hs::TypeCon>())
    {
    }
    // 3. fv (k) = {}
    else if (auto l = T.to<Hs::ListType>())
    {
        get_free_type_variables(l->element_type, bound, free);
    }
    // 3. fv (k) = {}
    else if (auto l = T.to<Hs::TupleType>())
    {
        for(auto& element_type: l->element_types)
            get_free_type_variables(element_type, bound, free);
    }
    // 3. fv (k) = {}
    else if (auto c = T.to<Hs::ConstrainedType>())
    {
        // The constraints should only mention variables that are mentioned in the main type
        get_free_type_variables(c->type, bound, free);
    }
    // 3. fv (k) = {}
    else if (auto sl = T.to<Hs::StrictLazyType>())
    {
        get_free_type_variables(sl->type, bound, free);
    }
    // 4. fv (forall a.tau) = fv(tau) - a
    else if (auto f = T.to<Hs::ForallType>())
    {
        for(auto& tv: f->type_var_binders)
            bound.insert(tv);

        get_free_type_variables(f->type, bound, free);

        for(auto& tv: f->type_var_binders)
            bound.erase(tv);
    }
    else
        std::abort();
}

void get_free_type_variables(const Hs::Type t, set<Hs::TypeVar>& free)
{
    std::multiset<Hs::TypeVar> bound;
    get_free_type_variables(t, bound, free);
}

std::set<Hs::TypeVar> free_type_variables(const Hs::Type& t)
{
    std::set<Hs::TypeVar> free;
    get_free_type_variables(t, free);
    return free;
}

void get_free_type_variables(const global_value_env& env, std::set<Hs::TypeVar>& free)
{
    for(auto& [x,type]: env)
        get_free_type_variables(type, free);
}

std::set<Hs::TypeVar> free_type_variables(const global_value_env& env)
{
    std::set<Hs::TypeVar> free;
    get_free_type_variables(env, free);
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

expression_ref typechecker_state::instantiate(const expression_ref& t)
{
    substitution_t s;
    auto t2 = t;
    while(auto fa = t2.to<Hs::ForallType>())
    {
        for(auto& tv: fa->type_var_binders)
        {
            auto new_tv = fresh_type_var();
            new_tv.kind = tv.kind;
            s = s.insert({tv,new_tv});
        }
        t2 = fa->type;
    }
    return apply_subst(s,t2);
}

pair<substitution_t, global_value_env>
typechecker_state::infer_type_for_decls(const global_value_env& env, const Hs::Binds& binds)
{
    substitution_t s;
    auto env2 = env;
    global_value_env binders;
    for(auto& decls: binds)
    {
        auto [s1, binders1] = infer_type_for_decls(env2, decls);
        env2 = plus_prefer_right(env2, binders1);
        binders = plus_no_overlap(binders, binders1);
        s = compose(s1, s);
    }
    binders = apply_subst(s, binders);
    return {s, binders};
}

pair<substitution_t,global_value_env>
typechecker_state::infer_type_for_decls(const global_value_env& env, const Hs::Decls& decls)
{
    // 1. Add each let-binder to the environment with a fresh type variable
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
            auto [type, lve] = infer_pattern_type(pd->lhs);
            decl_types.push_back({type,lve});
        }
        auto& [type,lve] = decl_types.back();
        binder_env = plus_no_overlap(binder_env, lve);
    }
    auto env2 = plus_prefer_right(env, binder_env);

    // 2. Infer the types of each of the x[i]
    substitution_t s;
    for(int i=0;i<decls.size();i++)
    {
        auto& decl = decls[i];
        if (auto fd = decl.to<Hs::FunDecl>())
        {
            auto& name = unloc(fd->v.name);
            auto lhs_type = env2.at(name);
            auto [s2, rhs_type] = infer_type(env2, fd->match);
            s = compose(s2, compose(unify(lhs_type, rhs_type), s));
        }
        else if (auto pd = decl.to<Hs::PatDecl>())
        {
            auto [lhs_type, lve] = infer_pattern_type(pd->lhs);
            auto [s2, rhs_type] = infer_type(env2, pd->rhs);

            s = compose(s2, compose(unify(lhs_type, rhs_type), s));
        }

        binder_env = apply_subst(s, binder_env);
        env2 = apply_subst(s, env2);
    }

    // 3. Generalize each type over variables not in the *original* environment
    value_env generalized_binder_env;
    for(auto& [var,type]: binder_env)
        generalized_binder_env = generalized_binder_env.insert({var,generalize(env, type)});

    return {s, generalized_binder_env};
}

// Figure 24. Rules for patterns
pair<Hs::Type, local_value_env>
typechecker_state::infer_pattern_type(const Hs::Pattern& pat)
{
    // TAUT-PAT
    if (auto x = pat.to<Haskell::Var>())
    {
        Hs::Type type = fresh_type_var();
        local_value_env lve;
        auto& name = unloc(x->name);
        lve = lve.insert({name, type});
	return { type , lve };
    }
    // CONSTR-PAT
    else if (auto con = pat.head().to<Haskell::Con>())
    {
        local_value_env lve;
        vector<Hs::Type> types;
        for(auto& pat: pat.copy_sub())
        {
            auto [t1,lve1] = infer_pattern_type(pat);
            types.push_back(t1);
            lve = plus_no_overlap(lve, lve1);
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

        lve = apply_subst(s, lve);
        type = apply_subst(s, type);
        return {type, lve};
    }
    // AS-PAT
    else if (auto ap = pat.to<Haskell::AsPattern>())
    {
        auto [t,lve] = infer_pattern_type(ap->pattern);
        auto& name = unloc(ap->var.as_<Hs::Var>().name);
        lve = lve.insert({name, t});
        return {t,lve};
    }
    // LAZY-PAT
    else if (auto lp = pat.to<Haskell::LazyPattern>())
        return infer_pattern_type(lp->pattern);
    // not in paper (STRICT-PAT)
    else if (auto sp = pat.to<Haskell::StrictPattern>())
        return infer_pattern_type(sp->pattern);
    // WILD-PAT
    else if (pat.is_a<Haskell::WildcardPattern>())
    {
        auto tv = fresh_type_var();
        return {tv,{}};
    }
    // LIST-PAT
    else if (auto l = pat.to<Haskell::List>())
    {
        local_value_env lve;
        Hs::Type t = fresh_type_var();
        substitution_t s;
        for(auto& element: l->elements)
        {
            auto [t1,lve1] = infer_pattern_type(element);
            auto s1 = unify(t, t1);
            s = compose(s1,s);
            lve = plus_no_overlap(lve, lve1);
        }
        t = apply_subst(s, t);
        lve = apply_subst(s, lve);
        return {t, lve};
    }
    // TUPLE-PAT
    else if (auto t = pat.to<Haskell::Tuple>())
    {
        vector<Hs::Type> types;
        local_value_env lve;
        for(auto& element: t->elements)
        {
            auto [t1, lve1] = infer_pattern_type(element);
            types.push_back(t1);
            lve = plus_no_overlap(lve, lve1);
        }
        return {Hs::TupleType(types), lve};
    }
    // ???
    else if (pat.is_int() or pat.is_double() or pat.is_char() or pat.is_log_double())
        return {num_type(),{}};
    else
        throw myexception()<<"Unrecognized pattern '"<<pat<<"'!";
    
}


// Figure 22. Rules for quals
//
// The implementation is rather... different?
// * the original figure doesn't have let quals.
// * the original figure seems to assume that quals only occur in list comprehensions?

pair<substitution_t,value_env>
typechecker_state::infer_quals_type(const global_value_env& env, const vector<Hs::Qual>& quals)
{
    substitution_t s;
    auto env2 = env;
    local_value_env binders;
    for(auto& qual: quals)
    {
        auto [qual_s, qual_binders] = infer_qual_type(env2, qual);
        s = compose(qual_s, s);
        env2 = plus_prefer_right(env2, qual_binders);
        binders = plus_prefer_right(binders, qual_binders);
    }
    binders = apply_subst(s, binders);
    return {s, binders};
}

pair<substitution_t,value_env>
typechecker_state::infer_qual_type(const global_value_env& env, const Hs::Qual& qual)
{
    // FILTER
    if (auto sq = qual.to<Hs::SimpleQual>())
    {
        auto [cond_s, cond_type] = infer_type(env, sq->exp);
        auto s2 = unify( cond_type, bool_type() );
        auto s = compose(s2, cond_s);
        return {s, {}};
    }
    // GENERATOR.
    else if (auto pq = qual.to<Hs::PatQual>())
    {
        // pat <- exp
        auto [pat_type,lve] = infer_pattern_type(pq->bindpat);
        auto [exp_s,exp_type] = infer_type(env, pq->exp);
        // type(pat) = type(exp)
        auto s3 = unify(Hs::ListType(pat_type), exp_type);
        auto s = compose(s3, exp_s);
        lve = apply_subst(s, lve);
        return {s, lve};
    }
    else if (auto lq = qual.to<Hs::LetQual>())
    {
        return infer_type_for_decls(env, unloc(lq->binds));
    }
    else
        std::abort();
}


pair<substitution_t,value_env>
typechecker_state::infer_guard_type(const global_value_env& env, const Hs::Qual& guard)
{
    if (auto sq = guard.to<Hs::SimpleQual>())
    {
        auto [cond_s, cond_type] = infer_type(env, sq->exp);
        auto s2 = unify( cond_type, bool_type() );
        auto s = compose(s2, cond_s);
        return {s, {}};
    }
    else if (auto pq = guard.to<Hs::PatQual>())
    {
        // pat <- exp
        auto [pat_type,lve] = infer_pattern_type(pq->bindpat);
        auto [exp_s,exp_type] = infer_type(env, pq->exp);
        // type(pat) = type(exp)
        auto s3 = unify(pat_type,exp_type);
        auto s = compose(s3, exp_s);
        lve = apply_subst(s, lve);
        return {s, lve};
    }
    else if (auto lq = guard.to<Hs::LetQual>())
    {
        return infer_type_for_decls(env, unloc(lq->binds));
    }
    else
        std::abort();
}


// Figure 25. Rules for match, mrule, and grhs
pair<substitution_t, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, const Hs::GuardedRHS& rhs)
{
    // Fig 25. GUARD-DEFAULT
    if (rhs.guards.empty()) return infer_type(env, rhs.body);

    // Fig 25. GUARD
    auto guard = rhs.guards[0];
    auto [s1, env1] = infer_guard_type(env, guard);
    auto env2 = plus_prefer_right(env, env1);

    auto rhs2 = rhs;
    rhs2.guards.erase(rhs2.guards.begin());
    auto [s2,t2] = infer_type(env2, rhs2);
    auto s = compose(s2, s1);

    Hs::Type type = apply_subst(s, t2);
    return {s, type};
}

// Fig 25. GUARD-OR
pair<substitution_t, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, const Hs::MultiGuardedRHS& rhs)
{
    substitution_t s;
    Hs::Type type = fresh_type_var();

    auto env2 = env;
    if (rhs.decls)
    {
        auto [s1, binders] = infer_type_for_decls(env, unloc(*rhs.decls));
        env2 = plus_prefer_right(env, binders);
        s = compose(s1, s);
    }

    for(auto& guarded_rhs: rhs.guarded_rhss)
    {
        auto [s1,t1] = infer_type(env2, guarded_rhs);
        auto s2 = unify(t1,type);
        s = compose(s2,compose(s1,s));
    }
    type = apply_subst(s, type);
    return {s, type};
};

pair<substitution_t, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, const Hs::MRule& rule)
{

    if (rule.patterns.empty())
        return infer_type(env, rule.rhs);
    else
    {
        auto [t1, lve1] = infer_pattern_type(rule.patterns.front());
        auto env2 = plus_no_overlap(env, lve1);

        // Remove the first pattern in the rule
        auto rule2 = rule;
        rule2.patterns.erase(rule2.patterns.begin());

        auto [s, t2] = infer_type(env2, rule2);
        t1 = apply_subst(s, t1);

        Hs::Type type = Hs::make_arrow_type(t1,t2);

        return {s, type};
    }
}

pair<substitution_t, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, const Hs::Match& m)
{
    substitution_t s;
    Hs::Type result_type = fresh_type_var();

    for(auto& rule: m.rules)
    {
        auto [s1,t1] = infer_type(env, rule);
        auto s2 = unify(result_type, t1);
        s = compose(s2,compose(s1,s));
        result_type = apply_subst(s, result_type);
    }

    return {s,result_type};
}

pair<substitution_t,Hs::Type>
typechecker_state::infer_type(const global_value_env& env, const expression_ref& E)
{
    if (auto x = E.to<Hs::Var>())
    {
        auto& x_name = unloc(x->name);
        auto sigma = env.find( x_name );

        // x should be in the type environment
        if (not sigma)
            throw myexception()<<"infer_type: can't find type of variable '"<<x->print()<<"'";

        auto tau = instantiate(*sigma);

        return {{},tau};
    }
    else if (E.is_int() or E.is_double() or E.is_log_double())
        return {{},num_type()};
    else if (E.is_char())
        return {{},char_type()};
    else if (auto l = E.to<Hs::List>())
    {
        Hs::Type element_type = fresh_type_var();
        auto L = *l;
        substitution_t s;
        for(auto& element: L.elements)
        {
            auto [s1, t1] = infer_type(env, element);
            auto s2 = unify(t1, element_type);
            s = compose(s2, compose(s1, s));
        }
        element_type = apply_subst(s, element_type);
        return {s, Hs::ListType(element_type)};
    }
    else if (auto tup = E.to<Hs::Tuple>())
    {
        auto T = *tup;

        substitution_t s;
        vector<Hs::Type> element_types;
        for(auto& element: T.elements)
        {
            auto [s1, element_type] = infer_type(env, element);
            s = compose(s1, s);
            element_types.push_back( element_type );
        }
        Hs::Type result_type = Hs::TupleType(element_types);
        result_type = apply_subst(s, result_type);
        return {s, result_type};
    }
    // COMB
    else if (is_apply_exp(E))
    {
        assert(E.size() >= 2);

        auto e1 = E.sub()[0];
        substitution_t s;

        auto [s1,t1] = infer_type(env,e1);

        for(int i=1;i<E.size();i++)
        {
            auto e2 = E.sub()[i];

            // tv <- fresh
            auto tv = fresh_type_var();

            // This is now done by the previous iteration of the loop!
            // (s1, t1) <- infer env e1
            // auto [s1,t1] = infer_type(env, e1);

            // (s2, t2) <- infer (apply s1 env) e2
            auto [s2,t2] = infer_type(apply_subst(s1,env), e2);

            // s3       <- unify (apply s2 t1) (TArr t2 tv)
            auto s3 = unify (apply_subst(s2,t1), make_arrow_type(t2,tv));

            s1 = compose(s3,compose(s2,s1));
            t1 = apply_subst(s3,tv);
        }

        // This is now done by the setup for the next loop iteration.
        // return {compose(s3,compose(s2,s1)), apply_subst(s3,tv)};
        return {s1,t1};
    }
    // LAMBDA
    else if (auto lam = E.to<Hs::LambdaExp>())
    {
        auto rule = Hs::MRule{lam->args, lam->body};

        return infer_type(env, rule);
    }
    // LET
    else if (auto let = E.to<Hs::LetExp>())
    {
        // 1. Extend environment with types for decls, get any substitutions
        auto [s_decls, env_decls] = infer_type_for_decls(env, unloc(let->binds));
        auto env2 = plus_no_overlap(env_decls, env);

        // 2. Compute type of let body
        auto [s_body, t_body] = infer_type(env2, unloc(let->body));

        // return (s1 `compose` s2, t2)
        return {compose(s_body, s_decls), t_body};
    }
    else if (auto con = E.head().to<Hs::Con>())
    {
        auto [object_type, field_types] = constr_types(*con);

        substitution_t s;
        auto env2 = env;
        vector<Hs::Type> arg_types;
        for(int i=0; i<E.size(); i++)
        {
            auto [s_i, t_i] = infer_type(env2, E.sub()[i]);
            arg_types.push_back(t_i);

            // REQUIRE that i-th argument matches the type for the i-th field.
            auto s2_i = unify( field_types[i], t_i);

            s = compose(compose(s2_i,s_i), s);
            env2 = apply_subst(s_i, env2);
        }

        return {s, apply_subst(s, object_type)};
    }
    else if (is_non_apply_op_exp(E))
    {
        std::abort();
        // this includes builtins like Prelude::add
    }
    // CASE
    else if (auto case_exp = E.to<Hs::CaseExp>())
    {
        // 1. Determine object type
        auto [s1, object_type] = infer_type(env, case_exp->object);
        auto env2 = apply_subst(s1, env);

        // 2. Determine data type for object from patterns.
        Hs::Match match;
        for(auto& alt: case_exp->alts)
        {
            auto& [pattern, body] = unloc(alt);
            match.rules.push_back(Hs::MRule{{pattern},body});
        }

        auto [s2, match_type] = infer_type(env2, match);

        Hs::Type result_type = fresh_type_var();

        auto s3 = unify( Hs::make_arrow_type(object_type,result_type), match_type );

        auto s = compose(s3, compose(s2, s1));

        result_type = apply_subst(s, result_type);

        return {s, result_type};
    }
    // IF
    else if (auto if_exp = E.to<Hs::IfExp>())
    {
        auto [cond_s, cond_type] = infer_type(env, unloc(if_exp->condition));
        auto [tbranch_s, tbranch_type] = infer_type(env, unloc(if_exp->true_branch));
        auto [fbranch_s, fbranch_type] = infer_type(env, unloc(if_exp->false_branch));

        auto s2 = unify(cond_type, bool_type());
        auto s3 = unify(tbranch_type, fbranch_type);

        auto s = compose(s3, compose(s2, compose(fbranch_s, compose(tbranch_s, cond_s))));

        auto result_type = apply_subst(s, tbranch_type);
        return {s, result_type};
    }
    // LISTCOMP
    else if (auto lcomp = E.to<Hs::ListComprehension>())
    {
        auto [quals_s, quals_binders] = infer_quals_type(env, lcomp->quals);
        auto [exp_s, exp_type] = infer_type(plus_prefer_right(env, quals_binders), lcomp->body);
        auto s = compose(exp_s, quals_s);
        Hs::Type result_type = apply_subst(s, Hs::ListType(exp_type));
        return {s, result_type};
    }
    // ENUM-FROM
    else if (auto l = E.to<Hs::ListFrom>())
    {
    // PROBLEM: the ENUM rules actually take any type t with an Enum t instance.
//        Hs::Type t = fresh_type_var();
        Hs::Type t = Hs::TypeCon({noloc,"Num#"});

        // PROBLEM: Do we need to desugar these here, in order to plug in the dictionary?
        auto [s_from, t_from] = infer_type(env, l->from);
        auto s1 = unify(t,t_from);
        auto s = compose(s1, s_from);
        t = apply_subst(s,t);

        return {s, Hs::ListType(t)};
    }
    // ENUM-FROM-THEN
    else if (auto l = E.to<Hs::ListFromThen>())
    {
//        Hs::Type t = fresh_type_var();
        Hs::Type t = Hs::TypeCon({noloc,"Num#"});
        auto [s_from, t_from] = infer_type(env, l->from);
        auto s1 = unify(t,t_from);
        auto s = compose(s1, s_from);
        t = apply_subst(s,t);

        auto [s_then, t_then] = infer_type(env, l->then);
        auto s2 = unify(t,t_then);
        s = compose(s2, compose(s_then,s));
        t = apply_subst(s,t);

        return {s, Hs::ListType(t)};
    }
    // ENUM-FROM-TO
    else if (auto l = E.to<Hs::ListFromTo>())
    {
//        Hs::Type t = fresh_type_var();
        Hs::Type t = Hs::TypeCon({noloc,"Num#"});
        auto [s_from, t_from] = infer_type(env, l->from);
        auto s1 = unify(t,t_from);
        auto s = compose(s1, s_from);
        t = apply_subst(s,t);

        auto [s_to, t_to] = infer_type(env, l->to);
        auto s2 = unify(t,t_to);
        s = compose(s2, compose(s_to,s));
        t = apply_subst(s,t);

        return {s, Hs::ListType(t)};
    }
    // ENUM-FROM-THEN-TO
    else if (auto l = E.to<Hs::ListFromThenTo>())
    {
//        Hs::Type t = fresh_type_var();
        Hs::Type t = Hs::TypeCon({noloc,"Num#"});
        auto [s_from, t_from] = infer_type(env, l->from);
        auto s1 = unify(t,t_from);
        auto s = compose(s1, s_from);
        t = apply_subst(s,t);

        auto [s_then, t_then] = infer_type(env, l->then);
        auto s2 = unify(t,t_then);
        s = compose(s2, compose(s_then,s));
        t = apply_subst(s,t);

        auto [s_to, t_to] = infer_type(env, l->to);
        auto s3 = unify(t,t_to);
        s = compose(s3, compose(s_to,s));
        t = apply_subst(s,t);

        return {s, Hs::ListType(t)};
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

constr_env get_constructor_info(const Module& m, const Hs::Decls& decls, const type_con_env& tce)
{
    constr_env cve;

    kindchecker_state ks(m, tce);

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

class_env get_class_info(const Module& m, const Hs::Decls& decls, const type_con_env& tce)
{
    class_env ce;

    constr_env cve;

    kindchecker_state ks(m, tce);

    for(auto& decl: decls)
    {
        auto c = decl.to<Hs::ClassDecl>();
        if (not c) continue;

        auto class_info = ks.type_check_type_class(*c);
        ce.insert({class_info.name, class_info});
    }

    return ce;
}

void typecheck(const Module& m, const Hs::ModuleDecls& M)
{
    // 1. Check the module's type declarations, and derives a Type Environment TE_T:(TCE_T, CVE_T)
    //    OK, so datatypes produce a
    //    * Type Constructor Environment (TCE) = tycon -> (kind, arity, method of applying the tycon?)
    //    * Constructor Value Environment (CVE)
    //
    // 2. Check the module's class declarations, produce some translated bindings -> binds_C ( GVE_C, CE_C, GIE_C )

    // TCE_T = type con info, part1
    std::cerr<<"-------- module "<<m.name<<"--------\n";
    auto tce = get_tycon_info(m, M.type_decls);
    for(auto& [tycon,ka]: tce)
    {
        auto& [k,arity] = ka;
        std::cerr<<tycon<<" :: "<<k->print()<<"\n";
    }
    std::cerr<<"\n";

    // CVE_T = constructor types :: map<string, polytype> = global_value_env
    auto constr_info = get_constructor_info(m, M.type_decls, tce);

    for(auto& [con,type]: constr_info)
    {
        std::cerr<<con<<" :: "<<type.print()<<"\n";
    }
    std::cerr<<"\n";

    //   CE_C  = class name -> class info
    class_env class_info = get_class_info(m, M.type_decls, tce);
    // GVE_C = {method -> type map} :: map<string, polytype> = global_value_env
    global_value_env class_method_info;
    for(auto& [name,class_info]: class_info)
        class_method_info = plus_no_overlap(class_method_info, class_info.methods);

    for(auto& [method,type]: class_method_info)
    {
        std::cerr<<method<<" :: "<<type.print()<<"\n";
    }
    std::cerr<<"\n";

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

    
    typechecker_state state(constr_info);
    global_value_env env0;

    auto [s,env] = state.infer_type_for_decls(env0, M.value_decls);

    for(auto& [x,t]: env)
    {
        std::cerr<<x<<" :: "<<remove_top_level_foralls(alphabetize_type(t))<<"\n";
//        std::cerr<<x<<" = "<<e<<"\n\n\n";
    }
    std::cerr<<"\n";
}


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
  A1. We can process instances before values, but we output instances in the same recursive block as values.

 */
