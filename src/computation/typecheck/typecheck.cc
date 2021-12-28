#include "typecheck.H"

#include <set>

#include <range/v3/all.hpp>

#include "parser/haskell.H"

#include "immer/map.hpp" // for immer::map

#include "util/set.H"

#include "computation/expression/apply.H"
#include "computation/expression/tuple.H" // for is_tuple_name( )
#include "computation/operation.H" // for is_non_apply_op( )

namespace views = ranges::views;

using std::vector;
using std::string;
using std::map;
using std::set;
using std::pair;
using std::shared_ptr;

typedef immer::map<Hs::TypeVar, Hs::Type> substitution_t;

// TODO
// 1. Merge unification / substitution code for types & kinds?

namespace std
{
    template <>
    class hash < Haskell::Var >{
    public :
        size_t operator()(const Haskell::Var &x) const
        {
            size_t h = std::hash<std::string>()(unloc(x.name));
            if (x.index)
                h = std::hash<int>()(*x.index) ^ h;
            return  h ;
        }
    };

    template <>
    class hash < Haskell::TypeVar >{
    public :
        size_t operator()(const Haskell::TypeVar &x) const
        {
            size_t h = std::hash<std::string>()(unloc(x.name));
            if (x.index)
                h = std::hash<int>()(*x.index) ^ h;
            return  h ;
        }
    };
}

expression_ref apply_subst(const substitution_t& s, const Hs::Type& t);
Hs::Context apply_subst(const substitution_t& s, Hs::Context C)
{
    for(auto& constraint: C.constraints)
        constraint = apply_subst(s, constraint);
    return C;
}

expression_ref apply_subst(const substitution_t& s, const Hs::Type& t)
{
    if (auto tv = t.to<Haskell::TypeVar>())
    {
        if (auto t2 = s.find(*tv))
            return *t2;
        else
            return t;
    }
    else if (t.is_a<Haskell::TypeCon>())
        return t;
    else if (auto tup = t.to<Hs::TupleType>())
    {
        auto T = *tup;
        for(auto& type: T.element_types)
            type = apply_subst(s, type);
        return T;
    }
    else if (auto l = t.to<Hs::ListType>())
    {
        auto L = *l;
        L.element_type = apply_subst(s, L.element_type);
        return L;
    }
    else if (auto p_app = t.to<Haskell::TypeApp>())
    {
        auto app = *p_app;
        app.head = apply_subst(s, app.head);
        app.arg  = apply_subst(s, app.arg);
        return app;
    }
    else if (auto p_forall = t.to<Haskell::ForallType>())
    {
        auto forall = *p_forall;

        auto s2 = s;
        for(auto& tv: forall.type_var_binders)
            s2 = s2.erase(tv);

        forall.type =  apply_subst(s2, forall.type);
        return forall;
    }
    else if (auto c = t.to<Hs::ConstrainedType>())
    {
        auto C = *c;
        C.context = apply_subst(s, C.context);
        C.type = apply_subst(s, C.type);
        return C;
    }
    else if (auto sl = t.to<Hs::StrictLazyType>())
    {
        auto SL = *sl;
        SL.type = apply_subst(s, SL.type);
        return SL;
    }
    else
        std::abort();
}

// This should yield a substitution that is equivalent to apply FIRST s1 and THEN s2,
// like f . g
substitution_t compose(substitution_t s2, substitution_t s1)
{
    if (s2.empty()) return s1;

    auto s3 = s2;
    for(auto& [tv,e]: s1)
        s3 = s3.insert({tv,apply_subst(s2,e)});
    return s3;
}

bool occurs_check(const Haskell::TypeVar& tv, const Hs::Type& t)
{
    if (auto x = t.to<Haskell::TypeVar>())
        return tv == *x;
    else if (t.is_a<Haskell::TypeCon>())
        return false;
    else if (auto tup = t.to<Hs::TupleType>())
    {
        for(auto& type: tup->element_types)
            if (occurs_check(tv, type))
                return true;
        return false;
    }
    else if (auto l = t.to<Hs::ListType>())
        return occurs_check(tv, l->element_type);
    else if (auto p_app = t.to<Haskell::TypeApp>())
        return occurs_check(tv, p_app->head) or occurs_check(tv, p_app->arg);
    else if (auto f = t.to<Haskell::ForallType>())
    {
        for(auto& x: f->type_var_binders)
            if (x == tv)
                return false;
        return occurs_check(tv, f->type);
    }
    else if (auto c = t.to<Hs::ConstrainedType>())
    {
        // The context may not contain vars that don't occur in the head;
        return occurs_check(tv, c->type);
    }
    else if (auto sl = t.to<Hs::StrictLazyType>())
        return occurs_check(tv, sl->type);
    else
        std::abort();
}

// Is there a better way to implement this?
substitution_t unify(const Hs::Type& t1, const Hs::Type& t2)
{
    if (auto tv1 = t1.to<Haskell::TypeVar>())
    {
        substitution_t s;
        if (t1 == t2)
            return s;
        if (occurs_check(*tv1, t2))
            throw myexception()<<"Occurs check: cannot construct infinite type: "<<*tv1<<" ~ "<<t2<<"\n";
        return s.insert({*tv1, t2});
    }
    else if (auto tv2 = t2.to<Haskell::TypeVar>())
    {
        substitution_t s;
        if (t1 == t2)
            return s;
        if (occurs_check(*tv2,t1))
            throw myexception()<<"Occurs check: cannot construct infinite type: "<<*tv2<<" ~ "<<t1<<"\n";
        return s.insert({*tv2, t1});
    }
    else if (t1.is_a<Haskell::TypeApp>() and t2.is_a<Haskell::TypeApp>())
    {
        auto& app1 = t1.as_<Haskell::TypeApp>();
        auto& app2 = t2.as_<Haskell::TypeApp>();

        auto s1 = unify(app1.head, app2.head);
        auto s2 = unify(apply_subst(s1, app1.arg), apply_subst(s1, app2.arg));
        return compose(s2,s1);
    }
    else if (t1.is_a<Haskell::TypeCon>() and
             t2.is_a<Haskell::TypeCon>() and
             t1.as_<Haskell::TypeCon>() == t2.as_<Haskell::TypeCon>())
    {
        return {};
    }
    else if (t1.is_a<Hs::TupleType>() and t2.is_a<Hs::TupleType>())
    {
        auto& tup1 = t1.as_<Hs::TupleType>();
        auto& tup2 = t2.as_<Hs::TupleType>();
        if (tup1.element_types.size() != tup2.element_types.size())
            throw myexception()<<"types do not unify!";

        substitution_t s;
        for(int i=0;i<tup1.element_types.size();i++)
        {
            auto si = unify(tup1.element_types[i], tup2.element_types[i]);
            s = compose(si, s);
        }
        return s;
    }
    else if (t1.is_a<Hs::ListType>() and t2.is_a<Hs::ListType>())
    {
        auto& L1 = t1.as_<Hs::ListType>();
        auto& L2 = t2.as_<Hs::ListType>();
        return unify(L1.element_type, L2.element_type);
    }
    else if (t1.is_a<Hs::ConstrainedType>() or t2.is_a<Hs::ConstrainedType>())
    {
        throw myexception()<<"unify "<<t1.print()<<" "<<t2.print()<<": How should we handle unification for constrained types?";
    }
    else if (t1.is_a<Hs::StrictLazyType>() or t2.is_a<Hs::StrictLazyType>())
    {
        throw myexception()<<"unify "<<t1.print()<<" "<<t2.print()<<": How should we handle unification for strict/lazy types?";
    }
    else
    {
        throw myexception()<<"types do not unify!";
    }
}


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

struct type_con_info
{
    // We want the kind info for the typecon.
    int arity;
    Hs::Type operator() (const vector<Hs::Type>& args) const;
// -- for type synonmys, we need the means to apply the constructor to (exactly) k arguments, for arity k.
// -- for data / newtypes, we need to means to apply up to k arguments.
// -- perhaps we need to store the KIND, and not just the arity?
};

typedef immer::map<Hs::Var, Hs::Type> value_env;

string print(const value_env& env)
{
    std::ostringstream oss;
    for(auto& [var,type]: env)
    {
        oss<<var.print()<<" :: "<<type.print()<<"\n";
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

typedef map<string, type_con_info> type_con_env;

struct class_info
{
    string name;
    string emitted_name;
    vector<Hs::TypeVar> type_vars;

    // Maybe change this to vector<pair<Type,string>>, 
    // FIXME: Should we record here the names of functions to extract 
    Hs::Context context;

    global_value_env methods;
};

typedef map<string, class_info> class_env;

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

// The GIE maps classes to a list of instances for them.
// Each instance corresponds to a dictionary function (dfun) with NO free type variables.
// For example:
//   instance Eq a => Eq [a] where
// leads to
//   dEqList :: forall a. Eq a => Eq [a]
typedef map<string, vector<instance_info>> global_instance_env;

// The LIE maps local dictionary variables to the constraint for which they are a dictionary.
// It DOES allow free type variables.
typedef map<Hs::Var, Hs::Type> local_instance_env;

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

Hs::Constructor apply_subst(const substitution_t& s, Hs::Constructor con)
{
    if (con.context)
        con.context = apply_subst(s, *con.context);

    if (con.fields.index() == 0)
    {
        for(auto& t: std::get<0>(con.fields))
                t = apply_subst(s, t);
    }
    else
    {
        for(auto& f: std::get<1>(con.fields).field_decls)
            f.type = apply_subst(s, f.type);
    }
    return con;
}

Hs::DataOrNewtypeDecl apply_subst(const substitution_t& s, Hs::DataOrNewtypeDecl d)
{
    d.context = apply_subst(s, d.context);
    for(auto& con: d.constructors)
        con = apply_subst(s, con);

    return d;
}

struct typechecker_state
{
    Hs::Type bool_type() const { return Hs::TypeCon({noloc,"Data.Bool.True"}); }

    Hs::Type num_type() const { return Hs::TypeCon({noloc,"Num#"}); }

    Hs::Type char_type() const { return Hs::TypeCon({noloc,"Char#"}); }

    map<string,shared_ptr<Hs::DataOrNewtypeDecl>> con_to_data;

    Hs::DataOrNewtypeDecl instantiate(const Hs::DataOrNewtypeDecl& d);

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

    void add_data_type(const shared_ptr<Hs::DataOrNewtypeDecl>& d)
    {
        for(auto& con: d->constructors)
            con_to_data[con.name] = d;
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

Hs::DataOrNewtypeDecl typechecker_state::instantiate(const Hs::DataOrNewtypeDecl& d)
{
    substitution_t s;
    for(auto ftv: d.type_vars)
        s = s.insert({ftv,fresh_type_var()});

    return apply_subst(s,d);
}

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
        int n = con_name.size()-1;
        vector<Hs::Type> types;
        for(int i=0;i<n;i++)
            types.push_back(fresh_type_var());
        return {Hs::TupleType(types),types};
    }

    // 1. Find the data type
    if (not con_to_data.count(con_name))
        throw myexception()<<"Unrecognized constructor: "<<con;
    auto d = instantiate(*con_to_data.at(con_name));

    // 2. Construct the data type for the pattern
    Hs::Type object_type = Hs::TypeCon({noloc,d.name});
    for(auto type_var: d.type_vars)
        object_type = Hs::TypeApp(object_type, type_var);

    // 3a. Get types for the pattern fields
    // 3b. Check that pattern has the correct arity
    auto con_info = *d.find_constructor_by_name(con_name);
    auto field_types = con_info.get_field_types();

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
            s = s.insert({tv,fresh_type_var()});
        t2 = fa->type;
    }
    return apply_subst(s,t2);
}

pair<substitution_t, global_value_env>
typechecker_state::infer_type_for_decls(const global_value_env& env, const Hs::Binds& binds)
{
    substitution_t s;
    auto env2 = env;
    for(auto& decls: binds)
    {
        auto [s1, gve1] = infer_type_for_decls(env2, decls);
        env2 = plus_no_overlap(env2, gve1);
        s = compose(s1, s);
    }
    env2 = apply_subst(s, env2);
    return {s, env2};
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
            lve = lve.insert({fd->v,type});
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
            auto lhs_type = env2.at(fd->v);
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
        lve = lve.insert({*x, type});
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
        lve = lve.insert({ap->var.as_<Hs::Var>(), t});
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
    }
    result_type = apply_subst(s, result_type);

    return {s,result_type};
}

pair<substitution_t,Hs::Type>
typechecker_state::infer_type(const global_value_env& env, const expression_ref& E)
{
    if (auto x = E.to<Hs::Var>())
    {
        auto sigma = env.find(*x);

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
        auto [s_body, t_body] = infer_type(env_decls, unloc(let->body));

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
    else
        throw myexception()<<"type check expression: I don't recognize expression '"<<E<<"'";
}

Hs::Type remove_top_level_foralls(Hs::Type t)
{
    while(auto fa = t.to<Hs::ForallType>())
        t = fa->type;
    return t;
}

void typecheck(const Hs::ModuleDecls& M)
{
    // 1. Check the module's type declarations, and derives a Type Environment TE_T:(TCE_T, CVE_T)
    //    OK, so datatypes produce a
    //    * Type Constructor Environment (TCE) = tycon 439-> arity, method of applying the tycon
    //    * Constructor Value Environment (CVE)
    //
    // 2. Check the module's class declarations, produce some translated bindings -> binds_C ( GVE_C, CE_C, GIE_C )
    //
    // 3. E' = (TCE_T, (CVE_T, GVE_C, {}), CE_C, (GIE_C,{}))
    //
    // 4. Check the module's instance declarations -> monobinds : GIE_I
    //    These are mutually recursive with the value declarations. ?!?
    //
    // 5. Check the module's value declarations.


    // OK, so it looks like the kind-checking pass actually handles the first two steps!
    // It needs to return
    // TCE_T = type con info, part1
    // CVE_T = constructor types
    // GVE_C = method -> type map
    // CE_C  = class name -> class info
    // GIE_C = functions to extract sub-dictionaries from containing dictionaries?

    //

    typechecker_state state;
    global_value_env env0;

    auto [s,env] = state.infer_type_for_decls(env0, M.value_decls);

    for(auto& [x,t]: env)
    {
        std::cerr<<x<<" :: "<<remove_top_level_foralls(alphabetize_type(t))<<"\n";
//        std::cerr<<x<<" = "<<e<<"\n\n\n";
    }
}
