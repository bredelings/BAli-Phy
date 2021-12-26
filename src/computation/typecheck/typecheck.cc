#include "typecheck.H"

#include <set>

#include <range/v3/all.hpp>

#include "parser/haskell.H"

#include "immer/map.hpp" // for immer::map

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

expression_ref apply_subst(const substitution_t& s, const Haskell::Type& t)
{
    if (auto tv = t.to<Haskell::TypeVar>())
    {
        if (auto t2 = s.find(*tv))
            return *t2;
        else
            return t;
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
    else if (auto p_app = t.to<Haskell::TypeApp>())
    {
        auto app = *p_app;
        app.head = apply_subst(s, app.head);
        app.arg  = apply_subst(s, app.arg);
        return app;
    }
    else if (t.is_a<Haskell::TypeCon>())
        return t;

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
    else if (auto f = t.to<Haskell::ForallType>())
    {
        for(auto& x: f->type_var_binders)
            if (x == tv)
                return false;
        return occurs_check(tv, f->type);
    }
    else if (auto p_app = t.to<Haskell::TypeApp>())
        return occurs_check(tv, p_app->head) or occurs_check(tv, p_app->arg);
    else if (t.is_a<Haskell::TypeCon>())
        return false;
    else
    {
        throw myexception()<<"types do not unify!";
    }
}

// Is there a better way to implement this?
substitution_t unify(const Hs::Type& t1, const Hs::Type& t2)
{
    if (t1.is_a<Haskell::TypeApp>() and t2.is_a<Haskell::TypeApp>())
    {
        auto& app1 = t1.as_<Haskell::TypeApp>();
        auto& app2 = t2.as_<Haskell::TypeApp>();

        auto s1 = unify(app1.head, app2.head);
        auto s2 = unify(apply_subst(s1, app1.arg), apply_subst(s1, app2.arg));
        return compose(s2,s1);
    }
    else if (auto tv1 = t1.to<Haskell::TypeVar>())
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
    else if (t1.is_a<Haskell::TypeCon>() and
             t2.is_a<Haskell::TypeCon>() and
             t1.as_<Haskell::TypeCon>() == t2.as_<Haskell::TypeCon>())
    {
        return {};
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

typedef immer::map<Hs::Var, Hs::Type> env_var_to_type_t;

typedef env_var_to_type_t global_value_env;

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

global_value_env apply_subst(const substitution_t& s, const global_value_env& env1)
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

Hs::Context apply_subst(const substitution_t& s, Hs::Context context)
{
    for(auto& constraint: context.constraints)
        constraint = apply_subst(s, constraint);
    return context;
}

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
    map<string,shared_ptr<Hs::DataOrNewtypeDecl>> con_to_data;

    Hs::DataOrNewtypeDecl instantiate(const Hs::DataOrNewtypeDecl& d);

    pair<Hs::Type, vector<Hs::Type>> lookup_data_from_con_pattern(const expression_ref& pattern);

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

    pair<substitution_t, expression_ref>
    infer_type(const global_value_env& env, const Hs::Type& E);

    pair<substitution_t, global_value_env>
    infer_type_for_decls(const global_value_env& env, const Hs::Binds& E);
};

Hs::DataOrNewtypeDecl typechecker_state::instantiate(const Hs::DataOrNewtypeDecl& d)
{
    substitution_t s;
    for(auto ftv: d.type_vars)
        s = s.insert({ftv,fresh_type_var()});

    return apply_subst(s,d);
}

set<Hs::TypeVar> free_type_variables(const Hs::Type& t);

pair<Hs::Type, vector<Hs::Type>> typechecker_state::lookup_data_from_con_pattern(const expression_ref& pattern)
{
    // 1. Find the data type
    auto& con = pattern.head().as_<Hs::Con>();
    auto& con_name = unloc(con.name);
    if (not con_to_data.count(con_name))
        throw myexception()<<"Unrecognized constructor: "<<con;
    auto d = con_to_data.at(con_name);
    d = std::make_shared<Hs::DataOrNewtypeDecl>(instantiate(*d));

    // 2. Construct the data type for the pattern
    Hs::Type object_type = Hs::TypeCon({noloc,d->name});
    for(auto type_var: d->type_vars)
        object_type = Hs::TypeApp(object_type, type_var);

    // 3a. Get types for the pattern fields
    // 3b. Check that pattern has the correct arity
    auto con_info = *d->find_constructor_by_name(con_name);
    auto pattern_types = con_info.get_field_types();
    if (pattern.size() != pattern_types.size())
        throw myexception()<<"pattern '"<<pattern<<"' has "<<pattern.size()<<" arguments, but constructor has arity '"<<pattern_types.size()<<"'";

    return {object_type, pattern_types};
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

void get_free_type_variables(const expression_ref& E, set<Hs::TypeVar>& free)
{
    std::multiset<Hs::TypeVar> bound;
    get_free_type_variables(E,bound,free);
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
}
