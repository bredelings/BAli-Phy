#include "typecheck.H"
#include <range/v3/all.hpp>
#include "parser/haskell.H"

#include "immer/map.hpp" // for immer::map

namespace views = ranges::views;

using std::vector;
using std::string;
using std::map;

typedef immer::map<Haskell::TypeVar,expression_ref> substitution_t;

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

bool occurs_check(const Haskell::TypeVar& tv, const expression_ref& t)
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
substitution_t unify(const expression_ref& t1, const expression_ref& t2)
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


typedef Haskell::Type monotype;
typedef Haskell::Type overtype;
typedef Haskell::Type polytype;

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
// CVE = con -> polytype
// GVE = var -> polytype
// LVE = var -> monotype


typedef immer::map<Haskell::Var, Haskell::Type> env_var_to_type_t;

env_var_to_type_t apply_subst(const substitution_t& s, const env_var_to_type_t& env1)
{
    env_var_to_type_t env2;
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

// What is the output of the kind checker?
// 

struct MRule
{
    std::vector<Haskell::Pattern> pats;
    Haskell::MultiGuardedRHS rhs;
};

struct Match
{
    std::vector<MRule> rules;
};

struct FunctionDecl
{
    std::string name;
    // See desugar-case.cc for translating matches
    Match match;
};

struct PatDecl
{
    Haskell::Pattern pat;
    Haskell::MultiGuardedRHS rhs;
};

struct BindingGroup
{
    std::vector<FunctionDecl> function_decls;
    std::vector<PatDecl> pat_decls;
    // Can these apply to variables in pattern decls?
    std::map<std::string, Haskell::Type> signatures;
};

struct TIModule
{
    // module name?
    // exports?
    // imports?

    std::vector<Haskell::ClassDecl> class_decls;
    std::vector<Haskell::TypeSynonymDecl> type_decls;
    std::vector<Haskell::DataOrNewtypeDecl> data_decls;
    std::vector<Haskell::InstanceDecl> instance_decls;

    std::optional<Haskell::DefaultDecl> default_decl;

    // The typechecker doesn't care about fixities...
    std::vector<Haskell::FixityDecl> fixities;

    // I guess the renamer is in charge of turning things into binding groups?
    std::vector<BindingGroup> binds;
};

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
