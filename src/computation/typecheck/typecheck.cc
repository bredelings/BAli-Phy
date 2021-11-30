#include "typecheck.H"
#include <range/v3/all.hpp>
#include "parser/haskell.H"

#include "immer/map.hpp" // for immer::map

namespace views = ranges::views;

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
                size_t h = std::hash<int>()(*x.index) ^ h;
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
                size_t h = std::hash<int>()(*x.index) ^ h;
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


// What is the output of the kind checker?
// 
