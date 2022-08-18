#include "unify.H"
#include <utility>
#include "immer/set.hpp"

#include "computation/haskell/ids.H"
#include "kind.H"

using std::vector;
using std::string;
using std::optional;

string print(const substitution_t& s)
{
    std::ostringstream oss;
    for(auto& [var,type]: s)
    {
        oss<<var.print()<<" :: "<<type.print()<<"\n";
    }
    return oss.str();
}

Hs::Context apply_subst(const substitution_t& s, Hs::Context C)
{
    for(auto& constraint: C.constraints)
        constraint = apply_subst(s, constraint);
    return C;
}

Hs::Type apply_subst(const substitution_t& s, const Hs::Type& t)
{
    if (s.empty()) return t;

    else if (auto tt = filled_meta_type_var(t))
        return apply_subst(s, *tt);
    else if (t.is_a<Hs::MetaTypeVar>())
        return t;
    else if (auto tv = t.to<Hs::TypeVar>())
    {
        if (auto t2 = s.find(*tv))
            return apply_subst(s,*t2);
        else
            return t;
    }
    else if (t.is_a<Hs::TypeCon>())
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
    else if (auto p_app = t.to<Hs::TypeApp>())
    {
        auto app = *p_app;
        app.head = apply_subst(s, app.head);
        app.arg  = apply_subst(s, app.arg);
        return app;
    }
    else if (auto p_forall = t.to<Hs::ForallType>())
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

bool occurs_check(const Hs::MetaTypeVar& tv, const Hs::Type& t)
{
    assert(not tv.filled());

    if (auto tt = filled_meta_type_var(t))
        return occurs_check(tv, *tt);
    else if (auto x = t.to<Hs::MetaTypeVar>())
        return tv == *x;
    else if (t.is_a<Hs::TypeVar>())
        return false;
    else if (t.is_a<Hs::TypeCon>())
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
    else if (auto p_app = t.to<Hs::TypeApp>())
        return occurs_check(tv, p_app->head) or occurs_check(tv, p_app->arg);
    else if (auto f = t.to<Hs::ForallType>())
        return occurs_check(tv, f->type);
    else if (auto c = t.to<Hs::ConstrainedType>())
    {
        // The context may not contain vars that don't occur in the head;
        for(auto& constraint: c->context.constraints)
            if (occurs_check(tv, constraint))
                return true;

        return occurs_check(tv, c->type);
    }
    else if (auto sl = t.to<Hs::StrictLazyType>())
        return occurs_check(tv, sl->type);
    else
        std::abort();
}

bool try_insert(const Hs::MetaTypeVar& tv, Hs::Type type)
{
    // 1. We can't insert tv ~ type if we already have a substitution for tv.
    assert(not tv.filled());

    // 2. We can only bind meta type vars to tau types.
    assert(Hs::is_tau_type(type));

    // 3. Walk any meta-type-var indirections
    auto safe_type = type;
    while(auto t2 = filled_meta_type_var(safe_type))
        safe_type = *t2;
    
    // 4. tv ~ tv is already true, so in that case return success without doing anything.
    if (auto tv2 = safe_type.to<Hs::MetaTypeVar>(); tv2 and *tv2 == tv)
        return true;

    // 5. If safe_type contains tv, then we have a substitution loop for tv.
    //    Therefore return failure.  (This rules out infinite types.)
    if (occurs_check(tv, safe_type)) return false;

    // 6. It is safe to add tv -> safe_type
    tv.fill(type);

    return true;
}

Hs::TypeCon tuple_tycon(int n)
{
    auto kind = make_n_args_kind(n);
    return Hs::TypeCon( {noloc, tuple_name(n)}, kind );
}

Hs::TypeCon list_tycon()
{
    auto kind = make_n_args_kind(1);
    return Hs::TypeCon( {noloc,"[]"}, kind );
}

Hs::Type canonicalize_type(const Hs::TupleType& type1)
{
    int n = type1.element_types.size();
    Hs::Type type2 = tuple_tycon(n);
    return Hs::make_tyapps(type2, type1.element_types);
}

Hs::Type canonicalize_type(const Hs::ListType& type1)
{
    Hs::Type type2 = list_tycon();
    return Hs::TypeApp(type2, type1.element_type);
}

bool same_type(const Hs::Type& t1, const Hs::Type& t2)
{
    if (auto type1 = filled_meta_type_var(t1))
        return same_type(*type1, t2);
    else if (auto type2 = filled_meta_type_var(t2))
        return same_type(t1, *type2);
    else if (t1.is_a<Hs::MetaTypeVar>())
        return (t1 == t2);
    else if (t1.is_a<Hs::TypeVar>())
        return (t1 == t2);
    else if (t1.is_a<Hs::TypeCon>())
        return (t1 == t2);
    else if (t1.is_a<Hs::TypeApp>() and t2.is_a<Hs::TypeApp>())
    {
        auto& app1 = t1.as_<Hs::TypeApp>();
        auto& app2 = t2.as_<Hs::TypeApp>();

        return same_type(app1.head, app2.head) and same_type(app1.arg, app2.arg);
    }
    else if (t1.is_a<Hs::TupleType>() and t2.is_a<Hs::TupleType>())
    {
        auto& tup1 = t1.as_<Hs::TupleType>();
        auto& tup2 = t2.as_<Hs::TupleType>();
        if (tup1.element_types.size() != tup2.element_types.size())
            return false;

        for(int i=0;i<tup1.element_types.size();i++)
            if (not same_type(tup1.element_types[i], tup2.element_types[i])) return false;

        return true;
    }
    else if (t1.is_a<Hs::ListType>() and t2.is_a<Hs::ListType>())
    {
        auto& L1 = t1.as_<Hs::ListType>();
        auto& L2 = t2.as_<Hs::ListType>();

        return same_type(L1.element_type, L2.element_type);
    }
    else if (t1.is_a<Hs::ForallType>() or t2.is_a<Hs::ForallType>())
    {
        throw myexception()<<"same_type "<<t1.print()<<" "<<t2.print()<<": How should we handle forall types?";
    }
    else if (t1.is_a<Hs::ConstrainedType>() or t2.is_a<Hs::ConstrainedType>())
    {
        throw myexception()<<"same_type "<<t1.print()<<" "<<t2.print()<<": How should we handle unification for constrained types?";
    }
    else if (t1.is_a<Hs::StrictLazyType>() or t2.is_a<Hs::StrictLazyType>())
    {
        throw myexception()<<"same_type "<<t1.print()<<" "<<t2.print()<<": How should we handle unification for strict/lazy types?";
    }
    else
        return false;
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

