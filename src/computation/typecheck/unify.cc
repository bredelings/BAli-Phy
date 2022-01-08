#include "unify.H"

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

// This should yield a substitution that is equivalent to apply FIRST s1 and THEN s2,
// like f . g
std::optional<substitution_t> combine(substitution_t s1, substitution_t s2)
{
    if (s2.empty()) return s1;

    auto s3 = s2;
    for(auto& [tv,e]: s1)
    {
        auto it = s3.find(tv);
        if (not it)
        {
            s3 = s3.insert({tv,apply_subst(s2,e)});
        }
        else
        {
            try {
                auto s4 = unify(*it, e);
                auto s5 = combine(s3, s4);
                if (s5)
                    s3 = *s5;
            }
            catch(...)
            {
                return {};
            }
        }
    }
            
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
            auto si = unify(apply_subst(s,tup1.element_types[i]), apply_subst(s,tup2.element_types[i]));
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

optional<substitution_t> match(const Hs::Type& t1, const Hs::Type& t2)
{
    if (auto tv1 = t1.to<Haskell::TypeVar>())
    {
        substitution_t s;
        if (t1 == t2)
            return s;
        if (occurs_check(*tv1, t2))
            return {}; // throw myexception()<<"Occurs check: cannot construct infinite type: "<<*tv1<<" ~ "<<t2<<"\n";
        return s.insert({*tv1, t2});
    }
    else if (t1.is_a<Haskell::TypeApp>() and t2.is_a<Haskell::TypeApp>())
    {
        auto& app1 = t1.as_<Haskell::TypeApp>();
        auto& app2 = t2.as_<Haskell::TypeApp>();

        auto s1 = match(app1.head, app2.head);
        if (not s1) return {};
        auto s2 = match(apply_subst(*s1, app1.arg), app2.arg);
        if (not s2) return {};
        return compose(*s2, *s1);
    }
    else if (t1.is_a<Haskell::TypeCon>() and
             t2.is_a<Haskell::TypeCon>() and
             t1.as_<Haskell::TypeCon>() == t2.as_<Haskell::TypeCon>())
    {
        substitution_t s;
        return s;
    }
    else if (t1.is_a<Hs::TupleType>() and t2.is_a<Hs::TupleType>())
    {
        substitution_t s;

        auto& tup1 = t1.as_<Hs::TupleType>();
        auto& tup2 = t2.as_<Hs::TupleType>();
        if (tup1.element_types.size() != tup2.element_types.size())
            return s;

        for(int i=0;i<tup1.element_types.size();i++)
        {
            auto si = match( apply_subst(s, tup1.element_types[i]), tup2.element_types[i]);
            if (not si) return {};
            s = compose(*si, s);
        }
        return s;
    }
    else if (t1.is_a<Hs::ListType>() and t2.is_a<Hs::ListType>())
    {
        auto& L1 = t1.as_<Hs::ListType>();
        auto& L2 = t2.as_<Hs::ListType>();
        return match(L1.element_type, L2.element_type);
    }
    else if (t1.is_a<Hs::ForallType>() or t2.is_a<Hs::ForallType>())
    {
        throw myexception()<<"match "<<t1.print()<<" "<<t2.print()<<": How should we handle forall types?";
    }
    else if (t1.is_a<Hs::ConstrainedType>() or t2.is_a<Hs::ConstrainedType>())
    {
        throw myexception()<<"match "<<t1.print()<<" "<<t2.print()<<": How should we handle unification for constrained types?";
    }
    else if (t1.is_a<Hs::StrictLazyType>() or t2.is_a<Hs::StrictLazyType>())
    {
        throw myexception()<<"match "<<t1.print()<<" "<<t2.print()<<": How should we handle unification for strict/lazy types?";
    }
    else
        return {};
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

