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

optional<Hs::Context> check_apply_subst(const substitution_t& s, Hs::Context C)
{
    bool changed = false;
    for(auto& constraint: C.constraints)
    {
        if (auto maybe_constraint = check_apply_subst(s, constraint))
        {
            constraint = *maybe_constraint;
            changed = true;
        }
    }
    if (changed)
        return C;
    else
        return {};
}

optional<Hs::Context> check_apply_subst(const usubstitution_t& s, Hs::Context C)
{
    bool changed = false;
    for(auto& constraint: C.constraints)
    {
        if (auto maybe_constraint = check_apply_subst(s, constraint))
        {
            constraint = *maybe_constraint;
            changed = true;
        }
    }
    if (changed)
        return C;
    else
        return {};
}

std::optional<Hs::Type> check_apply_subst(const substitution_t& s, const Hs::Type& t)
{
    if (s.empty()) return {};

    if (auto tt = filled_meta_type_var(t))
        return check_apply_subst(s,  *tt);
    else if (t.is_a<Hs::MetaTypeVar>())
        return {};
    else if (auto tv = t.to<Hs::TypeVar>())
    {
        if (auto t2 = s.find(*tv))
            return apply_subst(s,*t2);
        else
            return {};
    }
    else if (t.is_a<Hs::TypeCon>())
        return {};
    else if (auto tup = t.to<Hs::TupleType>())
    {
        auto T = *tup;
        bool changed = false;
        for(auto& type: T.element_types)
        {
            if (auto maybe_type = check_apply_subst(s,  type))
            {
                type = *maybe_type;
                changed = true;
            }
        }

        if (changed)
            return T;
        else
            return {};
    }
    else if (auto l = t.to<Hs::ListType>())
    {
        if (auto maybe_element_type = check_apply_subst(s,  l->element_type))
        {
            auto L = *l;
            L.element_type = *maybe_element_type;
            return L;
        }
        else
            return {};
    }
    else if (auto p_app = t.to<Hs::TypeApp>())
    {
        auto app = *p_app;
        bool changed = false;
        if (auto maybe_head = check_apply_subst(s, app.head))
        {
            app.head = *maybe_head;
            changed = true;
        }
        if (auto maybe_arg = check_apply_subst(s, app.arg))
        {
            app.arg = *maybe_arg;
            changed = true;
        }

        if (changed)
            return app;
        else
            return {};
    }
    else if (auto forall = t.to<Hs::ForallType>())
    {
        auto s2 = s;
        for(auto& tv: forall->type_var_binders)
            s2 = s2.erase(tv);

        if (auto maybe_type = check_apply_subst(s2, forall->type))
        {
            auto Forall = *forall;
            Forall.type = *maybe_type;
            return Forall;
        }
        else
            return {};
    }
    else if (auto c = t.to<Hs::ConstrainedType>())
    {
        auto C = *c;
        bool changed = false;
        if (auto maybe_context = check_apply_subst(s, c->context))
        {
            C.context = *maybe_context;
            changed = true;
        }
        if (auto maybe_type = check_apply_subst(s, c->type))
        {
            C.type = *maybe_type;
            changed = true;
        }
        if (changed)
            return C;
        else
            return {};
    }
    else if (auto sl = t.to<Hs::StrictLazyType>())
    {
        if (auto maybe_type = check_apply_subst(s, sl->type))
        {
            auto SL = *sl;
            SL.type = *maybe_type;
            return SL;
        }
        else
            return {};
    }
    else
        std::abort();
}

Hs::Context apply_subst(const substitution_t& s, const Hs::Context& C)
{
    if (auto c = check_apply_subst(s,C))
        return *c;
    else
        return C;
}

Hs::Type apply_subst(const substitution_t& s, const Hs::Type& t)
{
    if (auto T = check_apply_subst(s, t))
        return *T;
    else
        return t;
}

vector<Hs::Type> apply_subst(const substitution_t& s, const vector<Hs::Type>& t)
{
    vector<Hs::Type> t2 = t;
    for(auto& type: t2)
        type = apply_subst(s, type);
    return t2;
}

Hs::Type apply_subst(const usubstitution_t& s, const Hs::Type& t)
{
    if (auto T = check_apply_subst(s, t))
        return *T;
    else
        return t;
}

std::optional<Hs::Type> check_apply_subst(const usubstitution_t& s, const Hs::Type& t)
{
    if (s.empty()) return {};

    if (auto tt = filled_meta_type_var(t))
        return check_apply_subst(s,  *tt);
    else if (auto tv = t.to<Hs::MetaTypeVar>())
    {
        if (auto t2 = s.find(*tv))
        {
            if (tv->filled())
                throw myexception()<<"Trying to substitution for filled unification variable "<<unloc(tv->name);
            return apply_subst(s,*t2);
        }
        else
            return {};
    }
    else if (t.is_a<Hs::TypeVar>())
        return {};
    else if (t.is_a<Hs::TypeCon>())
        return {};
    else if (auto tup = t.to<Hs::TupleType>())
    {
        auto T = *tup;
        bool changed = false;
        for(auto& type: T.element_types)
        {
            if (auto maybe_type = check_apply_subst(s,  type))
            {
                type = *maybe_type;
                changed = true;
            }
        }

        if (changed)
            return T;
        else
            return {};
    }
    else if (auto l = t.to<Hs::ListType>())
    {
        if (auto maybe_element_type = check_apply_subst(s,  l->element_type))
        {
            auto L = *l;
            L.element_type = *maybe_element_type;
            return L;
        }
        else
            return {};
    }
    else if (auto p_app = t.to<Hs::TypeApp>())
    {
        auto app = *p_app;
        bool changed = false;
        if (auto maybe_head = check_apply_subst(s, app.head))
        {
            app.head = *maybe_head;
            changed = true;
        }
        if (auto maybe_arg = check_apply_subst(s, app.arg))
        {
            app.arg = *maybe_arg;
            changed = true;
        }

        if (changed)
            return app;
        else
            return {};
    }
    else if (auto forall = t.to<Hs::ForallType>())
    {
        if (auto maybe_type = check_apply_subst(s, forall->type))
        {
            auto Forall = *forall;
            Forall.type = *maybe_type;
            return Forall;
        }
        else
            return {};
    }
    else if (auto c = t.to<Hs::ConstrainedType>())
    {
        auto C = *c;
        bool changed = false;
        if (auto maybe_context = check_apply_subst(s, c->context))
        {
            C.context = *maybe_context;
            changed = true;
        }
        if (auto maybe_type = check_apply_subst(s, c->type))
        {
            C.type = *maybe_type;
            changed = true;
        }
        if (changed)
            return C;
        else
            return {};
    }
    else if (auto sl = t.to<Hs::StrictLazyType>())
    {
        if (auto maybe_type = check_apply_subst(s, sl->type))
        {
            auto SL = *sl;
            SL.type = *maybe_type;
            return SL;
        }
        else
            return {};
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

bool occurs_check(const Hs::TypeVar& tv, const Hs::Type& t)
{
    if (auto tt = filled_meta_type_var(t))
        return occurs_check(tv, *tt);
    else if (t.is_a<Hs::MetaTypeVar>())
        return false;
    else if (auto x = t.to<Hs::TypeVar>())
        return tv == *x;
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
    {
        for(auto & qv: f->type_var_binders)
            if (qv == tv)
                return false;

        return occurs_check(tv, f->type);
    }
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


Hs::ConstructorDecl apply_subst(const substitution_t& s, Hs::ConstructorDecl con)
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

