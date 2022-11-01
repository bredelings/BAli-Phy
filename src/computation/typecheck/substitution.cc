#include "substitution.H"
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

    if (d.is_regular_decl())
    {
        for(auto& con: d.get_constructors())
            con = apply_subst(s, con);
    }
    else if (d.is_gadt_decl())
    {
        for(auto& decl: d.get_gadt_constructors())
            unloc(decl.type) = apply_subst(s, unloc(decl.type));
    }

    return d;
}

