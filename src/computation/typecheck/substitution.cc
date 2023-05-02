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

optional<Context> check_apply_subst(const substitution_t& s, Context C)
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

optional<Context> check_apply_subst(const usubstitution_t& s, Context C)
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

optional<Context> check_apply_subst(const bsubstitution_t& s, Context C)
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

std::optional<Type> check_apply_subst(const substitution_t& s, const Type& t)
{
    if (s.empty()) return {};

    if (auto tt = filled_meta_type_var(t))
        return check_apply_subst(s,  *tt);
    else if (t.is_a<MetaTypeVar>())
        return {};
    else if (auto tv = t.to<TypeVar>())
    {
        if (auto t2 = s.find(*tv))
            return apply_subst(s,*t2);
        else
            return {};
    }
    else if (t.is_a<TypeCon>())
        return {};
    else if (auto p_app = t.to<TypeApp>())
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
    else if (auto forall = t.to<ForallType>())
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
    else if (auto c = t.to<ConstrainedType>())
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
    else if (auto st = t.to<StrictType>())
    {
        if (auto maybe_type = check_apply_subst(s, st->type))
        {
            auto ST = *st;
            ST.type = *maybe_type;
            return ST;
        }
        else
            return {};
    }
    else if (auto lt = t.to<LazyType>())
    {
        if (auto maybe_type = check_apply_subst(s, lt->type))
        {
            auto LT = *lt;
            LT.type = *maybe_type;
            return LT;
        }
        else
            return {};
    }
    else
        std::abort();
}

Context apply_subst(const substitution_t& s, const Context& C)
{
    if (auto c = check_apply_subst(s,C))
        return *c;
    else
        return C;
}

Type apply_subst(const substitution_t& s, const Type& t)
{
    if (auto T = check_apply_subst(s, t))
        return *T;
    else
        return t;
}

vector<Type> apply_subst(const substitution_t& s, const vector<Type>& t)
{
    vector<Type> t2 = t;
    for(auto& type: t2)
        type = apply_subst(s, type);
    return t2;
}

Type apply_subst(const usubstitution_t& s, const Type& t)
{
    if (auto T = check_apply_subst(s, t))
        return *T;
    else
        return t;
}

vector<Type> apply_subst(const bsubstitution_t& s, const vector<Type>& t)
{
    vector<Type> t2 = t;
    for(auto& type: t2)
        type = apply_subst(s, type);
    return t2;
}

Type apply_subst(const bsubstitution_t& s, const Type& t)
{
    if (auto T = check_apply_subst(s, t))
        return *T;
    else
        return t;
}

std::optional<Type> check_apply_subst(const usubstitution_t& s, const Type& t)
{
    if (s.empty()) return {};

    if (auto tt = filled_meta_type_var(t))
        return check_apply_subst(s,  *tt);
    else if (auto tv = t.to<MetaTypeVar>())
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
    else if (t.is_a<TypeVar>())
        return {};
    else if (t.is_a<TypeCon>())
        return {};
    else if (auto p_app = t.to<TypeApp>())
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
    else if (auto forall = t.to<ForallType>())
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
    else if (auto c = t.to<ConstrainedType>())
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
    else if (auto st = t.to<StrictType>())
    {
        if (auto maybe_type = check_apply_subst(s, st->type))
        {
            auto ST = *st;
            ST.type = *maybe_type;
            return ST;
        }
        else
            return {};
    }
    else if (auto lt = t.to<LazyType>())
    {
        if (auto maybe_type = check_apply_subst(s, lt->type))
        {
            auto LT = *lt;
            LT.type = *maybe_type;
            return LT;
        }
        else
            return {};
    }
    else
        std::abort();
}

std::optional<Type> check_apply_subst(const bsubstitution_t& s, const Type& t)
{
    if (s.empty()) return {};

    if (auto tt = filled_meta_type_var(t))
        return check_apply_subst(s,  *tt);
    else if (auto mtv = t.to<MetaTypeVar>())
    {
        if (auto t2 = s.find(*mtv))
        {
            if (mtv->filled())
                throw myexception()<<"Trying to substitute for filled unification variable "<<unloc(mtv->name);
            return apply_subst(s,*t2);
        }
        else
            return {};
    }
    else if (auto tv = t.to<TypeVar>())
    {
        if (auto t2 = s.find(*tv))
            return apply_subst(s,*t2);
        else
            return {};
    }
    else if (t.is_a<TypeCon>())
        return {};
    else if (auto p_app = t.to<TypeApp>())
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
    else if (auto forall = t.to<ForallType>())
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
    else if (auto c = t.to<ConstrainedType>())
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
    else if (auto st = t.to<StrictType>())
    {
        if (auto maybe_type = check_apply_subst(s, st->type))
        {
            auto ST = *st;
            ST.type = *maybe_type;
            return ST;
        }
        else
            return {};
    }
    else if (auto lt = t.to<LazyType>())
    {
        if (auto maybe_type = check_apply_subst(s, lt->type))
        {
            auto LT = *lt;
            LT.type = *maybe_type;
            return LT;
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

