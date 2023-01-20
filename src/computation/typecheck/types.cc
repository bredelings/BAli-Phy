#include "types.H"
#include "util/set.H"   // for add( , )
#include "haskell/ids.H"

#include "computation/expression/tuple.H"

using std::set;
using std::optional;
using std::pair;
using std::vector;
using std::string;

// Q: how/when do we rename default method definitions?

set<MetaTypeVar> free_meta_type_variables(const Context& context)
{
    return free_meta_type_variables(context.constraints);
}

set<MetaTypeVar> free_meta_type_variables(const vector<Type>& types)
{
    set<MetaTypeVar> tvars;
    for(auto& type: types)
        add(tvars, free_meta_type_variables(type));
    return tvars;
}

set<MetaTypeVar> free_meta_type_variables(const Type& type)
{
    set<MetaTypeVar> tvars;
    if (type.is_a<TypeCon>())
    { }
    else if (type.is_a<TypeVar>())
    { }
    else if (auto t = filled_meta_type_var(type))
    {
        return free_meta_type_variables(*t);
    }
    else if (type.is_a<MetaTypeVar>())
    {
        auto& tv = type.as_<MetaTypeVar>();
        assert(not tv.filled());
        auto& name = unloc(tv.name);
        assert(name.size());
        tvars.insert(tv);
    }
    else if (type.is_a<TypeApp>())
    {
        auto& app = type.as_<TypeApp>();
        add(tvars, free_meta_type_variables(app.head));
        add(tvars, free_meta_type_variables(app.arg));
    }
    else if (type.is_a<ForallType>())
    {
        auto& forall = type.as_<ForallType>();
        tvars = free_meta_type_variables(forall.type);
    }
    else if (auto c = type.to<ConstrainedType>())
    {
        add(tvars, free_meta_type_variables(c->context));
        add(tvars, free_meta_type_variables(c->type));
    }
    else if (auto st = type.to<StrictType>())
    {
        return free_meta_type_variables(st->type);
    }
    else if (auto lt = type.to<LazyType>())
    {
        return free_meta_type_variables(lt->type);
    }
    else
        std::abort();

    return tvars;
}

// Q: how/when do we rename default method definitions?

set<TypeVar> free_type_variables(const Context& context)
{
    return free_type_variables(context.constraints);
}

set<TypeVar> free_type_variables(const vector<Type>& types)
{
    set<TypeVar> tvars;
    for(auto& type: types)
        add(tvars, free_type_variables(type));
    return tvars;
}

set<TypeVar> free_type_variables(const Type& type)
{
    set<TypeVar> tvars;
    if (type.is_a<TypeCon>())
    {
    }
    else if (type.is_a<TypeVar>())
    {
        auto& tv = type.as_<TypeVar>();
        auto& name = unloc(tv.name);
        assert(name.size());
        assert(is_haskell_varid(name));
        tvars.insert(tv);
    }
    else if (auto t = filled_meta_type_var(type))
        return free_type_variables(*t);
    else if (type.is_a<MetaTypeVar>())
    {
    }
    else if (type.is_a<TypeApp>())
    {
        auto& app = type.as_<TypeApp>();
        add(tvars, free_type_variables(app.head));
        add(tvars, free_type_variables(app.arg));
    }
    else if (type.is_a<ForallType>())
    {
        auto& forall = type.as_<ForallType>();
        tvars = free_type_variables(forall.type);
        for(auto& type_var: forall.type_var_binders)
            tvars.erase(type_var);
    }
    else if (auto c = type.to<ConstrainedType>())
    {
        add(tvars, free_type_variables(c->context));
        add(tvars, free_type_variables(c->type));
    }
    else if (auto st = type.to<StrictType>())
    {
        return free_type_variables(st->type);
    }
    else if (auto lt = type.to<LazyType>())
    {
        return free_type_variables(lt->type);
    }
    else
        std::abort();

    return tvars;
}

set<string> free_type_vars(const Type& type)
{
    set<string> tvars;
    if (type.is_a<TypeCon>())
    {
        return {};
    }
    else if (auto tv = type.to<TypeVar>())
    {
        auto& name = unloc(tv->name);
        tvars.insert(name);
    }
    else if (type.is_a<TypeApp>())
    {
        auto& app = type.as_<TypeApp>();
        add(tvars, free_type_vars(app.head));
        add(tvars, free_type_vars(app.arg));
    }
    else if (auto forall = type.to<ForallType>())
    {
        tvars = free_type_vars(forall->type);
        for(auto& type_var: forall->type_var_binders)
            tvars.erase(unloc(type_var.name));
    }
    else if (auto c = type.to<ConstrainedType>())
    {
        add(tvars, free_type_vars(c->context));
        add(tvars, free_type_vars(c->type));
    }
    else if (auto st = type.to<StrictType>())
    {
        return free_type_vars(st->type);
    }
    else if (auto lt = type.to<LazyType>())
    {
        return free_type_vars(lt->type);
    }
    else
        std::abort();

    return tvars;
}

set<string> free_type_vars(const Context& context)
{
    set<string> tvars;
    for(auto& constraint: context.constraints)
        add(tvars, free_type_vars(constraint));
    return tvars;
}

bool affected_by_mtv(const vector<Type>& types, const MetaTypeVar& mtv)
{
    for(auto& type: types)
        if (affected_by_mtv(type, mtv))
            return true;
    return false;
}

bool affected_by_mtv(const Type& type, const MetaTypeVar& mtv)
{
    if (type.is_a<TypeCon>())
        return false;
    else if (type.is_a<TypeVar>())
        return false;
    else if (auto mtv2 = type.to<MetaTypeVar>())
    {
        if (*mtv2 == mtv)
            return true;
        else if (auto t2 = mtv2->filled())
            return affected_by_mtv(*t2, mtv);
        else
            return false;
    }
    else if (auto app = type.to<TypeApp>())
    {
        return affected_by_mtv(app->head, mtv) or affected_by_mtv(app->arg, mtv);
    }
    else if (auto forall = type.to<ForallType>())
    {
        return affected_by_mtv(forall->type, mtv);
    }
    else if (auto c = type.to<ConstrainedType>())
    {
        return affected_by_mtv(c->context.constraints, mtv) or affected_by_mtv(c->type, mtv);
    }
    else if (auto st = type.to<StrictType>())
    {
        return affected_by_mtv(st->type, mtv);
    }
    else if (auto lt = type.to<LazyType>())
    {
        return affected_by_mtv(lt->type, mtv);
    }
    else
        std::abort();
}

bool contains_mtv(const vector<Type>& types, const MetaTypeVar& mtv)
{
    for(auto& type: types)
        if (contains_mtv(type, mtv))
            return true;
    return false;
}

bool contains_mtv(const Type& type, const MetaTypeVar& mtv)
{
    assert(not mtv.filled());

    if (type.is_a<TypeCon>())
        return false;
    else if (type.is_a<TypeVar>())
        return false;
    else if (auto mtv2 = type.to<MetaTypeVar>())
    {
        if (auto t2 = mtv2->filled())
            return contains_mtv(*t2, mtv);
        else if (*mtv2 == mtv)
            return true;
        else
            return false;
    }
    else if (auto app = type.to<TypeApp>())
    {
        return contains_mtv(app->head, mtv) or contains_mtv(app->arg, mtv);
    }
    else if (auto forall = type.to<ForallType>())
    {
        return contains_mtv(forall->type, mtv);
    }
    else if (auto c = type.to<ConstrainedType>())
    {
        return contains_mtv(c->context.constraints, mtv) or contains_mtv(c->type, mtv);
    }
    else if (auto st = type.to<StrictType>())
    {
        return contains_mtv(st->type, mtv);
    }
    else if (auto lt = type.to<LazyType>())
    {
        return contains_mtv(lt->type, mtv);
    }
    else
        std::abort();
}

bool contains_tv(const vector<Type>& types, const TypeVar& tv)
{
    for(auto& type: types)
        if (contains_tv(type, tv))
            return true;
    return false;
}

bool contains_tv(const Type& type, const TypeVar& tv)
{
    if (type.is_a<TypeCon>())
        return false;
    else if (auto tv2 = type.to<TypeVar>())
    {
        return tv == *tv2;
    }
    else if (auto mtv = type.to<MetaTypeVar>())
    {
        if (auto t2 = mtv->filled())
            return contains_tv(*t2, tv);
        else
            return false;
    }
    else if (auto app = type.to<TypeApp>())
    {
        return contains_tv(app->head, tv) or contains_tv(app->arg, tv);
    }
    else if (auto forall = type.to<ForallType>())
    {
        return contains_tv(forall->type, tv);
    }
    else if (auto c = type.to<ConstrainedType>())
    {
        return contains_tv(c->context.constraints, tv) or contains_tv(c->type, tv);
    }
    else if (auto s = type.to<StrictType>())
    {
        return contains_tv(s->type, tv);
    }
    else if (auto l = type.to<LazyType>())
    {
        return contains_tv(l->type, tv);
    }
    else
        std::abort();
}

