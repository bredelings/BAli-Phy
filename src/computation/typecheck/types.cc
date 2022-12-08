#include "types.H"
#include "util/set.H"   // for add( , )
#include "haskell/ids.H"

#include "computation/expression/tuple.H"

using std::set;
using std::optional;
using std::pair;
using std::vector;
using std::string;

using namespace Haskell;

// Q: how/when do we rename default method definitions?

set<Hs::MetaTypeVar> free_meta_type_variables(const Hs::Context& context)
{
    set<Hs::MetaTypeVar> tvars;
    for(auto& constraint: context.constraints)
        add(tvars, free_meta_type_variables(constraint));
    return tvars;
}

set<Hs::MetaTypeVar> free_meta_type_variables(const Hs::Type& type)
{
    set<Hs::MetaTypeVar> tvars;
    if (type.is_a<Hs::TypeCon>())
    { }
    else if (type.is_a<Hs::TypeVar>())
    { }
    else if (auto t = filled_meta_type_var(type))
    {
        return free_meta_type_variables(*t);
    }
    else if (type.is_a<Hs::MetaTypeVar>())
    {
        auto& tv = type.as_<Hs::MetaTypeVar>();
        assert(not tv.filled());
        auto& name = unloc(tv.name);
        assert(name.size());
        tvars.insert(tv);
    }
    else if (type.is_a<Hs::TypeApp>())
    {
        auto& app = type.as_<Hs::TypeApp>();
        add(tvars, free_meta_type_variables(app.head));
        add(tvars, free_meta_type_variables(app.arg));
    }
    else if (type.is_a<Hs::TupleType>())
    {
        auto& tuple = type.as_<Hs::TupleType>();
        for(auto element_type: tuple.element_types)
            add(tvars, free_meta_type_variables(element_type));
    }
    else if (type.is_a<Hs::ListType>())
    {
        auto& list = type.as_<Hs::ListType>();
        add(tvars, free_meta_type_variables(list.element_type));
    }
    else if (type.is_a<Hs::ForallType>())
    {
        auto& forall = type.as_<Hs::ForallType>();
        tvars = free_meta_type_variables(forall.type);
    }
    else if (auto c = type.to<Hs::ConstrainedType>())
    {
        add(tvars, free_meta_type_variables(c->context));
        add(tvars, free_meta_type_variables(c->type));
    }
    else if (auto sl = type.to<Hs::StrictLazyType>())
    {
        return free_meta_type_variables(sl->type);
    }
    else
        std::abort();

    return tvars;
}

// Q: how/when do we rename default method definitions?

set<Hs::TypeVar> free_type_variables(const Hs::Context& context)
{
    set<Hs::TypeVar> tvars;
    for(auto& constraint: context.constraints)
        add(tvars, free_type_variables(constraint));
    return tvars;
}

set<Hs::TypeVar> free_type_variables(const Hs::Type& type)
{
    set<Hs::TypeVar> tvars;
    if (type.is_a<Hs::TypeCon>())
    {
    }
    else if (type.is_a<Hs::TypeVar>())
    {
        auto& tv = type.as_<Hs::TypeVar>();
        auto& name = unloc(tv.name);
        assert(name.size());
        assert(is_haskell_varid(name));
        tvars.insert(tv);
    }
    else if (auto t = filled_meta_type_var(type))
        return free_type_variables(*t);
    else if (type.is_a<Hs::MetaTypeVar>())
    {
    }
    else if (type.is_a<Hs::TypeApp>())
    {
        auto& app = type.as_<Hs::TypeApp>();
        add(tvars, free_type_variables(app.head));
        add(tvars, free_type_variables(app.arg));
    }
    else if (type.is_a<Hs::TupleType>())
    {
        auto& tuple = type.as_<Hs::TupleType>();
        for(auto element_type: tuple.element_types)
            add(tvars, free_type_variables(element_type));
    }
    else if (type.is_a<Hs::ListType>())
    {
        auto& list = type.as_<Hs::ListType>();
        add(tvars, free_type_variables(list.element_type));
    }
    else if (type.is_a<Hs::ForallType>())
    {
        auto& forall = type.as_<Hs::ForallType>();
        tvars = free_type_variables(forall.type);
        for(auto& type_var: forall.type_var_binders)
            tvars.erase(type_var);
    }
    else if (auto c = type.to<Hs::ConstrainedType>())
    {
        add(tvars, free_type_variables(c->context));
        add(tvars, free_type_variables(c->type));
    }
    else if (auto sl = type.to<Hs::StrictLazyType>())
    {
        return free_type_variables(sl->type);
    }
    else
        std::abort();

    return tvars;
}

set<string> free_type_vars(const Hs::Type& type)
{
    set<string> tvars;
    if (type.is_a<Hs::TypeCon>())
    {
        return {};
    }
    else if (auto tv = type.to<Hs::TypeVar>())
    {
        auto& name = unloc(tv->name);
        tvars.insert(name);
    }
    else if (type.is_a<Hs::TypeApp>())
    {
        auto& app = type.as_<Hs::TypeApp>();
        add(tvars, free_type_vars(app.head));
        add(tvars, free_type_vars(app.arg));
    }
    else if (type.is_a<Hs::TupleType>())
    {
        auto& tuple = type.as_<Hs::TupleType>();
        for(auto element_type: tuple.element_types)
            add(tvars, free_type_vars(element_type));
    }
    else if (type.is_a<Hs::ListType>())
    {
        auto& list = type.as_<Hs::ListType>();
        return free_type_vars(list.element_type);
    }
    else if (auto forall = type.to<Hs::ForallType>())
    {
        tvars = free_type_vars(forall->type);
        for(auto& type_var: forall->type_var_binders)
            tvars.erase(unloc(type_var.name));
    }
    else if (auto c = type.to<Hs::ConstrainedType>())
    {
        add(tvars, free_type_vars(c->context));
        add(tvars, free_type_vars(c->type));
    }
    else if (auto sl = type.to<Hs::StrictLazyType>())
    {
        return free_type_vars(sl->type);
    }
    else
        std::abort();

    return tvars;
}

set<string> free_type_vars(const Hs::Context& context)
{
    set<string> tvars;
    for(auto& constraint: context.constraints)
        add(tvars, free_type_vars(constraint));
    return tvars;
}

bool affected_by_mtv(const vector<Hs::Type>& types, const Hs::MetaTypeVar& mtv)
{
    for(auto& type: types)
        if (affected_by_mtv(type, mtv))
            return true;
    return false;
}

bool affected_by_mtv(const Hs::Type& type, const Hs::MetaTypeVar& mtv)
{
    if (type.is_a<Hs::TypeCon>())
        return false;
    else if (type.is_a<Hs::TypeVar>())
        return false;
    else if (auto mtv2 = type.to<Hs::MetaTypeVar>())
    {
        if (*mtv2 == mtv)
            return true;
        else if (auto t2 = mtv2->filled())
            return affected_by_mtv(*t2, mtv);
        else
            return false;
    }
    else if (auto app = type.to<Hs::TypeApp>())
    {
        return affected_by_mtv(app->head, mtv) or affected_by_mtv(app->arg, mtv);
    }
    else if (auto tup = type.to<Hs::TupleType>())
    {
        return affected_by_mtv(tup->element_types, mtv);
    }
    else if (auto list = type.to<Hs::ListType>())
    {
        return affected_by_mtv(list->element_type, mtv);
    }
    else if (auto forall = type.to<Hs::ForallType>())
    {
        return affected_by_mtv(forall->type, mtv);
    }
    else if (auto c = type.to<Hs::ConstrainedType>())
    {
        return affected_by_mtv(c->context.constraints, mtv) or affected_by_mtv(c->type, mtv);
    }
    else if (auto sl = type.to<Hs::StrictLazyType>())
    {
        return affected_by_mtv(sl->type, mtv);
    }
    else
        std::abort();
}

bool contains_mtv(const vector<Hs::Type>& types, const Hs::MetaTypeVar& mtv)
{
    for(auto& type: types)
        if (contains_mtv(type, mtv))
            return true;
    return false;
}

bool contains_mtv(const Hs::Type& type, const Hs::MetaTypeVar& mtv)
{
    assert(not mtv.filled());

    if (type.is_a<Hs::TypeCon>())
        return false;
    else if (type.is_a<Hs::TypeVar>())
        return false;
    else if (auto mtv2 = type.to<Hs::MetaTypeVar>())
    {
        if (auto t2 = mtv2->filled())
            return contains_mtv(*t2, mtv);
        else if (*mtv2 == mtv)
            return true;
        else
            return false;
    }
    else if (auto app = type.to<Hs::TypeApp>())
    {
        return contains_mtv(app->head, mtv) or contains_mtv(app->arg, mtv);
    }
    else if (auto tup = type.to<Hs::TupleType>())
    {
        return contains_mtv(tup->element_types, mtv);
    }
    else if (auto list = type.to<Hs::ListType>())
    {
        return contains_mtv(list->element_type, mtv);
    }
    else if (auto forall = type.to<Hs::ForallType>())
    {
        return contains_mtv(forall->type, mtv);
    }
    else if (auto c = type.to<Hs::ConstrainedType>())
    {
        return contains_mtv(c->context.constraints, mtv) or contains_mtv(c->type, mtv);
    }
    else if (auto sl = type.to<Hs::StrictLazyType>())
    {
        return contains_mtv(sl->type, mtv);
    }
    else
        std::abort();
}

bool contains_tv(const vector<Hs::Type>& types, const Hs::TypeVar& tv)
{
    for(auto& type: types)
        if (contains_tv(type, tv))
            return true;
    return false;
}

bool contains_tv(const Hs::Type& type, const Hs::TypeVar& tv)
{
    if (type.is_a<Hs::TypeCon>())
        return false;
    else if (auto tv2 = type.to<Hs::TypeVar>())
    {
        return tv == *tv2;
    }
    else if (auto mtv = type.to<Hs::MetaTypeVar>())
    {
        if (auto t2 = mtv->filled())
            return contains_tv(*t2, tv);
        else
            return false;
    }
    else if (auto app = type.to<Hs::TypeApp>())
    {
        return contains_tv(app->head, tv) or contains_tv(app->arg, tv);
    }
    else if (auto tup = type.to<Hs::TupleType>())
    {
        return contains_tv(tup->element_types, tv);
    }
    else if (auto list = type.to<Hs::ListType>())
    {
        return contains_tv(list->element_type, tv);
    }
    else if (auto forall = type.to<Hs::ForallType>())
    {
        return contains_tv(forall->type, tv);
    }
    else if (auto c = type.to<Hs::ConstrainedType>())
    {
        return contains_tv(c->context.constraints, tv) or contains_tv(c->type, tv);
    }
    else if (auto sl = type.to<Hs::StrictLazyType>())
    {
        return contains_tv(sl->type, tv);
    }
    else
        std::abort();
}

