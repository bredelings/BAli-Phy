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

