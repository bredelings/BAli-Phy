#include "types.H"
#include "util/set.H"   // for add( , )
#include "module.H"     // for is_haskell_varid( )

#include "computation/expression/tuple.H"

using std::set;
using std::optional;
using std::pair;
using std::vector;
using std::string;

using namespace Haskell;

Type make_arrow_type(const Type& t1, const Type& t2)
{
    static TypeCon type_arrow(Located<string>({},"->"));
    return TypeApp(TypeApp(type_arrow,t1),t2);
}

pair<Type,vector<Type>> decompose_type_apps(Type t)
{
    if (auto L = t.to<ListType>())
        return {Hs::TypeCon({noloc,"[]"}), {L->element_type}};

    if (auto T = t.to<TupleType>())
    {
        int n = T->element_types.size();
        return {Hs::TypeCon({noloc,tuple_name(n)}), T->element_types};
    }

    vector<Type> args;
    while(t.is_a<TypeApp>())
    {
        auto A = t.as_<TypeApp>();
        args.push_back(A.arg);
        t = A.head;
    }
    std::reverse(args.begin(), args.end());
    return {t,args};
}


optional<pair<Type,Type>> is_function_type(const Type& t)
{
    auto [head,args] = decompose_type_apps(t);

    if (args.size() != 2) return {};

    auto tc = head.to<TypeCon>();
    if (not tc) return {};

    if (unloc(tc->name) == "->")
        return {{args[0],args[1]}};
    else
        return {};
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

Hs::Type remove_top_gen(Hs::Type type)
{
    if (auto f = type.to<Hs::ForallType>())
        type = f->type;

    if (auto c = type.to<Hs::ConstrainedType>())
        type = c->type;

    return type;
}

