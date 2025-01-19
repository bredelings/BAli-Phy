#include "types.H"
#include "util/set.H"   // for add( , )
#include "haskell/ids.H"
#include "util/assert.hh"

using std::set;
using std::optional;
using std::pair;
using std::vector;
using std::string;

// Q: how/when do we rename default method definitions?

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
        assert(tv.name.size());
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
    else
        std::abort();

    return tvars;
}

// Q: how/when do we rename default method definitions?

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
        assert(tv.name.size());
        assert(is_haskell_varid(tv.name));
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
    else
        std::abort();

    return tvars;
}

void free_type_variables_(const vector<Hs::LType>& types, vector<Hs::LTypeVar>& tvs)
{
    for(auto& type: types)
        free_type_variables_(type, tvs);
}

void free_type_variables_(const Hs::LType& ltype, vector<Hs::LTypeVar>& tvs)
{
    auto& [loc, type] = ltype;

    if (auto tv = type.to<Hs::TypeVar>())
    {
        assert(tv->name.size());
        assert(is_haskell_varid(tv->name));
        tvs.push_back({loc,*tv});
    }
    else if (type.is_a<Hs::TypeCon>())
    {
    }
    else if (auto tuple_type = type.to<Hs::TupleType>())
    {
        for(auto& element_type: tuple_type->element_types)
            free_type_variables_(element_type, tvs);
    }
    else if (auto list_type = type.to<Hs::ListType>())
    {
        free_type_variables_(list_type->element_type, tvs);
    }
    else if (auto tapp = type.to<Hs::TypeApp>())
    {
        free_type_variables_(tapp->head, tvs);
        free_type_variables_(tapp->arg, tvs);
    }
    else if (auto c = type.to<Hs::ConstrainedType>())
    {
        free_type_variables_(c->context, tvs);
        free_type_variables_(c->type, tvs);
    }
    else if (auto fa = type.to<Hs::ForallType>())
    {
        auto binders = fa->type_var_binders | ranges::to<set>();

        vector<Hs::LTypeVar> body_tvs;
        free_type_variables_(fa->type, body_tvs);

        for(auto& tv: body_tvs)
            if (not binders.count(tv))
                tvs.push_back(tv);
    }
    else if (auto strict_type = type.to<Hs::StrictType>())
    {
        free_type_variables_(strict_type->type, tvs);
    }
    else if (auto lazy_type = type.to<Hs::LazyType>())
    {
        free_type_variables_(lazy_type->type, tvs);
    }
    else if (auto type_of_kind = type.to<Hs::TypeOfKind>())
    {
        // Extract the kind variables first.
        // Right new we are using CORETYPE for the kind!
        // But we should be using Hs::Type, and storing a located Kind.
        // free_type_variables(type_of_kind->kind, tvs);
        free_type_variables_(type_of_kind->type, tvs);
    }
    // FieldDecls actually this can't happen right now!
    else
        throw myexception()<<"free_type_vars: bad type "<<type.print()<<"!";
}

vector<Hs::LTypeVar> unique_type_vars(const vector<Hs::LTypeVar>& tvs)
{
    vector<Hs::LTypeVar> tvs2;

    set<Hs::LTypeVar> seen_tvs;
    for(auto& tv: tvs)
        if (not seen_tvs.count(tv))
        {
            tvs2.push_back(tv);
            seen_tvs.insert(tv);
        }

    return tvs2;
}

vector<Hs::LTypeVar> free_type_variables(const vector<Hs::LType>& types)
{
    vector<Hs::LTypeVar> tvs;
    free_type_variables_(types, tvs);
    return unique_type_vars(tvs);
}

vector<Hs::LTypeVar> free_type_variables(const Hs::LType& type)
{
    vector<Hs::LTypeVar> tvs;
    free_type_variables_(type, tvs);
    return unique_type_vars(tvs);
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
        tvars.insert(tv->name);
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
            tvars.erase(type_var.name);
    }
    else if (auto c = type.to<ConstrainedType>())
    {
        add(tvars, free_type_vars(c->context));
        add(tvars, free_type_vars(c->type));
    }
    else
        std::abort();

    return tvars;
}

set<string> free_type_vars(const std::vector<Type>& context)
{
    set<string> tvars;
    for(auto& constraint: context)
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
        return affected_by_mtv(c->context, mtv) or affected_by_mtv(c->type, mtv);
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
        return contains_mtv(c->context, mtv) or contains_mtv(c->type, mtv);
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
        return contains_tv(c->context, tv) or contains_tv(c->type, tv);
    }
    else
        std::abort();
}

