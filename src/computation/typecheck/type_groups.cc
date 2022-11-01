#include "kindcheck.H"
#include "expression/tuple.H"
#include "util/set.H"   // for add( , )
#include "util/graph.H"
#include "haskell/ids.H"

using std::pair;
using std::map;
using std::set;
using std::string;
using std::tuple;
using std::string;
using std::vector;


set<string> free_type_cons(const Hs::Context& context);

set<string> free_type_cons(const Hs::Type& type)
{
    set<string> tcons;
    if (type.is_a<Hs::TypeVar>())
    {
        return {};
    }
    else if (auto tc = type.to<Hs::TypeCon>())
    {
        auto& name = unloc(tc->name);
        if (is_qualified_symbol(name))
            tcons.insert(name);
    }
    else if (type.is_a<Hs::TypeApp>())
    {
        auto& app = type.as_<Hs::TypeApp>();
        add(tcons, free_type_cons(app.head));
        add(tcons, free_type_cons(app.arg));
    }
    else if (type.is_a<Hs::TupleType>())
    {
        auto& tuple = type.as_<Hs::TupleType>();
        for(auto element_type: tuple.element_types)
            add(tcons, free_type_cons(element_type));
    }
    else if (type.is_a<Hs::ListType>())
    {
        auto& list = type.as_<Hs::ListType>();
        return free_type_cons(list.element_type);
    }
    else if (auto forall = type.to<Hs::ForallType>())
    {
        return free_type_cons(forall->type);
    }
    else if (auto c = type.to<Hs::ConstrainedType>())
    {
        add(tcons, free_type_cons(c->context));
        add(tcons, free_type_cons(c->type));
    }
    else if (auto sl = type.to<Hs::StrictLazyType>())
    {
        return free_type_cons(sl->type);
    }
    else
        std::abort();
    return tcons;
}

set<string> free_type_cons(const Hs::Context& context)
{
    set<string> tvars;
    for(auto& constraint: context.constraints)
        add(tvars, free_type_cons(constraint));
    return tvars;
}

set<string> free_type_cons(const Hs::ClassDecl& class_decl)
{
    // QUESTION: We are ignoring default methods here -- should we?
    set<string> tvars;
    add(tvars, free_type_cons(class_decl.context));
    if (class_decl.binds)
    {
        for(auto& [name, type]: unloc(*class_decl.binds).signatures)
            add(tvars, free_type_cons(type));
    }
    return tvars;
}

set<string> free_type_cons(const Hs::DataOrNewtypeDecl& type_decl)
{
    set<string> tvars;
    add(tvars, free_type_cons(type_decl.context));
    if (type_decl.is_regular_decl())
    {
        for(auto& constr: type_decl.get_constructors())
        {
            if (constr.context)
                add(tvars, free_type_cons(*constr.context));

            if (constr.is_record_constructor())
            {
                auto& field_decls = std::get<1>(constr.fields).field_decls;
                for(auto& field: field_decls)
                    add(tvars, free_type_cons(field.type));
            }
            else
            {
                auto& types = std::get<0>(constr.fields);
                for(auto& type: types)
                    add(tvars, free_type_cons(type));
            }
        }
    }
    else if (type_decl.is_gadt_decl())
    {
        for(auto& data_cons_decl: type_decl.get_gadt_constructors())
            add(tvars, free_type_cons(unloc(data_cons_decl.type)));
    }
    return tvars;
}

set<string> free_type_cons(const Hs::TypeSynonymDecl& synonym_decl)
{
    return free_type_cons(unloc(synonym_decl.rhs_type));
}

set<string> free_type_cons(const Hs::InstanceDecl& instance_decl)
{
    set<string> tvars;
    add(tvars, free_type_cons(instance_decl.context));
    add(tvars, free_type_cons(instance_decl.constraint));
    return tvars;

}

vector<vector<expression_ref>> find_type_groups(const Hs::Decls& type_decls)
{
    // 1. Collection type and instance declarations

    // [(name,names-we-depend-on)]  No instances.
    map<string,set<string>> referenced_types;

    map<string, expression_ref> decl_for_type;

    vector<tuple<Hs::InstanceDecl,set<string>>> instance_decls;

    for(auto& decl: type_decls)
    {
        if (decl.is_a<Hs::ClassDecl>())
        {
            auto& class_decl = decl.as_<Hs::ClassDecl>();
            referenced_types[class_decl.name] = free_type_cons(class_decl);
            decl_for_type[class_decl.name] = decl;
        }
        else if (decl.is_a<Hs::DataOrNewtypeDecl>())
        {
            auto& type_decl = decl.as_<Hs::DataOrNewtypeDecl>();
            referenced_types[type_decl.name] = free_type_cons(type_decl);
            decl_for_type[type_decl.name] = decl;
        }
        else if (decl.is_a<Hs::TypeSynonymDecl>())
        {
            auto& type_decl = decl.as_<Hs::TypeSynonymDecl>();
            referenced_types[type_decl.name] = free_type_cons(type_decl);
            decl_for_type[type_decl.name] = decl;
        }
        else if (decl.is_a<Hs::InstanceDecl>())
        {
            auto& instance_decl = decl.as_<Hs::InstanceDecl>();
            instance_decls.push_back({instance_decl, free_type_cons(instance_decl)});
        }
        else
            std::abort();
    }

    // 2. Get order list of mutually dependent TYPE declarations

    // Input to the dependency analysis is a list of
    // * (declaration, name, cons that this declaration depends on)

    for(auto& [type, referenced_types]: referenced_types)
    {
        set<string> referenced_types_that_matter;
        for(auto& type: referenced_types)
            if (decl_for_type.count(type))
                referenced_types_that_matter.insert(type);
        referenced_types = referenced_types_that_matter;
    }

    auto ordered_name_groups = get_ordered_strong_components(referenced_types);

    auto type_decl_groups = map_groups( ordered_name_groups, decl_for_type );
    return type_decl_groups;
}
