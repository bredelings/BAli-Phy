#include <string>
#include <vector>
#include <set>
#include <map>
#include <optional>

#include "rename.H"
#include "haskell/ids.H"
#include "computation/module.H"
#include "computation/expression/apply.H"
#include "util/set.H"

using std::string;
using std::vector;
using std::pair;
using std::set;
using std::optional;
using std::map;

Haskell::Type renamer_state::rename_type(const Haskell::Type& type)
{
    if (auto tc = type.to<Haskell::TypeCon>())
    {
        auto& name = unloc(tc->name);
        auto& loc = tc->name.loc;

        if (m.type_is_declared(name))
        {
            auto T = m.lookup_type(name);
            auto& qualified_name = T.name;
            return Haskell::TypeCon({loc, qualified_name});
        }
        else
        {
            if (loc)
                throw myexception()<<"Can't find tycon '"<<name<<"' at "<<*loc;
            else
                throw myexception()<<"Can't find tycon '"<<name<<"'";
        }
    }
    else if (auto tv = type.to<Haskell::TypeVar>())
    {
        auto& name = unloc(tv->name);

        assert(is_haskell_varid(name));

        return type;
    }
    else if (type.is_a<Haskell::TypeApp>())
    {
        auto app = type.as_<Haskell::TypeApp>();
        app.head = rename_type(app.head);
        app.arg  = rename_type(app.arg);
        return app;
    }
    else if (type.is_a<Haskell::TupleType>())
    {
        auto tuple = type.as_<Haskell::TupleType>();
        for(auto& type: tuple.element_types)
            type = rename_type(type);
        return tuple;
    }
    else if (type.is_a<Haskell::ListType>())
    {
        auto list = type.as_<Haskell::ListType>();
        list.element_type = rename_type(list.element_type);
        return list;
    }
    else if (type.is_a<Haskell::ConstrainedType>())
    {
        auto ctype = type.as_<Haskell::ConstrainedType>();
        ctype.context = rename(ctype.context);
        ctype.type = rename_type(ctype.type);
        return ctype;
    }
    else if (auto fa = type.to<Hs::ForallType>())
    {
        auto Fa = *fa;
        Fa.type = rename_type(Fa.type);
        return Fa;
    }
    else
        throw myexception()<<"rename_type: unrecognized type \""<<type.print()<<"\"";
}

Haskell::Context renamer_state::rename(Haskell::Context context)
{
    for(auto& constraint: context.constraints)
        constraint = rename_type(constraint);
    return context;
}

Haskell::DataOrNewtypeDecl renamer_state::rename(Haskell::DataOrNewtypeDecl decl)
{
    decl.name = m.name + "." + decl.name;

    decl.context = rename(decl.context);

    if (decl.is_regular_decl())
    {
        for(auto& constructor: decl.get_constructors())
        {
            constructor.name = m.name + "." + constructor.name;

            if (constructor.context)
            {
                for(auto& constraint: constructor.context->constraints)
                    constraint = rename_type(constraint);
            }

            if (constructor.is_record_constructor())
            {
                for(auto& field: std::get<1>(constructor.fields).field_decls)
                {
                    for(auto& var: field.field_names)
                        unloc(var.name) = m.name + "." + unloc(var.name);
                    field.type = rename_type(field.type);
                }
            }
            else
            {
                for(auto& type: std::get<0>(constructor.fields))
                    type = rename_type(type);
            }
        }
    }
    else if (decl.is_gadt_decl())
    {
        for(auto& constructors: decl.get_gadt_constructors())
        {
            for(auto& con_name: constructors.con_names)
            {
                unloc(con_name) = m.name + "." + unloc(con_name);
            }
            unloc(constructors.type) = rename_type(unloc(constructors.type));
        }
    }
    return decl;
}

Haskell::InstanceDecl renamer_state::rename(Haskell::InstanceDecl I)
{
    I.context = rename(I.context);
    I.constraint = rename_type(I.constraint);

    // Renaming of the decl group is done in rename_decls

    return I;
}

Haskell::ClassDecl renamer_state::rename(Haskell::ClassDecl decl)
{
    decl.name = m.name + "." + decl.name;
    decl.context = rename(decl.context);
    // Renaming of the decl group is done in rename_decls
    return decl;
}

Haskell::TypeSynonymDecl renamer_state::rename(Haskell::TypeSynonymDecl decl)
{
    decl.name = m.name + "." + decl.name;
    unloc(decl.rhs_type) = rename_type(unloc(decl.rhs_type));
    return decl;
}


Haskell::Decls renamer_state::rename_type_decls(Haskell::Decls decls)
{
    for(auto& decl: decls)
    {
        if (decl.is_a<Haskell::ClassDecl>())
            decl = rename(decl.as_<Haskell::ClassDecl>());
        else if (decl.is_a<Haskell::DataOrNewtypeDecl>())
            decl = rename(decl.as_<Haskell::DataOrNewtypeDecl>());
        else if (decl.is_a<Haskell::InstanceDecl>())
            decl = rename(decl.as_<Haskell::InstanceDecl>());
        else if (decl.is_a<Haskell::TypeSynonymDecl>())
            decl = rename(decl.as_<Haskell::TypeSynonymDecl>());
    }

    return decls;
}

