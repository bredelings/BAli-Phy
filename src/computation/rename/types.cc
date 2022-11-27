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

Hs::TypeCon renamer_state::rename_type(const Hs::TypeCon& tc)
{
    auto& name = unloc(tc.name);
    auto& loc = tc.name.loc;

    if (m.type_is_declared(name))
    {
        auto T = m.lookup_type(name);
        auto& qualified_name = T.name;
        return Hs::TypeCon({loc, qualified_name});
    }
    else
    {
        if (loc)
            throw myexception()<<"Can't find tycon '"<<name<<"' at "<<*loc;
        else
            throw myexception()<<"Can't find tycon '"<<name<<"'";
    }
}

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
    qualify_name(decl.name);

    decl.context = rename(decl.context);

    if (decl.is_regular_decl())
    {
        for(auto& constructor: decl.get_constructors())
        {
            qualify_name(constructor.name);

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
                        qualify_name(var);
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
                qualify_name(con_name);

            unloc(constructors.type) = rename_type(unloc(constructors.type));
        }
    }
    return decl;
}

Haskell::InstanceDecl renamer_state::rename(Haskell::InstanceDecl I)
{
    I.context = rename(I.context);
    I.constraint = rename_type(I.constraint);

    for(auto& type_inst_decl: I.type_inst_decls)
        type_inst_decl = rename(type_inst_decl);

    // Renaming of the methods is done in rename_decls

    return I;
}

Haskell::ClassDecl renamer_state::rename(Haskell::ClassDecl C)
{
    qualify_name(C.name);
    C.context = rename(C.context);

    for(auto& type_fam_decl: C.type_fam_decls)
        type_fam_decl = rename(type_fam_decl);

    for(auto& default_type_inst_decl: C.default_type_inst_decls)
        default_type_inst_decl = rename(default_type_inst_decl);

    for(auto& sig_decl: C.sig_decls)
    {
        sig_decl.type = rename_type(sig_decl.type);

        for(auto& v: sig_decl.vars)
            qualify_name(v);
    }

    // Renaming of the default_methods is done in rename_decls

    return C;
}

Haskell::TypeSynonymDecl renamer_state::rename(Haskell::TypeSynonymDecl decl)
{
    qualify_name(decl.name);
    unloc(decl.rhs_type) = rename_type(unloc(decl.rhs_type));
    return decl;
}

Haskell::TypeFamilyDecl renamer_state::rename(Haskell::TypeFamilyDecl TF)
{
    qualify_name(TF.con);

    if (TF.where_instances)
        for(auto& inst: *TF.where_instances)
            inst = rename(inst);

    return TF;
}

Haskell::TypeFamilyInstanceEqn renamer_state::rename(Haskell::TypeFamilyInstanceEqn TIE)
{
    TIE.con = rename_type(TIE.con);
    for(auto& arg: TIE.args)
        rename_type(arg);
    TIE.rhs = rename_type(TIE.rhs);
    return TIE;
}

Haskell::TypeFamilyInstanceDecl renamer_state::rename(Haskell::TypeFamilyInstanceDecl TI)
{
    TI = rename( static_cast<Hs::TypeFamilyInstanceEqn&>(TI) );
    return TI;
}


Haskell::Decls renamer_state::rename_type_decls(Haskell::Decls decls)
{
    for(auto& decl: decls)
    {
        if (auto C = decl.to<Hs::ClassDecl>())
            decl = rename(*C);
        else if (auto D = decl.to<Hs::DataOrNewtypeDecl>())
            decl = rename(*D);
        else if (auto I = decl.to<Hs::InstanceDecl>())
            decl = rename(*I);
        else if (auto T = decl.to<Hs::TypeSynonymDecl>())
            decl = rename(*T);
        else if (auto TF = decl.to<Hs::TypeFamilyDecl>())
            decl = rename(*TF);
        else if (auto TI = decl.to<Hs::TypeFamilyInstanceDecl>())
            decl = rename(*TI);
        else
            throw myexception()<<"In module "<<m.name<<": Unrecognized declaration:\n  "<<decl.print();
    }

    return decls;
}

