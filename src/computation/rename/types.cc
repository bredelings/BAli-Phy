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

Hs::LTypeCon renamer_state::rename_type(Hs::LTypeCon ltc)
{
    auto& [loc, tc] = ltc;
    auto& name = tc.name;

    if (m.type_is_declared(name))
    {
        auto T = m.lookup_type(name);
        name = T->name;
    }
    else
        error(loc, Note()<<"Undeclared type constructor `"<<name<<"`");

    return ltc;
}

Haskell::LType renamer_state::rename_type(Haskell::LType ltype)
{
    auto& [loc, type] = ltype;

    if (auto tc = type.to<Haskell::TypeCon>())
    {
        ltype = rename_type(Hs::LTypeCon{loc,*tc});
    }
    else if (auto tv = type.to<Haskell::TypeVar>())
    {
        auto& name = tv->name;
        assert(is_haskell_varid(name));
    }
    else if (type.is_a<Haskell::TypeApp>())
    {
        auto app = type.as_<Haskell::TypeApp>();
        app.head = rename_type(app.head);
        app.arg  = rename_type(app.arg);
        type = app;
    }
    else if (type.is_a<Haskell::TupleType>())
    {
        auto tuple = type.as_<Haskell::TupleType>();
        for(auto& etype: tuple.element_types)
            etype = rename_type(etype);
        type = tuple;
    }
    else if (type.is_a<Haskell::ListType>())
    {
        auto list = type.as_<Haskell::ListType>();
        list.element_type = rename_type(list.element_type);
        type = list;
    }
    else if (type.is_a<Haskell::ConstrainedType>())
    {
        auto ctype = type.as_<Haskell::ConstrainedType>();
        ctype.context = rename(ctype.context);
        ctype.type = rename_type(ctype.type);
        type = ctype;
    }
    else if (auto fa = type.to<Hs::ForallType>())
    {
        auto Fa = *fa;
        Fa.type = rename_type(Fa.type);
        type = Fa;
    }
    else
        throw myexception()<<"rename_type: unrecognized type \""<<type.print()<<"\"";

    return ltype;
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
            qualify_name(unloc(*constructor.con).name);

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
                        qualify_name(unloc(var));
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

            constructors.type = rename_type(constructors.type);
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
            qualify_name(unloc(v));
    }

    // Renaming of the default_methods is done in rename_decls

    return C;
}

Haskell::TypeSynonymDecl renamer_state::rename(Haskell::TypeSynonymDecl decl)
{
    qualify_name(decl.name);
    decl.rhs_type = rename_type(decl.rhs_type);
    return decl;
}

Haskell::TypeFamilyDecl renamer_state::rename(Haskell::TypeFamilyDecl TF)
{
    qualify_name(unloc(TF.con));

    if (TF.where_instances)
        for(auto& inst: *TF.where_instances)
            inst = rename(inst);

    return TF;
}

Haskell::TypeFamilyInstanceEqn renamer_state::rename(Haskell::TypeFamilyInstanceEqn TIE)
{
    // TIE.con = rename_type(TIE.con);
    // But with a specific error message.
    {
	auto& [con_loc, tc] = TIE.con;
	auto& con_name = tc.name;

	if (m.type_is_declared(con_name))
	{
	    auto T = m.lookup_type(con_name);
	    con_name = T->name;
	}
	else
	    error(con_loc, Note()<<"Instance for undeclared type family `"<<con_name<<"`");
    }

    for(auto& arg: TIE.args)
        arg = rename_type(arg);
    TIE.rhs = rename_type(TIE.rhs);
    return TIE;
}

Haskell::TypeFamilyInstanceDecl renamer_state::rename(Haskell::TypeFamilyInstanceDecl TI)
{
    TI = rename( static_cast<Hs::TypeFamilyInstanceEqn&>(TI) );
    return TI;
}

Haskell::KindSigDecl renamer_state::rename(Haskell::KindSigDecl KS)
{
    for(auto& tycon: KS.tycons)
        tycon = rename_type(tycon);
    return KS;
}


Haskell::Decls renamer_state::rename_type_decls(Haskell::Decls decls)
{
    for(auto& [_,decl]: decls)
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
        else if (auto KS = decl.to<Hs::KindSigDecl>())
            decl = rename(*KS);
        else
            throw myexception()<<"In module "<<m.name<<": Unrecognized declaration:\n  "<<decl.print();
    }

    return decls;
}

