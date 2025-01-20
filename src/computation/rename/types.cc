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
        try
        {
            auto T = m.lookup_type(name);
            name = T->name;
        }
        catch (myexception& e)
        {
            error(loc, Note()<<e.what());
        }
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
    else if (auto st = type.to<Hs::StrictType>())
    {
        auto St = *st;
        St.type = rename_type(St.type);
        type = St;
    }
    else
        throw myexception()<<"rename_type: unrecognized type \""<<type.print()<<"\"";

    return ltype;
}

Haskell::Context renamer_state::rename(Haskell::Context context)
{
    for(auto& constraint: context)
        constraint = rename_type(constraint);
    return context;
}

Haskell::DataDefn renamer_state::rename(Haskell::DataDefn decl)
{
    decl.context = rename(decl.context);

    if (decl.is_regular_decl())
    {
        for(auto& constructor: decl.get_constructors())
        {
            qualify_name(unloc(*constructor.con).name);

            if (constructor.context)
            {
                for(auto& constraint: *constructor.context)
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

Haskell::DataOrNewtypeDecl renamer_state::rename(Haskell::DataOrNewtypeDecl decl)
{
    qualify_name(decl.name);

    Hs::DataDefn& decl2 = decl;
    decl2 = rename( decl2 );

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

    for(auto& fam_decl: C.fam_decls)
        fam_decl = rename(fam_decl);

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

Haskell::FamilyDecl renamer_state::rename(Haskell::FamilyDecl TF)
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
            try
            {
                auto T = m.lookup_type(con_name);
                con_name = T->name;
            }
            catch (myexception& e)
            {
                error(con_loc, Note()<<e.what());
            }
	}
	else
	    error(con_loc, Note()<<"Instance for undeclared type family `"<<con_name<<"`");
    }

    for(auto& arg: TIE.args)
        arg = rename_type(arg);
    TIE.rhs = rename_type(TIE.rhs);
    return TIE;
}

Haskell::DataFamilyInstanceDecl renamer_state::rename(Haskell::DataFamilyInstanceDecl DI)
{
    // TIE.con = rename_type(TIE.con);
    // But with a specific error message.
    {
	auto& [con_loc, tc] = DI.con;
	auto& con_name = tc.name;

	if (m.type_is_declared(con_name))
	{
            try
            {
                auto T = m.lookup_type(con_name);
                con_name = T->name;
            }
            catch (myexception& e)
            {
                error(con_loc, Note()<<e.what());
            }
	}
	else
	    error(con_loc, Note()<<"Instance for undeclared data family `"<<con_name<<"`");
    }

    for(auto& arg: DI.args)
        arg = rename_type(arg);

    DI.rhs = rename( DI.rhs );

    return DI;
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

Hs::DefaultDecl renamer_state::rename(Hs::DefaultDecl DD)
{
    if (DD.maybe_class)
    {
        auto [loc, class_name] = *DD.maybe_class;
        auto renamed_tycon = rename_type(Hs::LTypeCon({loc,class_name}));
        DD.maybe_class = {{loc,unloc(renamed_tycon).name}};
    }
    for(auto& type: DD.types)
        type = rename_type(type);
    return DD;
}

vector<Located<Hs::DefaultDecl>> renamer_state::rename_default_decls(vector<Located<Hs::DefaultDecl>> default_decls)
{
    for(auto& [loc, decl]: default_decls)
        decl = rename(decl);
    return default_decls;
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
        else if (auto TF = decl.to<Hs::FamilyDecl>())
            decl = rename(*TF);
        else if (auto TI = decl.to<Hs::TypeFamilyInstanceDecl>())
            decl = rename(*TI);
        else if (auto TI = decl.to<Hs::DataFamilyInstanceDecl>())
            decl = rename(*TI);
        else if (auto KS = decl.to<Hs::KindSigDecl>())
            decl = rename(*KS);
        else
            throw myexception()<<"In module "<<m.name<<": Unrecognized declaration:\n  "<<decl.print();
    }

    return decls;
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

