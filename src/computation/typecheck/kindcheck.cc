#include "kindcheck.H"
#include "range/v3/all.hpp"
#include "expression/tuple.H"
#include "util/set.H"   // for add( , )

using std::pair;
using std::map;
using std::set;
using std::optional;
using std::string;
using std::vector;

namespace views = ranges::views;

// Q: how/when do we rename default method definitions?

Haskell::Type add_context(const vector<Haskell::Type>& constraints, const Haskell::Type& type)
{
    if (constraints.empty())
        return type;
    else if (type.is_a<Haskell::ConstrainedType>())
    {
        auto CT = type.as_<Haskell::ConstrainedType>();
        for(auto& constraint: constraints)
            CT.context.constraints.push_back(constraint);
        return CT;
    }
    else
        return Haskell::ConstrainedType(Haskell::Context(constraints),type);
}

Haskell::Type add_forall_vars(const std::vector<Haskell::TypeVar>& type_vars, const Haskell::Type& type)
{
    if (type.is_a<Haskell::ForallType>())
    {
        auto FAT = type.as_<Haskell::ForallType>();
        auto new_type_vars = type_vars;
        for(auto& type_var: FAT.type_var_binders)
        {
            assert(not includes(type_vars, type_var));
            new_type_vars.push_back(type_var);
        }
        return Haskell::ForallType(new_type_vars, FAT.type);
    }
    else
        return Haskell::ForallType(type_vars, type);
}

set<Haskell::TypeVar> free_type_VARS_from_type(const Haskell::Type& type)
{
    // This version finds varids -- the other version finds qualified names.
    set<Haskell::TypeVar> tvars;
    if (type.is_a<Haskell::TypeCon>())
    {
    }
    else if (type.is_a<Haskell::TypeVar>())
    {
        auto& tv = type.as_<Haskell::TypeVar>();
        auto& name = unloc(tv.name);
        assert(name.size());
        assert(is_haskell_varid(name));
        tvars.insert(tv);
    }
    else if (type.is_a<Haskell::TypeApp>())
    {
        auto& app = type.as_<Haskell::TypeApp>();
        add(tvars, free_type_VARS_from_type(app.head));
        add(tvars, free_type_VARS_from_type(app.arg));
    }
    else if (type.is_a<Haskell::TupleType>())
    {
        auto& tuple = type.as_<Haskell::TupleType>();
        for(auto element_type: tuple.element_types)
            add(tvars, free_type_VARS_from_type(element_type));
    }
    else if (type.is_a<Haskell::ListType>())
    {
        auto& list = type.as_<Haskell::ListType>();
        add(tvars, free_type_VARS_from_type(list.element_type));
    }
    else if (type.is_a<Haskell::ForallType>())
    {
        auto& forall = type.as_<Haskell::ForallType>();
        tvars = free_type_VARS_from_type(forall.type);
        for(auto& type_var: forall.type_var_binders)
            tvars.erase(type_var);
    }
    else
        throw myexception()<<"free_type_VARS_from_type: unrecognized type \""<<type.print()<<"\"";

    return tvars;
}

bool kindchecker_state::type_var_in_scope(const Haskell::TypeVar& tv) const
{
    return type_var_to_kind.back().count(tv);
}

void kindchecker_state::bind_type_var(const Haskell::TypeVar& tv, const kind& k)
{
    // We can't modify the initial empty scope.
    assert(type_var_to_kind.size() > 1);

    auto& tvk = type_var_to_kind.back();
    if (tvk.count(tv))
        tvk.erase(tv);
    tvk.insert({tv,k});
}

void kindchecker_state::push_type_var_scope()
{
    assert(not type_var_to_kind.empty());
    type_var_to_kind.push_back( type_var_to_kind.back() );
}

void kindchecker_state::pop_type_var_scope()
{
    assert(type_var_to_kind.size() > 1);
    type_var_to_kind.pop_back();
    assert(not type_var_to_kind.empty());
}

void kindchecker_state::add_type_con_of_kind(const string& name, const kind& k)
{
    assert(not type_con_to_kind.count(name));
    type_con_to_kind.insert({name,k});
}

kind kindchecker_state::apply_substitution(const kind& k) const
{
    return apply_subst(kind_var_to_kind, k);
}

void kindchecker_state::add_substitution(const KindVar& kv, const kind& k)
{
    assert(not kind_var_to_kind.count(kv));

    kind_var_to_kind.insert({kv,k});
}


//FIXME -- is this the right order?
void kindchecker_state::add_substitution(const k_substitution_t& s)
{
    kind_var_to_kind = compose(s,kind_var_to_kind);
}

kind kindchecker_state::kind_for_type_con(const std::string& name) const
{
    auto k = type_con_to_kind.at(name);
    return apply_substitution(k);
}


kind kindchecker_state::kind_for_type_var(const Haskell::TypeVar& tv) const
{
    auto it = type_var_to_kind.back().find(tv);
    if (it == type_var_to_kind.back().end())
        throw myexception()<<"Can't find type variable '"<<tv.print()<<"'";
    auto k = it->second;
    return apply_subst(kind_var_to_kind, k);
}

bool kindchecker_state::unify(const kind& k1, const kind& k2)
{
    auto s = ::unify(k1,k2);
    if (s)
    {
        add_substitution(*s);
        return true;
    }
    else
        return false;
}

void kindchecker_state::kind_check_type_of_kind(const Haskell::Type& t, const kind& k)
{
    auto k2 = kind_check_type(t);
    if (not unify(k,k2))
        throw myexception()<<"Type "<<t<<" has kind "<<apply_substitution(k2)<<", but should have kind "<<apply_substitution(k)<<"\n";
}

kind kindchecker_state::kind_check_type_var(const Haskell::TypeVar& tv)
{
    return kind_for_type_var(tv);
}

kind kindchecker_state::kind_check_type_con(const string& name)
{
    if (type_con_to_kind.count(name))
        return kind_for_type_con(name);
    else
    {
        auto tinfo = mod.lookup_resolved_type(name);
        assert(tinfo.k);
        return tinfo.k;
    }
}

kind kindchecker_state::kind_check_type(const Haskell::Type& t)
{
    if (auto tc = t.to<Haskell::TypeCon>())
    {
        auto& name = unloc(tc->name);

        return kind_check_type_con(name);
    }
    else if (auto tv = t.to<Haskell::TypeVar>())
    {
        return kind_check_type_var(*tv);
    }
    else if (t.is_a<Haskell::TypeApp>())
    {
        auto& tapp = t.as_<Haskell::TypeApp>();

        auto k1 = kind_check_type(tapp.head);
        auto k2 = kind_check_type(tapp.arg);

        if (k1->is_kvar())
        {
            auto& a = dynamic_cast<const KindVar&>(*k1);
            auto a1 = fresh_kind_var();
            auto a2 = fresh_kind_var();
            add_substitution(a, make_kind_arrow(a1,a2));
            unify(a1, k2); /// can't fail.
            return a2;
        }
        else if (k1->is_karrow())
        {
            auto& a = dynamic_cast<const KindArrow&>(*k1);
            if (not unify(a.k1, k2))
                throw myexception()<<"In type '"<<t<<"', can't apply type ("<<tapp.head<<" :: "<<apply_substitution(k1)<<") to type ("<<tapp.arg<<" :: "<<apply_substitution(k2)<<")";
            return a.k2;
        }
        else
            throw myexception()<<"Can't apply type "<<tapp.head<<" :: "<<k1->print()<<" to type "<<tapp.arg<<".";

    }
    else if (t.is_a<Haskell::ListType>())
    {
        auto& L = t.as_<Haskell::ListType>();
        Haskell::Type list_con = Haskell::TypeCon(Unlocated("[]"));
        Haskell::Type list_type = Haskell::TypeApp(list_con, L.element_type);
        return kind_check_type(list_type);
    }
    else if (t.is_a<Haskell::TupleType>())
    {
        auto& T = t.as_<Haskell::TupleType>();
        auto n = T.element_types.size();
        Haskell::Type tuple_type = Haskell::TypeCon(Unlocated(tuple_name(n)));
        for(auto& element_type: T.element_types)
            tuple_type = Haskell::TypeApp(tuple_type, element_type);
        return kind_check_type(tuple_type);
    }
    else
        throw myexception()<<"I don't recognize type '"<<t.print()<<"'";
}


void kindchecker_state::kind_check_constraint(const Haskell::Type& constraint)
{
    return kind_check_type_of_kind(constraint, make_kind_constraint());
}

void kindchecker_state::kind_check_context(const Haskell::Context& context)
{
    for(auto& constraint: context.constraints)
        kind_check_constraint(constraint);
}

void kindchecker_state::kind_check_constructor(const Haskell::Constructor& constructor, const Haskell::Type& data_type)
{
    auto type2 = data_type;

    if (constructor.is_record_constructor())
    {
        auto& fields = std::get<1>(constructor.fields);

        for(auto& field_decl: fields.field_decls | views::reverse)
        {
            for(int i=0;i<field_decl.field_names.size();i++)
                type2 = Haskell::make_arrow_type(field_decl.type, type2);
        }
    }
    else
    {
        auto& types = std::get<0>(constructor.fields);

        for(auto& type: types | views::reverse)
            type2 = Haskell::make_arrow_type(type, type2);

    }

    // FIXME: how about constraints?
    // Perhaps constraints on constructors lead to records that contain pointers to dictionaries??

    kind_check_type_of_kind(type2, make_kind_star());
}

Haskell::Type kindchecker_state::type_check_constructor(const Haskell::Constructor& constructor, const Haskell::Type& data_type)
{
    // At the moment, constructors cannot introduce new type variables.
    // So, we just need to construct the type.

    auto type2 = data_type;

    if (constructor.is_record_constructor())
    {
        auto& fields = std::get<1>(constructor.fields);

        for(auto& field_decl: fields.field_decls | views::reverse)
        {
            for(int i=0;i<field_decl.field_names.size();i++)
                type2 = Haskell::make_arrow_type(field_decl.type, type2);
        }
    }
    else
    {
        auto& types = std::get<0>(constructor.fields);

        for(auto& type: types | views::reverse)
            type2 = Haskell::make_arrow_type(type, type2);

    }

    return type2;
}

void kindchecker_state::kind_check_data_type(const Haskell::DataOrNewtypeDecl& data_decl)
{
    push_type_var_scope();

    // a. Look up kind for this data type.
    kind k = kind_for_type_con(data_decl.name);  // FIXME -- check that this is a data type?

    // b. Put each type variable into the kind.
    for(auto& tv: data_decl.type_vars)
    {
        // the kind should be an arrow kind.
        assert(k->is_karrow());
        auto& ka = dynamic_cast<const KindArrow&>(*k);

        // map the name to its kind
        bind_type_var(tv, ka.k1);

        // set up the next iteration
        k = ka.k2;
    }
    assert(k->is_kstar());

    // c. Handle the context
    kind_check_context(data_decl.context);

    // d. construct the data type
    Haskell::Type data_type = Haskell::TypeCon(Unlocated(data_decl.name));
    for(auto& tv: data_decl.type_vars)
        data_type = Haskell::TypeApp(data_type, tv);

    // e. Handle the constructor terms
    for(auto& constructor: data_decl.constructors)
        kind_check_constructor(constructor, data_type);

    pop_type_var_scope();
}

map<string,Haskell::Type> kindchecker_state::type_check_data_type(const Haskell::DataOrNewtypeDecl& data_decl)
{
    push_type_var_scope();

    // a. Look up kind for this data type.
    kind k = kind_check_type_con(data_decl.name);  // FIXME -- check that this is a data type?

    // b. Bind each type variable.
    vector<Haskell::TypeVar> datatype_typevars;
    for(auto& tv: data_decl.type_vars)
    {
        // the kind should be an arrow kind.
        assert(k->is_karrow());
        auto& ka = dynamic_cast<const KindArrow&>(*k);

        // map the name to its kind
        bind_type_var(tv, ka.k1);

        // record a version of the var with that contains its kind
        auto tv2 = tv;
        tv2.kind = ka.k1;
        datatype_typevars.push_back(tv2);

        // set up the next iteration
        k = ka.k2;
    }
    assert(k->is_kstar());

    // c. handle the context
    // The context should already be type-checked.
    // We should already have checked that it doesn't contain any unbound variables.

    // d. construct the data type
    Haskell::Type data_type = Haskell::TypeCon(Unlocated(data_decl.name));
    for(auto& tv: datatype_typevars)
        data_type = Haskell::TypeApp(data_type, tv);

    // e. Handle the constructor terms
    map<string,Haskell::Type> types;
    for(auto& constructor: data_decl.constructors)
    {
        // Constructors are not allowed to introduce new variables w/o GADT form, with a where clause.
        // This should have already been checked.

        Haskell::Type constructor_type = type_check_constructor(constructor, data_type);

        // QUESTION: do we need to replace the original user type vars with the new kind-annotated versions?
        if (datatype_typevars.size())
            constructor_type = Haskell::ForallType(datatype_typevars, constructor_type);

        types.insert({constructor.name, constructor_type});
    }

    pop_type_var_scope();

    return types;
}

void kindchecker_state::kind_check_class_method_type(const Haskell::Type& type)
{
    // 1. Bind type parameters for type declaration
    push_type_var_scope();

    std::optional<Haskell::Context> context;

    // 2. Find the unconstrained type
    auto unconstrained_type = type;
    if (unconstrained_type.is_a<Haskell::ConstrainedType>())
    {
        auto& ct = unconstrained_type.as_<Haskell::ConstrainedType>();
        context = ct.context;
        unconstrained_type = ct.type;
    }

    // 3. Find the free type variables
    auto ftvs = free_type_VARS_from_type(unconstrained_type);
    for(auto& ftv: ftvs)
    {
        if (not type_var_in_scope(ftv))
        {
            auto a = fresh_kind_var();
            bind_type_var(ftv,a);
        }
    }

    // 4. Check the context
    if (context)
        kind_check_context(*context);

    // 5. Check the unconstrained type
    kind_check_type_of_kind(unconstrained_type, make_kind_star());

    // 6. Unbind type parameters
    pop_type_var_scope();
}

void kindchecker_state::kind_check_type_class(const Haskell::ClassDecl& class_decl)
{
    // Bind type parameters for class
    push_type_var_scope();

    auto& name = class_decl.name;

    // a. Look up kind for this data type.
    auto k = kind_for_type_con(name); // FIXME -- check that this is a type class?

    // b. Put each type variable into the kind.
    kind k2 = make_kind_star();
    for(auto& tv: class_decl.type_vars)
    {
        // the kind should be an arrow kind.
        assert(k->is_karrow());
        auto& ka = dynamic_cast<const KindArrow&>(*k);

        // map the name to its kind
        bind_type_var(tv, ka.k1);

        // set up the next iteration
        k = ka.k2;
    }
    assert(k->is_kconstraint());

    if (class_decl.decls)
    {
        for(auto& decl: unloc(*class_decl.decls))
        {
            if (decl.is_a<Haskell::TypeDecl>())
            {
                auto& TD = decl.as_<Haskell::TypeDecl>();
                kind_check_class_method_type(TD.type);
            }
        }
    }

    pop_type_var_scope();
}

Haskell::Type kindchecker_state::type_check_class_method_type(Haskell::Type type, const Haskell::Type& constraint)
{
    // 1. Bind type parameters for type declaration
    push_type_var_scope();

    std::optional<Haskell::Context> context;

    // 2. Find the unconstrained type
    auto unconstrained_type = type;
    if (unconstrained_type.is_a<Haskell::ConstrainedType>())
    {
        auto& ct = unconstrained_type.as_<Haskell::ConstrainedType>();
        context = ct.context;
        unconstrained_type = ct.type;
    }

    // 3. Find the NEW free type variables
    auto new_ftvs = free_type_VARS_from_type(unconstrained_type);
    vector<Haskell::TypeVar> to_erase;
    for(auto& type_var: new_ftvs)
        if (type_var_in_scope(type_var))
            to_erase.push_back(type_var);
    for(auto& type_var: to_erase)
        new_ftvs.erase(type_var);

    // 4. Bind fresh kind vars to new type variables
    for(auto& ftv: new_ftvs)
    {
        auto a = fresh_kind_var();
        bind_type_var(ftv,a);
    }

    // 5. Check the context
    if (context)
        kind_check_context(*context);

    // 6. Check the unconstrained type and infer kinds.
    kind_check_type_of_kind(unconstrained_type, make_kind_star());

    // 7. Bind fresh kind vars to new type variables
    vector<Haskell::TypeVar> new_type_vars;
    for(auto& type_var: new_ftvs)
    {
        auto type_var_with_kind = type_var;
        type_var_with_kind.kind = replace_kvar_with_star( kind_for_type_var(type_var) );
        new_type_vars.push_back( type_var_with_kind );
    }

    // Don't we need to simplify constraints if we do this?
    type = add_context({constraint}, type);

    type = add_forall_vars(new_type_vars, type);
    
    // 6. Unbind type parameters
    pop_type_var_scope();

    return type;
}

map<string, Haskell::Type> kindchecker_state::type_check_type_class(const Haskell::ClassDecl& class_decl)
{
    // Bind type parameters for class
    push_type_var_scope();

    auto& name = class_decl.name;

    // a. Look up kind for this data type.
    auto k = kind_check_type_con(name);

    // b. Put each type variable into the kind.
    vector<Haskell::TypeVar> class_typevars;
    for(auto& tv: class_decl.type_vars)
    {
        // the kind should be an arrow kind.
        assert(k->is_karrow());
        auto& ka = dynamic_cast<const KindArrow&>(*k);

        // map the name to its kind
        bind_type_var(tv, ka.k1);

        // record a version of the var with that contains its kind
        auto tv2 = tv;
        tv2.kind = ka.k1;
        class_typevars.push_back(tv2);

        // set up the next iteration
        k = ka.k2;
    }
    assert(k->is_kconstraint());

    // d. construct the constraint
    Haskell::Type constraint = Haskell::TypeCon(Unlocated(class_decl.name));
    for(auto& tv: class_typevars)
        constraint = Haskell::TypeApp(constraint, tv);

    // e. handle the class methods
    map<string,Haskell::Type> types;
    if (class_decl.decls)
    {
        for(auto& decl: unloc(*class_decl.decls))
        {
            if (decl.is_a<Haskell::TypeDecl>())
            {
                auto& TD = decl.as_<Haskell::TypeDecl>();
                Haskell::Type method_type = type_check_class_method_type(TD.type, constraint);
                if (class_typevars.size())
                    method_type = add_forall_vars(class_typevars, method_type);
                for(auto& var: TD.vars)
                    types.insert({unloc(var.name), method_type});
            }
        }
    }

    pop_type_var_scope();

    return types;
}

/*
data C a => D a = Foo (S a)
type S a = [D a]
class C a where
   bar :: a -> D a -> Bool
*/

void kindchecker_state::kind_check_type_synonym(const Haskell::TypeSynonymDecl& type_syn_decl)
{
    // Bind type parameters for class
    push_type_var_scope();

    auto& name = type_syn_decl.name;

    // a. Look up kind for this data type.
    auto k = kind_for_type_con(name); // FIXME -- check that this is a type class?

    // b. Put each type variable into the kind.
    for(auto& tv: type_syn_decl.type_vars)
    {
        // the kind should be an arrow kind.
        assert(k->is_karrow());
        auto& ka = dynamic_cast<const KindArrow&>(*k);

        // map the name to its kind
        bind_type_var(tv, ka.k1);

        // set up the next iteration
        k = ka.k2;
    }
    assert(k->is_kstar());

    kind_check_type_of_kind( unloc(type_syn_decl.rhs_type), make_kind_star() );

    pop_type_var_scope();
}

std::map<string,std::pair<int,kind>> kindchecker_state::infer_kinds(const vector<expression_ref>& type_decl_group)
{
    // 1. Infer initial kinds for types and type classes
    for(auto& type_decl: type_decl_group)
    {
        string name;
        int arity;
        kind k;
        if (type_decl.is_a<Haskell::DataOrNewtypeDecl>())
        {
            auto& D = type_decl.as_<Haskell::DataOrNewtypeDecl>();
            name = D.name;
            arity = D.type_vars.size();
            k = make_kind_star();
        }
        else if (type_decl.is_a<Haskell::ClassDecl>())
        {
            auto& C = type_decl.as_<Haskell::ClassDecl>();
            name = C.name;
            arity = C.type_vars.size();
            k = make_kind_constraint();
        }
        else if (type_decl.is_a<Haskell::TypeSynonymDecl>())
        {
            auto & T = type_decl.as_<Haskell::TypeSynonymDecl>();
            name = T.name;
            arity = T.type_vars.size();
            k = make_kind_star();
        }

        // Create an initial kind here...
        for(int i=0;i<arity;i++)
            k = make_kind_arrow( fresh_kind_var(), k );

        add_type_con_of_kind(name, k);
    }

    // 2. Do kind inference for each declaration
    for(auto& type_decl: type_decl_group)
    {
        try
        {
            if (type_decl.is_a<Haskell::DataOrNewtypeDecl>())
            {
                auto& D = type_decl.as_<Haskell::DataOrNewtypeDecl>();
                kind_check_data_type( D );
            }
            else if (type_decl.is_a<Haskell::ClassDecl>())
            {
                auto& C = type_decl.as_<Haskell::ClassDecl>();
                kind_check_type_class( C );
            }
            else if (type_decl.is_a<Haskell::TypeSynonymDecl>())
            {
                auto & T = type_decl.as_<Haskell::TypeSynonymDecl>();
                kind_check_type_synonym( T );
            }
        }
        catch (myexception& e)
        {
            std::ostringstream o;
            o<<"\n  In declaration: "<<type_decl.print()<<"\n    ";
            e.prepend(o.str());
            throw;
        }
    }

    map<string,pair<int,kind>> kind_and_arity;
    for(auto& type_decl: type_decl_group)
    {
        string name;
        int arity;
        if (type_decl.is_a<Haskell::DataOrNewtypeDecl>())
        {
            auto& D = type_decl.as_<Haskell::DataOrNewtypeDecl>();
            name = D.name;
            arity = D.type_vars.size();
        }
        else if (type_decl.is_a<Haskell::ClassDecl>())
        {
            auto& C = type_decl.as_<Haskell::ClassDecl>();
            name = C.name;
            arity = C.type_vars.size();
        }
        else if (type_decl.is_a<Haskell::TypeSynonymDecl>())
        {
            auto & T = type_decl.as_<Haskell::TypeSynonymDecl>();
            name = T.name;
            arity = T.type_vars.size();
        }

        auto k = kind_for_type_con(name);
        k = replace_kvar_with_star(k);
        kind_and_arity.insert({name,{arity,k}});
    }

    return kind_and_arity;
}

std::map<string,std::pair<int,kind>> kindchecker_state::infer_child_types(const vector<expression_ref>& type_decl_group)
{
    // 1. Do kind inference for each declaration
    for(auto& type_decl: type_decl_group)
    {
        try
        {
            if (type_decl.is_a<Haskell::DataOrNewtypeDecl>())
            {
                auto& D = type_decl.as_<Haskell::DataOrNewtypeDecl>();
                type_check_data_type( D );
            }
            else if (type_decl.is_a<Haskell::ClassDecl>())
            {
                auto& C = type_decl.as_<Haskell::ClassDecl>();
                type_check_type_class( C );
            }
            else if (type_decl.is_a<Haskell::TypeSynonymDecl>())
            {
                // auto & T = type_decl.as_<Haskell::TypeSynonymDecl>();
                // Is there actually anything to do here?
                // Type synonyms don't have any children.
            }
        }
        catch (myexception& e)
        {
            std::ostringstream o;
            o<<"\n  In declaration: "<<type_decl.print()<<"\n    ";
            e.prepend(o.str());
            throw;
        }
    }

    return {};
}

// How can type variables come up?
    // * In type/class headers (context => T u1 u2 .. un)
    //   (Note that the context can't contain any variables other than u1..u[n].

    // For data/newtype declarations
    // * In types or constraints on individual data constructors
    //   (Again, only the variables u1...u[n] can occur.
    // * The type constructor T has kind k1 -> k2 -> ... -> k[n] -> *, where
    //     u[i] :: k[i]
    // * These type constructors can be partially applied.

    // For type synonym declarations
    // * there is no context
    // * the rhs can only contain u1....u[n]
    // * T has kind k[1] -> k[2] -> ... -> k[n] -> k  where u[i] :: k[i]
    //   and  k is the kind of the rhs.
    // * These type constructors CANNOT be partially applied.
    // * Recursive type synonyms are not allowed, unless an ADT intervenes.
    // * A synonym and its rhs are completely interchangeable, except in ... instance decls? (Section 4.3.2)

    // For newtype declarations
    // ?? they are more like data declarations, but shouldn't have runtime overhead.

    // For class declarations:
    // * Class variables are scoped over the body.
    // * the type for each method must mention the class variable u[1].
    // * the type for each method MAY mention other variables.
    // * constraints on a class method may ONLY mention NON-class variables. (but this is a "silly" rule)

