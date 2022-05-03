#include "kindcheck.H"
#include "range/v3/all.hpp"
#include "expression/tuple.H"
#include "util/set.H"   // for add( , )
#include "util/graph.H"

using std::pair;
using std::map;
using std::set;
using std::string;
using std::tuple;
using std::optional;
using std::string;
using std::vector;

namespace views = ranges::views;

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

set<string> free_type_vars(const Hs::Context&);

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
    for(auto& constr: type_decl.constructors)
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

type_con_env get_tycon_info(const Hs::Decls& type_decls)
{
    type_con_env tce;

    auto type_decl_groups = find_type_groups(type_decls);

    // 3. Compute kinds for type/class constructors.
    for(auto& type_decl_group: type_decl_groups)
    {
        kindchecker_state K(tce);
        auto new_tycons = K.infer_kinds(type_decl_group);
        tce = plus_no_overlap(tce, new_tycons);
    }

    return tce;
}

// Q: how/when do we rename default method definitions?

Hs::Type add_constraints(const vector<Hs::Type>& constraints, const Hs::Type& type)
{
    if (constraints.empty())
        return type;
    else if (type.is_a<Hs::ConstrainedType>())
    {
        auto CT = type.as_<Hs::ConstrainedType>();
        for(auto& constraint: constraints)
            CT.context.constraints.push_back(constraint);
        return CT;
    }
    else
        return Hs::ConstrainedType(Hs::Context(constraints),type);
}

Hs::Type add_forall_vars(const std::vector<Hs::TypeVar>& type_vars, const Hs::Type& type)
{
    for(auto& tv: type_vars)
        assert(tv.kind);

    if (type_vars.empty())
        return type;
    else if (auto FAT = type.to<Hs::ForallType>())
    {
        auto new_type_vars = type_vars;
        for(auto& type_var: FAT->type_var_binders)
        {
            assert(not includes(type_vars, type_var));
            new_type_vars.push_back(type_var);
        }
        return Hs::ForallType(new_type_vars, FAT->type);
    }
    else
        return Hs::ForallType(type_vars, type);
}

set<Hs::TypeVar> free_type_VARS(const Hs::Context& context)
{
    set<Hs::TypeVar> tvars;
    for(auto& constraint: context.constraints)
        add(tvars, free_type_VARS(constraint));
    return tvars;
}

set<Hs::TypeVar> free_type_VARS(const Hs::Type& type)
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
        add(tvars, free_type_VARS(app.head));
        add(tvars, free_type_VARS(app.arg));
    }
    else if (type.is_a<Hs::TupleType>())
    {
        auto& tuple = type.as_<Hs::TupleType>();
        for(auto element_type: tuple.element_types)
            add(tvars, free_type_VARS(element_type));
    }
    else if (type.is_a<Hs::ListType>())
    {
        auto& list = type.as_<Hs::ListType>();
        add(tvars, free_type_VARS(list.element_type));
    }
    else if (type.is_a<Hs::ForallType>())
    {
        auto& forall = type.as_<Hs::ForallType>();
        tvars = free_type_VARS(forall.type);
        for(auto& type_var: forall.type_var_binders)
            tvars.erase(type_var);
    }
    else if (auto c = type.to<Hs::ConstrainedType>())
    {
        add(tvars, free_type_VARS(c->context));
        add(tvars, free_type_VARS(c->type));
    }
    else if (auto sl = type.to<Hs::StrictLazyType>())
    {
        return free_type_VARS(sl->type);
    }
    else
        std::abort();

    return tvars;
}

bool kindchecker_state::type_var_in_scope(const Hs::TypeVar& tv) const
{
    return type_var_to_kind.back().count(tv);
}

void kindchecker_state::bind_type_var(const Hs::TypeVar& tv, const Hs::Kind& kind)
{
    // We can't modify the initial empty scope.
    assert(type_var_to_kind.size() > 1);

    auto& tvk = type_var_to_kind.back();
    if (tvk.count(tv))
        tvk.erase(tv);
    tvk.insert({tv,kind});
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

void kindchecker_state::add_type_con_of_kind(const string& name, const Hs::Kind& kind, int arity)
{
    assert(not type_con_to_kind.count(name));
    type_con_to_kind.insert({name,{kind,arity}});
}

Hs::Kind kindchecker_state::apply_substitution(const Hs::Kind& kind) const
{
    return apply_subst(kind_var_to_kind, kind);
}

void kindchecker_state::add_substitution(const KindVar& kv, const Hs::Kind& kind)
{
    assert(not kind_var_to_kind.count(kv));

    kind_var_to_kind.insert({kv,kind});
}


//FIXME -- is this the right order?
void kindchecker_state::add_substitution(const k_substitution_t& s)
{
    kind_var_to_kind = compose(s,kind_var_to_kind);
}

Hs::Kind kindchecker_state::kind_for_type_con(const std::string& name) const
{
    auto kind = type_con_to_kind.at(name).kind;
    return apply_substitution(kind);
}


Hs::Kind kindchecker_state::kind_for_type_var(const Hs::TypeVar& tv) const
{
    auto it = type_var_to_kind.back().find(tv);
    if (it == type_var_to_kind.back().end())
        throw myexception()<<"Can't find type variable '"<<tv.print()<<"'";
    auto kind = it->second;
    return apply_subst(kind_var_to_kind, kind);
}

bool kindchecker_state::unify(const Hs::Kind& kind1, const Hs::Kind& kind2)
{
    auto s = ::kunify(kind1,kind2);
    if (s)
    {
        add_substitution(*s);
        return true;
    }
    else
        return false;
}

void kindchecker_state::kind_check_type_of_kind(const Hs::Type& t, const Hs::Kind& kind)
{
    auto kind2 = kind_check_type(t);
    if (not unify(kind, kind2))
        throw myexception()<<"Type "<<t<<" has kind "<<apply_substitution(kind2)<<", but should have kind "<<apply_substitution(kind)<<"\n";
}

Hs::Kind kindchecker_state::kind_check_type_var(const Hs::TypeVar& tv)
{
    return kind_for_type_var(tv);
}

Hs::Kind kindchecker_state::kind_check_type_con(const string& name)
{
    if (name == "Num#")
        return kind_star();
    else if (name == "Char")
        return kind_star();
    else if (name == "Double")
        return kind_star();
    else if (name == "Int")
        return kind_star();
    else if (name == "()")
        return kind_star();
    else if (name == "[]")
        return make_n_args_kind(1);
    else if (name == "->")
        return make_n_args_kind(2);
    else if (is_tuple_name(name))
    {
        int n = tuple_arity(name);

        return make_n_args_kind(n);
    }
    else
        return kind_for_type_con(name);
}

Hs::Kind kindchecker_state::kind_check_type(const Hs::Type& t)
{
    if (auto tc = t.to<Hs::TypeCon>())
    {
        auto& name = unloc(tc->name);

        return kind_check_type_con(name);
    }
    else if (auto tv = t.to<Hs::TypeVar>())
    {
        return kind_check_type_var(*tv);
    }
    else if (t.is_a<Hs::TypeApp>())
    {
        auto& tapp = t.as_<Hs::TypeApp>();

        auto kind1 = kind_check_type(tapp.head);
        auto kind2 = kind_check_type(tapp.arg);

        if (auto kv1 = kind1.to<KindVar>())
        {
            auto a1 = fresh_kind_var();
            auto a2 = fresh_kind_var();
            add_substitution(*kv1, kind_arrow(a1,a2));
            unify(a1, kind2); /// can't fail.
            return a2;
        }
        else if (auto a = kind1.to<KindArrow>())
        {
            if (not unify(a->k1, kind2))
                throw myexception()<<"In type '"<<t<<"', can't apply type ("<<tapp.head<<" :: "<<apply_substitution(kind1)<<") to type ("<<tapp.arg<<" :: "<<apply_substitution(kind2)<<")";
            return a->k2;
        }
        else
            throw myexception()<<"Can't apply type "<<tapp.head<<" :: "<<kind1.print()<<" to type "<<tapp.arg<<".";

    }
    else if (t.is_a<Hs::ListType>())
    {
        auto& L = t.as_<Hs::ListType>();
        Hs::Type list_con = Hs::TypeCon(Unlocated("[]"));
        Hs::Type list_type = Hs::TypeApp(list_con, L.element_type);
        return kind_check_type(list_type);
    }
    else if (t.is_a<Hs::TupleType>())
    {
        auto& T = t.as_<Hs::TupleType>();
        auto n = T.element_types.size();
        Hs::Type tuple_type = Hs::TypeCon(Unlocated(tuple_name(n)));
        for(auto& element_type: T.element_types)
            tuple_type = Hs::TypeApp(tuple_type, element_type);
        return kind_check_type(tuple_type);
    }
    else if (auto c = t.to<Hs::ConstrainedType>())
    {
        kind_check_context(c->context);
        return kind_check_type(c->type);
    }

    throw myexception()<<"I don't recognize type '"<<t.print()<<"'";
}


void kindchecker_state::kind_check_constraint(const Hs::Type& constraint)
{
    return kind_check_type_of_kind(constraint, kind_constraint());
}

void kindchecker_state::kind_check_context(const Hs::Context& context)
{
    for(auto& constraint: context.constraints)
        kind_check_constraint(constraint);
}

void kindchecker_state::kind_check_constructor(const Hs::Constructor& constructor, const Hs::Type& data_type)
{
    auto type2 = data_type;

    if (constructor.is_record_constructor())
    {
        auto& fields = std::get<1>(constructor.fields);

        for(auto& field_decl: fields.field_decls | views::reverse)
        {
            for(int i=0;i<field_decl.field_names.size();i++)
                type2 = make_arrow_type(field_decl.type, type2);
        }
    }
    else
    {
        auto& types = std::get<0>(constructor.fields);

        for(auto& type: types | views::reverse)
            type2 = make_arrow_type(type, type2);

    }

    // FIXME: how about constraints?
    // Perhaps constraints on constructors lead to records that contain pointers to dictionaries??

    kind_check_type_of_kind(type2, kind_star());
}

Hs::Type kindchecker_state::type_check_constructor(const Hs::Constructor& constructor, const Hs::Type& data_type)
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
                type2 = make_arrow_type(field_decl.type, type2);
        }
    }
    else
    {
        auto& types = std::get<0>(constructor.fields);

        for(auto& type: types | views::reverse)
            type2 = make_arrow_type(type, type2);

    }

    // FIXME: we need to check that these constraints constrain fields of the constructor.
    if (constructor.context)
        type2 = add_constraints(constructor.context->constraints, type2);

    return type2;
}

void kindchecker_state::kind_check_data_type(const Hs::DataOrNewtypeDecl& data_decl)
{
    push_type_var_scope();

    // a. Look up kind for this data type.
    auto kind = kind_for_type_con(data_decl.name);  // FIXME -- check that this is a data type?

    // b. Put each type variable into the kind.
    for(auto& tv: data_decl.type_vars)
    {
        // the kind should be an arrow kind.
        auto ka = kind.to<KindArrow>();
        assert(ka);

        // map the name to its kind
        bind_type_var(tv, ka->k1);

        // set up the next iteration
        kind = ka->k2;
    }
    assert(kind.is_a<KindStar>());

    // c. Handle the context
    kind_check_context(data_decl.context);

    // d. construct the data type
    Hs::Type data_type = Hs::TypeCon(Unlocated(data_decl.name));
    for(auto& tv: data_decl.type_vars)
        data_type = Hs::TypeApp(data_type, tv);

    // e. Handle the constructor terms
    for(auto& constructor: data_decl.constructors)
        kind_check_constructor(constructor, data_type);

    pop_type_var_scope();
}

map<string,Hs::Type> kindchecker_state::type_check_data_type(const Hs::DataOrNewtypeDecl& data_decl)
{
    push_type_var_scope();

    // a. Look up kind for this data type.
    auto k = kind_for_type_con(data_decl.name);  // FIXME -- check that this is a data type?

    // b. Bind each type variable.
    vector<Hs::TypeVar> datatype_typevars;
    for(auto& tv: data_decl.type_vars)
    {
        // the kind should be an arrow kind.
        auto ka = k.to<KindArrow>();
        assert(ka);

        // map the name to its kind
        bind_type_var(tv, ka->k1);

        // record a version of the var with that contains its kind
        auto tv2 = tv;
        tv2.kind = ka->k1;
        datatype_typevars.push_back(tv2);

        // set up the next iteration
        k = ka->k2;
    }
    assert(k.is_a<KindStar>());

    // c. handle the context
    // The context should already be type-checked.
    // We should already have checked that it doesn't contain any unbound variables.

    // d. construct the data type
    Hs::Type data_type = Hs::TypeCon(Unlocated(data_decl.name));
    for(auto& tv: datatype_typevars)
        data_type = Hs::TypeApp(data_type, tv);

    // e. Handle the constructor terms
    map<string,Hs::Type> types;
    for(auto& constructor: data_decl.constructors)
    {
        // Constructors are not allowed to introduce new variables w/o GADT form, with a where clause.
        // This should have already been checked.

        Hs::Type constructor_type = type_check_constructor(constructor, data_type);

        // Actually we should only add the constraints that use type variables that are used in the constructor.
        constructor_type = add_constraints(data_decl.context.constraints, constructor_type);

        // QUESTION: do we need to replace the original user type vars with the new kind-annotated versions?
        if (datatype_typevars.size())
            constructor_type = Hs::ForallType(datatype_typevars, constructor_type);

        types.insert({constructor.name, constructor_type});
    }

    pop_type_var_scope();

    return types;
}

Hs::Type kindchecker_state::kind_and_type_check_type(const Hs::Type& type)
{
    return kind_and_type_check_type_(type, kind_star() );
}

Hs::Type kindchecker_state::kind_and_type_check_constraint(const Hs::Type& type)
{
    return kind_and_type_check_type_(type, kind_constraint() );
}

Hs::Type kindchecker_state::kind_and_type_check_type_(const Hs::Type& type, const Hs::Kind& kind)
{
    // 1. Bind type parameters for type declaration
    push_type_var_scope();

    std::optional<Hs::Context> context;

    // 2. Find the NEW free type variables
    auto new_ftvs = free_type_VARS(type);
    vector<Hs::TypeVar> to_erase;
    for(auto& type_var: new_ftvs)
        if (type_var_in_scope(type_var))
            to_erase.push_back(type_var);
    for(auto& type_var: to_erase)
        new_ftvs.erase(type_var);

    // 3. Bind fresh kind vars to new type variables
    for(auto& ftv: new_ftvs)
    {
        auto a = fresh_kind_var();
        bind_type_var(ftv,a);
    }

    // 4. Check the unconstrained type and infer kinds.
    kind_check_type_of_kind(type, kind);

    // 5. Bind fresh kind vars to new type variables
    vector<Hs::TypeVar> new_type_vars;
    for(auto& type_var: new_ftvs)
    {
        auto type_var_with_kind = type_var;
        type_var_with_kind.kind = replace_kvar_with_star( kind_for_type_var(type_var) );
        new_type_vars.push_back( type_var_with_kind );
    }

    
    // 6. Unbind type parameters
    pop_type_var_scope();

    // 7. Add foralls with the appropriate kinds.
    return add_forall_vars(new_type_vars, type);
}

void kindchecker_state::kind_check_type_class(const Hs::ClassDecl& class_decl)
{
    // Bind type parameters for class
    push_type_var_scope();

    auto& name = class_decl.name;

    // a. Look up kind for this data type.
    auto k = kind_for_type_con(name); // FIXME -- check that this is a type class?

    // b. Put each type variable into the kind.
    Hs::Kind k2 = kind_star();
    for(auto& tv: class_decl.type_vars)
    {
        // the kind should be an arrow kind.
        auto ka = k.to<KindArrow>();
        assert(ka);

        // map the name to its kind
        bind_type_var(tv, ka->k1);

        // set up the next iteration
        k = ka->k2;
    }
    assert(k.is_a<KindConstraint>());

    if (class_decl.binds)
    {
        for(auto& [name,type]: unloc(*class_decl.binds).signatures)
            kind_and_type_check_type(type);
    }

    pop_type_var_scope();
}

/*
data C a => D a = Foo (S a)
type S a = [D a]
class C a where
   bar :: a -> D a -> Bool
*/

void kindchecker_state::kind_check_type_synonym(const Hs::TypeSynonymDecl& type_syn_decl)
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
        auto ka = k.to<KindArrow>();
        assert(ka);

        // map the name to its kind
        bind_type_var(tv, ka->k1);

        // set up the next iteration
        k = ka->k2;
    }
    assert(k.is_a<KindStar>());

    kind_check_type_of_kind( unloc(type_syn_decl.rhs_type), kind_star() );

    pop_type_var_scope();
}

type_con_env kindchecker_state::infer_kinds(const vector<expression_ref>& type_decl_group)
{
    // 1. Infer initial kinds for types and type classes
    type_con_env new_tycons;
    for(auto& type_decl: type_decl_group)
    {
        string name;
        int arity;
        Hs::Kind kind;
        if (type_decl.is_a<Hs::DataOrNewtypeDecl>())
        {
            auto& D = type_decl.as_<Hs::DataOrNewtypeDecl>();
            name = D.name;
            arity = D.type_vars.size();
            kind = kind_star();
        }
        else if (type_decl.is_a<Hs::ClassDecl>())
        {
            auto& C = type_decl.as_<Hs::ClassDecl>();
            name = C.name;
            arity = C.type_vars.size();
            kind = kind_constraint();
        }
        else if (type_decl.is_a<Hs::TypeSynonymDecl>())
        {
            auto & T = type_decl.as_<Hs::TypeSynonymDecl>();
            name = T.name;
            arity = T.type_vars.size();
            kind = kind_star();
        }
        else
            std::abort();

        // Create an initial kind here...
        for(int i=0;i<arity;i++)
            kind = kind_arrow( fresh_kind_var(), kind );

        add_type_con_of_kind(name, kind, arity);
        new_tycons.insert({name,{kind,arity}});
    }

    // 2. Do kind inference for each declaration
    for(auto& type_decl: type_decl_group)
    {
        try
        {
            if (type_decl.is_a<Hs::DataOrNewtypeDecl>())
            {
                auto& D = type_decl.as_<Hs::DataOrNewtypeDecl>();
                kind_check_data_type( D );
            }
            else if (type_decl.is_a<Hs::ClassDecl>())
            {
                auto& C = type_decl.as_<Hs::ClassDecl>();
                kind_check_type_class( C );
            }
            else if (type_decl.is_a<Hs::TypeSynonymDecl>())
            {
                auto & T = type_decl.as_<Hs::TypeSynonymDecl>();
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

    // 3. Construct the mapping from new tycon -> (kind,arity)
    for(auto& [name,info]: new_tycons)
    {
        // Lookup kind and do substitutions
        auto kind = kind_for_type_con(name);
        kind = replace_kvar_with_star(kind);

        auto& [kind_out,arity] = info;
        kind_out = kind;
    }

    return new_tycons;
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

