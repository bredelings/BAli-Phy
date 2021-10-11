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

set<Haskell::TypeVar> free_type_VARS_from_type(const Haskell::Type& type)
{
    // This version finds varids -- the other version finds qualified names.
    set<Haskell::TypeVar> tvars;
    if (type.is_a<Haskell::TypeVar>())
    {
        auto& tv = type.as_<Haskell::TypeVar>();
        auto& name = unloc(tv.name);
        assert(name.size());
        if (is_haskell_varid(name))
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
        {
            if (type_var.is_a<Haskell::TypeVar>())
            {
                auto& tv = type_var.as_<Haskell::TypeVar>();
                tvars.erase(tv);
            }
            if (type_var.is_a<Haskell::TypeVarOfKind>())
            {
                std::abort();
            }
        }
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
    if (t.is_a<Haskell::TypeVar>())
    {
        auto& tv = t.as_<Haskell::TypeVar>();
        auto& name = unloc(tv.name);

        if (is_haskell_varid(name))
            return kind_check_type_var(tv);
        else
            return kind_check_type_con(name);
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
        Haskell::Type list_con = Haskell::TypeVar({{},"[]"});
        Haskell::Type list_type = Haskell::TypeApp(list_con, L.element_type);
        return kind_check_type(list_type);
    }
    else if (t.is_a<Haskell::TupleType>())
    {
        auto& T = t.as_<Haskell::TupleType>();
        auto n = T.element_types.size();
        Haskell::Type tuple_type = Haskell::TypeVar({{},tuple_name(n)});
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

void kindchecker_state::kind_check_data_type(const Haskell::DataOrNewtypeDecl& data_decl)
{
    push_type_var_scope();

    // a. Look up kind for this data type.
    kind k = kind_for_type_con(data_decl.name);  // FIXME -- check that this is a data type?

    // b. Put each type variable into the kind.
    kind k2 = make_kind_star();
    for(auto& tv: data_decl.type_vars | views::reverse)
    {
        auto a = fresh_kind_var();
        bind_type_var(tv,a);
        k2 = make_kind_arrow(a,k2);
    }
    bool ok = unify(k,k2);
    assert(ok);

    // c. Handle the context
    kind_check_context(data_decl.context);

    // d. construct the data type
    Haskell::Type data_type = Haskell::TypeVar({{},data_decl.name});
    for(auto& tv: data_decl.type_vars)
        data_type = Haskell::TypeApp(data_type, tv);

    // e. Handle the constructor terms
    for(auto& constructor: data_decl.constructors)
        kind_check_constructor(constructor, data_type);

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
    for(auto& tv: class_decl.type_vars | views::reverse)
    {
        auto a = fresh_kind_var();
        bind_type_var(tv,a);
        k2 = make_kind_arrow(a,k2);
    }
    bool ok = unify(k,k2);
    assert(ok);

    if (class_decl.decls)
    {
        for(auto& decl: unloc(*class_decl.decls))
        {
            if (decl.is_a<Haskell::TypeDecl>())
            {
                // Bind type parameters for type declaration
                push_type_var_scope();

                std::optional<Haskell::Context> context;

                auto type = decl.as_<Haskell::TypeDecl>().type;

                if (type.is_a<Haskell::ConstrainedType>())
                {
                    auto& ct = type.as_<Haskell::ConstrainedType>();
                    context = ct.context;
                    type = ct.type;
                }

                auto ftvs = free_type_VARS_from_type(type);
                for(auto& ftv: ftvs)
                {
                    if (not type_var_in_scope(ftv))
                    {
                        auto a = fresh_kind_var();
                        bind_type_var(ftv,a);
                    }
                }

                if (context)
                    kind_check_context(*context);
                kind_check_type_of_kind(type, make_kind_star());

                pop_type_var_scope();
            }
        }
    }

    pop_type_var_scope();
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
    kind k2 = make_kind_star();
    for(auto& tv: type_syn_decl.type_vars | views::reverse)
    {
        auto a = fresh_kind_var();
        bind_type_var(tv,a);
        k2 = make_kind_arrow(a,k2);
    }
    bool ok = unify(k,k2);
    assert(ok);

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
//                auto& D = type_decl.as_<Haskell::DataOrNewtypeDecl>();
//                kind_check_data_type( D );
            }
            else if (type_decl.is_a<Haskell::ClassDecl>())
            {
//                auto& C = type_decl.as_<Haskell::ClassDecl>();
//                kind_check_type_class( C );
            }
            else if (type_decl.is_a<Haskell::TypeSynonymDecl>())
            {
//                auto & T = type_decl.as_<Haskell::TypeSynonymDecl>();
//                kind_check_type_synonym( T );
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

