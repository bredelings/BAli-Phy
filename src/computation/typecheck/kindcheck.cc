#include "kindcheck.H"
#include "range/v3/all.hpp"
#include "haskell/ids.H"
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

void kindchecker_state::kind_check_type_of_kind(Hs::Type& t, const Hs::Kind& kind)
{
    auto kind2 = kind_check_type(t);
    t = zonk_kind_for_type(t);
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
    else if (name == "~")
        return make_n_args_constraint_kind(2);
    else if (is_tuple_name(name))
    {
        int n = tuple_arity(name);

        return make_n_args_kind(n);
    }
    else
        return kind_for_type_con(name);
}

Hs::Kind kindchecker_state::kind_check_type(Hs::Type& t)
{
    if (auto tc = t.to<Hs::TypeCon>())
    {
        auto& name = unloc(tc->name);

        auto k = kind_check_type_con(name);
        auto Tc = *tc;
        Tc.kind = k;

        t = Tc;
        return k;
    }
    else if (auto tv = t.to<Hs::TypeVar>())
    {
        auto k = kind_for_type_var(*tv);

        auto Tv = *tv;
        Tv.kind = k;

        t = Tv;
        return k;
    }
    else if (auto tapp = t.to<Hs::TypeApp>())
    {
        auto Tapp = *tapp;

        auto kind1 = kind_check_type(Tapp.head);
        auto kind2 = kind_check_type(Tapp.arg);

        t = Tapp;

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
                throw myexception()<<"In type '"<<t<<"', can't apply type ("<<Tapp.head<<" :: "<<apply_substitution(kind1)<<") to type ("<<Tapp.arg<<" :: "<<apply_substitution(kind2)<<")";
            return a->k2;
        }
        else
            throw myexception()<<"Can't apply type "<<Tapp.head<<" :: "<<kind1.print()<<" to type "<<Tapp.arg<<".";
    }
    else if (auto l = t.to<Hs::ListType>())
    {
        auto L = *l;

        unify(kind_star(), kind_check_type(L.element_type));

        t = L;
        return kind_star();
    }
    else if (auto tup = t.to<Hs::TupleType>())
    {
        auto T = *tup;
        for(auto& element_type: T.element_types)
            unify(kind_star(), kind_check_type(element_type));

        t = T;
        return kind_star();
    }
    else if (auto c = t.to<Hs::ConstrainedType>())
    {
        auto C = *c;

        for(auto& constraint: C.context.constraints)
            unify(kind_constraint(), kind_check_type(constraint));
        auto k = kind_check_type(C.type);

        t = C;
        return k;
    }
    else if (auto fa = t.to<Hs::ForallType>())
    {
        auto Fa = *fa;
        push_type_var_scope();

        vector<Hs::TypeVar> binders2;
        for(auto& tv: Fa.type_var_binders)
        {
            auto k = fresh_kind_var();
            tv.kind = k;
            bind_type_var(tv, *tv.kind);
        }

        auto kind = kind_check_type(Fa.type);

        // to construct a new type, perhaps we should create an Hs::ForallType, with the kind of each variable set to the fresh kind.
        // however, afterwards, we have to zonk the final TYPE to fill in KIND variables.

        pop_type_var_scope();

        t = Fa;
        return kind;
    }

    throw myexception()<<"kind_check_type: I don't recognize type '"<<t.print()<<"'";
}

Hs::Kind kindchecker_state::kind_for_type(const Hs::Type& t)
{
    if (auto tc = t.to<Hs::TypeCon>())
        return *(tc->kind);
    else if (auto tv = t.to<Hs::TypeVar>())
        return apply_substitution(*tv->kind);
    else if (auto tapp = t.to<Hs::TypeApp>())
    {
        // Get the kind of the type being applied
        auto hkind = kind_for_type(tapp->head);

        // The kind should be k1 -> k2
        if (auto ka = hkind.to<KindArrow>())
            return ka->k2;
        else
            throw myexception()<<"Kind of applied tycon is not an arrow kind!";
    }
    else if (t.is_a<Hs::ListType>())
    {
        return kind_star();
    }
    else if (t.is_a<Hs::TupleType>())
    {
        return kind_star();
    }
    else if (auto c = t.to<Hs::ConstrainedType>())
    {
        return kind_for_type(c->type);
    }
    else if (auto fa = t.to<Hs::ForallType>())
    {
        push_type_var_scope();

        for(auto& tv: fa->type_var_binders)
        {
            assert(tv.kind);
            bind_type_var(tv, apply_substitution(*tv.kind));
        }

        auto kind = kind_for_type( fa->type );

        pop_type_var_scope();

        return kind;
    }

    throw myexception()<<"kind_for_type: I don't recognize type '"<<t.print()<<"'";
}

Hs::Type kindchecker_state::zonk_kind_for_type(const Hs::Type& t)
{
    if (auto tc = t.to<Hs::TypeCon>())
    {
        auto& name = unloc(tc->name);

        auto Tc = *tc;
        Tc.kind = kind_check_type_con(name);
        return Tc;
    }
    else if (auto tv = t.to<Hs::TypeVar>())
    {
        auto kind = replace_kvar_with_star(kind_for_type_var(*tv));

        auto Tv = *tv;
        Tv.kind = kind;
        return Tv;
    }
    else if (auto tapp = t.to<Hs::TypeApp>())
    {
        auto Tapp = *tapp;
        Tapp.head = zonk_kind_for_type(Tapp.head);
        Tapp.arg  = zonk_kind_for_type(Tapp.arg);
        return Tapp;
    }
    else if (auto l = t.to<Hs::ListType>())
    {
        auto L = *l;
        L.element_type = zonk_kind_for_type( L.element_type );
        return L;
    }
    else if (auto tup = t.to<Hs::TupleType>())
    {
        auto T = *tup;
        for(auto& element_type: T.element_types)
            element_type = zonk_kind_for_type( element_type );
        return T;
    }
    else if (auto c = t.to<Hs::ConstrainedType>())
    {
        auto C = *c;
        for(auto& constraint: C.context.constraints)
            constraint = zonk_kind_for_type( constraint );

        C.type = zonk_kind_for_type( C.type );
        return C;
    }
    else if (auto fa = t.to<Hs::ForallType>())
    {
        auto Fa = *fa;

        push_type_var_scope();

        for(auto& tv: Fa.type_var_binders)
        {
            assert(tv.kind);
            tv.kind = replace_kvar_with_star(*tv.kind);
            bind_type_var(tv, *tv.kind);
        }

        Fa.type = zonk_kind_for_type( Fa.type );

        pop_type_var_scope();

        return Fa;
    }

    throw myexception()<<"zonk_kind_for_type: I don't recognize type '"<<t.print()<<"'";
}


void kindchecker_state::kind_check_constraint(Hs::Type& constraint)
{
    return kind_check_type_of_kind(constraint, kind_constraint());
}

void kindchecker_state::kind_check_context(Hs::Context& context)
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
                type2 = Hs::make_arrow_type(field_decl.type, type2);
        }
    }
    else
    {
        auto& types = std::get<0>(constructor.fields);

        for(auto& type: types | views::reverse)
            type2 = Hs::make_arrow_type(type, type2);
    }

    // FIXME: how about constraints?
    // Perhaps constraints on constructors lead to records that contain pointers to dictionaries??

    // This requires {-# LANGUAGE ExistentialQuantification #-} ?
    type2 = add_forall_vars( constructor.forall, type2 );

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
                type2 = Hs::make_arrow_type(field_decl.type, type2);
        }
    }
    else
    {
        auto& types = std::get<0>(constructor.fields);

        for(auto& type: types | views::reverse)
            type2 = Hs::make_arrow_type(type, type2);

    }

    // FIXME: we need to check that these constraints constrain fields of the constructor.
    if (constructor.context)
        type2 = Hs::add_constraints(constructor.context->constraints, type2);

    return type2;
}

void kindchecker_state::kind_check_data_type(Hs::DataOrNewtypeDecl& data_decl)
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
        constructor_type = Hs::add_constraints(data_decl.context.constraints, constructor_type);

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

/*
  So.... how would we kind check a forall-type? 
  - If it was top-level, we'd bind the forall variables to fresh kind vars, and estimate the kind...
  - I guess we could record the kind on the forall variables themselves.
  - Then after we decide what all the kind variables are, we could go replace all the kind variables with
    their final substituted type.
 */

vector<Hs::TypeVar> kindchecker_state::unbound_free_type_variables(const Hs::Type& type)
{
    vector<Hs::TypeVar> ftvs;
    for(auto& type_var: free_type_variables(type))
        if (not type_var_in_scope(type_var))
            ftvs.push_back(type_var);
    return ftvs;
}

Hs::Type kindchecker_state::kind_and_type_check_type_(const Hs::Type& type, const Hs::Kind& kind)
{
    // 1. Find the NEW free type variables
    auto new_ftvs = unbound_free_type_variables(type);
    Hs::Type type2 = add_forall_vars(new_ftvs, type);

    kind_check_type_of_kind(type2, kind);

    return type2;
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

void kindchecker_state::kind_check_type_synonym(Hs::TypeSynonymDecl& type_syn_decl)
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
                auto D = type_decl.as_<Hs::DataOrNewtypeDecl>();
                kind_check_data_type( D );
            }
            else if (type_decl.is_a<Hs::ClassDecl>())
            {
                auto& C = type_decl.as_<Hs::ClassDecl>();
                kind_check_type_class( C );
            }
            else if (type_decl.is_a<Hs::TypeSynonymDecl>())
            {
                auto T = type_decl.as_<Hs::TypeSynonymDecl>();
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

