#include "kindcheck.H"
#include "range/v3/all.hpp"
#include "haskell/ids.H"
#include "util/set.H"   // for add( , )
#include "util/graph.H"
#include "typecheck.H"

using std::pair;
using std::map;
using std::set;
using std::string;
using std::tuple;
using std::optional;
using std::string;
using std::vector;

namespace views = ranges::views;


/* FIXME: We currently kind_check data types, and then duplicate the work to construct the DataConInfo.
  1. We first determine the constructor kinds.
     Here we do: kind_check_data_type() -> kind_check_constructor()
  2. Later we do: typecheck_data_type() -> type_check_constructor()

   Currently we must redo the kind-checking we do not record the kinds for the type variables.
   So we get the same errors the second time, but we need to redo it to find the kinds for the type variables.
*/

// FIXME: Simplify AST by replacing Context -> std::vector<Type>

/*
 * See Note: Error messages from the kind checker (below).
 */

TypeVar kindchecker_state::bind_type_var(const Hs::LTypeVar& ltv, const Kind& kind)
{
    // We can't modify the initial empty scope.
    assert(type_var_to_kind.size() > 1);

    auto& tvk = type_var_to_kind.back();
    if (tvk.count(ltv))
        tvk.erase(ltv);
    tvk.insert({ltv,kind});

    TypeVar tv2(unloc(ltv).name, kind);
    tv2.index = unloc(ltv).index;
    return tv2;
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

void kindchecker_state::add_type_con_of_kind(const string& name, const Kind& kind, int arity)
{
    assert(not type_con_to_kind.count(name));
    type_con_to_kind.insert({name,{kind,arity}});
}

Kind kindchecker_state::apply_substitution(const Kind& kind) const
{
    return apply_subst(kind_var_to_kind, kind);
}

void kindchecker_state::add_substitution(const KindVar& kv, const Kind& kind)
{
    assert(not kind_var_to_kind.count(kv));

    kind_var_to_kind = kind_var_to_kind.insert({kv,kind});
}


//FIXME -- is this the right order?
void kindchecker_state::add_substitution(const k_substitution_t& s)
{
    kind_var_to_kind = compose(s,kind_var_to_kind);
}

Kind kindchecker_state::kind_for_type_con(const std::string& name) const
{
    Kind kind;
    if (type_con_to_kind.count(name))
        kind = type_con_to_kind.at(name).kind;
    else
    {
        auto T = mod.lookup_resolved_type(name);
        assert(T);
        kind = T->kind;
    }

    return apply_substitution(kind);
}

Kind kindchecker_state::kind_for_type_var(const Hs::LTypeVar& ltv) const
{
    auto it = type_var_to_kind.back().find(ltv);
    if (it == type_var_to_kind.back().end())
    {
        type_checker.record_error(ltv.loc, Note()<<"Type variable '"<<unloc(ltv).print()<<"' not in scope");
        // Ideally we'd return fresh_kind_var() here.
        // But that requires this not to be const.
        return kind_type();
    }
    else
    {
        auto kind = it->second;
        return apply_subst(kind_var_to_kind, kind);
    }
}

bool kindchecker_state::unify(const Kind& kind1, const Kind& kind2)
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

Type kindchecker_state::kind_check_type_of_kind(const Hs::LType& t, const Kind& kind)
{
    auto [t2,kind2] = kind_check_type(t);
    t2 = zonk_kind_for_type(t2);
    if (not unify(kind, kind2))
    {
        if (t.loc) type_checker.push_source_span(*t.loc);
        type_checker.record_error(Note()<<"Type "<<t2<<" has kind "<<apply_substitution(kind2)<<", but should have kind "<<apply_substitution(kind)<<"\n");
        if (t.loc) type_checker.pop_source_span();
    }
    return t2;
}

Kind kindchecker_state::kind_check_type_con(const string& name)
{
    if (name == "Num#")
        return kind_type();
    else if (name == "Char")
        return kind_type();
    else if (name == "Double")
        return kind_type();
    else if (name == "Int")
        return kind_type();
    else if (name == "Integer")
        return kind_type();
    else if (name == "()")
        return kind_type();
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

tuple<Type,Kind> kindchecker_state::kind_check_type(const Hs::LType& ltype)
{
    auto& [loc, t] = ltype;

    if (auto tc = t.to<Hs::TypeCon>())
    {
        auto& name = tc->name;

        // FIXME: we should pass the location to kind_check_type_con( ).
        auto k = kind_check_type_con(name);
        TypeCon Tc(tc->name, k);

        return {Type(Tc),k};
    }
    else if (auto tv = t.to<Hs::TypeVar>())
    {
        auto k = kind_for_type_var({loc,*tv});
        TypeVar Tv(tv->name, k);

        return {Tv,k};
    }
    else if (auto tapp = t.to<Hs::TypeApp>())
    {
        auto [type1,kind1] = kind_check_type(tapp->head);
        auto [type2,kind2] = kind_check_type(tapp->arg);

        auto t2 = TypeApp(type1,type2);

        if (auto kv1 = kind1.to<KindVar>())
        {
            auto a1 = fresh_kind_var();
            auto a2 = fresh_kind_var();
            add_substitution(*kv1, kind_arrow(a1,a2));
            unify(a1, kind2); /// can't fail.
            return {t2,a2};
        }
        else if (auto a = is_function_type(kind1))
        {
	    auto& [arg_kind, result_kind] = *a;
            if (not unify(arg_kind, kind2))
            {
                if (ltype.loc) type_checker.push_source_span(*ltype.loc);
                type_checker.record_error(Note()<<"In type '"<<t<<"', can't apply type ("<<tapp->head<<" :: "<<apply_substitution(kind1)<<") to type ("<<tapp->arg<<" :: "<<apply_substitution(kind2)<<")");
                if (ltype.loc) type_checker.pop_source_span();
            }
            return {t2, result_kind};
        }
        else
        {
            if (loc) type_checker.push_source_span(*loc);
            type_checker.record_error(Note()<<"Can't apply type "<<tapp->head<<" :: "<<kind1.print()<<" to type "<<tapp->arg<<".");
            if (loc) type_checker.pop_source_span();

            return {t2, fresh_kind_var()};
        }
    }
    else if (auto c = t.to<Hs::ConstrainedType>())
    {
        Context context;
        for(auto& hs_constraint: c->context)
        {
            auto [c2,k2] = kind_check_type(hs_constraint);
            if (not unify(kind_constraint(), k2))
            {
                if (ltype.loc) type_checker.push_source_span(*ltype.loc);
		type_checker.record_error(Note()<<"Constraint '"<<hs_constraint.print()<<"' should be a Constraint, but has kind "<<k2.print());
                if (ltype.loc) type_checker.pop_source_span();
            }
            context.push_back(c2);
        }
        auto [type,k] = kind_check_type(c->type);

        return {Type(ConstrainedType(context, type)), k};
    }
    else if (auto fa = t.to<Hs::ForallType>())
    {
        push_type_var_scope();

        vector<TypeVar> binders;
        for(auto& lhtv: fa->type_var_binders)
        {
            auto& [loc,htv] = lhtv;

            Kind kind;
            if (htv.kind)
                kind = *htv.kind;
            else
                kind = fresh_kind_var();
            auto tv = bind_type_var(lhtv, kind);

            binders.push_back(tv);
        }

        auto [type,kind] = kind_check_type(fa->type);

        // to construct a new type, perhaps we should create an ForallType, with the kind of each variable set to the fresh kind.
        // however, afterwards, we have to zonk the final TYPE to fill in KIND variables.

        pop_type_var_scope();

        return {ForallType(binders, type), kind};
    }
    else if (auto list_type = t.to<Hs::ListType>())
    {
        Hs::Type type2 = Hs::TypeApp({noloc,Hs::TypeCon("[]")}, list_type->element_type);
        return kind_check_type({loc, type2});
    }
    else if (auto tuple_type = t.to<Hs::TupleType>())
    {
        int n = tuple_type->element_types.size();
        Hs::LType tuple_tycon(noloc, Hs::TypeCon(tuple_name(n)));
        return kind_check_type( type_apply(tuple_tycon, tuple_type->element_types) );
    }
    else if (auto st = t.to<Hs::StrictType>())
    {
        if (ltype.loc) type_checker.push_source_span(*ltype.loc);
        type_checker.record_error(Note()<<"Internal strictness mark not allowed");
        if (ltype.loc) type_checker.pop_source_span();
        return kind_check_type( st->type );
    }
    else if (auto lt = t.to<Hs::LazyType>())
    {
        if (ltype.loc) type_checker.push_source_span(*ltype.loc);
        type_checker.record_error(Note()<<"Internal strictness mark not allowed");
        if (ltype.loc) type_checker.pop_source_span();
        return kind_check_type( lt->type );
    }

    throw myexception()<<"kind_check_type: I don't recognize type '"<<t.print()<<"'";
}

Type kindchecker_state::zonk_kind_for_type(const Type& t)
{
    if (auto tc = t.to<TypeCon>())
    {
        auto& name = tc->name;

        auto Tc = *tc;
        Tc.kind = kind_check_type_con(name);
        return Tc;
    }
    else if (auto tv = t.to<TypeVar>())
    {
        auto Tv = *tv;
        Tv.kind = replace_kvar_with_star(Tv.kind);
        return Tv;
    }
    else if (auto tapp = t.to<TypeApp>())
    {
        auto Tapp = *tapp;
        Tapp.head = zonk_kind_for_type(Tapp.head);
        Tapp.arg  = zonk_kind_for_type(Tapp.arg);
        return Tapp;
    }
    else if (auto c = t.to<ConstrainedType>())
    {
        auto C = *c;
        for(auto& constraint: C.context)
            constraint = zonk_kind_for_type( constraint );

        C.type = zonk_kind_for_type( C.type );
        return C;
    }
    else if (auto fa = t.to<ForallType>())
    {
        auto Fa = *fa;

        for(auto& tv: Fa.type_var_binders)
            tv.kind = replace_kvar_with_star(tv.kind);

        Fa.type = zonk_kind_for_type( Fa.type );

        return Fa;
    }
    
    throw myexception()<<"zonk_kind_for_type: I don't recognize type '"<<t.print()<<"'";
}


Type kindchecker_state::kind_check_constraint(const Hs::LType& constraint)
{
    return kind_check_type_of_kind(constraint, kind_constraint());
}

Context kindchecker_state::kind_check_context(const Hs::Context& hs_context)
{
    Context context;
    for(auto& hs_constraint: hs_context)
        context.push_back( kind_check_constraint(hs_constraint) );
    return context;
}

std::pair<Hs::LType,bool> pop_strictness(Hs::LType ltype);

void kindchecker_state::kind_check_constructor(const Hs::ConstructorDecl& constructor)
{
    // FIXME: So much duplicated code with kind_check_constructor!  Can we fix?

    // This requires {-# LANGUAGE ExistentialQuantification #-}
    push_type_var_scope();
    for(auto& htv: constructor.forall)
    {
        auto kv = fresh_kind_var();
        auto tv = bind_type_var(htv, kv);
    }

    for(auto& constraint: constructor.context)
        kind_check_type_of_kind(constraint, kind_constraint());

    if (constructor.is_record_constructor())
    {
        auto& fields = std::get<1>(constructor.fields);

        for(auto& field_decl: fields.field_decls | views::reverse)
        {
            auto [field_type, _] = pop_strictness(field_decl.type);

            kind_check_type_of_kind(field_type, kind_type());
        }
    }
    else
    {
        vector<Type> field_types;
        for(auto& hs_field_type: std::get<0>(constructor.fields))
        {
            auto [field_type,_] = pop_strictness(hs_field_type);
            kind_check_type_of_kind(field_type, kind_type());
        }
    }

    pop_type_var_scope();
}

void kindchecker_state::kind_check_data_type(Hs::DataOrNewtypeDecl& data_decl)
{
    push_type_var_scope();

    // a. Look up kind for this data type.
    auto kind = kind_for_type_con(unloc(data_decl.con).name);  // FIXME -- check that this is a data type?

    // b. construct the data type
    Type data_type = TypeCon(unloc(data_decl.con).name, kind);
    for(auto& ltv: data_decl.type_vars)
    {
        // the kind should be an arrow kind.
        auto [arg_kind, result_kind] = is_function_type(kind).value();

        // map the name to its kind
        auto tv = bind_type_var(ltv, arg_kind);

        data_type = TypeApp(data_type, tv);

        // set up the next iteration
        kind = result_kind;
    }
    assert(is_kind_type(kind));

    // c. Handle the context
    for(auto& constraint: data_decl.context)
        kind_check_constraint(constraint);

    // d. Handle regular constructor terms (class variables ARE in scope)
    if (data_decl.is_regular_decl())
    {
        for(auto& constructor: data_decl.get_constructors())
            kind_check_constructor(constructor);
    }

    pop_type_var_scope();

    // f. Handle GADT constructor terms (class variables are NOT in scope)
    if (data_decl.is_gadt_decl())
    {
        // BUG: We don't handle strictness annotations on the fields here!

        // We should ensure that these types follow: forall univ_tvs. stupid_theta => forall ex_tvs. written_type
        for(auto& data_cons_decl: data_decl.get_gadt_constructors())
        {
            // FIXME! Handle strict fields: Con :: forall b. C b a => a -> !b -> T Int
            // FIXME! Handle record fields: Con :: { field1, field2 :: Int } -> T Int
            // OK: Con :: T Int
            // OK: Con :: a -> b -> T Int
            kind_and_type_check_type(data_cons_decl.type);
        }
    }
}

Type kindchecker_state::kind_and_type_check_type(const Hs::LType& type)
{
    return kind_check_type_of_kind(type, kind_type() );
}

Type kindchecker_state::kind_and_type_check_constraint(const Hs::LType& type)
{
    return kind_check_type_of_kind(type, kind_constraint() );
}

void kindchecker_state::kind_check_type_class(const Hs::ClassDecl& class_decl)
{
    // Bind type parameters for class
    push_type_var_scope();

    auto& name = unloc(class_decl.con).name;

    // a. Look up kind for this data type.
    auto k = kind_for_type_con(name); // FIXME -- check that this is a type class?

    // b. Put each type variable into the kind.
    Kind k2 = kind_type();
    for(auto& tv: class_decl.type_vars)
    {
        // the kind should be an arrow kind.
	auto [arg_kind, result_kind] = is_function_type(k).value();

        // map the name to its kind
        bind_type_var(tv, arg_kind);

        // set up the next iteration
        k = result_kind;
    }
    assert(is_kind_constraint(k));

    for(auto& sig_decl: class_decl.sig_decls)
        kind_and_type_check_type(sig_decl.type);

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

    auto& name = unloc(type_syn_decl.con).name;

    // a. Look up kind for this data type.
    auto k = kind_for_type_con(name); // FIXME -- check that this is a type class?

    // b. Put each type variable into the kind.
    for(auto& ltv: type_syn_decl.type_vars)
    {
        // the kind should be an arrow kind.
        auto [arg_kind, result_kind] = is_function_type(k).value();

        // map the name to its kind
        bind_type_var(ltv, arg_kind);

        // set up the next iteration
        k = result_kind;
    }
    assert(is_kind_type(k));

    kind_check_type_of_kind( type_syn_decl.rhs_type, kind_type() );

    pop_type_var_scope();
}

TypeConEnv kindchecker_state::infer_kinds(const vector<expression_ref>& type_decl_group)
{
    // 1. Infer initial kinds for types and type classes
    TypeConEnv new_tycons;
    for(auto& type_decl: type_decl_group)
    {
        string name;
        int arity;
        Kind kind;
        if (type_decl.is_a<Hs::DataOrNewtypeDecl>())
        {
            auto& D = type_decl.as_<Hs::DataOrNewtypeDecl>();
            name = unloc(D.con).name;
            arity = D.type_vars.size();
            kind = kind_type();
        }
        else if (type_decl.is_a<Hs::ClassDecl>())
        {
            auto& C = type_decl.as_<Hs::ClassDecl>();
            name = unloc(C.con).name;
            arity = C.type_vars.size();
            kind = kind_constraint();
        }
        else if (type_decl.is_a<Hs::TypeSynonymDecl>())
        {
            auto & T = type_decl.as_<Hs::TypeSynonymDecl>();
            name = unloc(T.con).name;
            arity = T.type_vars.size();
            kind = kind_type();
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

kindchecker_state::kindchecker_state(TypeChecker& tc)
    :mod(tc.this_mod()),type_checker(tc)
{
    type_var_to_kind.push_back({});
}
