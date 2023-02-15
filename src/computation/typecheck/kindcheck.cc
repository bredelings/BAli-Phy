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


bool kindchecker_state::type_var_in_scope(const TypeVar& tv) const
{
    return type_var_to_kind.back().count(tv);
}

void kindchecker_state::bind_type_var(const TypeVar& tv, const Kind& kind)
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

    kind_var_to_kind.insert({kv,kind});
}


//FIXME -- is this the right order?
void kindchecker_state::add_substitution(const k_substitution_t& s)
{
    kind_var_to_kind = compose(s,kind_var_to_kind);
}

Kind kindchecker_state::kind_for_type_con(const std::string& name) const
{
    auto kind = type_con_to_kind.at(name).kind;
    return apply_substitution(kind);
}


Kind kindchecker_state::kind_for_type_var(const TypeVar& tv) const
{
    auto it = type_var_to_kind.back().find(tv);
    if (it == type_var_to_kind.back().end())
        throw myexception()<<"Can't find type variable '"<<tv.print()<<"'";
    auto kind = it->second;
    return apply_subst(kind_var_to_kind, kind);
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

Type kindchecker_state::kind_check_type_of_kind(const Type& t, const Kind& kind)
{
    auto [t2,kind2] = kind_check_type(t);
    t2 = zonk_kind_for_type(t2);
    if (not unify(kind, kind2))
        throw myexception()<<"Type "<<t2<<" has kind "<<apply_substitution(kind2)<<", but should have kind "<<apply_substitution(kind)<<"\n";
    return t2;
}

Kind kindchecker_state::kind_check_type_var(const TypeVar& tv)
{
    return kind_for_type_var(tv);
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

tuple<Type,Kind> kindchecker_state::kind_check_type(const Type& t)
{
    if (auto tc = t.to<TypeCon>())
    {
        auto& name = unloc(tc->name);

        auto k = kind_check_type_con(name);
        auto Tc = *tc;
        Tc.kind = k;

        return {Tc,k};
    }
    else if (auto tv = t.to<TypeVar>())
    {
        auto k = kind_for_type_var(*tv);

        auto Tv = *tv;
        Tv.kind = k;

        return {Tv,k};
    }
    else if (auto tapp = t.to<TypeApp>())
    {
        auto Tapp = *tapp;

        auto [type1,kind1] = kind_check_type(Tapp.head);
        auto [type2,kind2] = kind_check_type(Tapp.arg);

        auto t2 = TypeApp(type1,type2);

        if (auto kv1 = kind1.to<KindVar>())
        {
            auto a1 = fresh_kind_var();
            auto a2 = fresh_kind_var();
            add_substitution(*kv1, kind_arrow(a1,a2));
            unify(a1, kind2); /// can't fail.
            return {t2,a2};
        }
        else if (auto a = kind1.to<KindArrow>())
        {
            if (not unify(a->arg_kind, kind2))
                throw myexception()<<"In type '"<<t<<"', can't apply type ("<<Tapp.head<<" :: "<<apply_substitution(kind1)<<") to type ("<<Tapp.arg<<" :: "<<apply_substitution(kind2)<<")";
            return {t2, a->result_kind};
        }
        else
            throw myexception()<<"Can't apply type "<<Tapp.head<<" :: "<<kind1.print()<<" to type "<<Tapp.arg<<".";
    }
    else if (auto c = t.to<ConstrainedType>())
    {
        auto C = *c;

        for(auto& constraint: C.context.constraints)
        {
            auto [c2,k2] = kind_check_type(constraint);
            unify(kind_constraint(), k2);
            constraint = c2;
        }
        auto [cc,k] = kind_check_type(C.type);
        C.type = cc;

        return {C,k};
    }
    else if (auto fa = t.to<ForallType>())
    {
        auto Fa = *fa;
        push_type_var_scope();

        vector<TypeVar> binders2;
        for(auto& tv: Fa.type_var_binders)
        {
            tv.kind = fresh_kind_var();
            bind_type_var(tv, *tv.kind);
        }

        auto [type,kind] = kind_check_type(Fa.type);
        Fa.type = type;

        // to construct a new type, perhaps we should create an ForallType, with the kind of each variable set to the fresh kind.
        // however, afterwards, we have to zonk the final TYPE to fill in KIND variables.

        pop_type_var_scope();

        return {Fa, kind};
    }

    throw myexception()<<"kind_check_type: I don't recognize type '"<<t.print()<<"'";
}

Kind kindchecker_state::kind_for_type(const Type& t)
{
    if (auto tc = t.to<TypeCon>())
        return *(tc->kind);
    else if (auto tv = t.to<TypeVar>())
        return apply_substitution(*tv->kind);
    else if (auto tapp = t.to<TypeApp>())
    {
        // Get the kind of the type being applied
        auto hkind = kind_for_type(tapp->head);

        // The kind should be k1 -> k2
        if (auto ka = hkind.to<KindArrow>())
            return ka->result_kind;
        else
            throw myexception()<<"Kind of applied tycon is not an arrow kind!";
    }
    else if (auto c = t.to<ConstrainedType>())
    {
        return kind_for_type(c->type);
    }
    else if (auto fa = t.to<ForallType>())
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

Type kindchecker_state::zonk_kind_for_type(const Type& t)
{
    if (auto tc = t.to<TypeCon>())
    {
        auto& name = unloc(tc->name);

        auto Tc = *tc;
        Tc.kind = kind_check_type_con(name);
        return Tc;
    }
    else if (auto tv = t.to<TypeVar>())
    {
        auto kind = replace_kvar_with_star(kind_for_type_var(*tv));

        auto Tv = *tv;
        Tv.kind = kind;
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
        for(auto& constraint: C.context.constraints)
            constraint = zonk_kind_for_type( constraint );

        C.type = zonk_kind_for_type( C.type );
        return C;
    }
    else if (auto fa = t.to<ForallType>())
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


Type kindchecker_state::kind_check_constraint(const Type& constraint)
{
    return kind_check_type_of_kind(constraint, kind_constraint());
}

Context kindchecker_state::kind_check_context(const Context& context)
{
    Context context2 = context;
    for(auto& constraint: context2.constraints)
        constraint = kind_check_constraint(constraint);
    return context2;
}

void kindchecker_state::kind_check_constructor(const Hs::ConstructorDecl& constructor, const Type& data_type)
{
    auto type2 = data_type;

    if (constructor.is_record_constructor())
    {
        auto& fields = std::get<1>(constructor.fields);

        for(auto& field_decl: fields.field_decls | views::reverse)
        {
            for(int i=0;i<field_decl.field_names.size();i++)
                type2 = make_arrow_type(desugar(field_decl.type), type2);
        }
    }
    else
    {
        auto types = desugar(std::get<0>(constructor.fields));

        for(auto& type: types | views::reverse)
            type2 = make_arrow_type(type, type2);
    }

    // Are we allowed to write constraints here?
    if (constructor.context)
        type2 = ::add_constraints( desugar(constructor.context->constraints), type2);

    // This requires {-# LANGUAGE ExistentialQuantification #-}
    type2 = add_forall_vars( desugar(constructor.forall), type2 );

    kind_check_type_of_kind(type2, kind_type());
}

DataConInfo kindchecker_state::type_check_constructor(const Hs::ConstructorDecl& constructor)
{
    DataConInfo info;

    // 1. Record exi_tvs, written_constraints, and field_types
    info.exi_tvs = desugar(constructor.forall);

    if (constructor.context)
        info.written_constraints = desugar(constructor.context->constraints);

    if (constructor.is_record_constructor())
    {
        for(auto& field_decl: std::get<1>(constructor.fields).field_decls)
            for(int i=0; i < field_decl.field_names.size(); i++)
                info.field_types.push_back( desugar(field_decl.type) );
    }
    else
        info.field_types = desugar( std::get<0>(constructor.fields) );

    // 2. Make up kind vars for exi_tvs
    push_type_var_scope();
    for(auto& tv: info.exi_tvs)
    {
        tv.kind = fresh_kind_var();
        bind_type_var(tv, *tv.kind);
    }

    // 3. Infer kind for exi_tvs
    for(auto& field_type: info.field_types)
    {
        auto [t2,k2] = kind_check_type(field_type);
        field_type = t2;
        unify(kind_type(), k2);
    }
    for(auto& constraint: info.written_constraints)
    {
        auto [t2,k2] = kind_check_type(constraint);
        unify(kind_constraint(), k2);
        constraint = t2;
    }
    
    // 4. Substitute and replace kind vars
    for(auto& field_type: info.field_types)
        field_type = zonk_kind_for_type( field_type );
    for(auto& constraint: info.written_constraints)
        constraint = zonk_kind_for_type( constraint );
    for(auto& tv : info.exi_tvs)
        tv.kind = apply_substitution(*tv.kind);

    pop_type_var_scope();

    return info;
}

void kindchecker_state::kind_check_data_type(Hs::DataOrNewtypeDecl& data_decl)
{
    push_type_var_scope();

    // a. Look up kind for this data type.
    auto kind = kind_for_type_con(unloc(data_decl.name));  // FIXME -- check that this is a data type?

    // b. Put each type variable into the kind.
    for(auto& tv: desugar(data_decl.type_vars))
    {
        // the kind should be an arrow kind.
        auto ka = kind.to<KindArrow>();
        assert(ka);

        // map the name to its kind
        bind_type_var(tv, ka->arg_kind);

        // set up the next iteration
        kind = ka->result_kind;
    }
    assert(kind.is_a<KindStar>());

    // c. Handle the context
    for(auto& constraint: desugar(data_decl.context.constraints))
        kind_check_constraint(constraint);

    // d. construct the data type
    Type data_type = TypeCon(data_decl.name);
    for(auto& tv: desugar(data_decl.type_vars))
        data_type = TypeApp(data_type, tv);

    // e. Handle regular constructor terms (class variables ARE in scope)
    if (data_decl.is_regular_decl())
    {
        for(auto& constructor: data_decl.get_constructors())
            kind_check_constructor(constructor, data_type);
    }

    pop_type_var_scope();

    // f. Handle GADT constructor terms (class variables are NOT in scope)
    if (data_decl.is_gadt_decl())
    {
        // We should ensure that these types follow: forall univ_tvs. stupid_theta => forall ex_tvs. written_type
        for(auto& data_cons_decl: data_decl.get_gadt_constructors())
            kind_and_type_check_type(desugar(data_cons_decl.type));
    }
}

DataConEnv kindchecker_state::type_check_data_type(FreshVarSource& fresh_vars, const Hs::DataOrNewtypeDecl& data_decl)
{
    push_type_var_scope();

    // a. Look up kind for this data type.
    auto k = kind_for_type_con(unloc(data_decl.name));  // FIXME -- check that this is a data type?

    // b. Bind each type variable.
    vector<TypeVar> datatype_typevars;
    for(auto& tv: desugar(data_decl.type_vars))
    {
        // the kind should be an arrow kind.
        auto ka = k.to<KindArrow>();
        assert(ka);

        // map the name to its kind
        bind_type_var(tv, ka->arg_kind);

        // record a version of the var with that contains its kind
        auto tv2 = tv;
        tv2.kind = ka->arg_kind;
        datatype_typevars.push_back(tv2);

        // set up the next iteration
        k = ka->result_kind;
    }
    assert(k.is_a<KindStar>());

    // c. handle the context
    // The context should already be type-checked.
    // We should already have checked that it doesn't contain any unbound variables.

    // d. construct the data type
    auto data_type_con = TypeCon(data_decl.name);
    Type data_type = data_type_con;
    for(auto& tv: datatype_typevars)
        data_type = TypeApp(data_type, tv);

    // e. Handle regular constructor terms (class variables ARE in scope)
    DataConEnv types;
    if (data_decl.is_regular_decl())
    {
        for(auto& constructor: data_decl.get_constructors())
        {
            DataConInfo info = type_check_constructor(constructor);
            info.uni_tvs = datatype_typevars;
            info.data_type = data_type_con;
            info.top_constraints = desugar(data_decl.context.constraints);
            types = types.insert({unloc(*constructor.con).name, info});
        }
    }

    pop_type_var_scope();

    // f. Handle GADT constructor terms (class variables are NOT in scope)
    if (data_decl.is_gadt_decl())
    {
        for(auto& data_cons_decl: data_decl.get_gadt_constructors())
        {
            DataConInfo info;

            // 1. Kind-check and add foralls for free type vars.
            auto written_type = desugar(data_cons_decl.type);
            written_type = kind_and_type_check_type( written_type );

            // 2. Extract tyvar, givens, and rho type.
            auto [written_tvs, written_constraints, rho_type] = peel_top_gen( written_type );

            // 3. Get field types and result_type
            auto [field_types, result_type] = ::arg_result_types(rho_type);

            // 4. Check result type name
            auto [con, args] = decompose_type_apps(result_type);
            if (con != data_type_con)
                throw myexception()<<"constructor result '"<<result_type<<"' doesn't match data type "<< data_type_con;

            // Failure here would have triggered a kind error earlier.
            assert(args.size() == data_decl.type_vars.size());

            // 5. Create equality constraints and universal vars
            vector<TypeVar> u_tvs;
            auto constraints = written_constraints;
            for(int i=0;i<args.size();i++)
            {
                auto& arg = args[i];

                if (auto tv = arg.to<TypeVar>())
                {
                    u_tvs.push_back(*tv);
                }
                else
                {
                    auto u_tv = fresh_vars.fresh_other_type_var(*unloc(data_decl.type_vars[i]).kind);
                    u_tvs.push_back(u_tv);
                    info.gadt_eq_constraints.push_back(make_equality_pred(u_tv,arg));
                }
            }

            // 6. Get existential vars
            set<TypeVar> exi_tvs_set = written_tvs | ranges::to<set>;
            for(auto& u_tv: u_tvs)
                exi_tvs_set.erase(u_tv);

            info.field_types = field_types;
            info.data_type = data_type_con;
            info.written_constraints = constraints;
            info.top_constraints = desugar( data_decl.context.constraints );
            info.exi_tvs = exi_tvs_set | ranges::to<vector>();
            info.uni_tvs = u_tvs;
            
            for(auto& con_name: data_cons_decl.con_names)
                types = types.insert({unloc(con_name), info});
        }
    }

    return types;
}

Type kindchecker_state::kind_and_type_check_type(const Type& type)
{
    return kind_and_type_check_type_(type, kind_type() );
}

Type kindchecker_state::kind_and_type_check_constraint(const Type& type)
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

vector<TypeVar> kindchecker_state::unbound_free_type_variables(const Type& type)
{
    vector<TypeVar> ftvs;
    for(auto& type_var: free_type_variables(type))
        if (not type_var_in_scope(type_var))
            ftvs.push_back(type_var);
    return ftvs;
}

Type kindchecker_state::kind_and_type_check_type_(const Type& type, const Kind& kind)
{
    // 1. Find the NEW free type variables
    auto new_ftvs = unbound_free_type_variables(type);
    Type type2 = add_forall_vars(new_ftvs, type);

    return kind_check_type_of_kind(type2, kind);
}

void kindchecker_state::kind_check_type_class(const Hs::ClassDecl& class_decl)
{
    // Bind type parameters for class
    push_type_var_scope();

    auto& name = unloc(class_decl.name);

    // a. Look up kind for this data type.
    auto k = kind_for_type_con(name); // FIXME -- check that this is a type class?

    // b. Put each type variable into the kind.
    Kind k2 = kind_type();
    for(auto& tv: desugar(class_decl.type_vars))
    {
        // the kind should be an arrow kind.
        auto ka = k.to<KindArrow>();
        assert(ka);

        // map the name to its kind
        bind_type_var(tv, ka->arg_kind);

        // set up the next iteration
        k = ka->result_kind;
    }
    assert(k.is_a<KindConstraint>());

    for(auto& sig_decl: class_decl.sig_decls)
        kind_and_type_check_type(desugar(sig_decl.type));

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

    auto& name = unloc(type_syn_decl.name);

    // a. Look up kind for this data type.
    auto k = kind_for_type_con(name); // FIXME -- check that this is a type class?

    // b. Put each type variable into the kind.
    for(auto& tv: desugar(type_syn_decl.type_vars))
    {
        // the kind should be an arrow kind.
        auto ka = k.to<KindArrow>();
        assert(ka);

        // map the name to its kind
        bind_type_var(tv, ka->arg_kind);

        // set up the next iteration
        k = ka->result_kind;
    }
    assert(k.is_a<KindStar>());

    kind_check_type_of_kind( desugar(type_syn_decl.rhs_type), kind_type() );

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
            name = unloc(D.name);
            arity = D.type_vars.size();
            kind = kind_type();
        }
        else if (type_decl.is_a<Hs::ClassDecl>())
        {
            auto& C = type_decl.as_<Hs::ClassDecl>();
            name = unloc(C.name);
            arity = C.type_vars.size();
            kind = kind_constraint();
        }
        else if (type_decl.is_a<Hs::TypeSynonymDecl>())
        {
            auto & T = type_decl.as_<Hs::TypeSynonymDecl>();
            name = unloc(T.name);
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

