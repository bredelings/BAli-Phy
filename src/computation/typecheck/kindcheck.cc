#include "kindcheck.H"
#include "range/v3/all.hpp"
#include "haskell/ids.H"
#include "haskell/desugar_type.H"
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

TypeVar kindchecker_state::bind_type_var(const Hs::LTypeVar& hs_tv, const Kind& kind)
{
    TypeVar tv(hs_tv.value().name, kind);
    bind_type_var(tv, kind);
    return tv;
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
    auto& [loc, tv] = ltv;
    if (loc) type_checker.push_source_span(*loc);
    auto result = kind_for_type_var(desugar(ltv));
    if (loc) type_checker.pop_source_span();
    return result;
}

Kind kindchecker_state::kind_for_type_var(const TypeVar& tv) const
{
    auto it = type_var_to_kind.back().find(tv);
    if (it == type_var_to_kind.back().end())
    {
        type_checker.record_error(Note()<<"Type variable '"<<tv.print()<<"' not in scope");
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
        for(auto& htv: fa->type_var_binders)
        {
            auto kv = fresh_kind_var();
            auto tv = bind_type_var(htv, kv);
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
        if (auto ka = is_function_type(hkind))
            return ka->second;
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
        auto& name = tc->name;

        auto Tc = *tc;
        Tc.kind = kind_check_type_con(name);
        return Tc;
    }
    else if (auto tv = t.to<TypeVar>())
    {
        Kind kind;
        if (tv->kind)
            kind = *tv->kind;
        else
            kind = kind_for_type_var(*tv);
        kind = replace_kvar_with_star(kind);

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
        for(auto& constraint: C.context)
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

std::pair<Hs::LType,bool> pop_strictness(Hs::LType ltype)
{
    bool strictness = false;
    auto& [loc, type] = ltype;

    if (auto strict_type = type.to<Hs::StrictType>())
    {
        strictness = true;
        ltype = strict_type->type;
    }
    return {ltype, strictness};
}

void kindchecker_state::kind_check_constructor(const Hs::ConstructorDecl& constructor)
{
    // FIXME: So much duplicated code with kind_check_constructor!  Can we fix?

    // This requires {-# LANGUAGE ExistentialQuantification #-}
    push_type_var_scope();
    for(auto& htv: constructor.forall)
    {
        auto kv = fresh_kind_var();
        bind_type_var(htv, kv);
        auto tv = desugar(htv);
        tv.kind = kv;
    }

    if (constructor.context)
    {
        for(auto& constraint: *constructor.context)
            kind_check_type_of_kind(constraint, kind_constraint());
    }

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

DataConInfo kindchecker_state::type_check_constructor(const Hs::ConstructorDecl& constructor)
{
    // FIXME: So much duplicated code with kind_check_constructor!  Can we fix?

    DataConInfo info;

    // 1. Record exi_tvs and make up kind vars for them.
    push_type_var_scope();
    for(auto& htv: constructor.forall)
    {
        auto k = fresh_kind_var();
        bind_type_var(htv, k);
        auto tv = desugar(htv);
        tv.kind = k;
        info.exi_tvs.push_back(tv);
    }

    // 2. Record written_constraints
    //   Do constraints affect kind determination?  Maybe not
    if (constructor.context)
    {
        for(auto& constraint: *constructor.context)
            info.written_constraints.push_back( kind_check_type_of_kind(constraint, kind_constraint()) );
    }

    // 3. Kind check field types
    if (constructor.is_record_constructor())
    {
        for(auto& field_decl: std::get<1>(constructor.fields).field_decls)
            for(int i=0; i < field_decl.field_names.size(); i++)
            {
                auto [field_type, strictness] = pop_strictness(field_decl.type);
                info.field_types.push_back( kind_check_type_of_kind(field_type, kind_type() ) );
                info.field_strictness.push_back( strictness );
            }
    }
    else
    {
        for(auto& hs_field_type: std::get<0>(constructor.fields))
        {
            auto [field_type, strictness] = pop_strictness(hs_field_type);
            info.field_types.push_back( kind_check_type_of_kind(field_type, kind_type() ) );
            info.field_strictness.push_back( strictness );
        }
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
    for(auto& ltv: data_decl.type_vars)
    {
        // the kind should be an arrow kind.
        auto [arg_kind, result_kind] = is_function_type(kind).value();

        // map the name to its kind
        bind_type_var(ltv, arg_kind);

        // set up the next iteration
        kind = result_kind;
    }
    assert(is_kind_type(kind));

    // c. Handle the context
    for(auto& constraint: data_decl.context)
        kind_check_constraint(constraint);

    // d. construct the data type
    Type data_type = TypeCon(unloc(data_decl.name));
    for(auto& tv: desugar(data_decl.type_vars))
        data_type = TypeApp(data_type, tv);

    // e. Handle regular constructor terms (class variables ARE in scope)
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

DataConEnv kindchecker_state::type_check_data_type(FreshVarSource& fresh_vars, const Hs::DataOrNewtypeDecl& data_decl)
{
    push_type_var_scope();

    // a. Look up kind for this data type.
    auto k = kind_for_type_con(unloc(data_decl.name));  // FIXME -- check that this is a data type?

    // b. Bind each type variable.
    vector<TypeVar> datatype_typevars;
    for(auto& tv: data_decl.type_vars)
    {
        // the kind should be an arrow kind.
	auto [arg_kind, result_kind] = is_function_type(k).value();

        // map the name to its kind
        bind_type_var(tv, arg_kind);

        // record a version of the var with that contains its kind
        auto tv2 = desugar(tv);
        tv2.kind = arg_kind;
        datatype_typevars.push_back(tv2);

        // set up the next iteration
        k = result_kind;
    }
    assert(is_kind_type(k));

    // c. handle the context
    // The context should already be type-checked.
    // We should already have checked that it doesn't contain any unbound variables.

    // d. construct the data type
    auto data_type_con = TypeCon(unloc(data_decl.name));
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
            info.top_constraints = desugar(data_decl.context);
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

            // BUG: We don't handle strictness annotations on the fields here!
            auto written_type = kind_and_type_check_type( data_cons_decl.type );

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
            info.top_constraints = desugar( data_decl.context );
            info.exi_tvs = exi_tvs_set | ranges::to<vector>();
            info.uni_tvs = u_tvs;
            
            for(auto& con_name: data_cons_decl.con_names)
                types = types.insert({unloc(con_name), info});
        }
    }

    return types;
}

/* NOTE: Error messages from the kind checker.
 *
 * Currently error messages don't have locations information.
 * In order to get better error messages, we need to:
 * - pass a mutable reference to the typechecker (maybe?)
 * - have location information.
 *
 * Typically we also do check_type(desugar(type)).
 * - We should probably do this in one step.
 * - We could remove locations from core types.
 * - We could make kinds mandatory in Core types.
 * - Do we need to eliminate default-constructed types/kinds (NOTYPE)?
 *   - We might need to make some types optional<Type>, e.g. the parser.
 */

Type kindchecker_state::kind_and_type_check_type(const Hs::LType& type)
{
    return kind_and_type_check_type_(type, kind_type() );
}

Type kindchecker_state::kind_and_type_check_constraint(const Hs::LType& type)
{
    return kind_and_type_check_type_(type, kind_constraint() );
}

vector<Hs::LTypeVar> kindchecker_state::unbound_free_type_variables(const Hs::LType& type)
{
    // FIXME: can we remove this if we quantify types in rename?
    vector<Hs::LTypeVar> tvs;
    for(auto& tv: unbound_free_type_variables(desugar(type)))
    {
        Hs::TypeVar htv(tv.name);
        tvs.push_back({noloc, htv});
    }
    return tvs;
}

vector<TypeVar> kindchecker_state::unbound_free_type_variables(const Type& type)
{
    vector<TypeVar> ftvs;
    for(auto& type_var: free_type_variables(type))
        if (not type_var_in_scope(type_var))
            ftvs.push_back(type_var);
    return ftvs;
}

Type kindchecker_state::kind_and_type_check_type_(const Hs::LType& type, const Kind& kind)
{
    // 1. Find the NEW free type variables
    auto new_ftvs = unbound_free_type_variables(type);

    auto type2 = Hs::add_forall_vars(new_ftvs, type);
    
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

    auto& name = unloc(type_syn_decl.name);

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

kindchecker_state::kindchecker_state(TypeChecker& tc)
    :mod(tc.this_mod()),type_checker(tc)
{
    type_var_to_kind.push_back({});
}
