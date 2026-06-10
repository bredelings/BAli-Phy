#include "typecheck.H"
#include "rename/rename.H"

#include <functional>

using std::vector;
using std::set;
using std::pair;
using std::map;

namespace
{
    Hs::LType gadt_constructor_result_type(Hs::LType type)
    {
        auto [stripped_type, _] = pop_constructor_signature_strictness(type);
        auto [tvs, context, rho_type] = Hs::peel_top_gen(stripped_type);

        while(auto function_type = Hs::is_function_type(rho_type))
            rho_type = function_type->second;

        return rho_type;
    }

    std::pair<vector<Hs::LType>, Hs::LType> gadt_constructor_arg_result_types(Hs::LType type)
    {
        auto [stripped_type, _] = pop_constructor_signature_strictness(type);
        auto [tvs, context, rho_type] = Hs::peel_top_gen(stripped_type);

        vector<Hs::LType> args;
        while(auto function_type = Hs::is_function_type(rho_type))
        {
            args.push_back(function_type->first);
            rho_type = function_type->second;
        }

        return {args, rho_type};
    }

    std::optional<yy::location> find_type_var_loc(const Hs::LType& type, const TypeVar& tv)
    {
        const auto& hs_type = unloc(type);

        if (auto hs_tv = hs_type.to<Hs::TypeVar>())
        {
            if (hs_tv->name == tv.name)
                return type.loc;
        }
        else if (auto tuple_type = hs_type.to<Hs::TupleType>())
        {
            for(auto& element_type: tuple_type->element_types)
                if (auto loc = find_type_var_loc(element_type, tv))
                    return loc;
        }
        else if (auto list_type = hs_type.to<Hs::ListType>())
            return find_type_var_loc(list_type->element_type, tv);
        else if (auto tapp = hs_type.to<Hs::TypeApp>())
        {
            if (auto loc = find_type_var_loc(tapp->head, tv))
                return loc;
            return find_type_var_loc(tapp->arg, tv);
        }
        else if (auto constrained_type = hs_type.to<Hs::ConstrainedType>())
        {
            for(auto& constraint: constrained_type->context)
                if (auto loc = find_type_var_loc(constraint, tv))
                    return loc;
            return find_type_var_loc(constrained_type->type, tv);
        }
        else if (auto forall_type = hs_type.to<Hs::ForallType>())
            return find_type_var_loc(forall_type->type, tv);
        else if (auto strict_type = hs_type.to<Hs::StrictType>())
            return find_type_var_loc(strict_type->type, tv);
        else if (auto lazy_type = hs_type.to<Hs::LazyType>())
            return find_type_var_loc(lazy_type->type, tv);

        return {};
    }

    std::optional<yy::location> find_type_var_loc(const vector<Hs::LType>& types, const TypeVar& tv)
    {
        for(auto& type: types)
            if (auto loc = find_type_var_loc(type, tv))
                return loc;
        return {};
    }

    std::optional<yy::location> gadt_constructor_context_loc(const Hs::GADTConstructorDecl& constructor)
    {
        auto [tvs, context, rho_type] = Hs::peel_top_gen(constructor.type);
        return range(context);
    }

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
    else if (auto lazy_type = type.to<Hs::LazyType>())
    {
        ltype = lazy_type->type;
    }
    return {ltype, strictness};
}

std::pair<Hs::LType,vector<bool>> pop_constructor_signature_strictness(Hs::LType ltype)
{
    auto& [loc, type] = ltype;

    if (auto forall_type = type.to<Hs::ForallType>())
    {
        auto [inner_type, strictness] = pop_constructor_signature_strictness(forall_type->type);
        return {{loc, Hs::ForallType(forall_type->type_var_binders, inner_type)}, strictness};
    }

    if (auto constrained_type = type.to<Hs::ConstrainedType>())
    {
        auto [inner_type, strictness] = pop_constructor_signature_strictness(constrained_type->type);
        return {{loc, Hs::ConstrainedType(constrained_type->context, inner_type)}, strictness};
    }

    if (auto function_type = Hs::is_function_type(ltype))
    {
        auto [arg_type, result_type] = *function_type;
        auto [field_type, strictness] = pop_strictness(arg_type);
        auto [stripped_result_type, result_strictness] = pop_constructor_signature_strictness(result_type);

        result_strictness.insert(result_strictness.begin(), strictness);
        return {Hs::make_arrow_type(field_type, stripped_result_type), result_strictness};
    }

    return {ltype, {}};
}

// Technically, we could add (forall tvs.<>) on the hs_con_type, and that would work.
// But that would bind free kind vars to the tvs, whereas actually we know the kinds.
// If we had a kind error, then we would complain in (Con tvs) instead of the actually error location.
DataConInfo TypeChecker::infer_type_for_constructor(const Hs::LTypeCon& con, const vector<Hs::LTypeVar>& tvs, const Hs::ConstructorDecl& constructor)
{
    DataConInfo info;
    info.data_type = TypeCon(unloc(con).name);

    // FIXME: So much duplicated code with kind_check_constructor!  Can we fix?
    kindchecker_state K(*this);
    K.push_type_var_scope();

    // a. Look up kind for this data type.
    auto k = K.kind_for_type_con(unloc(con).name);  // FIXME -- check that this is a data type?

    // b. Bind each type variable.
    for(auto& tv: tvs)
    {
        // the kind should be an arrow kind.
	auto [arg_kind, result_kind] = is_function_type(k).value();

        // map the name to its kind
        auto utv = K.bind_type_var(tv, arg_kind);

        info.uni_tvs.push_back(utv);
        
        // set up the next iteration
        k = result_kind;
    }
    assert(is_kind_type(k));

    // 3. Record strictness marks
    auto hs_field_types = constructor.get_field_types();
    for(auto& sfield_type: hs_field_types)
    {
        auto [field_type, strictness] = pop_strictness(sfield_type);
        sfield_type = field_type;
        info.field_strictness.push_back( strictness );
    }

    auto hs_con_type = Hs::function_type(hs_field_types, Hs::type_apply(con, tvs));
    hs_con_type = Hs::add_constraints(constructor.context, hs_con_type);
    hs_con_type = Hs::add_forall_vars(constructor.forall, hs_con_type);

    auto con_type = check_type(hs_con_type, K);

    auto [exi_tvs, written_constraints, rho_type] = peel_top_gen(con_type);
    auto [field_types, result_type] = arg_result_types(rho_type);

    info.exi_tvs = exi_tvs;
    info.written_constraints = written_constraints;
    info.field_types = field_types;

    assert(info.field_strictness.size() == info.field_types.size());

    K.pop_type_var_scope();
    return info;
}

// Infer the most permissive role for each data type parameter from its uses in constructors.
vector<Role> TypeChecker::infer_roles_for_data_type(const vector<TypeVar>& data_tvs, const DataConEnv& constructors) const
{
    vector<Role> roles(data_tvs.size(), Role::Phantom);
    map<TypeVar, int> tv_indices;
    for(int i=0;i<data_tvs.size();i++)
        tv_indices.insert({data_tvs[i], i});

    // Raise a type parameter's role when it appears in a stricter context.
    auto raise_role = [&](const TypeVar& tv, Role role)
    {
        auto index = tv_indices.find(tv);
        if (index != tv_indices.end())
            roles[index->second] = max_role(roles[index->second], role);
    };

    // Walk a type and mark data type parameters according to their ambient role.
    std::function<void(Type,Role)> mark_type = [&](Type type, Role ambient)
    {
        type = follow_meta_type_var(type);
        if (auto tv = type.to<TypeVar>())
            raise_role(*tv, ambient);
        else if (type.is_a<MetaTypeVar>() or type.is_a<TypeCon>())
            return;
        else if (auto syn = expand_type_synonym(type))
            mark_type(*syn, ambient);
        else if (auto con = type.to<ConstrainedType>())
        {
            for(auto& constraint: con->context)
                mark_type(constraint, Role::Nominal);
            mark_type(con->type, ambient);
        }
        else if (auto forall = type.to<ForallType>())
        {
            for(auto& tv: forall->type_var_binders)
                mark_type(tv.kind, Role::Nominal);
            mark_type(forall->type, ambient);
        }
        else if (auto tcapp = is_type_con_app(type))
        {
            auto& [tc, args] = *tcapp;
            vector<Role> arg_roles(args.size(), Role::Nominal);

            if (type_con_is_type_fam(tc))
                arg_roles.assign(args.size(), Role::Nominal);
            else if (auto T = this_mod().lookup_resolved_type(tc.name); T and T->roles.size() >= args.size())
                arg_roles = T->roles;

            for(int i=0;i<args.size();i++)
                mark_type(args[i], combine_roles(ambient, arg_roles[i]));
        }
        else if (auto app = type.to<TypeApp>())
        {
            mark_type(app->head, ambient);
            mark_type(app->arg, Role::Nominal);
        }
        else
            std::abort();
    };

    for(auto& [name, info]: constructors)
    {
        for(auto& constraint: info.all_constraints())
            mark_type(constraint, Role::Nominal);
        for(auto& field_type: info.field_types)
            mark_type(field_type, Role::Representational);
    }

    return roles;
}

DataConEnv TypeChecker::infer_type_for_data_type(const Hs::DataOrNewtypeDecl& data_decl)
{
    // c. handle the context
    // The context should already be type-checked.
    // We should already have checked that it doesn't contain any unbound variables.
    if (not data_decl.context.empty())
        record_error(range(data_decl.context), Note()<<"Data type contexts are not supported; put constraints on individual constructors instead");

    // d. construct the data type

    auto hs_data_type = Hs::type_apply(data_decl.con, data_decl.type_vars);

    hs_data_type = Hs::quantify(data_decl.type_vars, data_decl.context, hs_data_type);

    auto [data_tvs, data_context, data_type] = peel_top_gen(check_type(hs_data_type));

    auto data_type_con = TypeCon(unloc(data_decl.con).name);

    // e. Handle regular constructor terms (class variables ARE in scope)
    DataConEnv types;
    if (data_decl.is_regular_decl())
    {
        for(auto& constructor: data_decl.get_constructors())
        {
            auto con_name = unloc(*constructor.con).name;
            DataConInfo info = infer_type_for_constructor(data_decl.con, data_decl.type_vars, constructor);
            info.top_constraints = data_context;
            info.is_newtype_constructor = data_decl.data_or_newtype == Hs::DataOrNewtype::newtype;
            types = types.insert({con_name, info});
        }
    }

    // f. Handle GADT constructor terms (class variables are NOT in scope)
    if (data_decl.is_gadt_decl())
    {
        for(auto& data_cons_decl: data_decl.get_gadt_constructors())
        {
            DataConInfo info;

            // 1. Kind-check and add foralls for free type vars.
            auto [hs_constructor_type, field_strictness] = pop_constructor_signature_strictness(data_cons_decl.type);
            auto written_type = check_type( hs_constructor_type );

            // 2. Extract tyvar, givens, and rho type.
            auto [written_tvs, written_constraints, rho_type] = peel_top_gen( written_type );

            // 3. Get field types and result_type
            auto [field_types, result_type] = ::arg_result_types(rho_type);

            // 4. Check result type name
            auto [con, args] = decompose_type_apps(result_type);
            if (con != data_type_con)
            {
                TidyState tidy_state;
                record_error(gadt_constructor_result_type(data_cons_decl.type).loc, Note()<<"Constructor result type '"<<show_type_plain(tidy_state, result_type)<<"' does not match data type '"<<show_type_plain(tidy_state, data_type_con)<<"'");
                continue;
            }

            if (args.size() != data_decl.type_vars.size())
            {
                record_error(gadt_constructor_result_type(data_cons_decl.type).loc, Note()<<"Constructor result type has "<<args.size()<<" arguments, but data type '"<<show_type_plain(data_type_con)<<"' has "<<data_decl.type_vars.size());
                continue;
            }

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
                    auto u_tv = fresh_rigid_type_var(*unloc(data_decl.type_vars[i]).kind);
                    u_tvs.push_back(u_tv);
                    info.gadt_eq_constraints.push_back(make_equality_pred(u_tv,arg));
                }
            }

            // 6. Get existential vars
            set<TypeVar> exi_tvs_set = written_tvs | ranges::to<set>;
            for(auto& u_tv: u_tvs)
                exi_tvs_set.erase(u_tv);

            if (data_decl.data_or_newtype == Hs::DataOrNewtype::newtype)
            {
                auto type_name = get_unqualified_name(unloc(data_decl.con).name);
                if (not written_constraints.empty())
                    record_error(gadt_constructor_context_loc(data_cons_decl), Note()<<"newtype '"<<type_name<<"' GADT constructor must not have a context");
                if (not info.gadt_eq_constraints.empty())
                    record_error(gadt_constructor_result_type(data_cons_decl.type).loc, Note()<<"newtype '"<<type_name<<"' GADT constructor result type must be the newtype head applied to its parameters");
                if (not exi_tvs_set.empty())
                {
                    auto [hs_field_types, hs_result_type] = gadt_constructor_arg_result_types(data_cons_decl.type);
                    auto loc = find_type_var_loc(hs_field_types, *exi_tvs_set.begin());
                    record_error(loc ? loc : data_cons_decl.type.loc, Note()<<"newtype '"<<type_name<<"' GADT constructor must not have existential type variables");
                }
            }

            info.field_types = field_types;
            info.field_strictness = field_strictness;
            assert(info.field_strictness.size() == info.field_types.size());
            info.data_type = data_type_con;
            info.written_constraints = constraints;
            info.top_constraints = data_context;
            info.exi_tvs = exi_tvs_set | ranges::to<vector>();
            info.uni_tvs = u_tvs;
            info.is_newtype_constructor = data_decl.data_or_newtype == Hs::DataOrNewtype::newtype;
            
            for(auto& con_name: data_cons_decl.con_names)
                types = types.insert({unloc(con_name), info});
        }
    }

    if (auto T = this_mod().lookup_local_type(data_type_con.name))
        T->roles = infer_roles_for_data_type(data_tvs, types);

    return types;
}

DataConInfo TypeChecker::infer_type_for_data_family_constructor(const Hs::LType& hs_result_type, const vector<Hs::LTypeVar>& outer_tvs, const vector<Type>& top_constraints, const Hs::ConstructorDecl& constructor)
{
    DataConInfo info;

    auto hs_field_types = constructor.get_field_types();
    for(auto& sfield_type: hs_field_types)
    {
        auto [field_type, strictness] = pop_strictness(sfield_type);
        sfield_type = field_type;
        info.field_strictness.push_back( strictness );
    }

    auto hs_con_type = Hs::function_type(hs_field_types, hs_result_type);
    hs_con_type = Hs::add_constraints(constructor.context, hs_con_type);
    hs_con_type = Hs::add_forall_vars(constructor.forall, hs_con_type);
    hs_con_type = Hs::add_forall_vars(outer_tvs, hs_con_type);

    auto con_type = check_type(hs_con_type);

    auto [all_tvs, written_constraints, rho_type] = peel_top_gen(con_type);
    auto [field_types, result_type] = arg_result_types(rho_type);

    auto [result_head, result_args] = decompose_type_apps(result_type);
    auto result_con = result_head.to<TypeCon>();
    if (not result_con or not type_con_is_data_fam(*result_con))
        record_error(Note()<<"Data family constructor result type '"<<show_type_plain(result_type)<<"' is not a data family application");
    else
        info.data_type = *result_con;

    auto result_tvs = free_type_variables(result_type);
    for(auto& tv: all_tvs)
    {
        if (result_tvs.count(tv))
            info.uni_tvs.push_back(tv);
        else
            info.exi_tvs.push_back(tv);
    }

    info.written_constraints = written_constraints;
    info.top_constraints = top_constraints;
    info.field_types = field_types;
    info.constructor_result_type = result_type;

    assert(info.field_strictness.size() == info.field_types.size());

    return info;
}

DataConInfo TypeChecker::infer_type_for_gadt_data_family_constructor(const Type& instance_head, const TypeCon& data_family, const vector<Type>& top_constraints, const Hs::GADTConstructorDecl& constructor, bool is_newtype)
{
    DataConInfo info;

    auto [hs_constructor_type, field_strictness] = pop_constructor_signature_strictness(constructor.type);
    auto written_type = check_type(hs_constructor_type);

    auto [written_tvs, written_constraints, rho_type] = peel_top_gen(written_type);
    auto [field_types, result_type] = arg_result_types(rho_type);

    auto [result_head, result_args] = decompose_type_apps(result_type);
    auto result_con = result_head.to<TypeCon>();
    if (not result_con or not type_con_is_data_fam(*result_con))
        record_error(Note()<<"Data family constructor result type '"<<show_type_plain(result_type)<<"' is not a data family application");
    else if (*result_con != data_family)
    {
        TidyState tidy_state;
        record_error(Note()<<"Data family constructor result type '"<<show_type_plain(tidy_state, result_type)<<"' does not match data family '"<<show_type_plain(tidy_state, data_family)<<"'");
    }
    else if (not maybe_unify(result_type, instance_head))
    {
        TidyState tidy_state;
        if (is_newtype)
            record_error(gadt_constructor_result_type(constructor.type).loc, Note()<<"newtype '"<<get_unqualified_name(data_family.name)<<"' GADT constructor result type must be the instance head");
        else
            record_error(Note()<<"Data family constructor result type '"<<show_type_plain(tidy_state, result_type)<<"' does not match instance head '"<<show_type_plain(tidy_state, instance_head)<<"'");
    }

    auto result_tvs = free_type_variables(result_type);
    for(auto& tv: written_tvs)
    {
        if (result_tvs.count(tv))
            info.uni_tvs.push_back(tv);
        else
            info.exi_tvs.push_back(tv);
    }

    if (is_newtype)
    {
        auto type_name = get_unqualified_name(data_family.name);
        if (not written_constraints.empty())
            record_error(gadt_constructor_context_loc(constructor), Note()<<"newtype '"<<type_name<<"' GADT constructor must not have a context");
        if (not info.exi_tvs.empty())
        {
            auto [hs_field_types, hs_result_type] = gadt_constructor_arg_result_types(constructor.type);
            auto loc = find_type_var_loc(hs_field_types, info.exi_tvs.front());
            record_error(loc ? loc : constructor.type.loc, Note()<<"newtype '"<<type_name<<"' GADT constructor must not have existential type variables");
        }
    }

    info.field_types = field_types;
    info.field_strictness = field_strictness;
    info.data_type = data_family;
    info.written_constraints = written_constraints;
    info.top_constraints = top_constraints;
    info.constructor_result_type = result_type;

    assert(info.field_strictness.size() == info.field_types.size());

    return info;
}

pair<DataConEnv,std::optional<Type>> TypeChecker::infer_type_for_data_family_instance(const Hs::DataFamilyInstanceDecl& data_inst, const std::optional<std::string>& associated_class)
{
    auto span = source_span_scope(data_inst.con.loc);

    DataConEnv types;
    TypeCon data_family(unloc(data_inst.con).name);
    auto data_fam_info = info_for_data_fam(data_family.name);
    if (not data_fam_info)
    {
        record_error(Note()<<"No data family '"<<data_inst.con.print()<<"'");
        return {types, {}};
    }

    if (not check_family_instance_association(data_inst.con, data_fam_info->associated_class, associated_class, "data instance", "data family", false, false))
        return {types, {}};

    auto hs_result_type = Hs::type_apply(data_inst.con, data_inst.args);
    auto outer_tvs = data_inst.forall ? *data_inst.forall : free_type_variables(data_inst.args);

    int head_errors = num_errors();
    auto instance_head = check_family_instance_head(data_inst.con, data_inst.args, data_inst.forall, data_inst.rhs.context);
    auto note = note_scope( Note()<<"In data family instance '"<<show_type_plain(instance_head.type)<<"':" );

    if (not data_inst.rhs.context.empty())
        record_error(range(data_inst.rhs.context), Note()<<"Data family instance contexts are not supported; put constraints on individual constructors instead");

    auto [result_head, result_args] = decompose_type_apps(instance_head.type);
    auto result_con = result_head.to<TypeCon>();
    if (not result_con or not type_con_is_data_fam(*result_con))
        record_error(Note()<<"Data family instance head '"<<show_type_plain(instance_head.type)<<"' is not a data family application");
    else if (*result_con != data_family)
    {
        TidyState tidy_state;
        record_error(Note()<<"Data family instance head '"<<show_type_plain(tidy_state, instance_head.type)<<"' does not match data family '"<<show_type_plain(tidy_state, data_family)<<"'");
    }
    else if (result_args.size() != data_fam_info->arity())
        record_error(Note()<<"Data family takes "<<data_fam_info->arity()<<" arguments, but instance has "<<result_args.size());

    if (num_errors() > head_errors)
        return {types, {}};

    if (data_inst.rhs.is_regular_decl())
    {
        for(auto& constructor: data_inst.rhs.get_constructors())
        {
            auto con_name = unloc(*constructor.con).name;
            DataConInfo info = infer_type_for_data_family_constructor(hs_result_type, outer_tvs, instance_head.context, constructor);
            info.is_newtype_constructor = data_inst.rhs.data_or_newtype == Hs::DataOrNewtype::newtype;
            types = types.insert({con_name, info});
        }
    }
    else if (data_inst.rhs.is_gadt_decl())
    {
        for(auto& constructor: data_inst.rhs.get_gadt_constructors())
        {
            bool is_newtype = data_inst.rhs.data_or_newtype == Hs::DataOrNewtype::newtype;
            DataConInfo info = infer_type_for_gadt_data_family_constructor(instance_head.type, data_family, instance_head.context, constructor, is_newtype);
            info.is_newtype_constructor = data_inst.rhs.data_or_newtype == Hs::DataOrNewtype::newtype;
            for(auto& con_name: constructor.con_names)
                types = types.insert({unloc(con_name), info});
        }
    }

    return {types, instance_head.type};
}

void TypeChecker::get_constructor_info(const Hs::Decls& decls)
{
    vector<Type> data_family_instance_heads;

    auto instance_class_name = [](const Hs::InstanceDecl& instance_decl) -> std::optional<std::string>
    {
        auto [tvs, context, head] = Hs::peel_top_gen(instance_decl.polytype);
        auto [class_head, args] = Hs::decompose_type_apps(head);
        auto class_con = unloc(class_head).to<Hs::TypeCon>();
        if (class_con)
            return class_con->name;
        else
            return {};
    };

    auto get_data_family_instance_constructor_info = [&](const Hs::DataFamilyInstanceDecl& data_inst, const std::optional<std::string>& associated_class)
    {
        auto [new_con_infos, head] = infer_type_for_data_family_instance(data_inst, associated_class);

        if (head)
        {
            for(auto& previous_head: data_family_instance_heads)
            {
                auto instance_apartness = apartness(*head, previous_head);
                if (instance_apartness == Apartness::Unifiable)
                {
                    TidyState tidy_state;
                    record_error(data_inst.con.loc, Note()<<"Data family instance '"<<show_type_plain(tidy_state, *head)<<"' overlaps previous instance '"<<show_type_plain(tidy_state, previous_head)<<"'");
                }
                else if (instance_apartness == Apartness::MaybeApart)
                {
                    TidyState tidy_state;
                    record_error(data_inst.con.loc, Note()<<"Data family instance '"<<show_type_plain(tidy_state, *head)<<"' is not surely apart from previous instance '"<<show_type_plain(tidy_state, previous_head)<<"'");
                }
            }

            data_family_instance_heads.push_back(*head);
        }

        return new_con_infos;
    };

    for(auto& [_,decl]: decls)
    {
        DataConEnv con_infos;
        if (auto d = decl.to<Hs::DataOrNewtypeDecl>())
            con_infos = infer_type_for_data_type(*d);
        else if (auto d = decl.to<Hs::DataFamilyInstanceDecl>())
            con_infos = get_data_family_instance_constructor_info(*d, {});
        else if (auto i = decl.to<Hs::InstanceDecl>())
        {
            auto associated_class = instance_class_name(*i);
            for(auto& d: i->data_inst_decls)
                for(auto& [name, con_info]: get_data_family_instance_constructor_info(d, associated_class))
                    con_infos = con_infos.insert({name, con_info});
        }
        else
            continue;

        for(auto& [name,con_info]: con_infos)
        {
            auto C = this_mod().lookup_local_symbol(name);
            assert(C);
            assert(not C->con_info);
            C->con_info = std::make_shared<DataConInfo>(con_info);
        }
    }
}
