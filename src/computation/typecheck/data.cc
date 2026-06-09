#include "typecheck.H"
#include "rename/rename.H"

using std::vector;
using std::set;
using std::pair;

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
                    auto u_tv = fresh_rigid_type_var(*unloc(data_decl.type_vars[i]).kind);
                    u_tvs.push_back(u_tv);
                    info.gadt_eq_constraints.push_back(make_equality_pred(u_tv,arg));
                }
            }

            // 6. Get existential vars
            set<TypeVar> exi_tvs_set = written_tvs | ranges::to<set>;
            for(auto& u_tv: u_tvs)
                exi_tvs_set.erase(u_tv);

            info.field_types = field_types;
            info.field_strictness = field_strictness;
            assert(info.field_strictness.size() == info.field_types.size());
            info.data_type = data_type_con;
            info.written_constraints = constraints;
            info.top_constraints = data_context;
            info.exi_tvs = exi_tvs_set | ranges::to<vector>();
            info.uni_tvs = u_tvs;
            
            for(auto& con_name: data_cons_decl.con_names)
                types = types.insert({unloc(con_name), info});
        }
    }

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
        record_error(Note()<<"Data family constructor result type '"<<result_type<<"' is not a data family application");
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

DataConInfo TypeChecker::infer_type_for_gadt_data_family_constructor(const Type& instance_head, const TypeCon& data_family, const vector<Type>& top_constraints, const Hs::GADTConstructorDecl& constructor)
{
    DataConInfo info;

    auto [hs_constructor_type, field_strictness] = pop_constructor_signature_strictness(constructor.type);
    auto written_type = check_type(hs_constructor_type);

    auto [written_tvs, written_constraints, rho_type] = peel_top_gen(written_type);
    auto [field_types, result_type] = arg_result_types(rho_type);

    auto [result_head, result_args] = decompose_type_apps(result_type);
    auto result_con = result_head.to<TypeCon>();
    if (not result_con or not type_con_is_data_fam(*result_con))
        record_error(Note()<<"Data family constructor result type '"<<result_type<<"' is not a data family application");
    else if (*result_con != data_family)
        record_error(Note()<<"Data family constructor result type '"<<result_type<<"' does not match data family '"<<data_family<<"'");
    else if (not maybe_unify(result_type, instance_head))
        record_error(Note()<<"Data family constructor result type '"<<result_type<<"' does not match instance head '"<<instance_head<<"'");

    auto result_tvs = free_type_variables(result_type);
    for(auto& tv: written_tvs)
    {
        if (result_tvs.count(tv))
            info.uni_tvs.push_back(tv);
        else
            info.exi_tvs.push_back(tv);
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

pair<DataConEnv,std::optional<Type>> TypeChecker::infer_type_for_data_family_instance(const Hs::DataFamilyInstanceDecl& data_inst)
{
    push_note( Note()<<"In data family instance '"<<data_inst.print()<<"':" );
    if (data_inst.con.loc) push_source_span(*data_inst.con.loc);

    DataConEnv types;
    TypeCon data_family(unloc(data_inst.con).name);
    auto data_fam_info = info_for_data_fam(data_family.name);
    if (not data_fam_info)
    {
        record_error(Note()<<"No data family '"<<data_inst.con.print()<<"'");
        if (data_inst.con.loc) pop_source_span();
        pop_note();
        return {types, {}};
    }

    auto hs_result_type = Hs::type_apply(data_inst.con, data_inst.args);
    auto outer_tvs = data_inst.forall ? *data_inst.forall : free_type_variables(data_inst.args);
    if (not data_inst.rhs.context.empty())
        record_error(range(data_inst.rhs.context), Note()<<"Data family instance contexts are not supported; put constraints on individual constructors instead");

    auto hs_instance_type = Hs::quantify(outer_tvs, data_inst.rhs.context, hs_result_type);
    int head_errors = num_errors();
    auto [data_tvs, data_context, data_type] = peel_top_gen(check_type(hs_instance_type));

    auto [result_head, result_args] = decompose_type_apps(data_type);
    auto result_con = result_head.to<TypeCon>();
    if (not result_con or not type_con_is_data_fam(*result_con))
        record_error(Note()<<"Data family instance head '"<<data_type<<"' is not a data family application");
    else if (*result_con != data_family)
        record_error(Note()<<"Data family instance head '"<<data_type<<"' does not match data family '"<<data_family<<"'");
    else if (result_args.size() != data_fam_info->arity())
        record_error(Note()<<"Data family takes "<<data_fam_info->arity()<<" arguments, but instance has "<<result_args.size());

    if (num_errors() > head_errors)
    {
        if (data_inst.con.loc) pop_source_span();
        pop_note();
        return {types, {}};
    }

    if (data_inst.rhs.is_regular_decl())
    {
        for(auto& constructor: data_inst.rhs.get_constructors())
        {
            auto con_name = unloc(*constructor.con).name;
            DataConInfo info = infer_type_for_data_family_constructor(hs_result_type, outer_tvs, data_context, constructor);
            types = types.insert({con_name, info});
        }
    }
    else if (data_inst.rhs.is_gadt_decl())
    {
        for(auto& constructor: data_inst.rhs.get_gadt_constructors())
        {
            DataConInfo info = infer_type_for_gadt_data_family_constructor(data_type, data_family, data_context, constructor);
            for(auto& con_name: constructor.con_names)
                types = types.insert({unloc(con_name), info});
        }
    }

    if (data_inst.con.loc) pop_source_span();
    pop_note();

    return {types, data_type};
}

void TypeChecker::get_constructor_info(const Hs::Decls& decls)
{
    vector<pair<Type,std::string>> data_family_instance_heads;

    for(auto& [_,decl]: decls)
    {
        DataConEnv con_infos;
        if (auto d = decl.to<Hs::DataOrNewtypeDecl>())
            con_infos = infer_type_for_data_type(*d);
        else if (auto d = decl.to<Hs::DataFamilyInstanceDecl>())
        {
            auto [new_con_infos, head] = infer_type_for_data_family_instance(*d);
            con_infos = new_con_infos;

            if (head)
            {
                for(auto& [previous_head, previous_instance]: data_family_instance_heads)
                    if (maybe_unify(*head, previous_head))
                        record_error(d->con.loc, Note()<<"Data family instance '"<<d->print()<<"' overlaps previous instance '"<<previous_instance<<"'");

                data_family_instance_heads.push_back({*head, d->print()});
            }
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
