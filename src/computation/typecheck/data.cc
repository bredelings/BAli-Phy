#include "typecheck.H"

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
    return {ltype, strictness};
}

DataConInfo TypeChecker::infer_type_for_constructor(kindchecker_state& K, const Hs::ConstructorDecl& constructor)
{
    // FIXME: So much duplicated code with kind_check_constructor!  Can we fix?

    DataConInfo info;

    // 1. Record exi_tvs and make up kind vars for them.
    K.push_type_var_scope();
    for(auto& htv: constructor.forall)
    {
        auto k = K.fresh_kind_var();
        K.bind_type_var(htv, k);
        auto tv = desugar(htv);
        tv.kind = k;
        info.exi_tvs.push_back(tv);
    }

    // 2. Record written_constraints
    //   Do constraints affect kind determination?  Maybe not
    if (constructor.context)
    {
        for(auto& constraint: *constructor.context)
            info.written_constraints.push_back( K.kind_check_type_of_kind(constraint, kind_constraint()) );
    }

    // 3. Kind check field types
    if (constructor.is_record_constructor())
    {
        for(auto& field_decl: std::get<1>(constructor.fields).field_decls)
            for(int i=0; i < field_decl.field_names.size(); i++)
            {
                auto [field_type, strictness] = pop_strictness(field_decl.type);
                info.field_types.push_back( K.kind_check_type_of_kind(field_type, kind_type() ) );
                info.field_strictness.push_back( strictness );
            }
    }
    else
    {
        for(auto& hs_field_type: std::get<0>(constructor.fields))
        {
            auto [field_type, strictness] = pop_strictness(hs_field_type);
            info.field_types.push_back( K.kind_check_type_of_kind(field_type, kind_type() ) );
            info.field_strictness.push_back( strictness );
        }
    }

    // 4. Substitute and replace kind vars
    for(auto& field_type: info.field_types)
        field_type = K.zonk_kind_for_type( field_type );
    for(auto& constraint: info.written_constraints)
        constraint = K.zonk_kind_for_type( constraint );
    for(auto& tv : info.exi_tvs)
        tv.kind = K.apply_substitution(*tv.kind);

    K.pop_type_var_scope();

    return info;
}

DataConEnv TypeChecker::infer_type_for_data_type(const Hs::DataOrNewtypeDecl& data_decl)
{
    kindchecker_state K(*this);

    K.push_type_var_scope();

    // a. Look up kind for this data type.
    auto k = K.kind_for_type_con(unloc(data_decl.name));  // FIXME -- check that this is a data type?

    // b. Bind each type variable.
    vector<TypeVar> datatype_typevars;
    for(auto& tv: data_decl.type_vars)
    {
        // the kind should be an arrow kind.
	auto [arg_kind, result_kind] = is_function_type(k).value();

        // map the name to its kind
        K.bind_type_var(tv, arg_kind);

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
            DataConInfo info = infer_type_for_constructor(K,constructor);
            info.uni_tvs = datatype_typevars;
            info.data_type = data_type_con;
            info.top_constraints = desugar(data_decl.context);
            types = types.insert({unloc(*constructor.con).name, info});
        }
    }

    K.pop_type_var_scope();

    // f. Handle GADT constructor terms (class variables are NOT in scope)
    if (data_decl.is_gadt_decl())
    {
        for(auto& data_cons_decl: data_decl.get_gadt_constructors())
        {
            DataConInfo info;

            // 1. Kind-check and add foralls for free type vars.

            // BUG: We don't handle strictness annotations on the fields here!
            auto written_type = check_type( data_cons_decl.type );

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
                    auto u_tv = fresh_other_type_var(*unloc(data_decl.type_vars[i]).kind);
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


void TypeChecker::get_constructor_info(const Hs::Decls& decls)
{
    for(auto& [_,decl]: decls)
    {
        auto d = decl.to<Hs::DataOrNewtypeDecl>();
        if (not d) continue;

        for(auto& [name,con_info]: infer_type_for_data_type(*d))
        {
            auto C = this_mod().lookup_local_symbol(name);
            assert(C);
            assert(not C->con_info);
            C->con_info = std::make_shared<DataConInfo>(con_info);
        }
    }
}

