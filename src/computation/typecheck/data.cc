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
    vector<TypeVar> datatype_typevars;
    for(auto& tv: data_decl.type_vars)
    {
        datatype_typevars.push_back(desugar(tv));
    }

    // c. handle the context
    // The context should already be type-checked.
    // We should already have checked that it doesn't contain any unbound variables.

    // d. construct the data type

    Hs::LTypeCon hs_data_type_con = {data_decl.name.loc, Hs::TypeCon(unloc(data_decl.name))};
    auto hs_data_type = Hs::type_apply(hs_data_type_con, data_decl.type_vars);

    auto data_type_con = TypeCon(unloc(data_decl.name));
    Type data_type = type_apply(data_type_con, datatype_typevars);

    // Assume no "stupid theta".
    assert(data_decl.context.empty());

    // e. Handle regular constructor terms (class variables ARE in scope)
    DataConEnv types;
    if (data_decl.is_regular_decl())
    {
        for(auto& constructor: data_decl.get_constructors())
        {
            auto con_name = unloc(*constructor.con).name;
            DataConInfo info = infer_type_for_constructor(hs_data_type_con, data_decl.type_vars, constructor);
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

