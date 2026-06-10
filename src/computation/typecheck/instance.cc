#include "typecheck.H"
#include "kindcheck.H"
#include "haskell/ids.H"
#include "core/func.H"
#include "rename/rename.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::tuple;
using std::optional;


// TODO: maybe move some of the check_add_type_instance stuff to renaming?

// TODO: change kind unification to use meta-variables.

// TODO: maybe add Hs::ParenType( )

Hs::Decls TypeChecker::infer_type_for_default_methods(const Hs::ClassDecl& C)
{
    Hs::Decls decls_out;

    auto class_info = *info_for_class(unloc(C.con).name);

    for(auto& [loc,decl]: C.default_method_decls)
    {
        auto span = source_span_scope(loc);

        auto FD = decl.as_<Hs::FunDecl>();
	if (not class_info.default_methods.count(unloc(FD.v).name ))
	{
            // We already get an error message for this somewhere else.
	    continue;
	}
        auto dm = class_info.default_methods.at( unloc(FD.v).name );
        unloc(FD.v) = dm;

        auto sig_type = this_mod().lookup_resolved_symbol( unloc(FD.v).name )->type;
        auto decl2 = infer_type_for_single_fundecl_with_sig(FD, sig_type);
        decls_out.push_back({loc,decl2});
    }

//    std::cerr<<"Default method ops:\n";
//    std::cerr<<decls_out.print();
//    std::cerr<<"\n\n";

    return decls_out;
}

Hs::Binds TypeChecker::infer_type_for_default_methods(const Hs::Decls& decls)
{
    Hs::Binds default_method_decls;
    for(auto& [_,decl]: decls)
    {
        auto c = decl.to<Hs::ClassDecl>();
        if (not c) continue;

        default_method_decls.push_back( infer_type_for_default_methods(*c) );
    }
    return default_method_decls;
}

TypeFamilyInstanceCheck TypeChecker::check_type_instance(const Hs::LType& hs_lhs, const Hs::LType& hs_rhs)
{
    auto hs_free_tvs = free_type_variables(hs_lhs);
    auto hs_equality = Hs::make_equality_type(hs_lhs, hs_rhs);
    auto hs_inst_type = Hs::add_forall_vars(hs_free_tvs, hs_equality);

    auto inst_type = check_constraint(hs_inst_type);
    auto [free_tvs, context, equality] = peel_top_gen(inst_type);
    assert(context.empty());

    auto [core_sim, args] = decompose_type_apps(equality);
    auto lhs = args[0];
    auto rhs = args[1];
    auto [head, family_args] = decompose_type_apps(lhs);

    return {{free_tvs, context, lhs, family_args}, rhs};
}

void TypeChecker::add_type_instance(const vector<TypeVar>& free_tvs, const Type& lhs, const Type& rhs)
{
    auto dvar = fresh_dvar(make_equality_pred(lhs,rhs), true);

    auto S = symbol_info(dvar.name, symbol_type_t::instance_dfun, {}, {}, {});
    S.instance_info = std::make_shared<InstanceInfo>( InstanceInfo{free_tvs,{},TypeCon("~"),{lhs, rhs}, false, false, false} );
    S.eq_instance_info = std::make_shared<EqInstanceInfo>( EqInstanceInfo{free_tvs, lhs, rhs} );
    S.type = S.instance_info->type();
    this_mod().add_symbol(S);

    this_mod().local_instances.insert( {dvar, *S.instance_info} );
    this_mod().local_eq_instances.insert( {dvar, *S.eq_instance_info} );
}

bool TypeChecker::check_family_instance_association(const Hs::LTypeCon& family_con,
						    const optional<string>& family_associated_class,
						    const optional<string>& instance_associated_class,
						    const string& instance_name,
						    const string& family_name,
						    bool require_associated_family,
						    bool indent_message)
{
    auto prefix = indent_message ? "  " : "";
    auto family_display_name = get_unqualified_name(unloc(family_con).name);
    auto class_display_name = [](const string& name) {return get_unqualified_name(name);};

    if (not family_associated_class)
    {
        if (require_associated_family)
        {
            auto family_display = family_name == "data family" ? "Data family" : "Type family";
            record_error(family_con.loc, Note()<<prefix<<family_display<<" '"<<family_display_name<<"' is not associated with a class");
            return false;
        }
        return true;
    }

    if (not instance_associated_class)
    {
        record_error(family_con.loc, Note()<<prefix<<"Can't declare non-associated "<<instance_name<<" for "<<family_name<<" '"<<family_display_name<<"' associated with class '"<<class_display_name(*family_associated_class)<<"'");
        return false;
    }

    if (*family_associated_class != *instance_associated_class)
    {
        record_error(family_con.loc, Note()<<prefix<<"Trying to declare "<<instance_name<<" in class '"<<class_display_name(*instance_associated_class)<<"' for family '"<<family_display_name<<"' associated with class '"<<class_display_name(*family_associated_class)<<"'");
        return false;
    }

    return true;
}

FamilyInstanceHead TypeChecker::check_family_instance_head(const Hs::LTypeCon& family_con,
							   const vector<Hs::LType>& args,
							   const optional<vector<Hs::LTypeVar>>& forall,
							   const Hs::Context& context)
{
    auto hs_head = Hs::type_apply(family_con, args);
    auto outer_tvs = forall ? *forall : free_type_variables(args);
    auto hs_inst_type = Hs::quantify(outer_tvs, context, hs_head);
    auto [type_vars, checked_context, checked_head] = peel_top_gen(check_type(hs_inst_type));
    auto [head, checked_args] = decompose_type_apps(checked_head);

    return {type_vars, checked_context, checked_head, checked_args};
}

void TypeChecker::check_associated_family_instance_args(const vector<Hs::LType>& hs_args,
							const vector<Type>& args,
							const vector<TypeVar>& family_args,
							const substitution_t& instance_subst)
{
    for(int i=0; i<family_args.size(); i++)
    {
        auto fam_tv = family_args[i];
        auto span = source_span_scope(hs_args[i].loc);
        if (instance_subst.count(fam_tv))
        {
            auto expected = instance_subst.at(fam_tv);
            if (not same_type(args[i], expected))
            {
                TidyState tidy_state;
                record_error(Note()<<"Argument '"<<show_type_plain(tidy_state, args[i])<<"' should match instance parameter '"<<show_type_plain(tidy_state, expected)<<"'");
            }
        }
    }
}

void TypeChecker::check_add_type_instance(const Hs::TypeFamilyInstanceEqn& inst, const optional<string>& associated_class, const substitution_t& instance_subst)
{
    auto inst_loc = *(inst.con.loc * range(inst.args) * inst.rhs.loc);
    auto span = source_span_scope( inst_loc );

    // 1. Check that the type family exists.
    TypeCon tf_con(unloc(inst.con).name);
    if (not type_con_is_type_fam( tf_con ) )
    {
        auto con_span = source_span_scope(inst.con.loc);
        record_error( Note()<<"  No type family '"<<inst.con.print()<<"'");
        return;
    }

    // Check RHS -- move down next to add_type_instance??
    auto hs_lhs = Hs::type_apply(inst.con, inst.args);
    auto type_inst = check_type_instance(hs_lhs, inst.rhs);

    // There CAN be multiple type instances for an associated type family, if they don't unify with each other.
    // We don't check if a class has only one instance, so I guess we allow this.
    // But we don't check if the two instances are "apart" either.

    // 2. Get the type family info
    auto tf_info = info_for_type_fam( tf_con.name );

    // 3. Check family association
    if (not check_family_instance_association(inst.con, tf_info->associated_class, associated_class, "type instance", "type family", false, true))
        return;

    auto note = note_scope( Note()<<"In type instance '"<<show_type_plain(type_inst.head.type)<<"':" );

    // 4. Check the arity before comparing associated family arguments below.
    if (inst.args.size() != tf_info->args.size())
    {
        auto args_span = source_span_scope( *(inst.con.loc * range(inst.args)) );
        record_error(Note() << "  Type family takes "<<tf_info->args.size()<<" arguments, but was given "<<inst.args.size()<<".");
        return;
    }

    // 5. Check that arguments corresponding to class parameters are the same as the parameter type for the instance.
    check_associated_family_instance_args(inst.args, type_inst.head.args, tf_info->args, instance_subst);

    // 6. Check that the type family is not closed
    if (tf_info->closed)
    {
        record_error( Note() << "  Can't declare additional type instance for closed type family '"<<inst.con.print()<<"'");
        return;
    }

    add_type_instance(type_inst.head.type_vars, type_inst.head.type, type_inst.rhs);
}

void TypeChecker::check_data_instance(const Hs::DataFamilyInstanceDecl& inst, const optional<string>& associated_class, const substitution_t& instance_subst)
{
    auto span = source_span_scope(inst.con.loc);

    // 1. Check that the type family exists.
    TypeCon df_con(unloc(inst.con).name);
    if (not type_con_is_data_fam(df_con))
    {
        record_error(inst.con.loc, Note()<<"  No data family '"<<inst.con.print()<<"'");
        return;
    }

    // 2. Get the data family info
    auto df_info = info_for_data_fam(df_con.name);
    assert(df_info);

    // 3. Check family association
    if (not check_family_instance_association(inst.con, df_info->associated_class, associated_class, "data instance", "data family", true, true))
        return;

    // 4. Check the arity before comparing associated family arguments below.
    if (inst.args.size() != df_info->args.size())
    {
        auto args_span = source_span_scope(*(inst.con.loc * range(inst.args)));
        record_error(Note()<<"  Data family takes "<<df_info->args.size()<<" arguments, but was given "<<inst.args.size()<<".");
        return;
    }

    auto head = check_family_instance_head(inst.con, inst.args, inst.forall, {});
    auto note = note_scope( Note()<<"In data instance '"<<show_type_plain(head.type)<<"':" );

    // 5. Check that arguments corresponding to class parameters are the same as the parameter type for the instance.
    check_associated_family_instance_args(inst.args, head.args, df_info->args, instance_subst);
}

void TypeChecker::default_type_instance(const TypeCon& tf_con,
					const std::optional<TypeFamilyInstanceDecl>& maybe_default,
					const substitution_t& instance_subst)
{
    if (not maybe_default)
    {
	record_warning( Note() <<"No instance for associated type family "<<show_type_plain(tf_con) );
	return;
    }

    auto default_instance = *maybe_default;

    // 1. Get the type family info
    auto tf_info = info_for_type_fam( tf_con.name );

    // 2. Now we need to perform this substitution on the type family definition to get the default instance head.
    //   => 
    vector<Type> args;
    for(int i=0;i < tf_info->args.size();i++)
    {
	if (instance_subst.count(tf_info->args[i]))
	    args.push_back(instance_subst.at(tf_info->args[i]));
	else
	    args.push_back(tf_info->args[i]);
    }

    // 3. Now we need to substitute into the TypeFamilyInstanceEqn
    assert(tf_info->args.size() == default_instance.args.size());
    substitution_t default_subst;
    for(int i=0; i < default_instance.args.size(); i++)
    {
	auto tv = default_instance.args[i].as_<TypeVar>();
	default_subst = default_subst.insert({tv, args[i]});
    }
    auto rhs = default_instance.rhs;
    rhs = apply_subst(default_subst, rhs);

    // 4. add the instance
    auto lhs = type_apply(tf_con, args);
    auto free_tvs = free_type_variables(lhs) | ranges::to<vector>();
    add_type_instance(free_tvs, lhs, rhs);
}

std::optional<Core::Var<>>
TypeChecker::infer_type_for_instance1(const Hs::InstanceDecl& inst_decl)
{
    auto inst_loc = inst_decl.polytype.loc;
    auto span = source_span_scope( inst_loc );

    // 1. Kind-check the type and set the kinds for the type variables.

    // FIXME: allow check_constraint to fail by returning an optional<Type>.
    auto polytype = check_constraint(inst_decl.polytype);
    auto note = note_scope( Note()<<"In class instance '"<<show_type_plain(polytype)<<"':" );

    // 2. Get the free type variables, constraints, class_head and args
    auto [tvs, constraints, constraint] = peel_top_gen(polytype);
    auto [head, args] = decompose_type_apps(constraint);
    int N = args.size();

    // 2. Look up the class info
    auto tc = head.to<TypeCon>();
    // We checked that this is a typecon in rename.
    assert(tc);
    auto class_name = tc->name;
    auto maybe_class_info = info_for_class(class_name);
    assert(maybe_class_info);
    auto class_info = *maybe_class_info;

    // 5. Construct the mapping from original class variables to instance variables
    substitution_t instance_subst;
    for(int i = 0; i < N; i++)
        instance_subst = instance_subst.insert( {class_info.type_vars[i], args[i]} );

    // Look at associated type instances
    std::set<TypeCon> defined_ats;
    for(auto& inst: inst_decl.type_inst_decls)
    {
	TypeCon tf_con(unloc(inst.con).name);
        check_add_type_instance(inst, class_name, instance_subst);
	defined_ats.insert(tf_con);
    }

    for(auto& inst: inst_decl.data_inst_decls)
        check_data_instance(inst, class_name, instance_subst);

    for(auto& [tf_con, maybe_default]: class_info.associated_type_families)
    {
	// The user wrote an instance for this, we're all done.
	if (not defined_ats.contains(tf_con))
	    default_type_instance(tf_con, maybe_default, instance_subst);
    }

    auto dfun = fresh_dvar(constraint, true);

    auto class_con = head.to<TypeCon>();
    assert(class_con);

    bool incoherent = this_mod().language_extensions.has_extension(LangExt::IncoherentInstances) or
        (inst_decl.overlap_pragma == "INCOHERENT");
    bool overlappable = this_mod().language_extensions.has_extension(LangExt::OverlappingInstances) or
        inst_decl.overlap_pragma == "OVERLAPPABLE" or
        inst_decl.overlap_pragma == "OVERLAPS" or
        incoherent;
    bool overlapping = this_mod().language_extensions.has_extension(LangExt::OverlappingInstances) or
        inst_decl.overlap_pragma == "OVERLAPPING" or
        inst_decl.overlap_pragma == "OVERLAPS" or
        incoherent;

    auto S = symbol_info(dfun.name, symbol_type_t::instance_dfun, {}, {}, {});
    S.instance_info = std::make_shared<InstanceInfo>( InstanceInfo{tvs, constraints, *class_con, args, incoherent, overlappable, overlapping} );
    S.type = S.instance_info->type();
    this_mod().add_symbol(S);

    return dfun;
}


// See Tc/TyCl/Instance.hs
// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
vector<pair<Core::Var<>,Hs::InstanceDecl>>
TypeChecker::infer_type_for_instances1(const Hs::Decls& decls)
{
    vector<pair<Core::Var<>, Hs::InstanceDecl>> named_instances;

    for(auto& [loc,decl]: decls)
    {
        if (auto I = decl.to<Hs::InstanceDecl>())
        {
            if (auto result = infer_type_for_instance1(*I))
            {
                auto dfun = *result;
                auto inst_info = this_mod().lookup_local_symbol(dfun.name)->instance_info;
                assert(inst_info);

                named_instances.push_back({dfun, *I});
                this_mod().local_instances.insert( {dfun, *inst_info} );
            }
        }
        else if (auto TI = decl.to<Hs::TypeFamilyInstanceDecl>())
            check_add_type_instance(*TI, {}, {});
    }

    return named_instances;
}


TypeCon get_class_for_constraint(const Type& constraint)
{
    auto [class_head, args] = decompose_type_apps(constraint);
    auto tc = class_head.to<TypeCon>();
    assert(tc);
    return *tc;
}

// PROBLEM: we also need to know all the instance types to check the entails.
//          so, that part needs to come after a first pass over all instances...
// PROBLEM: we need to types for functions defined in the module...
//          so, typechecking the method bodies needs to come after typechecking the rest of the module.
// FIXME: What stuff do we want to know from infer_type_for_instance1( )?
//        * the dvar name

// Construct superclass dictionary entries from instance constraints

// Construct member function entries.

/* dfun idvar1:instance_constraint1 ... idvar[N]:instance_constraint[N] =
   let dvar1 = <construct superdict1>
   dvar2 = <construct superdictN>

   in let var1 = <body1>
   varM = <bodyM>
   in <dvar1, ..., dvarN, var1, ..., varM>
*/

map<Hs::Var, Hs::Matches> TypeChecker::get_instance_methods(const Hs::Decls& decls, const global_value_env& members, const string& class_name)
{
    std::map<Hs::Var, Hs::Matches> method_matches;
    for(auto& [loc,decl]: decls)
    {
        auto& fd = decl.as_<Hs::FunDecl>();
        auto& method = unloc(fd.v);
        string method_name = method.name;
        auto span = source_span_scope(fd.v.loc);

        if (not members.count(method))
        {
            record_error( Note()<<"'"<<method_name<<"' is not a member of class '"<<class_name<<"'" );
            continue;
        }

        if (method_matches.count(method))
        {
            record_error( Note() <<"method '"<<method_name<<"' defined twice!" );
            continue;
        }

        method_matches.insert({method, fd.matches});
    }

    return method_matches;
}

std::tuple<Hs::Decl,Core::Var<>> TypeChecker::type_check_superclass_entry(
    const Constraint& superclass_constraint, const string& class_name,
    const std::vector<TypeVar>& instance_tvs, const LIE& givens, const substitution_t& subst,
    const std::vector<Core::Var<>>& instance_dict_args, std::shared_ptr<const Core::Decls<>>& decls_super)
{
    auto dv = superclass_constraint.ev_var;

    auto selector_name = get_class_name_from_constraint(superclass_constraint.pred) + "From" + get_unqualified_name(class_name);

    auto superclass_selector = get_fresh_Var("sc$"+selector_name, true);

    // SC a
    auto selector_type = superclass_constraint.pred;
    // SC [x]
    selector_type = apply_subst(subst, selector_type);
    // forall x. (C1 x, C2 x) => SC [x]
    selector_type = add_forall_vars(instance_tvs,add_constraints(preds_from_lie(givens), selector_type));

    // superclass_selector = forall instance tvs. \instance_dict_args -> let decls_super in superclass_constraint.ev_var
    std::map<Hs::Var, Hs::BindInfo> bind_infos;
    bind_infos.insert({superclass_selector, Hs::BindInfo(superclass_selector, Hs::Var(dv.name), {}, {}, {}, true)});
    auto hs_decl = Hs::GenBind(instance_tvs, instance_dict_args, {decls_super}, {}, bind_infos);

    // Create the symbol
    auto S = symbol_info(superclass_selector.name, symbol_type_t::instance_superclass_selector, {}, {}, {});
    S.type = selector_type;
    this_mod().add_symbol(S);

    return {hs_decl, make_core_var(superclass_selector)};
}

// Find the representation class dictionary added to the synthetic GND context.
optional<pair<Type, Core::Var<>>> find_gnd_representation_dictionary(const TypeChecker& tc, const LIE& givens, const TypeCon& class_con, const vector<Type>& instance_args)
{
    for(auto it = givens.rbegin(); it != givens.rend(); ++it)
    {
        auto [head, args] = decompose_type_apps(it->pred);
        auto pred_class = head.to<TypeCon>();
        if (not pred_class or *pred_class != class_con or args.size() != instance_args.size())
            continue;

        bool fixed_args_match = true;
        for(int i=0; i+1<args.size(); i++)
            if (not tc.same_type(args[i], instance_args[i]))
                fixed_args_match = false;

        if (fixed_args_match)
            return pair(args.back(), it->ev_var);
    }

    return {};
}

// Find the dictionary selector for a class method, skipping superclass fields.
optional<Hs::Var> class_method_selector(const ClassInfo& class_info, const Hs::Var& method)
{
    if (not class_info.members.count(method))
        return {};

    for(const auto& field_and_type: class_info.fields)
    {
        const auto& field = field_and_type.first;
        if (get_unqualified_name(field.name) == method.name)
            return field;
    }

    return {};
}

std::tuple<Hs::Decl,Core::Var<>> TypeChecker::type_check_instance_method(
    const Hs::Var& method, const Type& method_type,
    const std::vector<TypeVar>& instance_tvs, const LIE& givens, const Type& inst_type, const substitution_t& subst,
    const std::map<Hs::Var,Hs::Matches>& method_matches, const ClassInfo& class_info)
{
    auto op = get_fresh_Var("i$"+method.name, true);

    // forall b. Ix b => a -> b -> b
    Type op_type = remove_top_gen(method_type);
    // forall b. Ix b => [x] -> b -> b
    op_type = apply_subst(subst, op_type);
    // forall x. (C1 x, C2 x) => forall b. Ix b => [x] -> b -> b
    op_type = add_forall_vars(instance_tvs,add_constraints(preds_from_lie(givens), op_type));

    // Don't write the op_type into the global type environment?
    // poly_env() = poly_env().insert( {op, op_type} );

    optional<Hs::FunDecl> FD;
    if (auto it = method_matches.find(method); it != method_matches.end())
    {
        FD = Hs::FunDecl({noloc,op}, it->second);
    }
    else if (class_info.default_methods.count(method.name))
    {
        auto dm_var = class_info.default_methods.at(method.name);

        FD = Hs::simple_decl({noloc,op}, {noloc,dm_var});
    }
    else
    {
        record_warning( Note() <<"instance "<<show_type_plain(inst_type)<<" is missing method '"<<method.name<<"'" );

        Hs::String msg("method `" + method.name + "` undefined in instance `" + show_type_plain(inst_type) + "`");
        Hs::LVar error{noloc,Hs::Var("Compiler.Error.error")};
        FD = Hs::simple_decl({noloc,op}, Hs::apply(error,{{noloc,Hs::Literal(msg)}}));
    }

    auto hs_decl = infer_type_for_single_fundecl_with_sig(*FD, op_type);

    auto S = symbol_info(op.name, symbol_type_t::instance_method, {}, {}, {});
    S.type = op_type;
    this_mod().add_symbol(S);

    return {hs_decl, make_core_var(op)};
}

// Generate a GND method by selecting the representation method under a representational equality wanted.
std::tuple<Hs::Decl,Core::Var<>> TypeChecker::type_check_gnd_instance_method(
    const Hs::Var& method, const Type& method_type,
    const std::vector<TypeVar>& instance_tvs, const LIE& givens, const substitution_t& subst,
    const ClassInfo& class_info, const TypeCon& class_con, const vector<Type>& instance_args)
{
    auto op = get_fresh_Var("i$"+method.name, true);

    auto representation = find_gnd_representation_dictionary(*this, givens, class_con, instance_args);
    assert(representation);
    auto [rep_type, rep_dict] = *representation;

    Type inst_method_type = apply_subst(subst, remove_top_gen(method_type));

    substitution_t rep_subst;
    auto rep_args = instance_args;
    rep_args.back() = rep_type;
    for(int i=0; i<class_info.type_vars.size(); i++)
        rep_subst = rep_subst.insert({class_info.type_vars[i], rep_args[i]});
    Type rep_method_type = apply_subst(rep_subst, remove_top_gen(method_type));

    // Core is untyped, so this wanted is the coercion proof gate; no Core wrapper is emitted.
    auto equality = make_role_equality_pred(Role::Representational, rep_method_type, inst_method_type);
    auto origin = GNDMethodOrigin{class_info.name, method.name, rep_method_type, inst_method_type};
    auto wanteds = preds_to_constraints(origin, Wanted, {equality});
    auto ev_decls = maybe_implication(instance_tvs, givens, [&](auto& tc) {tc.current_wanteds() = wanteds;});

    auto selector = class_method_selector(class_info, method);
    assert(selector);

    auto inner_id = get_fresh_Var("gnd$"+method.name, false);
    Core::Decls<> body_decls;
    body_decls.push_back({make_core_var(inner_id), make_apply<>(Core::Exp<>(make_core_var(*selector)), vector<Core::Var<>>{rep_dict})});
    std::shared_ptr<const Core::Decls<>> method_decls = std::make_shared<Core::Decls<>>(body_decls);

    Type op_type = add_forall_vars(instance_tvs,add_constraints(preds_from_lie(givens), inst_method_type));

    std::map<Hs::Var,Hs::BindInfo> bind_infos;
    bind_infos.insert({op, Hs::BindInfo(op, inner_id, inst_method_type, op_type, Core::WrapId)});
    auto hs_decl = Hs::GenBind(instance_tvs, dict_vars_from_lie(givens), {ev_decls, method_decls}, {}, bind_infos);

    auto S = symbol_info(op.name, symbol_type_t::instance_method, {}, {}, {});
    S.type = op_type;
    this_mod().add_symbol(S);

    return {hs_decl, make_core_var(op)};
}

// FIXME: can we make the dictionary definition into an Hs::Decl?
//        then we can just put the wrapper on the Hs::Var in the decl.
pair<Hs::Decls, Core::Decl<>>
TypeChecker::infer_type_for_instance2(const Core::Var<>& dfun, const Hs::InstanceDecl& inst_decl)
{
    // 1. Get instance head and constraints (givens)

    auto S = this_mod().lookup_local_symbol(dfun.name);

    // This could be Num Int or forall a b.(Ord a, Ord b) => Ord (a,b)
    auto inst_type = S->instance_info->type();
    auto note = note_scope( Note()<<"In instance `"<<show_type_plain(inst_type)<<"`:" );

    auto span = source_span_scope(inst_decl.polytype.loc);
    // Instantiate it with rigid type variables.
    auto tc2 = copy_clear_wanteds(true);
    auto [wrap_gen, instance_tvs, givens, instance_head] = tc2.skolemize(inst_type, true);
    auto [instance_class, instance_args] = decompose_type_apps(instance_head);
    auto instance_dict_args = dict_vars_from_lie(givens);

    // 2. Get the class info
    auto class_con = get_class_for_constraint(instance_head);
    auto class_name = class_con.name;
    auto class_info = *info_for_class(class_name);

    // 3. Get substitution from general class to specific instance head.
    substitution_t subst;
    for(int i=0; i<class_info.type_vars.size(); i++)
        subst = subst.insert({class_info.type_vars[i], instance_args[i]});

    // 4. Construct superclasses (wanteds) from instance dictionaries (givens)
    //    For example in the instance Ord a => Ord [a] we need to construct Eq [a] as part of the dictionary.
    //    So we need to do Ord a => Eq [a]
    vector<Type> superclass_constraints = class_info.context;
    for(auto& superclass_constraint: superclass_constraints)
        superclass_constraint = apply_subst(subst, superclass_constraint);

    auto wanteds = preds_to_constraints(GivenOrigin(), Wanted, superclass_constraints);
    std::shared_ptr<const Core::Decls<>> decls_super;
    {
        auto superclass_note = note_scope(Note()<<"Deriving superclass constraints for "<<show_type_plain(instance_head));
        decls_super = maybe_implication(instance_tvs, givens, [&](auto& tc) {tc.current_wanteds() = wanteds;});
    }

    // 5. Superclass fields in the instance dictionary.
    Hs::Decls decls;

    std::vector<Core::Var<>> instance_sc_methods;

    for(auto& superclass_constraint: dictionary_constraints(wanteds))
    {
        auto [hs_decl, superclass_selector] = type_check_superclass_entry(superclass_constraint, class_name,
                                                                          instance_tvs, givens, subst,
                                                                          instance_dict_args, decls_super);

        instance_sc_methods.push_back(Core::Var<>(superclass_selector.name));
        decls.push_back({noloc,hs_decl});
    }


    // 6. Method fields in the instance dictionary.
    auto method_matches = inst_decl.generalized_newtype_deriving
        ? map<Hs::Var, Hs::Matches>{}
        : get_instance_methods( inst_decl.method_decls, class_info.members, class_name );

    // OK, so lets say that we just do \idvar1 .. idvarn -> let ev_binds = entails( )
    for(const auto& [method, method_type]: class_info.members)
    {
        auto [hs_decl, op] = inst_decl.generalized_newtype_deriving
            ? type_check_gnd_instance_method(method, method_type, instance_tvs, givens, subst, class_info, class_con, instance_args)
            : type_check_instance_method(method, method_type, instance_tvs, givens, inst_type, subst, method_matches, class_info);

        instance_sc_methods.push_back(op);
        decls.push_back({noloc,hs_decl});
    }

    // 7. Construct the dictionary from the constructor functions for its entries
    //    (Extractor functions are implemented in class.cc: dictionary_extractor( ))
    Core::Decls<> dict_decls;
    vector<Core::Exp<>> dict_entries;
    for(auto& op: instance_sc_methods)
    {
        auto dict_entry = get_fresh_core_var("de$",false);
        Core::Decl<> dict_decl{dict_entry, make_apply<>(Core::Exp<>(op), instance_dict_args)};
        dict_decls.push_back(dict_decl);
        dict_entries.push_back(dict_entry);
    }

    // dfun = /\a1..an -> \dicts:theta -> <superdict_vars,method_vars>
    Core::Exp<> dict = Core::ConApp<>(class_name, dict_entries);
    if (dict_entries.size() == 1)
        dict = dict_entries[0];
    dict = make_let(dict_decls, dict);

    // 8. Construct the DFunUnfolding
    // We need to convert instance_dict_args and instance_sc_methods from Core::<> to Occ::
    vector<Occ::Var> occ_dvars;
    for(auto& dv: instance_dict_args)
    {
        Occ::Var du(dv.name);
        du.info.work_dup = amount_t::Once;
        du.info.code_dup = amount_t::Once;
        occ_dvars.push_back(du);
    }
    vector<Occ::Exp> occ_args;
    set<Occ::Var> free_vars;
    for(auto& op: instance_sc_methods)
    {
        Occ::Var occ_op(op.name);
        occ_op.info.work_dup = amount_t::Many;
        occ_op.info.code_dup = amount_t::Many;
        occ_args.push_back(make_apply<occurrence_info,std::monostate>(occ_op, occ_dvars));
        free_vars.insert(occ_op);
    }
    
    S->unfolding = DFunUnfolding{occ_dvars, class_name, occ_args, free_vars};
    // We also need to make sure that the instance methods reference here are marked exported?
    // What is the story on knowing what things to mark exported?
    // Unlike other things, we don't week to retain a list of the instance_sc_methods anywhere, so
    //  so mark_exported_decls doesn't have it.
    // We could ask the module to give us a list of the symbols with type symbol_type_t::instance_superclass_selector and
    //  symbol_type_t::instance_superclass_selector.

    return {decls, {dfun, wrap_gen(dict)}};
}

// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
pair<Hs::Binds, Core::Decls<>>
TypeChecker::infer_type_for_instances2(const vector<pair<Core::Var<>, Hs::InstanceDecl>>& named_instances)
{
    Hs::Binds instance_method_decls;
    Core::Decls<> dfun_decls;

    for(auto& [dfun, instance_decl]: named_instances)
    {
        auto [decls, dfun_decl] = infer_type_for_instance2(dfun, instance_decl);

        instance_method_decls.push_back(decls);
        dfun_decls.push_back(dfun_decl);
    }
//    std::cerr<<"\nInstance ops and dfuns:\n";
//    std::cerr<<instance_decls.print();
//    std::cerr<<"\n\n";

    return {instance_method_decls, dfun_decls};
}

bool TypeChecker::more_specific_than(const Type& type1, const Type& type2)
{
    // We can get type1 by constraining type2, so type1 is more specific than type2.
    return maybe_match(type2, type1) and not maybe_match(type1, type2);
}

bool is_type_variable(const Type& t)
{
    if (auto mtv = t.to<MetaTypeVar>())
    {
        if (auto tt = mtv->filled())
            return is_type_variable(*tt);
        else
            return true;
    }
    else if (t.to<TypeVar>())
        return true;
    else
        return false;
}

bool possible_instance_for(Type t)
{
    int n = 0;

    t = follow_meta_type_var(t);

    while(auto app = t.to<TypeApp>())
    {
        if (not is_type_variable(app->arg))
            n++;
        t = follow_meta_type_var(app->head);
    }

    if (t.to<TypeCon>())
        return (n > 0);
    else
        return false;
}

InstanceInfo TypeChecker::freshen(InstanceInfo info)
{
    auto s = fresh_tv_binders(info.tvs);
    info.args = apply_subst(s, info.args);
    info.constraints = apply_subst(s, info.constraints);
    return info;
}

EqInstanceInfo TypeChecker::freshen(EqInstanceInfo info)
{
    auto s = fresh_tv_binders(info.tvs);
    info.lhs = apply_subst(s, info.lhs);
    info.rhs = apply_subst(s, info.rhs);
    return info;
}

// 1. An instance looks like (forall as.Q1(as) => Q2(as))
// 2. We have some constraints Q3.
// 3. We instantiate the instance with substitutions [as->gs].
// 4. We'd like to check if Q3 ~ [as->gs]Q2, where only the variables gs are allowed to be unified.
// 5. If we give the unification variables gs a higher level, can we guarantee that only
//    gs will be constrained?
// 6. Actually, I don't think so... Suppose that the instance is (Eq Int) and the constraint is
//    Eq a.
// 7. Unless we actually FORBID unification of variables at any higher level, then this won't work.
// 8. Simply forbidding substitution to a deeper depth won't cut it.

// FIXME! Change this to take a Constraint, which includes the tc_state for the constraint we are trying to satisfy.
optional<pair<Core::Exp<>,LIE>> TypeChecker::lookup_instance(const Type& target_pred)
{
    vector<tuple<pair<Core::Exp<>, LIE>,Type,Type,InstanceInfo>> matching_instances;

    vector<InstanceInfo> unifying_instances;

    TypeCon target_class = get_class_for_constraint(target_pred);

    // It IS possible to have an instance like (HasRoots t, IsTree t) => HasRoot t.
    // So we can't bail just because all the arguments in target_pred are type variables.

    vector<const InstanceEnv*> instance_envs({&this_mod().local_instances});
    for(auto& [modid, mod]: this_mod().transitively_imported_modules)
        instance_envs.push_back(&mod->local_instances());

    for(auto& instance_env: instance_envs)
    {
        for(auto& [dfun, info_]: *instance_env)
        {
            if (info_.class_con != target_class) continue;

            auto info = freshen(info_);

            auto instance_head = type_apply(info.class_con, info.args);

            if (auto subst = maybe_match(instance_head, target_pred))
            {
                auto preds = apply_subst(*subst, info.constraints);

                auto wanteds = preds_to_constraints(InstanceOrigin(), Wanted, preds);

                auto type = apply_subst(*subst, instance_head);

                Core::Exp<> dfun_exp = make_apply<>(Core::Exp<>(dfun), dict_vars_from_lie(wanteds));

                matching_instances.push_back({{dfun_exp, wanteds}, type, instance_head, info_});
            }
            else if (auto subst = maybe_unify(instance_head, target_pred))
            {
                unifying_instances.push_back(info);
            }
        }
    }

    if (matching_instances.size() == 0)
        return {}; // No matching instances

    vector<tuple<pair<Core::Exp<>, LIE>,Type,Type,InstanceInfo>> surviving_instances;

    for(int i=0;i<matching_instances.size();i++)
    {
        auto& [_1, _2, type_i, info_i] = matching_instances[i];

        bool keep = true;
        for(int j=0;keep and j<matching_instances.size();j++)
        {
            if (i == j) continue;

            auto& [_3, _4, type_j, info_j] = matching_instances[j];

            if (more_specific_than(type_j, type_i) and (info_i.overlappable or info_j.overlapping))
                keep = false;
        }

        if (keep)
            surviving_instances.push_back(matching_instances[i]);
    }


    // FIXME: We really should print the instance LOCATIONS!
    if (surviving_instances.size() > 1)
    {
        TidyState tidy_state;
        auto n = Note()<<"Too many matching instances for "<<show_type_plain(tidy_state, target_pred)<<":\n";
        for(auto& [_,type1,type2,info]: surviving_instances)
	{
	    auto instance_head = type_apply(info.class_con, info.args);
            n<<"  "<<show_type_plain(tidy_state, remove_top_gen(instance_head))<<"\n";
	}
        record_error(n);
    }

    // We are also supposed to search in-scope given constraints.
    // These are actually OK if they are all top-level instances (not given constraints, I presume) that are marked incoherent.
    for(auto& unifying_instance: unifying_instances)
    {
        if (unifying_instance.incoherent) continue;

        TidyState tidy_state;
        auto n = Note()<<"Predicate "<<show_type_plain(tidy_state, target_pred)<<" unifies, but does not match with instance "
                       <<show_type_plain(tidy_state, remove_top_gen(unifying_instance.type()));
        record_error(n);
    }

    return std::get<0>(surviving_instances[0]);
}

bool TypeChecker::find_type_eq_instance_1way(const Type& t1, const Type& t2)
{
    if (not is_type_fam_app(t1)) return false;

    auto constraint = make_equality_pred(t1, t2);

    if (auto inst = lookup_instance(constraint))
    {
        auto [dfun_exp, super_wanteds] = *inst;

//            What is the evidence for type family instances?
//            decls.push_back( { dvar, dfun_exp } );

        if (super_wanteds.size())
            throw note_exception()<<"type family instances can't have constraints!";

        return true;
    }

    return false;
}

bool TypeChecker::find_type_eq_instance(const Type& t1, const Type& t2)
{
    return find_type_eq_instance_1way(t1,t2) or find_type_eq_instance_1way(t2,t1);
}
