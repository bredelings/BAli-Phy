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
        if (loc) push_source_span( *loc );

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

        if (loc) pop_source_span();
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

std::tuple<std::vector<TypeVar>, Type, Type> TypeChecker::check_type_instance(const Hs::LType& hs_lhs, const Hs::LType& hs_rhs)
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

    return {free_tvs, lhs, rhs};
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

void TypeChecker::check_add_type_instance(const Hs::TypeFamilyInstanceEqn& inst, const optional<string>& associated_class, const substitution_t& instance_subst)
{
    push_note( Note()<<"In instance '"<<inst.print()<<"':" );
    TypeCon tf_con(unloc(inst.con).name);
    auto inst_loc = *(inst.con.loc * range(inst.args) * inst.rhs.loc);
    push_source_span( inst_loc );

    // 1. Check that the type family exists.
    if (not type_con_is_type_fam( tf_con ) )
    {
        push_source_span( *inst.con.loc );
        record_error( Note()<<"  No type family '"<<inst.con.print()<<"'");
        pop_source_span();

        pop_source_span();
        pop_note();
        return;
    }

    auto hs_lhs = Hs::type_apply(inst.con, inst.args);
    auto [free_ltvs, lhs, rhs] = check_type_instance(hs_lhs, inst.rhs);
    auto [_, inst_args] = decompose_type_apps(lhs);

    // There CAN be multiple type instances for an associated type family, if they don't unify with each other.
    // We don't check if a class has only one instance, so I guess we allow this.
    // But we don't check if the two instances are "apart" either.

    // 2. Get the type family info
    auto tf_info = info_for_type_fam( tf_con.name );

    if (tf_info->associated_class)
    {
        // 3. Check for unassociated instances declared for associated classes
        if (not associated_class)
        {
            push_source_span( *inst.con.loc );
            record_error( Note() << "  Can't declare non-associated type instance for type family '"<<inst.con.print()<<"' associated with class '"
                          <<(*tf_info->associated_class)<<"'");
            pop_source_span();

            pop_source_span();
            pop_note();
            return;
        }

        // 4. Check for instances associated with the wrong class
        if (*tf_info->associated_class != *associated_class)
        {
            record_error(Note() << "  Trying to declare type instance in class '"<<*associated_class<<" for family '"<<inst.con.print()
                         <<"' associated with class '"<<(*tf_info->associated_class)<<"'");

            pop_source_span();
            pop_note();
            return;
        }

        // 4.5. Check that the type family was given the right number of arguments.
        if (inst.args.size() != tf_info->args.size())
        {
            push_source_span( *(inst.con.loc * range(inst.args)) );
            record_error(Note() << "  Type family takes "<<tf_info->args.size()<<" arguments, but was given "<<inst.args.size()<<".");
            pop_source_span();
            pop_source_span();
            pop_note();
            return;
        }

        // 5. Check that arguments corresponding to class parameters are the same as the parameter type for the instance.
        for(int i=0;i<tf_info->args.size();i++)
        {
            auto fam_tv = tf_info->args[i];
            push_source_span( *inst.args[i].loc );
            if (instance_subst.count(fam_tv))
            {
                auto expected = instance_subst.at(fam_tv);
                if (not same_type( inst_args[i], expected))
                    record_error( Note() << "    argument '"<<inst.args[i]<<"' should match instance parameter '"<<expected<<"'");
            }
            pop_source_span();
        }
    }

    // 6. Check that the type family is not closed
    if (tf_info->closed)
    {
        record_error( Note() << "  Can't declare additional type instance for closed type family '"<<inst.con.print()<<"'");

        pop_source_span();
        pop_note();
        return;
    }


    // 7. Check that the type instance has the right number of arguments
    if (inst.args.size() != tf_info->args.size())
    {
        push_source_span( *range(inst.args) );
        record_error( Note() << "    Expected "<<tf_info->args.size()<<" parameters, but got "<<inst.args.size());
        pop_source_span();

        pop_source_span();
        pop_note();
        return;
    }

    add_type_instance(free_ltvs, lhs, rhs);

    pop_source_span();
    pop_note();
}

void TypeChecker::default_type_instance(const TypeCon& tf_con,
					const std::optional<TypeFamilyInstanceDecl>& maybe_default,
					const substitution_t& instance_subst)
{
    if (not maybe_default)
    {
	record_warning( Note() <<"No instance for associated type family "<<tf_con );
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

std::optional<Core2::Var<>>
TypeChecker::infer_type_for_instance1(const Hs::InstanceDecl& inst_decl)
{
    auto inst_loc = inst_decl.polytype.loc;
    push_note( Note()<<"In instance '"<<unloc(inst_decl.polytype)<<"':" );
    push_source_span( *inst_loc );

    // 1. Kind-check the type and set the kinds for the type variables.

    // FIXME: allow check_constraint to fail by returning an optional<Type>.
    auto polytype = check_constraint(inst_decl.polytype);

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

    pop_source_span();
    pop_note();
    return dfun;
}


// See Tc/TyCl/Instance.hs
// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
vector<pair<Core2::Var<>,Hs::InstanceDecl>>
TypeChecker::infer_type_for_instances1(const Hs::Decls& decls)
{
    vector<pair<Core2::Var<>, Hs::InstanceDecl>> named_instances;

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

        if (fd.v.loc) push_source_span(*fd.v.loc);
        if (not members.count(method))
        {
            record_error( Note()<<"'"<<method_name<<"' is not a member of class '"<<class_name<<"'" );
            if (fd.v.loc) pop_source_span();
            continue;
        }

        if (method_matches.count(method))
        {
            record_error( Note() <<"method '"<<method_name<<"' defined twice!" );
            if (fd.v.loc) pop_source_span();
            continue;
        }

        method_matches.insert({method, fd.matches});
        if (fd.v.loc) pop_source_span();
    }

    return method_matches;
}

// FIXME: can we make the dictionary definition into an Hs::Decl?
//        then we can just put the wrapper on the Hs::Var in the decl.
pair<Hs::Decls, tuple<Core2::Var<>, Core2::wrapper, Core2::Exp<>>>
TypeChecker::infer_type_for_instance2(const Core2::Var<>& dfun, const Hs::InstanceDecl& inst_decl)
{
    push_note( Note()<<"In instance `"<<inst_decl.polytype<<"`:" );

    // 1. Get instance head and constraints (givens)

    // This could be Num Int or forall a b.(Ord a, Ord b) => Ord (a,b)
    auto inst_info = this_mod().lookup_local_symbol(dfun.name)->instance_info;
    auto inst_type = inst_info->type();

    push_source_span(*inst_decl.polytype.loc);
    // Instantiate it with rigid type variables.
    auto tc2 = copy_clear_wanteds(true);
    auto [wrap_gen, instance_tvs, givens, instance_head] = tc2.skolemize(inst_type, true);
    auto [instance_class, instance_args] = decompose_type_apps(instance_head);

    // 2. Get the class info
    auto class_con = get_class_for_constraint(instance_head);
    auto class_name = class_con.name;
    auto class_info = *info_for_class(class_name);

    // 3. Get substitution from general class to specific instance head.
    substitution_t subst;
    for(int i=0; i<class_info.type_vars.size(); i++)
        subst = subst.insert({class_info.type_vars[i], instance_args[i]});

    // 4. Get (constrained) superclass constraints (wanteds)
    push_note(Note()<<"Deriving superclass constraints for "<<instance_head.print());
    vector<Type> superclass_constraints = class_info.context;
    for(auto& superclass_constraint: superclass_constraints)
        superclass_constraint = apply_subst(subst, superclass_constraint);

    // 5. Compute the superclass dictionaries (wanteds) from the instance dictionaries (givens).
    auto wanteds = preds_to_constraints(GivenOrigin(), Wanted, superclass_constraints);
    auto decls_super = maybe_implication(instance_tvs, givens, [&](auto& tc) {tc.current_wanteds() = wanteds;});
    auto wrap_let = Core2::WrapLet(decls_super);
    pop_note();

    // 6. Start adding fields for the superclass dictionaries
    vector<Core2::Var<>> dict_entries = dict_vars_from_lie(wanteds);

    // 7. Construct binds_methods
    Hs::Decls decls;

    auto method_matches = get_instance_methods( inst_decl.method_decls, class_info.members, class_name );
    string classdict_name = "d$" + get_class_name_from_constraint(instance_head);

    string spec_type = "";
    for(auto& arg: instance_args)
        spec_type += class_arg_name(arg);
    
    // OK, so lets say that we just do \idvar1 .. idvarn -> let ev_binds = entails( )
    Core2::Decls<> dict_decls;
    for(const auto& [method, method_type]: class_info.members)
    {
        auto& method_name = method.name;

        // push_note( Note()<<"In method `"<<method_name<<"`:" );

        auto op = get_fresh_Var("i$"+method_name, true);

        // forall b. Ix b => a -> b -> b
        Type op_type = remove_top_gen(method_type);
        // forall b. Ix b => [x] -> b -> b
        op_type = apply_subst(subst, op_type);
        // forall x. (C1 x, C2 x) => forall b. Ix b => [x] -> b -> b
        op_type = add_forall_vars(instance_tvs,add_constraints(preds_from_lie(givens), op_type));

        // Don't write the op_type into the global type environment?
        // poly_env() = poly_env().insert( {op, op_type} );

        string dict_entry_name = "de_" + method.name + "$" + spec_type;
        optional<Hs::FunDecl> FD;
        if (auto it = method_matches.find(method); it != method_matches.end())
        {
            FD = Hs::FunDecl({noloc,op}, it->second);
        }
        else
        {
            if (class_info.default_methods.count(method_name))
            {
                auto dm_var = class_info.default_methods.at(method_name);

                FD = Hs::simple_decl({noloc,op}, {noloc,dm_var});
            }
            else
            {
                record_warning( Note() <<"instance "<<inst_decl.polytype<<" is missing method '"<<method_name<<"'" );

                // We could synthesize an actual method to call...
                // But how do we typecheck the expression (Compiler.Error.error msg) if error isn't in scope?
                auto dict_entry = get_fresh_core_var(dict_entry_name,false);
                dict_decls.push_back({dict_entry, Core2::error("method `" + method.name + "` undefined in instance `" + inst_decl.polytype.print() + "`") });
                dict_entries.push_back( dict_entry );

                // pop_note();
                continue;
            }
        }

        auto dict_entry = get_fresh_core_var(dict_entry_name, false);
        dict_decls.push_back({dict_entry, make_apply<>(Core2::Exp<>(make_core_var(op)), dict_vars_from_lie(givens))});
        dict_entries.push_back( dict_entry );

        auto decl2 = infer_type_for_single_fundecl_with_sig(*FD, op_type);
        decl2.dict_decls.insert( decl2.dict_decls.begin(), decls_super );
        decls.push_back({noloc,decl2});

        auto S = symbol_info(op.name, symbol_type_t::instance_method, {}, {}, {});
        S.type = op_type;
        this_mod().add_symbol(S);

        // pop_note();
    }

    // NOTE: See class.cc: dictionary_extractor( ) for the extractor functions.

    // dfun = /\a1..an -> \dicts:theta -> let decls_super in <superdict_vars,method_vars>
    Core2::Exp<> dict = Core2::ConApp<>(class_name, dict_entries);
    if (dict_entries.size() == 1)
        dict = dict_entries[0];
    dict = make_let(dict_decls, dict);

    auto wrap = wrap_gen * wrap_let;

    pop_source_span();

    pop_note();

    return {decls, {dfun, wrap, dict}};
}

// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
pair<Hs::Binds, vector<tuple<Core2::Var<>, Core2::wrapper, Core2::Exp<>>>>
TypeChecker::infer_type_for_instances2(const vector<pair<Core2::Var<>, Hs::InstanceDecl>>& named_instances)
{
    Hs::Binds instance_method_decls;
    vector<tuple<Core2::Var<>, Core2::wrapper, Core2::Exp<>>> dfun_decls;

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
optional<pair<Core2::Exp<>,LIE>> TypeChecker::lookup_instance(const Type& target_pred)
{
    vector<tuple<pair<Core2::Exp<>, LIE>,Type,Type,InstanceInfo>> matching_instances;

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

                Core2::Exp<> dfun_exp = make_apply<>(Core2::Exp<>(dfun), dict_vars_from_lie(wanteds));

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

    vector<tuple<pair<Core2::Exp<>, LIE>,Type,Type,InstanceInfo>> surviving_instances;

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
        auto n = Note()<<"Too many matching instances for "<<target_pred<<":\n";
        for(auto& [_,type1,type2,info]: surviving_instances)
	{
	    auto instance_head = type_apply(info.class_con, info.args);
            n<<"  "<<remove_top_gen(instance_head)<<"\n";
	}
        record_error(n);
    }

    // We are also supposed to search in-scope given constraints.
    // These are actually OK if they are all top-level instances (not given constraints, I presume) that are marked incoherent.
    for(auto& unifying_instance: unifying_instances)
    {
        if (unifying_instance.incoherent) continue;

        auto n = Note()<<"Predicate "<<target_pred<<" unifies, but does not match with instance "
                       <<remove_top_gen(unifying_instance.type());
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

