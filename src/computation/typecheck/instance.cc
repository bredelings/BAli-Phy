#include "typecheck.H"
#include "kindcheck.H"
#include "haskell/ids.H"

#include "computation/expression/apply.H"
#include "computation/expression/tuple.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::tuple;
using std::optional;

Hs::Decls TypeChecker::infer_type_for_default_methods(const Hs::ClassDecl& C)
{
    Hs::Decls decls_out;

    auto class_info = class_env().at(C.name);

    for(auto& decl: C.default_method_decls)
    {
        auto FD = decl.as_<Hs::FunDecl>();
        auto dm = class_info.default_methods.at(FD.v);
        FD.v = dm;

        auto [decl2, sig_type] = infer_type_for_single_fundecl_with_sig(FD);
        decls_out.push_back(decl2);
    }

//    std::cerr<<"Default method ops:\n";
//    std::cerr<<decls_out.print();
//    std::cerr<<"\n\n";

    return decls_out;
}

Hs::Binds TypeChecker::infer_type_for_default_methods(const Hs::Decls& decls)
{
    Hs::Binds default_method_decls;
    for(auto& decl: decls)
    {
        auto c = decl.to<Hs::ClassDecl>();
        if (not c) continue;

        default_method_decls.push_back( infer_type_for_default_methods(*c) );
    }
    return default_method_decls;
}

string get_name_for_typecon(const TypeCon& tycon)
{
    auto n = unloc(tycon.name);

    if (n == "[]")
        return "List";
    else if (n == "->")
        return "Func";
    else if (is_tuple_name(n))
    {
        int m = tuple_arity(n);
        return std::to_string(m)+"Tuple";
    }
    else
        return get_unqualified_name(n);
}

void TypeChecker::check_add_type_instance(const Hs::TypeFamilyInstanceEqn& inst, const optional<string>& associated_class, const substitution_t& instance_subst)
{
    push_note( Note()<<"In instance '"<<inst.print()<<"':" );
    auto tf_con = desugar(inst.con);
    
    // 1. Check that the type family exists.
    if (not type_fam_info().count( tf_con ))
        record_error( Note()<<"  No type family '"<<inst.con.print()<<"'");

    // 2. Get the type family info
    auto& tf_info = type_fam_info().at(tf_con);

    if (tf_info.associated_class)
    {
        // 3. Check for unassociated instances declared for associated classes
        if (not associated_class)
        {
            record_error(Note() << "  Can't declare non-associated type instance for type family '"<<inst.con.print()<<"' associated with class '"
                         <<(*tf_info.associated_class)<<"'");
            return;
        }

        // 4. Check for instances associated with the wrong class
        if (*tf_info.associated_class != *associated_class)
        {
            record_error(Note() << "  Trying to declare type instance in class '"<<*associated_class<<" for family '"<<inst.con.print()
                         <<"' associated with class '"<<(*tf_info.associated_class)<<"'");
            return;
        }

        // 5. Check that arguments corresponding to class parameters are the same as the parameter type for the instance.
        for(int i=0;i<tf_info.args.size();i++)
        {
            auto fam_tv = tf_info.args[i];
            if (instance_subst.count(fam_tv))
            {
                auto expected = instance_subst.at(fam_tv);
                if (not same_type( desugar(inst.args[i]), expected))
                    record_error(Note() << "    argument '"<<inst.args[i]<<"' should match instance parameter '"<<expected<<"'");
            }
        }
    }

    // 6. Check that the type family is not closed
    if (tf_info.closed)
    {
        record_error( Note() << "  Can't declare additional type instance for closed type family '"<<inst.con.print()<<"'");
        return;
    }
            

    // 7. Check that the type instance has the right number of arguments
    if (inst.args.size() != tf_info.args.size())
    {
        record_error( Note() << "    Expected "<<tf_info.args.size()<<" parameters, but got "<<inst.args.size());
        return;
    }

    // 8. The rhs may only mention type vars bound on the lhs.
    set<TypeVar> lhs_tvs;
    for(auto& arg: desugar(inst.args))
    {
        for(auto& tv: free_type_variables(arg))
            lhs_tvs.insert(tv);
    }

    for(auto& tv: free_type_variables(desugar(inst.rhs)))
        if (not lhs_tvs.count(tv))
        {
            record_error( Note() <<"  rhs variable '"<<tv.print()<<"' not bound on the lhs.");
            return;
        }

    // 9. Kind-check the parameters and result type, and record the free type variables.
    TypeFamEqnInfo eqn{ desugar(inst.args), desugar(inst.rhs), lhs_tvs | ranges::to<vector>()};

    // 9a. Bind the free type vars
    kindchecker_state K( tycon_info() );
    K.push_type_var_scope();
    for(auto& tv: eqn.free_tvs)
    {
        assert(not K.type_var_in_scope(tv));
        tv.kind = K.fresh_kind_var();
        K.bind_type_var(tv, *tv.kind);
    }

    // 9b. Kind-check the type vars
    for(int i=0; i<eqn.args.size(); i++)
        K.kind_check_type_of_kind(eqn.args[i], *tf_info.args[i].kind);
    K.kind_check_type_of_kind(eqn.rhs, tf_info.result_kind);

    // 9c. Record the final kinds for the free type vars
    for(int i=0; i<eqn.args.size(); i++)
        eqn.args[i] = K.zonk_kind_for_type(eqn.args[i]);
    eqn.rhs = K.zonk_kind_for_type(eqn.rhs);

    for(auto& tv: eqn.free_tvs)
        tv.kind = replace_kvar_with_star(K.kind_for_type_var(tv));

    // 10. Add the (~) instance to the instance environment
    Type lhs = make_tyapps(tf_con, eqn.args);
    Type constraint = make_equality_pred(lhs, eqn.rhs);
    Type inst_type = add_forall_vars(eqn.free_tvs, constraint);

    InstanceInfo info{eqn.free_tvs,{},TypeCon({noloc,"~"}),{lhs, eqn.rhs}};

    int eqn_id = FreshVarSource::current_index();
    auto dvar = fresh_dvar(constraint);

    instance_env().insert( {dvar, info} );

    // 11. Make up an equation id -- this is the "evidence" for the type family instance.
    tf_info.equations.insert({eqn_id, eqn});

    pop_note();
}

pair<Core::Var, InstanceInfo>
TypeChecker::infer_type_for_instance1(const Hs::InstanceDecl& inst_decl)
{
    push_note( Note()<<"In instance '"<<inst_decl.constraint<<"':" );

    // 1. Get class name and parameters for the instance
    auto [class_head, class_args] = decompose_type_apps(desugar(inst_decl.constraint));

    // 2. Look up the class info
    auto tc = class_head.to<TypeCon>();
    if (not tc)
        throw note_exception()<<"  "<<class_head<<" is not a type constructor!";

    // Check that this is a class, and not a data or type?
    auto class_name = unloc(tc->name);
    if (not class_env().count(class_name))
        throw note_exception()<<"  no class '"<<class_name<<"'!";
    auto class_info = class_env().at(class_name);

    // 3. Check that the instance has the right number of parameters
    int N = class_info.type_vars.size();
    if (class_args.size() != class_info.type_vars.size())
        throw note_exception()<<"  should have "<<N<<" parameters, but has "<<class_args.size()<<".";

    // 4. Construct the mapping from original class variables to instance variables
    substitution_t instance_subst;
    for(int i = 0; i < N; i++)
        instance_subst = instance_subst.insert( {class_info.type_vars[i], class_args[i]} );

    // 5. Find the type vars mentioned in the constraint.
    set<TypeVar> type_vars = free_type_variables(desugar(inst_decl.constraint));

    // 6. The class_args must be (i) a variable or (ii) a type constructor applied to simple, distinct type variables.
    string tycon_names;
    for(auto& class_arg: class_args)
    {
        if (class_arg.to<TypeVar>())
        {
            tycon_names += "_";
        }
        else
        {
            auto [a_head, a_args] = decompose_type_apps(class_arg);

            if (auto tc = a_head.to<TypeCon>())
                tycon_names += get_name_for_typecon(*tc);
            else
                throw note_exception()<<"  '"<<a_head.print()<<"' is not a type constructor!";

            // Now, tc needs to be a data type constructor!
            // With FlexibleInstances, (i) the arguments do NOT have to be variables and (ii) type synonyms are allowed.
        }
    }

    // Premise 5: Check that the context contains no variables not mentioned in `class_arg`
    for(auto& tv: free_type_variables(desugar(inst_decl.context.constraints)))
    {
        if (not type_vars.count(tv))
            throw note_exception()<<"  Constraint context '"<<inst_decl.context.print()<<"' contains type variable '"<<tv.print()<<"' that is not mentioned in the instance declaration";
    }


    // Look at associated type instances
    for(auto& inst: inst_decl.type_inst_decls)
        check_add_type_instance(inst, class_name, instance_subst);

    string dfun_name = "d"+get_unqualified_name(class_info.name)+tycon_names;

    auto dfun = get_fresh_var(dfun_name, true);

    //  -- new -- //
    Type inst_type = add_constraints(desugar(inst_decl.context.constraints), desugar(inst_decl.constraint));
    inst_type = check_constraint( inst_type );  // kind-check the constraint and quantify it.

    // -- break down the inst_type into pieces. -- //
    auto tt = inst_type;
    vector<TypeVar> tvs;
    if (auto forall = tt.to<ForallType>())
    {
        tvs = forall->type_var_binders;
        tt = forall->type;
    }
    vector<Type> constraints;
    if (auto c = tt.to<ConstrainedType>())
    {
        constraints = c->context.constraints;
        tt = c->type;
    }
    auto [head,args] = decompose_type_apps(tt);
    auto class_con = head.to<TypeCon>();
    assert(class_con);
    
    InstanceInfo info{tvs, constraints, *class_con, args};
    
    pop_note();

    return {dfun, info};
}


// See Tc/TyCl/Instance.hs
// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
vector<pair<Core::Var,Hs::InstanceDecl>>
TypeChecker::infer_type_for_instances1(const Hs::Decls& decls)
{
    vector<pair<Core::Var, Hs::InstanceDecl>> named_instances;

    for(auto& decl: decls)
    {
        if (auto I = decl.to<Hs::InstanceDecl>())
        {
            auto [dfun, inst_info] = infer_type_for_instance1(*I);

            named_instances.push_back({dfun, *I});
            instance_env().insert( {dfun, inst_info} );
        }

        if (auto TI = decl.to<Hs::TypeFamilyInstanceDecl>())
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

map<Hs::Var, Hs::Matches> TypeChecker::get_instance_methods(const Hs::Decls& decls, const global_value_env& members, const string& class_name) const
{
    std::map<Hs::Var, Hs::Matches> method_matches;
    for(auto& decl: decls)
    {
        auto& fd = decl.as_<Hs::FunDecl>();
        auto& method = fd.v;
        string method_name = unloc(method.name);

        if (not members.count(method))
            throw note_exception()<<"'"<<method_name<<"' is not a member of class '"<<class_name<<"'";

        if (method_matches.count(method))
            throw note_exception()<<"method '"<<method_name<<"' defined twice!";

        method_matches.insert({method, fd.matches});
    }

    return method_matches;
}

// FIXME: can we make the dictionary definition into an Hs::Decl?
//        then we can just put the wrapper on the Hs::Var in the decl.
pair<Hs::Decls, tuple<Core::Var, Core::wrapper, Core::Exp>>
TypeChecker::infer_type_for_instance2(const Core::Var& dfun, const Hs::InstanceDecl& inst_decl)
{
    push_note( Note()<<"In instance `"<<inst_decl.constraint<<"`:" );

    // 1. Get instance head and constraints 

    // This could be Num Int or forall a b.(Ord a, Ord b) => Ord (a,b)
    auto& inst_info = instance_env().at(dfun);
    auto inst_type = inst_info.type();

    push_source_span(*inst_info.class_con.name.loc);
    // Instantiate it with rigid type variables.
    auto tc2 = copy_clear_wanteds(true);
    auto [wrap_gen, instance_tvs, givens, instance_head] = tc2.skolemize(inst_type, true);
    auto [instance_class, instance_args] = decompose_type_apps(instance_head);

    // 2. Get the class info
    auto class_con = get_class_for_constraint(instance_head);
    auto class_name = unloc(class_con.name);
    auto class_info = class_env().at(class_name);

    // 3. Get constrained version of the class
    substitution_t subst;
    for(int i=0; i<class_info.type_vars.size(); i++)
        subst = subst.insert({class_info.type_vars[i], instance_args[i]});

    // 4. Get (constrained) superclass constraints
    push_note(Note()<<"Deriving superclass constraints for "<<instance_head.print());
    auto superclass_constraints = class_info.context.constraints;
    for(auto& superclass_constraint: superclass_constraints)
        superclass_constraint = apply_subst(subst, superclass_constraint);

    // 5. Construct binds_super
    auto wanteds = preds_to_constraints(GivenOrigin(), Wanted, superclass_constraints);
    auto decls_super = maybe_implication(instance_tvs, givens, [&](auto& tc) {tc.current_wanteds() = wanteds;});
    auto wrap_let = Core::WrapLet(decls_super);
    pop_note();

    // 6. Start adding fields for the superclass dictionaries
    vector<Hs::Expression> dict_entries;
    for(auto& wanted: wanteds)
        dict_entries.push_back(wanted.ev_var);

    // 7. Construct binds_methods
    Hs::Decls decls;

    auto method_matches = get_instance_methods( inst_decl.method_decls, class_info.members, class_name );
    string classdict_name = "d" + get_class_name_from_constraint(instance_head);

    // OK, so lets say that we just do \idvar1 .. idvarn -> let ev_binds = entails( )
    for(const auto& [method, method_type]: class_info.members)
    {
        auto& method_name = unloc(method.name);

        push_note( Note()<<"In method `"<<method_name<<"`:" );

        auto op = get_fresh_Var("i"+method_name, true);

        dict_entries.push_back( Core::Apply(make_var(op), dict_vars_from_lie<Core::Exp>(givens)) );

        // forall b. Ix b => a -> b -> b
        Type op_type = remove_top_gen(method_type);
        // forall b. Ix b => [x] -> b -> b
        op_type = apply_subst(subst, op_type);
        // forall x. (C1 x, C2 x) => forall b. Ix b => [x] -> b -> b
        op_type = add_forall_vars(instance_tvs,add_constraints(preds_from_lie(givens), op_type));

        poly_env() = poly_env().insert( {op, op_type} );

        optional<Hs::FunDecl> FD;
        if (auto it = method_matches.find(method); it != method_matches.end())
        {
            FD = Hs::FunDecl(op, it->second);
        }
        else
        {
            if (not class_info.default_methods.count(method))
                record_error( Note() <<"instance "<<inst_decl.constraint<<" is missing method '"<<method_name<<"'" );

            auto dm_var = class_info.default_methods.at(method);

            FD = Hs::simple_decl(op, {noloc,dm_var});
        }
        auto [decl2, __] = infer_type_for_single_fundecl_with_sig(*FD);
        decls.push_back(decl2);
        pop_note();
    }

    // dfun = /\a1..an -> \dicts:theta -> let decls_super in <superdict_vars,method_vars>
    auto dict = Core::Tuple(dict_entries);

    auto wrap = wrap_gen * wrap_let;

    pop_source_span();

    pop_note();

    return {decls, {dfun, wrap, dict}};
}

// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
pair<Hs::Binds, vector<tuple<Core::Var, Core::wrapper, Core::Exp>>> TypeChecker::infer_type_for_instances2(const vector<pair<Core::Var, Hs::InstanceDecl>>& named_instances)
{
    Hs::Binds instance_method_decls;
    vector<tuple<Core::Var, Core::wrapper, Core::Exp>> dfun_decls;

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

bool TypeChecker::instance_matches(const Type& type1, const Type& type2)
{
    auto [_1, _2, head1] = instantiate(InstanceOrigin(), type1);
    auto [_3, _4, head2] = instantiate(InstanceOrigin(), type2);
    return maybe_match(head1, head2);
}

bool TypeChecker::more_specific_than(const Type& type1, const Type& type2)
{
    // We can get type1 by constraining type2, so type1 is more specific than type2.
    return instance_matches(type2, type1) and not instance_matches(type1, type2);
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

optional<pair<Core::Exp,LIE>> TypeChecker::lookup_instance(const Type& target_constraint)
{
    vector<pair<pair<Core::Exp, LIE>,Type>> matching_instances;

    TypeCon target_class = get_class_for_constraint(target_constraint);

    // If all arguments are variables, then we can't match an instance.
    if (not possible_instance_for(target_constraint)) return {};

    for(auto& [dfun, info]: instance_env() )
    {
        if (info.class_con != target_class) continue;

        auto type = info.type();

        auto [_, wanteds, instance_head] = instantiate(InstanceOrigin(), type);

        assert(not constraint_is_hnf(instance_head));

        if (not maybe_match(instance_head, target_constraint)) continue;

        auto dfun_exp = Core::Apply(dfun, dict_vars_from_lie<Core::Exp>(wanteds));

        matching_instances.push_back({{dfun_exp, wanteds}, type});
    }

    if (matching_instances.size() == 0)
        return {}; // No matching instances

    vector<pair<pair<Core::Exp, LIE>,Type>> surviving_instances;

    for(int i=0;i<matching_instances.size();i++)
    {
        auto type_i = matching_instances[i].second;

        bool keep = true;
        for(int j=0;keep and j<matching_instances.size();j++)
        {
            if (i == j) continue;

            auto type_j = matching_instances[j].second;

            if (more_specific_than(type_j, type_i))
                keep = false;
        }

        if (keep)
            surviving_instances.push_back(matching_instances[i]);
    }

    if (surviving_instances.size() > 1)
    {
        auto n = Note()<<"Too many matching instances for "<<target_constraint<<":\n";
        for(auto& [_,type]: surviving_instances)
            n<<"  "<<remove_top_gen(type)<<"\n";
        record_error(n);
    }

    assert(surviving_instances.size() == 1);

    return surviving_instances[0].first;
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

