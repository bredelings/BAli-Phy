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
using std::optional;

Hs::Decls typechecker_state::infer_type_for_default_methods(const Hs::ClassDecl& C)
{
    Hs::Decls decls_out;

    auto class_info = class_env().at(C.name);
    if (C.binds)
    {
        for(auto& decls: unloc(*C.binds))
            for(auto& decl: decls)
            {
                auto FD = decl.as_<Hs::FunDecl>();
                auto method_name = unloc(FD.v.name);
                auto dm = class_info.default_methods.at(method_name);
                FD.v = dm;

                auto [decl2, name, sig_type] = infer_type_for_single_fundecl_with_sig(FD);
                decls_out.push_back(decl2);
            }
    }

//    std::cerr<<"Default method ops:\n";
//    std::cerr<<decls_out.print();
//    std::cerr<<"\n\n";

    return decls_out;
}

Hs::Binds typechecker_state::infer_type_for_default_methods(const Hs::Decls& decls)
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

string get_name_for_typecon(const Hs::TypeCon& tycon)
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

pair<Hs::Var, Hs::Type>
typechecker_state::infer_type_for_instance1(const Hs::InstanceDecl& inst_decl)
{
    // -- old -- //
    auto [class_head, class_args] = Hs::decompose_type_apps(inst_decl.constraint);

    // Premise #1: Look up the info for the class
    optional<ClassInfo> class_info;
    if (auto tc = class_head.to<Hs::TypeCon>())
    {
        // Check that this is a class, and not a data or type?
        auto class_name = unloc(tc->name);
        if (not class_env().count(class_name))
            throw myexception()<<"In instance '"<<inst_decl.constraint<<"': no class '"<<class_name<<"'!";
        class_info = class_env().at(class_name);
    }
    else
        throw myexception()<<"In instance for '"<<inst_decl.constraint<<"': "<<class_head<<" is not a class!";


    // Premise #2: Find the type vars mentioned in the constraint.
    string tycon_names;
    set<Hs::TypeVar> type_vars;
    // Premise #4: the class_arg must be a type constructor applied to simple, distinct type variables.
    vector<Hs::TypeCon> types;
    for(auto& class_arg: class_args)
    {
        auto [a_head, a_args] = Hs::decompose_type_apps(class_arg);
        auto tc = a_head.to<Hs::TypeCon>();
        if (not tc)
            throw myexception()<<"In instance for '"<<inst_decl.constraint<<"': "<<a_head<<" is not a type constructor!";

        tycon_names += get_name_for_typecon(*tc);

        types.push_back(*tc);

        // Add distinct type variables
        for(auto& a_arg: a_args)
        {
            auto tv = a_arg.to<Hs::TypeVar>();

            if (not tv)
                throw myexception()<<"In instance for '"<<inst_decl.constraint<<"' for type '"<<class_arg<<"': "<<a_arg<<" is not a type variable!";

            if (type_vars.count(*tv))
                throw myexception()<<"Type variable '"<<tv->print()<<"' occurs twice in constraint '"<<inst_decl.constraint<<"'";

            type_vars.insert(*tv);
        }
    }

    // Premise 5: Check that the context contains no variables not mentioned in `class_arg`
    for(auto& tv: free_type_variables(inst_decl.context))
    {
        if (not type_vars.count(tv))
            throw myexception()<<"Constraint context '"<<inst_decl.context.print()<<"' contains type variable '"<<tv.print()<<"' that is not mentioned in '"<<inst_decl.constraint<<"'";
    }

    string dfun_name = "d"+get_unqualified_name(class_info->name)+tycon_names;
    
    auto dfun = get_fresh_Var(dfun_name, true);

    //  -- new -- //
    Hs::Type inst_type = Hs::add_constraints(inst_decl.context, inst_decl.constraint);
    inst_type = check_constraint( inst_type );
    return {dfun, inst_type};
}


// See Tc/TyCl/Instance.hs
// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
vector<pair<Hs::Var,Hs::InstanceDecl>>
typechecker_state::infer_type_for_instances1(const Hs::Decls& decls)
{
    global_instance_env gie_inst;

    vector<pair<Hs::Var, Hs::InstanceDecl>> named_instances;

    for(auto& decl: decls)
    {
        if (auto I = decl.to<Hs::InstanceDecl>())
        {
            auto [dfun, inst_type] = infer_type_for_instance1(*I);

            named_instances.push_back({dfun, *I});
            gie_inst = gie_inst.insert({unloc(dfun.name), inst_type});
        }
    }

//    std::cerr<<"GIE:\n";
//    for(auto& [method,type]: gie_inst)
//    {
//        std::cerr<<method<<" :: "<<type.print()<<"\n";
//    }
//    std::cerr<<"\n";

    instance_env() += gie_inst;

    return named_instances;
}

string get_class_for_constraint(const Hs::Type& constraint)
{
    auto [class_head, args] = Hs::decompose_type_apps(constraint);
    auto tc = class_head.to<Hs::TypeCon>();
    assert(tc);
    return unloc(tc->name);
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

map<string, Hs::Match> get_instance_methods(const Hs::Binds& binds, const global_value_env& members, const string& class_name)
{
    std::map<string,Hs::Match> method_matches;
    for(auto& decls: binds)
    {
        for(auto& decl: decls)
        {
            auto& fd = decl.as_<Hs::FunDecl>();
            string method_name = unloc(fd.v.name);

            if (not members.count(method_name))
                throw myexception()<<"'"<<method_name<<"' is not a member of class '"<<class_name<<"'";

            if (method_matches.count(method_name))
                throw myexception()<<"method '"<<method_name<<"' defined twice!";

            method_matches.insert({method_name, fd.match});
        }
    }

    return method_matches;
}

Hs::Decls
typechecker_state::infer_type_for_instance2(const Hs::Var& dfun, const Hs::InstanceDecl& inst_decl)
{
    // 1. Get instance head and constraints 

    // This could be Num Int or forall a b.(Ord a, Ord b) => Ord (a,b)
    auto inst_type = instance_env().at(unloc(dfun.name));
    // Instantiate it with rigid type variables.
    auto [instance_tvs, givens, instance_head] = skolemize(inst_type, false);
    auto [instance_class, instance_args] = Hs::decompose_type_apps(instance_head);

    // 2. Get the class info
    auto class_name = get_class_for_constraint(instance_head);
    auto class_info = class_env().at(class_name);

    // 3. Get constrained version of the class
    substitution_t subst;
    for(int i=0; i<class_info.type_vars.size(); i++)
        subst = subst.insert({class_info.type_vars[i], instance_args[i]});

    // 4. Get (constrained) superclass constraints
    auto superclass_constraints = class_info.context.constraints;
    for(auto& superclass_constraint: superclass_constraints)
        superclass_constraint = apply_subst(subst, superclass_constraint);

    // 5. Construct binds_super
    auto wanteds = constraints_to_lie(superclass_constraints);
    auto [binds_super, _, failed_constraints] = entails(givens, wanteds);
    if (not failed_constraints.empty())
        throw myexception()<<"Can't derive superclass constraints "<<print(failed_constraints)<<" from instance constraints "<<print(givens)<<"!";

    // 7. make some intermediates
    auto instance_constraint_dvars = vars_from_lie(givens);

    vector<Hs::Pattern> lambda_vars;
    for(auto dv: instance_constraint_dvars)
        lambda_vars.push_back(dv);

    vector<Hs::Expression> dict_entries;
    for(auto& [var,constraint]: wanteds)
        dict_entries.push_back(var);

    // 7. Construct binds_methods
    Hs::Decls decls;

    map<string, Hs::Match> method_matches;
    if (inst_decl.binds)
        method_matches = get_instance_methods( unloc( *inst_decl.binds ), class_info.members, class_name );

    string classdict_name = "d" + get_class_name_from_constraint(instance_head);

    // OK, so lets say that we just do \idvar1 .. idvarn -> let ev_binds = entails( )
    for(const auto& [method_name, method_type]: class_info.members)
    {
        auto op = get_fresh_Var("i"+method_name, true);

        dict_entries.push_back( apply_expression(op, lambda_vars) );

        // forall b. Ix b => a -> b -> b
        Hs::Type op_type = Hs::remove_top_gen(method_type);
        // forall b. Ix b => [x] -> b -> b
        op_type = apply_subst(subst, op_type);
        // forall x. (C1 x, C2 x) => forall b. Ix b => [x] -> b -> b
        op_type = Hs::add_forall_vars(instance_tvs,Hs::add_constraints(constraints_from_lie(givens), op_type));

        gve = gve.insert( {unloc(op.name), op_type} );

        optional<Hs::FunDecl> FD;
        if (auto it = method_matches.find(method_name); it != method_matches.end())
        {
            FD = Hs::FunDecl(op, it->second);
        }
        else
        {
            if (not class_info.default_methods.count(method_name))
                throw myexception()<<"instance "<<inst_decl.constraint<<" is missing method '"<<method_name<<"'";

            auto dm_var = class_info.default_methods.at(method_name);

            FD = simple_decl(op, dm_var);
        }
        auto [decl2, _, __] = infer_type_for_single_fundecl_with_sig(*FD);
        decls.push_back(decl2);
    }

    // dfun = /\a1..an -> \dicts:theta -> let binds_super in let_binds_methods in <superdict_vars,method_vars>
    expression_ref dict = Hs::tuple(dict_entries);

    if (binds_super.size())
        dict = Hs::LetExp( {noloc,binds_super}, {noloc, dict} );

    decls.push_back ({ simple_fun_decl(dfun, lambda_vars, dict) });
    return decls;
}

// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
Hs::Binds typechecker_state::infer_type_for_instances2(const vector<pair<Hs::Var, Hs::InstanceDecl>>& named_instances)
{
    Hs::Binds instance_decls;

    for(auto& [dfun, instance_decl]: named_instances)
    {
        try
        {
            auto decls = infer_type_for_instance2(dfun, instance_decl);

            instance_decls.push_back(decls);
        }
        catch (myexception& e)
        {
            string header = "In instance '" + instance_decl.constraint.print() + "' ";
            if (instance_decl.binds and instance_decl.binds->loc)
                header += " at " + convertToString(*instance_decl.binds->loc);
            header += ":\n";
            e.prepend(header);
            throw;
        }
    }
//    std::cerr<<"\nInstance ops and dfuns:\n";
//    std::cerr<<instance_decls.print();
//    std::cerr<<"\n\n";

    return instance_decls;
}

