#include "typecheck.H"
#include "kindcheck.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;

Hs::Decls typechecker_state::infer_type_for_default_methods(const Hs::ClassDecl& C)
{
    Hs::Decls decls_out;

    auto class_info = class_env.at(C.name);
    for(auto& decls: unloc(*C.binds))
        for(auto& decl: decls)
        {
            auto FD = decl.as_<Hs::FunDecl>();
            auto method_name = unloc(FD.v.name);
            auto dm = class_info.default_methods.at(method_name);
            FD.v = dm;

            auto [decl2, name, sig_type] = infer_type_for_single_fundecl_with_sig(gve, FD);
            decls_out.push_back(decl2);
        }
    return decls_out;
}

Hs::Binds typechecker_state::infer_type_for_default_methods(const Hs::Decls& decls)
{
    Hs::Binds binds;

    for(auto& decl: decls)
    {
        auto c = decl.to<Hs::ClassDecl>();
        if (not c) continue;

        binds.push_back( infer_type_for_default_methods(*c) );
    }
    return binds;
}

pair<Hs::Var, Hs::Type>
typechecker_state::infer_type_for_instance1(const Hs::InstanceDecl& inst_decl)
{
    // -- old -- //
    auto [class_head, class_args] = decompose_type_apps(inst_decl.constraint);

    // Premise #1: Look up the info for the class
    optional<ClassInfo> class_info;
    if (auto tc = class_head.to<Hs::TypeCon>())
    {
        // Check that this is a class, and not a data or type?
        auto class_name = unloc(tc->name);
        if (not class_env.count(class_name))
            throw myexception()<<"In instance '"<<inst_decl.constraint<<"': no class '"<<class_name<<"'!";
        class_info = class_env.at(class_name);
    }
    else
        throw myexception()<<"In instance for '"<<inst_decl.constraint<<"': "<<class_head<<" is not a class!";


    // Premise #2: Find the type vars mentioned in the constraint.
    set<Hs::TypeVar> type_vars;
    // Premise #4: the class_arg must be a type constructor applied to simple, distinct type variables.
    vector<Hs::TypeCon> types;
    for(auto& class_arg: class_args)
    {
        auto [a_head, a_args] = decompose_type_apps(class_arg);
        auto tc = a_head.to<Hs::TypeCon>();
        if (not tc)
            throw myexception()<<"In instance for '"<<inst_decl.constraint<<"': "<<a_head<<" is not a type constructor!";

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

    auto dfun = fresh_var("dfun", true);

    //  -- new -- //
    kindchecker_state K(tce);
    Hs::Type inst_type = Hs::add_constraints(inst_decl.context, inst_decl.constraint);
    inst_type = K.kind_and_type_check_constraint(inst_type);
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

    gie += gie_inst;

    return named_instances;
}

string get_class_for_constraint(const Hs::Type& constraint)
{
    auto [class_head, args] = decompose_type_apps(constraint);
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

map<string, Hs::Match> get_instance_methods(const Hs::Decls& decls, const global_value_env& members, const string& class_name)
{
    std::map<string,Hs::Match> method_matches;
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

    return method_matches;
}


Hs::Decls
typechecker_state::infer_type_for_instance2(const Hs::Var& dfun, const Hs::InstanceDecl& inst_decl)
{
    // 1. Get instance head and constraints 

    // This could be Num Int or forall a b.(Ord a, Ord b) => Ord (a,b)
    auto inst_type = gie.at(unloc(dfun.name));
    // Instantiate it with rigid type variables.
    auto [instance_tvs, instance_constraints, instance_head] = instantiate(inst_type, false);

    // 2. Get the class info
    auto class_name = get_class_for_constraint(instance_head);
    ClassInfo class_info = class_env.at(class_name);
    Hs::Type class_type = Hs::TypeCon(Unlocated(class_name));
    for(auto& tv: class_info.type_vars)
        class_type = Hs::TypeApp(class_type, tv);
    class_type = Hs::add_forall_vars(class_info.type_vars,Hs::add_constraints(class_info.context, class_type));
    auto [class_tvs, superclass_constraints, class_head] = instantiate(class_type, true);

    // 3. Match the class head, and substitute into the superclass constraints
    auto s = ::maybe_match(class_head, instance_head);
    assert(s);
    for(auto& superclass_constraint: superclass_constraints)
        superclass_constraint = apply_subst(*s, superclass_constraint);

    // 4. build a local instance environment from the instance constraints
    auto ordered_lie_instance = constraints_to_lie(instance_constraints);
    auto lie_instance = unordered_lie(ordered_lie_instance);

    // 5. Construct binds_super
    auto ordered_lie_super = constraints_to_lie(superclass_constraints);
    auto lie_super = unordered_lie(ordered_lie_super);
    auto binds_super = entails(lie_instance, lie_super);
    if (not binds_super)
        throw myexception()<<"Can't derive "<<print(lie_super)<<" from "<<print(lie_instance)<<"!";

    // 6. make some intermediates
    auto instance_constraint_dvars = vars_from_lie(ordered_lie_instance);
    vector<Hs::Pattern> lambda_vars;
    vector<Hs::Expression> dict_entries;
    for(auto dv: instance_constraint_dvars)
    {
        lambda_vars.push_back(dv);
        dict_entries.push_back(dv);
    }

    if (not inst_decl.binds)
        std::cerr<<"Instance for "<<inst_decl.constraint<<" has no methods!\n";
    return {};

    // 7. Construct binds_methods
    Hs::Decls decls;

    Hs::Binds binds_methods;
    auto method_matches = get_instance_methods( unloc( *inst_decl.binds )[0], class_info.members, class_name );

    // OK, so lets say that we just do \idvar1 .. idvarn -> let ev_binds = entails( )
    for(auto& [name,type]: class_info.members)
    {
        auto it = method_matches.find(name);
        if (it == method_matches.end())
        {
            if (class_info.default_methods.count(name))
            {
                // handle default method.
            }
            else
                throw myexception()<<"instance "<<inst_decl.constraint<<" is missing method '"<<name<<"'";
        }
        else
        {
            auto method = fresh_var(name,false);
            dict_entries.push_back(method);
        
            Hs::Decls decls;
            decls.push_back(Hs::FunDecl(method,it->second));
            binds_methods.push_back(decls);
        }
    }

    // dfun = /\a1..an -> \dicts:theta -> let binds_super in let_binds_methods in <superdict_vars,method_vars>
    expression_ref dict = Hs::tuple(dict_entries);

    expression_ref E = Hs::LetExp( {noloc, binds_methods}, {noloc, dict} );

    if (binds_super->size())
        E = Hs::LetExp( {noloc,*binds_super}, {noloc,E} );

    if (not lambda_vars.empty())
        E = Hs::LambdaExp( lambda_vars, E);

    decls.push_back ({ simple_decl(dfun,E) });
    return decls;
}

// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
Hs::Decls
typechecker_state::infer_type_for_instances2(const vector<pair<Hs::Var, Hs::InstanceDecl>>& named_instances)
{
    Hs::Decls out_decls;
    global_instance_env gie_inst;
    for(auto& [dfun, instance_decl]: named_instances)
    {
        auto decls_ = infer_type_for_instance2(dfun, instance_decl);

        for(auto& d: decls_)
            out_decls.push_back(d);
    }
    return out_decls;
}

