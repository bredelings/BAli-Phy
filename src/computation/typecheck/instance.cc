#include "typecheck.H"
#include "kindcheck.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;

pair<Hs::Var, Hs::Type>
typechecker_state::infer_type_for_instance1(const Hs::InstanceDecl& inst_decl, const class_env& ce)
{
    // -- old -- //
    auto [class_head, class_args] = decompose_type_apps(inst_decl.constraint);

    // Premise #1: Look up the info for the class
    optional<class_info> cinfo;
    if (auto tc = class_head.to<Hs::TypeCon>())
    {
        // Check that this is a class, and not a data or type?
        auto class_name = unloc(tc->name);
        if (not ce.count(class_name))
            throw myexception()<<"In instance '"<<inst_decl.constraint<<"': no class '"<<class_name<<"'!";
        cinfo = ce.at(class_name);
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
pair<global_instance_env,vector<pair<Hs::Var,Hs::InstanceDecl>>>
typechecker_state::infer_type_for_instances1(const Hs::Decls& decls, const class_env& ce)
{
    global_instance_env gie_inst;

    vector<pair<Hs::Var, Hs::InstanceDecl>> named_instances;

    for(auto& decl: decls)
    {
        if (auto I = decl.to<Hs::InstanceDecl>())
        {
            auto [dfun, inst_type] = infer_type_for_instance1(*I, ce);

            named_instances.push_back({dfun, *I});
            gie_inst = gie_inst.insert({unloc(dfun.name), inst_type});
        }
    }
    return {gie_inst, named_instances};
}

Hs::Decls
typechecker_state::infer_type_for_instance2(const Hs::Var& dfun, const Hs::InstanceDecl& inst_decl, const class_env& ce)
{

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

    auto [class_head, monotypes] = decompose_type_apps(inst_decl.constraint);

    // 1. Look up the info for the main class
    auto tc = class_head.to<Hs::TypeCon>();
    assert(tc); // already checked.
    string class_name = unloc(tc->name);
    class_info cinfo = ce.at(class_name);

    // 2. build a local instance environment from the instance constraints
    auto ordered_lie_instance = constraints_to_lie(inst_decl.context.constraints);
    auto lie_instance = unordered_lie(ordered_lie_instance);

    // 3. Construct binds_super
    auto ordered_lie_super = constraints_to_lie(cinfo.context.constraints);
    auto lie_super = unordered_lie(ordered_lie_super);
    auto binds_super = entails(lie_instance, lie_super);
    if (not binds_super)
        throw myexception()<<"Can't derive "<<print(lie_super)<<" from "<<print(lie_instance)<<"!";

    // 5. make some intermediates
    auto instance_constraint_dvars = vars_from_lie(ordered_lie_instance);
    vector<Hs::Pattern> lambda_vars;
    vector<Hs::Expression> dict_entries;
    for(auto dv: instance_constraint_dvars)
    {
        lambda_vars.push_back(dv);
        dict_entries.push_back(dv);
    }


    // 4. Construct binds_methods
    Hs::Binds binds_methods;
    std::map<string,Hs::Match> method_matches;
    for(auto& decl: unloc(*inst_decl.binds)[0])
    {
        auto& fd = decl.as_<Hs::FunDecl>();
        string name = unloc(fd.v.name);
        if (not cinfo.members.count(name))
            throw myexception()<<"'"<<name<<"' is not a member of class '"<<class_name<<"'";
        if (method_matches.count(name))
            throw myexception()<<"method '"<<name<<"' defined twice!";
        method_matches.insert({name,fd.match});
    }

    for(int i=(int)cinfo.fields.size()-(int)cinfo.members.size();i<cinfo.fields.size();i++)
    {
        auto& [name,type] = cinfo.fields[i];

        auto it = method_matches.find(name);
        if (it == method_matches.end())
        {
            if (cinfo.default_methods.count(name))
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

    Hs::Decls decls ({ simple_decl(dfun,E) });
    return decls;
}

// We need to handle the instance decls in a mutually recursive way.
// And we may need to do instance decls once, then do value decls, then do instance decls a second time to generate the dfun bodies.
Hs::Decls
typechecker_state::infer_type_for_instances2(const vector<pair<Hs::Var, Hs::InstanceDecl>>& named_instances, const class_env& ce)
{
    Hs::Decls out_decls;
    global_instance_env gie_inst;
    for(auto& [dfun, instance_decl]: named_instances)
    {
        auto decls_ = infer_type_for_instance2(dfun, instance_decl, ce);

        for(auto& d: decls_)
            out_decls.push_back(d);
    }
    return out_decls;
}

