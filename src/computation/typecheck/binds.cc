#include "typecheck.H"
#include "kindcheck.H"

#include "parser/rename.H" // for get_indices_for_names( )

#include "util/set.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;


template <typename T>
Hs::Type quantify(const T& tvs, const Hs::Type& monotype)
{
    if (tvs.empty())
        return monotype;
    else
    {
        for(auto& tv: tvs)
            assert(tv.kind);
        return Hs::ForallType(tvs | ranges::to<vector>, monotype);
    }
}

template <typename T>
value_env quantify(const T& tvs, const value_env& env1)
{
    value_env env2;
    for(auto& [name, monotype]: env1)
        env2 = env2.insert( {name, quantify(tvs, monotype)} );
    return env2;
}

global_value_env sig_env(const map<string, Hs::Type>& signatures)
{
    global_value_env sig_env;
    for(auto& [name, type]: signatures)
        sig_env = sig_env.insert({name, type});
    return sig_env;
}

tuple<Hs::Binds, global_value_env>
typechecker_state::infer_type_for_binds(const global_value_env& env, Hs::Binds binds)
{
    kindchecker_state K(tce);
    for(auto& [name,type]: binds.signatures)
        type = K.kind_and_type_check_type(type);
    auto env2 = plus_prefer_right(env, sig_env(binds.signatures));

    global_value_env binders;
    for(auto& decls: binds)
    {
        auto [decls1, binders1] = infer_type_for_decls(env2, binds.signatures, decls);
        decls = decls1;
        // We could remove the binders with sigs
        env2 = plus_prefer_right(env2, binders1);
        binders += binders1;
    }

    return {binds, binders};
}

value_env remove_sig_binders(value_env binder_env, const signature_env& signatures)
{
    auto no_sig_binder_env = binder_env;
    for(auto& [name,_]: binder_env)
        if (signatures.count(name))
            no_sig_binder_env = no_sig_binder_env.erase(name);
    return no_sig_binder_env;
}

vector<Hs::Decls> split_decls_by_signatures(const Hs::Decls& decls, const map<string, Hs::Type>& signatures)
{
    // 1. Map names to indices
    map<string,int> index_for_name = get_indices_for_names(decls);

    // 2. Figure out which indices reference each other
    vector<vector<int>> referenced_decls;
    for(int i=0;i<decls.size();i++)
    {
        vector<int> refs;
        for(auto& name: get_rhs_free_vars(decls[i]))
        {
            auto it = index_for_name.find(name);

            // Skip if this name isn't one of the ids being defined.
            if (it == index_for_name.end()) continue;

            // Skip if this name has a signature
            if (signatures.count(name)) continue;

            refs.push_back(it->second);
        }
        referenced_decls.push_back( std::move(refs) );
    }

    // 3. Compute strongly-connected components and split
    return split_decls(decls, referenced_decls);
}

tuple<Hs::Decls, global_value_env>
typechecker_state::infer_type_for_decls(const global_value_env& env, const signature_env& signatures, const Hs::Decls& decls)
{
    // The signatures for the binders should already be in the environment.

    auto bind_groups = split_decls_by_signatures(decls, signatures);

    auto env2 = env;
    Hs::Decls decls2;
    local_value_env binders;
    for(auto& group: bind_groups)
    {
        auto [group_decls, group_binders] = infer_type_for_decls_groups(env2, signatures, group);

        for(auto& decl: group_decls)
            decls2.push_back(decl);

        binders += group_binders;

        env2 += remove_sig_binders(group_binders, signatures);
    }
    return {decls2, binders};
}

bool single_fundecl_with_sig(const Hs::Decls& decls, const signature_env& signatures)
{
    if (decls.size() != 1) return false;

    auto& decl = decls[0];

    if (not decl.is_a<Hs::FunDecl>()) return false;

    auto& FD = decl.as_<Hs::FunDecl>();

    auto& name = unloc(FD.v.name);

    return signatures.count(name) > 0;
}

vector<Hs::Type> constraints_from_lie(const local_instance_env& lie)
{
    vector<Hs::Type> constraints;
    for(auto& [_, constraint]: lie)
        constraints.push_back(constraint);
    return constraints;
}

vector<Hs::Var> vars_from_lie(const local_instance_env& lie)
{
    vector<Hs::Var> dict_vars;
    for(auto& [name, constraint]: lie)
    {
        Hs::Var dict_var({noloc,name});
        dict_var.type = constraint;
        dict_vars.push_back( dict_var );
    }
    return dict_vars;
}

vector<Hs::Var> vars_from_lie(const vector<pair<Hs::Var, Hs::Type>>& lie)
{
    vector<Hs::Var> vars;
    for(auto& [var, constraint]: lie)
        vars.push_back( var );
    return vars;
}

tuple<expression_ref, string, Hs::Type>
typechecker_state::infer_type_for_single_fundecl_with_sig(const global_value_env& env, Hs::FunDecl FD)
{
    auto& name = unloc(FD.v.name);

    auto sig_type = env.at(name);

    // OK, so what we want to do is:

    // 1. instantiate the type -> (tvs, givens, rho-type)
    auto [tvs, given_constraints, given_type] = instantiate(sig_type, false);
    auto ordered_lie_given = constraints_to_lie(given_constraints);
    auto lie_given = unordered_lie(ordered_lie_given);
    
    // 2. typecheck the rhs -> (rhs_type, wanted, body)
    push_lie();
    auto [decl2, most_general_type] = infer_rhs_type(env, FD);
    auto lie_wanted = pop_lie();

    // 3. alpha[i] in most_general_type but not in env
    auto ftv_mgt = free_type_variables(most_general_type) - free_type_variables(env);
    // FIXME -- what if the instantiated type contains variables that are free in the environment?

    // 4. match(given_type <= most_general_type)
    unify(most_general_type, given_type);

    // 5. check if the given => wanted ~ EvBinds
    lie_wanted = apply_current_subst( lie_wanted );
    auto evbinds = entails(lie_given, lie_wanted);
    if (not evbinds)
        throw myexception()<<"Can't derive constraints '"<<print(lie_wanted)<<"' from specified constraints '"<<print(lie_given)<<"'";

    // 6. return GenBind with tvs, givens, body
    auto dict_vars = vars_from_lie( ordered_lie_given );

    Hs::Decls decls;
    decls.push_back(decl2);

    map<string,Hs::BindInfo> bind_infos;
    Hs::BindInfo bind_info;
    bind_info.dict_args = dict_vars;
    bind_info.monotype = given_type;
    bind_infos.insert({name,bind_info});

    auto decl = Hs::GenBind( tvs, dict_vars, *evbinds, decls, bind_infos );
    return {decl, name, sig_type};
}

bool is_restricted(const map<string, Hs::Type>& signatures, const Hs::Decls& decls)
{
    for(auto& decl: decls)
    {
        if (decl.is_a<Hs::PatDecl>())
            return true;
        else if (auto fd = decl.to<Hs::FunDecl>())
        {
            // Simple pattern declaration
            if (fd->match.rules[0].patterns.size() == 0)
            {
                auto& name = unloc(fd->v.name);
                if (not signatures.count(name)) return true;
            }
        }
    }
    return false;
};

// Why aren't we using `fixed_type_vars`?
// I guess the deferred constraints that do not mention fixed_type_vars are ambiguous?
pair<local_instance_env, local_instance_env>
classify_constraints(const local_instance_env& lie,
                     const set<Hs::TypeVar>& fixed_type_vars)
{
    local_instance_env lie_deferred;
    local_instance_env lie_retained;

    for(auto& [name, constraint]: lie)
    {
        auto constraint_type_vars = free_type_variables(constraint);

        // Does the constraint contain any ambiguous vars?
        bool all_fixed = true;
        for(auto& type_var: constraint_type_vars)
            if (not fixed_type_vars.count(type_var))
                all_fixed = false;

        if (all_fixed)
            lie_deferred = lie_deferred.insert({name,constraint});
        else
            lie_retained = lie_retained.insert({name,constraint});
    }
    return {lie_deferred, lie_retained};
}

tuple<Hs::Var, Hs::Type, local_value_env>
typechecker_state::infer_lhs_var_type(Hs::Var v)
{
    auto& name = unloc(v.name);

    Hs::Type type = fresh_meta_type_var( kind_star() );
    v.type = type;

    // Check that this is a NEW name.
    local_value_env lve;
    lve = lve.insert({name,type});
    return {v, type, lve};
}

tuple<expression_ref, Hs::Type, local_value_env>
typechecker_state::infer_lhs_type(const expression_ref& decl, const map<string, Hs::Type>& signatures)
{
    if (auto fd = decl.to<Hs::FunDecl>())
    {
        auto FD = *fd;
        // If there was a signature, we would have called infer_type_for_single_fundecl_with_sig
        assert(not signatures.count(unloc(FD.v.name)));

        auto [v2, type, lve] = infer_lhs_var_type(FD.v);
        FD.v.type = type;
        return {FD, type, lve};
    }
    else if (auto pd = decl.to<Hs::PatDecl>())
    {
        auto PD = *pd;
        auto [lhs, type, lve] = infer_pattern_type(PD.lhs, signatures);
        PD.lhs = lhs;
        return {PD, type, lve};
    }
    else
        std::abort();
}

tuple<expression_ref, Hs::Type>
typechecker_state::infer_rhs_type(const global_value_env& env, const expression_ref& decl)
{
    if (auto fd = decl.to<Hs::FunDecl>())
    {
        auto FD = *fd;
        auto [match, rhs_type] = infer_type(env, FD.match);
        FD.match = match;

        return {FD, rhs_type};
    }
    else if (auto pd = decl.to<Hs::PatDecl>())
    {
        auto PD = *pd;
        auto [rhs, rhs_type] = infer_type(env, PD.rhs);
        PD.rhs = rhs;

        return {PD, rhs_type};
    }
    else
        std::abort();
}

tuple<Hs::Decls, global_value_env>
typechecker_state::infer_type_for_decls_groups(const global_value_env& env, const map<string, Hs::Type>& signatures, Hs::Decls decls)
{
    if (single_fundecl_with_sig(decls, signatures))
    {
        auto& FD = decls[0].as_<Hs::FunDecl>();

        auto [decl, name, sig_type] = infer_type_for_single_fundecl_with_sig(env, FD);

        Hs::Decls decls({decl});

        global_value_env binders;;
        binders = binders.insert({name, sig_type});
        return {decls, binders};
    }

// How & when do we complain if there are predicates on signatures with the monomorphism restriction?

    push_lie();

    // 1. Add each let-binder to the environment with a fresh type variable
    value_env binder_env;

    vector<Hs::Type> lhs_types;
    for(int i=0;i<decls.size();i++)
    {
        auto [decl, type, lve] = infer_lhs_type( decls[i], signatures );
        decls[i] = decl;

        binder_env += lve;
        lhs_types.push_back(type);
    }

    auto env2 = env + remove_sig_binders(binder_env, signatures);

    // 2. Infer the types of each of the x[i]
    for(int i=0;i<decls.size();i++)
    {
        auto [decl, rhs_type] = infer_rhs_type(env2, decls[i]);
        decls[i] = decl;

        unify(lhs_types[i], rhs_type);
    }

    // We need to substitute before looking for free type variables!
    // We also need to substitute before we quantify below.
    binder_env = apply_current_subst(binder_env);

    auto fixed_tvs = free_type_variables(env);
    set<Hs::TypeVar> tvs_in_any_type;  // type variables in ANY of the definitions
    set<Hs::TypeVar> tvs_in_all_types;  // type variables in ALL of the definitions
    {
        // FIXME - should we be looping over binder vars, or over definitions?
        optional<set<Hs::TypeVar>> tvs_in_all_types_;
        for(auto& [_, type]: binder_env)
        {
            auto tvs = free_type_variables(type);
            add(tvs_in_any_type, tvs);
            if (tvs_in_all_types_)
                tvs_in_all_types_ = intersection(*tvs_in_all_types_, tvs);
            else
                tvs_in_all_types_ = tvs;
        }
        assert(tvs_in_all_types_);
        tvs_in_all_types = *tvs_in_all_types_;
    }

    // OK, we've got to do defaulting before we consider what variables to quantify over.

    vector< Hs::Var > dict_vars;

    // A. First, REDUCE the lie by
    //    (i)  converting to Hnf
    //     -- when do we do this?  Always?
    //    (ii) representing some constraints in terms of others.
    // This also substitutes into the current LIE, which we need to do 
    //    before finding free type vars in the LIE below.
    Hs::Binds binds = reduce_current_lie();

    // B. Second, extract the "retained" predicates can be added without causing abiguity.
    auto [lie_deferred, lie_retained] = classify_constraints( current_lie(), fixed_tvs );

    /* NOTE: Constraints can reference variables that are in
     *        (i) ALL types in a recursive group
     *       (ii) SOME-BUT-NOT-ALL types
     *      (iii) NO types.
     *
     * For unrestricted bindings, classes (ii) and (iii) need defaults.
     * For restricted bindings, only class (iii) (I think) needs defaults.
     */


    // FIXME: return {dvar = expression} as a mapping, instead of a sequence of binds?
    // If we want to substitute an expression for an argument in the wrapper,
    
    // For the COMPLETELY ambiguous constraints, we should be able to just discard the constraints,
    //   after generating definitions of their 
    auto [s1, binds1, lie_not_completely_ambiguous] = default_preds( fixed_tvs, tvs_in_any_type, lie_retained );
    binds = binds1 + binds;

    set<Hs::TypeVar> qtvs = tvs_in_any_type - fixed_tvs;

    map<string, Hs::BindInfo> bind_infos;

    if (is_restricted(signatures, decls))
    {
        // 1. This removes defaulted constraints from the LIE.  (not_completely_ambiguous = retained but not defaulted)
        current_lie() = lie_deferred + lie_not_completely_ambiguous;

        // NOTE: in theory, we should be able to subtract just ftvs(lie_completely_ambiguous)
        //       since lie_deferred should contain only fixed type variables that have already been
        //       removed from qtvs.

        // 2. Quantify only over variables that are "unconstrained" (not part of the LIE)
        // -- after defaulting!
        qtvs = qtvs - free_type_variables(current_lie());

        global_value_env binder_env2;
        for(auto& [name,type]: binder_env)
        {
            Hs::BindInfo info;
            info.monotype = type;
            bind_infos.insert({name,info});

            auto qtvs_in_this_type = intersection(qtvs, free_type_variables(type));

            // 3. Quantify type
            // FIXME: We also need to add type lambdas for the tuple, and wrappers for each binder...
            auto type2 = quantify(qtvs_in_this_type, type);
            binder_env2 = binder_env2.insert({name, type2});
        }
        binder_env = binder_env2;
    }
    else
    {
        // For the SOMEWHAT ambiguous constraints, we don't need the defaults to define the recursive group,
        // but we do need the defaults to define individual symbols.

        // 1. Quantify over variables in ANY type that are not fixed -- doesn't depend on defaulting.
        // Never quantify over variables that are only in a LIE -- those must be defaulted.

        // 2. Only the constraints with all fixed tvs are going to be visible outside this declaration group.
        current_lie() = lie_deferred;

        dict_vars = vars_from_lie( lie_not_completely_ambiguous );

        global_value_env binder_env2;
        for(auto& [name,type]: binder_env)
        {
            auto tvs_in_this_type = free_type_variables(type);

            // Default any constraints that do not occur in THIS type.
            auto [s2, binds2, lie_for_this_type] = default_preds( fixed_tvs, tvs_in_this_type, lie_not_completely_ambiguous );

            auto constraints_for_this_type = constraints_from_lie(lie_for_this_type);

            // Only quantify over type variables that occur in THIS type.
            Hs::Type qualified_type = quantify( tvs_in_this_type,
                                                Hs::add_constraints( constraints_for_this_type,
                                                                     type ) );

            // How can we generate a wrapper between qualified_type and lie_deferred => (type1, unrestricted_type, type3)?

            binder_env2 = binder_env2.insert( {name, qualified_type} );

            Hs::BindInfo info;
            info.monotype = type;
            info.binds = binds2;
            for(auto& [name, constraint]: lie_for_this_type)
                info.dict_args.push_back( Hs::Var({noloc,name}) );

            bind_infos.insert({name, info});
        }
        binder_env = binder_env2;
        assert(bind_infos.size() >= 1);
    }

    Hs::Decls decls2 = decls;
    Hs::GenBind gen_bind( qtvs | ranges::to<vector>, dict_vars, binds, decls, bind_infos );
    decls2.push_back( gen_bind );

    pop_and_add_lie();

    return {decls2, binder_env};
}

