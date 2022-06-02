#include "typecheck.H"
#include "kindcheck.H"

#include "rename/rename.H" // for get_indices_for_names( )

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

global_value_env sig_env(const map<string, Hs::Type>& signatures)
{
    global_value_env sig_env;
    for(auto& [name, type]: signatures)
        sig_env = sig_env.insert({name, type});
    return sig_env;
}

Hs::Binds typechecker_state::infer_type_for_binds_top(const Hs::Binds& binds)
{
    auto [type_checked_binds, env2] = infer_type_for_binds(gve, binds, true);
    gve = env2;

    return type_checked_binds;
}

void typechecker_state::infer_type_for_foreign_imports(vector<Hs::ForeignDecl>& foreign_decls)
{
    global_value_env fte;
    kindchecker_state K( tycon_info() );
    for(auto& f: foreign_decls)
    {
        f.type = K.kind_and_type_check_type( f.type );
        fte = fte.insert({f.function_name, f.type});
    }
    gve += fte;
}

global_value_env typechecker_state::infer_type_for_sigs(signature_env& signatures) const
{
    kindchecker_state K( tycon_info() );
    for(auto& [name,type]: signatures)
        type = K.kind_and_type_check_type(type);

    return sig_env(signatures);
}


tuple<Hs::Binds, global_value_env>
typechecker_state::infer_type_for_binds(const global_value_env& env, Hs::Binds binds, bool is_top_level)
{
    auto env2 = plus_prefer_right(env, infer_type_for_sigs(binds.signatures));

    for(auto& decls: binds)
    {
        auto [decls1, env3] = infer_type_for_decls(env2, binds.signatures, decls, is_top_level);
        decls = decls1;
        env2 = env3;
    }

    return {binds, env2};
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
typechecker_state::infer_type_for_decls(const global_value_env& env, const signature_env& signatures, const Hs::Decls& decls, bool is_top_level)
{
    // The signatures for the binders should already be in the environment.

    auto bind_groups = split_decls_by_signatures(decls, signatures);

    auto env2 = env;
    Hs::Decls decls2;
    local_value_env binders;
    for(auto& group: bind_groups)
    {
        auto [group_decls, group_binders] = infer_type_for_decls_groups(env2, signatures, group, is_top_level);

        for(auto& decl: group_decls)
            decls2.push_back(decl);

        binders += group_binders;

        env2 += remove_sig_binders(group_binders, signatures);
    }
    return {decls2, env2};
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

expression_ref
rename_from_bindinfo(expression_ref decl, const map<string, Hs::BindInfo>& bind_infos)
{
    if (auto fd = decl.to<Hs::FunDecl>())
    {
        auto FD = *fd;
        FD.v = rename_var_pattern_from_bindinfo(FD.v, bind_infos);
        return FD;
    }
    else if (auto pd = decl.to<Hs::PatDecl>())
    {
        auto PD = *pd;
        unloc(PD.lhs) = rename_pattern_from_bindinfo(unloc(PD.lhs), bind_infos);
        return PD;
    }
    else
        std::abort();
}

Hs::Decls rename_from_bindinfo(Hs::Decls decls,const map<string, Hs::BindInfo>& bind_infos)
{
    for(auto& decl: decls)
        decl = rename_from_bindinfo(decl, bind_infos);
    return decls;
}

Hs::GenBind mkGenBind(const vector<Hs::TypeVar>& tvs,
                      const vector<Hs::Var>& dict_vars,
                      const Hs::Binds& binds,
                      Hs::Decls decls,
                      const map<string, Hs::BindInfo>& bind_infos)
{
    decls = rename_from_bindinfo(decls, bind_infos);
    return Hs::GenBind(tvs, dict_vars, binds, decls, bind_infos);
}

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

tuple<expression_ref, ID, Hs::Type>
typechecker_state::infer_type_for_single_fundecl_with_sig(const global_value_env& env, Hs::FunDecl FD)
{
    try
    {
        auto& name = unloc(FD.v.name);

        auto polytype = env.at(name);

        // 1. instantiate the type -> (tvs, givens, rho-type)
        auto [tvs, given_constraints, given_type] = instantiate(polytype, false);
        auto ordered_lie_given = constraints_to_lie(given_constraints);
        auto dict_vars = vars_from_lie( ordered_lie_given );
        auto lie_given = unordered_lie(ordered_lie_given);

        // 2. typecheck the rhs -> (rhs_type, wanted, body)
        auto tcs2 = copy_clear_lie();
        auto [match2, rhs_type] = tcs2.infer_type(env, FD.match);
        FD.match = match2;
        auto unreduced_collected_lie = tcs2.current_lie();

        // 3. match(given_type <= rhs_type)
        unify(rhs_type, given_type);
        Hs::Type monotype = apply_current_subst(rhs_type);

        // 4. simplify constraints
        auto [ev_binds, collected_lie] = reduce( apply_current_subst( unreduced_collected_lie) );

        // 5. find free type variables in the most general type
        auto fixed_tvs = free_type_variables( apply_current_subst(env) );
        auto free_tvs = free_type_variables(monotype) - fixed_tvs;

        // 6. figure out which constraints are relevant here
        auto [lie_deferred, lie_retained] = classify_constraints( collected_lie, fixed_tvs );
        auto [s1, binds1, lie_unambiguous_retained] = default_preds( fixed_tvs, free_tvs, lie_retained );
        ev_binds = binds1 + ev_binds;

        current_lie() += lie_deferred;

        // 7. check that the remaining constraints are satisfied by the constraints in the type signature
        auto [ev_binds2, lie_failed] = entails(lie_given, lie_unambiguous_retained);
        if (not ev_binds2)
            throw myexception()<<"Can't derive constraints '"<<print(lie_failed)<<"' from specified constraints '"<<print(lie_given)<<"'";
        ev_binds = *ev_binds2 + ev_binds;

        // 8. return GenBind with tvs, givens, body
        Hs::Var inner_id = get_fresh_Var(unloc(FD.v.name),false);

        Hs::BindInfo bind_info(FD.v, inner_id, monotype, polytype, dict_vars, {});

        auto decl = mkGenBind( tvs, dict_vars, ev_binds, Hs::Decls({FD}), {{name, bind_info}} );

        return {decl, name, polytype};
    }
    catch (myexception& e)
    {
        string header = "In function '" + FD.v.print()+"'";
        if (FD.v.name.loc)
            header += " at " + convertToString(*FD.v.name.loc);
        header += ":\n";
        e.prepend(header);
        throw;
    }
}

bool is_restricted(const map<ID, Hs::Type>& signatures, const Hs::Decls& decls)
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

tuple<expression_ref, Hs::Type, local_value_env>
typechecker_state::infer_lhs_type(const expression_ref& decl, const map<string, Hs::Type>& signatures)
{
    if (auto fd = decl.to<Hs::FunDecl>())
    {
        auto FD = *fd;
        // If there was a signature, we would have called infer_type_for_single_fundecl_with_sig
        assert(not signatures.count(unloc(FD.v.name)));

        auto [v2, type, lve] = infer_var_pattern_type(FD.v);
        FD.v = v2;
        return {FD, type, lve};
    }
    else if (auto pd = decl.to<Hs::PatDecl>())
    {
        auto PD = *pd;
        auto [lhs, type, lve] = infer_pattern_type( unloc(PD.lhs), signatures);
        unloc(PD.lhs) = lhs;
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

pair<set<Hs::TypeVar>, set<Hs::TypeVar>> tvs_in_any_all_types(const local_value_env& mono_binder_env)
{
    set<Hs::TypeVar> tvs_in_any_type;  // type variables in ANY of the definitions
    set<Hs::TypeVar> tvs_in_all_types;  // type variables in ALL of the definitions
    {
        // FIXME - should we be looping over binder vars, or over definitions?
        optional<set<Hs::TypeVar>> tvs_in_all_types_;
        for(auto& [_, type]: mono_binder_env)
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

    return {tvs_in_any_type, tvs_in_all_types};
}



tuple<Hs::Decls, global_value_env>
typechecker_state::infer_type_for_decls_groups(const global_value_env& env, const map<string, Hs::Type>& signatures, Hs::Decls decls, bool is_top_level)
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

    // 1. Add each let-binder to the environment with a fresh type variable
    local_value_env mono_binder_env;

    vector<Hs::Type> lhs_types;
    for(int i=0;i<decls.size();i++)
    {
        auto [decl, type, lve] = infer_lhs_type( decls[i], signatures );
        decls[i] = decl;

        mono_binder_env += lve;
        lhs_types.push_back(type);
    }

    auto env2 = env + remove_sig_binders(mono_binder_env, signatures);

    auto tcs2 = copy_clear_lie();
    // 2. Infer the types of each of the x[i]
    for(int i=0;i<decls.size();i++)
    {
        try{
            auto [decl, rhs_type] = tcs2.infer_rhs_type(env2, decls[i]);
            decls[i] = decl;

            tcs2.unify(lhs_types[i], rhs_type);
        }
        catch (myexception& e)
        {
            string header;
            if (auto FD = decls[i].to<Hs::FunDecl>())
            {
                header = "In function '" + FD->v.print()+"'";
                if (FD->v.name.loc)
                    header += " at " + convertToString(*FD->v.name.loc);
            }
            else if (auto PD = decls[i].to<Hs::PatDecl>())
            {
                header = "In definition of '" + unloc(PD->lhs).print() + "'";
                if (PD->lhs.loc)
                    header += " at " + convertToString(*PD->lhs.loc);
            }
            else
                std::abort();
            header += ":\n";
            e.prepend(header);
            throw;
        }
    }
    auto unreduced_collected_lie = tcs2.current_lie();

    // We need to substitute before looking for free type variables!
    // We also need to substitute before we quantify below.
    mono_binder_env = apply_current_subst(mono_binder_env);

    auto fixed_tvs = free_type_variables( apply_current_subst(env) );

    auto [tvs_in_any_type, tvs_in_all_types] = tvs_in_any_all_types(mono_binder_env);

    // OK, we've got to do defaulting before we consider what variables to quantify over.


    // A. First, REDUCE the lie by
    //    (i)  converting to Hnf
    //     -- when do we do this?  Always?
    //    (ii) representing some constraints in terms of others.
    // This also substitutes into the current LIE, which we need to do 
    //    before finding free type vars in the LIE below.
    auto [binds, collected_lie] = reduce( apply_current_subst( unreduced_collected_lie ) );

    // B. Second, extract the "retained" predicates can be added without causing abiguity.
    auto [lie_deferred, lie_retained] = classify_constraints( collected_lie, fixed_tvs );


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
    lie_retained = lie_not_completely_ambiguous;

    set<Hs::TypeVar> qtvs = tvs_in_any_type - fixed_tvs;

    map<string, Hs::BindInfo> bind_infos;

    global_value_env poly_binder_env;
    bool restricted = is_restricted(signatures, decls) and not is_top_level;
    if (restricted)
    {
        // 1. This removes defaulted constraints from the LIE.  (not_completely_ambiguous = retained but not defaulted)
        lie_deferred += lie_retained;

        lie_retained = {};

        add(fixed_tvs, free_type_variables(lie_deferred));
    }

    current_lie() += lie_deferred;

    // For the SOMEWHAT ambiguous constraints, we don't need the defaults to define the recursive group,
    // but we do need the defaults to define individual symbols.

    // 1. Quantify over variables in ANY type that are not fixed -- doesn't depend on defaulting.
    // Never quantify over variables that are only in a LIE -- those must be defaulted.

    // 2. Only the constraints with all fixed tvs are going to be visible outside this declaration group.

    vector< Hs::Var > dict_vars = vars_from_lie( lie_retained );

    for(auto& [name, monotype]: mono_binder_env)
    {
        auto qtvs_in_this_type = free_type_variables(monotype);

        // Default any constraints that do not occur in THIS type.
        auto [s2, binds2, lie_for_this_type] = default_preds( fixed_tvs, qtvs_in_this_type, lie_retained );

        auto constraints_for_this_type = constraints_from_lie(lie_for_this_type);

        // Only quantify over type variables that occur in THIS type.
        Hs::Type polytype = quantify( qtvs_in_this_type,
                                      Hs::add_constraints( constraints_for_this_type,
                                                           monotype ) );

        // How can we generate a wrapper between qualified_type and lie_deferred => (type1, unrestricted_type, type3)?

        poly_binder_env = poly_binder_env.insert( {name, polytype} );

        Hs::Var x_outer({noloc,name});
        Hs::Var x_inner = get_fresh_Var(name, false);

        vector<Hs::Var> dict_args;
        for(auto& [name, constraint]: lie_for_this_type)
            dict_args.push_back( Hs::Var({noloc,name}) );

        Hs::BindInfo info(x_outer, x_inner, monotype, polytype, dict_args, binds2);
        bind_infos.insert({name, info});

        if (restricted)
        {
            assert(dict_args.empty());
            assert(constraints_for_this_type.empty());
        }
    }
    assert(bind_infos.size() >= 1);

    auto gen_bind = mkGenBind( qtvs | ranges::to<vector>, dict_vars, binds, decls, bind_infos );
    Hs::Decls decls2({ gen_bind });

    return {decls2, poly_binder_env};
}

