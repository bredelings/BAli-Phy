#include "typecheck.H"
#include "kindcheck.H"
#include "types.H"
#include "match.H" // for tcMatchesFun

#include "rename/rename.H" // for get_indices_for_names( )

#include "util/set.H"

#include <range/v3/all.hpp>

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;
using std::shared_ptr;


template <typename T>
Type quantify(const T& tvs, const Type& monotype)
{
    if (tvs.empty())
        return monotype;
    else
    {
        for(auto& tv: tvs)
            assert(tv.kind);
        return ForallType(tvs | ranges::to<vector>, monotype);
    }
}

global_value_env sig_env(const map<string, Type>& signatures)
{
    global_value_env sig_env;
    for(auto& [name, type]: signatures)
        sig_env = sig_env.insert({name, type});
    return sig_env;
}

Hs::Binds TypeChecker::infer_type_for_binds_top(Hs::Binds binds)
{
    infer_type_for_binds(binds, true);
    return binds;
}

void TypeChecker::infer_type_for_foreign_imports(vector<Hs::ForeignDecl>& foreign_decls)
{
    global_value_env fte;
    for(auto& f: foreign_decls)
    {
        auto type = check_type( desugar(f.type) );
        fte = fte.insert({f.function_name, type});
    }
    gve += fte;
}

void
TypeChecker::infer_type_for_binds(Hs::Binds& binds, bool is_top_level)
{
    global_value_env sigs;
    signature_env sigs2;
    for(auto& [name,type]: binds.signatures)
    {
        auto type2 = check_type(desugar(type));
        sigs = sigs.insert({name,type2});
        sigs2.insert({name,type2});
    }

    add_binders(sigs);

    for(auto& decls: binds)
        decls = infer_type_for_decls(sigs2, decls, is_top_level);
}

value_env remove_sig_binders(value_env binder_env, const signature_env& signatures)
{
    auto no_sig_binder_env = binder_env;
    for(auto& [name,_]: binder_env)
        if (signatures.count(name))
            no_sig_binder_env = no_sig_binder_env.erase(name);
    return no_sig_binder_env;
}

vector<Hs::Decls> split_decls_by_signatures(const Hs::Decls& decls, const map<string, Type>& signatures)
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

Hs::Decls
TypeChecker::infer_type_for_decls(const signature_env& signatures, const Hs::Decls& decls, bool is_top_level)
{
    // The signatures for the binders should already be in the environment.

    auto bind_groups = split_decls_by_signatures(decls, signatures);

    Hs::Decls decls2;
    for(auto& group: bind_groups)
    {
        ErrorContext ec;
        ec<<"In recursive group:\n";
        for(auto& decl: group)
        {
            if (auto fd = decl.to<Hs::FunDecl>())
                ec<<"    "<<fd->v.print()<<"\n";
            else if (auto pd = decl.to<Hs::PatDecl>())
                ec<<"    "<<pd->lhs.print()<<"\n";
            else
                std::abort();
        }
        context.push_err_context(ec);

        auto group_decls = infer_type_for_decls_group(signatures, group, is_top_level);

        context.pop_err_context();
        
        for(auto& decl: group_decls)
            decls2.push_back(decl);
    }
    return decls2;
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

expression_ref
rename_from_bindinfo(expression_ref decl, const map<string, Hs::BindInfo>& bind_infos)
{
    if (auto fd = decl.to<Hs::FunDecl>())
    {
        auto FD = *fd;
        FD.v = rename_var_from_bindinfo(FD.v, bind_infos);
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

Hs::GenBind mkGenBind(const vector<TypeVar>& tvs,
                      const vector<Core::Var>& dict_vars,
                      const shared_ptr<const Core::Decls>& ev_decls,
                      Hs::Decls decls,
                      const map<string, Hs::BindInfo>& bind_infos)
{
    decls = rename_from_bindinfo(decls, bind_infos);
    return Hs::GenBind(tvs, dict_vars, ev_decls, decls, bind_infos);
}

// Why aren't we using `fixed_type_vars`?
// I guess the deferred constraints that do not mention fixed_type_vars are ambiguous?
pair<LIE, LIE>
classify_constraints(bool restricted, const LIE& lie, const set<MetaTypeVar>& qtvs)
{
    if (restricted) return {lie, {}};

    LIE lie_deferred;
    LIE lie_retained;

    for(auto& [dvar, constraint]: lie)
    {
        if (intersects( free_meta_type_variables(constraint), qtvs ))
            lie_retained.push_back({dvar,constraint});
        else
            lie_deferred.push_back({dvar,constraint});
    }
    return {lie_deferred, lie_retained};
}

pair<LIE, LIE>
classify_constraints(const LIE& lie, const set<TypeVar>& qtvs)
{
    LIE lie_deferred;
    LIE lie_retained;

    for(auto& [dvar, constraint]: lie)
    {
        if (intersects( free_type_variables(constraint), qtvs ))
            lie_retained.push_back({dvar,constraint});
        else
            lie_deferred.push_back({dvar,constraint});
    }
    return {lie_deferred, lie_retained};
}

/// Compare to checkSigma, which also check for any skolem variables in the wanteds
tuple<expression_ref, ID, Type>
TypeChecker::infer_type_for_single_fundecl_with_sig(Hs::FunDecl FD)
{
    // Q: Are we getting the monotype correct?

    try
    {
        auto& name = unloc(FD.v.name);

        // 1. skolemize the type -> (tvs, givens, rho-type)
        auto polytype = gve.at(name);
        auto [wrap_gen, tvs, givens, rho_type] =
            skolemize_and(polytype,
                          [&](const Type& rho_type, auto& tcs2) {
                              tcs2.tcMatchesFun( getArity(FD.matches), Check(rho_type),
                                                 [&](const vector<Expected>& arg_types, const Expected& result_type) {return [&](auto& tc) {
                                                     tc.tcMatches(FD.matches, arg_types, result_type);};});
                          }
                );

        // 2. return GenBind with tvs, givens, body
        Hs::Var inner_id = get_fresh_Var(unloc(FD.v.name),false);

        Type monotype = rho_type;

        Hs::BindInfo bind_info(FD.v, inner_id, monotype, polytype, wrap_gen);

        auto decl = mkGenBind( {}, {}, std::make_shared<Core::Decls>(), Hs::Decls({FD}), {{name, bind_info}} );

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

bool is_restricted(const map<ID, Type>& signatures, const Hs::Decls& decls)
{
    for(auto& decl: decls)
    {
        if (decl.is_a<Hs::PatDecl>())
            return true;
        else if (auto fd = decl.to<Hs::FunDecl>())
        {
            // Simple pattern declaration
            if (fd->matches[0].patterns.size() == 0)
            {
                auto& name = unloc(fd->v.name);
                if (not signatures.count(name)) return true;
            }
        }
    }
    return false;
};

tuple<Type, local_value_env>
TypeChecker::infer_lhs_type(expression_ref& decl, const map<string, Type>& signatures)
{
    if (auto fd = decl.to<Hs::FunDecl>())
    {
        auto FD = *fd;
        // If there was a signature, we would have called infer_type_for_single_fundecl_with_sig
        assert(not signatures.count(unloc(FD.v.name)));

        local_value_env lve;
        auto type = inferPat(lve, FD.v);
        decl = FD;
        return {type, lve};
    }
    else if (auto pd = decl.to<Hs::PatDecl>())
    {
        auto PD = *pd;
        local_value_env lve;
        auto type = inferPat( lve, unloc(PD.lhs), signatures);
        decl = PD;
        return {type, lve};
    }
    else
        std::abort();
}

void TypeChecker::infer_rhs_type(expression_ref& decl, const Expected& rhs_type)
{
    if (auto fd = decl.to<Hs::FunDecl>())
    {
        auto FD = *fd;
        tcMatchesFun( getArity(FD.matches), rhs_type, [&](const auto& arg_types, const auto& result_type) {return [&](auto& tc) {
            tc.tcMatches(FD.matches, arg_types, result_type);};}
                        );
        decl = FD;
    }
    else if (auto pd = decl.to<Hs::PatDecl>())
    {
        auto PD = *pd;
        tcRho(PD.rhs, rhs_type);
        decl = PD;
    }
    else
        std::abort();
}

tuple< map<string, Hs::Var>, local_value_env > TypeChecker::tc_decls_group_mono(const signature_env& signatures, Hs::Decls& decls)
{
    // 1. Add each let-binder to the environment with a fresh type variable
    local_value_env mono_binder_env;

    std::map<std::string, Hs::Var> mono_ids;

    vector<Type> lhs_types;
    for(int i=0;i<decls.size();i++)
    {
        auto [type, lve] = infer_lhs_type( decls[i], signatures );

        lhs_types.push_back(type);
        mono_binder_env += lve;
    }

    for(auto& [name, type]: mono_binder_env)
    {
        Hs::Var mono_id = get_fresh_Var(name, false);
        mono_ids.insert({name, mono_id});

        if (not signatures.count(name))
        {
            mono_local_env = mono_local_env.erase(name);
            mono_local_env = mono_local_env.insert({name,{mono_id, type}});
        }
    }

    // 2. Infer the types of each of the x[i]
    for(int i=0;i<decls.size();i++)
    {
        try{
            infer_rhs_type(decls[i], Check(lhs_types[i]));
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

    return {mono_ids, mono_binder_env};
}

bool type_is_hnf(const Type& type)
{
    auto [head,args] = decompose_type_apps(type);

    head = follow_meta_type_var(head);

    if (head.is_a<TypeVar>())
        return true;
    else if (head.is_a<MetaTypeVar>())
        return true;
    else if (head.is_a<TypeCon>())
        return false;
    else if (head.is_a<ListType>())
        return false;
    else if (head.is_a<TupleType>())
        return false;
    else
        std::abort();
}

// OK:     K a, K (a b), K (a [b]), etc. OK
// NOT OK: K [a], K (a,b), etc. NOT OK.
// Question: for multiparameter type classes, how about i.e. `K Int a`?
bool constraint_is_hnf(const Type& constraint)
{
    auto [class_con, args] = decompose_type_apps(constraint);
    for(auto& arg: args)
        if (not type_is_hnf(arg))
            return false;
    return true;
}


void check_HNF(const LIE& wanteds)
{
    for(auto& [_,constraint]: wanteds)
        if (not constraint_is_hnf(constraint))
            throw myexception()<<"No instance for '"<<constraint<<"'";
}


/* NOTE: Constraints can reference variables that are in
 *        (i) ALL types in a recursive group
 *       (ii) SOME-BUT-NOT-ALL types
 *      (iii) NO types.
 *
 * For unrestricted bindings, classes (ii) and (iii) need defaults.
 * For restricted bindings, only class (iii) (I think) needs defaults.
 */

// For the COMPLETELY ambiguous constraints, we should be able to just discard the constraints,
//   after generating definitions of their dictionaries.

set<MetaTypeVar> find_fixed_tvs(bool restricted, int level, const LIE& wanteds, const set<MetaTypeVar>& tvs)
{
    set<MetaTypeVar> fixed;

    for(auto& tv: tvs)
        if (tv.level() <= level)
            fixed.insert(tv);

    if (restricted)
        add(fixed, free_meta_type_variables(wanteds));

    // If we have alpha[1] ~ [ beta[2] ], then beta should also be considered fixed.
    for(auto& [dvar,constraint]: wanteds)
    {
        if (auto eq = is_equality_constraint(constraint))
        {
            auto [t1,t2] = *eq;

            if (auto lvl = unfilled_meta_type_var(t1); lvl and *lvl <= level)
            {
                add( fixed, free_meta_type_variables(t2) );
            }
            else
            {
                assert(not unfilled_meta_type_var(t2));
            }
        }
    }

    return fixed;
}

Hs::Decls
TypeChecker::infer_type_for_decls_group(const map<string, Type>& signatures, Hs::Decls decls, bool is_top_level)
{
    if (single_fundecl_with_sig(decls, signatures))
    {
        auto& FD = decls[0].as_<Hs::FunDecl>();

        auto [decl, name, sig_type] = infer_type_for_single_fundecl_with_sig(FD);

        Hs::Decls decls({decl});

        return decls;
    }

    // 1. Type check the decls group with monomorphic types for vars w/o signatures.
    auto tcs2 = copy_clear_wanteds(true);
    auto [mono_ids, mono_binder_env] = tcs2.tc_decls_group_mono(signatures, decls);
    auto wanteds = tcs2.current_wanteds();

    // 2. Check if there are predicates on signatures with the monomorphism restriction..
    bool restricted = is_restricted(signatures, decls) and not is_top_level;
    // TODO: complain here if restricted variable have signatures with constraints?

    // FIXME! We also need to minimize constraints like (Eq a, Ord a) down to (Ord a).
    // See mkMinimayBySCs -- am I already doing this inside entails( ) / solve( )?

    // 3. Try and solve the wanteds.  (See simplifyInfer)
    //
    //    This also substitutes into the current LIE, which we need to do 
    //       before finding free type vars in the LIE below.
    auto solve_decls = tcs2.entails({},  wanteds );

    auto tvs_in_any_type = free_meta_type_variables(mono_binder_env);
    auto local_tvs = tvs_in_any_type;
    add( local_tvs, free_meta_type_variables(wanteds.simple) );

    // 4. Figure out which type vars we cannot quantify over.
    auto fixed_tvs = find_fixed_tvs(restricted, level, wanteds.simple, local_tvs);

    // 5. After deciding which vars we may NOT quantify over, figure out which ones we CAN quantify over.
    set<MetaTypeVar> qmtvs = tvs_in_any_type - fixed_tvs;

    // 6. Defer constraints w/o any vars to quantify over
    auto [lie_deferred, lie_retained] = classify_constraints( restricted, wanteds.simple, qmtvs );
    current_wanteds() += lie_deferred;

    // 7. Replace quantified meta-typevars with fresh type vars, and promote the other ones.
    set<TypeVar> qtvs;
    for(auto& qmtv: qmtvs)
    {
        TypeVar qtv = fresh_other_type_var(unloc(qmtv.name), *qmtv.kind);
        qtvs.insert(qtv);
        qmtv.fill(qtv);
    }

    // promote type vars that we are not quantifying over.
    for(auto& tv: local_tvs)
        if (not tv.filled())
            maybe_promote_mtv(tv, level);

    for(auto& tv: local_tvs)
        assert(max_level(tv) <= level);

    // For the SOMEWHAT ambiguous constraints, we don't need the defaults to define the recursive group,
    // but we do need the defaults to define individual symbols.

    // 7. Quantify over variables in ANY type that are not fixed -- doesn't depend on defaulting.
    // Never quantify over variables that are only in a LIE -- those must be defaulted.

    // 8. Only the constraints with all fixed tvs are going to be visible outside this declaration group.
    check_HNF( lie_retained );
    assert(not restricted or lie_retained.empty());

    map<string, Hs::BindInfo> bind_infos;
    global_value_env poly_binder_env;

    for(auto& [name, monotype]: mono_binder_env)
    {
        set<TypeVar> qtvs_in_this_type = intersection( qtvs, free_type_variables( monotype ) );

        set<TypeVar> qtvs_unused = qtvs - qtvs_in_this_type;

        // Replace any unused typevars with metavariables
        substitution_t s;
        for(auto& tv: qtvs_unused)
        {
            assert(tv.kind);
            auto new_tv = fresh_meta_type_var(unloc(tv.name), *tv.kind);
            s = s.insert({tv, new_tv});
        }
        auto lie_all = apply_subst(s, lie_retained);

        // Get new dict vars for constraints
        for(auto& [dvar,constraint]: lie_all)
            dvar = fresh_dvar(constraint);

        // Any constraints that don't mention type vars of this type are ambiguous.
        // We will put them into the environment in hopes that we can default them later.
        auto [lie_unused, lie_used] = classify_constraints( lie_all, qtvs_in_this_type );
        current_wanteds() += lie_unused;

        auto dict_args = vars_from_lie( lie_used );
        auto tup_dict_args = vars_from_lie( lie_all );
        auto wrap = Core::WrapLambda(dict_args) * Core::WrapApply(tup_dict_args);

        auto constraints_used = constraints_from_lie(lie_used);
        Type polytype = quantify( qtvs_in_this_type, add_constraints( constraints_used, monotype ) );
        if (not signatures.count(name))
            poly_binder_env = poly_binder_env.insert( {name, polytype} );
        else
        {
            auto sub_polytype = polytype;
            polytype = signatures.at(name);
            wrap = subsumptionCheck(sub_polytype, polytype) * wrap;
        }

        Hs::Var poly_id({noloc,name});
        Hs::Var mono_id = mono_ids.at(name);
        bind_infos.insert({name, Hs::BindInfo(poly_id, mono_id, monotype, polytype, wrap)});
    }
    assert(bind_infos.size() >= 1);

    add_binders(poly_binder_env);

    vector< Core::Var > dict_vars = vars_from_lie( lie_retained );
    auto gen_bind = mkGenBind( qtvs | ranges::to<vector>, dict_vars, std::make_shared<Core::Decls>(solve_decls), decls, bind_infos );
    Hs::Decls decls2({ gen_bind });

    for(auto& [_,constraint]: current_wanteds().simple)
    {
        assert( max_level(constraint) <= level );
    }

    return decls2;
}

