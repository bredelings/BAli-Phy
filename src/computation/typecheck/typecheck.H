#ifndef TYPECHECK_H
#define TYPECHECK_H

#include <string>
#include <vector>
#include <map>
#include "immer/map.hpp" // for immer::map
#include "kind.H"
#include "kindcheck.H"
#include "env.H"
#include "unify.H"
#include <range/v3/all.hpp>
#include "computation/expression/apply.H"

#include "computation/parser/haskell.H"
#include "types.H"

Hs::ModuleDecls typecheck(const std::string& s, const Module& m, Hs::ModuleDecls M);

Haskell::Type type_check_class_method_type(kindchecker_state& K, Haskell::Type type, const Haskell::Type& constraint);

std::tuple<global_value_env, global_instance_env,class_info,Hs::Decls>
type_check_type_class(const Module& m, const type_con_env& tce, const Haskell::ClassDecl& class_decl);

typedef value_env constr_env;

typedef std::map<std::string, Hs::Type> signature_env;

struct typechecker_state
{
    int next_var_index = 1;

    int next_tvar_index = 1;

    constr_env con_info;

    global_instance_env gie;

    std::vector<Hs::Type> defaults;

    std::string mod_name;

    const Module& this_mod; // for name lookups like Bool, Num, etc.

    substitution_t type_var_to_type;

    std::vector<local_instance_env> lie_stack;

    int level = 0;

    local_instance_env& current_lie();

    void add_dvar(const std::string& name, const Hs::Type& constraint);

    void add_dvar(const Hs::Var& dvar, const Hs::Type& constraint);

    Hs::Var fresh_dvar(const Hs::Type& constraint);

    Hs::Var add_dvar(const Hs::Type& constraint);

    void push_lie();

    local_instance_env pop_lie();

    void pop_and_add_lie();

    Hs::Var find_prelude_var(std::string name) const;

    std::string find_prelude_tycon_name(const std::string& name) const;

    Hs::TypeCon find_prelude_tycon(const std::string& name) const;

    Hs::Type bool_type() const;

    Hs::Type char_type() const;

    Hs::Type enum_class(const Hs::Type& arg) const;

    Hs::Type num_class(const Hs::Type& arg) const;

    Hs::Type fractional_class(const Hs::Type& arg) const;

    std::tuple<Hs::Var, Hs::Type> fresh_enum_type(bool meta = true);

    std::tuple<Hs::Var, Hs::Type> fresh_num_type(bool meta = true);

    std::tuple<Hs::Var, Hs::Type> fresh_fractional_type(bool meta = true);

    template <typename T>
    T apply_current_subst(const T& thing) const
    {
        return apply_subst(type_var_to_type, thing);
    }

    bool add_substitution(const substitution_t& s);

    bool add_substitution(const Hs::TypeVar& a, const Hs::Type& type);

    bool maybe_unify(const Hs::Type& t1, const Hs::Type& t2);

    void unify(const Hs::Type& t1, const Hs::Type& t2);

    std::pair<Hs::Type, std::vector<Hs::Type>> constr_types(const Hs::Con&);

    Hs::Var fresh_var(const std::string& s, bool qualified);

    // "Rigid" type vars come from forall-quantified variables.
    // "Wobbly" type vars come from existentially-quantified variables (I think).  We don't have any.
    // "Meta" type vars are unification type vars.
    Hs::TypeVar fresh_rigid_type_var();

    Hs::TypeVar fresh_meta_type_var();

    Hs::TypeVar fresh_type_var(bool meta);

    std::optional<std::tuple<substitution_t, Hs::Binds>>
    candidates(const Hs::TypeVar& tv, const local_instance_env& tv_lie);

    std::tuple<substitution_t, Hs::Binds, local_instance_env >
    default_preds( const std::set<Hs::TypeVar>& fixed_tvs,
                   const std::set<Hs::TypeVar>& referenced_tvs,
                   const local_instance_env& lie);

    Hs::Binds default_subst();

    std::tuple<std::vector<Hs::TypeVar>, std::vector<Hs::Type>, Hs::Type> instantiate(const Hs::Type& t, bool meta=true);

    // Figure 22.
    std::tuple<std::vector<Hs::Qual>, local_value_env>
    infer_quals_type(const global_value_env& env, std::vector<Hs::Qual> quals);

    // Figure 22.
    std::tuple<Hs::Qual, local_value_env>
    infer_qual_type(const global_value_env& env, const Hs::Qual& qual);

    std::tuple<Hs::Qual, local_value_env>
    infer_guard_type(const global_value_env& env, const Hs::Qual& guard);

    // Figure 24.
    std::tuple<Hs::Pattern, Hs::Type, local_value_env>
    infer_pattern_type(const Hs::Pattern& pat);

    std::tuple<Hs::Var, Hs::Type, local_value_env>
    infer_lhs_var_type(Hs::Var v);

    std::tuple<expression_ref, Hs::Type, local_value_env>
    infer_lhs_type(const expression_ref& decl);

    std::tuple<expression_ref, Hs::Type>
    infer_rhs_type(const global_value_env& env, const expression_ref& decl);

    std::tuple<expression_ref, Hs::Type>
    infer_type(const global_value_env& env, expression_ref exp);

    std::tuple<Hs::GuardedRHS, Hs::Type>
    infer_type(const global_value_env& env, Hs::GuardedRHS);

    std::tuple<Hs::MultiGuardedRHS, Hs::Type>
    infer_type(const global_value_env& env, Hs::MultiGuardedRHS);

    std::tuple<Hs::MRule, Hs::Type>
    infer_type(const global_value_env& env, Hs::MRule);

    std::tuple<Hs::Match, Hs::Type>
    infer_type(const global_value_env& env, Hs::Match);

    global_value_env sig_env(const std::map<std::string, Hs::Type>& signatures);

    // Figures 13, 14, 15?
    std::tuple<Hs::Decls, global_value_env>
    infer_type_for_decls(const global_value_env& env, const signature_env&, Hs::Decls E);

    std::tuple<expression_ref, std::string, Hs::Type>
    infer_type_for_single_fundecl_with_sig(const global_value_env& env, Hs::FunDecl FD);

    std::tuple<Hs::Decls, global_value_env>
    infer_type_for_decls_groups(const global_value_env& env, const signature_env&, Hs::Decls E);

    // Figures 13, 14, 15?
    std::tuple<Hs::Binds, global_value_env>
    infer_type_for_binds(const global_value_env& env, Hs::Binds binds);

    std::tuple<global_value_env, global_instance_env, class_env, Hs::Binds>
    infer_type_for_classes(const Hs::Decls& decls, const type_con_env& tce);

    std::tuple<global_value_env,global_instance_env,class_info,Hs::Decls>
    infer_type_for_class(const type_con_env& tce, const Hs::ClassDecl& class_decl);

    // Figure 12
    global_instance_env
    infer_type_for_instances1(const Hs::Decls& decls, const class_env& ce);

    // Figure 12
    global_instance_env
    infer_type_for_instance1(const Hs::InstanceDecl& instance_del, const class_env& ce);

    // Figure 12
    Hs::Decls
    infer_type_for_instances2(const Hs::Decls& decls, const class_env& ce);

    // Figure 12
    Hs::Decls
    infer_type_for_instance2(const Hs::InstanceDecl& instance_del, const class_env& ce);

    // Figure 26
    // Express lie2 in terms of gie (functions) and lie1 (arguments to this dfun, I think).
    local_instance_env constraints_to_lie(const std::vector<Hs::Type>&);

    // FIXME: this should be  const
    std::vector<std::pair<std::string, Hs::Type>> superclass_constraints(const Hs::Type& constraint);

    // FIXME: this should be  const
    std::optional<std::vector<std::string>>
    is_superclass_of(const Hs::Type&, const Hs::Type&);

    // FIXME: this should be  const
    std::optional<Hs::Binds>
    entails_by_superclass(const std::pair<std::string, Hs::Type>& to_keep,
                          const std::pair<std::string, Hs::Type>& to_remove);

    template <typename T>
    std::optional<Hs::Binds> entails(const T& to_keep, const std::pair<std::string, Hs::Type>& to_check);

    std::optional<Hs::Binds> entails(const local_instance_env& lie1, const local_instance_env& lie2);

    std::optional<std::pair<Hs::Var,std::vector<Hs::Type>>> lookup_instance(const Hs::Type& constraint);

    std::pair<Hs::Binds,local_instance_env> toHnf(const std::string& name, const Hs::Type& constraint);
    std::pair<Hs::Binds, local_instance_env> toHnfs(const local_instance_env& lie_in);
    std::pair<Hs::Binds, local_instance_env> simplify(const local_instance_env& lie_in);
    std::pair<Hs::Binds, local_instance_env> reduce(const local_instance_env& lie_in);
    Hs::Binds reduce_current_lie();

    typechecker_state(const std::string& s, const Module& m, const Hs::ModuleDecls& M, const constr_env& ce);
};

template <typename T>
std::optional<Hs::Binds> typechecker_state::entails(const T& to_keep, const std::pair<std::string, Hs::Type>& to_remove)
{
    // 1. First check if the relevant constraints are superclasses of the current constraint.
    for(auto& constraint2: to_keep)
    {
        if (auto binds = entails_by_superclass(constraint2, to_remove))
            return *binds;
    }

    // 2. Then check if there is an instance dfun :: (K1 a, K2 a) => K a
    if (auto inst = lookup_instance(to_remove.second))
    {
        auto [dfun, constraints] = *inst;

        Hs::Binds binds;
        // If we can get (dvar1 :: K1 a) and (dvar2 :: K2 a) and a dfun so that dvar = dfun dvar1 dvar2
        std::vector<Hs::Exp> args;
        for(auto& constraint: constraints)
        {
            Hs::Decls decls;
            auto dvar = fresh_dvar(constraint);
            args.push_back(dvar);
            if (auto cbinds = entails(to_keep, {unloc(dvar.name),constraint}))
            {
                ranges::insert(binds, binds.begin(), *cbinds);
            }
            else
                return {};
        }

        Hs::Decls decls;
        Hs::Var dvar( {noloc, to_remove.first} );
        decls.push_back( Hs::simple_decl( dvar, apply_expression(dfun, args)) );
        binds.push_back( decls );

        return binds;
    }

    return {};
}

#endif
