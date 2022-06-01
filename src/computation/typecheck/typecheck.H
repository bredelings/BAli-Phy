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

#include "computation/haskell/haskell.H"
#include "types.H"

Hs::ModuleDecls typecheck(const std::string& s, const Module& m, Hs::ModuleDecls M);

Haskell::Type type_check_class_method_type(kindchecker_state& K, Haskell::Type type, const Haskell::Type& constraint);

std::tuple<global_value_env, global_instance_env, ClassInfo, Hs::Decls>
type_check_type_class(const Module& m, const type_con_env& tce, const Haskell::ClassDecl& class_decl);

typedef value_env constr_env;

typedef std::map<ID, Hs::Type> signature_env;

global_value_env sig_env(const signature_env& signatures);

struct typechecker_state: FreshVarSource
{
    //------- global state ------------//
    const Module& this_mod; // for name lookups like Bool, Num, etc.

    type_con_env tycon_info;

    // maybe merge with gve by mapping name -> type,con-or-var
    constr_env con_info_;

          constr_env& con_info()       {return con_info_;}
    const constr_env& con_info() const {return con_info_;}

    ClassEnv class_env_;

          ClassEnv& class_env()       {return class_env_;}
    const ClassEnv& class_env() const {return class_env_;}

    global_instance_env instance_env_;

          global_instance_env& instance_env()       {return instance_env_;}
    const global_instance_env& instance_env() const {return instance_env_;}

    std::vector<Hs::Type> defaults_;

          std::vector<Hs::Type>& defaults()       {return defaults_;}
    const std::vector<Hs::Type>& defaults() const {return defaults_;}

    //------- shared state -----------//

    std::shared_ptr<substitution_t> type_var_to_type;

    //------- local state ------------//

    global_value_env gve;

    std::vector<local_instance_env> lie_stack;

    //------- Functions handling the LIE ---------
    local_instance_env& current_lie();

    void add_dvar(const ID& name, const Hs::Type& constraint);

    void add_dvar(const Hs::Var& dvar, const Hs::Type& constraint);

    Hs::Var fresh_dvar(const Hs::Type& constraint);

    Hs::Var add_dvar(const Hs::Type& constraint);

    void push_lie();

    local_instance_env pop_lie();

    void pop_and_add_lie();
    // ------ End: Functions handling the LIE ---

    void get_tycon_info(const Hs::Decls& type_decls);

    Hs::Decls add_type_var_kinds(Hs::Decls type_decls);

    void get_constructor_info(const Hs::Decls& decls);

    Hs::Var find_prelude_var(std::string name) const;

    ID find_prelude_tycon_name(const std::string& name) const;

    Hs::TypeCon find_prelude_tycon(const std::string& name) const;

    Hs::Type bool_type() const;

    Hs::Type char_type() const;

    Hs::Type int_type() const;

    Hs::Type double_type() const;

    template <typename T>
    T apply_current_subst(const T& thing) const
    {
        return apply_subst(*type_var_to_type, thing);
    }

    bool add_substitution(const substitution_t& s);

    bool add_substitution(const Hs::TypeVar& a, const Hs::Type& type);

    bool maybe_unify(const Hs::Type& t1, const Hs::Type& t2);

    void unify(const Hs::Type& t1, const Hs::Type& t2);

    void unify(const Hs::Type& t1, const Hs::Type& t2, const myexception& e);

    Hs::Type constructor_type(const Hs::Con&);

    std::pair<Hs::Type, std::vector<Hs::Type>> constructor_pattern_types(const Hs::Con&);

    std::optional<std::tuple<substitution_t, Hs::Binds>>
    candidates(const Hs::TypeVar& tv, const local_instance_env& tv_lie);

    std::tuple<substitution_t, Hs::Binds, local_instance_env >
    default_preds( const std::set<Hs::TypeVar>& fixed_tvs,
                   const std::set<Hs::TypeVar>& referenced_tvs,
                   const local_instance_env& lie);

    Hs::Binds simplify_and_default_top_level();

    std::tuple<std::vector<Hs::TypeVar>, std::vector<Hs::Type>, Hs::Type> instantiate(const Hs::Type& t, bool meta=true);

    Hs::Type
    infer_stmts_type(const global_value_env& env, int i, std::vector<Hs::Qual>& stmts);

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
    infer_pattern_type(const Hs::Pattern& pat, const signature_env& sigs = {});

    std::tuple<Hs::Var, Hs::Type, local_value_env>
    infer_var_pattern_type(Hs::Var v, const signature_env& sigs = {});

    std::tuple<expression_ref, Hs::Type, local_value_env>
    infer_lhs_type(const expression_ref& decl, const signature_env& sigs);

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

    // Figures 13, 14, 15?
    std::tuple<Hs::Decls, global_value_env>
    infer_type_for_decls(const global_value_env& env, const signature_env&, const Hs::Decls&, bool is_top_level);

    std::tuple<expression_ref, ID, Hs::Type>
    infer_type_for_single_fundecl_with_sig(const global_value_env& env, Hs::FunDecl FD);

    std::tuple<Hs::Decls, global_value_env>
    infer_type_for_decls_groups(const global_value_env& env, const signature_env&, Hs::Decls E, bool is_top_level);

    void infer_type_for_foreign_imports(std::vector<Hs::ForeignDecl>& foreign_decls);

    global_value_env infer_type_for_sigs(signature_env& signatures) const;

    // Figures 13, 14, 15?
    Hs::Binds infer_type_for_binds_top(const Hs::Binds& binds);

    std::tuple<Hs::Binds, global_value_env>
    infer_type_for_binds(const global_value_env& env, Hs::Binds binds, bool is_top_level = false);

    Hs::Binds infer_type_for_classes(const Hs::Decls& decls);

    std::tuple<global_value_env, ClassInfo,Hs::Decls>
    infer_type_for_class(const Hs::ClassDecl& class_decl);

    Hs::Decls infer_type_for_default_methods(const Hs::ClassDecl&);

    Hs::Binds infer_type_for_default_methods(const Hs::Decls&);

    // Figure 12
    std::pair<Hs::Var, Hs::Type>
    infer_type_for_instance1(const Hs::InstanceDecl& instance_decl);

    // Figure 12
    std::vector< std::pair<Hs::Var, Hs::InstanceDecl> >
    infer_type_for_instances1(const Hs::Decls& decls);

    // Figure 12
    Hs::Binds
    infer_type_for_instances2(const std::vector<std::pair<Hs::Var, Hs::InstanceDecl>>& named_instances);

    // Figure 12
    Hs::Decls
    infer_type_for_instance2(const Hs::Var& dfun, const Hs::InstanceDecl& instance_decl);

    // Figure 26
    // Express lie2 in terms of gie (functions) and lie1 (arguments to this dfun, I think).
    std::vector<std::pair<Hs::Var,Hs::Type>> constraints_to_lie(const std::vector<Hs::Type>&);

    // FIXME: this should be  const
    std::vector<std::pair<std::string, Hs::Type>> superclass_constraints(const Hs::Type& constraint);

    // FIXME: this should be  const
    std::optional<std::vector<std::string>>
    is_superclass_of(const Hs::Type&, const Hs::Type&);

    // FIXME: this should be  const
    std::optional<Hs::Binds>
    entails_by_superclass(const std::pair<ID, Hs::Type>& to_keep,
                          const std::pair<ID, Hs::Type>& to_remove);

    template <typename T>
    std::optional<Hs::Binds>
    entails(const T& to_keep, const std::pair<ID, Hs::Type>& to_check);

    std::pair<std::optional<Hs::Binds>, local_instance_env>
    entails(const local_instance_env& lie1, const local_instance_env& lie2);

    std::optional<std::pair<Hs::Var,std::vector<Hs::Type>>> lookup_instance(const Hs::Type& constraint);

    std::pair<Hs::Binds,local_instance_env> toHnf(const ID& name, const Hs::Type& constraint);
    std::pair<Hs::Binds, local_instance_env> toHnfs(const local_instance_env& lie_in);
    std::pair<Hs::Binds, local_instance_env> simplify(const local_instance_env& lie_in);
    std::pair<Hs::Binds, local_instance_env> reduce(const local_instance_env& lie_in);
    Hs::Binds reduce_current_lie();

    void get_defaults(const Hs::ModuleDecls& M);

    typechecker_state(FreshVarState& fvs, const std::string& s, const Module& m);
};

template <typename T>
std::optional<Hs::Binds> typechecker_state::entails(const T& to_keep, const std::pair<ID, Hs::Type>& to_remove)
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

local_instance_env unordered_lie(const std::vector<std::pair<Hs::Var, Hs::Type>>& lie1);
std::vector<Hs::Var> vars_from_lie(const local_instance_env& lie);
std::vector<Hs::Var> vars_from_lie(const std::vector<std::pair<Hs::Var, Hs::Type>>& lie);

std::optional<ID> maybe_get_class_name_from_constraint(const Hs::Type& constraint);
ID get_class_name_from_constraint(const Hs::Type& constraint);

global_value_env apply_subst(const substitution_t& s, const value_env& env1);

std::map<ID, Hs::Match> get_instance_methods(const Hs::Binds& decls, const global_value_env& members, const std::string& class_name);

Hs::Var
rename_var_pattern_from_bindinfo(Hs::Var V, const std::map<ID, Hs::BindInfo>& bind_info);

Hs::Pattern
rename_pattern_from_bindinfo(const Hs::Pattern& pat, const std::map<ID, Hs::BindInfo>& bind_info);

std::vector<std::vector<expression_ref>> find_type_groups(const Hs::Decls& type_decls);

ID get_full_class_name_from_constraint(const Hs::Type& constraint);

#endif
