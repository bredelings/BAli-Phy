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
#include "computation/typecheck/solver.H"
#include "types.H"
#include "expected.H"

// The kind-inference paper treats substitutions as a list of terms of the form
// * a     (declaration)
// * b ~ a (definition)
// I think that you have a sequence like {a, b ~ a} then you can't later define a,
// since it is already "declared".

struct unification_env
{
    mutable int next_index = 0;
    immer::map<Hs::TypeVar, Hs::TypeVar> mapping1;
    immer::map<Hs::TypeVar, Hs::TypeVar> mapping2;
    Hs::TypeVar fresh_tyvar(const std::optional<Hs::Kind>& k) const;
};

Hs::ModuleDecls typecheck(const std::string& s, const Module& m, Hs::ModuleDecls M);

Haskell::Type type_check_class_method_type(kindchecker_state& K, Haskell::Type type, const Haskell::Type& constraint);

std::tuple<global_value_env, GIE, ClassInfo, Hs::Decls>
type_check_type_class(const Module& m, const type_con_env& tce, const Haskell::ClassDecl& class_decl);

global_value_env sig_env(const signature_env& signatures);

struct global_tc_state
{
    const Module& this_mod; // for name lookups like Bool, Num, etc.

    type_con_env tycon_info;

    TypeSynonymEnv type_syn_info;

    ClassEnv class_env;

    GIE instance_env;

    std::vector<Hs::Type> defaults;

    // maybe merge with gve by mapping name -> type,con-or-var
    constr_env con_info;

    global_tc_state(const Module& m);
};

struct typechecker_state;

template <typename T>
using tc_action = std::function<void(T,typechecker_state&)>;

template <typename R, typename T>
using tc_function = std::function<R (T,typechecker_state&)>;

struct typechecker_state: FreshVarSource
{
    //------- global state ------------//
    std::shared_ptr<global_tc_state> global_state;

    const Module& this_mod() const {return global_state->this_mod;}

          type_con_env& tycon_info()       {return global_state->tycon_info;}
    const type_con_env& tycon_info() const {return global_state->tycon_info;}

          constr_env& con_info()       {return global_state->con_info;}
    const constr_env& con_info() const {return global_state->con_info;}

          TypeSynonymEnv& type_syn_info()       {return global_state->type_syn_info;}
    const TypeSynonymEnv& type_syn_info() const {return global_state->type_syn_info;}

          ClassEnv& class_env()       {return global_state->class_env;}
    const ClassEnv& class_env() const {return global_state->class_env;}

          GIE& instance_env()       {return global_state->instance_env;}
    const GIE& instance_env() const {return global_state->instance_env;}

          std::vector<Hs::Type>& defaults()       {return global_state->defaults;}
    const std::vector<Hs::Type>& defaults() const {return global_state->defaults;}

    Hs::Type expTypeToType(const Expected& E);

    Hs::Type inferResultToType(const Expected& E);

    void set_expected_type(const Expected& E, const Hs::Type& type);

    //------- local state ------------//

    immer::map<std::string, std::pair<Hs::Var, Hs::Type>> mono_local_env;

    int level = 0;

    global_value_env imported_gve;

    global_value_env gve;

    WantedConstraints collected_wanteds;

    //-------
    Hs::MetaTypeVar fresh_meta_type_var(const std::string& name, const Hs::Kind& k);
    Hs::MetaTypeVar fresh_meta_type_var(const Hs::Kind& k);

    Hs::TypeVar fresh_rigid_type_var(const std::string& name, const Hs::Kind& k);
    Hs::TypeVar fresh_rigid_type_var(const Hs::Kind& k);

    //------- Functions handling the LIE ---------
    typechecker_state copy_clear_wanteds() const;

    typechecker_state copy_inc_level_clear_wanteds() const;

    void promote_mtv(const Hs::MetaTypeVar& mtv);
    bool maybe_promote_mtv(const Hs::MetaTypeVar& mtv);
    void promote(Hs::Type t);

    void add_binders(const local_value_env& binders);

    typechecker_state copy_add_binders(const global_value_env& env) const;

    WantedConstraints& current_wanteds();

    void add_dvar(const Core::Var& dvar, const Hs::Type& constraint);

    Core::Var fresh_dvar(const Hs::Type& constraint);

    Core::Var add_dvar(const Hs::Type& constraint);

    // ------ End: Functions handling the LIE ---

    void get_tycon_info(const Hs::Decls& type_decls);

    Hs::Decls add_type_var_kinds(Hs::Decls type_decls);

    void get_constructor_info(const Hs::Decls& decls);

    const TypeSynonymInfo* maybe_find_type_synonym(const Hs::Type& type) const;

    std::optional<Hs::Type> is_type_synonym(const Hs::Type& type) const;

    void expand_type_synonyms(Hs::Type& type) const;

    Hs::Type check_type(const Hs::Type&) const;

    Hs::Type check_type(const Hs::Type&, kindchecker_state& K) const;

    Hs::Type check_constraint(const Hs::Type&) const;

    Hs::Var find_prelude_var(std::string name) const;

    ID find_prelude_tycon_name(const std::string& name) const;

    Hs::TypeCon find_prelude_tycon(const std::string& name) const;

    Hs::Type bool_type() const;

    Hs::Type char_type() const;

    Hs::Type int_type() const;

    Hs::Type double_type() const;

    bool add_substitution(const Hs::MetaTypeVar& a, const Hs::Type& type);

    bool maybe_unify_(bool allow_unification, bool both_ways, const unification_env& env, const Hs::Type& t1, const Hs::Type& t2);

    bool same_type(const Hs::Type& t1, const Hs::Type& t2) const;

    void unify(const Hs::Type& t1, const Hs::Type& t2);

    void unify(const Hs::Type& t1, const Hs::Type& t2, const myexception& e);

    bool try_insert(const Hs::MetaTypeVar& tv, Hs::Type type) const;

    bool occurs_check(const Haskell::MetaTypeVar& tv, const Hs::Type& t) const;

    bool occurs_check(const Hs::TypeVar& tv, const Hs::Type& t) const;

    bool maybe_match(const Hs::Type& t1, const Hs::Type& t2);

    void match(const Hs::Type& t1, const Hs::Type& t2, const myexception& e);

    void match(const Hs::Type& t1, const Hs::Type& t2);

    std::pair<Hs::Type, Hs::Type> unify_function(const Hs::Type& t);

    std::pair<Hs::Type, Hs::Type> unify_function(const Hs::Type& t, const myexception& e);

    DataConInfo constructor_info(const Hs::Con&);

    std::optional<Core::Decls>
    candidates(const Hs::MetaTypeVar& tv, const LIE& tv_lie);

    Core::Decls default_preds( WantedConstraints& wanteds );

    Core::Decls simplify_and_default_top_level();

    std::tuple<std::vector<Hs::MetaTypeVar>, LIE, Hs::Type> instantiate(const Hs::Type& t);

    std::tuple<Core::wrapper, Hs::Type> instantiate_emit(const Hs::Type& t);

    Core::wrapper subsumptionCheck(const Hs::Type& t1, const Expected& e2);

    Core::wrapper subsumptionCheck(const Hs::Type& t1, const Hs::Type& t2);

    Core::wrapper instantiateSigma(const Hs::Type& t, const Expected& exp_rho);

    std::tuple<Core::wrapper, std::vector<Hs::TypeVar>, LIE, Hs::Type> skolemize(const Hs::Type& t, bool skolem);

    std::tuple<Core::wrapper, std::vector<Hs::TypeVar>, LIE, Hs::Type, std::shared_ptr<const Core::Decls>> skolemize_and(const Hs::Type& t, const tc_action<Hs::Type>& nested_action);

    template <typename R>
    std::tuple<Core::wrapper, std::vector<Hs::TypeVar>, LIE, Hs::Type, std::shared_ptr<const Core::Decls>, R> skolemize_and_result(const Hs::Type& t, const tc_function<R, Hs::Type>& nested_action);

    void tcRhoStmts(int i, std::vector<Hs::Qual>& stmts, const Expected& exp_type);

    // Figure 22.
    void infer_quals_type(std::vector<Hs::Qual>& quals);

    // Figure 22.
    void infer_qual_type(Hs::Qual& qual);

    void infer_guard_type(Hs::Qual& guard);

    // Figure 24.
    Hs::Type inferPat(local_value_env& penv, Hs::Pattern& pat, const signature_env& sigs = {});

    void tcPats(local_value_env& penv, std::vector<Hs::Pattern>& pat, const std::vector<Expected>&, const signature_env& sigs, const tc_action<local_value_env&>& a, int i=0);

    // takes and returns sigma types
    void tcPat(local_value_env& penv, Hs::Pattern& pat, const Expected&, const signature_env& sigs, const tc_action<local_value_env&>& a);

    // takes and returns sigma types
    void checkPat(local_value_env& penv, Hs::Pattern& pat, const Hs::SigmaType&, const signature_env& sigs = {});

    Hs::Type inferPat(local_value_env& penv, Hs::Var& v, const signature_env& sigs = {});

    void tcPat(local_value_env& penv, Hs::Var& v, const Expected&, const signature_env& sigs, const tc_action<local_value_env&>&);

    void checkPat(local_value_env& penv, Hs::Var& v, const Hs::SigmaType& exp_type, const signature_env& sigs);

    Core::wrapper instPatSigma(const Hs::SigmaType& sigma_type, const Expected& exp_sigma_type);

    std::tuple<Hs::Type, local_value_env>
    infer_lhs_type(expression_ref& decl, const signature_env& sigs);

    void infer_rhs_type(expression_ref& decl, const Expected& type);

    Core::wrapper checkSigma(Hs::Expression& e, const Hs::SigmaType& sigma_type);

    template <typename T>
    void checkRho(T& t, const Hs::Type& exp_rho)
    {
        tcRho(t, Check(exp_rho));
    }

    template <typename T>
    Hs::Type inferRho(T& t)
    {
        Expected result_type = Infer();
        tcRho(t, result_type);
        return result_type.read_type();
    }

    void tcRho(expression_ref& exp, const Expected& E);

    void tcRho(Hs::Var&, const Expected& E);

    void tcRho(Hs::Con& con, const Expected& E);

    void tcRho(Hs::ApplyExp& app_exp, const Expected& E, int i=0);

    void tcRho(Hs::LambdaExp&, const Expected& E);

    void tcRho(Hs::LetExp&, const Expected&);

    void tcRho(Hs::TypedExp& TExp, const Expected&);

    void tcRho(Hs::CaseExp& Case, const Expected&);

    void tcRho(Hs::List& L, const Expected&);

    void tcRho(Hs::Tuple& T, const Expected&);

    void tcRho(Hs::Literal& Lit, const Expected&);

    void tcRho(Hs::IfExp&, const Expected&);

    void tcRho(Hs::LeftSection&, const Expected&);

    void tcRho(Hs::RightSection&, const Expected&);

    void tcRho(Hs::Do&, const Expected&);

    void tcRho(Hs::ListComprehension&, const Expected&);

    void tcRho(Hs::ListFrom&, const Expected&);

    void tcRho(Hs::ListFromThen&, const Expected&);

    void tcRho(Hs::ListFromTo&, const Expected&);

    void tcRho(Hs::ListFromThenTo&, const Expected&);

    void tcRho(Hs::GuardedRHS&, const Expected&, int i=0);

    void tcRho(Hs::MultiGuardedRHS&, const Expected&);

    void tcMatch(Hs::MRule& m, const std::vector<Expected>& pat_types, const Expected& result_type);

    void tcMatches(Hs::Matches& m, const std::vector<Expected>& pat_types, const Expected& result_type);

    template <typename A>
    Core::wrapper tcMatchesFunInfer(std::vector<Expected>& arg_types, int arity, const Expected& fun_type, const A& a);

    template <typename A>
    Core::wrapper tcMatchesFunCheck(std::vector<Expected>& arg_types, int arity, Hs::Type type, const A& a);

    template <typename A>
    Core::wrapper tcMatchesFun(int arity, const Expected&, const A& a);

    // Figures 13, 14, 15?
    Hs::Decls
    infer_type_for_decls(const signature_env&, const Hs::Decls&, bool is_top_level);

    std::tuple<expression_ref, ID, Hs::Type>
    infer_type_for_single_fundecl_with_sig(Hs::FunDecl FD);

    Hs::Decls
    infer_type_for_decls_group(const signature_env&, Hs::Decls E, bool is_top_level);

    std::tuple< std::map<std::string, Hs::Var>, local_value_env >
    tc_decls_group_mono(const signature_env& sigs, Hs::Decls& decls);

    void infer_type_for_foreign_imports(std::vector<Hs::ForeignDecl>& foreign_decls);

    global_value_env infer_type_for_sigs(signature_env& signatures) const;

    // Figures 13, 14, 15?
    Hs::Binds infer_type_for_binds_top(Hs::Binds binds);

    void infer_type_for_binds(Hs::Binds& binds, bool is_top_level = false);

    void
    check_type_synonyms(const Hs::Decls& decls);

    Hs::Binds infer_type_for_classes(const Hs::Decls& decls);

    std::tuple<global_value_env, ClassInfo,Hs::Decls>
    infer_type_for_class(const Hs::ClassDecl& class_decl);

    Hs::Decls infer_type_for_default_methods(const Hs::ClassDecl&);

    Hs::Binds infer_type_for_default_methods(const Hs::Decls&);

    // Figure 12
    std::pair<Core::Var, Hs::Type>
    infer_type_for_instance1(const Hs::InstanceDecl& instance_decl);

    // Figure 12
    std::vector< std::pair<Core::Var, Hs::InstanceDecl> >
    infer_type_for_instances1(const Hs::Decls& decls);

    // Figure 12
    std::pair<Hs::Binds, Core::Decls>
    infer_type_for_instances2(const std::vector<std::pair<Core::Var, Hs::InstanceDecl>>& named_instances);

    // Figure 12
    std::pair<Hs::Decls, Core::Decl>
    infer_type_for_instance2(const Core::Var& dfun, const Hs::InstanceDecl& instance_decl);

    // Figure 26
    // Express lie2 in terms of gie (functions) and lie1 (arguments to this dfun, I think).
    LIE constraints_to_lie(const std::vector<Hs::Type>&);

    // FIXME: this should be  const
    std::vector<std::pair<Core::Var, Hs::Type>> superclass_constraints(const Hs::Type& constraint);

    // FIXME: this should be  const
    std::optional<std::vector<Core::Var>>
    is_superclass_of(const Hs::Type&, const Hs::Type&);

    bool is_touchable(const Hs::MetaTypeVar& mtv);

    // FIXME: this should be  const
    std::optional<Core::Decls>
    entails_by_superclass(const std::pair<Core::Var, Hs::Type>& to_keep,
                          const std::pair<Core::Var, Hs::Type>& to_remove);

    std::optional<Reaction> interact_same(const Predicate& P1, const Predicate& P2);
    std::optional<Reaction> interact_g_w(const Predicate& P1, const Predicate& P2);
    std::optional<Reaction> top_react(const Predicate& P);

    Core::Decls simplify(const LIE& givens, LIE& wanteds);

    template <typename T>
    std::optional<Core::Decls>
    entails(const T& to_keep, const std::pair<Core::Var, Hs::Type>& to_check);

    Core::Decls entails(const LIE& lie1, WantedConstraints& lie2);

    bool instance_matches(const Hs::Type&, const Hs::Type&);
    bool more_specific_than(const Hs::Type&, const Hs::Type&);
    std::optional<std::pair<Core::Exp,LIE>> lookup_instance(const Hs::Type& constraint);

    void get_defaults(const Hs::ModuleDecls& M);

    typechecker_state(FreshVarState& fvs, const std::string& s, const Module& m);
};

std::optional<ID> maybe_get_class_name_from_constraint(const Hs::Type& constraint);
ID get_class_name_from_constraint(const Hs::Type& constraint);

LIE apply_subst(const substitution_t& s, const LIE& env1);

std::map<ID, Hs::Matches> get_instance_methods(const Hs::Binds& decls, const global_value_env& members, const std::string& class_name);

Hs::Var rename_var_from_bindinfo(const Hs::Var& V, const std::map<ID, Hs::BindInfo>& bind_info);

Hs::Pattern
rename_pattern_from_bindinfo(const Hs::Pattern& pat, const std::map<ID, Hs::BindInfo>& bind_info);

std::vector<std::vector<expression_ref>> find_type_groups(const Hs::Decls& type_decls);

ID get_full_class_name_from_constraint(const Hs::Type& constraint);

bool constraint_is_hnf(const Hs::Type& constraint);

int getArity(const Hs::Matches& m);
int getArity(const Hs::MRule& m);

template <typename R>
std::tuple<Core::wrapper, std::vector<Hs::TypeVar>, LIE, Hs::Type, std::shared_ptr<const Core::Decls>, R>
typechecker_state::skolemize_and_result(const Hs::Type& polytype, const tc_function<R, Hs::Type>& nested_function)
{
    // 1. Skolemize the type at level
    auto [wrap, tvs, givens, rho_type] = skolemize(polytype, true);

    // 2. typecheck the rhs at level+1
    auto tcs2 = copy_inc_level_clear_wanteds();
    auto result = nested_function(rho_type, tcs2);
    auto wanteds = tcs2.current_wanteds();

    auto ev_decls = std::make_shared<Core::Decls>();
    auto imp = std::make_shared<Implication>(level+1, tvs, givens, wanteds, ev_decls);
    current_wanteds().implications.push_back( imp );

    return {wrap, tvs, givens, rho_type, ev_decls, result};
}


#endif


