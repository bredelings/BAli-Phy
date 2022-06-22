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

struct global_tc_state
{
    const Module& this_mod; // for name lookups like Bool, Num, etc.

    type_con_env tycon_info;

    TypeSynonymEnv type_syn_info;

    ClassEnv class_env;

    global_instance_env instance_env;

    std::vector<Hs::Type> defaults;

    u_substitution_t type_var_to_type;

    // maybe merge with gve by mapping name -> type,con-or-var
    constr_env con_info;

    global_tc_state(const Module& m);
};

struct Check
{
    Hs::Type type;
    Check(const Hs::Type& t):type(t) {}
};

class Infer
{
    Hs::Type& type_;

public:
    void set_type(const Hs::Type& t)
    {
        assert(type_.empty());
        type_ = t;
        assert(not type_.empty());
    }

    std::optional<Hs::Type> type() const
    {
        if (type_.empty())
            return {};
        else
            return type_;
    }

    Infer(Hs::Type& t):type_(t) { assert(type_.empty()); }
};

class Expected
{
    mutable std::variant<Infer, Check> value;
public:
    bool check() const {return std::holds_alternative<Check>(value);}
    bool infer() const {return std::holds_alternative<Infer>(value);}

    Hs::Type check_type() const
    {
        assert(check());
        return std::get<Check>(value).type;
    }

    std::optional<Hs::Type> inferred_type() const
    {
        assert(infer());
        return std::get<Infer>(value).type();
    }

    void infer_type(const Hs::Type& t)  const
    {
        assert(infer());
        std::get<Infer>(value).set_type(t);
    }

    Expected(const Check& c):value(c) {}
    Expected(const Infer& i):value(i) {}
};

typedef std::function<Hs::Expression(const Hs::Expression&)> wrapper;

std::string print(const LIE& lie);

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

          global_instance_env& instance_env()       {return global_state->instance_env;}
    const global_instance_env& instance_env() const {return global_state->instance_env;}

          std::vector<Hs::Type>& defaults()       {return global_state->defaults;}
    const std::vector<Hs::Type>& defaults() const {return global_state->defaults;}

          u_substitution_t& type_var_to_type()       {return global_state->type_var_to_type;}
    const u_substitution_t& type_var_to_type() const {return global_state->type_var_to_type;}

    Hs::Type expTypeToType(const Expected& E);

    //------- local state ------------//

    immer::map<std::string, std::pair<Hs::Var, Hs::Type>> mono_local_env;

    global_value_env gve;

    local_instance_env lie_given;

    local_instance_env lie;

    //------- Functions handling the LIE ---------
    typechecker_state copy_clear_lie() const;

    void add_binders(const local_value_env& binders);

    typechecker_state copy_add_binders(const global_value_env& env) const;

    local_instance_env& current_lie();

    void add_dvar(const ID& name, const Hs::Type& constraint);

    void add_dvar(const Hs::Var& dvar, const Hs::Type& constraint);

    Hs::Var fresh_dvar(const Hs::Type& constraint);

    Hs::Var add_dvar(const Hs::Type& constraint);

    // ------ End: Functions handling the LIE ---

    void get_tycon_info(const Hs::Decls& type_decls);

    Hs::Decls add_type_var_kinds(Hs::Decls type_decls);

    void get_constructor_info(const Hs::Decls& decls);

    const TypeSynonymInfo* maybe_find_type_synonym(const Hs::Type& type) const;

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

    Hs::Type apply_current_subst(const Hs::Type& t) const;

    value_env apply_current_subst(const value_env& env) const;

    bool add_substitution(const u_substitution_t& s);

    bool add_substitution(const Hs::MetaTypeVar& a, const Hs::Type& type);

    bool maybe_unify(const Hs::Type& t1, const Hs::Type& t2);

    void unify(const Hs::Type& t1, const Hs::Type& t2);

    void unify(const Hs::Type& t1, const Hs::Type& t2, const myexception& e);

    bool maybe_match(const Hs::Type& t1, const Hs::Type& t2);

    void match(const Hs::Type& t1, const Hs::Type& t2, const myexception& e);

    void match(const Hs::Type& t1, const Hs::Type& t2);

    std::pair<Hs::Type, Hs::Type> unify_function(const Hs::Type& t);

    std::pair<Hs::Type, Hs::Type> unify_function(const Hs::Type& t, const myexception& e);

    Hs::Type constructor_type(const Hs::Con&);

    std::pair<Hs::Type, std::vector<Hs::Type>> constructor_pattern_types(const Hs::Con&);

    std::optional<std::tuple<u_substitution_t, Hs::Binds>>
    candidates(const Hs::MetaTypeVar& tv, const local_instance_env& tv_lie);

    std::tuple<u_substitution_t, Hs::Binds, local_instance_env >
    default_preds( const std::set<Hs::MetaTypeVar>& fixed_tvs,
                   const std::set<Hs::MetaTypeVar>& referenced_tvs,
                   const local_instance_env& lie);

    Hs::Binds simplify_and_default_top_level();

    std::tuple<std::vector<Hs::MetaTypeVar>, LIE, Hs::Type> instantiate(const Hs::Type& t);

    wrapper subsumptionCheck(const Hs::Type& t1, const Hs::Type& t2);

    std::pair<wrapper, LIE> instantiateSigma(const Hs::Type& t, const Expected& exp_rho);

    std::tuple<std::vector<Hs::TypeVar>, std::vector<std::pair<Hs::Var,Hs::Type>>, Hs::Type> skolemize(const Hs::Type& t, bool skolem);

    void tcRhoStmts(int i, std::vector<Hs::Qual>& stmts, const Expected& exp_type);

    // Figure 22.
    void infer_quals_type(std::vector<Hs::Qual>& quals);

    // Figure 22.
    void infer_qual_type(Hs::Qual& qual);

    void infer_guard_type(Hs::Qual& guard);

    // Figure 24.
    std::tuple<Hs::Type, local_value_env>
    infer_pattern_type(Hs::Pattern& pat, const signature_env& sigs = {});

    // takes and returns sigma types
    global_value_env tcPat(Hs::Pattern& pat, const Expected&, const signature_env& sigs = {});

    // takes and returns sigma types
    global_value_env checkPat(Hs::Pattern& pat, const Hs::SigmaType&, const signature_env& sigs = {});

    std::tuple<Hs::Type, local_value_env>
    infer_var_pattern_type(Hs::Var& v, const signature_env& sigs = {});

    global_value_env
    tcVarPat(Hs::Var& v, const Expected&, const signature_env& sigs = {});

    global_value_env
    checkVarPat(Hs::Var& v, const Hs::SigmaType& exp_type, const signature_env& sigs);

    std::tuple<Hs::Type, local_value_env>
    infer_lhs_type(expression_ref& decl, const signature_env& sigs);

    Hs::Type
    infer_rhs_type(expression_ref& decl);

    // checkRho
    template <typename T>
    void checkRho(T& t, const Hs::Type& exp_rho)
    {
        // FIXME -- we need the location!
        auto orig_t = t;
        auto type = inferRho(t);
        try
        {
            unify(type, exp_rho);
        }
        catch (myexception& ex)
        {
            std::ostringstream header;
            header<<"Expected type\n\n";
            header<<"   "<<apply_current_subst(exp_rho)<<"\n\n";
            header<<"but got type\n\n";
            header<<"   "<<apply_current_subst(type)<<"\n\n";
            header<<"for expression\n\n";
            header<<"   "<<orig_t.print()<<"\n";

            ex.prepend(header.str());
            throw;
        }
    }

    void checkSigma(Hs::Expression& e, const Hs::SigmaType& sigma_type);

    // inferRho

    template <typename T>
    Hs::Type inferRho(T& t)
    {
        Hs::Type result_type;
        tcRho(t, Infer(result_type));
        return result_type;
    }

    void tcRho(expression_ref& exp, const Expected& E);

    Hs::Expression tcRho(const Hs::Var&, const Expected& E);

    void tcRho(const Hs::Con& con, const Expected& E);

    void tcRho(Hs::ApplyExp& app_exp, const Expected& E);

    void tcRho(Hs::LambdaExp&, const Expected& E);

    void tcRho(Hs::LetExp&, const Expected&);

    Hs::Expression tcRho(const Hs::TypedExp& TExp, const Expected&);

    void  tcRho(Hs::CaseExp& Case, const Expected&);

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

    void tcRho(Hs::MRule&, const Expected&, int i=0);

    void tcRho(Hs::Match&, const Expected&);

    // Figures 13, 14, 15?
    Hs::Decls
    infer_type_for_decls(const signature_env&, const Hs::Decls&, bool is_top_level);

    std::tuple<expression_ref, ID, Hs::Type>
    infer_type_for_single_fundecl_with_sig(Hs::FunDecl FD);

    Hs::Decls
    infer_type_for_decls_groups(const signature_env&, Hs::Decls E, bool is_top_level);

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

    // FIXME: this should be  const
    std::optional<Hs::Binds>
    entails_by_superclass(const std::pair<Hs::Var, Hs::Type>& to_keep,
                          const std::pair<Hs::Var, Hs::Type>& to_remove);

    template <typename T>
    std::optional<Hs::Binds>
    entails(const T& to_keep, const std::pair<ID, Hs::Type>& to_check);

    template <typename T>
    std::optional<Hs::Binds>
    entails(const T& to_keep, const std::pair<Hs::Var, Hs::Type>& to_check);

    std::pair<std::optional<Hs::Binds>, local_instance_env>
    entails(const local_instance_env& lie1, const local_instance_env& lie2);

    std::pair<std::optional<Hs::Binds>, LIE>
    entails(const LIE& lie1, const LIE& lie2);

    std::optional<std::pair<Hs::Expression,LIE>> lookup_instance(const Hs::Type& constraint);

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
        auto [dfun_exp, wanteds] = *inst;

        Hs::Binds binds;
        // If we can get (dvar1 :: K1 a) and (dvar2 :: K2 a) and a dfun so that dvar = dfun dvar1 dvar2
        for(auto& [dvar,constraint]: wanteds)
        {
            if (auto cbinds = entails(to_keep, {unloc(dvar.name),constraint}))
                ranges::insert(binds, binds.begin(), *cbinds);
            else
                return {};
        }

        Hs::Decls decls;
        Hs::Var dvar( {noloc, to_remove.first} );
        decls.push_back( Hs::simple_decl( dvar, dfun_exp) );
        binds.push_back( decls );

        return binds;
    }

    return {};
}

template <typename T>
std::optional<Hs::Binds> typechecker_state::entails(const T& to_keep, const std::pair<Hs::Var, Hs::Type>& to_remove)
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
        auto [dfun_exp, wanteds] = *inst;

        Hs::Binds binds;
        // If we can get (dvar1 :: K1 a) and (dvar2 :: K2 a) and a dfun so that dvar = dfun dvar1 dvar2
        for(auto& [dvar, constraint]: wanteds)
        {
            if (auto cbinds = entails(to_keep, {dvar,constraint}))
                ranges::insert(binds, binds.begin(), *cbinds);
            else
                return {};
        }

        Hs::Decls decls;
        decls.push_back( Hs::simple_decl( to_remove.first, dfun_exp) );
        binds.push_back( decls );

        return binds;
    }

    return {};
}

local_instance_env unordered_lie(const std::vector<std::pair<Hs::Var, Hs::Type>>& lie1);

std::optional<ID> maybe_get_class_name_from_constraint(const Hs::Type& constraint);
ID get_class_name_from_constraint(const Hs::Type& constraint);

value_env apply_subst(const u_substitution_t& s, const value_env& env1);

std::map<ID, Hs::Match> get_instance_methods(const Hs::Binds& decls, const global_value_env& members, const std::string& class_name);

Hs::Var
rename_var_pattern_from_bindinfo(Hs::Var V, const std::map<ID, Hs::BindInfo>& bind_info);

Hs::Pattern
rename_pattern_from_bindinfo(const Hs::Pattern& pat, const std::map<ID, Hs::BindInfo>& bind_info);

std::vector<std::vector<expression_ref>> find_type_groups(const Hs::Decls& type_decls);

ID get_full_class_name_from_constraint(const Hs::Type& constraint);

std::vector<Hs::Type> constraints_from_lie(const local_instance_env& lie);

std::vector<Hs::Type> constraints_from_lie(const std::vector<std::pair<Hs::Var, Hs::Type>>& lie);

std::vector<Hs::Var> vars_from_lie(const local_instance_env& lie);

std::vector<Hs::Var> vars_from_lie(const std::vector<std::pair<Hs::Var, Hs::Type>>& lie);

template <typename T>
std::vector<T> vars_from_lie(const std::vector<std::pair<Hs::Var, Hs::Type>>& lie)
{
    std::vector<T> dvars;
    for(auto& [dvar, _]: lie)
        dvars.push_back(dvar);
    return dvars;
}
#endif
