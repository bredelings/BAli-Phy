#ifndef TYPECHECK_H
#define TYPECHECK_H

#include <string>
#include <vector>
#include <map>
#include <bitset>
#include "immer/map.hpp" // for immer::map
#include "kindcheck.H"
#include "env.H"
#include "substitution.H"
#include <range/v3/all.hpp>
#include "util/cow-ptr.H"

#include "computation/haskell/haskell.H"
#include "computation/message.H"
#include "types.H"
#include "expected.H"
#include "context.H"
#include "constraints.H"
#include "tidy.H"

struct unification_env
{
    mutable int next_index = 0;
    immer::map<TypeVar, TypeVar> mapping1;
    immer::map<TypeVar, TypeVar> mapping2;
    TypeVar fresh_tyvar(const Kind& k) const;
};

Hs::ModuleDecls typecheck(const std::string& s, Module& m, Hs::ModuleDecls M);

Type type_check_class_method_type(kindchecker_state& K, Type type, const Type& constraint);

std::pair<Hs::LType,bool> pop_strictness(Hs::LType ltype);
std::pair<Hs::LType,std::vector<bool>> pop_constructor_signature_strictness(Hs::LType ltype);

std::tuple<global_value_env, GIE, ClassInfo, Hs::Decls>
type_check_type_class(Module& m, const Haskell::ClassDecl& class_decl);

global_value_env sig_env(const signature_env& signatures);

typedef std::map<TypeCon,Kind> KindSigEnv;

// Can the instances be something like (Log Double)?  I suppose they could.
typedef std::map<TypeCon,std::vector<Type>> DefaultEnv;

struct FamilyInstanceHead
{
    std::vector<TypeVar> type_vars;
    Context context;
    Type type;
    std::vector<Type> args;
};

struct TypeFamilyInstanceCheck
{
    FamilyInstanceHead head;
    Type rhs;
};

enum class Apartness
{
    Unifiable,
    SurelyApart,
    MaybeApart
};

struct global_tc_state
{
    Module& this_mod; // for name lookups like Bool, Num, etc.

    KindSigEnv kind_sigs;

    DefaultEnv default_env;

    std::optional<int> unification_level;

    global_tc_state(Module& m);
};

constexpr int occurs_definitely_bit = 0;
constexpr int occurs_maybe_bit = 1;
constexpr int impredicative_bit = 2;
constexpr int type_family_bit = 3;
constexpr std::bitset<8> ok_result;
constexpr std::bitset<8> occurs_definitely_result(1);
constexpr std::bitset<8> occurs_maybe_result(2);
constexpr std::bitset<8> impredicative_result(4);
constexpr std::bitset<8> type_family_result(8);

bool has_occurs_check(std::bitset<8> result);

class TypeChecker;

template <typename... Args>
using tc_action = std::function<void(Args...,TypeChecker&)>;

template <typename R, typename... Args>
using tc_function = std::function<R (Args...,TypeChecker&)>;

class TypeChecker: FreshVarSource
{
    //------- global state ------------//
    std::shared_ptr<global_tc_state> global_state;

    //------- local state ------------//
    cow_ptr<TypeCheckerContext> local_state;

public:
          Module& this_mod()        {return global_state->this_mod;}
    const Module& this_mod() const {return global_state->this_mod;}

          KindSigEnv& kind_sigs()       {return global_state->kind_sigs;}
    const KindSigEnv& kind_sigs() const {return global_state->kind_sigs;}

    const ClassInfo* info_for_class(const std::string& cname) const;

    const TypeSynonymInfo* info_for_type_synonym(const std::string& sname) const;

    const TypeFamInfo* info_for_type_fam(const std::string& fname) const;
    const DataFamInfo* info_for_data_fam(const std::string& fname) const;

          DefaultEnv& default_env()       {return global_state->default_env;}
    const DefaultEnv& default_env() const {return global_state->default_env;}

          std::vector<Message>& messages()       {return this_mod().messages;}
    const std::vector<Message>& messages() const {return this_mod().messages;}

    int num_errors();

    void record_error(cow_ptr<TypeCheckerContext> context, const Notes& notes);
    void record_error(cow_ptr<TypeCheckerContext> context, const Note& e);
    void record_error(const std::optional<yy::location>& loc, const Note& e);
    void record_error(const Note& e);

    void record_warning(cow_ptr<TypeCheckerContext> context, const Notes& notes);
    void record_warning(cow_ptr<TypeCheckerContext> context, const Note& e);
    void record_warning(const std::optional<yy::location>& loc, const Note& e);
    void record_warning(const Note& e);

    std::optional<int> unification_level() const;

    void set_unification_level(int l);

    void clear_unification_level();

    Type expTypeToType(const Expected& E);

    Type inferResultToType(Infer& I);

    Expected newInfer();

    void fillInfer(const Type& type, Infer& I);

    Type promote_type(int dest_level, const Type& type);

    void set_expected_type(const Expected& E, const Type& type);

    void ensure_monotype(const Type& type);

    //------- local state ------------//


    WantedConstraints collected_wanteds;

    void pop_note() {local_state.modify()->pop_note();}
    void push_note(const Note& n) {local_state.modify()->push_note(n);}
    std::string print_note() const {return local_state->print_note();}

    class NoteScope
    {
        TypeChecker& type_checker;
        bool active = true;

    public:
        NoteScope(TypeChecker& tc, const Note& n):type_checker(tc) {type_checker.push_note(n);}
        ~NoteScope() noexcept { if (active) type_checker.pop_note(); }

        NoteScope(const NoteScope&) = delete;
        NoteScope& operator=(const NoteScope&) = delete;

        NoteScope(NoteScope&& other) noexcept
            :type_checker(other.type_checker),
             active(other.active)
        {
            other.active = false;
        }

        NoteScope& operator=(NoteScope&&) = delete;
    };

    [[nodiscard]] NoteScope note_scope(const Note& n) {return NoteScope(*this, n);}

    std::optional<yy::location> source_span() const {return local_state->source_span();}
    void push_source_span(const yy::location& loc) {local_state.modify()->push_source_span(loc);}
    void pop_source_span() {local_state.modify()->pop_source_span();}

    class SourceSpanScope
    {
        TypeChecker& type_checker;
        bool active = false;

    public:
        SourceSpanScope(TypeChecker& tc, const yy::location& loc)
            :type_checker(tc),
             active(true)
        {
            type_checker.push_source_span(loc);
        }

        SourceSpanScope(TypeChecker& tc, const std::optional<yy::location>& loc)
            :type_checker(tc),
             active(loc.has_value())
        {
            if (active) type_checker.push_source_span(*loc);
        }

        ~SourceSpanScope() noexcept { if (active) type_checker.pop_source_span(); }

        SourceSpanScope(const SourceSpanScope&) = delete;
        SourceSpanScope& operator=(const SourceSpanScope&) = delete;

        SourceSpanScope(SourceSpanScope&& other) noexcept
            :type_checker(other.type_checker),
             active(other.active)
        {
            other.active = false;
        }

        SourceSpanScope& operator=(SourceSpanScope&&) = delete;
    };

    [[nodiscard]] SourceSpanScope source_span_scope(const yy::location& loc) {return SourceSpanScope(*this, loc);}
    [[nodiscard]] SourceSpanScope source_span_scope(const std::optional<yy::location>& loc) {return SourceSpanScope(*this, loc);}

    void push_binder(const binder_info& binder) { local_state.modify()->push_binder(binder);}
    void pop_binder() {local_state.modify()->pop_binder();}

    int level() const {return local_state->level;}
    void inc_level() {local_state.modify()->level++;}
    void dec_level() {local_state.modify()->level--;}
    void set_level(int l) {local_state.modify()->level = l;}


          mono_env_t& mono_local_env()       {return local_state.modify()->mono_env;}
          global_value_env& poly_env()       {return local_state.modify()->poly_env;}

    const mono_env_t& mono_local_env() const {return local_state->mono_env;}
    const global_value_env& poly_env() const {return local_state->poly_env;}

    const cow_ptr<TypeCheckerContext>& context() const {return local_state;}

    // If we could cache this info on the TypeCon, then we could move these out of the class.
    bool type_con_is_type_fam(const TypeCon& tc) const;
    bool type_con_is_data_fam(const TypeCon& tc) const;
    bool type_con_is_type_syn(const TypeCon& tc) const;
    bool type_con_is_type_class(const TypeCon& tc) const;
    bool type_con_must_be_saturated(const TypeCon& tc) const;
    int type_con_arity(const TypeCon& tc) const;
    std::optional<std::tuple<Type, Type>> is_type_app(Type t) const;
    std::optional<std::tuple<TypeCon,std::vector<Type>>> is_type_fam_app(const Type& t) const;
    std::optional<std::tuple<TypeCon,std::vector<Type>>> is_data_fam_app(const Type& t) const;
    std::optional<std::tuple<TypeCon,std::vector<Type>>> is_type_class_app(const Type& t) const;

    bool same_type(const Type& t1, const Type& t2) const;
    bool same_type_no_syns(const Type& t1, const Type& t2) const;

    bool same_type(bool keep_syns, const Type& t1, const Type& t2) const;
    bool same_type(bool keep_syns, const RenameTyvarEnv2& env, const Type& t1, const Type& t2) const;
    bool same_types(bool keep_syns, const std::vector<Type>& ts1, const std::vector<Type>& ts2) const;
    bool same_types(bool keep_syns, const RenameTyvarEnv2& env, const std::vector<Type>& ts1, const std::vector<Type>& ts2) const;

    bool has_errors() const;
    myexception note_exception() const;

    //-------
    MetaTypeVar fresh_meta_type_var(const std::string& name, const Kind& k);
    MetaTypeVar fresh_meta_type_var(const Kind& k);

    TypeVar fresh_rigid_type_var(const std::string& name, const Kind& k);
    TypeVar fresh_rigid_type_var(const Kind& k);

    //------- Functions handling the LIE ---------
    TypeChecker copy_clear_wanteds(bool bump_level = false) const;

    void promote_mtv(const MetaTypeVar& mtv, int new_level);
    bool maybe_promote_mtv(const MetaTypeVar& mtv, int new_level);
    void promote(Type t, int new_level);

    void add_binders(const local_value_env& binders);

    TypeChecker copy_add_binders(const global_value_env& env) const;

    WantedConstraints& current_wanteds();

    Core::Var<> fresh_dvar(const Type& constraint, bool qualified = false);

    Core::Var<> add_wanted(const ConstraintOrigin& origin, const Type& constraint);

    // ------ End: Functions handling the LIE ---

    void get_kind_sigs(const Hs::Decls& type_decls);

    void get_tycon_info(const Hs::FamilyDecl&);
    void get_tycon_info(const Hs::Decls& type_decls);

    Hs::Decls add_type_var_kinds(Hs::Decls type_decls);

    void get_constructor_info(const Hs::Decls& decls);

    const TypeSynonymInfo* maybe_find_type_synonym(const Type& type) const;

    std::optional<Type> expand_type_synonym(const Type& type) const;

    Type expand_all_type_synonyms(Type type) const;

    Type check_type(const Hs::LType&);

    Type check_type(const Hs::LType&, kindchecker_state& K);

    Type check_constraint(const Hs::LType&);

    TypeFamilyInstanceCheck check_type_instance(const Hs::LType&, const Hs::LType&);

    Hs::Var find_prelude_var(std::string name) const;

    ID find_prelude_tycon_name(const std::string& name) const;

    TypeCon find_prelude_tycon(const std::string& name) const;

    std::optional<ID> find_tycon_name(const std::string& name) const;

    std::optional<TypeCon> find_tycon(const std::string& name) const;

    Type bool_type() const;

    Type rational_type() const;

    bool add_substitution(const MetaTypeVar& a, const Type& type);

    void unify_defer(const ConstraintOrigin& origin, const Type& t1, const Type& t2);

    void unify_solve_(const ConstraintOrigin& origin, const Type& t1, const Type& t2);

    // calls unify_solve_ with UnifyOrigin(t1,t2)
    void unify(const Type& t1, const Type& t2);

    // calls unify_solve_
    void unify(const Type& t1, const Type& t2, const ConstraintOrigin& orig);

    bool try_insert(const MetaTypeVar& tv, Type type) const;

    bool occurs_check(const MetaTypeVar& tv, const Type& t) const;

    bool occurs_check(const TypeVar& tv, const Type& t) const;

    std::pair<Type, Type> unify_function(const Type& t);

    std::pair<Type, Type> unify_function(const Type& t, const ConstraintOrigin& o);

    std::tuple<Type, Type, Type> unify_two_arg_function(const Type& t);

    // The pure unifier
    bool maybe_unify_(bool both_ways, const unification_env& env, const Type& t1, const Type& t2, bsubstitution_t& s) const;

    bool maybe_unify_var_(bool both_ways, const unification_env& env, const std::variant<TypeVar,MetaTypeVar>& btv1, const Type& t2, bsubstitution_t& s) const;

    std::optional<bsubstitution_t> maybe_unify(const Type& t1, const Type& t2) const;

    std::optional<bsubstitution_t> maybe_match(const Type& t1, const Type& t2) const;

    bsubstitution_t match(const Type& t1, const Type& t2, const myexception& e) const;

    bsubstitution_t match(const Type& t1, const Type& t2) const;

    Apartness apartness(const Type& t1, const Type& t2) const;
    // end pure unifier.
    
    DataConInfo constructor_info(const Hs::Con&);

    bool candidates(const MetaTypeVar& tv, const LIE& tv_lie);

    bool default_preds( WantedConstraints& wanteds );

    Core::Decls<> simplify_and_default_top_level();

    std::bitset<8> check_type_equality(const Type& lhs, const Type& rhs) const;

    Type look_thru(const Type& t) const;

    Notes check_eq_tv_constraint(TidyState&, std::vector<std::shared_ptr<Implication>>& implic_scopes, const Constraint& wanted, const Type& t1, const Type& t2) const;
    Notes check_eq_constraint(TidyState&, std::vector<std::shared_ptr<Implication>>& implic_scopes, const Constraint& wanted, const Type& t1, const Type& t2) const;
    void check_wanteds(TidyState&, std::vector<std::shared_ptr<Implication>>& implic_scopes, const WantedConstraints& wanteds);
    void check_wanteds(const WantedConstraints& wanteds);

    substitution_t get_subst_for_tv_binders(const std::vector<TypeVar>& type_var_binders);
    substitution_t fresh_tv_binders(std::vector<TypeVar>& type_var_binders);
    InstanceInfo freshen(InstanceInfo);
    EqInstanceInfo freshen(EqInstanceInfo);

    std::tuple<std::vector<MetaTypeVar>, LIE, Type> instantiate(const ConstraintOrigin& origin, const Type& t);

    std::tuple<Core::wrapper, Type> instantiate_emit(const ConstraintOrigin& origin, const Type& t);

    Core::wrapper subsumptionCheck(const ConstraintOrigin& origin, const Type& t1, const Expected& e2);

    Core::wrapper subsumptionCheck(const ConstraintOrigin& origin, const Type& t1, const Type& t2);

    Core::wrapper instantiateSigma(const ConstraintOrigin& origin, const Type& t, const Expected& exp_rho);

    std::tuple<Core::wrapper, std::vector<TypeVar>, LIE, Type> skolemize(const Type& t, bool skolem);

    std::tuple<Core::wrapper, std::vector<TypeVar>, LIE, Type> skolemize_and(const Type& t, const tc_action<Type>& nested_action);

    template <typename R>
    std::tuple<Core::wrapper, std::vector<TypeVar>, LIE, Type, R> skolemize_and_result(const Type& t, const tc_function<R, Type>& nested_action);

    void tcRhoStmts(int i, std::vector<Located<Hs::Qual>>& stmts, const Expected& exp_type);

    std::shared_ptr<const Core::Decls<>> maybe_implication(const std::vector<TypeVar>& tvs, const LIE& givens, const tc_action<>& nested_action);

    template <typename R>
    std::tuple<std::shared_ptr<const Core::Decls<>>,R> maybe_implication_result(const std::vector<TypeVar>& tvs, const LIE& givens, const tc_function<R>& nested_action);

    // Figure 22.
    void infer_quals_type(std::vector<Located<Hs::Qual>>& quals);

    // Figure 22.
    void infer_qual_type(Located<Hs::Qual>& qual);

    void infer_guard_type(Located<Hs::Qual>& guard);

    // Figure 24.
    Type inferPat(local_value_env& penv, Hs::LPat& pat, const signature_env& sigs = {});

    void tcPats(local_value_env& penv, Hs::LPats& pat, const std::vector<Expected>&, const signature_env& sigs, const tc_action<local_value_env&>& a, int i=0);

    // takes and returns sigma types
    void tcPat(local_value_env& penv, Hs::LPat& pat, const Expected&, const signature_env& sigs, const tc_action<local_value_env&>& a);

    // takes and returns sigma types
    void checkPat(local_value_env& penv, Hs::LPat& pat, const SigmaType&, const signature_env& sigs = {});

    Type inferPat(local_value_env& penv, Hs::LVar& lv, const signature_env& sigs = {});

    void tcPat(local_value_env& penv, Hs::LVar& lv, const Expected&, const signature_env& sigs, const tc_action<local_value_env&>&);

    void checkPat(local_value_env& penv, Hs::LVar& lv, const SigmaType& exp_type, const signature_env& sigs);

    Core::wrapper instPatSigma(const SigmaType& sigma_type, const Expected& exp_sigma_type);

    std::tuple<Type, local_value_env>
    infer_lhs_type(Hs::LDecl& decl, const signature_env& sigs);

    void infer_rhs_type(Hs::LDecl& decl, const Expected& type);

    Core::wrapper checkSigma(Hs::LExp& e, const SigmaType& sigma_type);

    template <typename T>
    void checkRho(T& t, const Type& exp_rho)
    {
        tcRho(t, Check(exp_rho));
    }

    template <typename T>
    Type inferRho(T& t)
    {
        Expected result_type = newInfer();
        tcRho(t, result_type);
        return result_type.read_type();
    }

    void tcRho(Located<Hs::Expression>& exp, const Expected& E);

    // Eliminate?
    void tcRho_(Hs::Expression& exp, const Expected& E);

    void tcRho(Hs::Var&, const Expected& E);

    void tcRho(Hs::Con& con, const Expected& E);

    void tcRho(Hs::ApplyExp& app_exp, const Expected& E);

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

    void tcMatch(const Hs::MatchContext& ctx, Hs::MRule& m, const std::vector<Expected>& pat_types, const Expected& result_type);

    void tcMatches(const Hs::MatchContext& ctx, Hs::Matches& m, const std::vector<Expected>& pat_types, const Expected& result_type);

    template <typename A>
    Core::wrapper tcMatchesFunInfer(std::vector<Expected>& arg_types, int arity, const Expected& fun_type, const A& a);

    template <typename A>
    Core::wrapper tcMatchesFunCheck(std::vector<Expected>& arg_types, int arity, Type type, const A& a);

    template <typename A>
    Core::wrapper tcMatchesFun(int arity, const Expected&, const A& a);

    // Figures 13, 14, 15?
    Hs::Decls
    infer_type_for_decls(const signature_env&, const Hs::Decls&, bool is_top_level);

    Hs::GenBind
    infer_type_for_single_fundecl_with_sig(Hs::FunDecl FD, const Type& ltype);

    Hs::BindInfo compute_bind_info(const Hs::Var& poly_id, const Hs::Var& mono_id,
                                   const std::set<TypeVar>& qtvs,
                                   const Type& monotype, const signature_env& signatures,
                                   const LIE& lie_retained);

    std::set<MetaTypeVar> find_fixed_tvs(bool restricted, int level, const std::vector<Type>& preds, const std::set<MetaTypeVar>& tvs) const;
    std::set<MetaTypeVar> injective_vars_for_type(const Type& type) const;

    bool is_quantifiable_pred(const Type& pred, const std::set<TypeVar>& qtvs) const;

    std::vector<Type> get_quantifiable_preds(bool restricted, const std::vector<Type>& preds, const std::set<TypeVar>& qtvs) const;

    std::tuple<std::set<TypeVar>, LIE, Core::Decls<>> simplify_and_quantify(bool restricted, WantedConstraints& wanteds, const value_env& mono_binder_env);

    Hs::Decls
    infer_type_for_decls_group(const signature_env&, Hs::Decls E, bool is_top_level);

    std::tuple< std::map<Hs::Var, Hs::Var>, local_value_env >
    fd_mono_nonrec(Hs::FunDecl&);

    std::tuple< std::map<Hs::Var, Hs::Var>, local_value_env >
    pd_mono_nonrec(Hs::PatDecl&);

    std::tuple< std::map<Hs::Var, Hs::Var>, local_value_env >
    tc_decls_group_mono(const signature_env& sigs, Hs::Decls& decls);

    // Figures 13, 14, 15?
    Hs::Binds infer_type_for_binds_top(Hs::Binds binds);

    void infer_type_for_binds(Hs::LBinds& binds, bool is_top_level = false);

    void get_type_synonyms(const Hs::Decls& decls);

    void get_type_families(const Hs::Decls& decls);

    Core::Decls<> infer_type_for_classes(const Hs::Decls& decls);

    std::tuple<ClassInfo,Core::Decls<>>
    infer_type_for_class(const Hs::ClassDecl& class_decl);

    DataConInfo infer_type_for_constructor(const Hs::LTypeCon& con, const std::vector<Hs::LTypeVar>& tvs, const Hs::ConstructorDecl& constructor);

    DataConEnv infer_type_for_data_type(const Hs::DataOrNewtypeDecl& data_decl);

    DataConInfo infer_type_for_data_family_constructor(const Hs::LType& result_type, const std::vector<Hs::LTypeVar>& outer_tvs, const std::vector<Type>& top_constraints, const Hs::ConstructorDecl& constructor);

    DataConInfo infer_type_for_gadt_data_family_constructor(const Type& instance_head, const TypeCon& data_family, const std::vector<Type>& top_constraints, const Hs::GADTConstructorDecl& constructor, bool is_newtype);

    std::vector<Role> infer_roles_for_data_type(const std::vector<TypeVar>& data_tvs, const DataConEnv& constructors) const;

    std::pair<DataConEnv,std::optional<Type>> infer_type_for_data_family_instance(const Hs::DataFamilyInstanceDecl& data_inst, const std::optional<std::string>& associated_class = {});

    Hs::Decls infer_type_for_default_methods(const Hs::ClassDecl&);

    Hs::Binds infer_type_for_default_methods(const Hs::Decls&);

    // Figure 12
    std::optional<Core::Var<>>
    infer_type_for_instance1(const Hs::InstanceDecl& instance_decl);

    // Figure 12
    std::vector< std::pair<Core::Var<>, Hs::InstanceDecl> >
    infer_type_for_instances1(const Hs::Decls& decls);

    Hs::Decls synthesize_derived_instances(const Hs::Decls& decls);

    std::optional<Type> find_missing_derived_constraint(const Type& pred, const std::set<TypeVar>& data_tvs, std::vector<Type>& active);

    void check_derived_instances(const Hs::Decls& decls);

    // Figure 12
    std::pair<Hs::Binds, Core::Decls<>>
    infer_type_for_instances2(const std::vector<std::pair<Core::Var<>, Hs::InstanceDecl>>& named_instances);

    // Figure 12
    std::pair<Hs::Decls, Core::Decl<>>
    infer_type_for_instance2(const Core::Var<>& dfun, const Hs::InstanceDecl& instance_decl);

    std::tuple<Hs::Decl,Core::Var<>> type_check_superclass_entry(
        const Constraint& superclass_constraint, const std::string& class_name,
        const std::vector<TypeVar>& instance_tvs, const LIE& givens, const substitution_t& subst,
        const std::vector<Core::Var<>>& instance_dict_args, std::shared_ptr<const Core::Decls<>>& decls_super);

    std::tuple<Hs::Decl, Core::Var<>>
    type_check_instance_method(const Hs::Var& method, const Type& method_type, const std::vector<TypeVar>& instance_tvs, const LIE& givens, const Type& inst_type, const substitution_t& subst, const std::map<Hs::Var,Hs::Matches>& method_matches, const ClassInfo& class_info);

    std::map<Hs::Var, Hs::Matches> get_instance_methods(const Hs::Decls& decls, const global_value_env& members, const std::string& class_name);

    void add_type_instance(const std::vector<TypeVar>& free_tvs, const Type& lhs, const Type& rhs);

    bool check_family_instance_association(const Hs::LTypeCon& family_con,
					   const std::optional<std::string>& family_associated_class,
					   const std::optional<std::string>& instance_associated_class,
					   const std::string& instance_name,
					   const std::string& family_name,
					   bool require_associated_family,
					   bool indent_message);

    FamilyInstanceHead check_family_instance_head(const Hs::LTypeCon& family_con,
						  const std::vector<Hs::LType>& args,
						  const std::optional<std::vector<Hs::LTypeVar>>& forall,
						  const Hs::Context& context);

    void check_associated_family_instance_args(const std::vector<Hs::LType>& hs_args,
					       const std::vector<Type>& args,
					       const std::vector<TypeVar>& family_args,
					       const substitution_t& instance_subst);

    void check_add_type_instance(const Hs::TypeFamilyInstanceEqn& inst, const std::optional<std::string>& associated_class, const substitution_t& instance_subst);

    void check_data_instance(const Hs::DataFamilyInstanceDecl& inst, const std::optional<std::string>& associated_class, const substitution_t& instance_subst);

    void default_type_instance(const TypeCon& tf_con,
			       const std::optional<TypeFamilyInstanceDecl>& maybe_default,
			       const substitution_t& instance_subst);

    // Figure 26
    // Express lie2 in terms of gie (functions) and lie1 (arguments to this dfun, I think).
    LIE preds_to_constraints(const ConstraintOrigin&, ConstraintFlavor, const std::vector<Type>&);

    std::vector<std::pair<Core::Var<>, Type>> superclass_constraints(const Type& constraint);

    std::optional<std::vector<Core::Var<>>>
    is_superclass_of(const Type&, const Type&);

    std::optional<Core::Decls<>>
    entails_by_superclass(const Constraint& to_keep, const Constraint& to_remove);

    Core::Decls<> simplify(const LIE& givens, LIE& wanteds);

    Core::Decls<> entails(const LIE& lie1, WantedConstraints& lie2);

    bool more_specific_than(const Type&, const Type&);
    std::optional<std::pair<Core::Exp<>,LIE>> lookup_instance(const Type& constraint);

    bool find_type_eq_instance_1way(const Type& t1, const Type& t2);
    bool find_type_eq_instance(const Type& t1, const Type& t2);
    std::optional<Type> unwrap_newtype_type(const Type&) const;

    void get_defaults(const Hs::ModuleDecls& M);

    typechecker_result typecheck_module( Hs::ModuleDecls M );

    TypeChecker(Module& m);
};

std::optional<ID> maybe_get_class_name_from_constraint(const Type& constraint);
ID get_class_name_from_constraint(const Type& constraint);

LIE apply_subst(const substitution_t& s, const LIE& env1);

std::map<Hs::Var, Hs::Matches> get_instance_methods(const Hs::Decls& decls, const global_value_env& members, const std::string& class_name);

Hs::LVar rename_var_from_bindinfo(Hs::LVar V, const std::map<Hs::Var, Hs::BindInfo>& bind_info);

Hs::LPat rename_pattern_from_bindinfo(Hs::LPat pat, const std::map<Hs::Var, Hs::BindInfo>& bind_info);

std::vector<std::vector<expression_ref>> find_type_groups(const Hs::Decls& type_decls);

ID get_full_class_name_from_constraint(const Type& constraint);

bool constraint_is_hnf(const Type& constraint);

int getArity(const Hs::Matches& m);
int getArity(const Hs::MRule& m);

LIE float_wanteds(bool float_past_equalities, const WantedConstraints& wanteds, const std::set<TypeVar>& trapping_tvs = {});

std::string class_arg_name(const Type& class_arg);

template <typename R>
std::tuple<Core::wrapper, std::vector<TypeVar>, LIE, Type, R>
TypeChecker::skolemize_and_result(const Type& polytype, const tc_function<R, Type>& nested_function)
{
    // 1. Skolemize the type at level
    inc_level();
    auto [wrap, tvs, givens, rho_type] = skolemize(polytype, true);
    dec_level();

    // 2. Perform the action, maybe creating an implication.
    // c++20 should allow us to capture rho-type, but clang is broken until probably clang-16.
    auto [ev_decls, result] = maybe_implication_result<R>(tvs, givens, [&,&rho_type=rho_type](auto& tc) -> R {return nested_function(rho_type, tc);});

    // 3. Combine the wrappers
    return {wrap * Core::WrapLet(ev_decls), tvs, givens, rho_type, result};
}

template <typename R>
std::tuple<std::shared_ptr<const Core::Decls<>>, R>
TypeChecker::maybe_implication_result(const std::vector<TypeVar>& tvs, const LIE& givens, const tc_function<R>& nested_function)
{
    auto ev_decls = std::make_shared<Core::Decls<>>();

    bool need_implication = not (tvs.empty() and givens.empty());

    auto tcs2 = copy_clear_wanteds(need_implication);

    auto result = nested_function(tcs2);

    auto wanteds = tcs2.current_wanteds();

    if (not wanteds.empty())
    {
        if (need_implication)
        {
            auto imp = std::make_shared<Implication>(level()+1, tvs, givens, wanteds, ev_decls, context());
            current_wanteds().implications.push_back( imp );
        }
        else
            current_wanteds() += wanteds;
    }

    return {ev_decls, result};
}

#endif
