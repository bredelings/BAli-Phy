#ifndef RENAME_H
#define RENAME_H

#include <map>
#include <memory>
#include <optional>
#include <set>
#include <string>
#include <tuple>
#include "computation/expression/expression_ref.H"
#include "computation/haskell/haskell.H"
#include "computation/symbols.H"
#include "computation/optimization/simplifier_options.H"       // for simplifier options
#include "computation/message.H"

class Module;
struct renamer_state;

Hs::LPat parsed_expression_to_pattern(Hs::LExp E);

typedef std::set<std::string> bound_var_info;
typedef std::set<std::string> bound_type_var_info;
typedef std::map<std::string, fixity_info> fixity_env_t;

struct record_constructor_layout
{
    int arity = 0;
    std::map<std::string,int> fields;
};

struct renamer_global_state
{
    mutable std::vector<Message> messages;
    std::map<std::string, record_constructor_layout> record_constructor_layouts;
    std::map<std::string, std::set<std::string>> record_field_constructors;
};

Located<expression_ref> desugar_infix(const renamer_state& Rn, const std::vector<Located<expression_ref>>& T);
Hs::LPat desugar_pattern_infix(const renamer_state& Rn, const std::vector<Located<expression_ref>>& T);
bool is_infix_operator_term(const std::vector<Hs::LExp>& terms, int index);
bool is_infix_operand_term(const std::vector<Hs::LExp>& terms, int index);
Hs::LExp make_infix_exp(const std::vector<Hs::LExp>& terms);
Hs::Binds classify_value_decls(Hs::Binds);
Hs::Decls classify_value_decls(Hs::Decls);

bool disjoint_add(bound_var_info& bv1, const bound_var_info& bv2);

struct renamer_state
{
    const Module& m;

    std::shared_ptr<renamer_global_state> global_state;
    fixity_env_t fixity_env;

    // Create a child renamer that shares global diagnostics/metadata but copies local fixity state.
    renamer_state child() const { return *this; }

    std::vector<Message>& messages() const { return global_state->messages; }
    std::map<std::string, record_constructor_layout>& record_constructor_layouts() const { return global_state->record_constructor_layouts; }
    std::map<std::string, std::set<std::string>>& record_field_constructors() const { return global_state->record_field_constructors; }

    void error(const Note& e) const;
    void error(const std::optional<yy::location>& loc, const Note& e) const;
    void warning(const Note& e) const;
    void warning(const std::optional<yy::location>& loc, const Note& e) const;

    void record_record_layouts(const Hs::Decls&);
    std::optional<record_constructor_layout> record_layout_for_constructor(const std::string&);
    OpInfo get_operator(const std::string&) const;
    fixity_env_t add_fixities_from_decls(fixity_env_t, const Hs::Decls&) const;

    void qualify_name(std::string& name) const;
    void qualify_name(Located<std::string>& name) const;
    void qualify_name(Hs::Var& v) const;
    void qualify_name(Hs::Con& v) const;
    void qualify_name(Hs::TypeCon& v) const;

    bound_var_info rename_patterns(Hs::LPats& pat, bool top = false);
    bound_var_info rename_var_pattern(Hs::LVar&, bool top = false);
    bound_var_info rename_pattern(Hs::LPat& pat, bool top = false);

    bound_var_info find_vars_in_patterns(const Hs::LPats& pats, bool top = false);
    bound_var_info find_vars_in_pattern(const Hs::LPat& pat, bool top = false);
    // the pattern*2 versions are for AFTER rename, and don't check things.  They just report what they find.
    bound_var_info find_bound_vars_in_stmt(Located<expression_ref>& stmt);

    // these all assume decls have been translated to FunDecl or PatDecl
    bound_var_info find_bound_vars_in_funpatdecl(const expression_ref& decl, bool top = false);
    bound_var_info find_bound_vars_in_decls(const Hs::Decls& decls, bool top = false);
    bound_var_info find_bound_vars_in_decls(const Hs::Binds& decls, bool top = false);

    bound_var_info find_bound_vars_in_decl(const Hs::TypeSigDecl& decl, bool top = false);

    std::vector<std::vector<int>> rename_grouped_decls(Hs::Decls& decls, const bound_var_info& bound, std::set<std::string>& free_vars, bool top = false);
    void rename_signatures(std::map<Hs::LVar, Hs::LType>& signatures, std::map<Hs::LVar, Hs::inline_pragma_t>& inline_sigs, const bound_var_info& bound, bool top = false);
    bound_var_info rename_decls(Hs::Binds& decls, const bound_var_info& bound, const bound_var_info& binders, std::set<std::string>& free_vars, bool top = false);
    bound_var_info rename_decls(Hs::Binds& decls, const bound_var_info& bound, std::set<std::string>& free_vars, bool top = false);
    bound_var_info rename_rec_stmt(Hs::LExp& stmt, const bound_var_info& bound, std::set<std::string>& free_vars);
    bound_var_info rename_stmt(Hs::LExp& stmt, const bound_var_info& bound, std::set<std::string>& free_vars);
    bound_var_info rename_stmt(Hs::LExp& stmt, const bound_var_info& bound, const bound_var_info& binders, std::set<std::string>& free_vars);
    Hs::MultiGuardedRHS rename(Hs::MultiGuardedRHS R, const bound_var_info& bound, const bound_var_info& binders, std::set<std::string>& free_vars);
    Hs::MultiGuardedRHS rename(Hs::MultiGuardedRHS R, const bound_var_info& bound, std::set<std::string>& free_vars);
    Hs::MRule rename(Hs::MRule match, const bound_var_info& bound, std::set<std::string>& free_vars);
    Hs::Matches rename(Hs::Matches match, const bound_var_info& bound, std::set<std::string>& free_vars);

    Hs::Decls rename_type_decls(Hs::Decls decls);
    Hs::InstanceDecl rename(Hs::InstanceDecl);
    Hs::StandaloneDerivingDecl rename(Hs::StandaloneDerivingDecl);
    Hs::RoleAnnotationDecl rename(Hs::RoleAnnotationDecl);
    Hs::ClassDecl rename(Hs::ClassDecl);
    Hs::TypeSynonymDecl rename(Hs::TypeSynonymDecl);
    Hs::DataDefn rename(Hs::DataDefn, const std::vector<Hs::LTypeVar>&);
    Hs::DataOrNewtypeDecl rename(Hs::DataOrNewtypeDecl);
    Hs::FamilyDecl rename(Hs::FamilyDecl);
    Hs::DataFamilyInstanceDecl rename(Hs::DataFamilyInstanceDecl);
    Hs::TypeFamilyInstanceDecl rename(Hs::TypeFamilyInstanceDecl);
    Hs::TypeFamilyInstanceEqn rename(Hs::TypeFamilyInstanceEqn);
    Hs::KindSigDecl rename(Hs::KindSigDecl);
    void check_newtype_decl(const Hs::DataDefn&, const Hs::LTypeCon&) const;
    Hs::LTypeCon rename_type(Hs::LTypeCon);
    Hs::LType rename_type(Hs::LType);
    Hs::LType rename_and_quantify_type(Hs::LType, const std::vector<Hs::LTypeVar>& = {});

    std::vector<Located<Hs::DefaultDecl>> rename_default_decls(std::vector<Located<Hs::DefaultDecl>>);

    Hs::DefaultDecl rename(Hs::DefaultDecl);

    Hs::Exp rename(const Hs::Exp& E, const bound_var_info& bound, std::set<std::string>& free_vars);
    Hs::LExp rename(Hs::LExp E, const bound_var_info& bound, std::set<std::string>& free_vars);

    std::pair<Hs::LExp,std::set<std::string>> rename(const Hs::LExp& E, const bound_var_info& bound);
    Hs::LExp rename(const Hs::LExp& E, const bound_var_info& bound, const bound_var_info& binders, std::set<std::string>& free_vars);

    renamer_state(const Module& m_):m(m_), global_state(std::make_shared<renamer_global_state>()) {}
};

Hs::Decls synthesize_field_accessors(const Hs::Decls& decls);

Hs::Decls group_fundecls(const Hs::Decls& decls); // only value decls
std::tuple<std::map<Hs::LVar,Hs::LType>, std::map<Hs::LVar, Hs::inline_pragma_t>, Hs::Decls> group_decls(const Hs::Decls& decls);
void remove_fixity_decls(Hs::Decls& decls);

// I think ghc returns a moduledecls AND a... class environment?
Hs::ModuleDecls rename(const simplifier_options&, const Module& p, Hs::ModuleDecls);
expression_ref rename(const Module& p, const expression_ref&);
expression_ref rename(const Module& p, const expression_ref&,const std::set<std::string>&);

typedef std::set<std::string> bound_var_info;
const bound_var_info& get_rhs_free_vars(const expression_ref& decl);
std::vector<Hs::Decls> split_decls(const Hs::Decls& decls, const std::vector< std::vector<int> >& referenced_decls);

std::tuple<std::map<std::string,int>, std::map<Hs::Var,std::vector<Hs::LVar>>>
get_indices_for_names(const Hs::Decls& decls);

void free_type_variables_(const std::vector<Hs::LType>& types, std::vector<Hs::LTypeVar>&);
void free_type_variables_(const Hs::LType& type, std::vector<Hs::LTypeVar>&);

std::vector<Hs::LTypeVar> free_type_variables(const std::vector<Hs::LType>& types);
std::vector<Hs::LTypeVar> free_type_variables(const Hs::LType& type);

#endif
