#ifndef RENAME_H
#define RENAME_H

#include <map>
#include <set>
#include <string>
#include "computation/expression/expression_ref.H"
#include "computation/haskell/haskell.H"
#include "computation/optimization/simplifier_options.H"       // for simplifier options
#include "computation/message.H"

class Module;

Hs::LPat unapply(Hs::LExp E);

typedef std::set<std::string> bound_var_info;
typedef std::set<std::string> bound_type_var_info;

Located<expression_ref> desugar_infix(const Module& m, const std::vector<Located<expression_ref>>& T);
Hs::LExp rename_infix(const Module& m, Hs::LExp E);
Hs::MultiGuardedRHS rename_infix(const Module& m, Hs::MultiGuardedRHS R);
Hs::Binds rename_infix(const Module& p, Hs::Binds);
Hs::ModuleDecls rename_infix(const Module& m, Hs::ModuleDecls);

expression_ref rename_infix_decl(const Module& m, const expression_ref& E);

bool disjoint_add(bound_var_info& bv1, const bound_var_info& bv2);

struct renamer_state
{
    const Module& m;

    mutable std::vector<Message> messages;

    void error(const Note& e) const;
    void error(const std::optional<yy::location>& loc, const Note& e) const;
    void warning(const Note& e) const;
    void warning(const std::optional<yy::location>& loc, const Note& e) const;

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
    bound_var_info find_bound_vars_in_stmt(const Located<expression_ref>& stmt);

    // these all assume decls have been translated to FunDecl or PatDecl
    bound_var_info find_bound_vars_in_funpatdecl(const expression_ref& decl, bool top = false);
    bound_var_info find_bound_vars_in_decls(const Hs::Decls& decls, bool top = false);
    bound_var_info find_bound_vars_in_decls(const Hs::Binds& decls, bool top = false);

    bound_var_info find_bound_vars_in_decl(const Hs::TypeSigDecl& decl, bool top = false);

    std::vector<std::vector<int>> rename_grouped_decls(Hs::Decls& decls, const bound_var_info& bound, std::set<std::string>& free_vars, bool top = false);
    void rename_signatures(std::map<Hs::LVar, Hs::LType>& signatures, const bound_var_info& bound, bool top = false);
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
    Hs::ClassDecl rename(Hs::ClassDecl);
    Hs::TypeSynonymDecl rename(Hs::TypeSynonymDecl);
    Hs::DataDefn rename(Hs::DataDefn, const std::vector<Hs::LTypeVar>&);
    Hs::DataOrNewtypeDecl rename(Hs::DataOrNewtypeDecl);
    Hs::FamilyDecl rename(Hs::FamilyDecl);
    Hs::DataFamilyInstanceDecl rename(Hs::DataFamilyInstanceDecl);
    Hs::TypeFamilyInstanceDecl rename(Hs::TypeFamilyInstanceDecl);
    Hs::TypeFamilyInstanceEqn rename(Hs::TypeFamilyInstanceEqn);
    Hs::KindSigDecl rename(Hs::KindSigDecl);
    Hs::LTypeCon rename_type(Hs::LTypeCon);
    Hs::LType rename_type(Hs::LType);
    Hs::LType rename_and_quantify_type(Hs::LType, const std::vector<Hs::LTypeVar>& = {});

    std::vector<Located<Hs::DefaultDecl>> rename_default_decls(std::vector<Located<Hs::DefaultDecl>>);

    Hs::DefaultDecl rename(Hs::DefaultDecl);

    Hs::Exp rename(const Hs::Exp& E, const bound_var_info& bound, std::set<std::string>& free_vars);
    Hs::LExp rename(Hs::LExp E, const bound_var_info& bound, std::set<std::string>& free_vars);

    std::pair<Hs::LExp,std::set<std::string>> rename(const Hs::LExp& E, const bound_var_info& bound);
    Hs::LExp rename(const Hs::LExp& E, const bound_var_info& bound, const bound_var_info& binders, std::set<std::string>& free_vars);

    renamer_state(const Module& m_):m(m_) {}
};

Hs::Decls synthesize_field_accessors(const Hs::Decls& decls);

Hs::Decls group_fundecls(const Hs::Decls& decls); // only value decls

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
