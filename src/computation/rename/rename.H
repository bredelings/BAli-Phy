#ifndef RENAME_H
#define RENAME_H

#include <map>
#include <set>
#include <string>
#include "computation/expression/expression_ref.H"
#include "computation/haskell/haskell.H"
#include "computation/optimization/simplifier.H"       // for simplifier options

class Module;

expression_ref unapply(expression_ref E);

typedef std::set<std::string> bound_var_info;
typedef std::set<std::string> bound_type_var_info;

expression_ref desugar_infix(const Module& m, const std::vector<expression_ref>& T);
expression_ref rename_infix(const Module& m, const expression_ref& E);
Hs::MultiGuardedRHS rename_infix(const Module& m, Hs::MultiGuardedRHS R);

bool disjoint_add(bound_var_info& bv1, const bound_var_info& bv2);

struct renamer_state
{
    const Module& m;

    bound_var_info rename_patterns(std::vector<expression_ref>& pat, bool top = false);
    bound_var_info rename_var_pattern(Hs::Var&, bool top = false);
    bound_var_info rename_pattern(expression_ref& pat, bool top = false);

    bound_var_info find_vars_in_patterns(const std::vector<expression_ref>& pats, bool top = false);
    bound_var_info find_vars_in_pattern(const expression_ref& pat, bool top = false);
    // the pattern*2 versions are for AFTER rename, and don't check things.  They just report what they find.
    bound_var_info find_bound_vars_in_stmt(const expression_ref& stmt);

    // these all assume decls have been translated to FunDecl or PatDecl
    bound_var_info find_bound_vars_in_funpatdecl(const expression_ref& decl, bool top = false);
    bound_var_info find_bound_vars_in_decls(const Haskell::Decls& decls, bool top = false);
    bound_var_info find_bound_vars_in_decls(const Haskell::Binds& decls, bool top = false);

    bound_var_info find_bound_vars_in_decl(const Haskell::SignatureDecl& decl, bool top = false);

    std::vector<std::vector<int>> rename_grouped_decls(Haskell::Decls& decls, const bound_var_info& bound, std::set<std::string>& free_vars, bool top = false);
    bound_var_info rename_signatures(std::map<std::string, Hs::Type>& signatures, bool top = false);
    bound_var_info rename_decls(Haskell::Binds& decls, const bound_var_info& bound, const bound_var_info& binders, std::set<std::string>& free_vars, bool top = false);
    bound_var_info rename_decls(Haskell::Binds& decls, const bound_var_info& bound, std::set<std::string>& free_vars, bool top = false);
    bound_var_info rename_rec_stmt(expression_ref& stmt, const bound_var_info& bound, std::set<std::string>& free_vars);
    bound_var_info rename_stmt(expression_ref& stmt, const bound_var_info& bound, std::set<std::string>& free_vars);
    bound_var_info rename_stmt(expression_ref& stmt, const bound_var_info& bound, const bound_var_info& binders, std::set<std::string>& free_vars);
    Hs::MultiGuardedRHS rename(Hs::MultiGuardedRHS R, const bound_var_info& bound, const bound_var_info& binders, std::set<std::string>& free_vars);
    Hs::MultiGuardedRHS rename(Hs::MultiGuardedRHS R, const bound_var_info& bound, std::set<std::string>& free_vars);
    Hs::MRule rename(Hs::MRule match, const bound_var_info& bound, std::set<std::string>& free_vars);
    Hs::Matches rename(Hs::Matches match, const bound_var_info& bound, std::set<std::string>& free_vars);

    Haskell::Decls rename_type_decls(Haskell::Decls decls);
    Haskell::InstanceDecl rename(Haskell::InstanceDecl);
    Haskell::ClassDecl rename(Haskell::ClassDecl);
    Haskell::TypeSynonymDecl rename(Haskell::TypeSynonymDecl);
    Haskell::DataOrNewtypeDecl rename(Haskell::DataOrNewtypeDecl);
    Haskell::Type rename_type(const Haskell::Type&);
    Haskell::Context rename(Haskell::Context);

    std::pair<expression_ref,std::set<std::string>> rename(const expression_ref& E, const bound_var_info& bound);
    expression_ref rename(const expression_ref& E, const bound_var_info& bound, std::set<std::string>& free_vars);
    expression_ref rename(const expression_ref& E, const bound_var_info& bound, const bound_var_info& binders, std::set<std::string>& free_vars);

    renamer_state(const Module& m_):m(m_) {}
};

std::set<std::string> find_bound_vars(const expression_ref& E);

Hs::Decls synthesize_field_accessors(const Hs::Decls& decls);

Haskell::Binds rename_infix(const Module& p, Haskell::Binds);
expression_ref rename_infix(const Module& p, const expression_ref&);
Hs::ModuleDecls rename_infix(const Module& m, Hs::ModuleDecls);

expression_ref rename_infix_decl(const Module& m, const expression_ref& E);
Hs::Decls group_fundecls(const Haskell::Decls& decls); // only value decls

// I think ghc returns a moduledecls AND a... class environment?
Haskell::ModuleDecls rename(const simplifier_options&, const Module& p, Haskell::ModuleDecls);
expression_ref rename(const Module& p, const expression_ref&);
expression_ref rename(const Module& p, const expression_ref&,const std::set<std::string>&);

typedef std::set<std::string> bound_var_info;
bound_var_info find_vars_in_pattern2(const expression_ref& pat);
const bound_var_info& get_rhs_free_vars(const expression_ref& decl);
std::vector<Hs::Decls> split_decls(const Hs::Decls& decls, const std::vector< std::vector<int> >& referenced_decls);
std::map<std::string,int> get_indices_for_names(const Hs::Decls& decls);

#endif
