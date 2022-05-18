#ifndef RENAME_H
#define RENAME_H

#include <map>
#include <set>
#include <string>
#include "computation/expression/expression_ref.H"
#include "computation/parser/haskell.H"

class Module;

std::string get_func_name(const Haskell::ValueDecl& decl);
std::set<std::string> find_bound_vars(const expression_ref& E);
bool is_pattern_binding(const Haskell::ValueDecl& decl);
bool is_function_binding(const Haskell::ValueDecl& decl);

Hs::Decls synthesize_field_accessors(const Hs::Decls& decls);

Haskell::Binds rename_infix(const Module& p, Haskell::Binds);
expression_ref rename_infix(const Module& p, const expression_ref&);
Hs::ModuleDecls rename_infix(const Module& m, Hs::ModuleDecls);


// I think ghc returns a moduledecls AND a... class environment?
Haskell::ModuleDecls rename(const Module& p, Haskell::ModuleDecls);
expression_ref rename(const Module& p, const expression_ref&);
expression_ref rename(const Module& p, const expression_ref&,const std::set<std::string>&);

typedef std::set<std::string> bound_var_info;
bound_var_info find_vars_in_pattern2(const expression_ref& pat);
const bound_var_info& get_rhs_free_vars(const expression_ref& decl);
std::vector<Hs::Decls> split_decls(const Hs::Decls& decls, const std::vector< std::vector<int> >& referenced_decls);
std::map<std::string,int> get_indices_for_names(const Hs::Decls& decls);

#endif
