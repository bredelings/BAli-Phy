#ifndef RENAME_H
#define RENAME_H

#include <set>
#include <string>
#include "computation/expression/expression_ref.H"
#include "computation/parser/haskell.H"

class Module;

std::string get_func_name(const Haskell::ValueDecl& decl);
std::set<std::string> find_bound_vars(const expression_ref& E);
bool is_pattern_binding(const Haskell::ValueDecl& decl);
bool is_function_binding(const Haskell::ValueDecl& decl);
Haskell::Decls group_decls(const Haskell::Decls& decls);

Haskell::Decls rename_infix_top(const Module& p, const Haskell::Decls&);
expression_ref rename_infix(const Module& p, const expression_ref&);
Haskell::Decls rename(const Module& p, Haskell::Decls);
// I think ghc returns a moduledecls AND a... class environment?
Haskell::ModuleDecls rename(const Module& p, Haskell::ModuleDecls&);
expression_ref rename(const Module& p, const expression_ref&);
expression_ref rename(const Module& p, const expression_ref&,const std::set<std::string>&);

#endif
