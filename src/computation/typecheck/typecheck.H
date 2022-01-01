#ifndef TYPECHECK_H
#define TYPECHECK_H

#include <string>
#include <vector>
#include <map>
#include "immer/map.hpp" // for immer::map
#include "kind.H"
#include "kindcheck.H"
#include "env.H"

#include "computation/parser/haskell.H"

void typecheck(const Hs::ModuleDecls& M);

Haskell::Type type_check_class_method_type(kindchecker_state& K, Haskell::Type type, const Haskell::Type& constraint);

std::tuple<global_value_env, global_instance_env,class_info,Hs::Decls>
type_check_type_class(const Module& m, const type_con_env& tce, const Haskell::ClassDecl& class_decl);

#endif
