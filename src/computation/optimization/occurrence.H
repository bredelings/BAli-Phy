#ifndef OCCURRENCE_H
#define OCCURRENCE_H

#include <set>
#include <utility>
#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"
#include "computation/expression/var.H"

class Module;

std::pair<Occ::Exp,std::set<Occ::Var>> occurrence_analyzer(const Module& m, const Core2::Exp<>& E, var_context context=var_context::unknown);
std::vector<Occ::Decls> occurrence_analyze_decls(const Module& m, const Core2::Decls<>& decls, std::set<Occ::Var>& free_vars);
std::vector<Occ::Decls> occurrence_analyze_decl_groups(const Module& m, const std::vector<Core2::Decls<>>& decls, std::set<Occ::Var>& free_vars);

#endif
