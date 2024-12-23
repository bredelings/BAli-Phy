#ifndef OCCURRENCE_H
#define OCCURRENCE_H

#include <set>
#include <utility>
#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"
#include "computation/expression/var.H"

class Module;

std::pair<expression_ref,std::set<var>> occurrence_analyzer(const Module& m, const Occ::Exp& E, var_context context=var_context::unknown);
std::vector<CDecls> occurrence_analyze_decls(const Module& m, Occ::Decls decls, std::set<var>& free_vars);
std::vector<CDecls> occurrence_analyze_decl_groups(const Module& m, const std::vector<CDecls>& decls, std::set<var>& free_vars);

#endif
