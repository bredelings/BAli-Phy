#ifndef OCCURRENCE_H
#define OCCURRENCE_H

#include <set>
#include <utility>
#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"
#include "computation/expression/var.H"

std::pair<expression_ref,std::set<var>> occurrence_analyzer(const expression_ref& E, var_context context=var_context::unknown);
std::vector<CDecls> occurrence_analyze_decls(CDecls decls, std::set<var>& free_vars);
std::vector<CDecls> occurrence_analyze_decl_groups(const std::vector<CDecls>& decls, std::set<var>& free_vars);

#endif
