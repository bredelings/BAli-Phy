#ifndef OCCURRENCE_H
#define OCCURRENCE_H

#include <set>
#include <map>
#include <utility>
#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"
#include "computation/expression/dummy.H"

std::pair<expression_ref,std::set<dummy>> occurrence_analyzer(const expression_ref& E, var_context context=var_context::unknown);
std::vector<CDecls> occurrence_analyze_decls(CDecls decls, std::set<dummy>& free_vars);
std::vector<CDecls> occurrence_analyze_decl_groups(const std::vector<CDecls>& decls, std::set<dummy>& free_vars);

#endif
