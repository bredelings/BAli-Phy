#ifndef OCCURRENCE_H
#define OCCURRENCE_H

#include <set>
#include <utility>
#include "computation/optimization/occurrence_info.H"

class Module;

std::pair<Occ::Exp,std::set<Occ::Var>> occurrence_analyzer(const Module& m, const Core::Exp<>& E, var_context context=var_context::unknown);
Occ::Binds occurrence_analyze_decls(const Module& m, const Core::Decls<>& decls, std::set<Occ::Var>& free_vars);
Occ::Binds occurrence_analyze_binds(const Module& m, const Core::Binds<>& binds, std::set<Occ::Var>& free_vars);

#endif
