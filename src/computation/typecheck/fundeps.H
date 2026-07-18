#ifndef TYPECHECK_FUNDEPS_H
#define TYPECHECK_FUNDEPS_H

#include "computation/core/type.H"

#include <set>
#include <utility>
#include <vector>

class TypeChecker;
struct FunctionalDependency;
struct InstanceInfo;

using InstantiatedFunDep =
    std::pair<std::vector<Type>, std::vector<Type>>;

InstantiatedFunDep
instantiate_fun_dep(const FunctionalDependency&,
                    const std::vector<Type>& class_args);

std::set<TypeVar>
close_wrt_fun_deps(TypeChecker&,
                   const std::vector<Type>& predicates,
                   std::set<TypeVar> fixed);

std::set<MetaTypeVar>
close_wrt_fun_deps(TypeChecker&,
                   const std::vector<Type>& predicates,
                   std::set<MetaTypeVar> fixed);

std::vector<InstanceInfo>
inconsistent_fun_dep_instances(TypeChecker&,
                               const InstanceInfo& candidate);

#endif
