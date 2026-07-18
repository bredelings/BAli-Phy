#include "fundeps.H"

#include "typecheck.H"
#include "types.H"
#include "util/set.H"

#include <algorithm>

using std::set;
using std::vector;

namespace
{
    // Collects rigid variables from a type for the generic closure algorithm.
    set<TypeVar> free_variables(const Type& type, TypeVar*)
    {
        return free_type_variables(type);
    }

    // Collects metavariables from a type for the generic closure algorithm.
    set<MetaTypeVar> free_variables(const Type& type, MetaTypeVar*)
    {
        return free_meta_type_variables(type);
    }

    // Adds variables mentioned by the kinds of variables already in the set.
    template <class Var>
    set<Var> close_over_kinds(set<Var> variables)
    {
        bool changed = true;
        while(changed)
        {
            auto old_size = variables.size();
            vector<Var> current(variables.begin(), variables.end());
            for(const auto& variable: current)
                if (not variable.kind.empty())
                    add(variables, free_variables(variable.kind, static_cast<Var*>(nullptr)));
            changed = variables.size() != old_size;
        }
        return variables;
    }

    // Finds all variables in the types, including variables used by their kinds.
    template <class Var>
    set<Var> free_variables_with_kinds(const vector<Type>& types)
    {
        set<Var> variables;
        for(const auto& type: types)
            add(variables, free_variables(type, static_cast<Var*>(nullptr)));
        return close_over_kinds(std::move(variables));
    }

    // Finds rigid variables recoverable from a type without reducing a type family.
    set<TypeVar> injective_type_vars(const TypeChecker& tc, const Type& type)
    {
        if (auto type2 = filled_meta_type_var(type))
            return injective_type_vars(tc, *type2);
        if (auto tv = type.to<TypeVar>())
            return {*tv};
        if (type.is_a<MetaTypeVar>())
            return {};
        if (auto app = tc.is_type_app(type))
        {
            auto& [head, arg] = *app;
            auto variables = injective_type_vars(tc, head);
            add(variables, injective_type_vars(tc, arg));
            return variables;
        }
        if (tc.is_type_fam_app(type))
            return {};
        if (auto type2 = tc.expand_type_synonym(type))
            return injective_type_vars(tc, *type2);
        if (type.is_a<TypeCon>())
            return {};
        if (auto forall = type.to<ForallType>())
        {
            auto variables = injective_type_vars(tc, forall->type);
            for(const auto& variable: forall->type_var_binders)
                variables.erase(variable);
            return variables;
        }
        if (auto constrained = type.to<ConstrainedType>())
        {
            auto variables = injective_type_vars(tc, constrained->type);
            for(const auto& predicate: constrained->context)
                add(variables, injective_type_vars(tc, predicate));
            return variables;
        }
        std::abort();
    }

    // Selects the rigid variables recoverable from this type.
    set<TypeVar> injective_variables(const TypeChecker& tc, const Type& type, TypeVar*)
    {
        return injective_type_vars(tc, type);
    }

    // Selects the metavariables recoverable from this type.
    set<MetaTypeVar> injective_variables(const TypeChecker& tc, const Type& type, MetaTypeVar*)
    {
        return tc.injective_vars_for_type(type);
    }

    // Adds a predicate and its instantiated superclasses, stopping at repeats.
    void add_superclass_predicates(const TypeChecker& tc, const Type& predicate,
                                   vector<Type>& predicates)
    {
        if (std::find(predicates.begin(), predicates.end(), predicate) != predicates.end())
            return;
        predicates.push_back(predicate);

        auto [head, args] = decompose_type_apps(predicate);
        auto class_con = head.to<TypeCon>();
        if (not class_con) return;

        auto class_info = tc.info_for_class(class_con->name);
        if (not class_info or args.size() != class_info->type_vars.size())
            return;

        substitution_t substitution;
        for(int i = 0; i < args.size(); i++)
            substitution = substitution.insert({class_info->type_vars[i], args[i]});

        for(const auto& superclass: class_info->context)
            add_superclass_predicates(tc, apply_subst(substitution, superclass),
                                      predicates);
    }

    // Repeatedly applies equalities and class dependencies to the fixed variables.
    template <class Var>
    set<Var> close_wrt_fun_deps_impl(const TypeChecker& tc,
                                     const vector<Type>& source_predicates,
                                     set<Var> fixed)
    {
        vector<Type> predicates;
        for(const auto& predicate: source_predicates)
            add_superclass_predicates(tc, predicate, predicates);

        fixed = close_over_kinds(std::move(fixed));
        bool changed = true;
        while(changed)
        {
            auto old_size = fixed.size();
            for(const auto& predicate: predicates)
            {
                vector<InstantiatedFunDep> dependencies;
                if (auto equality = is_equality_pred(predicate))
                {
                    auto& [lhs, rhs] = *equality;
                    dependencies.push_back({{lhs}, {rhs}});
                    dependencies.push_back({{rhs}, {lhs}});
                }
                else
                {
                    auto [head, args] = decompose_type_apps(predicate);
                    auto class_con = head.to<TypeCon>();
                    auto class_info = class_con ? tc.info_for_class(class_con->name) : nullptr;
                    if (class_info and args.size() == class_info->type_vars.size())
                        for(const auto& dependency: class_info->functional_dependencies)
                            dependencies.push_back(instantiate_fun_dep(dependency, args));
                }

                for(const auto& [determining, determined]: dependencies)
                {
                    auto needed = free_variables_with_kinds<Var>(determining);
                    if (not includes(fixed, needed)) continue;

                    for(const auto& type: determined)
                        add(fixed, injective_variables(tc, type,
                                                      static_cast<Var*>(nullptr)));
                }
            }
            fixed = close_over_kinds(std::move(fixed));
            changed = fixed.size() != old_size;
        }
        return fixed;
    }
}

// Selects both sides of a dependency from a class application.
InstantiatedFunDep
instantiate_fun_dep(const FunctionalDependency& dependency,
                    const vector<Type>& class_args)
{
    InstantiatedFunDep result;
    for(int index: dependency.determining)
    {
        assert(index >= 0 and index < class_args.size());
        result.first.push_back(class_args[index]);
    }
    for(int index: dependency.determined)
    {
        assert(index >= 0 and index < class_args.size());
        result.second.push_back(class_args[index]);
    }
    return result;
}

// Closes rigid variables under kinds, equalities, superclasses, and FunDeps.
set<TypeVar>
close_wrt_fun_deps(const TypeChecker& tc, const vector<Type>& predicates,
                   set<TypeVar> fixed)
{
    return close_wrt_fun_deps_impl(tc, predicates, std::move(fixed));
}

// Closes metavariables under kinds, equalities, superclasses, and FunDeps.
set<MetaTypeVar>
close_wrt_fun_deps(const TypeChecker& tc, const vector<Type>& predicates,
                   set<MetaTypeVar> fixed)
{
    return close_wrt_fun_deps_impl(tc, predicates, std::move(fixed));
}

// Finds existing instances whose determined arguments conflict with a candidate.
vector<InstanceInfo>
inconsistent_fun_dep_instances(TypeChecker& tc,
                               const InstanceInfo& candidate_info)
{
    vector<InstanceInfo> conflicts;
    auto class_info = tc.info_for_class(candidate_info.class_con.name);
    if (not class_info) return conflicts;

    // Compares the candidate with every same-class instance in one environment.
    auto check_environment = [&](const InstanceEnv& environment)
    {
        for(const auto& [_, existing_info]: environment)
        {
            if (existing_info.class_con != candidate_info.class_con) continue;

            bool inconsistent = false;
            for(const auto& dependency: class_info->functional_dependencies)
            {
                auto candidate = tc.freshen(candidate_info);
                auto existing = tc.freshen(existing_info);
                auto [candidate_lhs, candidate_rhs] = instantiate_fun_dep(
                    dependency, candidate.args);
                auto [existing_lhs, existing_rhs] = instantiate_fun_dep(
                    dependency, existing.args);

                auto determinant_substitution = tc.maybe_unify(candidate_lhs,
                                                                existing_lhs);
                if (not determinant_substitution) continue;

                candidate_rhs = apply_subst(*determinant_substitution,
                                            candidate_rhs);
                existing_rhs = apply_subst(*determinant_substitution,
                                           existing_rhs);
                if (not tc.maybe_unify(candidate_rhs, existing_rhs))
                {
                    inconsistent = true;
                    break;
                }
            }

            if (inconsistent)
                conflicts.push_back(existing_info);
        }
    };

    check_environment(tc.this_mod().local_instances);
    for(const auto& [_, module]: tc.this_mod().transitively_imported_modules)
        check_environment(module->local_instances());
    return conflicts;
}

// Derives determined-argument equalities when two same-class constraints agree on a determinant.
vector<FunDepEquations>
improve_from_constraint(const ClassInfo& class_info, const vector<Type>& template_args, const vector<Type>& work_args)
{
    assert(template_args.size() == class_info.type_vars.size());
    assert(work_args.size() == class_info.type_vars.size());

    vector<FunDepEquations> equation_groups;
    for(const auto& dependency: class_info.functional_dependencies)
    {
        auto [template_lhs, template_rhs] = instantiate_fun_dep(dependency, template_args);
        auto [work_lhs, work_rhs] = instantiate_fun_dep(dependency, work_args);
        if (template_lhs != work_lhs) continue;

        FunDepEquations equations;
        for(int i = 0; i < template_rhs.size(); i++)
            if (template_rhs[i] != work_rhs[i])
                equations.equalities.push_back({template_rhs[i], work_rhs[i]});
        if (not equations.equalities.empty())
            equation_groups.push_back(std::move(equations));
    }
    return equation_groups;
}

// Derives determined-argument equalities from instances whose determinant matches the wanted constraint.
vector<FunDepEquations>
improve_from_instances(TypeChecker& tc, const ClassInfo& class_info, const vector<Type>& work_args)
{
    assert(work_args.size() == class_info.type_vars.size());
    vector<FunDepEquations> equation_groups;

    // Adds equations contributed by every same-class instance in one environment.
    auto improve_from_environment = [&](const InstanceEnv& environment)
    {
        for(const auto& [_, instance]: environment)
        {
            if (instance.class_con.name != class_info.name) continue;

            for(const auto& dependency: class_info.functional_dependencies)
            {
                auto [instance_lhs, instance_rhs] = instantiate_fun_dep(dependency, instance.args);
                auto [work_lhs, work_rhs] = instantiate_fun_dep(dependency, work_args);
                auto determinant_substitution = tc.maybe_match(instance_lhs, work_lhs);
                if (not determinant_substitution) continue;

                instance_rhs = apply_subst(*determinant_substitution, instance_rhs);
                auto remaining = free_variables_with_kinds<TypeVar>(instance_rhs);

                FunDepEquations equations;
                for(const auto& variable: instance.tvs)
                    if (remaining.contains(variable))
                        equations.quantified.push_back(variable);

                for(int i = 0; i < instance_rhs.size(); i++)
                    if (not tc.same_type(instance_rhs[i], work_rhs[i]))
                        equations.equalities.push_back({instance_rhs[i], work_rhs[i]});
                if (not equations.equalities.empty())
                    equation_groups.push_back(std::move(equations));
            }
        }
    };

    improve_from_environment(tc.this_mod().local_instances);
    for(const auto& [_, module]: tc.this_mod().transitively_imported_modules)
        improve_from_environment(module->local_instances());
    return equation_groups;
}
