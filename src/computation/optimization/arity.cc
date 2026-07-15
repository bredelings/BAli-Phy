#include "arity.H"

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <map>

#include "computation/core/func.H"

using std::map;
using std::vector;

namespace
{

using arity_environment = map<Occ::Var, arity_type>;

// Combine sequential costs; any expensive component makes the result expensive.
arity_cost add_cost(arity_cost first, arity_cost second)
{
    if (first == arity_cost::expensive or second == arity_cost::expensive)
        return arity_cost::expensive;
    return arity_cost::cheap;
}

// Describe a known id arity as a prefix of cheap, ordinary lambdas.
arity_type known_arity(int arity)
{
    arity_type result;
    result.lambdas.resize(std::max(0, arity));
    return result;
}

// Charge work before the first prospective lambda without changing later costs.
arity_type float_in(arity_cost cost, arity_type arity)
{
    if (arity.lambdas.empty() or arity.lambdas.front().cost == arity_cost::expensive)
        return arity;
    arity.lambdas.front().cost = cost;
    return arity;
}

// Mark evaluation before the first prospective lambda as expensive.
arity_type add_work(arity_type arity)
{
    if (not arity.lambdas.empty())
        arity.lambdas.front().cost = arity_cost::expensive;
    return arity;
}

// Apply one argument, consuming one lambda and charging argument work to the remainder.
arity_type apply_argument(arity_type arity, arity_cost argument_cost)
{
    if (arity.lambdas.empty()) return arity;
    auto cost = add_cost(arity.lambdas.front().cost, argument_cost);
    arity.lambdas.erase(arity.lambdas.begin());
    return float_in(cost, std::move(arity));
}

// Keep only lambda information justified by every case alternative.
arity_type intersect_arities(const arity_type& first, const arity_type& second)
{
    arity_type result;
    const auto count = std::min(first.lambdas.size(), second.lambdas.size());
    result.lambdas.reserve(count);
    for (std::size_t i = 0; i < count; ++i)
    {
        const auto one_shot = first.lambdas[i].one_shot == Core::one_shot_info::one_shot and
                              second.lambdas[i].one_shot == Core::one_shot_info::one_shot
                                  ? Core::one_shot_info::one_shot
                                  : Core::one_shot_info::unknown;
        result.lambdas.push_back({add_cost(first.lambdas[i].cost, second.lambdas[i].cost), one_shot});
    }
    return result;
}

// Trim an arity type where accumulated work could be duplicated by eta expansion.
arity_type safe_arity(arity_type arity)
{
    arity_cost accumulated = arity_cost::cheap;
    std::size_t safe_count = 0;
    for (const auto& lambda: arity.lambdas)
    {
        accumulated = add_cost(accumulated, lambda.cost);
        if (accumulated == arity_cost::expensive and
            lambda.one_shot != Core::one_shot_info::one_shot)
            break;
        ++safe_count;
    }
    arity.lambdas.resize(safe_count);
    return arity;
}

// Use call arity only after an RHS has supplied evidence that its value is a function.
arity_type add_call_arity(arity_type arity, int call_arity)
{
    if (arity.lambdas.empty() or call_arity <= 1) return arity;

    const auto existing = std::min<std::size_t>(arity.lambdas.size(), call_arity);
    for (std::size_t i = 1; i < existing; ++i)
        arity.lambdas[i].one_shot = Core::one_shot_info::one_shot;

    while (arity.lambdas.size() < static_cast<std::size_t>(call_arity))
        arity.lambdas.push_back({arity_cost::expensive, Core::one_shot_info::one_shot});
    return arity;
}

arity_type expression_arity(const Occ::Exp&, const arity_environment&, const id_info_lookup&);

// Remove case-pattern binders before analyzing one alternative body.
arity_type alternative_arity(const Occ::Alt& alternative, const arity_environment& environment,
                             const id_info_lookup& lookup)
{
    auto body_environment = environment;
    for (const auto& binder: alternative.pat.args)
        body_environment.erase(binder);
    return expression_arity(alternative.body, body_environment, lookup);
}

// Analyze a binding body with local arities while charging retained binding work.
arity_type let_arity(const Occ::Let& let, const arity_environment& environment,
                     const id_info_lookup& lookup)
{
    auto body_environment = environment;
    arity_cost binding_cost = arity_cost::cheap;
    if (auto nonrec = let.to_nonrec())
    {
        auto rhs_arity = safe_arity(expression_arity(nonrec->decl.body, environment, lookup));
        body_environment[nonrec->decl.x] = std::move(rhs_arity);
        if (not is_work_free(nonrec->decl.body)) binding_cost = arity_cost::expensive;
    }
    else
    {
        for (const auto& [binder, rhs]: let.to_rec()->decls)
        {
            body_environment[binder] = known_arity(binder.id.arity);
            if (not is_work_free(rhs)) binding_cost = arity_cost::expensive;
        }
    }
    return float_in(binding_cost, expression_arity(let.body, body_environment, lookup));
}

// Infer the untrimmed arity type of an expression in a non-recursive environment.
arity_type expression_arity(const Occ::Exp& expression, const arity_environment& environment,
                            const id_info_lookup& lookup)
{
    if (auto variable = expression.to_var())
    {
        if (auto local = environment.find(*variable); local != environment.end())
            return local->second;
        return known_arity(lookup(*variable).arity);
    }
    if (auto lambda = expression.to_lambda())
    {
        auto body_environment = environment;
        body_environment.erase(lambda->x);
        auto arity = expression_arity(lambda->body, body_environment, lookup);
        arity.lambdas.insert(arity.lambdas.begin(), {arity_cost::cheap, lambda->x.id.one_shot});
        return arity;
    }
    if (auto application = expression.to_apply())
        return apply_argument(expression_arity(application->head, environment, lookup),
                              is_work_free(application->arg) ? arity_cost::cheap : arity_cost::expensive);
    if (auto let = expression.to_let())
        return let_arity(*let, environment, lookup);
    if (auto case_expression = expression.to_case())
    {
        if (case_expression->alts.empty()) return {};
        auto alternatives_arity = alternative_arity(case_expression->alts.front(), environment, lookup);
        for (std::size_t i = 1; i < case_expression->alts.size(); ++i)
            alternatives_arity = intersect_arities(
                alternatives_arity, alternative_arity(case_expression->alts[i], environment, lookup));
        return is_work_free(case_expression->object) ? alternatives_arity : add_work(alternatives_arity);
    }
    return {};
}

// Restrict eta expansion to forms where exposing a lambda can enable simplification.
bool eta_expansion_candidate(const Occ::Exp& expression)
{
    return expression.to_lambda() or expression.to_let() or expression.to_case();
}

}

// Infer the safe arity of a non-recursive RHS, including transient call arity.
arity_type find_rhs_arity(const Occ::Exp& rhs, int call_arity, const id_info_lookup& lookup)
{
    auto arity = expression_arity(rhs, {}, lookup);
    return safe_arity(add_call_arity(std::move(arity), call_arity));
}

// Count lambdas already visible at the head of an expression.
int manifest_arity(const Occ::Exp& expression)
{
    int arity = 0;
    auto body = expression;
    while (auto lambda = body.to_lambda())
    {
        ++arity;
        body = lambda->body;
    }
    return arity;
}

// Add only the missing lambdas justified by a safe RHS arity.
Occ::Exp eta_expand(FreshVarSource& fresh, Occ::Exp expression, const arity_type& arity)
{
    const int old_arity = manifest_arity(expression);
    if (arity.arity() <= old_arity or not eta_expansion_candidate(expression)) return expression;

    vector<Occ::Var> binders;
    auto body = expression;
    while (auto lambda = body.to_lambda())
    {
        binders.push_back(lambda->x);
        body = lambda->body;
    }

    vector<Occ::Var> new_binders;
    for (int i = old_arity; i < arity.arity(); ++i)
    {
        auto binder = fresh.get_fresh_occ_var("eta");
        binder.info.work_dup = amount_t::Once;
        binder.info.code_dup = amount_t::Once;
        binder.info.context = var_context::argument;
        binder.id.one_shot = arity.lambdas[i].one_shot;
        new_binders.push_back(binder);
    }
    body = make_apply(body, new_binders);
    binders.insert(binders.end(), new_binders.begin(), new_binders.end());
    return lambda_quantify(binders, std::move(body));
}

// Report whether evaluating an expression to weak head normal form performs no work.
bool is_work_free(const Occ::Exp& expression)
{
    if (expression.to_var() or expression.to_constant() or expression.to_conApp() or expression.to_lambda())
        return true;
    if (expression.to_apply() or expression.to_builtinOp())
        return false;
    if (auto case_expression = expression.to_case())
    {
        if (not is_work_free(case_expression->object)) return false;
        for (const auto& [pattern, body]: case_expression->alts)
            if (not is_work_free(body)) return false;
        return true;
    }
    if (auto let = expression.to_let())
    {
        if (not is_work_free(let->body)) return false;
        if (auto nonrec = let->to_nonrec()) return is_work_free(nonrec->decl.body);
        for (const auto& [binder, rhs]: let->to_rec()->decls)
            if (not is_work_free(rhs)) return false;
        return true;
    }
    std::abort();
}
