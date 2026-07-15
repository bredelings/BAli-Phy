#include "call-arity.H"

#include <algorithm>
#include <map>
#include <set>
#include <utility>

#include "arity.H"
#include "computation/module.H"

using std::map;
using std::set;

namespace
{

using Var = Core::Var<>;
using interesting_set = set<Var>;
using call_map = map<Var, int>;

struct co_call_graph
{
    map<Var, set<Var>> edges;

    // Add one undirected edge, retaining explicit nodes for empty neighborhoods.
    void add_edge(const Var& first, const Var& second)
    {
        edges[first].insert(second);
        edges[second].insert(first);
    }

    // Merge all nodes and edges from another co-call graph.
    void merge(const co_call_graph& other)
    {
        for (const auto& [node, neighbors]: other.edges)
        {
            edges[node];
            for (const auto& neighbor: neighbors)
                add_edge(node, neighbor);
        }
    }

    // Remove a binder and every edge incident on it.
    void remove(const Var& variable)
    {
        edges.erase(variable);
        for (auto& [node, neighbors]: edges)
            neighbors.erase(variable);
    }

    // Return the variables that may be called in the same execution as a binder.
    set<Var> neighbors(const Var& variable) const
    {
        if (auto found = edges.find(variable); found != edges.end())
            return found->second;
        return {};
    }

    // Test whether a variable may be called more than once in one execution.
    bool has_self_edge(const Var& variable) const
    {
        if (auto found = edges.find(variable); found != edges.end())
            return found->second.contains(variable);
        return false;
    }

    // Connect every pair across two call domains.
    void add_bipartite(const set<Var>& first, const set<Var>& second)
    {
        for (const auto& x: first)
            for (const auto& y: second)
                add_edge(x, y);
    }

    // Mark every variable in a domain as mutually and multiply called.
    void make_complete(const set<Var>& variables)
    {
        for (const auto& x: variables)
            for (const auto& y: variables)
                add_edge(x, y);
    }
};

struct call_arity_result
{
    co_call_graph co_calls;
    call_map calls;
};

struct analyzed_expression
{
    Core::Exp<> expression;
    call_arity_result result;
};

struct analyzed_bind
{
    Core::Bind<> bind;
    call_arity_result result;
};

// Match the simplifier's current trivial-argument category for plain Core.
bool is_trivial_argument(const Core::Exp<>& expression)
{
    return expression.to_var();
}

// Return the variables for which a result records at least one call.
set<Var> call_domain(const call_arity_result& result)
{
    set<Var> domain;
    for (const auto& [variable, arity]: result.calls)
        domain.insert(variable);
    return domain;
}

// Record the minimum argument count seen at any call of a variable.
void record_call(call_arity_result& result, const Var& variable, int arity)
{
    auto [position, inserted] = result.calls.insert({variable, arity});
    if (not inserted) position->second = std::min(position->second, arity);
    result.co_calls.edges[variable];
}

// Union mutually exclusive results without adding cross-path co-call edges.
void merge_paths(call_arity_result& result, const call_arity_result& other)
{
    for (const auto& [variable, arity]: other.calls)
        record_call(result, variable, arity);
    result.co_calls.merge(other.co_calls);
}

// Combine sequential computations and connect all calls made by the two sides.
call_arity_result combine_sequential(call_arity_result first, const call_arity_result& second)
{
    const auto first_domain = call_domain(first);
    const auto second_domain = call_domain(second);
    merge_paths(first, second);
    first.co_calls.add_bipartite(first_domain, second_domain);
    return first;
}

// Conservatively mark every recorded variable as possibly called more than once.
void make_multiply_called(call_arity_result& result)
{
    result.co_calls.make_complete(call_domain(result));
}

// Remove a locally bound variable from an outward analysis result.
void remove_variable(call_arity_result& result, const Var& variable)
{
    result.calls.erase(variable);
    result.co_calls.remove(variable);
}

class call_arity_analyzer
{
    const Module& module_;
    interesting_set top_level_variables_;
    map<Var, Core::id_info> top_level_info_;

    // Resolve top-level local metadata before consulting imported module information.
    Core::id_info get_id_info(const Var& variable) const
    {
        if (auto local = top_level_info_.find(variable); local != top_level_info_.end())
            return local->second;
        auto info = variable.id;
        if (auto symbol = module_.lookup_resolved_symbol(variable.name))
            info.arity = symbol->id_arity;
        return info;
    }

    // Use the shared arity-aware cheapness model for a plain Core RHS.
    bool rhs_is_cheap(const Core::Exp<>& rhs) const
    {
        const core_id_info_lookup lookup = [this](const Var& variable) { return get_id_info(variable); };
        return is_work_free(rhs, lookup);
    }

    analyzed_expression analyze_expression(const Core::Exp<>& expression, int incoming_arity,
                                           const interesting_set& interesting);

    analyzed_bind analyze_bind(const Core::Bind<>& bind, call_arity_result body_result,
                               const interesting_set& outer_interesting);

    // Transfer calls through one non-recursive binding and annotate its binder.
    analyzed_bind analyze_nonrec(const Core::NonRec<>& nonrec, call_arity_result body_result,
                                 const interesting_set& outer_interesting)
    {
        auto binder = nonrec.decl.x;
        const auto observed = body_result.calls.find(binder);
        const bool called = observed != body_result.calls.end();
        const bool called_repeatedly = body_result.co_calls.has_self_edge(binder);
        const bool called_at_most_once = called and not called_repeatedly;
        const int observed_arity = called ? observed->second : 0;
        const int safe_call_arity = rhs_is_cheap(nonrec.decl.body) or called_at_most_once
                                        ? observed_arity
                                        : 0;

        auto rhs_interesting = outer_interesting;
        rhs_interesting.erase(binder);
        auto rhs = analyze_expression(nonrec.decl.body, safe_call_arity, rhs_interesting);
        if (called_repeatedly and safe_call_arity != 0)
            make_multiply_called(rhs.result);

        auto co_called_with_binder = body_result.co_calls.neighbors(binder);
        co_called_with_binder.erase(binder);
        remove_variable(body_result, binder);
        auto result = body_result;
        merge_paths(result, rhs.result);
        result.co_calls.add_bipartite(call_domain(rhs.result), co_called_with_binder);

        binder.id.call_arity = safe_call_arity;
        return {Core::NonRec<>{{binder, std::move(rhs.expression)}}, std::move(result)};
    }

    // Analyze a recursive group conservatively without attempting an arity fixed point.
    analyzed_bind analyze_rec(const Core::Rec<>& rec, call_arity_result body_result,
                              const interesting_set& outer_interesting)
    {
        auto recursive_interesting = outer_interesting;
        for (const auto& [binder, rhs]: rec.decls)
            recursive_interesting.insert(binder);

        Core::Decls<> decls;
        auto result = std::move(body_result);
        for (const auto& [original_binder, original_rhs]: rec.decls)
        {
            auto binder = original_binder;
            binder.id.call_arity = 0;
            auto rhs = analyze_expression(original_rhs, 0, recursive_interesting);
            decls.push_back({binder, std::move(rhs.expression)});
            merge_paths(result, rhs.result);
        }
        make_multiply_called(result);
        for (const auto& [binder, rhs]: rec.decls)
            remove_variable(result, binder);
        return {Core::Rec<>{std::move(decls)}, std::move(result)};
    }

public:
    // Capture every top-level binder as an interesting variable and cheapness fact.
    call_arity_analyzer(const Module& module, const Core::Binds<>& binds):module_(module)
    {
        for (const auto& bind: binds)
        {
            if (auto nonrec = std::get_if<Core::NonRec<>>(&bind))
            {
                top_level_variables_.insert(nonrec->decl.x);
                top_level_info_[nonrec->decl.x] = nonrec->decl.x.id;
            }
            else
                for (const auto& [binder, rhs]: std::get<Core::Rec<>>(bind).decls)
                {
                    top_level_variables_.insert(binder);
                    top_level_info_[binder] = binder.id;
                }
        }
    }

    // Analyze top-level bindings from right to left, seeded by external uses.
    Core::Binds<> analyze(const Core::Binds<>& binds)
    {
        auto output = binds;
        call_arity_result result;
        for (const auto& variable: top_level_variables_)
            if (variable.is_exported)
                record_call(result, variable, 0);
        make_multiply_called(result);

        for (std::size_t i = output.size(); i-- > 0;)
        {
            auto analyzed = analyze_bind(output[i], std::move(result), top_level_variables_);
            output[i] = std::move(analyzed.bind);
            result = std::move(analyzed.result);
        }
        return output;
    }
};

// Analyze one expression according to its surrounding application arity.
analyzed_expression call_arity_analyzer::analyze_expression(const Core::Exp<>& expression, int incoming_arity,
                                                            const interesting_set& interesting)
{
    if (auto variable = expression.to_var())
    {
        call_arity_result result;
        if (interesting.contains(*variable)) record_call(result, *variable, incoming_arity);
        return {expression, std::move(result)};
    }
    if (auto lambda = expression.to_lambda())
    {
        auto body_interesting = interesting;
        body_interesting.erase(lambda->x);
        auto body = analyze_expression(lambda->body, std::max(0, incoming_arity - 1), body_interesting);
        if (incoming_arity == 0) make_multiply_called(body.result);
        auto output = expression;
        auto output_lambda = output.to_lambda_modify();
        output_lambda->x.id.call_arity = 0;
        output_lambda->body = std::move(body.expression);
        return {std::move(output), std::move(body.result)};
    }
    if (auto application = expression.to_apply())
    {
        auto head = analyze_expression(application->head, incoming_arity + 1, interesting);
        auto argument = analyze_expression(application->arg, 0, interesting);
        if (is_trivial_argument(application->arg)) make_multiply_called(argument.result);
        auto result = combine_sequential(std::move(head.result), argument.result);
        auto output = expression;
        auto output_application = output.to_apply_modify();
        output_application->head = std::move(head.expression);
        output_application->arg = std::move(argument.expression);
        return {std::move(output), std::move(result)};
    }
    if (auto let = expression.to_let())
    {
        auto body_interesting = interesting;
        if (auto nonrec = let->to_nonrec())
            body_interesting.insert(nonrec->decl.x);
        else
            for (const auto& [binder, rhs]: let->to_rec()->decls)
                body_interesting.insert(binder);
        auto body = analyze_expression(let->body, incoming_arity, body_interesting);
        auto bind = analyze_bind(let->bind, std::move(body.result), interesting);
        auto output = expression;
        auto output_let = output.to_let_modify();
        output_let->bind = std::move(bind.bind);
        output_let->body = std::move(body.expression);
        return {std::move(output), std::move(bind.result)};
    }
    if (auto case_expression = expression.to_case())
    {
        auto object = analyze_expression(case_expression->object, 0, interesting);
        call_arity_result alternatives_result;
        auto output = expression;
        auto output_case = output.to_case_modify();
        for (std::size_t i = 0; i < case_expression->alts.size(); ++i)
        {
            auto alternative_interesting = interesting;
            for (const auto& binder: case_expression->alts[i].pat.args)
                alternative_interesting.erase(binder);
            auto body = analyze_expression(case_expression->alts[i].body, incoming_arity,
                                           alternative_interesting);
            merge_paths(alternatives_result, body.result);
            output_case->alts[i].body = std::move(body.expression);
            for (auto& binder: output_case->alts[i].pat.args)
                binder.id.call_arity = 0;
        }
        auto result = combine_sequential(std::move(object.result), alternatives_result);
        output_case->object = std::move(object.expression);
        return {std::move(output), std::move(result)};
    }
    if (auto constructor = expression.to_conApp())
    {
        call_arity_result result;
        auto output = expression;
        auto output_constructor = output.to_conApp_modify();
        for (std::size_t i = 0; i < constructor->args.size(); ++i)
        {
            auto argument = analyze_expression(constructor->args[i], 0, interesting);
            result = combine_sequential(std::move(result), argument.result);
            output_constructor->args[i] = std::move(argument.expression);
        }
        return {std::move(output), std::move(result)};
    }
    if (auto builtin = expression.to_builtinOp())
    {
        call_arity_result result;
        auto output = expression;
        auto output_builtin = output.to_builtinOp_modify();
        for (std::size_t i = 0; i < builtin->args.size(); ++i)
        {
            auto argument = analyze_expression(builtin->args[i], 0, interesting);
            result = combine_sequential(std::move(result), argument.result);
            output_builtin->args[i] = std::move(argument.expression);
        }
        return {std::move(output), std::move(result)};
    }
    return {expression, {}};
}

// Dispatch a binding transfer while reusing the already analyzed body result.
analyzed_bind call_arity_analyzer::analyze_bind(const Core::Bind<>& bind, call_arity_result body_result,
                                                const interesting_set& outer_interesting)
{
    if (auto nonrec = std::get_if<Core::NonRec<>>(&bind))
        return analyze_nonrec(*nonrec, std::move(body_result), outer_interesting);
    return analyze_rec(std::get<Core::Rec<>>(bind), std::move(body_result), outer_interesting);
}

}

// Infer transient call arities for non-recursive Core bindings.
Core::Binds<> call_arity_analyze(const Module& module, const Core::Binds<>& binds)
{
    return call_arity_analyzer(module, binds).analyze(binds);
}
