#include "models/rule-call-inference.H"

#include "computation/expression/apply.H"
#include "computation/haskell/haskell.H"
#include "computation/expression/var.H"
#include "computation/loader.H"
#include "computation/module.H"
#include "computation/program.H"
#include "computation/typecheck/typecheck.H"
#include "models/rule-template.H"
#include "util/myexception.H"

#include <cctype>

using std::map;
using std::set;
using std::string;
using std::vector;

namespace
{

struct RuleCallInputs
{
    map<string, expression_ref> template_args;
    map<string, string> arg_vars;
};

// Converts arbitrary rule/argument names into lowercase Haskell binders for a
// synthetic inference module.
string safe_haskell_var_name(const string& prefix, const string& name)
{
    string result = prefix;
    for(char c: name)
    {
        if (std::isalnum(static_cast<unsigned char>(c)))
            result += c;
        else
            result += '_';
    }
    return result;
}

// Builds the display-only synthetic declaration body used for deterministic
// module hashes and diagnostics.
string synthetic_rule_module_body(const RuleInferenceInput& rule)
{
    return "infer_rule = " + rule.name + "\n";
}

// Builds the synthetic Haskell argument variables used both by resolution
// inspection and by full rule-call type inference.
RuleCallInputs build_rule_call_inputs(const RuleInferenceInput& rule)
{
    RuleCallInputs inputs;
    for(std::size_t i=0; i<rule.args.size(); i++)
    {
        const auto& arg = rule.args[i];
        auto arg_var = safe_haskell_var_name("arg_" + std::to_string(i) + "_", arg.name);
        inputs.arg_vars.insert({arg.name, arg_var});
        inputs.template_args.insert({arg.name, var(arg_var)});
    }
    return inputs;
}

// Temporary limitation: this inference path only constrains signatures from the
// call template. Remove this once defaults and alphabets participate in inference.
void require_call_only_inference_input(const RuleInferenceInput& rule)
{
    for(const auto& arg: rule.args)
        if (arg.default_value_source or arg.alphabet_source)
            throw myexception()<<"In rule for "<<rule.name<<": inferred signatures do not use default_value or alphabet yet; keep this rule explicitly annotated";
}

// Converts the legacy codegen application S-expression produced by template
// lowering into Hs::ApplyExp nodes and resolves imported globals for typecheck.
expression_ref convert_template_apps_for_typecheck(const expression_ref& expr, const Module& module, const set<string>& local_vars, vector<string>* resolved_symbols = nullptr)
{
    if (is_apply_exp(expr))
    {
        auto terms = expr.sub();
        Haskell::LExp result = {noloc, convert_template_apps_for_typecheck(terms[0], module, local_vars, resolved_symbols)};
        for(std::size_t i=1; i<terms.size(); i++)
            result = {noloc, Haskell::ApplyExp(result, {noloc, convert_template_apps_for_typecheck(terms[i], module, local_vars, resolved_symbols)})};
        return unloc(result);
    }
    else if (expr.is_expression())
    {
        Haskell::LExp result = {noloc, convert_template_apps_for_typecheck(expr.head(), module, local_vars, resolved_symbols)};
        for(const auto& arg: expr.sub())
            result = {noloc, Haskell::ApplyExp(result, {noloc, convert_template_apps_for_typecheck(arg, module, local_vars, resolved_symbols)})};
        return unloc(result);
    }
    else if (auto v = expr.to<var>())
    {
        if (local_vars.count(v->name))
            return Haskell::Var(v->name);
        auto symbol = module.lookup_symbol(v->name);
        if (resolved_symbols)
            resolved_symbols->push_back(symbol->name);
        return Haskell::Var(symbol->name);
    }
    else if (auto app = expr.to<Haskell::ApplyExp>())
    {
        auto converted = *app;
        converted.head = {converted.head.loc, convert_template_apps_for_typecheck(unloc(converted.head), module, local_vars, resolved_symbols)};
        converted.arg = {converted.arg.loc, convert_template_apps_for_typecheck(unloc(converted.arg), module, local_vars, resolved_symbols)};
        return converted;
    }
    else if (auto list = expr.to<Haskell::List>())
    {
        auto converted = *list;
        for(auto& element: converted.elements)
            element = {element.loc, convert_template_apps_for_typecheck(unloc(element), module, local_vars, resolved_symbols)};
        return converted;
    }
    else if (auto tuple = expr.to<Haskell::Tuple>())
    {
        auto converted = *tuple;
        for(auto& element: converted.elements)
            element = {element.loc, convert_template_apps_for_typecheck(unloc(element), module, local_vars, resolved_symbols)};
        return converted;
    }
    else if (auto lambda = expr.to<Haskell::LambdaExp>())
    {
        auto converted = *lambda;
        auto lambda_locals = local_vars;
        for(const auto& bound_var: Haskell::vars_in_patterns(converted.match.patterns))
            lambda_locals.insert(unloc(bound_var).name);
        for(auto& guarded: converted.match.rhs.guarded_rhss)
        {
            for(auto& guard: guarded.guards)
                guard = {guard.loc, convert_template_apps_for_typecheck(unloc(guard), module, lambda_locals, resolved_symbols)};
            guarded.body = {guarded.body.loc, convert_template_apps_for_typecheck(unloc(guarded.body), module, lambda_locals, resolved_symbols)};
        }
        return converted;
    }
    return expr;
}

// Raises a rule-qualified error if call-only inference cannot see every JSON
// argument in the lowered call template.
void require_all_args_referenced(const RuleInferenceInput& rule, const set<string>& referenced_args)
{
    for(const auto& arg: rule.args)
        if (not referenced_args.count(arg.name))
            throw myexception()<<"In rule for "<<rule.name<<": argument '"<<arg.name<<"' is absent from call template; explicit JSON annotations are required";
}

// Builds a mutable import context and infers one synthetic function declaration.
// This avoids re-running parser disambiguation on already-lowered template ASTs.
Type infer_rule_function_type(const HaskellBindingContexts& contexts, const RuleInferenceInput& rule, const Haskell::LExp& lowered_call, const map<string, string>& arg_vars)
{
    const string module_name = "BindingInfer.Rule.Main";
    const BindingImportSet imports{rule.imports};
    auto inference_module = contexts.make_imported_module(imports, module_name, synthetic_rule_module_body(rule));

    const Haskell::LVar function = {noloc, Haskell::Var("infer_rule")};
    vector<Haskell::LPat> patterns;
    set<string> local_vars;
    for(const auto& arg: rule.args)
    {
        local_vars.insert(arg_vars.at(arg.name));
        patterns.push_back({noloc, Haskell::VarPattern({noloc, Haskell::Var(arg_vars.at(arg.name))})});
    }

    Haskell::Decls declarations;
    declarations.recursive = false;
    // Temporary conversion: lowering now builds Hs::ApplyExp, but global
    // resolution still lives in this pass until inference-mode lowering owns it.
    Haskell::LExp body = {lowered_call.loc, convert_template_apps_for_typecheck(unloc(lowered_call), *inference_module, local_vars)};
    declarations.push_back({noloc, Haskell::simple_fun_decl(function, patterns, body)});

    inference_module->add_local_symbols(declarations);

    TypeChecker typechecker(*inference_module);
    (void)typechecker.infer_type_for_decls_group({}, declarations, true);

    auto type = typechecker.poly_env().find(unloc(function));
    if (not type)
        throw myexception()<<"In rule for "<<rule.name<<": failed to find synthetic inference binding";
    return *type;
}

}

// Resolves the globals used by the lowered rule template through the same
// conversion path used by full Haskell type inference.
RuleCallResolution resolve_rule_call_template(const HaskellBindingContexts& contexts, const RuleInferenceInput& rule)
{
    require_call_only_inference_input(rule);
    auto inputs = build_rule_call_inputs(rule);
    auto lowered = lower_rule_template_expr(rule.call, inputs.template_args);
    require_all_args_referenced(rule, lowered.referenced_args);

    const string module_name = "BindingInfer.Rule.Main";
    const BindingImportSet imports{rule.imports};
    auto inference_module = contexts.make_imported_module(imports, module_name, synthetic_rule_module_body(rule));

    set<string> local_vars;
    for(const auto& [_, arg_var]: inputs.arg_vars)
        local_vars.insert(arg_var);

    RuleCallResolution resolution;
    (void)convert_template_apps_for_typecheck(unloc(lowered.expr), *inference_module, local_vars, &resolution.resolved_symbols);
    return resolution;
}

// Infers one rule call by compiling a synthetic function and reading its
// generalized semantic Haskell type.
InferredRuleSignature infer_rule_call_signature(const HaskellBindingContexts& contexts, const RuleInferenceInput& rule)
{
    require_call_only_inference_input(rule);
    auto inputs = build_rule_call_inputs(rule);

    auto lowered = lower_rule_template_expr(rule.call, inputs.template_args);
    require_all_args_referenced(rule, lowered.referenced_args);

    auto inferred_type = infer_rule_function_type(contexts, rule, lowered.expr, inputs.arg_vars);
    auto [type_vars, constraints, rho_type] = peel_top_gen(inferred_type);
    auto [arg_types, result_type] = arg_result_types(rho_type);
    if (arg_types.size() != rule.args.size())
        throw myexception()<<"In rule for "<<rule.name<<": inferred "<<arg_types.size()<<" arguments, expected "<<rule.args.size();

    InferredRuleSignature result;
    result.result_type = result_type;
    result.constraints = std::move(constraints);
    for(std::size_t i=0; i<rule.args.size(); i++)
        result.arg_types.insert({rule.args[i].name, arg_types[i]});
    (void)type_vars;
    return result;
}
