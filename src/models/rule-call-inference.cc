#include "models/rule-call-inference.H"

#include "computation/haskell/haskell.H"
#include "computation/expression/var.H"
#include "computation/loader.H"
#include "computation/module.H"
#include "computation/program.H"
#include "computation/typecheck/typecheck.H"
#include "models/rule-template.H"
#include "util/myexception.H"

#include <cctype>
#include <sstream>

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

// Builds diagnostic-only synthetic source that mirrors the inference declaration
// shape; the AST declaration below remains the source of behavior.
string synthetic_rule_module_body(const RuleCallAnalysisInput& rule, const RuleCallInputs& inputs)
{
    auto lowered = lower_rule_template_expr(rule.call, inputs.template_args);

    std::ostringstream out;
    out<<"infer_rule";
    for(const auto& arg: rule.args)
        out<<" "<<inputs.arg_vars.at(arg.name);
    out<<" = "<<unloc(lowered.expr).print()<<"\n";
    return out.str();
}

// Builds the synthetic Haskell argument variables used both by resolution
// inspection and by full rule-call type inference.
RuleCallInputs build_rule_call_inputs(const RuleCallAnalysisInput& rule)
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
void require_call_only_inference_input(const RuleCallAnalysisInput& rule)
{
    for(const auto& arg: rule.args)
        if (arg.default_value_source or arg.alphabet_source)
            throw myexception()<<"In rule for "<<rule.name<<": inferred signatures do not use default_value or alphabet yet; keep this rule explicitly annotated";
}

// Raises a rule-qualified error if call-only inference cannot see every JSON
// argument in the lowered call template.
void require_all_args_referenced(const RuleCallAnalysisInput& rule, const set<string>& referenced_args)
{
    for(const auto& arg: rule.args)
        if (not referenced_args.count(arg.name))
            throw myexception()<<"In rule for "<<rule.name<<": argument '"<<arg.name<<"' is absent from call template; explicit JSON annotations are required";
}

// Builds the synthetic module that supplies the binding imports used while
// lowering and typechecking one rule inference declaration.
std::shared_ptr<Module> make_rule_inference_module(const HaskellBindingContexts& contexts, const RuleCallAnalysisInput& rule, const RuleCallInputs& inputs)
{
    const string module_name = "BindingInfer.Rule.Main";
    const BindingImportSet imports{rule.imports};
    return contexts.make_imported_module(imports, module_name, synthetic_rule_module_body(rule, inputs));
}

// Builds inference-mode lowering options, keeping synthetic argument binders
// local while resolving template globals through the imported module.
RuleTemplateLoweringOptions make_inference_lowering_options(const Module& inference_module, const RuleCallInputs& inputs, vector<string>* resolved_symbols = nullptr)
{
    RuleTemplateLoweringOptions options;
    options.inference_module = &inference_module;
    options.resolved_symbols = resolved_symbols;
    for(const auto& [_, arg_var]: inputs.arg_vars)
        options.local_vars.insert(arg_var);
    return options;
}

// Adds and infers one synthetic function declaration in the already imported
// inference module.
Type infer_rule_function_type(Module& inference_module, const RuleCallAnalysisInput& rule, const Haskell::LExp& lowered_call, const map<string, string>& arg_vars)
{
    const Haskell::LVar function = {noloc, Haskell::Var("infer_rule")};
    vector<Haskell::LPat> patterns;
    for(const auto& arg: rule.args)
        patterns.push_back({noloc, Haskell::VarPattern({noloc, Haskell::Var(arg_vars.at(arg.name))})});

    Haskell::Decls declarations;
    declarations.recursive = false;
    declarations.push_back({noloc, Haskell::simple_fun_decl(function, patterns, lowered_call)});

    inference_module.add_local_symbols(declarations);

    TypeChecker typechecker(inference_module);
    (void)typechecker.infer_type_for_decls_group({}, declarations, true);

    auto type = typechecker.poly_env().find(unloc(function));
    if (not type)
        throw myexception()<<"In rule for "<<rule.name<<": failed to find synthetic inference binding";
    return *type;
}

// Peels the generalized synthetic function type into the signature pieces used
// by rule loading and audit analysis.
InferredRuleSignature signature_from_inferred_type(const RuleCallAnalysisInput& rule, const Type& inferred_type)
{
    auto [type_vars, constraints, rho_type] = peel_top_gen(inferred_type);
    auto [arg_types, result_type] = arg_result_types(rho_type);
    if (arg_types.size() != rule.args.size())
        throw myexception()<<"In rule for "<<rule.name<<": inferred "<<arg_types.size()<<" arguments, expected "<<rule.args.size();

    InferredRuleSignature result;
    result.quantified_vars = std::move(type_vars);
    result.result_type = result_type;
    result.constraints = std::move(constraints);
    for(std::size_t i=0; i<rule.args.size(); i++)
        result.arg_types.insert({rule.args[i].name, arg_types[i]});
    return result;
}

}

// Performs best-effort Haskell analysis of one rule call template, with callers
// choosing whether to continue from resolution into signature inference.
static RuleCallAnalysis analyze_rule_call_impl(const HaskellBindingContexts& contexts, const RuleCallAnalysisInput& rule, bool infer_signature)
{
    RuleCallAnalysis analysis;
    auto inputs = build_rule_call_inputs(rule);

    std::shared_ptr<Module> inference_module;
    try
    {
        inference_module = make_rule_inference_module(contexts, rule, inputs);
        auto options = make_inference_lowering_options(*inference_module, inputs, &analysis.resolved_symbols);
        auto lowered = lower_rule_template_expr(rule.call, inputs.template_args, options);
        analysis.referenced_args = std::move(lowered.referenced_args);
        analysis.resolved_call = std::move(lowered.expr);
    }
    catch(const std::exception& e)
    {
        analysis.resolution_error = e.what();
        return analysis;
    }

    if (not infer_signature)
        return analysis;

    try
    {
        auto inferred_type = infer_rule_function_type(*inference_module, rule, *analysis.resolved_call, inputs.arg_vars);
        analysis.signature = signature_from_inferred_type(rule, inferred_type);
    }
    catch(const std::exception& e)
    {
        analysis.inference_error = e.what();
    }

    return analysis;
}

// Performs best-effort Haskell analysis of one rule call template, returning a
// resolved expression even when later signature inference fails.
RuleCallAnalysis analyze_rule_call(const HaskellBindingContexts& contexts, const RuleCallAnalysisInput& rule)
{
    return analyze_rule_call_impl(contexts, rule, true);
}

// Performs the resolution half of rule-call analysis without invoking Haskell
// signature inference for callers that need broad, non-fatal inspection.
RuleCallAnalysis analyze_rule_call_resolution(const HaskellBindingContexts& contexts, const RuleCallAnalysisInput& rule)
{
    return analyze_rule_call_impl(contexts, rule, false);
}

// Infers one rule call by compiling a synthetic function and reading its
// generalized semantic Haskell type.
InferredRuleSignature infer_rule_call_signature(const HaskellBindingContexts& contexts, const RuleCallAnalysisInput& rule)
{
    require_call_only_inference_input(rule);
    auto analysis = analyze_rule_call(contexts, rule);
    if (analysis.resolution_error)
        throw myexception()<<*analysis.resolution_error;
    require_all_args_referenced(rule, analysis.referenced_args);
    if (analysis.signature)
        return *analysis.signature;
    if (analysis.inference_error)
        throw myexception()<<*analysis.inference_error;
    throw myexception()<<"In rule for "<<rule.name<<": Haskell signature inference did not produce a signature";
}
