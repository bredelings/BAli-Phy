#ifndef RULE_CALL_INFERENCE_H
#define RULE_CALL_INFERENCE_H

#include <map>
#include <optional>
#include <set>
#include <string>
#include <vector>

#include "computation/haskell/coretype.H"
#include "computation/haskell/haskell.H"
#include "models/haskell-binding-contexts.H"
#include "models/model-expr.H"

struct RuleCallAnalysisArg
{
    std::string name;
    std::optional<std::string> default_value_source;
    std::optional<std::string> alphabet_source;
};

struct RuleCallAnalysisInput
{
    std::string name;
    CM::UntypedExpr call;
    std::vector<RuleCallAnalysisArg> args;
    std::set<std::string> imports;
};

struct InferredRuleSignature
{
    std::vector<TypeVar> quantified_vars;
    Type result_type;
    std::map<std::string, Type> arg_types;
    std::vector<Type> constraints;
};

struct RuleCallResolution
{
    std::vector<std::string> resolved_symbols;
};

struct RuleCallAnalysis
{
    std::optional<Haskell::LExp> resolved_call;
    std::set<std::string> referenced_args;
    std::vector<std::string> resolved_symbols;
    std::optional<InferredRuleSignature> signature;
    std::optional<std::string> resolution_error;
    std::optional<std::string> inference_error;
};

RuleCallAnalysis analyze_rule_call(const HaskellBindingContexts&, const RuleCallAnalysisInput&);

RuleCallResolution resolve_rule_call_template(const HaskellBindingContexts&, const RuleCallAnalysisInput&);

InferredRuleSignature infer_rule_call_signature(const HaskellBindingContexts&, const RuleCallAnalysisInput&);

#endif
