#ifndef RULE_CALL_INFERENCE_H
#define RULE_CALL_INFERENCE_H

#include <map>
#include <optional>
#include <set>
#include <string>
#include <vector>

#include "computation/haskell/coretype.H"
#include "models/haskell-binding-contexts.H"
#include "models/model-expr.H"

struct RuleInferenceArg
{
    std::string name;
    std::optional<std::string> default_value_source;
    std::optional<std::string> alphabet_source;
};

struct RuleInferenceInput
{
    std::string name;
    CM::UntypedExpr call;
    std::vector<RuleInferenceArg> args;
    std::set<std::string> imports;
};

struct InferredRuleSignature
{
    Type result_type;
    std::map<std::string, Type> arg_types;
    std::vector<Type> constraints;
};

struct RuleCallResolution
{
    std::vector<std::string> resolved_symbols;
};

RuleCallResolution resolve_rule_call_template(const HaskellBindingContexts&, const RuleInferenceInput&);

InferredRuleSignature infer_rule_call_signature(const HaskellBindingContexts&, const RuleInferenceInput&);

#endif
