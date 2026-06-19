#ifndef RULE_CALL_INFERENCE_H
#define RULE_CALL_INFERENCE_H

#include <map>
#include <string>
#include <vector>

#include "computation/haskell/coretype.H"
#include "models/haskell-binding-contexts.H"
#include "models/rules.H"

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

RuleCallResolution resolve_rule_call_template(const HaskellBindingContexts&, const Rule&);

InferredRuleSignature infer_rule_call_signature(const HaskellBindingContexts&, const Rule&);

#endif
