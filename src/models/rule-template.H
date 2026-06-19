#ifndef RULE_TEMPLATE_H
#define RULE_TEMPLATE_H

#include <map>
#include <set>
#include <string>
#include <vector>

#include "computation/expression/expression_ref.H"
#include "computation/haskell/haskell.H"
#include "models/model-expr.H"

class Module;

struct RuleTemplateLoweringOptions
{
    const Module* inference_module = nullptr;
    std::set<std::string> local_vars;
    std::vector<std::string>* resolved_symbols = nullptr;
};

struct RuleTemplateLoweringResult
{
    Haskell::LExp expr;
    std::set<std::string> referenced_args;
};

RuleTemplateLoweringResult lower_rule_template_expr(const CM::UntypedExpr&, const std::map<std::string, expression_ref>&);

RuleTemplateLoweringResult lower_rule_template_expr(const CM::UntypedExpr&, const std::map<std::string, expression_ref>&, const RuleTemplateLoweringOptions&);

expression_ref make_rule_template_expr(const CM::UntypedExpr&, const std::map<std::string, expression_ref>&);

#endif
