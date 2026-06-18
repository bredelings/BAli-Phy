#ifndef RULE_TEMPLATE_H
#define RULE_TEMPLATE_H

#include <map>
#include <set>
#include <string>

#include "computation/expression/expression_ref.H"
#include "models/model-expr.H"

struct RuleTemplateLoweringResult
{
    expression_ref expr;
    std::set<std::string> referenced_args;
};

RuleTemplateLoweringResult lower_rule_template_expr(const CM::UntypedExpr&, const std::map<std::string, expression_ref>&);

expression_ref make_rule_template_expr(const CM::UntypedExpr&, const std::map<std::string, expression_ref>&);

#endif
