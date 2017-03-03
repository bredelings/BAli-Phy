#include "model_expression.H"
#include "context.H"
#include "computation/module.H"
#include "computation/operations.H"
#include "parser/AST.H"
#include "computation/expression/expression.H"

using std::vector;
using std::set;
using std::string;

expression_ref perform_exp(const expression_ref& F)
{
    expression_ref E = F;
    E = (identifier("gen_model"),E);
    E = (identifier("unsafePerformIO'"),E);
    E = (identifier("evaluate"),-1,E);
    return E;
}

expression_ref perform_exp(const expression_ref& F, const string& prefix)
{
    expression_ref E = F;
    E = (identifier("add_prefix"),prefix,E);
    E = (identifier("gen_model"),E);
    E = (identifier("unsafePerformIO'"),E);
    E = (identifier("evaluate"),-1,E);
    return E;
}

