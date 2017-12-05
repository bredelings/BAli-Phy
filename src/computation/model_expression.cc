#include "model_expression.H"
#include "context.H"
#include "computation/module.H"
#include "computation/operations.H"
#include "computation/expression/expression.H"
#include "computation/expression/dummy.H"

using std::vector;
using std::set;
using std::string;

expression_ref perform_exp(const expression_ref& F)
{
    expression_ref E = F;
    E = (dummy("Distributions.gen_model"),E);
    E = (dummy("Prelude.unsafePerformIO'"),E);
    E = (dummy("Parameters.evaluate"),-1,E);
    return E;
}

expression_ref perform_exp(const expression_ref& F, const string& prefix)
{
    expression_ref E = F;
    E = (dummy("Distributions.add_prefix"),prefix,E);
    E = (dummy("Distributions.gen_model"),E);
    E = (dummy("Prelude.unsafePerformIO'"),E);
    E = (dummy("Parameters.evaluate"),-1,E);
    return E;
}

