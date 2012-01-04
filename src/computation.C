#include "computation.H"
#include "expression.H"

using boost::shared_ptr;

shared_ptr<const Object> OperationArgs::evaluate_expression(const expression_ref& E)
{
  return E;
}
