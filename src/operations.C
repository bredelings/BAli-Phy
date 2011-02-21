#include "operations.H"

using std::vector;
using std::string;

string IfThenElse::expression(const vector<string>& inputs) const
{
  if (inputs.size() != 3)
    throw myexception()<<"IfThenElse::expression - got "<<inputs.size()<<" arguments instead of 3.";

  return function_expression_("if",inputs);
}

