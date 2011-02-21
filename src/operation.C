#include "operation.H"

#include "util.H"

using std::vector;
using std::string;

string function_expression_(const string& name, const vector<string>& arguments)
{
  string output = name;
  output += "(" + join(arguments,", ") + ")";
  return output;
}

string Operation::expression(const vector<string>& inputs) const
{
  return function_expression_("[unknown]",inputs);
}

