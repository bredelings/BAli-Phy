#include "operator.H"
#include "util.H"

using std::vector;
using std::string;

tribool Operator::compare(const Object& o) const
{
  if (this == &o) return true;

  const Operator* O = dynamic_cast<const Operator*>(&o);

  if (not O) return false;

  if (name() != O->name()) return false;

  return indeterminate;
}

string print_operator_expression(const vector<string>& arguments)
{
  return join(arguments," ");
}

string print_infix_expression(const vector<string>& arguments)
{
  if (arguments.size() != 3)
    throw myexception()<<"Infix("<<arguments[0]<<"): got "<<arguments.size()-1<<" additional arguments!";

  return arguments[1]+arguments[0]+arguments[2];
}

string Operator::print() const
{
  return name();
}

string Operator::print_expression(const vector<string>& inputs) const
{
  return print_operator_expression(inputs);
}

