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

string print_operator_expression(const string& name, const vector<string>& arguments)
{
  string output = name;
  if (arguments.size())
    output += "(" + join(arguments,", ") + ")";
  return output;
}

string print_infix_expression(const string& name, const vector<string>& arguments)
{
  if (arguments.size() != 2)
    throw myexception()<<"Infix("<<name<<"): got "<<arguments.size()<<" arguments!";

  return join(arguments,name);
}

string Operator::print() const
{
  return name();
}

string Operator::print_expression(const vector<string>& inputs) const
{
  return print_operator_expression(name(), inputs);
}

