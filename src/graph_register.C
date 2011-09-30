#include "graph_register.H"

using boost::shared_ptr;
using std::string;
using std::vector;

reg::reg():name(convertToString(this)),named(false) {}
reg::reg(const string& s):name(s),named(true) {}

shared_ptr<reg> incremental_evaluate(const context&, const shared_ptr<reg>&);

/// Return the value of a particular index, computing it if necessary
shared_ptr<const Object> context::evaluate(int index) const
{
  shared_ptr<reg> result = incremental_evaluate(*this,heads[index]);
  return result->E;
}

/// Get the value of a non-constant, non-computed index -- or should this be the nth parameter?
shared_ptr<const Object> context::get_parameter_value(int index) const
{
  return heads[index]->E;
}

/// Get the value of a non-constant, non-computed index
shared_ptr<const Object> context::get_parameter_value(const std::string&) const
{
  return shared_ptr<const Object>();
}

/// Update the value of a non-constant, non-computed index
void context::set_parameter_value(int index, const object_ref&)
{
  
}

/// Update the value of a non-constant, non-computed index
void context::set_parameter_value(const std::string& var, const object_ref&)
{
}

int context::add_expression(const expression_ref& e)
{
  shared_ptr<reg> R ( new reg );
  R->E = e;
  heads.push_back(R);
  return heads.size()-1;
}
