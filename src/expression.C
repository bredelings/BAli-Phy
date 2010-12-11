#include "expression.H"

using namespace std;

int ParameterBase::total = 0;

ParameterBase::ParameterBase(const std::string& s, const ValueBase& V)
  :name(s),
   id(ParameterBase::total++),
   exemplar(V)
{
}

std::string compute_expression(const ValueBase& V, const std::vector<polymorphic_cow_ptr<ParameterBase> >& inputs)
{
  vector<string> args;
  for(int i=0;i<inputs.size();i++)
    args.push_back(inputs[i]->expression());
  return V.formula_expression(args);
}

FreeParameterBase::FreeParameterBase(const ValueBase& V, const std::vector<polymorphic_cow_ptr<ParameterBase> >& i)
  :ParameterBase(compute_expression(V,i), V),
   inputs(i)
{
}

