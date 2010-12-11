//#include "value.H"
#include "values.H"
//#include "value.HC"

using namespace std;

vector<string> ValueBase::input_names() const 
{
  vector<string> names;
  for(int i=0;i<n_inputs();i++) {
    names.push_back(string("$")+convertToString(i+1));
  }
  return names;
}

string MultiplyValue::formula_expression(const vector<string>& args) const
{
  assert(args.size() == n_inputs());

  return join(args,'*');
}

void MultiplyValue::update(const Values& V, const std::vector<int>& mapping)
{
  assert(mapping.size()==n);
  double value = 1;
  for(int i=0;i<mapping.size();i++)
  {
    int j = mapping[i];
    if (not V.completely_up_to_date(j)) return;

    value *= V.get_value_as<Double>(j);
  }

  data = Double(value);

  up_to_date = true;
}

string FunctionValue::formula_expression(const vector<string>& args) const
{
  assert(args.size() == n_inputs());

  return name + "("+join(args,',')+")";
}

void FunctionValue::update(const Values& V, const std::vector<int>& mapping)
{
  double arg1 = V.get_value_as<Double>(mapping[0]);
  double arg2 = V.get_value_as<Double>(mapping[1]);

  double value = function(arg1, arg2);

  data = Double(value);

  up_to_date = true;
}

FunctionValue::FunctionValue(const string& s, double (*f)(double,double))
  :Value<Double>(computed),
   name(s),
   function(f)
{ }

