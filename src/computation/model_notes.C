#include "model_notes.H"

using std::vector;
using std::set;
using std::string;

int Model_Notes::add_note(const expression_ref& E)
{
  for(int i=0;i<notes.size();i++)
    if (notes[i] == E)
      return i;
    else
      assert(notes[i]->print() != E->print());

  notes.push_back(E);
  return notes.size()-1;
}

void Model_Notes::add_notes(const std::vector<expression_ref>& N)
{
  // Note: It is quite likely that we'll add notes that we already contain.
  //       That will be handled by add_note( ).

  for(int i=0;i<N.size();i++)
    for(int j=0;j<i;j++)
      assert(N[i] != N[j]);

  for(int i=0;i<N.size();i++)
    add_note(N[i]);
}

int Model_Notes::find_match_notes(const expression_ref& query, std::vector<expression_ref>& results, int start) const
{
  assert(start >= 0);
  for(int i=start;i<n_notes();i++)
  {
    results.clear();
    if (find_match(query, get_note(i), results))
      return i;
  }
  return -1;
}

Model_Notes::Model_Notes()
{ }

Model_Notes::Model_Notes(const std::vector<expression_ref>& N)
{
  add_notes(N);
}

Model_Notes add_prefix(const std::string& prefix, const Model_Notes& M1)
{
  Model_Notes M2;
  for(int i=0;i<M1.n_notes();i++)
    M2.add_note(add_prefix(prefix, M1.get_note(i)));

  return M2;
}

Model_Notes prefix_formula(const std::string& prefix,const Model_Notes& N)
{
  set<string> declared_parameter_names = find_declared_parameters(N);
  Model_Notes N2 = N;
  for(const auto& name: declared_parameter_names)
    N2 = substitute(N2, parameter(name), parameter(prefix+"."+name));
  return N2;
}

expression_ref def_parameter(Model_Notes& N, const std::string& name)
{
  expression_ref var = parameter(name);
  N.add_note( constructor("DeclareParameter",1) + var );
  return var;
}

expression_ref def_parameter(Model_Notes& N, const std::string& name, const expression_ref& def_value)
{
  expression_ref var = def_parameter(N,name);
  N.add_note( constructor("DefaultValue",2) + var + def_value );
  return var;
}

expression_ref def_parameter(Model_Notes& N, const std::string& name, const expression_ref& def_value, const Bounds<double>& b)
{
  expression_ref var = def_parameter(N, name, def_value);
  N.add_note( constructor("VarBounds",2) + var + b );
  return var;
}

expression_ref def_parameter(Model_Notes& N, const std::string& name, const expression_ref& def_value, const Bounds<double>& b, const expression_ref& D)
{
  expression_ref var = def_parameter(N, name, def_value, b);
  N.add_note( constructor(":~",2) + var + D );
  return var;
}

expression_ref def_parameter(Model_Notes& N, const std::string& name, const expression_ref& def_value, std::nullptr_t, const expression_ref& D)
{
  expression_ref var = def_parameter(N, name, def_value);
  N.add_note( constructor(":~",2) + var + D );
  return var;
}

Model_Notes substitute(const Model_Notes& N, const expression_ref& E1, const expression_ref& E2)
{
  Model_Notes N2 = N;
  for(auto& n: N2.get_notes())
    n = substitute(n, E1, E2);
  return N2;
}


set<string> find_declared_parameters(const vector<expression_ref>& Notes)
{
  set<string> parameter_names;

  // Check each expression in the Formula
  for(const auto& n: Notes)
    if (is_exactly(n,"DeclareParameter"))
      parameter_names.insert( assert_is_a<parameter>(n->sub[0])->parameter_name );

  return parameter_names;
}

set<string> find_declared_parameters(const Model_Notes& Notes)
{
  return find_declared_parameters(Notes.get_notes());
}
