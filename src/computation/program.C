#include "computation/program.H"
#include "myexception.H"
#include "computation/graph_register.H"
#include "computation/operations.H"
#include "let-float.H"

using std::pair;
using std::map;
using std::string;
using std::vector;

// Unknown type
expression_ref unknown_type = constructor("?",0);

// Kind of Types with 0 arguments.  E.g. 
expression_ref simple_kind = constructor("*",0);

// A type constructor, and also a kind (?) constructor?
expression_ref function = lambda_expression( right_assoc_constructor("->",2) );

// Type constructor.  Kind = *->*
expression_ref list_constructor = constructor("[]",1);

void parse_combinator_application(const expression_ref& E, string& name, vector<expression_ref>& patterns)
{
  expression_ref E2 = E;

  // Look through the arguments
  while( E2->size() )
  {
    if (not is_a<Apply>(E2) ) throw myexception()<<" Combinator definition '"<<E<<"' contains non-apply expression '"<<E2<<"'";
    patterns.insert(patterns.begin(), E2->sub[1]);
    E2 = E2->sub[0];
  }

  // Find the name
  object_ptr<const var> V = is_a<var>(E2);
  if (not V)
    throw myexception()<<"Combinator definition '"<<E<<"' does not start with variable!";
  name = V->name;
}

Def& Def::operator()(const expression_ref& pattern, const expression_ref& body)
{
  patterns.push_back(pattern);
  bodies.push_back(body);
  return *this;
}



Def::Def(const expression_ref& pattern, const expression_ref& body)
{
  patterns.push_back(pattern);
  bodies.push_back(body);
}

std::ostream& operator<<(std::ostream& o, const Def& D)
{
  for(int i=0;i<D.patterns.size();i++)
    o<<D.patterns[i]<<" = "<<D.bodies[i]<<"\n";
  o<<"\n";
  return o;
}

Program& Program::operator+=(const Def& D)
{
  def_function(D.patterns, D.bodies);
  return *this;
}

Program& Program::operator+=(const Program& P)
{
  for(const auto& f: P.functions)
    def_function(f.first, f.second.first, f.second.second);

  return *this;
}

void Program::def_function(const string& name, const expression_ref& E, const expression_ref& T)
{
  map<string, pair<expression_ref, expression_ref> >::const_iterator loc = functions.find(name);
  if (loc != functions.end())
    throw myexception()<<"Can't add function with name '"<<name<<"': that name is already used!";

  functions[name] = pair<expression_ref,expression_ref>(E, T);
}

void Program::def_function(const string& name, const expression_ref& E)
{
  def_function(name, E, unknown_type);
}

void Program::def_parameter(const string& name)
{
  def_function(name, parameter(name));
}

void Program::def_parameter(const string& name, const expression_ref& T)
{
  def_function(name, parameter(name), T);
}

void Program::def_function(const vector<expression_ref>& patterns, const vector<expression_ref>& bodies)
{
  assert(patterns.size());
  assert(patterns.size() == bodies.size());

  string name;
  vector< vector<expression_ref> > sub_patterns(patterns.size());
  parse_combinator_application(patterns[0], name, sub_patterns[0]);

  for(int i=1;i<patterns.size();i++)
  {
    string name2;
    parse_combinator_application(patterns[i], name2, sub_patterns[i]);

    if (name != name2) throw myexception()<<"In definition of function '"<<name<<"', incorrect function name '"<<name2<<"' found as well.";
  }

  expression_ref E = ::def_function(sub_patterns, bodies);
  def_function(name, E);
}

bool Program::is_declared(const std::string& name) const
{
  map<string, pair<expression_ref,expression_ref> >::const_iterator loc = functions.find(name);
  if (loc == functions.end())
    return false;
  else
    return true;
}

expression_ref Program::get_function(const std::string& name) const
{
  map<string, pair<expression_ref,expression_ref> >::const_iterator loc = functions.find(name);
  if (loc == functions.end())
    throw myexception()<<"Can't find function of name '"<<name<<"'";

  return loc->second.first;
}

std::ostream& operator<<(std::ostream& o, const Program& D)
{
  for(const auto& f: D.functions)
  {
    o<<f.first<<" = "<<f.second.first<<"  ("<<f.second.second<<")\n";
    o<<f.first<<" = "<<let_float(f.second.first)<<"  ("<<f.second.second<<")";
    o<<"\n";
  }
  return o;
}

