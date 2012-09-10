#include "computation/program.H"
#include "myexception.H"
#include "computation/graph_register.H"
#include "computation/operations.H"
#include "let-float.H"

using std::pair;
using std::map;
using std::string;
using std::vector;

symbol_info::symbol_info(const std::string& s, symbol_type_t st, int i2, int i3, fixity_t f)
  :name(s), symbol_type(st), arity(i2), precedence(i3), fixity(f)
{ }

symbol_info::symbol_info(const std::string& s, symbol_type_t st, int i2, int i3, fixity_t f, const expression_ref& b)
  :name(s), symbol_type(st), arity(i2), precedence(i3), fixity(f), body(b)
{ }

symbol_info::symbol_info(const std::string& s, symbol_type_t st, int i2, int i3, fixity_t f, const expression_ref& b, const expression_ref& t)
  :name(s), symbol_type(st), arity(i2), precedence(i3), fixity(f), body(b), type(t)
{ }

symbol_info Program::get_operator(const string& name) const
{
  auto s = symbols.find(name);
  if (s == symbols.end()) throw myexception()<<"Cannot find symbol '"<<name<<"'";

  if (s->second.arity != 2) throw myexception()<<"Operator '"<<name<<"' does not have arity 2!";

  symbol_info S = s->second;

  // An operator of undefined precedence is treated as if it has the highest precedence
  if (S.precedence == -1) 
  {
    S.precedence = 9;
    S.fixity = non_fix;
  }

  return S;
}

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

Program& Program::operator+=(const symbol_info& S)
{
  auto loc = symbols.find(S.name);
  if (loc != symbols.end())
    throw myexception()<<"Can't add symbol with name '"<<S.name<<"': that name is already used!";

  symbols[S.name] = S;
  return *this;
}

Program& Program::operator+=(const Program& P)
{
  for(const auto& s: P.symbols)
    (*this) += s.second;

  return *this;
}

void Program::def_function(const std::string& name, int arity, int precedence, fixity_t f, const expression_ref& body, const expression_ref& type)
{
  auto loc = symbols.find(name);
  if (loc != symbols.end())
    throw myexception()<<"Can't add function with name '"<<name<<"': that name is already used!";

  (*this) += symbol_info{name, variable_symbol, arity, precedence, f, body, type};
}

void Program::def_function(const std::string& name, int arity, int precedence, fixity_t f, const expression_ref& body)
{
  def_function(name, arity, precedence, f, body, {});
}

void Program::def_function(const std::string& name, const expression_ref& body)
{
  def_function(name, -1, -1, unknown_fix, body, {});
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
  auto loc = symbols.find(name);
  if (loc == symbols.end())
    return false;
  else
    return true;
}

expression_ref Program::get_function(const std::string& name) const
{
  auto loc = symbols.find(name);
  if (loc == symbols.end())
    throw myexception()<<"Can't find function of name '"<<name<<"'";

  return loc->second.body;
}

// A name of "" means that we are defining a top-level program, or a piece of a top-level program.
Program::Program(const std::string& n)
  :name(n)
{ }

std::ostream& operator<<(std::ostream& o, const Program& D)
{
  for(const auto& s: D.symbols)
  {
    const symbol_info& S = s.second;
    if (S.body)
    {
      o<<S.name<<" = "<<S.body<<")\n";
      o<<S.name<<" = "<<let_float(S.body)<<")\n";
      o<<"\n";
    }
  }
  return o;
}

