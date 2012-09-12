#include "computation/program.H"
#include "myexception.H"
#include "computation/graph_register.H"
#include "computation/operations.H"
#include "let-float.H"

using std::pair;
using std::map;
using std::string;
using std::vector;

symbol_info::symbol_info(const std::string& s, symbol_type_t st, scope_t sc, int i2, int i3, fixity_t f)
  :name(s), symbol_type(st), scope(sc), arity(i2), precedence(i3), fixity(f)
{ }

symbol_info::symbol_info(const std::string& s, symbol_type_t st, scope_t sc, int i2, int i3, fixity_t f, const expression_ref& b)
  :name(s), symbol_type(st), scope(sc), arity(i2), precedence(i3), fixity(f), body(b)
{ }

symbol_info::symbol_info(const std::string& s, symbol_type_t st, scope_t sc, int i2, int i3, fixity_t f, const expression_ref& b, const expression_ref& t)
  :name(s), symbol_type(st), scope(sc), arity(i2), precedence(i3), fixity(f), body(b), type(t)
{ }

// symbols in a module can be:
// (a) this_module.local_identifier
// (b) local_identifier
// (c) other_modules.external_identifier
// (d) external_identifier
//
// Cases (a) and (c) are stored in symbols[].
// Cases (b) and (d) are stored as mapping from unqualified -> qualified names;


void Program::add_alias(const string& s)
{
  if (not is_qualified_symbol(s))
    throw myexception()<<"Can't add alias for unqualified identifier '"<<s<<"' to module '"<<name<<"'";

  if (not is_declared_qualified(s))
    throw myexception()<<"Can't add alias for undeclared identifier '"<<s<<"' to module '"<<name<<"'";

  if (symbols[s].symbol_type == parameter_symbol)
    throw myexception()<<"Can't add alias for parameter! (Parameters must be fully qualified).";

  string s2 = get_unqualified_name(s);

  if (is_declared_unqualified(s2))
    throw myexception()<<"Trying to add duplicate alias '"<<s2<<"' for identifier '"<<s<<"' to module '"<<name<<"'";

  // Mapping from name -> module.name
  aliases[s2] = s;
}

void Program::add_symbol(const symbol_info& S)
{
  if (name.size() and not is_qualified_symbol(S.name) and S.symbol_type != parameter_symbol)
    throw myexception()<<"Can't add unqualified identifier '"<<S.name<<"' to module '"<<name<<"'";

  if (is_declared_qualified(S.name))
    throw myexception()<<"Trying to add identifier '"<<S.name<<"' twice to module '"<<name<<"'";

  if (S.scope == unknown_scope)
    throw myexception()<<"Can't add symbol with unknown scope!";

  if (not name.size() and S.scope != global_scope)
    throw myexception()<<"Unnamed module: cannot add symbol '"<<S.name<<"' with non-global scope";

  symbols[S.name] = S;
}

void Program::add_symbol(const symbol_info& S, scope_t sc)
{
  symbol_info S2 = S;
  S2.scope = sc;
  add_symbol(S2);
}

void Program::declare_symbol(const symbol_info& S)
{
  // FIXME - perhaps I should not declare symbols at top level, but only import them?

  if (is_qualified_symbol(S.name))
    throw myexception()<<"Locally defined symbols should not be qualified.";

  if (S.symbol_type == parameter_symbol)
    throw myexception()<<"Declare parameters through declare_parameter(): "<<S.name;

  symbol_info S2 = S;
  S2.name = name + "." + S.name;
  add_symbol(S2, local_scope);
  
  // Add the alias for S.name -> S2.name;
  if (S.symbol_type != parameter_symbol)
    add_alias(S2.name);
}

void Program::declare_parameter(const std::string& pname)
{
  declare_parameter(pname, {});
}

void Program::declare_parameter(const std::string& pname, const expression_ref& type)
{
  add_symbol({pname, parameter_symbol, global_scope, -1, -1, unknown_fix, parameter(pname),type});
}

// Question: what if we import m1.s, which depends on an unimported m2.s?

void Program::import_symbol(const symbol_info& S, bool qualified)
{
  if (S.symbol_type == parameter_symbol)
    throw myexception()<<"May not import parameters...";

  if (not is_qualified_symbol(S.name))
    throw myexception()<<"Imported symbols must have qualified names.";

  // Add the symbol
  symbol_info S2 = S;
  if (S2.scope == local_scope)
    S2.scope = external_scope;

  add_symbol(S2);

  if (not qualified)
    add_alias(S2.name);
}

void Program::import_module(const Program& P2, const string& module_name, bool qualified)
{
  for(const auto& p: P2.symbols)
  {
    const symbol_info& S = p.second;

    if (get_module_name(S.name) == module_name)
      import_symbol(S, qualified);
  }
}

bool Program::is_declared_qualified(const std::string& name) const
{
  auto loc = symbols.find(name);
  if (loc == symbols.end())
    return false;
  else
    return true;
}

bool Program::is_declared_unqualified(const std::string& name) const
{
  auto loc = aliases.find(name);
  if (loc == aliases.end())
    return false;
  else
    return true;
}

bool Program::is_declared(const std::string& name) const
{
  return is_declared_qualified(name) or is_declared_unqualified(name);
}

symbol_info Program::lookup_qualified_symbol(const std::string& name) const
{
  if (is_declared_qualified(name))
    return symbols.find(name)->second;
  else
    throw myexception()<<"Qualified name '"<<name<<"' not declared.";
}

symbol_info Program::lookup_unqualified_symbol(const std::string& name) const
{
  if (is_qualified_symbol(name))
    throw myexception()<<"Lookup up qualified symbol '"<<name<<"' as unqualified!";

  if (not is_declared_unqualified(name))
    throw myexception()<<"Unqualified name '"<<name<<"' not declared.";
  string name2 = aliases.find(name)->second;

  if (not is_declared_qualified(name2))
    throw myexception()<<"Alias for '"<<name2<<"', which is not declared!";

  return lookup_qualified_symbol(name2);
}

symbol_info Program::lookup_symbol(const std::string& name) const
{
  if (is_qualified_symbol(name))
    return lookup_qualified_symbol(name);
  else
    return lookup_unqualified_symbol(name);
}

symbol_info Program::get_operator(const string& name) const
{
  symbol_info S = lookup_symbol(name);

  if (S.arity < 2) throw myexception()<<"Operator '"<<S.name<<"' does not have arity at least 2!";

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

vector<string> get_haskell_identifier_path(const std::string& s)
{
  if (not s.size())
    throw myexception()<<"Empty string is not a legal Haskell identifier!";

  vector<string> path = split(s, '.');

  for(int i=0;i<path.size()-1;i++)
    if (not is_haskell_conid(path[i]))
      throw myexception()<<"Module id component '"<<path[i]<<"' in identifier '"<<s<<"' is not legal!";

  if (not is_haskell_varid(path.back()) and
      not is_haskell_conid(path.back()) and
      not is_haskell_varsym(path.back()) and
      not is_haskell_consym(path.back()))
    throw myexception()<<"Unqualified name '"<<path.back()<<"' in identifier '"<<s<<"' is not legal!";

  return path;
}

bool is_haskell_varid(const std::string& s)
{
  if (s.empty()) return false;

  if (not islower(s[0])) return false;
  for(int i=1;i<s.size();i++)
  {
    char c = s[i];
    if (not (isupper(c) or islower(c) or isdigit(c) or c=='\''))
      return false;
  }
  return true;
}

bool is_haskell_conid(const std::string& s)
{
  if (s.empty()) return false;

  if (not isupper(s[0])) return false;
  for(int i=1;i<s.size();i++)
  {
    char c = s[i];
    if (not (isupper(c) or islower(c) or isdigit(c) or c=='\''))
      return false;
  }
  return true;
}

bool is_haskell_varsym(const string& s)
{
  static const string symbols = "!#$%&*+./<=>?@\\%|-~:";

  if (not s.size()) return false;

  for(int i=0;i<s.size();i++)
    if (symbols.find(s[i]) == -1)
      return false;

  if (s[0] == ':') return false;

  return true;
}

bool is_haskell_consym(const string& s)
{
  static const string symbols = "!#$%&*+./<=>?@\\%|-~:";

  if (not s.size()) return false;

  for(int i=0;i<s.size();i++)
    if (symbols.find(s[i]) == -1)
      return false;

  if (s[0] != ':') return false;

  return true;
}

bool is_haskell_var_name(const std::string& s)
{
  vector<string> path = split(s,'.');
  if (path.empty()) return false;
  if (not is_haskell_varid(path.back())) return false;
  for(int i=0;i<path.size()-1;i++)
    if (not is_haskell_conid(path[i])) return false;
  return true;
}

bool is_haskell_con_name(const std::string& s)
{
  vector<string> path = split(s,'.');
  if (path.empty()) return false;
  if (not is_haskell_varid(path.back())) return false;
  for(int i=0;i<path.size()-1;i++)
    if (not is_haskell_conid(path[i])) return false;
  return true;
}

bool is_haskell_module_name(const std::string& s)
{
  return is_haskell_con_name(s);
}

bool is_qualified_symbol(const string& s)
{
  return (get_haskell_identifier_path(s).size() >= 2);
}

string get_module_name(const std::string& s)
{
  vector<string> path = get_haskell_identifier_path(s);
  path.pop_back();

  if (not path.size())
    return "";
  else
    return join(path,'.');
}

string get_unqualified_name(const std::string& s)
{
  return get_haskell_identifier_path(s).back();
}

Program& Program::operator+=(const Def& D)
{
  def_function(D.patterns, D.bodies);
  return *this;
}

void Program::def_function(const std::string& name, int arity, int precedence, fixity_t f, const expression_ref& body, const expression_ref& type)
{
  auto loc = symbols.find(name);
  if (loc != symbols.end())
    throw myexception()<<"Can't add function with name '"<<name<<"': that name is already used!";

  declare_symbol({name, variable_symbol, local_scope, arity, precedence, f, body, type});
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

expression_ref Program::get_function(const std::string& name) const
{
  return lookup_symbol(name).body;
}

// A name of "" means that we are defining a top-level program, or a piece of a top-level program.
Program::Program(const std::string& n)
  :name(n)
{ 
  if (not n.size())
    throw myexception()<<"Program name may not be empty!";
}

std::ostream& operator<<(std::ostream& o, const Program& D)
{
  for(const auto& s: D.get_symbols())
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

