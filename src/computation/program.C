#include "computation/program.H"
#include "myexception.H"
#include "computation/graph_register.H"
#include "computation/operations.H"
#include "let-float.H"

using std::pair;
using std::map;
using std::string;
using std::vector;

/*
 * 1. If we are going to put parameter names into the same namespace as everything else, then
 *    (a) add_alias - yes we can add aliases for parameters!
 *    (b) add_symbol - parameters must also live in modules.
 *    (c) declare_symbol - allow declaring parameters.
 *    (d) import_symbol - importing parameters is not only allowed, but required to use them
 *                        if they are declared elsewhere!
 *
 *    (e) Also, we'll need to translate parameter names from model_notes!
 *        - default_value, distributed, etc.
 *
 * 2. How do we handle parameter namespaces? 
 *    (a) We are using those for a totally different purpose than modules!
 *    (b) This is really the fault of the model setup code, not of the programming framework...
 *    (c) Specifically, if we construct mini-programs with these names, then
 *        prefix-formula allows us to keep parameters created with the same name
 *        separate.
 *    (d) Maybe we can think of this more as automatic code generation!
 *        - OK, so suppose we have Module HKY where { ... parameter kappa ... }
 *    (e) So, we have the model graph (statistics) from which we generate a program (computation).
 *
 * 3. How does a BUGS file relate to the program? (This may be more a question about how
 *     we construct MCMC samplers than a question about the underlying framework.)
 *
 * 4. How does Model_Notes relate to the program? (This may also not be a problem with
 *     Context & such; it may instead be a question of programming BUGS samplers.)
 *    (a) The model graph in Model_Notes is logically prior to the program.
 *    (b) Model_Notes represent (sub)models, as in formula_expression_ref.
 *    (c) Question: should each submodel simply define a 'main'?
 *        Or, should it define a 
 *
 * (a) How about notes such as logging frequency, etc.?
 *
 */

symbol_info::symbol_info(const std::string& s, symbol_type_t st, scope_t sc, int i2)
  :name(s), symbol_type(st), scope(sc), arity(i2)
{ }

symbol_info::symbol_info(const std::string& s, symbol_type_t st, scope_t sc, int i2, const expression_ref& b)
  :name(s), symbol_type(st), scope(sc), arity(i2), body(b)
{ }

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
    throw myexception()<<"Can't add alias for unqualified identifier '"<<s<<"' to module '"<<module_name<<"'";

  if (not is_declared_qualified(s))
    throw myexception()<<"Can't add alias for undeclared identifier '"<<s<<"' to module '"<<module_name<<"'";

  string s2 = get_unqualified_name(s);

  if (is_declared_unqualified(s2))
    throw myexception()<<"Trying to add duplicate alias '"<<s2<<"' for identifier '"<<s<<"' to module '"<<module_name<<"'";

  // Mapping from name -> module.name
  aliases[s2] = s;
}

void Program::add_symbol(const symbol_info& S)
{
  if (is_haskell_builtin_con_name(S.name))
    throw myexception()<<"Can't add builtin symbol '"<<S.name<<"'";

  if (is_declared_qualified(S.name))
    throw myexception()<<"Trying to add identifier '"<<S.name<<"' twice to module '"<<module_name<<"'";

  if (S.scope == unknown_scope)
    throw myexception()<<"Can't add symbol with unknown scope!";

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
    throw myexception()<<"Locally defined symbol '"<<S.name<<"' should not be qualified.";

  symbol_info S2 = S;
  S2.name = module_name + "." + S.name;
  add_symbol(S2, local_scope);
  
  // Add the alias for S.name -> S2.name;
  add_alias(S2.name);
}

// "Also like a type signature, a fixity declaration can only occur in the same sequence of declarations as the declaration of the operator itself, and at most one fixity declaration may be given for any operator."

// "Fixity is a property of a particular entity (constructor or variable), just like its type; fixity is not a property of that entity’s name."
void Program::declare_fixity(const std::string& s, int precedence, fixity_t fixity)
{
  if (is_qualified_symbol(s))
    throw myexception()<<"Trying to declare fixity of qualified symbol '"<<s<<"'.  Use its unqualified name.";

  string s2 = module_name + "." + s;

  if (not is_declared_qualified(s2))
    throw myexception()<<"Identifier '"<<s2<<"' not declared.  Cannot set fixity.";

  symbol_info& S = symbols.find(s2)->second;
  if (precedence < 0 or precedence > 9)
    throw myexception()<<"Precedence level "<<precedence<<" not allowed.";
  if (fixity == unknown_fix)
    throw myexception()<<"Cannot set fixity to unknown!";

  S.precedence = precedence;
  S.fixity = fixity;
}

void Program::declare_parameter(const std::string& pname)
{
  declare_parameter(pname, {});
}

void Program::declare_parameter(const std::string& pname, const expression_ref& type)
{
  declare_symbol({pname, parameter_symbol, local_scope, -1, -1, unknown_fix, parameter(pname),type});
}

// Question: what if we import m1.s, which depends on an unimported m2.s?

void Program::import_symbol(const symbol_info& S, bool qualified)
{
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

void Program::import_module(const Program& P2, const string& module_name2, bool qualified)
{
  for(const auto& p: P2.symbols)
  {
    const symbol_info& S = p.second;

    if (get_module_name(S.name) == module_name2)
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
  return is_declared_qualified(name) or is_declared_unqualified(name) or is_haskell_builtin_con_name(name);
}

const symbol_info& Program::lookup_qualified_symbol(const std::string& name) const
{
  if (is_declared_qualified(name))
    return symbols.find(name)->second;
  else
    throw myexception()<<"Qualified name '"<<name<<"' not declared.";
}

const symbol_info& Program::lookup_unqualified_symbol(const std::string& name) const
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

symbol_info Program::lookup_builtin_symbol(const std::string& name) const
{
  if (name == "()")
    return symbol_info("()", constructor_symbol, global_scope, 0, constructor("()",0));
  else if (name == "[]")
    return symbol_info("[]", constructor_symbol, global_scope, 0, constructor("[]",0));
  else if (name == ":")
    return symbol_info(":", constructor_symbol, global_scope, 2, 5, right_fix, lambda_expression( right_assoc_constructor(":",2) ) );
  else if (is_tuple_name(name))
  {
    int arity = name.size() - 1;
    expression_ref body = lambda_expression( tuple_head(arity) );
    return symbol_info(name, constructor_symbol, global_scope, arity, body);
  }
  throw myexception()<<"Symbol 'name' is not a builtin (constructor) symbol.";
}

symbol_info Program::lookup_symbol(const std::string& name) const
{
  if (is_haskell_builtin_con_name(name))
    return lookup_builtin_symbol(name);
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
  if (S.precedence == -1 or S.fixity == unknown_fix) 
  {
    // If either is unset, then both must be unset!
    assert(S.precedence == -1 and S.fixity == unknown_fix);
    S.precedence = 9;
    S.fixity = left_fix;
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

vector<string> haskell_name_path(const std::string& s)
{
  if (s == ".") return {s};
  else if (s.size() >= 2 and s.substr(s.size()-2,2) == "..")
  {
    vector<string> path = split(s.substr(0,s.size()-2),'.');
    path.push_back(".");
    return path;
  }
  else
    return split(s,'.');
}

vector<string> get_haskell_identifier_path(const std::string& s)
{
  if (not s.size())
    throw myexception()<<"Empty string is not a legal Haskell identifier!";

  vector<string> path = haskell_name_path(s);

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
  if (s == ":") return false;

  return true;
}

bool is_haskell_var_name(const std::string& s)
{
  vector<string> path = haskell_name_path(s);
  if (path.empty()) return false;
  if (not is_haskell_varid(path.back()) and not is_haskell_varsym(path.back())) return false;
  for(int i=0;i<path.size()-1;i++)
    if (not is_haskell_conid(path[i])) return false;
  return true;
}

bool is_haskell_builtin_con_name(const std::string& s)
{
  if (s == "()" or s == "[]" or s == ":" or is_tuple_name(s)) 
    return true;
  else
    return false;
}

bool is_haskell_normal_con_name(const std::string& s)
{
  vector<string> path = haskell_name_path(s);
  if (path.empty()) return false;
  if (not is_haskell_conid(path.back()) and not is_haskell_consym(path.back())) return false;
  for(int i=0;i<path.size()-1;i++)
    if (not is_haskell_conid(path[i])) return false;
  return true;
}

bool is_haskell_con_name(const std::string& s)
{
  return (is_haskell_builtin_con_name(s) or is_haskell_normal_con_name(s));
}

bool is_haskell_module_name(const std::string& s)
{
  return is_haskell_normal_con_name(s);
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

void Program::def_function(const std::string& name, int arity, const expression_ref& body, const expression_ref& type)
{
  auto loc = symbols.find(name);
  if (loc != symbols.end())
    throw myexception()<<"Can't add function with name '"<<name<<"': that name is already used!";

  declare_symbol({name, variable_symbol, local_scope, arity, -1, unknown_fix, body, type});
}

void Program::def_function(const std::string& name, int arity, const expression_ref& body)
{
  def_function(name, arity, body, {});
}

void Program::def_function(const vector<expression_ref>& patterns, const vector<expression_ref>& bodies)
{
  assert(patterns.size());
  assert(patterns.size() == bodies.size());

  string name;
  vector< vector<expression_ref> > sub_patterns(patterns.size());
  parse_combinator_application(patterns[0], name, sub_patterns[0]);
  int arity = sub_patterns[0].size();

  for(int i=1;i<patterns.size();i++)
  {
    string name2;
    parse_combinator_application(patterns[i], name2, sub_patterns[i]);

    if (name != name2) throw myexception()<<"In definition of function '"<<name<<"', incorrect function name '"<<name2<<"' found as well.";
  }

  expression_ref E = ::def_function(sub_patterns, bodies);
  def_function(name, arity, E);
}

void Program::def_constructor(const std::string& s, int arity)
{
  symbol_info S(module_name+"."+s, constructor_symbol, local_scope, arity, -1, unknown_fix);
  S.body = lambda_expression( constructor(s, arity) );
  add_symbol( S );
}

expression_ref Program::get_function(const std::string& name) const
{
  return lookup_symbol(name).body;
}

// A name of "" means that we are defining a top-level program, or a piece of a top-level program.
Program::Program(const std::string& n)
  :module_name(n)
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

