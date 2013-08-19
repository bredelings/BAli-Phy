#include <set>
#include "computation/module.H"
#include "myexception.H"
#include "computation/graph_register.H"
#include "computation/operations.H"
#include "let-float.H"
#include "parser/parse.H"
#include "parser/desugar.H"
#include "parser/AST.H"
#include "computation/loader.H"

using std::pair;
using std::map;
using std::set;
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
 *
 * 5. Remove 'arity' argument from def_function?
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

bool operator==(const symbol_info&S1, const symbol_info& S2)
{
  return (S1.name == S2.name) and (S1.symbol_type == S2.symbol_type) and (S1.scope == S2.scope) and
    (S1.arity == S2.arity) and (S1.precedence == S2.precedence) and (S1.fixity == S2.fixity) and
    (S1.body == S2.body) and (S1.type == S2.type);
}

bool operator!=(const symbol_info&S1, const symbol_info& S2)
{
  return not (S1 == S2);
}

bool Module::symbol_exists(const string& name) const
{
  auto loc = symbols.find(name);
  if (loc == symbols.end())
    return false;
  else
    return true;
}

void Module::add_symbol(const symbol_info& S)
{
  if (is_haskell_builtin_con_name(S.name))
    throw myexception()<<"Can't add builtin symbol '"<<S.name<<"'";

  if (not is_qualified_symbol(S.name))
    throw myexception()<<"Symbol '"<<S.name<<"' unqualified, can't be added to symbol table";

  if (S.scope == unknown_scope)
    throw myexception()<<"Can't add symbol with unknown scope!";

  auto loc = symbols.find(S.name);
  if (loc == symbols.end())
    symbols[S.name] = S;
  else if (loc != symbols.end() and loc->second != S)
    throw myexception()<<"Trying to add symbol '"<<S.name<<"' twice to module '"<<name<<"' with different body";
}

void Module::add_symbol(const symbol_info& S, scope_t sc)
{
  symbol_info S2 = S;
  S2.scope = sc;
  add_symbol(S2);
}

void Module::add_alias(const string& identifier_name, const string& resolved_name)
{
  if (not symbol_exists(resolved_name))
    throw myexception()<<"Can't add alias '"<<identifier_name<<"' -> '"<<resolved_name<<"' in module '"<<name<<"' because '"<<resolved_name<<"' is neither declared nor imported.";

  std::pair<string,string> element(identifier_name,resolved_name);

  int count = aliases.count(identifier_name);
  assert(count == 0 or count == 1);

  if (count == 0)
    aliases.insert( std::pair<string,string>(identifier_name, resolved_name) );
}

void Module::declare_symbol(const symbol_info& S)
{
  if (is_qualified_symbol(S.name))
    throw myexception()<<"Locally defined symbol '"<<S.name<<"' should not be qualified in declaration.";

  symbol_info S2 = S;
  S2.name = name + "." + S.name;

  if (symbol_exists(S2.name))
    throw myexception()<<"Trying to declare '"<<S.name<<"' twice in module '"<<name<<"'";

  // Add the symbol first.
  add_symbol(S2, local_scope);
  // Add the alias for qualified name: S.name -> S2.name;
  add_alias(S2.name, S2.name);
  // Add the alias for unqualified name: S.name -> S2.name;
  add_alias(S.name, S2.name);
}

// "Also like a type signature, a fixity declaration can only occur in the same sequence of declarations as the declaration of the operator itself, and at most one fixity declaration may be given for any operator."

// "Fixity is a property of a particular entity (constructor or variable), just like its type; fixity is not a property of that entity’s name."
void Module::declare_fixity(const std::string& s, int precedence, fixity_t fixity)
{
  if (is_qualified_symbol(s))
    throw myexception()<<"Trying to declare fixity of qualified symbol '"<<s<<"'.  Use its unqualified name.";

  string s2 = name + "." + s;

  if (not symbol_exists(s2))
    declare_symbol({s, unknown_symbol, local_scope, -1, -1, unknown_fix, {}, {}});

  symbol_info& S = symbols.find(s2)->second;
  if (precedence < 0 or precedence > 9)
    throw myexception()<<"Precedence level "<<precedence<<" not allowed.";
  if (fixity == unknown_fix)
    throw myexception()<<"Cannot set fixity to unknown!";

  S.precedence = precedence;
  S.fixity = fixity;
}

void Module::declare_parameter(const std::string& pname)
{
  declare_parameter(pname, {});
}

void Module::declare_parameter(const std::string& pname, const expression_ref& type)
{
  declare_symbol({pname, parameter_symbol, local_scope, -1, -1, unknown_fix, parameter(pname),type});
}

void Module::add_import(bool qualified, const string& modid)
{
  vector<expression_ref> sub;
  if (qualified)
    sub.push_back(String("qualified"));
  sub.push_back(String(modid));
  
  add_impdecl({AST_node("impdecl"),sub});
}

void Module::add_import_as(bool qualified, const string& modid, const string& modid2)
{
  vector<expression_ref> sub;
  if (qualified)
    sub.push_back(String("qualified"));
  sub.push_back(String(modid));
  sub.push_back(String("as"));
  sub.push_back(String(modid2));
  
  add_impdecl({AST_node("impdecl"),sub});
}

void Module::add_impdecl(const expression_ref& impdecl)
{
  vector<expression_ref> sub;
  if (impdecls)
    sub = impdecls->sub;
  sub.push_back(impdecl);
  impdecls = {AST_node("impdecls"),sub};
}

// Question: what if we import m1.s, which depends on an unimported m2.s?
void Module::import_symbol(const symbol_info& S, const string& modid, bool qualified)
{
  if (not is_qualified_symbol(S.name))
    throw myexception()<<"Imported symbols must have qualified names.";

  // Add the symbol.
  add_symbol(S, external_scope);
  // Add the alias for qualified name.
  add_alias(modid+"."+get_unqualified_name(S.name), S.name);
  // Add the alias for unqualified name.
  if (not qualified)
    add_alias(get_unqualified_name(S.name), S.name);
}

void Module::import_module(const Module& M2, const string& modid, bool qualified)
{
  assert(modid != name);

  for(const auto& p: M2.symbols)
  {
    const symbol_info& S = p.second;

    if (S.scope == local_scope)
      import_symbol(S, modid, qualified);
  }
}

void Module::import_module(const Module& M2, bool qualified)
{
  import_module(M2, M2.name, qualified);
}

Module find_module(const string& module_name, const std::vector<Module>& P)
{
  for(const auto& module: P)
    if (module.name == module_name)
      return module;
  std::abort();
}

std::set<std::string> Module::dependencies() const
{
  if (not impdecls) return {};
  
  set<string> module_names;

  for(const auto& impdecl:impdecls->sub)
  {
    int i=0;
    bool qualified = *impdecl->sub[0].is_a<String>() == "qualified";
    if (qualified) i++;

    bool submodel = *impdecl->sub[i].is_a<String>() == "submodel";
    if (submodel) i++;

    // bail out if this is a submodel declaration.
    if (submodel) continue;
    
    string imp_module_name = *impdecl->sub[i++].is_a<String>();
    module_names.insert(imp_module_name);
  }

  return module_names;
}

// in module m0: import [qualified] submodel [m1] as [m2]
map<string,string> Module::submodel_dependencies() const
{
  if (not impdecls) return {};
  
  map<string,string> module_names;

  for(const auto& impdecl:impdecls->sub)
  {
    int i=0;
    bool qualified = *impdecl->sub[0].is_a<String>() == "qualified";
    if (qualified) i++;

    bool submodel = *impdecl->sub[i].is_a<String>() == "submodel";
    if (submodel) i++;

    // bail out if this is NOT a submodel declaration.
    if (not submodel) continue;
    
    string imp_module_name = *impdecl->sub[i++].is_a<String>();

    string imp_module_name_as = imp_module_name;
    if (i < impdecl->sub.size() and *impdecl->sub[i++].is_a<String>() == "as")
      imp_module_name_as = *impdecl->sub[i++].is_a<String>();

    // Store map from m1 -> m0.m2
    module_names.insert({imp_module_name, name+"."+imp_module_name_as});
  }

  return module_names;
}

void Module::resolve_symbols(const std::vector<Module>& P)
{
  if (resolved) return;
  resolved = true;

  bool saw_Prelude = false;
  if (impdecls)
    for(const auto& impdecl:impdecls->sub)
    {
      int i=0;
      bool qualified = *impdecl->sub[0].is_a<String>() == "qualified";
      if (qualified) i++;
    
      bool submodel = *impdecl->sub[i].is_a<String>() == "submodel";
      if (submodel) i++;
    
      string imp_module_name = *impdecl->sub[i++].is_a<String>();
      
      string imp_module_name_as = imp_module_name;
      if (i < impdecl->sub.size() and *impdecl->sub[i++].is_a<String>() == "as")
	imp_module_name_as = *impdecl->sub[i++].is_a<String>();
      
      assert(i == impdecl->sub.size());
      
      if (submodel){
	// module M where { import submodel A as B } => 
	// 1. load A
	// 2. Rename it to M.B
	// 3. Import it (e.g M.B) as B;
	
	// Note that the loaded module is imp_module_name, though.
	imp_module_name = name+"."+imp_module_name_as;
      }

      Module M = find_module(imp_module_name,P);

      import_module(M, imp_module_name_as, qualified);
      if (imp_module_name == "Prelude")
	saw_Prelude = true;
    }

  // Import the Prelude if it wasn't explicitly mentioned in the import list.
  if (not saw_Prelude and name != "Prelude")
  {
    Module M = find_module("Prelude",P);
    import_module(M,"Prelude",false);
  }

  if (not topdecls) return;

  // 1. Desugar the module
  expression_ref decls = desugar(*this,topdecls);
  
  // 2. Convert top-level dummies into global vars, in both decls AND notes.
  vector<expression_ref> decls_sub = decls->sub;
  for(auto& decl: decls_sub)
    for(auto& p: symbols)
    {
      const auto& S = p.second;
      if (S.symbol_type != variable_symbol) continue;
      if (S.scope != local_scope) continue;
      
      string qname = S.name;
      string name = get_unqualified_name(qname);
      
      decl = substitute(decl,dummy(qname),identifier(qname));
      decl = substitute(decl,dummy( name),identifier(qname));
    }
  
  // 3. Add notes
  for(const auto& note: decls_sub)
    if (is_AST(note,"BugsDataDist") or
	is_AST(note,"BugsExternalDist") or
	is_AST(note,"BugsDist") or
	is_AST(note,"BugsDefaultValue") or
	is_AST(note,"BugsNote"))
      add_note(note->sub[0]);

  // 4. Define the symbols
  for(const auto& decl: decls_sub)
    if (is_AST(decl,"Decl"))
    {
      string name = decl->sub[0].assert_is_a<identifier>()->name;
      symbols.at(name).body = decl->sub[1];
    }
}

void Module::load_builtins(const module_loader& L)
{
  const string builtin_prefix = "builtin_function_";

  if (not topdecls) return;

  for(const auto& decl: topdecls->sub)
    if (is_AST(decl,"Builtin"))
    {
      string function_name = *decl->sub[0].assert_is_a<String>();
      int n = *decl->sub[1].assert_is_a<Int>();
      string symbol_name = *decl->sub[2].assert_is_a<String>();
      string plugin_name = symbol_name;

      if (decl->sub.size() > 3)
	plugin_name = *decl->sub[3].assert_is_a<String>();

      function_name = lookup_symbol(function_name).name;

      symbols.at(function_name).body = load_builtin(L, builtin_prefix + symbol_name, plugin_name, n, function_name);
    }
}

bool Module::is_declared(const std::string& name) const
{
  return is_haskell_builtin_con_name(name) or (aliases.count(name) > 0);
}

bool Module::is_declared_local(const std::string& name) const
{
  auto loc = symbols.find(name);
  if (loc == symbols.end()) return false;

  return (loc->second.scope == local_scope);
}

symbol_info Module::lookup_builtin_symbol(const std::string& name)
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

symbol_info Module::lookup_symbol(const std::string& name) const
{
  if (is_haskell_builtin_con_name(name))
    return lookup_builtin_symbol(name);

  int count = aliases.count(name);
  if (count == 0)
    throw myexception()<<"Indentifier '"<<name<<"' not declared.";
  else if (count == 1)
  {
    string symbol_name = aliases.find(name)->second;
    if (not symbol_exists(symbol_name))
      throw myexception()<<"Identifier '"<<name<<"' -> '"<<symbol_name<<"', which does not exist!";
    return symbols.find(symbol_name)->second;
  }
  else
  {
    myexception e;
    e<<"Identifier '"<<name<<" is ambiguous!";
    auto range = aliases.equal_range(name);
    for(auto i = range.first; i != range.second ;i++)
      e<<"\n "<<i->first<<" -> "<<i->second;
    throw e;
  }
}

symbol_info Module::get_operator(const string& name) const
{
  symbol_info S = lookup_symbol(name);

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

  assert_is_a<Apply>(E);
  
  // 1. Find the head.  This should be a var, not an apply.
  object_ptr<const identifier> V = assert_is_a<identifier>(E->sub[0]);

  // 2. Look through the arguments
  for(int i=1;i<E->size();i++)
    patterns.push_back(E->sub[i]);

  if (not V)
    throw myexception()<<"Combinator definition '"<<E<<"' does not start with variable!";
  name = V->name;
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
      not is_haskell_consym(path.back()) and
      not is_haskell_builtin_con_name(path.back()))
    throw myexception()<<"Unqualified name '"<<path.back()<<"' in identifier '"<<s<<"' is not legal!";

  return path;
}

bool haskell_is_lower(char c)
{
  return (islower(c) or c=='_');
}

bool is_haskell_varid(const std::string& s)
{
  if (s.empty()) return false;

  if (not haskell_is_lower(s[0])) return false;
  for(int i=1;i<s.size();i++)
  {
    char c = s[i];
    if (not (isupper(c) or haskell_is_lower(c) or isdigit(c) or c=='\''))
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
    if (not (isupper(c) or haskell_is_lower(c) or isdigit(c) or c=='\''))
      return false;
  }
  return true;
}

bool is_haskell_varsym(const string& s)
{
  static const string symbols = "!#$%&*+./<=>?@\\^|-~:";

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

Module& Module::operator+=(const char* s)
{
  return operator+=(string(s));
}

Module& Module::operator+=(const string& s)
{
  return operator+=(parse_haskell_decls(s));
}

// Here we do only phase 1 -- we only parse the decls enough to
//   determine which is a variable, and which are e.g. constructors.
// We can't determine function bodies at all, since we can't even handle op definitions
//   before we know fixities!

string get_function_name(const expression_ref& E)
{
  if (is_AST(E,"funlhs1"))
  {
    expression_ref f = E->sub[0];
    assert(is_AST(f,"id"));

    return f.assert_is_a<AST_node>()->value;
  }
  else if (is_AST(E,"funlhs2"))
  {
    expression_ref f = E->sub[1];
    assert(is_AST(f,"id"));
    return f.assert_is_a<AST_node>()->value;
  }
  else if (is_AST(E,"funlhs3"))
    return get_function_name(E->sub[0]);
  std::abort();
}

set<string> find_bound_vars(const expression_ref& E);
set<string> find_all_ids(const expression_ref& E);

Module& Module::operator+=(const expression_ref& E)
{
  expression_ref decls = E;

  if (is_AST(E,"Module"))
  {
    if (module)
      throw myexception()<<"Can't load new module over old module '"<<name<<"'";

    module = E;

    // 1. module = [optional name] + body
    if (module->sub.size() == 1)
      body = module->sub[0];
    else
    {
      string module_name2 = *module->sub[0].is_a<String>();
      if (not name.empty() and name != module_name2)
	throw myexception()<<"Overwriting module name '"<<name<<"' with '"<<module_name2<<"'";
      name = module_name2;

      body = module->sub[1];
    }
    assert(is_AST(body,"Body"));

    // 2. body = impdecls + [optional topdecls]
    for(const auto& E: body->sub)
      if (is_AST(E,"TopDecls"))
	topdecls = E;
      else if (is_AST(E,"impdecls"))
	impdecls = E;
    
    // 3. Do imports.
    if (impdecls)
    {
      for(const auto& impdecl:impdecls->sub)
      {
	int i=0;
	bool qualified = *impdecl->sub[0].is_a<String>() == "qualified";
	if (qualified) i++;

	string imp_module_name = *impdecl->sub[i++].is_a<String>();

	string imp_module_name_as = imp_module_name;
	if (i < impdecl->sub.size() and *impdecl->sub[i++].is_a<String>() == "as")
	  imp_module_name_as = *impdecl->sub[i++].is_a<String>();

	assert(i == impdecl->sub.size());
      }
    }

    if (not topdecls)
      return *this;
    else
      decls = topdecls;
  }

  assert(is_AST(decls,"Decls") or is_AST(decls,"TopDecls"));

  // 0. Get names that are being declared.
  for(const auto& decl: decls->sub)
    if (is_AST(decl,"FixityDecl"))
    {
      // Determine fixity.
      string f = *decl->sub[0].assert_is_a<String>();
      fixity_t fixity = unknown_fix;
      if (f == "infixl")
	fixity = left_fix;
      else if (f == "infixr")
	fixity = right_fix;
      else if (f == "infix")
	fixity = non_fix;
      else
	std::abort();

      // Determine precedence.
      int precedence = 9;
      if (decl->sub.size() == 3)
	precedence = *decl->sub[1].assert_is_a<Int>();

      // Find op names and declare fixity and precedence.
      for(const auto& op: decl->sub.back()->sub)
      {
	string name = *op.assert_is_a<String>();
	declare_fixity(name, precedence, fixity);
      }
    }
    else if (is_AST(decl,"Decl"))
    {
      expression_ref lhs = decl->sub[0];
      set<string> vars;
      if (is_AST(lhs,"funlhs1") or is_AST(lhs,"funlhs2") or is_AST(lhs,"funlhs3"))
	vars.insert( get_function_name(lhs) );
      else
	vars = find_bound_vars(lhs);

      for(const auto& var_name: vars)
      {
	// We don't know the type yet, probably, because we don't know the body.
	string qualified_name = name+"."+var_name;
	auto loc = symbols.find(qualified_name);

	if (loc != symbols.end())
	{
	  symbol_info& S = loc->second;
	  // Only the fixity has been declared!
	  if (S.symbol_type == unknown_symbol and not S.body and not S.type)
	    S.symbol_type = variable_symbol;
	}
	else
	  def_function(var_name,{});
      }
    }
    else if (is_AST(decl,"Builtin"))
    {
      string bname = *decl->sub[0].assert_is_a<String>();
      def_function(bname,{});
    }
    else if (is_AST(decl,"Decl:data"))
    {
      if (decl->sub.size() >= 2)
      {
	expression_ref constrs = decl->sub[1];
	assert(is_AST(constrs,"constrs"));
	for(const auto& constr: constrs->sub)
	{
	  if (is_AST(constr,"constr"))
	  {
	    string name = *constr->sub[0].is_a<String>();
	    int arity = constr->sub.size() - 1;
	    def_constructor(name,arity);
	  }
	  else if (is_AST(constr,"constr_op"))
	  {
	    string name = *constr->sub[1].is_a<String>();
	    int arity = 2;
	    def_constructor(name,arity);
	  }
	  else
	    std::abort();
	}
      }
    }
      
  // 3. Find explicitly and implicitly-declared parameters
  set<string> parameters;
  for(const auto& cmd: decls->sub)
  {
    // This doesn't happen for BugsExternalDist or BugsDataDist
    if (is_AST(cmd,"BugsDist"))
      add(parameters, find_all_ids(cmd->sub[0]));
    else if (is_AST(cmd,"Parameter"))
    {
      string name = *(cmd->sub[0].assert_is_a<String>());
      parameters.insert(name);
    }
  }
  
  // 4. Actually declare the parameters
  for(const auto& name: parameters)
    if (not is_declared(name))
      declare_parameter(name);

  if (module) return *this;

  if (not topdecls)
    topdecls = decls;

  // This means that operator::+=() can only be called once with a module, and once without.
  // \todo FIXME:cleanup - Make this part of the function body then, and allow constructing the module from it.
  assert(decls == topdecls);

  return *this;
}

void Module::def_function(const std::string& fname, const expression_ref& body, const expression_ref& type)
{
  if (is_qualified_symbol(fname))
    throw myexception()<<"Locally defined symbol '"<<fname<<"' should not be qualified in function declaration.";

  string qualified_name = name+"."+fname;
  auto loc = symbols.find(qualified_name);

  if (loc != symbols.end())
  {
    symbol_info& S = loc->second;
    // Only the fixity has been declared!
    if (S.symbol_type == unknown_symbol and not S.body and not S.type)
    {
      S.symbol_type = variable_symbol;
      S.body = body;
      S.type = type;
    }
    else 
      throw myexception()<<"Can't add function with name '"<<fname<<"': that name is already used!";
  }
  else
    declare_symbol({fname, variable_symbol, local_scope, -1, -1, unknown_fix, body, type});
}

void Module::def_function(const std::string& fname, const expression_ref& body)
{
  def_function(fname, body, {});
}

void Module::def_function(const vector<expression_ref>& patterns, const vector<expression_ref>& bodies)
{
  assert(patterns.size());
  assert(patterns.size() == bodies.size());

  string fname;
  vector< vector<expression_ref> > sub_patterns(patterns.size());
  parse_combinator_application(patterns[0], fname, sub_patterns[0]);

  for(int i=1;i<patterns.size();i++)
  {
    string fname2;
    parse_combinator_application(patterns[i], fname2, sub_patterns[i]);

    if (fname != fname2) throw myexception()<<"In definition of function '"<<fname<<"', incorrect function name '"<<fname2<<"' found as well.";
  }

  expression_ref E = ::def_function(sub_patterns, bodies);
  def_function(fname, E);
}

void Module::def_constructor(const std::string& cname, int arity)
{
  if (is_qualified_symbol(cname))
    throw myexception()<<"Locally defined symbol '"<<cname<<"' should not be qualified.";

  string qualified_name = name+"."+cname;
  expression_ref body = lambda_expression( constructor(qualified_name, arity) );

  auto loc = symbols.find(qualified_name);
  if (loc != symbols.end())
  {
    symbol_info& S = loc->second;
    // Only the fixity has been declared!
    if (S.symbol_type == unknown_symbol and not S.body and not S.type)
    {
      S.symbol_type = constructor_symbol;
      S.body = body;
      return;
    }
  }

  declare_symbol( {cname, constructor_symbol, local_scope, arity, -1, unknown_fix, body, {}} );
}

expression_ref Module::get_function(const std::string& fname) const
{
  return lookup_symbol(fname).body;
}

vector<string> Module::parameter_names() const
{
  vector<string> names;
  for(const auto& x:get_symbols())
    if (x.second.symbol_type == parameter_symbol and x.second.scope == local_scope)
      names.push_back(x.first);
  return names;
}

int Module::n_parameters() const
{
  return parameter_names().size();
}

// A name of "" means that we are defining a top-level program, or a piece of a top-level program.
Module::Module(const string& n)
  :name(n)
{ 
  if (not n.size())
    throw myexception()<<"Module name may not be empty!";
}

Module::Module(const char *n)
  :Module(string(n))
{ }

Module::Module(const expression_ref& E)
{ 
  (*this) += E;
}

std::ostream& operator<<(std::ostream& o, const Module& D)
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

expression_ref resolve_refs(const vector<Module>& P, const expression_ref& E)
{
  // Replace parameters with the appropriate reg_var: of value parameter( )
  if (object_ptr<const parameter> p = is_a<parameter>(E))
  {
    string name = p->parameter_name;
    if (not is_qualified_symbol(name))
      for(const auto& module: P)
	if (module.is_declared(name))
	{
	  symbol_info S = module.lookup_symbol(name);
	  assert(S.symbol_type = parameter_symbol);
	  string qualified_name = S.name;
	  return parameter(qualified_name);
	}
  }

  // Replace parameters with the appropriate reg_var: of value whatever
  if (object_ptr<const identifier> V = is_a<identifier>(E))
  {
    string name = V->name;
    if (not is_qualified_symbol(name))
    {
      for(const auto& module: P)
	if (module.is_declared(name))
	{
	  symbol_info S = module.lookup_symbol(name);
	  assert(S.symbol_type = parameter_symbol);
	  string qualified_name = S.name;
	  return identifier(qualified_name);
	}
      throw myexception()<<"Can't find any module for unqualified symbol '"<<name<<"'";
    }
  }

  // Other constants have no parts, and don't need to be resolved
  if (not E->size()) return E;

  // Resolve the parts of the expression
  object_ptr<expression> V ( new expression(*E) );
  for(int i=0;i<V->size();i++)
    V->sub[i] = resolve_refs(P, V->sub[i]);

  return V;
}

string rename_module(const string& s, const string& modid1, const string& modid2)
{
  if (not is_qualified_symbol(s)) return s;

  string modid = get_module_name(s);
  string name = get_unqualified_name(s);

  if (modid == modid1)
    return modid2 + "." + name;
  else
    return s;
}


// Rename parts of the AST!
expression_ref rename_module(const expression_ref& E, const std::string& modid1, const std::string& modid2)
{
  if (object_ptr<const AST_node> n = E.is_a<AST_node>())
  {
    if (n->type == "Module")
    {
      vector<expression_ref> sub = E->sub;
      if (sub.size() == 1)
	sub[0] = rename_module(sub[1], modid1, modid2);
      else if (sub.size() == 2)
      {
	string modid = *sub[0].is_a<String>();
	if (modid == modid1) modid = modid2;
	sub[0] = String(modid);
	sub[1] = rename_module(sub[1], modid1, modid2);
      }
      return {AST_node("Module"),sub};
    }
    // Rename ids
    if (n->type == "id")
      return AST_node("id",rename_module(n->value, modid1, modid2));
  }

  // Rename parameters
  else if (auto p = is_a<parameter>(E))
    return parameter( rename_module(p->parameter_name, modid1, modid2) );

  // Rename vars
  else if (auto V = is_a<identifier>(E))
    return identifier( rename_module(V->name, modid1, modid2) );

  // Rename dummies
  else if (auto D = is_a<dummy>(E))
  {
    dummy d2 = *D;
    if (d2.name.size() and is_qualified_symbol(d2.name))
      d2.name = rename_module(d2.name, modid1, modid2);
    return d2;
  }

  // Other constants have no parts, and don't need to be resolved
  if (not E->size()) return E;

  // Resolve the parts of the expression
  object_ptr<expression> V ( new expression(*E) );
  for(int i=0;i<V->size();i++)
    V->sub[i] = rename_module(V->sub[i], modid1, modid2);

  return V;

}
