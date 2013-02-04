#include "computation/program.H"
#include "computation/module.H"
#include "myexception.H"
#include "computation/loader.H"
#include "models/model.H"

using std::vector;
using std::set;
using std::map;
using std::string;

bool contains_module(const vector<Module>& P, const string& module_name)
{
  return find_module(P,module_name) != -1;
}

int find_module(const vector<Module>& P, const string& module_name)
{
  for(int i=0;i<P.size();i++)
    if (P[i].name == module_name)
      return i;
  return -1;
}

const Module& get_module(const vector<Module>& P, const string& module_name)
{
  int index = find_module(P,module_name);
  if (index == -1)
    throw myexception()<<"Progam does not contain module '"<<module_name<<"'";
  return P[index];
}

vector<string> module_names(const vector<Module>& P)
{
  vector<string> names;
  for(const auto& module: P)
    names.push_back(module.name);
  return names;
}

string module_names_path(const vector<Module>& P)
{
  return "["+join(module_names(P),",") +"]";
}

set<string> module_names_set(const vector<Module>& P)
{
  set<string> names;
  for(const auto& module: P)
    names.insert(module.name);
  return names;
}

static int count_module(const vector<Module>& P,const string& module_name)
{
  int count = 0;
  for(const auto& module: P)
    if (module.name == module_name)
      count++;
  return count;
}

set<string> unresolved_imports(const vector<Module>& P)
{
  set<string> modules_to_add;

  // Add dependencies on modules
  for(const auto& module: P)
    for(const string& module_name: module.dependencies())
      if (not contains_module(P, module_name))
	modules_to_add.insert(module_name);

  return modules_to_add;
}

map<string,string> unresolved_submodel_imports(const vector<Module>& P)
{
  map<string,string> submodels_to_add;

  // Add dependencies on modules
  for(const auto& module: P)
    for(const auto& x: module.submodel_dependencies())
      if (not contains_module(P, x.second))
	submodels_to_add.insert(x);

  return submodels_to_add;
}

void add_missing_imports(const module_loader& L, vector<Module>& P, Model_Notes& N)
{
  // 1. Perform a closure over missing modules.
  std::set<string> modules_to_add;
  std::map<string,string> submodels_to_add;

  do
  {
    for(const string& module_name: modules_to_add)
      P.push_back(load_module(L, module_name));

    for(const auto& x: submodels_to_add)
      P.push_back(load_and_rename_module(L, x.first, x.second));

    modules_to_add = unresolved_imports(P);
    submodels_to_add = unresolved_submodel_imports(P);
  } 
  while (not modules_to_add.empty() or not submodels_to_add.empty());
  
  // 2. Process new modules and 
  for(auto& module: P)
    if (not module.is_resolved())
    {
      try {
	module.load_builtins(L);
	module.resolve_symbols(P);
	N.add_notes( module.get_notes() );
      }
      catch (myexception& e)
      {
	std::ostringstream o;
	o<<"In module '"<<module.name<<"': ";
	e.prepend(o.str());
	throw e;
      }
    }
  
}

void add(const module_loader& L, vector<Module>& P, Model_Notes& N, const vector<Module>& modules)
{
  // Get module_names, but in a set<string>
  set<string> old_module_names = module_names_set(P);

  // 1. Check that the program doesn't already contain these module names.
  for(const auto& module: modules)
    if (contains_module(P,module.name))
      throw myexception()<<"Trying to add duplicate module '"<<module.name<<"' to program "<<module_names_path(P);

  // 2. Check that we aren't adding any module twice.
  for(const auto& module: modules) 
  {
    int count = count_module(modules, module.name);
    if (count > 1)
      throw myexception()<<"Trying to add module '"<<module.name<<"' to program "<<module_names_path(P)<<" "<<count<<" times.";
  }

  // 3. Actually add the modules.
  P.insert(P.end(), modules.begin(), modules.end());

#ifndef NDEBUG
  // 4. Assert that every module exists only once in the list.
  for(const auto& module: P)
    assert(count_module(P, module.name) == 1);
#endif

  // 5. Import any modules that are (transitively) implied by the ones we just loaded.
  add_missing_imports(L, P, N);
}

bool is_declared(const vector<Module>& modules, const string& qvar)
{
  if (is_haskell_builtin_con_name(qvar)) return true;

  if (not is_qualified_symbol(qvar))
    throw myexception()<<"Can't search program for non-builtin, unqualified varid '"<<qvar<<"'";

  for(const auto& module: modules)
    if (module.is_declared_local(qvar))
      return true;

  return false;
}

map<string,string> get_simplified_names(const set<string>& names)
{
  // 1. Construct mapping from unqualified names to qualified names.
  std::multimap<string,string> aliases;
  for(const string& name: names)
    aliases.insert({get_unqualified_name(name), name});

  // 2. Invert the mapping if the unqualified name maps to only 1 qualified name.
  map<string,string> simplified;
  for(auto current = aliases.begin();current != aliases.end();)
  {
    int count = 1;
    auto next = current;
    next++;
    while(next != aliases.end() and next->first == current->first)
    {
      // The same qualified name should not occur twice.
      assert(next->second != current->second);
      count++;
      next++;
    }

    if (count == 1)
      simplified[current->second] = current->first;

    current = next;
  }

  return simplified;
}

map<string,string> get_simplified_names(const vector<Module>& P)
{
  vector<string> parameter_names;

  // 1. Find all parameters and variables
  set<string> names;
  for(const auto& module: P)
    for(const auto& S: module.get_symbols())
      if (S.second.scope == local_scope)
      {
	string name = S.first;
	symbol_type_t T = S.second.symbol_type;
	if (T == parameter_symbol)
	  parameter_names.push_back(name);
	if (T == variable_symbol or T == parameter_symbol or T == constructor_symbol)
	  names.insert(name);
      }

  // 2. Map qualified names to their unqualified versions IF there is only one occurrence of the unqualified version.
  map<string,string> simplified = get_simplified_names(names);

  // 3. Map long parameter names to their shortened equivalent
  // \todo FIXME:extend This doesn't handle parameter names clashing with variable names
  vector<string> short_names = short_parameter_names(parameter_names);
  for(int i=0;i<parameter_names.size();i++)
  {
    // If the short name isn't shorter, then stop recording a simplification
    if (short_names[i] == parameter_names[i])
    {
      auto loc = simplified.find(parameter_names[i]);
      if (loc != simplified.end())
	simplified.erase(parameter_names[i]);
    }
    // If we simplify down to a short name, then either 
    //  (a) We are already using that short name, or 
    //  (b) its ambiguous.
    // In both cases, don't do anything different.
    else if (not is_qualified_symbol(short_names[i]))
      ;
    // If we simplified, but left some prefix components, then check that there isn't any variable named this.
    // FIXME:extend Actually, if this is a parameter, then its OK. So, look up the symbol, and check if its a var.
    else if (not is_declared(P, short_names[i]))
      simplified[parameter_names[i]] = short_names[i];
  }

  return simplified;
}

expression_ref map_symbol_names(const expression_ref& E, const std::map<string,string>& simplify)
{
  if (not E->size())
  {
    if (auto V = is_a<identifier>(E))
    {
      auto loc = simplify.find(V->name);
      if (loc != simplify.end())
	return identifier(loc->second);
      else
	return E;
    }
    else if (auto P = is_a<parameter>(E))
    {
      auto loc = simplify.find(P->parameter_name);
      if (loc != simplify.end())
	return parameter(loc->second);
      else
	return E;
    }
    else
      return E;
  }

  object_ptr<expression> V = E->clone();
  for(int i=0;i<E->size();i++)
    V->sub[i] = map_symbol_names(V->sub[i], simplify);
  return V;
}

expression_ref simplify_names(const expression_ref& E, const vector<Module>& P)
{
  return map_symbol_names(E, get_simplified_names(P) );
}
