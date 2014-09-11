#include "computation/program.H"
#include "computation/module.H"
#include "myexception.H"
#include "computation/loader.H"
#include "models/model.H"

using std::vector;
using std::set;
using std::pair;
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

void add_missing_imports(const module_loader& L, vector<Module>& P)
{
  // 1. Perform a closure over missing modules.
  std::set<string> modules_to_add;

  do
  {
    for(const string& module_name: modules_to_add)
      P.push_back( L.load_module(module_name) );

    modules_to_add = unresolved_imports(P);
  } 
  while (not modules_to_add.empty());
  
  // 2. Process new modules and 
  for(auto& module: P)
    if (not module.is_resolved())
    {
      try {
	module.load_builtins(L);
	module.resolve_symbols(P);
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

void add(const module_loader& L, vector<Module>& P, const Module& M)
{
  // Get module_names, but in a set<string>
  set<string> old_module_names = module_names_set(P);

  // 1. Check that the program doesn't already contain this module name.
  if (contains_module(P, M.name))
    throw myexception()<<"Trying to add duplicate module '"<<M.name<<"' to program "<<module_names_path(P);

  // 2. Actually add the module.
  P.push_back( M );

#ifndef NDEBUG
  // 3. Assert that every module exists only once in the list.
  for(const auto& module: P)
    assert(count_module(P, M.name) == 1);
#endif

  // 4. Import any modules that are (transitively) implied by the ones we just loaded.
  add_missing_imports(L, P);
}

void add(const module_loader& L, vector<Module>& P, const vector<Module>& modules)
{
  for(const auto& M: modules)
    add(L, P, M);
}

void add(const module_loader& L, std::vector<Module>& P, const std::string& name)
{
  if (not contains_module(P, name))
    add(L, P, L.load_module(name));
}

void add(const module_loader& L, vector<Module>& P, const vector<string>& module_names)
{
  vector<Module> modules;

  // Load the regular modules
  for(const auto& name: module_names)
    add(L, P, name);

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


expression_ref map_symbol_names(const expression_ref& E, const std::map<string,string>& simplify)
{
  if (not E.size())
  {
    if (auto V = is_a<identifier>(E))
    {
      auto loc = simplify.find(V->name);
      if (loc != simplify.end())
	return identifier(loc->second);
      else
	return E;
    }
    else
      return E;
  }

  object_ptr<expression> V = E.clone_expression();
  for(int i=0;i<E.size();i++)
    V->sub[i] = map_symbol_names(V->sub[i], simplify);
  return V;
}
