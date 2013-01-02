#include "computation/program.H"
#include "computation/module.H"
#include "myexception.H"
#include "computation/loader.H"
#include "models/model.H"

using std::vector;
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

static int count_module(const vector<Module>& P,const string& module_name)
{
  int count = 0;
  for(const auto& module: P)
    if (module.name == module_name)
      count++;
  return count;
}

void add(const module_loader& L, vector<Module>& P, const vector<Module>& modules)
{
  // 1. Check that the program doesn't already contain these module names.
  for(const auto& module: modules)
    if (contains_module(P,module.name))
      throw myexception()<<"Trying to add duplicate module '"<<module.name<<"' to program ["<<join(module_names(P),",")<<"]";

  // 2. Check that we aren't adding any module twice.
  for(const auto& module: modules) 
  {
    int count = count_module(modules, module.name);
    if (count > 1)
      throw myexception()<<"Trying to add module '"<<module.name<<"' to program ["<<join(module_names(P),",")<<"] "<<count<<" times.";
  }

  // 3. Actually add the modules.
  P.insert(P.end(), modules.begin(), modules.end());

#ifndef NDEBUG
  // 4. Assert that every module exists only once in the list.
  for(const auto& module: P)
    assert(count_module(P, module.name) == 1);
#endif

  // 5. Add any additional modules needed to complete the program.
  std::set<string> modules_to_add;
  do
  {
    for(const string& module_name: modules_to_add)
      P.push_back(load_module(L.modules_path, module_name));

    modules_to_add.clear();
      
    for(const auto& module: P)
    {
      for(const string& module_name: module.dependencies)
	if (not contains_module(P, module_name))
	  modules_to_add.insert(module_name);
    }
      
  } while (not modules_to_add.empty());

  // 6a. Perform any needed imports.
  // 6b. Desugar the module here.
  for(auto& module: P)
    if (contains_module(modules, module.name))
    {
      try {
	module.load_builtins(L.builtins_path);
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

std::map<string,string> get_simplified_names(const vector<Module>& P)
{
  std::map<string,string> simplified;

  vector<string> parameter_names;

  // 1. Find all parameters and variables
  std::multimap<string,string> aliases;
  for(const auto& module: P)
    for(const auto& S: module.get_symbols())
      if (S.second.scope == local_scope)
      {
	string qname = S.first;
	if (S.second.symbol_type == variable_symbol)
	{
	  string name = get_unqualified_name(qname);
	  aliases.insert({name,qname});
	}
	else if (S.second.symbol_type == parameter_symbol)
	  parameter_names.push_back(qname);
      }

  // 2. Make long names to their shortened equivalent
  vector<string> short_names = short_parameter_names(parameter_names);
  for(int i=0;i<parameter_names.size();i++)
    simplified[parameter_names[i]] = short_names[i];

  // 3. Map qualified names to their unqualified versions IF there is only one occurrence of the unqualified version.
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
  if (not E->size())
  {
    if (auto V = is_a<var>(E))
    {
      auto loc = simplify.find(V->name);
      if (loc != simplify.end())
	return var(loc->second);
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

