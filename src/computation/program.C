#include "computation/program.H"
#include "computation/module.H"
#include "myexception.H"
#include "computation/prelude.H"

using std::vector;
using std::string;

bool contains_module(const vector<Module>& P, const string& module_name)
{
  return find_module(P,module_name) != -1;
}

int find_module(const vector<Module>& P, const string& module_name)
{
  for(int i=0;i<P.size();i++)
    if (P[i].module_name == module_name)
      return i;
  return -1;
}

const Module& get_module(const std::vector<Module>& P, const std::string& module_name)
{
  int index = find_module(P,module_name);
  if (index == -1)
    throw myexception()<<"Progam does not contain module '"<<module_name<<"'";
  return P[index];
}

void add(const std::vector<std::string>& modules_path, std::vector<Module>& P, const std::vector<Module>& modules)
{
  P.insert(P.end(), modules.begin(), modules.end());

  // 1. Add any additional modules needed to complete the program.
  std::set<string> modules_to_add;
  do
  {
    for(const string& module_name: modules_to_add)
      P.push_back(load_module(modules_path, module_name));

    modules_to_add.clear();
      
    for(const auto& module: P)
    {
      for(const string& module_name: module.dependencies)
	if (not contains_module(P, module_name))
	  modules_to_add.insert(module_name);
    }
      
  } while (not modules_to_add.empty());

  // 2a. Perform any needed imports.
  // 2b. Desugar the module here.
  for(auto& module: P)
    if (contains_module(modules, module.module_name))
      module.resolve_symbols(P);
}
