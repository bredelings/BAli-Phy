#include "computation/program.H"
#include "computation/module.H"

using std::vector;
using std::string;

bool contains_module(const vector<Module>& P, const string& module_name)
{
  for(const auto& module: P)
    if (module.module_name == module_name)
      return true;
  return false;
}
