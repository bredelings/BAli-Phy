#ifndef LOADER_H
#define LOADER_H

#include <string>
#include <vector>
#include <set>
#include <map>
#include "computation/module.H"

class module_loader
{
  mutable std::map<std::string, expression_ref> modules;
public:
  std::vector<std::string> modules_path;
  std::vector<std::string> builtins_path;

  std::string find_module(const std::string& modid) const;
  std::string find_plugin(const std::string& plugin_name) const;
  /// Load the module file from disk.
  expression_ref read_module_from_file(const std::string&) const;
  /// Add the hard-coded elements to the module.
  Module load_module_from_file(const std::string&) const;
  Module load_module(const std::string&) const;
};

std::vector<Module> load_modules(const module_loader&, const std::vector<std::string>&);
std::vector<Module> load_modules(const module_loader&, const std::set<std::string>&);

expression_ref load_builtin(const module_loader& L, const std::string& symbol_name, const std::string& filename, int n, const std::string& fname);
expression_ref load_builtin(const std::string& symbol_name, const std::string& filename, int n, const std::string& fname);

extern const std::string plugin_extension;
#endif
