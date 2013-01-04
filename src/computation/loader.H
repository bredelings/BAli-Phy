#ifndef LOADER_H
#define LOADER_H

#include <string>
#include <vector>
#include <set>
#include <map>
#include "computation/module.H"
#include "computation/model_notes.H"

struct module_loader
{
  std::vector<std::string> modules_path;
  std::vector<std::string> builtins_path;
};

expression_ref read_module_from_file(const std::string& filename);
std::string find_module(const module_loader&, const std::string& modid);

Module load_module(const std::string& filename);
Module load_module(const module_loader&, const std::string&);
Module load_and_rename_module(const module_loader&, const std::string&, const std::string&);

std::vector<Module> load_modules(const module_loader&, const std::vector<std::string>&);
std::vector<Module> load_modules(const module_loader&, const std::set<std::string>&);
std::vector<Module> load_and_rename_modules(const module_loader&, const std::map<std::string,std::string>&);

expression_ref load_builtin(const module_loader& L, const std::string& filename, int n, const std::string& fname);
expression_ref load_builtin(const std::string& filename, int n, const std::string& fname);
#endif
