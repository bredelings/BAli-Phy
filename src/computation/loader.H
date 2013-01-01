#ifndef LOADER_H
#define LOADER_H

#include <string>
#include <vector>
#include "computation/module.H"

Module load_module(const std::vector<std::string>&, const std::string&);
std::vector<Module> load_modules(const std::vector<std::string>&, const std::vector<std::string>&);

expression_ref load_builtin(const std::vector<std::string>& builtins_path, const std::string& filename, int n, const std::string& fname);
expression_ref load_builtin(const std::string& filename, int n, const std::string& fname);
#endif
