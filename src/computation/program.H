#ifndef PROGRAM_H
#define PROGRAM_H

#include <vector>
#include <string>
#include <map>

class Module;

typedef std::vector<Module> Program;

bool contains_module(const std::vector<Module>& P, const std::string& module_name);

int find_module(const std::vector<Module>& P, const std::string& module_name);

const Module& get_module(const std::vector<Module>& P, const std::string& module_name);

void add(const std::vector<std::string>& modules_path, const std::vector<std::string>& builtins_path, std::vector<Module>& P, const std::vector<Module>& modules);

bool is_declared(const std::vector<Module>& modules, const std::string& qvar);

std::map<std::string,std::string> get_simplified_names(const std::vector<Module>& P);
#endif
