#ifndef PROGRAM_H
#define PROGRAM_H

#include <vector>
#include <string>
#include <map>
#include "computation/expression.H"
#include "computation/loader.H"

class Module;

typedef std::vector<Module> Program;

bool contains_module(const std::vector<Module>& P, const std::string& module_name);

int find_module(const std::vector<Module>& P, const std::string& module_name);

std::vector<std::string> module_names(const std::vector<Module>& P);
std::string module_names_path(const std::vector<Module>& P);
std::set<std::string> module_names_set(const std::vector<Module>& P);

const Module& get_module(const std::vector<Module>& P, const std::string& module_name);

void add(const module_loader& L, std::vector<Module>& P, Model_Notes& N, const std::vector<Module>& modules);

bool is_declared(const std::vector<Module>& modules, const std::string& qvar);

std::map<std::string,std::string> get_simplified_names(const std::vector<Module>& P);

std::map<std::string,std::string> get_simplified_names(const std::set<std::string>&);

expression_ref map_symbol_names(const expression_ref& E, const std::map<std::string, std::string>&);

expression_ref simplify_names(const expression_ref& E, const std::vector<Module>& P);
#endif
