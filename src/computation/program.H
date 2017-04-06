#ifndef PROGRAM_H
#define PROGRAM_H

#include <vector>
#include <string>
#include <map>
#include <memory>
#include "computation/expression/expression_ref.H"

class module_loader;

class Module;

class Program: public std::vector<Module>
{
    std::shared_ptr<module_loader> loader;

    void check_dependencies();

    void desugar_and_optimize();

    int find_module(const std::string& module_name) const;

public:
          std::vector<Module>& modules();

    const std::vector<Module>& modules() const;

    void add(const Module& M);

    void add(const std::vector<Module>& modules);

    void add(const std::string& name);

    void add(const std::vector<std::string>& module_names);

    bool contains_module(const std::string& module_name) const;

    const Module& get_module(const std::string& module_name) const;

    const std::shared_ptr<module_loader>& get_module_loader() const;

    Program(const std::shared_ptr<module_loader>&);
};

std::vector<std::string> module_names(const std::vector<Module>& P);
std::string module_names_path(const std::vector<Module>& P);
std::set<std::string> module_names_set(const std::vector<Module>& P);

bool is_declared(const std::vector<Module>& modules, const std::string& qvar);

std::map<std::string,std::string> get_simplified_names(const std::set<std::string>&);

expression_ref map_symbol_names(const expression_ref& E, const std::map<std::string, std::string>&);
#endif
