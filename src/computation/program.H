#ifndef PROGRAM_H
#define PROGRAM_H

#include <set>
#include <vector>
#include <string>
#include <map>
#include <memory>
#include <optional>
#include "computation/expression/expression_ref.H"
#include "computation/fresh_vars.H"

class module_loader;

class Module;

class Program: public std::vector<std::shared_ptr<Module>>
{
    std::shared_ptr<module_loader> loader;

    void check_dependencies();

    void compile(int i);

    std::optional<int> find_module(const std::string& module_name) const;

    int count_module(const std::string& module_name) const;

    mutable FreshVarState fresh_var_state_;

public:

    enum class exe_type {standard, log_list, log_pair};

    exe_type type = exe_type::standard;

    std::optional<std::string> main;

    std::vector<std::shared_ptr<Module>>& modules();

    FreshVarState& fresh_var_state() const {return fresh_var_state_;}

    void add(const std::shared_ptr<Module>& M);

    void add(const std::vector<std::shared_ptr<Module>>& modules);

    void add(const std::string& name);

    void add(const std::vector<std::string>& module_names);

    std::vector<std::string> module_names() const;

    std::set<std::string> module_names_set() const;

    std::string module_names_path() const;

    bool contains_module(const std::string& module_name) const;

    std::shared_ptr<const Module> get_module(const std::string& module_name) const;

    std::shared_ptr<Module> get_module(const std::string& module_name);

    const std::shared_ptr<module_loader>& get_module_loader() const;

    Program(const std::shared_ptr<module_loader>&);
    Program(const std::shared_ptr<module_loader>&,exe_type);
};

std::map<std::string,std::string> get_simplified_names(const std::set<std::string>&);

expression_ref map_symbol_names(const expression_ref& E, const std::map<std::string, std::string>&);
#endif
