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
#include <filesystem>

class module_loader;

class Module;

class CompiledModule;

class Program: public std::vector<std::shared_ptr<CompiledModule>>
{
    std::shared_ptr<module_loader> loader;

    void check_dependencies();

    std::optional<int> find_module(const std::string& module_name) const;

    int count_module(const std::string& module_name) const;

    std::optional<std::string> main;

    void add(const std::shared_ptr<Module>& M);

    void add(const std::vector<std::shared_ptr<Module>>& modules);

    void add(const std::string& name);

    void add(const std::vector<std::string>& module_names);

public:

    std::shared_ptr<FreshVarState> fresh_var_state() const;

    std::optional<std::string> get_main_name() const;

    std::vector<std::shared_ptr<CompiledModule>>& modules();

    std::vector<std::string> module_names() const;

    std::set<std::string> module_names_set() const;

    std::string module_names_path() const;

    bool contains_module(const std::string& module_name) const;

    std::shared_ptr<const CompiledModule> get_module(const std::string& module_name) const;

    const std::shared_ptr<module_loader>& get_module_loader() const;

    Program(const std::shared_ptr<module_loader>&, const std::vector<std::shared_ptr<Module>>&);
    Program(const std::shared_ptr<module_loader>&, const std::vector<std::shared_ptr<Module>>&, const std::string& m);
};

std::map<std::string,std::string> get_simplified_names(const std::set<std::string>&);

expression_ref map_symbol_names(const expression_ref& E, const std::map<std::string, std::string>&);

void execute_program(std::unique_ptr<Program> P);

std::unique_ptr<Program> load_program_from_file(const std::shared_ptr<module_loader>& L, const std::filesystem::path& filename);

void execute_file(const std::shared_ptr<module_loader>& L, const std::filesystem::path& filename);

#endif
