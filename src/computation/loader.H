#ifndef LOADER_H
#define LOADER_H

#include <string>
#include <vector>
#include <set>
#include <map>
#include <filesystem>
#include "computation/optimization/simplifier_options.H" // for simplifier_options
#include "computation/expression/expression_ref.H"

class Module;

class module_loader: public simplifier_options
{
    mutable std::map<std::string, std::shared_ptr<const Module>> modules;

public:
    std::vector<std::filesystem::path> plugins_path;

    std::optional<std::filesystem::path> cache_path;

    std::vector<std::string> args;

    bool try_add_plugin_path(const std::filesystem::path& path);
  
    std::filesystem::path find_module(const std::string& modid) const;
    std::filesystem::path find_plugin(const std::string& plugin_name) const;
    /// Load the module file from disk.
    /// Add the hard-coded elements to the module.
    std::shared_ptr<Module> load_module_from_file(const std::filesystem::path&) const;
    std::shared_ptr<Module> load_module(const std::string&) const;
    module_loader(const std::optional<std::filesystem::path>& cp, const std::vector<std::filesystem::path>& paths);
};

expression_ref load_builtin(const module_loader& L, const std::string& symbol_name, const std::string& plugin_name, int n, const std::string& fname);
expression_ref load_builtin(const std::string& symbol_name, const std::filesystem::path& filename, int n, const std::string& fname);

extern const std::string plugin_extension;
#endif
