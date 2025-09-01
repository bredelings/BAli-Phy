#ifndef LOADER_H
#define LOADER_H

#include <string>
#include <vector>
#include <set>
#include <map>
#include <filesystem>
#include "computation/optimization/simplifier_options.H" // for simplifier_options
#include "computation/core/ast.H"

class Module;

class module_loader: public simplifier_options
{
    mutable std::map<std::string, std::shared_ptr<const Module>> modules;

public:
    std::vector<std::filesystem::path> plugins_path;

    std::optional<std::filesystem::path> cache_path;

    std::vector<std::string> args;

    bool recompile_all = false;

    std::set<std::string> recompile_modules;

    mutable std::map<std::tuple<std::string,std::string>, void*> cached_builtins;

    bool try_add_plugin_path(const std::filesystem::path& path);
  
    std::optional<std::filesystem::path> cache_path_for_module(const std::string& modid) const;
    std::optional<std::filesystem::path> find_cached_module(const std::string& modid) const;
    std::filesystem::path find_module(const std::string& modid) const;
    std::filesystem::path find_plugin(const std::string& plugin_name) const;

    /// Load the module file from disk.
    /// Add the hard-coded elements to the module.
    std::shared_ptr<Module> load_module_from_file(const std::filesystem::path&) const;
    std::shared_ptr<Module> load_module(const std::string&) const;

    void* load_builtin_ptr(const std::string& lib_name, const std::string& func_name) const;

    Core2::Exp<> load_builtin(const std::string& lib_name, const std::string& func_name, int n) const;

    module_loader(const std::optional<std::filesystem::path>& cp, const std::vector<std::filesystem::path>& paths);
};

extern const std::string plugin_extension;
#endif
