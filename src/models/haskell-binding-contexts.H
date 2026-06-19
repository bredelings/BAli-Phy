#ifndef HASKELL_BINDING_CONTEXTS_H
#define HASKELL_BINDING_CONTEXTS_H

#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

class CompiledModule;
class Module;
class Program;
class module_loader;

struct BindingImportSet
{
    std::set<std::string> modules;
};

class HaskellBindingContexts
{
    std::shared_ptr<Program> program_;
    std::map<std::set<std::string>, std::string> context_module_names_;

public:
    HaskellBindingContexts() = default;

    HaskellBindingContexts(std::shared_ptr<Program>, std::map<std::set<std::string>, std::string>);

    static HaskellBindingContexts build(const std::shared_ptr<module_loader>&, const std::vector<BindingImportSet>&);

    const Program& program() const;

    std::shared_ptr<const CompiledModule> context_for(const BindingImportSet&) const;

    std::shared_ptr<Module> make_imported_module(const BindingImportSet&, const std::string&, const std::string&) const;

    std::size_t context_count() const;
};

BindingImportSet normalize_binding_imports(const BindingImportSet&);

#endif
