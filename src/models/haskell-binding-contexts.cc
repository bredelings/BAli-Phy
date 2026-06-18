#include "models/haskell-binding-contexts.H"

#include "computation/haskell/haskell.H"
#include "computation/loader.H"
#include "computation/module.H"
#include "computation/program.H"
#include "util/myexception.H"
#include "util/string/join.H"

#include <sstream>

namespace
{

// Builds a stable synthetic module name for one normalized import-set index.
std::string context_module_name(std::size_t index)
{
    return "BindingInfer.Context.C" + std::to_string(index);
}

// Converts a normalized import set to the Haskell import declarations used by
// the synthetic context module.
std::vector<Haskell::LImpDecl> import_decls_for(const BindingImportSet& imports)
{
    std::vector<Haskell::LImpDecl> declarations;
    for(const auto& module_name: imports.modules)
        declarations.emplace_back(noloc, Haskell::ImpDecl{false, {noloc, module_name}, {}, {}});
    return declarations;
}

// Creates display-only source text so synthetic modules have deterministic
// hash inputs and useful names in diagnostics.
std::string synthetic_module_contents(const std::string& module_name, const BindingImportSet& imports)
{
    std::ostringstream out;
    out<<"module "<<module_name<<" where\n";
    for(const auto& import_name: imports.modules)
        out<<"import "<<import_name<<"\n";
    return out.str();
}

// Creates one import-only Haskell module to provide a compiled scope for
// binding signature lookup and later call inference.
std::shared_ptr<Module> make_context_module(const std::string& module_name, const BindingImportSet& imports)
{
    auto contents = synthetic_module_contents(module_name, imports);
    FileContents file{module_name, contents};
    Haskell::Module module{{noloc, module_name}, {}, import_decls_for(imports), {}};
    return std::make_shared<Module>(module, LanguageExtensions(), file);
}

}

// Normalizes JSON binding imports to the compiled context import convention:
// an empty binding import list means Prelude.
BindingImportSet normalize_binding_imports(const BindingImportSet& imports)
{
    if (imports.modules.empty())
        return {{"Prelude"}};
    return imports;
}

// Stores the compiled context program and the normalized import-set to module
// name map used by context_for().
HaskellBindingContexts::HaskellBindingContexts(std::shared_ptr<Program> p, std::map<std::set<std::string>, std::string> names)
    :program_(std::move(p)),
     context_module_names_(std::move(names))
{
}

// Compiles one synthetic module per normalized import set into a single Program
// so later lookup/inference can reuse compiled Haskell scopes.
HaskellBindingContexts HaskellBindingContexts::build(const std::shared_ptr<module_loader>& loader, const std::vector<BindingImportSet>& import_sets)
{
    std::set<std::set<std::string>> normalized_sets;
    if (import_sets.empty())
        normalized_sets.insert(normalize_binding_imports({}).modules);
    else
        for(const auto& imports: import_sets)
            normalized_sets.insert(normalize_binding_imports(imports).modules);

    std::vector<std::shared_ptr<Module>> modules;
    std::map<std::set<std::string>, std::string> names;
    std::size_t index = 0;
    for(const auto& modules_set: normalized_sets)
    {
        BindingImportSet imports{modules_set};
        auto module_name = context_module_name(index++);
        modules.push_back(make_context_module(module_name, imports));
        names.insert({modules_set, module_name});
    }

    return HaskellBindingContexts(std::make_shared<Program>(loader, modules), std::move(names));
}

// Returns the compiled Program backing every synthetic context module.
const Program& HaskellBindingContexts::program() const
{
    if (not program_)
        throw myexception()<<"Haskell binding contexts have not been built";
    return *program_;
}

// Returns the compiled synthetic context module for one binding import set,
// applying the same default-Prelude normalization used during construction.
std::shared_ptr<const CompiledModule> HaskellBindingContexts::context_for(const BindingImportSet& imports) const
{
    auto normalized = normalize_binding_imports(imports);
    auto iter = context_module_names_.find(normalized.modules);
    if (iter == context_module_names_.end())
        throw myexception()<<"No Haskell binding context for imports {"<<join(std::vector<std::string>(normalized.modules.begin(), normalized.modules.end()), ", ")<<"}";
    return program().get_module(iter->second);
}

std::size_t HaskellBindingContexts::context_count() const
{
    return context_module_names_.size();
}
