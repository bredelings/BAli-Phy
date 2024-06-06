#ifndef MODULE_H
#define MODULE_H

#include <set>
#include <map>
#include <string>
#include <vector>
#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"
#include "computation/expression/var.H"
#include <utility>                                  // for pair
#include <iostream>
#include "computation/haskell/haskell.H"
#include "computation/haskell/extensions.H"
#include "computation/optimization/simplifier_options.H"
#include "computation/fresh_vars.H"
#include "computation/instance_info.H"
#include "computation/data_con_info.H"
#include "message.H"
#include "symbols.H"

class module_loader;

class Program;

struct typechecker_result
{
    Hs::Binds class_binds;

    Hs::Binds value_decls;

    Hs::Binds default_method_decls;

    Hs::Binds instance_method_decls;

    Core::Decls dfun_decls;

    Core::Decls top_simplify_decls;

    std::pair<Hs::Binds, Core::Decls> all_binds() const;
};

typedef std::shared_ptr<symbol_info> symbol_ptr;
typedef std::shared_ptr<const symbol_info> const_symbol_ptr;

typedef std::shared_ptr<type_info> type_ptr;
typedef std::shared_ptr<const type_info> const_type_ptr;

class CompiledModule;
class Module;

typechecker_result typecheck(FreshVarState&, Hs::ModuleDecls, Module&);

std::shared_ptr<CompiledModule> compile(const Program&, std::shared_ptr<Module>);

class Module
{
    // only to look at the symbols variable!
    friend std::shared_ptr<CompiledModule> compile(const Program&, std::shared_ptr<Module>);

    std::map<std::string, symbol_ptr> symbols;

    std::multimap<std::string, const_symbol_ptr> aliases;

    std::map<std::string, type_ptr> types;

    std::multimap<std::string, const_type_ptr> type_aliases;

    std::map<std::string, const_symbol_ptr> exported_symbols_;

    std::map<std::string, const_type_ptr> exported_types_;

public:

    std::vector<Message> messages;

    LanguageExtensions language_extensions;

    bool do_optimize = true;
    
    Haskell::Module module_AST;

    std::map<std::string, std::shared_ptr<const CompiledModule>> transitively_imported_modules;

    InstanceEnv local_instances;
    EqInstanceEnv local_eq_instances;

    std::vector<std::vector<expression_ref>> class_and_type_decls;

    std::string name;

    std::shared_ptr<std::string> filename;

    // For error messages, and also computing the source file SHA.
    FileContents file;

    std::string all_inputs_sha(const Program& P) const;

    mutable std::optional<std::string> _cached_sha;

    std::set<std::string> dependencies() const;

    std::vector<Hs::LImpDecl> imports() const;

    const std::map<std::string, const_symbol_ptr>& exported_symbols() const {return exported_symbols_;}

    const std::map<std::string, const_type_ptr>& exported_types() const {return exported_types_;}

    std::string qualify_local_name(const std::string&) const;
    bool is_local_qualified_name(const std::string& n) const;

    std::map<std::string, const_type_ptr> required_types() const;

    void export_symbol(const const_symbol_ptr& S);

    void export_type(const const_type_ptr& S);

    void export_module(const std::string& S);

    void clear_symbol_table();

    std::optional<DataConInfo> constructor_info(const std::string& n) const;

    /// Add a function
    void def_function(const std::string& name);
    /// Add a function
    void maybe_def_function(const std::string& name);

    /// Add a constructor
    void def_constructor(const std::string& name, int arity, const std::string& type_name);

    /// Add an ADT
    void def_ADT(const std::string& name, const type_info::data_info& info);
    void def_ADT(const std::string& name, const fixity_info&, const type_info::data_info& info);

    void def_type_class(const std::string& class_name, const type_info::class_info& info);

    void def_type_class_method(const std::string& method_name, const std::string& class_name);

    void def_type_synonym(const std::string& syn_name, int arity);

    void def_type_family(const std::string& family_name, int arity);

    void def_data_family(const std::string& family_name, int arity);

    symbol_ptr add_symbol(const symbol_info&);

    type_ptr add_type(const type_info&);

    void add_alias(const std::string&, const const_symbol_ptr&);

    void add_type_alias(const std::string&, const const_type_ptr&);

    void declare_fixity(const std::string&, int precedence, fixity_t f);

    void declare_symbol(const symbol_info&);

    void declare_type(const type_info&);

    void write_compile_artifact(const Program& P);

    void import_symbol(const const_symbol_ptr&, const std::string&, bool qualified);

    void import_type(const const_type_ptr&, const std::string&, bool qualified);

    void import_module(const Program& P, const Hs::LImpDecl& I);

    void declare_fixities_(const Hs::FixityDecl&);

    void declare_fixities_(const Hs::Decls&);

    void declare_fixities(const Hs::ModuleDecls&);

    void add_local_symbols(const Hs::Decls&);

    void perform_imports(const Program&);

    void perform_exports();

    Hs::ModuleDecls rename(const simplifier_options&, Hs::ModuleDecls);

    CDecls desugar(const simplifier_options&, FreshVarState&, const Hs::Binds&);

    void export_small_decls(const CDecls&);

    CDecls optimize(const simplifier_options&, FreshVarState&, CDecls);

    CDecls load_builtins(const module_loader&, const std::vector<Hs::ForeignDecl>&, CDecls);

    CDecls load_constructors(const Hs::Decls&, CDecls);

    bool is_declared(const std::string&) const;

    bool type_is_declared(const std::string&) const;

    bool symbol_in_scope_with_name(const std::string&, const std::string&) const;

    bool type_in_scope_with_name(const std::string&, const std::string&) const;

    static const_symbol_ptr lookup_builtin_symbol(const std::string&);
    const_symbol_ptr lookup_symbol(const std::string&) const;
    const_symbol_ptr lookup_resolved_symbol(const std::string&) const;
    const_symbol_ptr lookup_local_symbol(const std::string&) const;
          symbol_ptr lookup_local_symbol(const std::string&);
          symbol_ptr lookup_make_local_symbol(const std::string&);
    const_symbol_ptr lookup_external_symbol(const std::string&) const;

    static const_type_ptr lookup_builtin_type(const std::string&);
    const_type_ptr lookup_type(const std::string&) const;
    const_type_ptr lookup_resolved_type(const std::string&) const;
    const_type_ptr lookup_local_type(const std::string&) const;
    type_ptr lookup_local_type(const std::string&);
    const_type_ptr lookup_external_type(const std::string&) const;

    OpInfo get_operator(const std::string& name) const;

    explicit Module(const std::string&);

    explicit Module(const char*);

    explicit Module(const Haskell::Module&, const LanguageExtensions& lo, const FileContents& f);
};


// Eliminate this_compiled_mod?

// If Module builds some things, have CompiledModule steal them at the end?
class CompiledModule
{
    std::shared_ptr<Module> mod;

    std::set<std::string> dependencies_;

    std::string modid;

    CDecls value_decls;

public:

    // Discard everything-except-code since all compilation is done.
    // This "everything-except-code" should probably be a different object.
    void clear_symbol_table() {mod->clear_symbol_table();}

    // Now discard the code -- what is left?
    void clear_code() { value_decls.clear(); }

    // Where is this used?
    const std::map<std::string, std::shared_ptr<const CompiledModule>>& transitively_imported_modules() const
    {
	return mod->transitively_imported_modules;
    }

    std::string all_inputs_sha(const Program& P) const {return mod->all_inputs_sha(P);}

    const std::set<std::string>& dependencies() const {return dependencies_;}

    const LanguageExtensions& language_extensions() const {return mod->language_extensions;}

    const std::string& name() const {return modid;}

    const InstanceEnv& local_instances() const {return mod->local_instances;}
    const EqInstanceEnv& local_eq_instances() const {return mod->local_eq_instances;}

    const std::map<std::string, const_symbol_ptr>& exported_symbols() const {return mod->exported_symbols();}

    const std::map<std::string, const_type_ptr>& exported_types() const {return mod->exported_types();}

    // How does this relate to the exported symbols?
    const_symbol_ptr lookup_local_symbol(const std::string& s) const {return mod->lookup_local_symbol(s);}

    const_type_ptr lookup_local_type(const std::string& t) const {return mod->lookup_local_type(t);}

    void finish_value_decls( CDecls& cdecls) {std::swap(value_decls, cdecls);}

    // Why do we have code that isn't for this module?
    std::map<var,expression_ref> code_defs() const;

    CompiledModule(const std::shared_ptr<Module>& m);
};

std::ostream& operator<<(std::ostream&, const Module&);

expression_ref resolve_refs(const Program&, const expression_ref&);

bool special_prelude_symbol(const std::string& name);

extern std::set<std::string> special_prelude_symbols;
#endif
