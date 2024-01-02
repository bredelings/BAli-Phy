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
#include "computation/haskell/coretype.H"
#include "computation/typecheck/kind.H"
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

class Module
{
    std::map<std::string, symbol_ptr> symbols;

    std::multimap<std::string, const_symbol_ptr> aliases;

    std::map<std::string, type_ptr> types;

    std::multimap<std::string, const_type_ptr> type_aliases;

    std::map<std::string, const_symbol_ptr> exported_symbols_;

    std::map<std::string, const_type_ptr> exported_types_;

    bool resolved = false;

    bool optimized = false;

public:

    std::vector<Message> messages;

    LanguageExtensions language_extensions;

    bool do_optimize = true;
    
    Haskell::Module module;

    std::map<std::string, std::shared_ptr<const Module>> transitively_imported_modules;

    InstanceEnv local_instances;
    EqInstanceEnv local_eq_instances;

    CDecls value_decls;

    std::vector<std::vector<expression_ref>> class_and_type_decls;

    std::string name;

    std::shared_ptr<std::string> filename;

    FileContents file;

    bool is_resolved() const {return resolved;}

    bool is_optimized() const {return optimized;}

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

    void import_symbol(const const_symbol_ptr&, const std::string&, bool qualified);

    void import_type(const const_type_ptr&, const std::string&, bool qualified);

    void import_module(const Program& P, const Hs::LImpDecl& I);

    void compile(const Program&);

    void declare_fixities_(const Hs::FixityDecl&);

    void declare_fixities_(const Hs::Decls&);

    void declare_fixities(const Hs::ModuleDecls&);

    void add_local_symbols(const Hs::Decls&);

    void perform_imports(const Program&);

    typechecker_result typecheck(FreshVarState&, Hs::ModuleDecls);

    void perform_exports();

    Hs::ModuleDecls rename(const simplifier_options&, Hs::ModuleDecls);

    CDecls desugar(const simplifier_options&, FreshVarState&, const Hs::Binds&);

    void export_small_decls(const CDecls&);

    std::map<var,expression_ref> code_defs() const;

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

    Hs::Var get_external_var(const std::string& s) const;

    OpInfo get_operator(const std::string& name) const;

    explicit Module(const std::string&);

    explicit Module(const char*);

    explicit Module(const Haskell::Module&, const LanguageExtensions& lo, const FileContents& f);
};

std::ostream& operator<<(std::ostream&, const Module&);

expression_ref resolve_refs(const Program&, const expression_ref&);

bool special_prelude_symbol(const std::string& name);

extern std::set<std::string> special_prelude_symbols;
#endif
