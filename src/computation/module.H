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
#include "computation/optimization/simplifier.H"       // for simplifier options
#include "computation/fresh_vars.H"
#include "message.H"
#include "symbols.H"

class module_loader;

class Program;

class TypeChecker;

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

class Module
{
    std::map<std::string, symbol_ptr> symbols;

    std::multimap<std::string, const_symbol_ptr> aliases;

    std::map<std::string, type_info> types;

    std::multimap<std::string, std::string> type_aliases;

    std::map<std::string, const_symbol_ptr> exported_symbols_;

    std::map<std::string, type_info> exported_types_;

    bool resolved = false;

    bool optimized = false;

public:

    std::vector<Message> messages;

    LanguageExtensions language_extensions;

    bool do_optimize = true;
    
    Haskell::Module module;

    std::shared_ptr<TypeChecker> tc_state;

    CDecls value_decls;

    std::vector<std::vector<expression_ref>> class_and_type_decls;

    std::map<var,expression_ref> small_decls_in;

    // When the simplifier becomes type-aware, then change to global_value_env
    std::set<var> small_decls_in_free_vars;

    std::map<var,expression_ref> small_decls_out;

    // When the simplifier becomes type-aware, then change to global_value_env
    std::set<var> small_decls_out_free_vars;

    std::string name;

    std::shared_ptr<std::string> filename;

    FileContents file;

    bool is_resolved() const {return resolved;}

    bool is_optimized() const {return optimized;}

    std::set<std::string> dependencies() const;

    std::vector<Hs::LImpDecl> imports() const;

    const std::map<std::string, const_symbol_ptr>& exported_symbols() const {return exported_symbols_;}

    const std::map<std::string, type_info>& exported_types() const {return exported_types_;}

    std::map<std::string, type_info> required_types() const;

    void export_symbol(const const_symbol_ptr& S);

    void export_type(const type_info& S);

    void export_module(const std::string& S);

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

    symbol_ptr add_symbol(const symbol_info&);

    void add_type(const type_info&);

    void add_alias(const std::string&, const const_symbol_ptr&);

    void add_type_alias(const std::string&, const std::string&);

    void declare_fixity(const std::string&, int precedence, fixity_t f);

    void declare_symbol(const symbol_info&);

    void declare_type(const type_info&);

    void import_symbol(const const_symbol_ptr&, const std::string&, bool qualified);

    void import_type(const type_info&, const std::string&, bool qualified, const std::map<std::string, const_symbol_ptr>&);

    void import_module(const Program& P, const Hs::LImpDecl& I);

    void compile(const Program&);

    void declare_fixities_(const Hs::FixityDecl&);

    void declare_fixities_(const Hs::Decls&);

    void declare_fixities(const Hs::ModuleDecls&);

    void add_local_symbols(const Hs::Decls&);

    void perform_imports(const Program&);

    typechecker_result typecheck(Hs::ModuleDecls);

    void perform_exports();

    Hs::ModuleDecls rename(const simplifier_options&, Hs::ModuleDecls);

    CDecls desugar(const simplifier_options&, FreshVarState&, const Hs::Binds&);

    std::pair< std::map<var,expression_ref>, std::set<var> > import_small_decls(const Program&);

    std::pair< std::map<var,expression_ref>, std::set<var> > export_small_decls(const CDecls&, const std::map< var, expression_ref>& );

    std::map<var,expression_ref> code_defs() const;

    CDecls optimize(const simplifier_options&, FreshVarState&, CDecls, const std::map<var, expression_ref>&, const std::set<var>&);

    CDecls load_builtins(const module_loader&, const std::vector<Hs::ForeignDecl>&, CDecls);

    CDecls load_constructors(const Hs::Decls&, CDecls);

    bool is_declared(const std::string&) const;

    bool type_is_declared(const std::string&) const;

    bool symbol_in_scope_with_name(const std::string&, const std::string&) const;

    bool type_in_scope_with_name(const std::string&, const std::string&) const;

    static std::pair<const_symbol_ptr,expression_ref> lookup_builtin_symbol(const std::string&);

    static type_info lookup_builtin_type(const std::string&);

    const_symbol_ptr lookup_symbol(const std::string&) const;

    const_symbol_ptr lookup_resolved_symbol(const std::string&) const;
    const symbol_info* lookup_resolved_symbol_(const std::string&) const;
    // This should just be for local symbols
          symbol_info* lookup_resolved_symbol_(const std::string&);

    OpInfo get_operator(const std::string& name) const;

    type_info lookup_type(const std::string&) const;

    std::optional<type_info> lookup_resolved_type(const std::string&) const;
    const type_info* lookup_resolved_type_(const std::string&) const;
          type_info* lookup_resolved_type_(const std::string&);

    explicit Module(const std::string&);

    explicit Module(const char*);

    explicit Module(const Haskell::Module&, const LanguageExtensions& lo, const FileContents& f);
};

std::ostream& operator<<(std::ostream&, const Module&);

expression_ref resolve_refs(const Program&, const expression_ref&);
#endif
