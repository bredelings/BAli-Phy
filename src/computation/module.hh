#ifndef MODULE_H
#define MODULE_H

#include <set>
#include <map>
#include <string>
#include <vector>
#include <utility>                                  // for pair
#include <iostream>
#include "computation/haskell/haskell.hh"
#include "computation/haskell/extensions.hh"
#include "computation/core/ast.hh"
#include "computation/runtime/ast.hh"
#include "computation/optimization/simplifier_options.hh"
#include "computation/fresh_vars.hh"
#include "computation/instance_info.hh"
#include "computation/data_con_info.hh"
#include "message.hh"
#include "symbols.hh"
#include "typecheck/types.hh"

class module_loader;

class Program;

struct typechecker_result
{
    Core::Decls<> class_decls;

    Hs::Binds value_decls;

    Hs::Binds default_method_decls;

    Hs::Binds instance_method_decls;

    Core::Decls<> dfun_decls;

    Core::Decls<> top_simplify_decls;

    std::vector<Hs::ForeignDecl> foreign_decls;

    std::pair<Hs::Binds, Core::Decls<>> all_binds() const;
};

typedef std::shared_ptr<symbol_info> symbol_ptr;
typedef std::shared_ptr<const symbol_info> const_symbol_ptr;

typedef std::shared_ptr<type_info> type_ptr;
typedef std::shared_ptr<const type_info> const_type_ptr;

class CompiledModule;
class Module;

typechecker_result typecheck(FreshVarState&, Hs::ModuleDecls, Module&);

std::shared_ptr<CompiledModule> compile(const Program&, std::shared_ptr<Module>);
std::shared_ptr<CompiledModule> compile_parsed_module(const Program&, std::shared_ptr<Module>);
std::string module_inputs_hash(const Program&, const std::string&, const std::set<std::string>&);

class Module
{
    friend class CompiledModule;

    // so that it can set _cached_sha
    friend std::shared_ptr<CompiledModule> compiler_prim_module();

    // only to look at the symbols variable!
    friend std::shared_ptr<CompiledModule> compile(const Program&, std::shared_ptr<Module>);
    friend std::shared_ptr<CompiledModule> compile_parsed_module(const Program&, std::shared_ptr<Module>);

    std::map<std::string, symbol_ptr> symbols;

    std::multimap<std::string, const_symbol_ptr> aliases;
    std::multimap<std::string, FieldInfo> field_aliases;

    std::map<std::string, type_ptr> types;

    std::multimap<std::string, const_type_ptr> type_aliases;

    std::map<std::string, const_symbol_ptr> exported_symbols_;
    std::multimap<std::string, FieldInfo> exported_fields_;

    std::map<std::string, const_type_ptr> exported_types_;
    DefaultEnv exported_defaults_;

    std::shared_ptr<FreshVarState> fresh_var_state_;

public:

    // Local and imported declarations, excluding automatic built-in fallbacks.
    DefaultEnv effective_defaults;

    std::vector<Message> messages;
    std::vector<Message> parser_warnings;

    LanguageExtensions language_extensions;

    bool do_optimize = true;
    
    Haskell::Module module_AST;

    std::map<std::string, std::shared_ptr<const CompiledModule>> transitively_imported_modules;

    InstanceEnv local_instances;
    EqInstanceEnv local_eq_instances;

    std::string name;

    std::shared_ptr<std::string> filename;

    // For error messages, and also computing the source file SHA.
    FileContents file;

    const FreshVarState& fresh_var_state() const {return *fresh_var_state_;}

          FreshVarState& fresh_var_state() {return *fresh_var_state_;}

    std::shared_ptr<FreshVarState> fresh_var_state_ptr() {return fresh_var_state_;}

    std::string all_inputs_hash(const Program& P) const;

    mutable std::optional<std::string> _cached_hash;

    std::set<std::string> dependencies() const;

    std::set<std::string> hidden_dependencies() const;

    std::vector<Hs::LImpDecl> imports() const;

    const std::map<std::string, const_symbol_ptr>& exported_symbols() const {return exported_symbols_;}
    const std::multimap<std::string, FieldInfo>& exported_fields() const {return exported_fields_;}

    const std::map<std::string, const_type_ptr>& exported_types() const {return exported_types_;}
    const DefaultEnv& exported_defaults() const {return exported_defaults_;}

    std::string qualify_local_name(const std::string&) const;
    bool is_local_qualified_name(const std::string& n) const;

    std::map<std::string, const_type_ptr> required_types() const;

    void export_symbol(const const_symbol_ptr& S);
    void export_field(const FieldInfo& field);

    void export_type(const const_type_ptr& S);

    void export_module(const std::string& S);

    void clear_symbol_table();

    std::optional<DataConInfo> constructor_info(const std::string& n) const;

    std::vector<FieldInfo> record_fields_for_type(const type_info& type) const;
    bool type_has_record_field(const type_info& type, const std::string& field_name) const;
    std::map<std::string, FieldInfo> local_synthesizable_record_fields() const;
    std::vector<symbol_ptr> local_record_selectors();

    std::vector<FieldInfo> lookup_record_field_candidates(const std::string& field_name) const;
    std::optional<std::vector<std::string>> record_field_names_for_constructor(const std::string& constructor_name) const;

    /// Add a function
    void def_function(const std::string& name);
    /// Add a function
    void maybe_def_function(const std::string& name);

    /// Mark an already declared generated function as a record selector.
    void mark_record_selector(const FieldInfo& field);

    /// Declare a generated record selector function and attach its field identity.
    void def_record_selector(const FieldInfo& field);

    /// Add a constructor
    void def_constructor(const std::string& name, int arity, const std::string& type_name);

    /// Add an ADT
    void def_ADT(const std::string& name, int arity, const type_info::data_info& info);
    void def_ADT(const std::string& name, int arity, const Infix::Fixity&, const type_info::data_info& info);

    void def_type_class(const std::string& class_name, int arity, const type_info::class_info& info);

    void def_type_synonym(const std::string& syn_name, int arity);

    void def_type_family(const std::string& family_name, int arity);

    void def_data_family(const std::string& family_name, int arity);

    void def_type_class_method(const std::string& method_name, const std::string& class_name);

    symbol_ptr add_symbol(const symbol_info&);

    type_ptr add_type(const type_info&);

    void add_alias(const std::string&, const const_symbol_ptr&);

    void add_type_alias(const std::string&, const const_type_ptr&);

    void declare_fixity(const std::string&, int precedence, Infix::Associativity);

    void declare_symbol(const symbol_info&);

    void declare_type(const type_info&);

    void import_symbol(const const_symbol_ptr&, const std::string&, bool qualified);
    void import_field(const FieldInfo&, const std::string&, bool qualified);

    void import_type(const const_type_ptr&, const std::string&, bool qualified);

    void import_module(const Program& P, const Hs::LImpDecl& I);

    void declare_fixities_(const Hs::FixityDecl&);

    void declare_fixities_(const Hs::Decls&);

    void declare_fixities(const Hs::ModuleDecls&);

    void add_local_symbols(const Hs::Decls&);

    void perform_imports(const Program&);

    void perform_exports();

    Hs::ModuleDecls rename(const simplifier_options&, Hs::ModuleDecls);

    Core::Decls<> desugar(const simplifier_options&, FreshVarState&, const Hs::Binds&);

    void export_small_decls(const inliner_options& opts, const Core::Decls<>&);

    Core::Decls<> optimize(const simplifier_options&, FreshVarState&, Core::Decls<>);

    Core::Decls<> load_builtins(const module_loader&, const std::vector<Hs::ForeignDecl>&);

    Core::Decls<> load_constructors(const Hs::Decls&);

    bool is_refutable_pattern(const Hs::LPat& pattern) const;

    bool is_declared(const std::string&) const;

    bool type_is_declared(const std::string&) const;

    bool symbol_in_scope_with_name(const std::string&, const std::string&) const;

    bool type_in_scope_with_name(const std::string&, const std::string&) const;

    const_symbol_ptr lookup_symbol(const std::string&) const;
    const_symbol_ptr lookup_resolved_symbol(const std::string&) const;
    const_symbol_ptr lookup_local_symbol(const std::string&) const;
          symbol_ptr lookup_local_symbol(const std::string&);
          symbol_ptr lookup_make_local_symbol(const std::string&);
    const_symbol_ptr lookup_external_symbol(const std::string&) const;

    const_type_ptr lookup_type(const std::string&) const;
    const_type_ptr lookup_resolved_type(const std::string&) const;
    const_type_ptr lookup_local_type(const std::string&) const;
    type_ptr lookup_local_type(const std::string&);
    const_type_ptr lookup_external_type(const std::string&) const;

    Infix::Operator get_operator(const std::string& name) const;

    explicit Module(const std::string&);

    explicit Module(const char*);

    explicit Module(const Haskell::Module&, const LanguageExtensions& lo, const FileContents& f);
};

const_symbol_ptr lookup_builtin_symbol(const std::string& name);

// If Module builds some things, have CompiledModule steal them at the end?
class CompiledModule
{
    std::string modid;

    std::set<std::string> dependencies_;

    std::map<Core::Var<>, Runtime::Exp> prepared_value_decls;

    std::multimap<std::string, const_symbol_ptr> aliases;

    std::map<std::string, symbol_ptr> symbols;

    std::map<std::string, type_ptr> types;

    std::map<std::string, const_symbol_ptr> exported_symbols_;
    std::multimap<std::string, FieldInfo> exported_fields_;

    std::map<std::string, const_type_ptr> exported_types_;
    DefaultEnv exported_defaults_;

    InstanceEnv local_instances_;

    EqInstanceEnv local_eq_instances_;

    std::string all_inputs_hash_;
    std::string source_hash_;
    std::vector<Message> parser_warnings_;

    std::map<std::string, std::shared_ptr<const CompiledModule>> transitively_imported_modules_;

    std::shared_ptr<FreshVarState> fresh_var_state_;

    // Compact source and ABI metadata retained in compiled-module caches for
    // diagnostics such as --dump-ffi.
    std::vector<Hs::CompiledForeignInfo> foreign_infos_;

public:

    // Discard everything-except-code since all compilation is done.
    // This "everything-except-code" should probably be a different object.
    void clear_symbol_table();

    // Now discard the code -- what is left?
    void clear_code() { prepared_value_decls.clear(); }

    const FreshVarState& fresh_var_state() const {return *fresh_var_state_;}

          FreshVarState& fresh_var_state()       {return *fresh_var_state_;}

    std::shared_ptr<FreshVarState> fresh_var_state_ptr() {return fresh_var_state_;}

    // Where is this used?
    const std::map<std::string, std::shared_ptr<const CompiledModule>>& transitively_imported_modules() const
    {
	return transitively_imported_modules_;
    }

    const std::string& all_inputs_hash() const {return all_inputs_hash_;}
    const std::string& source_hash() const {return source_hash_;}
    void show_parser_warnings(const std::string& filename, const std::string& source) const;

    const std::set<std::string>& dependencies() const {return dependencies_;}

    const std::string& name() const {return modid;}

    const InstanceEnv& local_instances() const {return local_instances_;}

    const EqInstanceEnv& local_eq_instances() const {return local_eq_instances_;}

    const std::vector<Hs::CompiledForeignInfo>& foreign_infos() const {return foreign_infos_;}

    void set_foreign_infos(std::vector<Hs::CompiledForeignInfo> infos)
    {
        foreign_infos_ = std::move(infos);
    }

    const std::map<std::string, const_symbol_ptr>& exported_symbols() const {return exported_symbols_;}
    const std::multimap<std::string, FieldInfo>& exported_fields() const {return exported_fields_;}

    const std::map<std::string, const_type_ptr>& exported_types() const {return exported_types_;}
    const DefaultEnv& exported_defaults() const {return exported_defaults_;}

    // How does this relate to the exported symbols?
    const_symbol_ptr lookup_local_symbol(const std::string& s) const;

    const_type_ptr lookup_local_type(const std::string& t) const;

    const_symbol_ptr lookup_symbol(const std::string&) const;

    void finish_value_decls( const Core::Decls<>& decls );

    std::map<Core::Var<>,Runtime::Exp> prepared_code_defs() const;

    void inflate(const Program& P);

    // The prefix is readable without Program: discovery fields and hashes contain no object references.
    // Warning locations are stored without borrowed filename pointers.
    template <class Archive>
    void save_prefix(Archive& ar) const
    {
        ar(modid, source_hash_, dependencies_, all_inputs_hash_, cereal::size_type(parser_warnings_.size()));
        for (const auto& message: parser_warnings_)
        {
            ar(message.loc.has_value());
            if (message.loc)
                ar(message.loc->begin.line, message.loc->begin.column,
                   message.loc->end.line, message.loc->end.column);
            ar(cereal::size_type(message.notes.size()));
            for (const auto& note: message.notes) ar(note.ctx);
        }
    }

    // Decode discovery information independently of symbols and imported references.
    template <class Archive>
    void load_prefix(Archive& ar)
    {
        cereal::size_type count;
        ar(modid, source_hash_, dependencies_, all_inputs_hash_, count);
        parser_warnings_.clear();
        for (cereal::size_type i = 0; i < count; ++i)
        {
            bool located;
            ar(located);
            std::optional<yy::location> loc;
            if (located)
            {
                loc.emplace();
                ar(loc->begin.line, loc->begin.column, loc->end.line, loc->end.column);
            }
            cereal::size_type n;
            ar(n);
            Notes notes;
            for (cereal::size_type j = 0; j < n; ++j)
            {
                std::string text;
                ar(text);
                notes.emplace_back(text);
            }
            parser_warnings_.emplace_back(WarningMsg, loc, notes);
        }
    }

    // Preserve the ordinary cereal interface for callers serializing complete modules.
    template <class Archive>
    void save(Archive& ar) const
    {
        save_prefix(ar);
        save_body(ar);
    }

    // Complete modules still resolve imported references through the archive's Program.
    template <class Archive>
    void load(Archive& ar)
    {
        load_prefix(ar);
        load_body(ar);
    }

    // Store owned definitions once and references by canonical name.
    template <class Archive>
    void save_body(Archive& ar) const
    {
        ar(prepared_value_decls, symbols, types);
        ar(cereal::size_type(aliases.size()));
        for (const auto& [key, symbol]: aliases)
            ar(key, symbol->name);
        ar(cereal::size_type(exported_symbols_.size()));
        for (const auto& [key, symbol]: exported_symbols_)
            ar(key, symbol->name);
        ar(cereal::size_type(exported_types_.size()));
        for (const auto& [key, type]: exported_types_)
            ar(key, type->name);
        ar(exported_fields_, exported_defaults_, local_instances_, local_eq_instances_, fresh_var_state_);
        ar(foreign_infos_);
    }

    // Definitions precede references, and dependencies are already in Program. Resolve each entry
    // directly into its final map, preserving ambiguous aliases without exposing an unresolved module.
    template <class Archive>
    void load_body(Archive& ar)
    {
        const auto& program = cereal::get_user_data<const Program>(ar);
        ar(prepared_value_decls, symbols, types);
        cereal::size_type count;
        std::string key, name;
        aliases.clear();
        // Match cereal's multimap insertion order, including the order of ambiguous-name diagnostics.
        auto hint = aliases.begin();
        ar(count);
        for (cereal::size_type i = 0; i < count; ++i)
        {
            ar(key, name);
            hint = aliases.emplace_hint(hint, key, resolve_symbol_reference(program, name));
        }
        exported_symbols_.clear();
        ar(count);
        for (cereal::size_type i = 0; i < count; ++i)
        {
            ar(key, name);
            exported_symbols_.emplace(key, resolve_symbol_reference(program, name));
        }
        exported_types_.clear();
        ar(count);
        for (cereal::size_type i = 0; i < count; ++i)
        {
            ar(key, name);
            exported_types_.emplace(key, resolve_type_reference(program, name));
        }
        ar(exported_fields_, exported_defaults_, local_instances_, local_eq_instances_, fresh_var_state_);
        ar(foreign_infos_);
    }

    CompiledModule(const std::shared_ptr<Module>& m);

private:
    const_symbol_ptr resolve_symbol_reference(const Program&, const std::string&) const;
    const_type_ptr resolve_type_reference(const Program&, const std::string&) const;

    friend class module_loader;
    friend class cereal::access;
    CompiledModule() = default;
};

std::ostream& operator<<(std::ostream&, const Module&);

bool special_prelude_symbol(const std::string& name);

extern std::set<std::string> special_prelude_symbols;
#endif
