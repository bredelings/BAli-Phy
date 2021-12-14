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
#include "computation/parser/haskell.H"
#include "computation/typecheck/kind.H"
#include "computation/optimization/simplifier.H"       // for simplifier options

constexpr auto unknown_fix = Haskell::Fixity::unknown;
constexpr auto left_fix = Haskell::Fixity::infixl;
constexpr auto right_fix = Haskell::Fixity::infixr;
constexpr auto non_fix = Haskell::Fixity::infix;
typedef Haskell::Fixity fixity_t;

// "Fixity is a property of a particular entity (constructor or variable), just like its type;
//  fixity is not a property of that entity’s name."
struct fixity_info
{
    fixity_t fixity = unknown_fix;
    int precedence = -1;
};


enum symbol_type_t {variable_symbol = 0, constructor_symbol = 1, class_method_symbol = 2, unknown_symbol = 3};

struct symbol_info
{
    std::string name;
    symbol_type_t symbol_type;
    std::optional<std::string> parent;
    int arity = -1;
    fixity_info fixity;

    symbol_info() {};
    symbol_info(const std::string&, symbol_type_t, const std::optional<std::string>& p, int);
    symbol_info(const std::string&, symbol_type_t, const std::optional<std::string>& p, int, fixity_info);
};

enum class type_name_category {type_class, type_syn, type_func, ADT};

struct type_info
{
    std::string name;
    type_name_category category;
    std::optional<fixity_info> fixity;
    std::optional<int> arity;
    kind k;
};

bool operator==(const symbol_info&S1, const symbol_info& S2);
bool operator!=(const symbol_info&S1, const symbol_info& S2);

bool operator==(const type_info&S1, const type_info& S2);
bool operator!=(const type_info&S1, const type_info& S2);

bool is_haskell_varid(const std::string& s);
bool is_haskell_conid(const std::string& s);
bool is_haskell_id(const std::string& s);
bool is_haskell_varsym(const std::string& s);
bool is_haskell_consym(const std::string& s);
bool is_haskell_normal_con_name(const std::string& s);
bool is_haskell_builtin_con_name(const std::string& s);
bool is_haskell_builtin_type_name(const std::string& s);
bool is_haskell_sym(const std::string& s);
bool is_haskell_uqsym(const std::string& s);
bool is_haskell_qsym(const std::string& s);
bool is_haskell_qid(const std::string& s);

bool is_haskell_var_name(const std::string& s);
bool is_haskell_con_name(const std::string& s);
bool is_haskell_module_name(const std::string& s);

bool is_qualified_symbol(const std::string&);

std::string get_module_name(const std::string&);
std::string get_unqualified_name(const std::string&);

std::vector<std::string> get_haskell_identifier_path(const std::string& s);

struct module_import
{
    std::string name;
    bool qualified = false;
    std::string as;
    bool only = false;
    bool hiding = false;
    std::vector<Haskell::Export> symbols;
    module_import() = default;
    module_import(const std::string& s):name(s),as(s) {}
};

class module_loader;

class Program;

class Module
{
    std::map<std::string, symbol_info> symbols;

    std::multimap<std::string, std::string> aliases;

    std::map<std::string, type_info> types;

    std::multimap<std::string, std::string> type_aliases;

    std::map<std::string, symbol_info> exported_symbols_;

    std::map<std::string, type_info> exported_types_;

    std::set<std::string> language_options;

    bool resolved = false;

    bool optimized = false;

public:

    bool do_optimize = true;
    
    Haskell::Module module;

    CDecls value_decls;

    std::vector<std::vector<expression_ref>> class_and_type_decls;

    std::map<var,expression_ref> small_decls_in;

    std::set<var> small_decls_in_free_vars;

    std::map<var,expression_ref> small_decls_out;

    std::set<var> small_decls_out_free_vars;

    std::string name;

    std::shared_ptr<std::string> filename;

    bool is_resolved() const {return resolved;}

    bool is_optimized() const {return optimized;}

    std::set<std::string> dependencies() const;

    std::vector<module_import> imports() const;

    const std::map<std::string, symbol_info>& exported_symbols() const {return exported_symbols_;}

    const std::map<std::string, type_info>& exported_types() const {return exported_types_;}

    void export_symbol(const symbol_info& S);

    void export_type(const type_info& S);

    void export_module(const std::string& S);

    /// Add a function
    void def_function(const std::string& name);

    /// Add a constructor
    void def_constructor(const std::string& name, int arity, const std::string& type_name);

    /// Add an ADT
    void def_ADT(const std::string& name);
    void def_ADT(const std::string& name, const fixity_info&);

    void def_type_class(const std::string& class_name);

    void def_type_class_method(const std::string& method_name, const std::string& class_name);

    void def_type_synonym(const std::string& syn_name);

    void add_symbol(const symbol_info&);

    void add_type(const type_info&);

    void add_alias(const std::string&, const std::string&);

    void add_type_alias(const std::string&, const std::string&);

    void declare_fixity(const std::string&, int precedence, fixity_t f);

    void declare_symbol(const symbol_info&);

    void declare_type(const type_info&);

    void import_symbol(const symbol_info&, const std::string&, bool qualified);

    void import_type(const type_info&, const std::string&, bool qualified);

    void import_module(const Program& P, const module_import& I);

    void compile(const Program&);

    void declare_fixities();

    void declare_fixities(const Hs::Decls&);

    void add_local_symbols(const Hs::Decls&);

    void perform_imports(const Program&);

    void perform_exports();

    Hs::Decls rename_infix(const Hs::Decls&);

    Hs::Decls rename(const simplifier_options&, Hs::Decls);

    std::vector< std::vector< expression_ref > > find_type_groups(const std::vector<expression_ref>&);

    CDecls desugar(const simplifier_options&, const Hs::Decls&);

    std::map<var,expression_ref> import_small_decls(const Program&);

    std::pair< std::map<var,expression_ref>, std::set<var> > export_small_decls(const CDecls&, const std::map< var, expression_ref>& );

    std::map<std::string,expression_ref> code_defs() const;

    CDecls optimize(const Program&, CDecls);

    CDecls load_builtins(const module_loader&, const Hs::Decls& topdecls, CDecls);

    CDecls load_constructors(const Hs::Decls&, CDecls);

    bool is_declared(const std::string&) const;

    bool type_is_declared(const std::string&) const;

    bool symbol_in_scope_with_name(const std::string&, const std::string&) const;

    bool type_in_scope_with_name(const std::string&, const std::string&) const;

    static std::pair<symbol_info,expression_ref> lookup_builtin_symbol(const std::string&);

    static type_info lookup_builtin_type(const std::string&);

    symbol_info lookup_symbol(const std::string&) const;

    symbol_info lookup_resolved_symbol(const std::string&) const;

    symbol_info get_operator(const std::string& name) const;

    type_info lookup_type(const std::string&) const;

    type_info lookup_resolved_type(const std::string&) const;

    explicit Module(const std::string&);

    explicit Module(const char*);

    explicit Module(const Haskell::Module&, const std::set<std::string>& lo);
};

std::ostream& operator<<(std::ostream&, const Module&);

expression_ref resolve_refs(const Program&, const expression_ref&);
#endif
