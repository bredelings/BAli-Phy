#include <set>
#include <regex>
#include <tuple>
#include "computation/module.H"
#include "util/myexception.H"
#include "range/v3/all.hpp"
#include "util/range.H"
#include "util/set.H"   // for add( , )
#include "util/string/split.H"
#include "util/string/join.H"
#include "program.H"
#include "operations.H"
#include "computation/machine/graph_register.H" // for graph_normalize( )
#include "computation/operations.H"
#include "parser/rename.H"
#include "parser/desugar.H"
#include "typecheck/kindcheck.H"
#include "computation/loader.H"
#include "expression/AST_node.H"
#include "expression/var.H"
#include "expression/lambda.H"
#include "expression/let.H"
#include "expression/case.H"
#include "expression/tuple.H"
#include "expression/substitute.H"
#include "computation/optimization/simplifier.H"
#include "computation/optimization/occurrence.H"
#include "computation/optimization/float-out.H"
#include "computation/optimization/inliner.H"
#include "computation/parser/haskell.H"
#include "util/graph.H"

namespace views = ranges::views;

using std::pair;
using std::map;
using std::set;
using std::optional;
using std::multiset;
using std::string;
using std::vector;
using std::tuple;

symbol_info::symbol_info(const std::string& s, symbol_type_t st, const optional<string>& p, int a)
    :name(s), symbol_type(st), parent(p), arity(a)
{ }

symbol_info::symbol_info(const std::string& s, symbol_type_t st, const optional<string>& p, int a, fixity_info f)
    :name(s), symbol_type(st), parent(p), arity(a), fixity(f)
{
}


bool operator==(const symbol_info&S1, const symbol_info& S2)
{
    return (S1.name == S2.name) and (S1.symbol_type == S2.symbol_type) and 
        (S1.arity == S2.arity);
}

bool operator!=(const symbol_info&S1, const symbol_info& S2)
{
    return not (S1 == S2);
}

bool operator==(const type_info& T1, const type_info& T2)
{
    return (T1.name == T2.name) and (T1.category == T2.category);
}

bool operator!=(const type_info& T1, const type_info& T2)
{
    return not (T1 == T2);
}

symbol_info lookup_symbol(const string& name, const Program& P);

Haskell::Decls make_topdecls(const CDecls& cdecls)
{
    if (cdecls.empty()) return {};

    vector<expression_ref> decls;
    for(auto& [x,e]: cdecls)
	decls.push_back( Haskell::ValueDecl(x,e) );

    return {decls};
}

void Module::add_type(const type_info& T)
{
    if (is_haskell_builtin_type_name(T.name))
        throw myexception()<<"Can't add builtin symbol '"<<T.name<<"'";

    if (not is_qualified_symbol(T.name))
        throw myexception()<<"Type '"<<T.name<<"' unqualified, can't be added to symbol table";

    auto loc = types.find(T.name);
    if (loc == types.end())
        types[T.name] = T;
    else if (loc != types.end() and loc->second != T)
        throw myexception()<<"Trying to add type '"<<T.name<<"' twice to module '"<<name<<"' with different body";
}

void Module::add_symbol(const symbol_info& S)
{
    if (is_haskell_builtin_con_name(S.name))
        throw myexception()<<"Can't add builtin symbol '"<<S.name<<"'";

    if (not is_qualified_symbol(S.name))
        throw myexception()<<"Symbol '"<<S.name<<"' unqualified, can't be added to symbol table";

    auto loc = symbols.find(S.name);
    if (loc == symbols.end())
        symbols[S.name] = S;
    else if (loc != symbols.end() and loc->second != S)
        throw myexception()<<"Trying to add symbol '"<<S.name<<"' twice to module '"<<name<<"' with different body";
}

void Module::add_alias(const string& identifier_name, const string& resolved_name)
{
    if (not symbols.count(resolved_name))
        throw myexception()<<"Can't add alias '"<<identifier_name<<"' -> '"<<resolved_name<<"' in module '"<<name<<"' because '"<<resolved_name<<"' is neither declared nor imported.";

    // Don't add duplicate aliases.
    if (symbol_in_scope_with_name(resolved_name, identifier_name)) return;

    aliases.insert( {identifier_name, resolved_name} );
}

void Module::add_type_alias(const string& identifier_name, const string& resolved_name)
{
    if (not types.count(resolved_name))
        throw myexception()<<"Can't add alias '"<<identifier_name<<"' -> '"<<resolved_name<<"' in module '"<<name<<"' because '"<<resolved_name<<"' is neither declared nor imported.";

    // Don't add duplicate aliases.
    if (type_in_scope_with_name(resolved_name, identifier_name)) return;

    type_aliases.insert( {identifier_name, resolved_name} );
}

void Module::declare_symbol(const symbol_info& S)
{
    if (is_qualified_symbol(S.name))
        throw myexception()<<"Locally defined symbol '"<<S.name<<"' should not be qualified in declaration.";

    symbol_info S2 = S;
    S2.name = name + "." + S.name;

    if (symbols.count(S2.name))
    {
        // FIXME! We created a partially-empty symbol to hold the fixity information.
        auto& S3 = symbols.at(S2.name);

        if (S3.symbol_type == unknown_symbol)
        {
            S2.fixity = S3.fixity;
            S3 = S2;
            return;
        }
        else
            throw myexception()<<"Trying to declare '"<<S.name<<"' twice in module '"<<name<<"'";
    }

    // Add the symbol first.
    add_symbol(S2);
    // Add the alias for qualified name: S.name -> S2.name;
    add_alias(S2.name, S2.name);
    // Add the alias for unqualified name: S.name -> S2.name;
    add_alias(S.name, S2.name);
}

void Module::declare_type(const type_info& T)
{
    if (is_haskell_builtin_type_name(T.name))
        throw myexception()<<"Can't declare builtin type name '"<<T.name<<"' as local type!.";

    if (is_qualified_symbol(T.name))
        throw myexception()<<"Locally defined symbol '"<<T.name<<"' should not be qualified in declaration.";

    auto T2 = T;
    T2.name = name + "." + T.name;

    if (types.count(T2.name))
        throw myexception()<<"Trying to declare '"<<T.name<<"' twice in module '"<<name<<"'";

    // Add the symbol first.
    add_type(T2);
    // Add the alias for qualified name: S.name -> S2.name;
    add_type_alias(T2.name, T2.name);
    // Add the alias for unqualified name: S.name -> S2.name;
    add_type_alias(T.name, T2.name);
}

// "Also like a type signature, a fixity declaration can only occur in the same sequence of declarations as the declaration of the operator itself, and at most one fixity declaration may be given for any operator."

// "Fixity is a property of a particular entity (constructor or variable), just like its type; fixity is not a property of that entity’s name."
void Module::declare_fixity(const std::string& s, int precedence, fixity_t fixity)
{
    if (is_qualified_symbol(s))
        throw myexception()<<"Trying to declare fixity of qualified symbol '"<<s<<"'.  Use its unqualified name.";

    if (precedence < 0 or precedence > 9)
        throw myexception()<<"Precedence level "<<precedence<<" not allowed.";

    if (fixity == unknown_fix)
        throw myexception()<<"Cannot set fixity to unknown!";

    string s2 = name + "." + s;

    if (not symbols.count(s2))
        declare_symbol({s, unknown_symbol, {}, -1, {unknown_fix,-1}});

    symbol_info& S = symbols.find(s2)->second;

    S.fixity.precedence = precedence;
    S.fixity.fixity = fixity;
}

// Question: what if we import m1.s, which depends on an unimported m2.s?
void Module::import_symbol(const symbol_info& S, const string& modid, bool qualified)
{
    if (not is_qualified_symbol(S.name))
        throw myexception()<<"Imported symbols must have qualified names.";

    // Add the symbol.
    add_symbol(S);
    // Add the alias for qualified name.
    add_alias(modid+"."+get_unqualified_name(S.name), S.name);
    // Add the alias for unqualified name.
    if (not qualified)
        add_alias(get_unqualified_name(S.name), S.name);
}

// Question: what if we import m1.s, which depends on an unimported m2.s?
void Module::import_type(const type_info& T, const string& modid, bool qualified)
{
    if (not is_qualified_symbol(T.name))
        throw myexception()<<"Imported symbols must have qualified names.";

    // Add the symbol.
    add_type(T);
    // Add the alias for qualified name.
    add_type_alias(modid+"."+get_unqualified_name(T.name), T.name);
    // Add the alias for unqualified name.
    if (not qualified)
        add_type_alias(get_unqualified_name(T.name), T.name);
}

bool contains_import(const Haskell::Export& e, const string& name)
{
    if (auto em = std::get_if<Haskell::ExportModule>(&e))
        throw myexception()<<"found "<<em->print()<<" in import list!";

    auto es = std::get_if<Haskell::ExportSymbol>(&e);
    auto s_name = unloc(es->symbol);

    return s_name == name;
}

bool contains_import(const vector<Haskell::Export>& es, const string& name)
{
    for(auto& e: es)
        if (contains_import(e, name))
            return true;
    return false;
}


void Module::import_module(const Program& P, const module_import& I)
{
    auto& M2 = P.get_module(I.name);
    auto& modid = I.as;
    bool qualified = I.qualified;
    assert(modid != name);

    // Right now 'exports' only has functions, not data types or constructors

    auto& m2_exports = M2.exported_symbols();
    auto& m2_exported_types = M2.exported_types();

    if (I.only)
    {
        for(const auto& s: I.symbols)
        {
            if (auto e = std::get_if<Haskell::ExportModule>(&s))
                throw myexception()<<"module "<<I.name<<": found "<<e->print()<<" in import list!";
            auto e = std::get_if<Haskell::ExportSymbol>(&s);
            auto s_name = unloc(e->symbol);

            if (m2_exports.count(s_name))
            {
                const symbol_info& S = m2_exports.at(s_name);
                import_symbol(S, modid, qualified);
            }
            else if (m2_exported_types.count(s_name))
            {
                auto& T = m2_exported_types.at(s_name);
                import_type(T, modid, qualified);
            }
        }
    }
    else
    {
        for(const auto& [_,S]: m2_exports)
        {
            auto unqualified_name = get_unqualified_name(S.name);

            if (I.hiding and contains_import(I.symbols, unqualified_name)) continue;

            import_symbol(S, modid, qualified);
        }
        for(const auto& [_,T]: m2_exported_types)
        {
            auto unqualified_name = get_unqualified_name(T.name);

            if (I.hiding and contains_import(I.symbols, unqualified_name)) continue;

            import_type(T, modid, qualified);
        }
    }
}

module_import parse_import(const Haskell::ImpDecl& impdecl)
{
    module_import mi;

    mi.qualified = impdecl.qualified;
            
    mi.name = impdecl.modid;
            
    if (impdecl.as)
        mi.as = *impdecl.as;
    else
        mi.as = mi.name;

    if (auto impspec = impdecl.impspec)
    {
        mi.hiding = impspec->hiding;
        mi.only = not mi.hiding;

        for(auto& x: impspec->imports)
            mi.symbols.push_back(x);
    }

    return mi;
}

vector<module_import> Module::imports() const
{
    vector<module_import> imports_list;

    bool seen_Prelude = false;
    for(const auto& impdecl: module.impdecls)
    {
        auto import = parse_import(impdecl);
        if (import.name == "Prelude")
            seen_Prelude = true;
        imports_list.push_back(import);
    }

    // Import the Prelude if it wasn't explicitly mentioned in the import list.
    if (not seen_Prelude and name != "Prelude" and not language_options.count("NoImplicitPrelude"))
        imports_list.push_back( module_import("Prelude") );

    return imports_list;
}

set<string> Module::dependencies() const
{
    set<string> modules;
    for(auto& mi: imports())
        modules.insert(mi.name);
    return modules;
}

void Module::compile(const Program& P)
{
    assert(not resolved);
    resolved = true;
    simplifier_options& opts = *P.get_module_loader();

    perform_imports(P);

    declare_fixities();

    // FIXME - merge with rename() below.
    // Currently this (1) translates field-decls into function declarations
    //                (2) rewrites @ f x y -> f x y (where f is the head) using unapply( ).
    //                (3) rewrites infix expressions through desugar_infix( )
    if (module.topdecls)
        module.topdecls = rename_infix(*module.topdecls);

    add_local_symbols();

    // Currently we do "renaming" here.
    // That just means (1) qualifying top-level declarations and (2) desugaring rec statements.
    if (module.topdecls)
        module.topdecls = rename(opts, *module.topdecls);

    if (module.topdecls)
    {
        // It should be possible to replace each of these (i) an object (ii) that is located.
        vector<expression_ref> tmp;
        for(auto& decl: *module.topdecls)
            if (decl.is_a<Haskell::ClassDecl>() or
                decl.is_a<Haskell::InstanceDecl>() or
                decl.is_a<Haskell::DataOrNewtypeDecl>() or
                decl.is_a<Haskell::TypeSynonymDecl>()
                )
                tmp.push_back(decl);

        class_and_type_decls = find_type_groups(tmp);
    }

    perform_exports();

    // look only in value_decls now
    if (module.topdecls)
        value_decls = desugar(opts, *module.topdecls);

    load_builtins(*P.get_module_loader());

    load_constructors();

    get_types(P);

    // Check for duplicate top-level names.
    check_duplicate_var(value_decls);

    import_small_decls(P);

    optimize(P);

    export_small_decls();
}

void Module::perform_imports(const Program& P)
{
    for(auto& i: imports())
        import_module(P, i);
}

void Module::export_symbol(const symbol_info& S)
{
    assert(is_qualified_symbol(S.name));
    auto uname = get_unqualified_name(S.name);
    if (not exported_symbols_.count(uname))
        exported_symbols_[uname] = S;
    // FIXME, this doesn't really say how the entities (which are different) were referenced in the export list
    else if (exported_symbols_[uname].name != S.name)
        throw myexception()<<"attempting to export both '"<<exported_symbols_[uname].name<<"' and '"<<S.name<<"', which have the same unqualified name!";
}

void Module::export_type(const type_info& T)
{
    assert(is_qualified_symbol(T.name));
    auto uname = get_unqualified_name(T.name);
    if (not exported_types_.count(uname))
        exported_types_[uname] = T;
    // FIXME, this doesn't really say how the entities (which are different) were referenced in the export list
    else if (exported_types_[uname].name != T.name)
        throw myexception()<<"attempting to export both '"<<exported_types_[uname].name<<"' and '"<<T.name<<"', which have the same unqualified name!";
}

bool Module::symbol_in_scope_with_name(const string& symbol_name, const string& id_name) const
{
    auto range = aliases.equal_range(id_name);
    for(auto it = range.first; it != range.second; it++)
        if (it->second == symbol_name)
            return true;
    return false;
}

bool Module::type_in_scope_with_name(const string& type_name, const string& id_name) const
{
    auto range = type_aliases.equal_range(id_name);
    for(auto it = range.first; it != range.second; it++)
        if (it->second == type_name)
            return true;
    return false;
}

// From the Haskell 2010 report:
//    The form “module M” names the set of all entities that are in scope with both an unqualified name
//    “e” and a qualified name “M.e”. This set may be empty.
void Module::export_module(const string& modid)
{
    if (modid != name and not dependencies().count(modid))
        throw myexception()<<"module '"<<modid<<"' is exported but not imported";

    // Find all the symbols that are in scope as both `modid.e` and `e`
    for(auto& [id_name, symbol_name]: aliases)
        if (is_qualified_symbol(id_name) and
            get_module_name(id_name) == modid and
            symbol_in_scope_with_name(symbol_name, get_unqualified_name(id_name)))
        {
            try
            {
                auto S = lookup_symbol(id_name);
                export_symbol(S);
            }
            catch (myexception& e)
            {
                e.prepend("exporting module '"+modid+"': ");
                throw;
            }
        }

    // Find all the symbols that are in scope as both `modid.e` and `e`
    for(auto& [id_name, type_name]: type_aliases)
        if (is_qualified_symbol(id_name) and
            get_module_name(id_name) == modid and
            type_in_scope_with_name(type_name, get_unqualified_name(id_name)))
        {
            try
            {
                auto T = lookup_type(id_name);
                export_type(T);
            }
            catch (myexception& e)
            {
                e.prepend("exporting module '"+modid+"': ");
                throw;
            }
        }
}

void Module::perform_exports()
{
    // Currently we just export the local symbols
    if (not module.exports or module.exports->size() == 0)
        export_module(name);
    else
    {
        for(auto& ex: *module.exports)
        {
            if (auto e = std::get_if<Haskell::ExportSymbol>(&ex))
            {
                string qvarid = unloc(e->symbol); // This ignores export subspec - see grammar.
                if (aliases.count(qvarid))
                    export_symbol(lookup_symbol(qvarid));
                else if (type_aliases.count(qvarid))
                    export_type(lookup_type(qvarid));
                else
                    throw myexception()<<"trying to export variable '"<<qvarid<<"', which is not in scope.";
            }
            else if (auto e = std::get_if<Haskell::ExportModule>(&ex))
            {
                string modid = unloc(e->modid);
                export_module(modid);
            }
        }
    }
}

map<string,expression_ref> Module::code_defs() const
{
    map<string, expression_ref> code;

    for(const auto& [x,rhs]: value_decls)
    {
        assert(is_qualified_symbol(x.name));

        if (this->name == get_module_name(x.name))
        {
            // get the body for the  decl
            assert(rhs);

            code[x.name] = rhs;
        }
    }

    return code;
}

Hs::Decls Module::rename_infix(const Hs::Decls& topdecls)
{
    return ::rename_infix_top(*this, topdecls);
}

Hs::Decls Module::rename(const simplifier_options& opts, Hs::Decls topdecls)
{
    topdecls = ::rename(*this, topdecls);

    if (opts.dump_renamed)
        std::cout<<name<<"[renamed]:\n"<<topdecls.print()<<"\n\n";

    return topdecls;
}

set<string> free_type_names_from_type(const Haskell::Type& type)
{
    set<string> tvars;
    if (auto tc = type.to<Haskell::TypeCon>())
    {
        auto& name = unloc(tc->name);
        if (is_qualified_symbol(name))
            tvars.insert(name);
    }
    else if (type.is_a<Haskell::TypeApp>())
    {
        auto& app = type.as_<Haskell::TypeApp>();
        add(tvars, free_type_names_from_type(app.head));
        add(tvars, free_type_names_from_type(app.arg));
    }
    else if (type.is_a<Haskell::TupleType>())
    {
        auto& tuple = type.as_<Haskell::TupleType>();
        for(auto element_type: tuple.element_types)
            add(tvars, free_type_names_from_type(element_type));
    }
    else if (type.is_a<Haskell::ListType>())
    {
        auto& list = type.as_<Haskell::ListType>();
        return free_type_names_from_type(list.element_type);
    }
    else if (auto forall = type.to<Haskell::ForallType>())
    {
        return free_type_names_from_type(forall->type);
    }
    return tvars;
}

set<string> free_type_vars_from_type(const Haskell::Type& type)
{
    set<string> tvars;
    if (auto tv = type.to<Haskell::TypeVar>())
    {
        auto& name = unloc(tv->name);
        tvars.insert(name);
    }
    else if (type.is_a<Haskell::TypeApp>())
    {
        auto& app = type.as_<Haskell::TypeApp>();
        add(tvars, free_type_vars_from_type(app.head));
        add(tvars, free_type_vars_from_type(app.arg));
    }
    else if (type.is_a<Haskell::TupleType>())
    {
        auto& tuple = type.as_<Haskell::TupleType>();
        for(auto element_type: tuple.element_types)
            add(tvars, free_type_vars_from_type(element_type));
    }
    else if (type.is_a<Haskell::ListType>())
    {
        auto& list = type.as_<Haskell::ListType>();
        return free_type_vars_from_type(list.element_type);
    }
    else if (auto forall = type.to<Haskell::ForallType>())
    {
        tvars = free_type_vars_from_type(forall->type);
        for(auto& type_var: forall->type_var_binders)
        {
            tvars.erase(unloc(type_var.name));
        }
    }
    return tvars;
}

set<string> free_type_vars(const Haskell::Context& context)
{
    set<string> tvars;
    for(auto& constraint: context.constraints)
        add(tvars, free_type_vars_from_type(constraint));
    return tvars;
}

set<string> free_type_names(const Haskell::Context& context)
{
    set<string> tvars;
    for(auto& constraint: context.constraints)
        add(tvars, free_type_names_from_type(constraint));
    return tvars;
}

set<string> free_type_names(const Haskell::ClassDecl& class_decl)
{
    // QUESTION: We are ignoring default methods here -- should we?
    set<string> tvars;
    add(tvars, free_type_names(class_decl.context));
    if (class_decl.decls)
    {
        for(auto& decl: unloc(*class_decl.decls))
        {
            if (decl.is_a<Haskell::SignatureDecl>())
            {
                auto& T = decl.as_<Haskell::SignatureDecl>();
                add(tvars, free_type_names_from_type(T.type));
            }
        }
    }
    return tvars;
}

set<string> free_type_names(const Haskell::DataOrNewtypeDecl& type_decl)
{
    set<string> tvars;
    add(tvars, free_type_names(type_decl.context));
    for(auto& constr: type_decl.constructors)
    {
        if (constr.context)
            add(tvars, free_type_names(*constr.context));

        if (constr.is_record_constructor())
        {
            auto& field_decls = std::get<1>(constr.fields).field_decls;
            for(auto& field: field_decls)
                add(tvars, free_type_names_from_type(field.type));
        }
        else
        {
            auto& types = std::get<0>(constr.fields);
            for(auto& type: types)
                add(tvars, free_type_names_from_type(type));
        }
    }
    return tvars;
}

set<string> free_type_names(const Haskell::TypeSynonymDecl& synonym_decl)
{
    return free_type_names_from_type(unloc(synonym_decl.rhs_type));
}

set<string> free_type_names(const Haskell::InstanceDecl& instance_decl)
{
    set<string> tvars;
    add(tvars, free_type_names(instance_decl.context));
    tvars.insert(instance_decl.name);
    for(auto& type_arg: instance_decl.type_args)
        add(tvars, free_type_names_from_type(type_arg));
    return tvars;

}

set<string> from_this_module(const string& mod_name, set<string> names)
{
    set<string> ok_names;
    for(auto& name: names)
        if (get_module_name(name) == mod_name)
            ok_names.insert(name);
    return ok_names;
}

vector<vector<expression_ref>> Module::find_type_groups(const vector<expression_ref>& initial_class_and_type_decls)
{
    // 1. Collection type and instance declarations

    // [(name,names-we-depend-on)]  No instances.
    map<string,set<string>> referenced_types;

    map<string, expression_ref> decl_for_type;

    vector<tuple<Haskell::InstanceDecl,set<string>>> instance_decls;

    for(auto& decl: initial_class_and_type_decls)
    {
        if (decl.is_a<Haskell::ClassDecl>())
        {
            auto& class_decl = decl.as_<Haskell::ClassDecl>();
            referenced_types[class_decl.name] = free_type_names(class_decl);
            decl_for_type[class_decl.name] = decl;
        }
        else if (decl.is_a<Haskell::DataOrNewtypeDecl>())
        {
            auto& type_decl = decl.as_<Haskell::DataOrNewtypeDecl>();
            referenced_types[type_decl.name] = free_type_names(type_decl);
            decl_for_type[type_decl.name] = decl;
        }
        else if (decl.is_a<Haskell::TypeSynonymDecl>())
        {
            auto& type_decl = decl.as_<Haskell::TypeSynonymDecl>();
            referenced_types[type_decl.name] = free_type_names(type_decl);
            decl_for_type[type_decl.name] = decl;
        }
        else if (decl.is_a<Haskell::InstanceDecl>())
        {
            auto& instance_decl = decl.as_<Haskell::InstanceDecl>();
            instance_decls.push_back({instance_decl, free_type_names(instance_decl)});
        }
        else
            std::abort();
    }

    // 2. Get order list of mutually dependent TYPE declarations

    // Input to the dependency analysis is a list of
    // * (declaration, name, names that this declaration depends on)

    for(auto& [type, referenced_types]: referenced_types)
        referenced_types = from_this_module(name, referenced_types);

    auto ordered_name_groups = get_ordered_strong_components(referenced_types);

    auto type_decl_groups = map_groups( ordered_name_groups, decl_for_type );

    // 3. Compute kinds for type/class constructors.
    for(auto& type_decl_group: type_decl_groups)
    {
        kindchecker_state K(*this);
        for(auto& [name,ka]: K.infer_kinds(type_decl_group))
        {
            auto& [arity,k] = ka;
            auto& tinfo = types.at(name);
            tinfo.arity = arity;
            tinfo.k     = k;
        }
    }


    // 4. Compute types and kinds for data constructors and class methods?
    for(auto& type_decl_group: type_decl_groups)
    {
        kindchecker_state K(*this);
        for(auto& _: K.infer_child_types(type_decl_group))
        {
//            auto& [arity,k] = ka;
//            auto& tinfo = types.at(name);
//            tinfo.arity = arity;
//            tinfo.k     = k;
        }
    }

    // FIXME: Handle instances.

    // Instances: an instance is a function from dictionaries a dictionaries.
    //    instance (A a, B b) => A (b a) is a function of the form \dict_A_a dict_B_b -> dict_A_(b_a)

    // Q: How are instances grouped?
    // A: Each instance needs to be at-or-after all the types/classes referenced,
    //    Do instances depend on other instances?  Maybe this is check in the context...
    //    e.g. instance Eq a => Eq [a] where

    // See equivalents in GHC Rename/Module.hs
    // We are trying to find strongly connected components of
    //  types, type classes, and instances.

    // Shouldn't instances be AFTER everything?
    // * We only have type class instances (ClsInstDecl), but GHC
    //   also has data family instances and type family instances.

    // GHC looks at types and classes first, then adds instances to the SCCs later.


    // 5. Compute types for functions.

    //   Does the type-checker need to augment all bound variables with their type?

    //   Does the type-checker need to add type lambdas?

    //   Does the type-checker need to specify type arguments to type lambdas?

    //   So, let, lambda, and case would need to specify the type

    // 6. Compute types for class default methods?

    // Q: How are default method declarations handled here?
    //    Do they affect type class resolution?
    //    Do we need to do more work on them when handling value decls?
    // A: I think default methods do not affect the type.

    // See function `rnTyClDecls`, which calls `depAnalTyClDecls`.
    // * Dependency analysis on values can be done by name, since instances are not included.
    // * Code is in GHC.Data.Graph.Directed.

    // I don't think we need to look up "parents" during typechecking unless we are promoting data constructors
    // to types or kinds.

    // For values, each value can have a body decl, a fixity decl, and a signature decl.
    // So we can't use the decl itself as the key -- we have to use something like the name.

    // It looks like GHC rename extracts the "free variables" from everything.
    // For example: rnSrcInstDecl operates on ClsInstD, which wraps ClsInstDecl from Hs/Decl.hs

    // FreeVars = OccEnv ID.  See Core/Opt/OccurAnal.hs.

    // Looks like code for determining inlining

    return type_decl_groups;
}

CDecls Module::desugar(const simplifier_options& opts, const Hs::Decls& topdecls)
{
    auto cdecls = ::desugar(*this, topdecls);

    if (opts.dump_desugared)
        std::cout<<name<<"[desugared]:\n"<<print_cdecls(cdecls)<<"\n\n";

    return cdecls;
}

void add_constructor(map<var,expression_ref>& decls, const constructor& con)
{
    var x(con.name());
    expression_ref body = lambda_expression(con);
    auto res = occurrence_analyzer(body);
    decls.insert({x,res.first});
}

void Module::import_small_decls(const Program& P)
{
    if (not module.topdecls) return;

    assert(small_decls_in.empty());

    // Collect small decls from imported modules;
    for(auto& imp_mod_name: dependencies())
    {
        auto& M = P.get_module(imp_mod_name);
        small_decls_in.insert(M.small_decls_out.begin(), M.small_decls_out.end());
        small_decls_in_free_vars.insert(M.small_decls_out_free_vars.begin(), M.small_decls_out_free_vars.end());
    }

    add_constructor(small_decls_in, constructor(":",2));
    add_constructor(small_decls_in, constructor("[]",0));
    add_constructor(small_decls_in, constructor("()",0));
    add_constructor(small_decls_in, constructor("(,)",2));
    add_constructor(small_decls_in, constructor("(,,)",3));
    add_constructor(small_decls_in, constructor("(,,,)",4));
    add_constructor(small_decls_in, constructor("(,,,,)",5));
    add_constructor(small_decls_in, constructor("(,,,,,)",6));
    add_constructor(small_decls_in, constructor("(,,,,,,)",7));
    add_constructor(small_decls_in, constructor("(,,,,,,,)",8));
    add_constructor(small_decls_in, constructor("(,,,,,,,,)",9));
    add_constructor(small_decls_in, constructor("(,,,,,,,,,)",10));
}

void Module::export_small_decls()
{
    assert(small_decls_out.empty());

    // Modules that we imported should have their small_decls transitively inherited
    small_decls_out = small_decls_in;

    for(auto& [x,rhs]: value_decls)
    {
        assert(not x.name.empty());

        if (simple_size(rhs) <= 5)
            small_decls_out.insert({x, rhs});
    }

    // Find free vars in the decls that are not bound by *other* decls.
    for(auto& decl: small_decls_out)
    {
        auto [E, free_vars] = occurrence_analyzer(decl.second);
        decl.second = E;

        for(auto& x: free_vars)
            if (not small_decls_out.count(x))
                small_decls_out_free_vars.insert(x);
    }
}

template <typename T>
void erase_one(multiset<T>& mset, const T& elem)
{
    auto it = mset.find(elem);
    assert(it != mset.end());
    mset.erase(it);
}

expression_ref rename(const expression_ref& E, const map<var,var>& substitution, multiset<var>& bound)
{
    assert(E);

    // 1. Var (x)
    if (E.is_a<var>())
    {
        auto& x = E.as_<var>();
        // 1.1 If there's a substitution x -> E
        if (not bound.count(x) and substitution.count(x))
            return substitution.at(x);
        else
            return E;
    }

    // 2. Lambda (E = \x -> body)
    if (E.head().is_a<lambda>())
    {
        assert(E.size() == 2);

        auto x = E.sub()[0].as_<var>();
        auto body = E.sub()[1];

        bound.insert(x);
        body = rename(body, substitution, bound);
        erase_one(bound,x);

        return lambda_quantify(x,body);
    }

    // 6. Case
    expression_ref object;
    vector<expression_ref> patterns;
    vector<expression_ref> bodies;
    if (parse_case_expression(E, object, patterns, bodies))
    {
        // Analyze the object
        object = rename(object, substitution, bound);
        for(int i=0; i<patterns.size(); i++)
        {
            for(int j=0;j<patterns[i].size(); j++)
            {
                auto& x = patterns[i].sub()[j].as_<var>();
                if (not x.is_wildcard())
                    bound.insert(x);
            }

            bodies[i] = rename(bodies[i], substitution, bound);

            for(int j=0;j<patterns[i].size(); j++)
            {
                auto& x = patterns[i].sub()[j].as_<var>();
                if (not x.is_wildcard())
                    erase_one(bound,x);
            }
        }
        return make_case_expression(object, patterns, bodies);
    }

    // 5. Let (let {x[i] = F[i]} in body)
    else if (is_let_expression(E))
    {
        auto L = E.as_<let_exp>();

        for(auto& [x,_]: L.binds)
            bound.insert(x);

        L.body = rename(L.body, substitution, bound);

        for(auto& [_,e]: L.binds)
            e = rename(e, substitution, bound);

        for(auto& [x,e]: L.binds)
            erase_one(bound, x);

        return L;
    }

    // 5. (partial) Literal constant.  Treat as 0-arg constructor.
    else if (not E.size()) return E;

    // 4. Constructor or Operation or Apply
    else if (E.head().is_a<constructor>() or E.head().is_a<Operation>())
    {
        object_ptr<expression> E2 = E.as_expression().clone();
        for(int i=0;i<E.size();i++)
            E2->sub[i] = rename(E2->sub[i], substitution, bound);
        return E2;
    }

    std::abort();
    return E;
}


std::optional<string> get_new_name(const var& x, const string& module_name)
{
    assert(not is_haskell_builtin_con_name(x.name));

    if (is_qualified_var(x))
    {
        // Allow adding suffixes like #1 to qualified names IF they are not exported.
        // Such suffixes can be added by renaming inside of let-floating.
        assert(x.index == 0 or not x.is_exported);
        return {};
    }

    return module_name + "." + x.name + "#" + convertToString(x.index);
}

CDecls rename_top_level(const CDecls& decls, const string& module_name)
{
    map<var, var> substitution;

    set<var> top_level_vars;

    CDecls decls2;

#ifndef NDEBUG
    check_duplicate_var(decls);
#endif

    for(int i = 0; i< decls.size(); i++)
    {
        auto& [x,rhs] = decls[i];
        var x2 = x;
        assert(not substitution.count(x));

        if (auto new_name = get_new_name(x, module_name))
        {
            x2 = var(*new_name);
            assert(not substitution.count(var(x2.name)));
            substitution.insert({x,x2});
        }

        decls2.push_back({x2,rhs});

        // None of the renamed vars should have the same name;
        assert(not top_level_vars.count(x2));
        top_level_vars.insert(x2);
    }

    multiset<var> bound;
    for(auto& [_,rhs]: decls2)
    {
        assert(bound.empty());
        rhs = rename(rhs, substitution, bound);
        assert(bound.empty());
    }

#ifndef NDEBUG
    check_duplicate_var(decls2);
#endif

    return decls2;
}

expression_ref func_type(const expression_ref& a, const expression_ref& b)
{
    expression_ref Arrow = right_assoc_constructor("->",1);
    return Arrow+a+b;
}

void Module::get_types(const Program&)
{
    expression_ref Star = constructor("*",0);
    //  std::cerr<<func_type(func_type(Star,Star),Star)<<"\n";
    //  std::cerr<<func_type(Star,func_type(Star,Star))<<"\n";
}

vector<expression_ref> peel_lambdas(expression_ref& E)
{
    vector<expression_ref> args;
    while(E.head().type() == lambda_type)
    {
        args.push_back(E.sub()[0]);
        E = E.sub()[1];
    }
    return args;
}

void mark_exported_decls(CDecls& decls, const map<string,symbol_info>& exports, const string& module_name)
{
    // Record exports
    set<string> exported;
    for(auto& ex: exports)
        if (get_module_name(ex.second.name) == module_name)
            exported.insert(ex.second.name);

    // Mark exported vars as exported
    for(auto& decl: decls)
    {
        if (exported.count(decl.first.name))
        {
            decl.first.is_exported = true;
            exported.erase(decl.first.name);
        }
        else
            decl.first.is_exported = false;
    }

    // Check that we don't export things that don't exist
    if (false and not exported.empty())
    {
        // FIXME: class members don't have a value def, and so this doesn't work.
        myexception e;
        e<<"Module '"<<module_name<<"' exports undefined symbols:\n";
        for(auto& name: exported)
            e<<"  "<<name;
        throw e;
    }
}

void Module::optimize(const Program& P)
{
    // 1. why do we keep on re-optimizing the same module?
    if (optimized) return;
    optimized = true;

    // 2. Graph-normalize the bodies
    for(auto& [x,rhs]: value_decls)
    {
        // This won't float things to the top level!
        rhs = graph_normalize(rhs);
    }

    if (do_optimize)
    {
        mark_exported_decls(value_decls, exported_symbols(), name);

        vector<CDecls> decl_groups = {value_decls};

        decl_groups = simplify_module_gently(*P.get_module_loader(), small_decls_in, small_decls_in_free_vars, decl_groups);

        if (P.get_module_loader()->fully_lazy)
            float_out_from_module(decl_groups);

        decl_groups = simplify_module(*P.get_module_loader(), small_decls_in, small_decls_in_free_vars, decl_groups);

        if (P.get_module_loader()->fully_lazy)
            float_out_from_module(decl_groups);

        value_decls = flatten(decl_groups);
    }

    value_decls = rename_top_level(value_decls, name);
}

pair<string,expression_ref> parse_builtin(const Haskell::BuiltinDecl& B, const module_loader& L)
{
    const string builtin_prefix = "builtin_function_";

    string operation_name = B.plugin_name+":"+B.symbol_name;

    auto body = load_builtin(L, builtin_prefix + B.symbol_name, B.plugin_name, B.n_args, operation_name);

    return {B.function_name, body};
}

void Module::load_builtins(const module_loader& L)
{
    if (not module.topdecls) return;

    for(const auto& decl: *module.topdecls)
        if (decl.is_a<Haskell::BuiltinDecl>())
        {
            auto [function_name, body] = parse_builtin(decl.as_<Haskell::BuiltinDecl>(), L);

            function_name = lookup_symbol(function_name).name;

            value_decls.push_back( { var(function_name), body} );
        }
}

string get_constructor_name(const Hs::Type& constr)
{
    auto [con,_] = Haskell::decompose_type_apps(constr);
    assert(con.is_a<Haskell::TypeCon>());
    return unloc(con.as_<Haskell::TypeCon>().name);
}

void Module::load_constructors()
{
    if (not module.topdecls) return;

    for(const auto& decl: *module.topdecls)
        if (decl.is_a<Haskell::DataOrNewtypeDecl>())
        {
            auto constrs = decl.as_<Haskell::DataOrNewtypeDecl>().constructors;
            if (constrs.size() == 0) continue;

            for(const auto& constr: constrs)
            {
                auto arity = constr.arity();
                auto cname = constr.name;

                string qualified_name = name+"."+cname;
                expression_ref body = lambda_expression( constructor(qualified_name, arity) );
                value_decls.push_back( { var(qualified_name) , body} );
            }
            // Strip out the constructor definition here new_decls.push_back(decl);
        }
}

bool Module::is_declared(const std::string& name) const
{
    return is_haskell_builtin_con_name(name) or (aliases.count(name) > 0);
}

bool Module::type_is_declared(const std::string& name) const
{
    return is_haskell_builtin_type_name(name) or (type_aliases.count(name) > 0);
}

pair<symbol_info,expression_ref> Module::lookup_builtin_symbol(const std::string& name)
{
    if (name == "()")
        return {symbol_info("()", constructor_symbol, "()", 0), constructor("()",0)};
    else if (name == "[]")
        return {symbol_info("[]", constructor_symbol, "[]", 0), constructor("[]",0)};
    else if (name == ":")
        return {symbol_info(":", constructor_symbol, "[]", 2, {right_fix,5}), lambda_expression( right_assoc_constructor(":",2) )};
    else if (is_tuple_name(name))
    {
        int arity = name.size() - 1;
        expression_ref body = lambda_expression( tuple_head(arity) );
        return {symbol_info(name, constructor_symbol, name, arity), body};
    }
    throw myexception()<<"Symbol 'name' is not a builtin (constructor) symbol.";
}

type_info Module::lookup_builtin_type(const std::string& name)
{
    if (name == "Char")
        return {"Char", type_name_category::ADT, {}, 0, make_kind_star()};
    else if (name == "Double")
        return {"Double", type_name_category::ADT, {}, 0, make_kind_star()};
    else if (name == "Int")
        return {"Int", type_name_category::ADT, {}, 0, make_kind_star()};
    else if (name == "()")
        return {"()", type_name_category::ADT, {}, 0, make_kind_star()};
    else if (name == "[]")
        return {"[]", type_name_category::ADT, {}, 1, make_n_args_kind(1)};
    else if (name == "->")
    {
        return {"->", type_name_category::type_func, {{right_fix,0}}, 2, make_n_args_kind(2)};
    }
    // This doesn't include ()
    else if (is_tuple_name(name))
    {
        int n = tuple_arity(name);

        return {name, type_name_category::ADT,{}, n, make_n_args_kind(n)};
    }
    throw myexception()<<"Symbol 'name' is not a builtin (type) symbol.";
}

symbol_info Module::lookup_symbol(const std::string& name) const
{
    if (is_haskell_builtin_con_name(name))
        return lookup_builtin_symbol(name).first;

    int count = aliases.count(name);
    if (count == 0)
        throw myexception()<<"Indentifier '"<<name<<"' not declared.";
    else if (count == 1)
    {
        string symbol_name = aliases.find(name)->second;
        if (not symbols.count(symbol_name))
            throw myexception()<<"Identifier '"<<name<<"' -> '"<<symbol_name<<"', which does not exist!";
        return symbols.find(symbol_name)->second;
    }
    else
    {
        myexception e;
        e<<"Identifier '"<<name<<"' is ambiguous!";
        auto range = aliases.equal_range(name);
        for(auto i = range.first; i != range.second ;i++)
            e<<"\n "<<i->first<<" -> "<<i->second;
        throw e;
    }
}

symbol_info Module::lookup_resolved_symbol(const std::string& symbol_name) const
{
    if (is_haskell_builtin_con_name(symbol_name))
        return lookup_builtin_symbol(symbol_name).first;

    if (not symbols.count(symbol_name))
        throw myexception()<<"Identifier '"<<name<<"' -> '"<<symbol_name<<"', which does not exist!";
    return symbols.find(symbol_name)->second;
}

symbol_info Module::get_operator(const string& name) const
{
    symbol_info S = lookup_symbol(name);

    // An operator of undefined precedence is treated as if it has the highest precedence
    if (S.fixity.precedence == -1 or S.fixity.fixity == unknown_fix) 
    {
        // If either is unset, then both must be unset!
        assert(S.fixity.precedence == -1 and S.fixity.fixity == unknown_fix);
        S.fixity.precedence = 9;
        S.fixity.fixity = left_fix;
    }

    return S;
}

type_info Module::lookup_type(const std::string& name) const
{
    if (is_haskell_builtin_type_name(name))
        return lookup_builtin_type(name);

    int count = type_aliases.count(name);
    if (count == 0)
        throw myexception()<<"Type identifier '"<<name<<"' not declared.";
    else if (count == 1)
    {
        string type_name = type_aliases.find(name)->second;
        if (not types.count(type_name))
            throw myexception()<<"Identifier '"<<name<<"' -> '"<<type_name<<"', which does not exist!";
        return types.find(type_name)->second;
    }
    else
    {
        myexception e;
        e<<"Type identifier '"<<name<<"' is ambiguous!";
        auto range = type_aliases.equal_range(name);
        for(auto i = range.first; i != range.second ;i++)
            e<<"\n "<<i->first<<" -> "<<i->second;
        throw e;
    }
}

type_info Module::lookup_resolved_type(const std::string& type_name) const
{
    if (is_haskell_builtin_type_name(type_name))
        return lookup_builtin_type(type_name);

    if (not types.count(type_name))
        throw myexception()<<"Identifier '"<<name<<"' -> '"<<type_name<<"', which does not exist!";

    return types.find(type_name)->second;
}

void parse_combinator_application(const expression_ref& E, string& name, vector<expression_ref>& patterns)
{
    expression_ref E2 = E;

    assert(E.head().is_a<Apply>());
  
    // 1. Find the head.  This should be a var or a var, not an apply.
    auto x = E.sub()[0];
    if (is_var(x))
        name = x.as_<var>().name;
    else
        throw myexception()<<"Combinator definition '"<<E<<"' does not start with variable!";

    // 2. Look through the arguments
    for(int i=1;i<E.size();i++)
        patterns.push_back(E.sub()[i]);
}

const std::regex rgx( R"(^([A-Z][a-zA-Z0-9_']*)\.)" );

vector<string> haskell_name_path(const std::string& s)
{
    vector<string> path;
    string rest = s;
    std::smatch m;
    while(std::regex_search(rest, m, rgx))
    {
        path.push_back(m[1]);
        rest = m.suffix().str();
    }
    // FIXME if the rest looks like (for example) BAli-Phy.name, but not if it looks like (for example) .|.
    // FIXME move splitting paths into components into the lexer.  That is, split there and store them already split.
    path.push_back(rest);
    return path;
}

bool is_valid_identifier(const string& s)
{
    if (is_haskell_varid(s)) return true;

    if (is_haskell_conid(s)) return true;

    if (is_haskell_varsym(s)) return true;

    if (is_haskell_consym(s)) return true;

    if (is_haskell_builtin_con_name(s)) return true;

    return false;
}

vector<string> get_haskell_identifier_path(const std::string& s)
{
    if (not s.size())
        throw myexception()<<"Empty string is not a legal Haskell identifier!";

    vector<string> path = haskell_name_path(s);

    for(int i=0;i<path.size()-1;i++)
        if (not is_haskell_conid(path[i]))
            throw myexception()<<"Module id component '"<<path[i]<<"' in identifier '"<<s<<"' is not legal!";

    return path;
}

bool haskell_is_lower(char c)
{
    return (islower(c) or c=='_');
}

bool is_haskell_id(const std::string& s)
{
    if (s.empty()) return false;

    if (not (haskell_is_lower(s[0]) or isupper(s[0]))) return false;

    for(int i=1;i<s.size();i++)
    {
        char c = s[i];
        if (not (isupper(c) or haskell_is_lower(c) or isdigit(c) or c=='\''))
            return false;
    }
    return true;
}

bool is_haskell_varid(const std::string& s)
{
    if (not is_haskell_id(s)) return false;

    return haskell_is_lower(s[0]);
}

bool is_haskell_conid(const std::string& s)
{
    if (not is_haskell_id(s)) return false;

    return isupper(s[0]);
}

bool is_haskell_uqsym(const string& s)
{
    static const string symbols = "!#$%&*+./<=>?@\\^|-~:";

    if (not s.size()) return false;

    for(int i=0;i<s.size();i++)
        if (symbols.find(s[i]) == -1)
            return false;

    return true;
}

bool is_haskell_varsym(const string& s)
{
    if (not is_haskell_uqsym(s)) return false;

    return (s[0] != ':');
}

bool is_haskell_consym(const string& s)
{
    if (not is_haskell_uqsym(s)) return false;

    if (s[0] != ':') return false;
    if (s == ":") return false;

    return true;
}

bool is_haskell_var_name(const std::string& s)
{
    vector<string> path = haskell_name_path(s);
    if (path.empty()) return false;
    if (not is_haskell_varid(path.back()) and not is_haskell_varsym(path.back())) return false;
    for(int i=0;i<path.size()-1;i++)
        if (not is_haskell_conid(path[i])) return false;
    return true;
}

bool is_haskell_builtin_con_name(const std::string& s)
{
    if (s == "()" or s == "[]" or s == ":" or is_tuple_name(s)) 
        return true;
    else
        return false;
}

bool is_haskell_builtin_type_name(const std::string& s)
{
    if (s == "()" or s == "[]" or s == "->" or is_tuple_name(s)) 
        return true;
    else if (s == "Double" or s == "Int" or s == "Char")
        return true;
    else
        return false;
}

bool valid_path_prefix(const vector<string>& path)
{
    if (path.empty()) return false;
    for(int i=0;i<path.size()-1;i++)
        if (not is_haskell_conid(path[i])) return false;
    return true;
}

bool is_haskell_qsym(const std::string& s)
{
    vector<string> path = haskell_name_path(s);
    return valid_path_prefix(path) and is_haskell_uqsym(path.back());
}

bool is_haskell_sym(const std::string& s)
{
    return is_haskell_uqsym(s) or is_haskell_qsym(s);
}

bool is_haskell_qid(const std::string& s)
{
    vector<string> path = haskell_name_path(s);
    return valid_path_prefix(path) and is_haskell_id(path.back());
}

bool is_haskell_normal_con_name(const std::string& s)
{
    vector<string> path = haskell_name_path(s);
    if (not valid_path_prefix(path)) return false;
    if (not is_haskell_conid(path.back()) and not is_haskell_consym(path.back())) return false;
    return true;
}

bool is_haskell_con_name(const std::string& s)
{
    return (is_haskell_builtin_con_name(s) or is_haskell_normal_con_name(s));
}

bool is_haskell_module_name(const std::string& s)
{
    return is_haskell_normal_con_name(s);
}

bool is_qualified_symbol(const string& s)
{
    return (get_haskell_identifier_path(s).size() >= 2);
}

string get_module_name(const std::string& s)
{
    vector<string> path = get_haskell_identifier_path(s);
    path.pop_back();

    if (not path.size())
        return "";
    else
        return join(path,'.');
}

string get_unqualified_name(const std::string& s)
{
    return get_haskell_identifier_path(s).back();
}

// Here we do only phase 1 -- we only parse the decls enough to
//   determine which is a variable, and which are e.g. constructors.
// We can't determine function bodies at all, since we can't even handle op definitions
//   before we know fixities!

void Module::def_function(const std::string& fname)
{
    if (is_qualified_symbol(fname))
        throw myexception()<<"Locally defined symbol '"<<fname<<"' should not be qualified in function declaration.";

    declare_symbol({fname, variable_symbol, {}, -1, {unknown_fix, -1}});
}

void Module::def_constructor(const string& cname, int arity, const string& type_name)
{
    if (is_qualified_symbol(cname))
        throw myexception()<<"Locally defined symbol '"<<cname<<"' should not be qualified.";

//    if (not is_qualified_symbol(type_name))
//        throw myexception()<<"Locally defined symbol '"<<type_name<<"' should not be qualified.";

    declare_symbol( {cname, constructor_symbol, type_name, arity, {unknown_fix, -1}} );
}

void Module::def_ADT(const std::string& tname)
{
    if (is_qualified_symbol(tname))
        throw myexception()<<"Locally defined type '"<<tname<<"' should not be qualified.";

    declare_type( {tname, type_name_category::ADT, {}} );
}

void Module::def_ADT(const std::string& tname, const fixity_info& fixity)
{
    if (is_qualified_symbol(tname))
        throw myexception()<<"Locally defined symbol '"<<tname<<"' should not be qualified.";

    declare_type( {tname, type_name_category::ADT, fixity} );
}

void Module::def_type_synonym(const std::string& sname)
{
    if (is_qualified_symbol(sname))
        throw myexception()<<"Locally defined type '"<<sname<<"' should not be qualified.";

    declare_type( {sname, type_name_category::type_syn, {}} );
}

void Module::def_type_class(const std::string& cname)
{
    if (is_qualified_symbol(cname))
        throw myexception()<<"Locally defined type '"<<cname<<"' should not be qualified.";

    declare_type( {cname, type_name_category::type_class, {}} );
}

void Module::def_type_class_method(const string& method_name, const string& class_name)
{
    if (is_qualified_symbol(method_name))
        throw myexception()<<"Locally defined type class method '"<<method_name<<"' should not be qualified.";

//    if (not is_qualified_symbol(class_name))
//        throw myexception()<<"Locally defined type class '"<<class_name<<"' should be qualified.";

    declare_symbol( {method_name, class_method_symbol, class_name, {}, {unknown_fix, -1}} );
}

void Module::declare_fixities(const Haskell::Decls& decls)
{
    // 0. Get names that are being declared.
    for(const auto& decl: decls)
        if (decl.is_a<Haskell::FixityDecl>())
        {
            auto FD = decl.as_<Haskell::FixityDecl>();

            // Determine precedence.
            int precedence = (FD.precedence)?*FD.precedence:9;

            // Find op names and declare fixity and precedence.
            for(const auto& name: FD.names)
                declare_fixity(name, precedence, FD.fixity);
        }
}

void Module::declare_fixities()
{
    if (not module.topdecls) return;

    // 0. Get names that are being declared.
    declare_fixities(*module.topdecls);

    for(const auto& topdecl: *module.topdecls)
        if (topdecl.is_a<Haskell::ClassDecl>())
        {
            auto& C = topdecl.as_<Haskell::ClassDecl>();
            if (C.decls)
                declare_fixities(unloc(*C.decls));
        }
}

void Module::add_local_symbols()
{
    if (not module.topdecls) return;

    // 0. Get names that are being declared.
    for(const auto& decl: *module.topdecls)
        if (decl.is_a<Haskell::ValueDecl>())
        {
            auto& D = decl.as_<Haskell::ValueDecl>();
            set<string> vars;
            if (is_function_binding(D))
                vars.insert( get_func_name(D) );
            else
            {
                vars = find_bound_vars(D.lhs);
            }

            for(const auto& var_name: vars)
            {
                // We don't know the type yet, probably, because we don't know the body.
                string qualified_name = name+"."+var_name;
                auto loc = symbols.find(qualified_name);

                if (loc != symbols.end())
                {
                    symbol_info& S = loc->second;
                    // Only the fixity has been declared!
                    if (S.symbol_type == unknown_symbol)
                        S.symbol_type = variable_symbol;
                }
                else
                    def_function(var_name);
            }
        }
        else if (decl.is_a<Haskell::BuiltinDecl>())
        {
            string bname = decl.as_<Haskell::BuiltinDecl>().function_name;
            def_function(bname);
        }
        else if (decl.is_a<Haskell::DataOrNewtypeDecl>())
        {
            auto& ADT = decl.as_<Haskell::DataOrNewtypeDecl>();
            def_ADT(ADT.name);

            auto constrs = decl.as_<Haskell::DataOrNewtypeDecl>().constructors;
            if (not constrs.size()) continue;

            for(const auto& constr: constrs)
                def_constructor(constr.name, constr.arity(), ADT.name);
        }
        else if (decl.is_a<Haskell::ClassDecl>())
        {
            auto& Class = decl.as_<Haskell::ClassDecl>();

            def_type_class(Class.name);

            if (Class.decls)
            {
                for(auto& decl: unloc(*Class.decls))
                {
                    if (decl.is_a<Haskell::SignatureDecl>())
                    {
                        auto& T = decl.as_<Haskell::SignatureDecl>();
                        for(auto& var: T.vars)
                        {
                            def_type_class_method(unloc(var.name), Class.name);
                        }
                    }
                }
            }
        }
        else if (decl.is_a<Haskell::TypeSynonymDecl>())
        {
            auto& Syn = decl.as_<Haskell::TypeSynonymDecl>();

            def_type_synonym(Syn.name);
        }
}

// A name of "" means that we are defining a top-level program, or a piece of a top-level program.
Module::Module(const string& n)
    :name(n)
{
    if (not n.size())
        throw myexception()<<"Module name may not be empty!";
}

Module::Module(const char *n)
    :Module(string(n))
{ }

Module::Module(const Haskell::Module& M, const set<string>& lo)
    :language_options(lo),
     module(M),
     name(module.modid)
{
}

std::ostream& operator<<(std::ostream& o, const Module& M)
{
    if (M.module.topdecls)
        for(const auto& decl: *M.module.topdecls)
        {
            auto& D = decl.as_<Haskell::ValueDecl>();
            o<<D.print()<<"\n";
        }
    return o;
}

symbol_info lookup_symbol(const string& name, const Program& P)
{
    if (is_haskell_builtin_con_name(name))
        return Module::lookup_builtin_symbol(name).first;

    assert(is_qualified_symbol(name));
    string name1 = get_module_name(name);
    string name2 = get_unqualified_name(name);

    for(const auto& module: P)
        if (module.name == name1)
            return module.lookup_symbol(name);

    std::abort();
}
