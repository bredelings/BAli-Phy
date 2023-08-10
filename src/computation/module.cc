#include <set>
#include <regex>
#include <tuple>
#include "computation/module.H"
#include "util/myexception.H"
#include "util/variant.H"
#include "util/log-level.H"
#include "range/v3/all.hpp"
#include "util/range.H"
#include "util/set.H"   // for add( , )
#include "util/string/split.H"
#include "util/string/join.H"
#include "program.H"
#include "operations.H"
#include "computation/machine/graph_register.H" // for graph_normalize( )
#include "computation/operations.H"
#include "rename/rename.H"
#include "desugar/desugar.H"
#include "typecheck/typecheck.H"
#include "computation/loader.H"
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
#include "computation/haskell/haskell.H"
#include "computation/haskell/ids.H"
#include "computation/varinfo.H"

namespace views = ranges::views;

using std::pair;
using std::map;
using std::set;
using std::optional;
using std::multiset;
using std::string;
using std::vector;
using std::tuple;
using std::shared_ptr;

std::string Module::qualify_local_name(const std::string& n) const
{
    assert(not is_qualified_symbol(n));
    assert(not is_haskell_builtin_con_name(n));
    assert(not is_haskell_builtin_type_name(n)); // This includes e.g. Double, Char, etc.  Is this right?

    return name + "." + n;
}

bool Module::is_local_qualified_name(const std::string& n) const
{
    return is_local_symbol(n, name);
}

type_ptr Module::add_type(const type_info& T)
{
    if (is_haskell_builtin_type_name(T.name))
        throw myexception()<<"Can't add builtin symbol '"<<T.name<<"'";

    if (not is_qualified_symbol(T.name))
        throw myexception()<<"Type '"<<T.name<<"' unqualified, can't be added to symbol table";

    auto loc = types.find(T.name);
    if (loc == types.end())
    {
        auto ptr = std::make_shared<type_info>(T);
        types.insert({T.name, ptr});
        return ptr;
    }
    else if (loc != types.end() and loc->second->name != T.name)
        throw myexception()<<"Trying to add type '"<<T.name<<"' twice to module '"<<name<<"' with different body";
    else
        return loc->second;
}

symbol_ptr Module::add_symbol(const symbol_info& S)
{
    if (is_haskell_builtin_con_name(S.name))
        throw myexception()<<"Can't add builtin symbol '"<<S.name<<"'";

    if (not is_qualified_symbol(S.name))
        throw myexception()<<"Symbol '"<<S.name<<"' unqualified, can't be added to symbol table";

    auto loc = symbols.find(S.name);
    if (loc == symbols.end())
    {
        auto ptr = std::make_shared<symbol_info>(S);

        symbols.insert({S.name, ptr});
        return ptr;
    }
    else if (loc != symbols.end() and loc->second->name != S.name)
        throw myexception()<<"Trying to add symbol '"<<S.name<<"' twice to module '"<<name<<"' with different body";
    else
        return loc->second;
}

void Module::add_alias(const string& identifier_name, const const_symbol_ptr& resolved_symbol)
{
    if (not lookup_resolved_symbol(resolved_symbol->name))
        throw myexception()<<"Can't add alias '"<<identifier_name<<"' -> '"<<resolved_symbol->name<<"' in module '"<<name<<"' because '"<<resolved_symbol->name<<"' is neither declared nor imported.";

    // Don't add duplicate aliases.
    if (symbol_in_scope_with_name(resolved_symbol->name, identifier_name)) return;

    aliases.insert( {identifier_name, resolved_symbol} );
}

void Module::add_type_alias(const string& identifier_name, const const_type_ptr& resolved_type)
{
    if (not lookup_resolved_type(resolved_type->name))
        throw myexception()<<"Can't add alias '"<<identifier_name<<"' -> '"<<resolved_type->name<<"' in module '"<<name<<"' because '"<<resolved_type->name<<"' is neither declared nor imported.";

    // Don't add duplicate aliases.
    if (type_in_scope_with_name(resolved_type->name, identifier_name)) return;

    type_aliases.insert( {identifier_name, resolved_type} );
}

void Module::declare_symbol(const symbol_info& S)
{
    if (is_qualified_symbol(S.name))
        throw myexception()<<"Locally defined symbol '"<<S.name<<"' should not be qualified in declaration.";

    symbol_info S2 = S;
    S2.name = qualify_local_name(S.name);

    if (symbols.count(S2.name))
    {
        // FIXME! We created a partially-empty symbol to hold the fixity information.
        auto& S3 = symbols.at(S2.name);

        if (S3->symbol_type == unknown_symbol)
        {
            S2.fixity = S3->fixity;
            *S3 = S2;
            return;
        }
        else
            throw myexception()<<"Trying to declare '"<<S.name<<"' twice in module '"<<name<<"'";
    }

    // Add the symbol first.
    auto S2p = add_symbol(S2);
    // Add the alias for qualified name: S.name -> S2.name;
    add_alias(S2.name, S2p);
    // Add the alias for unqualified name: S.name -> S2.name;
    add_alias(S.name, S2p);
}

void Module::declare_type(const type_info& T)
{
    if (is_haskell_builtin_type_name(T.name))
        throw myexception()<<"Can't declare builtin type name '"<<T.name<<"' as local type!.";

    if (is_qualified_symbol(T.name))
        throw myexception()<<"Locally defined symbol '"<<T.name<<"' should not be qualified in declaration.";

    auto T2 = T;
    T2.name = qualify_local_name(T.name);

    if (types.count(T2.name))
        throw myexception()<<"Trying to declare '"<<T.name<<"' twice in module '"<<name<<"'";

    // Add the symbol first.
    auto T2p = add_type(T2);
    // Add the alias for qualified name: S.name -> S2.name;
    add_type_alias(T2.name, T2p);
    // Add the alias for unqualified name: S.name -> S2.name;
    add_type_alias(T.name, T2p);
}

// "Also like a type signature, a fixity declaration can only occur in the same sequence of declarations as the declaration of the operator itself, and at most one fixity declaration may be given for any operator."

// "Fixity is a property of a particular entity (constructor or variable), just like its type; fixity is not a property of that entity’s name."
void Module::declare_fixity(const std::string& s, int precedence, fixity_t fixity)
{
    if (is_qualified_symbol(s))
        throw myexception()<<"Trying to declare fixity of qualified symbol '"<<s<<"'.  Use its unqualified name.";

    if (precedence < 0 or precedence > 9)
        throw myexception()<<"Precedence level "<<precedence<<" not allowed.";

    string s2 = qualify_local_name(s);

    if (not symbols.count(s2))
        declare_symbol({s, unknown_symbol, {}, {}, {}});

    symbols.at(s2)->fixity = fixity_info{fixity, precedence};
}

// Question: what if we import m1.s, which depends on an unimported m2.s?
void Module::import_symbol(const const_symbol_ptr& S, const string& modid, bool qualified)
{
    if (not is_qualified_symbol(S->name))
        throw myexception()<<"Error importing symbol '"<<S->name<<"': name not qualified";

    // Add the alias for qualified name.
    add_alias(modid+"."+get_unqualified_name(S->name), S);
    // Add the alias for unqualified name.
    if (not qualified)
        add_alias(get_unqualified_name(S->name), S);
}

// Question: what if we import m1.s, which depends on an unimported m2.s?
void Module::import_type(const const_type_ptr& T, const string& modid, bool qualified)
{
    if (not is_qualified_symbol(T->name))
        throw myexception()<<"Error importing type '"<<T->name<<"': name not qualified";

    // Add the alias for qualified name.
    add_type_alias(modid+"."+get_unqualified_name(T->name), T);
    // Add the alias for unqualified name.
    if (not qualified)
        add_type_alias(get_unqualified_name(T->name), T);
}

bool contains_import(const Haskell::Export& e, const string& name)
{
    if (e.is_module())
        throw myexception()<<"found "<<e.print()<<" in import list!";

    return unloc(e.symbol) == name;
}

bool contains_import(const vector<Haskell::LExport>& es, const string& name)
{
    for(auto& [_,e]: es)
        if (contains_import(e, name))
            return true;
    return false;
}

void Module::import_module(const Program& P, const Hs::LImpDecl& limpdecl)
{
    auto& [imploc, impdecl] = limpdecl;

    auto M2 = P.get_module( unloc(impdecl.modid) );

    transitively_imported_modules.insert({M2->name, M2});
    for(auto& [mod_name, mod]: M2->transitively_imported_modules)
        transitively_imported_modules.insert({mod_name, mod});

    auto modid = unloc(impdecl.modid);
    if (impdecl.as)
        modid = unloc(*impdecl.as);
    bool qualified = impdecl.qualified;
    assert(modid != name);

    auto& m2_exports = M2->exported_symbols();
    auto& m2_exported_types = M2->exported_types();

    // import modid
    if (not impdecl.impspec)
    {
        for(const auto& [_,S]: m2_exports)
            import_symbol(S, modid, qualified);

        for(const auto& [_,T]: m2_exported_types)
            import_type(T, modid, qualified);
    }
    // import modid ( etc )
    else if (impdecl.impspec and not impdecl.impspec->hiding)
    {
        for(const auto& [loc,s]: impdecl.impspec->imports)
        {
            string id = unloc(s.symbol);

            if (s.is_module())
            {
                messages.push_back( error(loc, Note()<<"found "<<s.print()<<" in import list!"));
                continue;
            }
            else if (s.is_value())
            {
                if (not m2_exports.count(id))
                {
                    messages.push_back( error(loc, Note()<<"variable `"<<id<<"` not exported.") );
                    continue;
                }

                if (s.subspec)
                    messages.push_back( error(loc, Note()<<"variable `"<<id<<"` can't have export subspec") );

                auto variable = m2_exports.at(id);
                import_symbol( variable, modid, qualified );
            }
            else if (s.is_type())
            {
                if (not m2_exported_types.count(id))
                {
                    messages.push_back( error(loc, Note()<<"type `"<<id<<"` is not exported.") );
                    continue;
                }

                auto type = m2_exported_types.at(id);
                import_type( type, modid, qualified );
                if (s.subspec)
                {
                    if (type->is_type_fam())
                    {
                        messages.push_back( error(loc, Note()<<"type family `"<<id<<"`can't have import subspec") );
                        continue;
                    }
                    else if (type->is_type_syn())
                    {
                        messages.push_back( error(loc, Note()<<"type synonym `"<<id<<"` can't have import subspec") );
                        continue;
                    }
                    else if (auto c = type->is_class())
                    {
                        if (not s.subspec->names)
                        {
                            for(auto& method: c->methods)
                                import_symbol( m2_exports.at(method), modid, qualified );
                        }
                        else
                        {
                            for(auto& [loc,name]: *s.subspec->names)
                            {
                                if (not c->methods.count(name))
                                    messages.push_back( error(loc, Note()<<"`"<<name<<"` is not a method for class `"<<id<<"`") );
                                else
                                    import_symbol( m2_exports.at(name), modid, qualified );
                            }
                        }
                    }
                    else if (auto d = type->is_data())
                    {
                        if (not s.subspec->names)
                        {
                            for(auto& constructor: d->constructors)
			    {
				auto name = get_unqualified_name(constructor);
				if (m2_exports.contains(name))
				    import_symbol(m2_exports.at( name ), modid, qualified);
			    }
                            for(auto& field: d->fields)
			    {
				auto name = get_unqualified_name(field);
				if (m2_exports.contains(name))
				    import_symbol(m2_exports.at( name ), modid, qualified);
			    }
                        }
                        else
                        {
                            auto type_modid = get_module_name(type->name);
                            for(auto& [loc,name]: *s.subspec->names)
                            {
                                if (is_haskell_conid(name) and not d->constructors.count(type_modid + "." + name))
                                {
                                    messages.push_back( error(loc, Note()<<"`"<<name<<"` is not a constructor for data type `"<<id<<"`"));
                                    continue;
                                }
                                if (is_haskell_varid(name) and not d->fields.count(type_modid + "." + name))
                                {
                                    messages.push_back( error(loc, Note()<<"`"<<name<<"` is not a field for data type `"<<id<<"`"));
                                    continue;
                                }

				if (not m2_exports.contains(name))
				{
                                    messages.push_back( error(loc, Note()<<"`"<<name<<"` was not exported"));
                                    continue;
				}
				    
                                import_symbol(m2_exports.at(name), modid, qualified);
                            }
                        }
                    }
                }
            }
        }
    }
    // import modid hiding ( etc )
    else if (impdecl.impspec and impdecl.impspec->hiding)
    {
        for(const auto& [_,S]: m2_exports)
        {
            auto unqualified_name = get_unqualified_name(S->name);

            if (contains_import(impdecl.impspec->imports, unqualified_name)) continue;

            import_symbol(S, modid, qualified);
        }
        for(const auto& [_,T]: m2_exported_types)
        {
            auto unqualified_name = get_unqualified_name(T->name);

            if (contains_import(impdecl.impspec->imports, unqualified_name)) continue;

            import_type(T, modid, qualified);
        }
    }
}

vector<Hs::LImpDecl> Module::imports() const
{
    // 1. Copy the imports list
    auto imports_list = module.impdecls;

    // 2. Check if we've seen the Prelude
    if (language_extensions.has_extension(LangExt::ImplicitPrelude) and name != "Prelude")
    {
        bool seen_Prelude = false;
        for(auto& [loc, impdecl]: imports_list)
            if (unloc(impdecl.modid) == "Prelude")
                seen_Prelude = true;

        if (not seen_Prelude)
        {
            Hs::ImpDecl impdecl{false, {noloc,"Prelude"}, {}, {}};
            imports_list.emplace_back(noloc,impdecl);
        }
    }

    return imports_list;
}

set<string> Module::dependencies() const
{
    set<string> modules;
    for(auto& [loc,impdecl]: imports() )
        modules.insert( unloc(impdecl.modid) );
    return modules;
}

/* 
infixr 6 >>

x >> y : z = x

main = putStrLn (show ("a" Main.>> 'b':[]))

-- This compiles, which proves that infix expressions are NOT handled exactly the same way
-- in declaration LHSs as they are in expressions.
-- Or, at least, in the definition, we look up and rename >> to Main.>> before we do the infix handling...
*/

void Module::compile(const Program& P)
{
    assert(not resolved);
    resolved = true;

    auto& loader = *P.get_module_loader();
    simplifier_options& opts = loader;

    if (opts.dump_parsed or opts.dump_renamed or opts.dump_desugared or opts.dump_typechecked or log_verbose)
        std::cerr<<"[ Compiling "<<name<<" ]\n";

    // Scans imported modules and modifies symbol table and type table
    perform_imports(P);

    Hs::ModuleDecls M;
    if (module.topdecls)
        M = Hs::ModuleDecls(*module.topdecls);

    // We should create a "local fixity environment" mapping from var and Module.var -> fixity info.
    // This can supplement the symbols that we imported from other modules.
    // Then we can AUGMENT this local fixity environment when we encounter fixities at lower levels.
    // Currently rename_infix has to handle UNresolved top-level names, because we do it BEFORE renaming.
    declare_fixities(M);

    if (language_extensions.has_extension(LangExt::FieldSelectors))
    {
        auto field_accessors = synthesize_field_accessors(M.type_decls);
        M.value_decls[0].insert(M.value_decls[0].end(), field_accessors.begin(), field_accessors.end());
    }

    // FIXME - merge with rename() below.
    // Currently this (1) translates field-decls into function declarations
    //                (2) rewrites @ f x y -> f x y (where f is the head) using unapply( ).
    //                (3) rewrites infix expressions through desugar_infix( )
    //                (4) merges adjacent function declaration lines into a Match.
    M = ::rename_infix(*this, M);

    // We should be able to build these as we go, in rename!
    // We can merge them into a global symbol table (if we want) afterwards.

    // calls def_function, def_ADT, def_constructor, def_type_class, def_type_synonym, def_type_family
    add_local_symbols(M.type_decls);

    add_local_symbols(M.value_decls[0]);

    for(auto& f: M.foreign_decls)
        def_function( unloc(f.function).name );

    // Currently we do "renaming" here.
    // That just means (1) qualifying top-level declarations and (2) desugaring rec statements.
    M = rename(opts, M);

    auto tc_result = typecheck(P.fresh_var_state(), M);

    auto [hs_decls, core_decls] = tc_result.all_binds();

    if (opts.dump_typechecked)
    {
        std::cerr<<"\nType-checked:\n";
        for(auto& [name, sym]: symbols)
        {
            std::cerr<<name<<" :: "<<sym->type.print()<<"\n";
        }
        for(auto& decl: hs_decls)
        {
            std::cerr<<decl.print()<<"\n";
        }
        std::cerr<<print_cdecls(core_decls)<<"\n";
    }

    // Updates exported_symols_ + exported_types_
    perform_exports();

    // look only in value_decls now
    // FIXME: how to handle functions defined in instances and classes?
    value_decls = desugar(opts, P.fresh_var_state(), hs_decls);
    value_decls += core_decls;

    value_decls = load_builtins(loader, M.foreign_decls, value_decls);

    value_decls = load_constructors(M.type_decls, value_decls);

    if (opts.dump_desugared)
    {
        std::cerr<<"\nCore:\n";
        for(auto& [x,rhs] : value_decls)
            std::cerr<<x.print()<<" = "<<rhs.print()<<"\n";
        std::cerr<<"\n\n";
    }

    // Check for duplicate top-level names.
    check_duplicate_var(value_decls);

    value_decls = optimize(opts, P.fresh_var_state(), value_decls);

    // this records unfoldings.
    export_small_decls(value_decls);
}

void Module::perform_imports(const Program& P)
{
    for(auto& impdecl: imports() )
        import_module(P, impdecl);

    show_messages(file, std::cerr, messages);
    exit_on_error(messages);
}

void Module::export_symbol(const const_symbol_ptr& S)
{
    assert(is_qualified_symbol(S->name));

    if (S->symbol_type == default_method_symbol)
    {
        // normal exported symbols are access by unqualified name.
        exported_symbols_.insert({S->name, S});
        return;
    }

    auto uname = get_unqualified_name(S->name);
    if (not exported_symbols_.count(uname))
        exported_symbols_.insert({uname,S});
    // FIXME, this doesn't really say how the entities (which are different) were referenced in the export list
    else if (exported_symbols_.at(uname)->name != S->name)
        throw myexception()<<"attempting to export both '"<<exported_symbols_.at(uname)->name<<"' and '"<<S->name<<"', which have the same unqualified name!";
}

void Module::export_type(const const_type_ptr& T)
{
    assert(is_qualified_symbol(T->name));

    auto uname = get_unqualified_name(T->name);
    if (not exported_types_.count(uname))
        exported_types_.insert({uname,T});
    // FIXME, this doesn't really say how the entities (which are different) were referenced in the export list
    else if (exported_types_.at(uname)->name != T->name)
        throw myexception()<<"attempting to export both '"<<exported_types_.at(uname)->name<<"' and '"<<T->name<<"', which have the same unqualified name!";

    // Export default methods if its a class
    if (auto c = T->is_class())
    {
        for(auto& [_,dmvar]: c->info->default_methods)
            export_symbol(lookup_resolved_symbol( dmvar.name ) );
    }
}

bool Module::symbol_in_scope_with_name(const string& symbol_name, const string& id_name) const
{
    auto range = aliases.equal_range(id_name);
    for(auto it = range.first; it != range.second; it++)
        if (it->second->name == symbol_name)
            return true;
    return false;
}

bool Module::type_in_scope_with_name(const string& type_name, const string& id_name) const
{
    auto range = type_aliases.equal_range(id_name);
    for(auto it = range.first; it != range.second; it++)
        if (it->second->name == type_name)
            return true;
    return false;
}

optional<string> symbol_in_module(const string& s, const string& modid)
{
    int M = modid.size();

    if (s.size() < M+2) return {};

    if (s[M] != '.') return {};

    for(int i=0;i<M;i++)
        if (s[i] != modid[i]) return {};

    return s.substr(M+1);
}


// From the Haskell 2010 report:
//    The form “module M” names the set of all entities that are in scope with both an unqualified name
//    “e” and a qualified name “M.e”. This set may be empty.
void Module::export_module(const string& modid)
{
    if (modid != name and not dependencies().count(modid))
        throw myexception()<<"module '"<<modid<<"' is exported but not imported";

    // Find all the symbols that are in scope as both `modid.e` and `e`
    for(auto& [id_name, symbol]: aliases)
        if (auto uqname = symbol_in_module(id_name, modid);
            uqname and symbol_in_scope_with_name(symbol->name, *uqname))
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
    for(auto& [id_name, type]: type_aliases)
        if (auto uqname = symbol_in_module(id_name, modid);
            uqname and type_in_scope_with_name(type->name, *uqname))
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
    for(auto& [name,value_info]:symbols)
    {
        assert(value_info->symbol_type == constructor_symbol or not value_info->type.empty());
    }

    // Currently we just export the local symbols
    if (not module.exports or module.exports->size() == 0)
        export_module(name);
    else
    {
        for(auto& [loc,ex]: *module.exports)
        {
            string id = unloc(ex.symbol);
            if (ex.is_module())
            {
                export_module(id);
            }
            else if (ex.is_value())
            {
                if (not aliases.count(id))
                {
                    messages.push_back( error(loc, Note()<<"trying to export variable '"<<id<<"', which is not in scope.") );
                    continue;
                }

                if (ex.subspec)
                    messages.push_back( error(loc, Note()<<"variable `"<<ex.print()<<"` can't have export subspec") );

                export_symbol(lookup_symbol(id));
            }
            else if (ex.is_type())
            {
                if (not type_aliases.count(id))
                {
                    messages.push_back( error(loc, Note()<<"trying to export type '"<<id<<"', which is not in scope.") );
                    continue;
                }

                auto t = lookup_type(id);
                export_type(t);
                if (ex.subspec)
                {
                    if (t->is_type_fam())
                    {
                        messages.push_back( error(loc, Note()<<"type family `"<<id<<"`can't have export subspec") );
                        continue;
                    }
                    else if (t->is_type_syn())
                    {
                        messages.push_back( error(loc, Note()<<"type synonym `"<<id<<"` can't have export subspec") );
                        continue;
                    }

                    if (not ex.subspec->names)
                    {
                        // all children
                        if (auto c = t->is_class())
                        {
                            for(auto& method: c->methods)
                                export_symbol(lookup_symbol(method));
                        }
                        else if (auto d = t->is_data())
                        {
                            for(auto& constructor: d->constructors)
                                export_symbol(lookup_symbol(constructor));
                            for(auto& field: d->fields)
                                export_symbol(lookup_symbol(field));
                        }
                    }
                    else
                    {
                        // all children
                        if (auto c = t->is_class())
                        {
                            for(auto& [loc,name]: *ex.subspec->names)
                            {
                                if (not c->methods.count(name))
                                    messages.push_back( error(loc, Note()<<"`"<<name<<"` is not a method for class `"<<id<<"`") );
                                else
                                    export_symbol(lookup_symbol(name));
                            }
                        }
                        else if (auto d = t->is_data())
                        {
                            for(auto& [loc,name]: *ex.subspec->names)
                            {
                                auto qualified_name = get_module_name(t->name) + "." + name;
                                if (is_haskell_conid(name) and not d->constructors.count( qualified_name ))
                                {
                                    messages.push_back( error(loc, Note()<<"`"<<name<<"` is not a constructor for data type `"<<id<<"`"));
                                    continue;
                                }
                                if (is_haskell_varid(name) and not d->fields.count( qualified_name) )
                                {
                                    messages.push_back( error(loc, Note()<<"`"<<name<<"` is not a field for data type `"<<id<<"`") );
                                    continue;
                                }

                                export_symbol(lookup_symbol(name));
                            }
                        }
                    }
                }
            }
        }
    }
    show_messages(file, std::cerr, messages);
    exit_on_error(messages);
}

void Module::clear_symbol_table()
{
    symbols.clear();
    aliases.clear();
    types.clear();
    type_aliases.clear();
    exported_symbols_.clear();
    exported_types_.clear();

    transitively_imported_modules.clear();
}


map<var,expression_ref> Module::code_defs() const
{
    map<var, expression_ref> code;

    for(const auto& [x,rhs]: value_decls)
    {
        assert(is_qualified_symbol(x.name));

        if (this->name == get_module_name(x.name))
        {
            // get the body for the  decl
            assert(rhs);

            code[x] = rhs;
        }
    }

    return code;
}

Hs::ModuleDecls Module::rename(const simplifier_options& opts, Hs::ModuleDecls M)
{
    M = ::rename(opts, *this, M);

    if (opts.dump_renamed)
    {
        std::cout<<name<<"[renamed]:\n";
        std::cout<<M.type_decls.print()<<"\n";
        std::cout<<M.value_decls.print()<<"\n";
        std::cout<<"\n";
    }

    return M;
}

CDecls Module::desugar(const simplifier_options& /*opts*/, FreshVarState& state, const Hs::Binds& topdecls)
{
    auto cdecls = ::desugar(*this, state, topdecls);

//    if (opts.dump_desugared)
//        std::cout<<name<<"[desugared]:\n"<<print_cdecls(cdecls)<<"\n\n";

    return cdecls;
}

symbol_ptr Module::lookup_make_local_symbol(const std::string& var_name)
{
    auto S = lookup_local_symbol(var_name);

    // If there isn't a symbol, then make one.
    if (not S)
    {
        add_symbol({var_name, variable_symbol, {}, {}, {}});
        S = lookup_local_symbol(var_name);
        assert(S);
        S->visible = false;
    }

    assert(S);
    assert(S->var_info);

    return S;
}


void Module::export_small_decls(const CDecls& cdecls)
{
    // FIXME: add a wrapper for EVERY constructor!

    for(auto& [x,rhs]: cdecls)
    {
        assert(not x.name.empty());
        assert(get_module_name(x.name) == name);

        if (simple_size(rhs) <= 5)
        {
            // Add the unfolding for this variable.
            auto S = lookup_make_local_symbol(x.name);

            // Label vars with whether they are used or not, and collect free vars.
            auto [E, free_vars] = occurrence_analyzer(*this, rhs);

            // The unfolding need to be occurrence analyzed.
            S->var_info->unfolding = E;

            // Check that we have local symbols for everything that we've put in an unfolding.
            for(auto& y: free_vars)
                if (is_qualified_symbol(y.name) and get_module_name(y.name) == name)
                    lookup_make_local_symbol(y.name);
        }
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
    if (auto C = parse_case_expression(E))
    {
        auto& [object, alts] = *C;
        // Analyze the object
        object = rename(object, substitution, bound);
        for(auto& [pattern, body]: alts)
        {
            for(int j=0;j<pattern.size(); j++)
            {
                auto& x = pattern.sub()[j].as_<var>();
                if (not x.is_wildcard())
                    bound.insert(x);
            }

            body = rename(body, substitution, bound);

            for(int j=0;j<pattern.size(); j++)
            {
                auto& x = pattern.sub()[j].as_<var>();
                if (not x.is_wildcard())
                    erase_one(bound,x);
            }
        }
        return make_case_expression(object, alts);
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

void mark_exported_decls(CDecls& decls,
                         const InstanceEnv& instance_env,
                         const map<string,const_symbol_ptr>& exports,
                         const map<string,type_ptr>& type_exports,
                         const string& module_name)
{
    // Record exports
    set<string> exported;
    for(auto& [name,symbol]: exports)
        if (get_module_name(symbol->name) == module_name)
            exported.insert(symbol->name);

    // Instances are exported
    for(auto& [dvar, _]: instance_env)
        exported.insert(dvar.name);

    for(auto& [tname,tinfo]: type_exports)
    {
        if (auto c = tinfo->is_class())
        {
            for(auto& [dvar, _]: c->info->superclass_extractors)
                exported.insert(dvar.name);

            // Default methods are exported
            for(auto& [method, dm]: c->info->default_methods)
                exported.insert(dm.name);
        }
    }

    // Mark exported vars as exported
    for(auto& [x,_]: decls)
    {
        if (exported.count(x.name))
        {
            x.is_exported = true;
            exported.erase(x.name);
        }
        else
            x.is_exported = false;
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

CDecls Module::optimize(const simplifier_options& opts, FreshVarState& fvstate, CDecls cdecls)
{
    // 1. why do we keep on re-optimizing the same module?
    if (optimized) return cdecls;
    optimized = true;

    // 2. Graph-normalize the bodies
    for(auto& [x,rhs]: cdecls)
    {
        // This won't float things to the top level!
        rhs = graph_normalize( fvstate, rhs);
    }

    if (opts.optimize)
    {
        mark_exported_decls(cdecls, local_instances, exported_symbols(), types, name);

        vector<CDecls> decl_groups = {cdecls};

        decl_groups = simplify_module_gently(opts, fvstate, *this, decl_groups);

        if (opts.fully_lazy)
            float_out_from_module(fvstate, decl_groups);

        decl_groups = simplify_module(opts, fvstate, *this, decl_groups);

        if (opts.fully_lazy)
            float_out_from_module(fvstate, decl_groups);

        cdecls = flatten(decl_groups);
    }

    return rename_top_level(cdecls, name);
}

expression_ref parse_builtin(const Haskell::ForeignDecl& B, int n_args, const module_loader& L)
{
    const string builtin_prefix = "builtin_function_";

    string operation_name = B.plugin_name+":"+B.symbol_name;

    return load_builtin(L, builtin_prefix + B.symbol_name, B.plugin_name, n_args, operation_name);
}

CDecls Module::load_builtins(const module_loader& L, const std::vector<Hs::ForeignDecl>& foreign_decls, CDecls cdecls)
{
    for(const auto& decl: foreign_decls)
    {
        auto function_name = unloc(decl.function).name;

        auto S = lookup_symbol(function_name);

        function_name = S->name;

        int n_args = gen_type_arity(S->type);

        auto [_,result_type] = gen_arg_result_types(S->type);

        expression_ref body;

        if (is_IO_type(result_type))
        {
            // Change IO a to RealWorld -> a
            body = parse_builtin(decl, n_args+1, L);

            for(int i=0;i<n_args;i++)
                body = {body,var(i)};

            body = {var("Compiler.IO.makeIO"), body};

            for(int i=n_args-1;i>=0;i--)
                body = lambda_quantify(i,body);
            // (\x0 x1 ... x<n-1> -> makeIO ((\x0 x1 .. x<n> -> builtinOp x0 x1 .. x<n>) x0 x1 .. x<n-1>))
        }
        else
            body = parse_builtin(decl, n_args, L);

        cdecls.push_back( { var(function_name), body} );
    }

    return cdecls;
}

string get_constructor_name(const Hs::LType& constr)
{
    auto [con,_] = Hs::decompose_type_apps(constr);
    auto tc = unloc(con).to<Hs::TypeCon>();
    assert(tc);
    return tc->name;
}

CDecls Module::load_constructors(const Hs::Decls& topdecls, CDecls cdecls)
{
    for(const auto& [_,decl]: topdecls)
    {
        auto d = decl.to<Haskell::DataOrNewtypeDecl>();
        if (not d) continue;

        if (d->is_regular_decl())
        {
            for(const auto& constr: d->get_constructors())
            {
                auto cname = unloc(*constr.con).name;
                auto info = lookup_resolved_symbol(cname)->con_info;
                assert(info);
                int arity = info->dict_arity() + info->arity();

                expression_ref body = lambda_expression( constructor(cname, arity) );
                cdecls.push_back( { var(cname) , body} );
            }
        }
        else if (d->is_gadt_decl())
        {
            for(const auto& cons_decl: d->get_gadt_constructors())
                for(auto& con_name: cons_decl.con_names)
                {
                    auto cname = unloc(con_name);
                    auto info = lookup_resolved_symbol(cname)->con_info;
                    assert(info);
                    int arity = info->dict_arity() + info->arity();

                    expression_ref body = lambda_expression( constructor(cname, arity) );
                    cdecls.push_back( { var(cname) , body} );
                }
        }
    }
    return cdecls;
}

bool Module::is_declared(const std::string& name) const
{
    return is_haskell_builtin_con_name(name) or (aliases.count(name) > 0);
}

bool Module::type_is_declared(const std::string& name) const
{
    return is_haskell_builtin_type_name(name) or (type_aliases.count(name) > 0);
}

const_symbol_ptr make_builtin_symbol(const std::string& name)
{
    symbol_ptr S;
    expression_ref U;
    if (name == "()")
    {
        S = std::make_shared<symbol_info>("()", constructor_symbol, "()", 0);
        U = constructor("()",0);
    }
    else if (name == "[]")
    {
        S = std::make_shared<symbol_info>("[]", constructor_symbol, "[]", 0);
        U = constructor("[]",0);
    }
    else if (name == ":")
    {
        symbol_info cons(":", constructor_symbol, "[]", 2, {{right_fix,5}});
        S = std::make_shared<symbol_info>(cons);
        U = lambda_expression( right_assoc_constructor(":",2) );
    }
    else if (is_tuple_name(name))
    {
        int arity = name.size() - 1;
        S = std::make_shared<symbol_info>(name, constructor_symbol, name, arity);
        U = lambda_expression( tuple_head(arity) );
    }
    else
        throw myexception()<<"Symbol 'name' is not a builtin (constructor) symbol.";

    Module empty("Empty");

    auto [E, free_vars] = occurrence_analyzer(empty, U);
    S->var_info->unfolding = E;
    assert(free_vars.empty());
    return S;
}

// Holding the symbols here permanently ensures that there is always a
// reference to the VarInfo for these symbols.
std::map<std::string, const_symbol_ptr> builtin_symbols_cache;

const_symbol_ptr Module::lookup_builtin_symbol(const std::string& name)
{
    auto iter = builtin_symbols_cache.find(name);
    if (iter == builtin_symbols_cache.end())
    {
        builtin_symbols_cache.insert({name, make_builtin_symbol(name)});
        iter = builtin_symbols_cache.find(name);
        assert(iter != builtin_symbols_cache.end());
    }

    return iter->second;
}

const_type_ptr Module::lookup_builtin_type(const std::string& name)
{
    if (name == "Char")
        return const_type_ptr(new type_info{"Char", type_info::data_info(), {}, 0, kind_type()});
    else if (name == "Double")
        return const_type_ptr(new type_info{"Double", type_info::data_info(), {}, 0, kind_type()});
    else if (name == "Int")
        return const_type_ptr(new type_info{"Int", type_info::data_info(), {}, 0, kind_type()});
    else if (name == "Integer")
        return const_type_ptr(new type_info{"Integer", type_info::data_info(), {}, 0, kind_type()});
    else if (name == "()")
    {
        return const_type_ptr(new type_info{"()", type_info::data_info{{"()"},{}}, {}, 0, kind_type()});
    }
    else if (name == "[]")
        return const_type_ptr(new type_info{"[]", type_info::data_info{{"[]",":"},{}}, {}, 1, make_n_args_kind(1)});
    else if (name == "->")
    {
        return const_type_ptr(new type_info{"->", {}, {{right_fix,0}}, 2, make_n_args_kind(2)});
    }
    else if (name == "~")
    {
        return const_type_ptr(new type_info{"~", {}, {}, 2, make_n_args_constraint_kind(2)});
    }
    // This doesn't include ()
    else if (is_tuple_name(name))
    {
        int n = tuple_arity(name);

        return const_type_ptr(new type_info{name, type_info::data_info{{name},{}},{}, n, make_n_args_kind(n)});
    }
    throw myexception()<<"Symbol 'name' is not a builtin (type) symbol.";
}

const_symbol_ptr Module::lookup_symbol(const std::string& name) const
{
    if (is_haskell_builtin_con_name(name))
        return lookup_builtin_symbol(name);

    int count = aliases.count(name);
    if (count == 0)
        throw myexception()<<"Indentifier '"<<name<<"' not declared.";
    else if (count == 1)
        return aliases.find(name)->second;
    else
    {
        myexception e;
        e<<"Identifier '"<<name<<"' is ambiguous!";
        auto range = aliases.equal_range(name);
        for(auto i = range.first; i != range.second ;i++)
            e<<"\n "<<i->first<<" -> "<<i->second->name;
        throw e;
    }
}

const_symbol_ptr Module::lookup_resolved_symbol(const std::string& symbol_name) const
{
    // 1. Handle builtin names
    if (is_haskell_builtin_con_name(symbol_name))
        return lookup_builtin_symbol(symbol_name);

    // 2. Handle local names
    else if (name == get_module_name(symbol_name))
        return lookup_local_symbol(symbol_name);

    // 3. Handle external names
    else
        return lookup_external_symbol(symbol_name);
}

const_symbol_ptr Module::lookup_local_symbol(const std::string& symbol_name) const
{
    assert( get_module_name(symbol_name) == name);
    auto iter = symbols.find(symbol_name);
    if (iter == symbols.end())
        return nullptr;
    else
        return iter->second;
}

symbol_ptr Module::lookup_local_symbol(const std::string& symbol_name)
{
    assert( get_module_name(symbol_name) == name);
    auto iter = symbols.find(symbol_name);
    if (iter == symbols.end())
        return nullptr;
    else
        return iter->second;
}

const_symbol_ptr Module::lookup_external_symbol(const std::string& symbol_name) const
{
    assert(is_qualified_symbol(symbol_name));

    auto mod_name = get_module_name(symbol_name);

    auto uname = get_unqualified_name(symbol_name);

    auto mod_iter = transitively_imported_modules.find(mod_name);
    if (mod_iter == transitively_imported_modules.end())
        return nullptr;
    else
        return mod_iter->second->lookup_local_symbol(symbol_name);
}

std::optional<DataConInfo> Module::constructor_info(const string& con_name) const
{
    if (con_name == ":")
    {
        DataConInfo info;
        TypeVar a({noloc, "a"}, kind_type());
        info.uni_tvs = { a };
        info.field_types = { a, list_type(a) };
        info.data_type = list_tycon();
        return info;
    }
    else if (con_name == "[]")
    {
        DataConInfo info;
        TypeVar a({noloc, "a"}, kind_type());
        info.uni_tvs = { a };
        info.data_type = list_tycon();
        return info;
    }
    else if (is_tuple_name(con_name) or con_name == "()")
    {
        DataConInfo info;
        int n = tuple_arity(con_name);
        for(int i=0;i<n;i++)
        {
            TypeVar tv({noloc, "a"+std::to_string(i+1)}, kind_type());
            info.uni_tvs.push_back( tv );
            info.field_types.push_back( tv );
        }
        info.data_type = tuple_tycon(n);
        return info;
    }

    auto C = lookup_resolved_symbol(con_name);

    if (not C) return {};

    return *C->con_info;
}

OpInfo Module::get_operator(const string& name) const
{
    OpInfo O;

    auto S = lookup_symbol(name);
    O.name = S->name;

    // An operator of undefined precedence is treated as if it has the highest precedence
    if (not S->fixity)
        O.fixity = {left_fix, 9};
    else
        O.fixity = *S->fixity;

    return O;
}

const_type_ptr Module::lookup_type(const std::string& name) const
{
    if (is_haskell_builtin_type_name(name))
        return lookup_builtin_type(name);

    int count = type_aliases.count(name);
    if (count == 0)
        throw myexception()<<"Type identifier '"<<name<<"' not declared.";
    else if (count == 1)
        return type_aliases.find(name)->second;
    else
    {
        myexception e;
        e<<"Type identifier '"<<name<<"' is ambiguous!";
        auto range = type_aliases.equal_range(name);
        for(auto i = range.first; i != range.second ;i++)
            e<<"\n "<<i->first<<" -> "<<i->second->name;
        throw e;
    }
}

const_type_ptr Module::lookup_resolved_type(const std::string& type_name) const
{
    // 1. Handle builtin names
    if (is_haskell_builtin_type_name(type_name))
        return lookup_builtin_type(type_name);

    // 2. Handle local names
    else if (name == get_module_name(type_name))
        return lookup_local_type(type_name);

    // 3. Handle external names
    else
        return lookup_external_type(type_name);
}

const_type_ptr Module::lookup_local_type(const std::string& type_name) const
{
    assert( get_module_name(type_name) == name);
    auto iter = types.find(type_name);
    if (iter == types.end())
        return nullptr;
    else
        return iter->second;
}

type_ptr Module::lookup_local_type(const std::string& type_name)
{
    assert( get_module_name(type_name) == name);
    auto iter = types.find(type_name);
    if (iter == types.end())
        return nullptr;
    else
        return iter->second;
}

const_type_ptr Module::lookup_external_type(const std::string& type_name) const
{
    assert(is_qualified_symbol(type_name));

    auto mod_name = get_module_name(type_name);

    auto uname = get_unqualified_name(type_name);

    auto mod_iter = transitively_imported_modules.find(mod_name);
    if (mod_iter == transitively_imported_modules.end())
        throw myexception()<<"Can't find external type '"<<type_name<<"' because module '"<<mod_name<<"' is not transitively imported.";

    return mod_iter->second->lookup_local_type(type_name);
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

// Here we do only phase 1 -- we only parse the decls enough to
//   determine which is a variable, and which are e.g. constructors.
// We can't determine function bodies at all, since we can't even handle op definitions
//   before we know fixities!

void Module::def_function(const std::string& fname)
{
    if (is_qualified_symbol(fname))
        throw myexception()<<"Locally defined symbol '"<<fname<<"' should not be qualified in function declaration.";

    declare_symbol({fname, variable_symbol, {}, {}, {}});
}

void Module::def_constructor(const string& cname, int arity, const string& type_name)
{
    if (is_qualified_symbol(cname))
        throw myexception()<<"Locally defined symbol '"<<cname<<"' should not be qualified.";

//    if (not is_qualified_symbol(type_name))
//        throw myexception()<<"Locally defined symbol '"<<type_name<<"' should not be qualified.";

    auto S = symbol_info(cname, constructor_symbol, qualify_local_name(type_name), arity, {});
    S.var_info->conlike = true;
    declare_symbol( S );
}

void Module::def_ADT(const std::string& tname, const type_info::data_info& info)
{
    if (is_qualified_symbol(tname))
        throw myexception()<<"Locally defined type '"<<tname<<"' should not be qualified.";

    declare_type( {tname, info, {}, /*arity*/ {}, /*kind*/ {}} );
}

void Module::def_ADT(const std::string& tname, const fixity_info& fixity, const type_info::data_info& info)
{
    if (is_qualified_symbol(tname))
        throw myexception()<<"Locally defined symbol '"<<tname<<"' should not be qualified.";

    declare_type( {tname, info, fixity, /*arity*/ {}, /*kind*/ {}} );
}

void Module::def_type_synonym(const std::string& sname, int arity)
{
    if (is_qualified_symbol(sname))
        throw myexception()<<"Locally defined type '"<<sname<<"' should not be qualified.";

    declare_type( {sname, type_info::type_syn_info(), {}, arity, /*kind*/ {}} );
}

void Module::def_type_family(const std::string& fname, int arity)
{
    if (is_qualified_symbol(fname))
        throw myexception()<<"Locally defined type '"<<fname<<"' should not be qualified.";

    declare_type( {fname, type_info::type_fam_info(), {}, arity, /*kind*/ {}} );
}

void Module::def_type_class(const std::string& cname, const type_info::class_info& info)
{
    if (is_qualified_symbol(cname))
        throw myexception()<<"Locally defined type '"<<cname<<"' should not be qualified.";

    declare_type( {cname, info, {}, /*arity*/ {}, /*kind*/ {}} );
}

void Module::def_type_class_method(const string& method_name, const string& class_name)
{
    if (is_qualified_symbol(method_name))
        throw myexception()<<"Locally defined type class method '"<<method_name<<"' should not be qualified.";

//    if (not is_qualified_symbol(class_name))
//        throw myexception()<<"Locally defined type class '"<<class_name<<"' should be qualified.";

    declare_symbol( {method_name, class_method_symbol, class_name, {}, {}} );
}

void Module::declare_fixities_(const Haskell::FixityDecl& FD)
{
    // Determine precedence.
    int precedence = (FD.precedence)?*FD.precedence:9;

    // Find op names and declare fixity and precedence.
    for(const auto& name: FD.names)
        declare_fixity(unloc(name), precedence, FD.fixity);
}

void Module::declare_fixities_(const Haskell::Decls& decls)
{
    // 0. Get names that are being declared.
    for(const auto& [_,decl]: decls)
        if (auto FD = decl.to<Haskell::FixityDecl>())
            declare_fixities_(*FD);
}

void Module::declare_fixities(const Hs::ModuleDecls& M)
{
    // At the top level, we have removed fixities from value_decls.
    for(auto& FD: M.fixity_decls)
        declare_fixities_(FD);

    for(const auto& [_,type_decl]: M.type_decls)
        if (auto C = type_decl.to<Haskell::ClassDecl>())
            for(auto& fixity_decl: C->fixity_decls)
                declare_fixities_(fixity_decl);
}

void Module::maybe_def_function(const string& var_name)
{
    // We don't know the type yet, probably, because we don't know the body.
    string qualified_name = qualify_local_name(var_name);
    auto loc = symbols.find(qualified_name);

    if (loc != symbols.end())
    {
        auto S = loc->second;
        // Only the fixity has been declared!
        if (S->symbol_type == unknown_symbol)
            S->symbol_type = variable_symbol;
    }
    else
        def_function(var_name);
}

void Module::add_local_symbols(const Hs::Decls& topdecls)
{
    // 0. Get names that are being declared.
    for(const auto& [_,decl]: topdecls)
    {
        if (auto pd = decl.to<Hs::PatDecl>())
        {
            for(const auto& var: Hs::vars_in_pattern( pd->lhs ))
                maybe_def_function( var.name );
        }
        else if (auto fd = decl.to<Hs::FunDecl>())
        {
            maybe_def_function( unloc(fd->v).name );
        }
        else if (auto data_decl = decl.to<Haskell::DataOrNewtypeDecl>())
        {
            // Why are we recording the arity here?  This is too early...
            // It looks like we use it when renaming patterns, but...
            type_info::data_info info;
            if (data_decl->is_regular_decl())
            {
                for(const auto& constr: data_decl->get_constructors())
                {
                    auto cname = unloc(*constr.con).name;
                    def_constructor(cname, constr.arity(), unloc(data_decl->name));
                    info.constructors.insert( qualify_local_name(cname) );
                    if (auto fields = to<Hs::FieldDecls>(constr.fields))
                        for(auto& field_decl: fields->field_decls)
                            for(auto& [loc,var]: field_decl.field_names)
                                info.fields.insert( qualify_local_name(var.name) );
                }
            }
            else if (data_decl->is_gadt_decl())
            {
                for(const auto& cons_decl: data_decl->get_gadt_constructors())
                    for(auto& con_name: cons_decl.con_names)
                    {
                        int arity = Hs::gen_type_arity( cons_decl.type );
                        auto cname = unloc(con_name);
                        def_constructor(cname, arity, unloc(data_decl->name));
                        info.constructors.insert( qualify_local_name(cname) );

                        // FIXME: handle GADT fielddecls Constr :: { name1 :: ArgType1, name2 :: ArgType2 } -> ResultType
                    }
            }

            def_ADT(unloc(data_decl->name), info);
        }
        else if (decl.is_a<Haskell::ClassDecl>())
        {
            auto& Class = decl.as_<Haskell::ClassDecl>();

            type_info::class_info info;

            for(auto& tf: Class.type_fam_decls)
                def_type_family( unloc(tf.con).name, tf.arity() );

            for(auto& sig_decl: Class.sig_decls)
                for(auto& v: sig_decl.vars)
                {
                    def_type_class_method(unloc(v).name, unloc(Class.name));
                    info.methods.insert(unloc(v).name);
                }
            
            
            def_type_class(unloc(Class.name), info);
        }
        else if (auto S = decl.to<Haskell::TypeSynonymDecl>())
        {
            def_type_synonym(unloc(S->name), S->arity());
        }
        else if (auto TF = decl.to<Hs::TypeFamilyDecl>())
        {
            def_type_family( unloc(TF->con).name, TF->arity() );
        }
    }
}

// A name of "" means that we are defining a top-level program, or a piece of a top-level program.
Module::Module(const string& n)
    :name(n)
{
    if (not name.size())
        throw myexception()<<"Module name may not be empty!";
}

Module::Module(const char *n)
    :Module(string(n))
{ }

Module::Module(const Haskell::Module& M, const LanguageExtensions& le, const FileContents& f)
    :language_extensions(le),
     module(M),
     name(unloc(module.modid)),
     file(f)
{
    if (not name.size())
        throw myexception()<<"Module name may not be empty!";
}

std::ostream& operator<<(std::ostream& o, const Module& M)
{
    if (M.module.topdecls)
        for(const auto& decls: *M.module.topdecls)
            o<<decls.print()<<"\n";

    return o;
}


Hs::Var Module::get_external_var(const string& sname) const
{
    Hs::Var v(name);
    v.info = lookup_external_symbol(sname);
    return v;
}

set<string> special_prelude_symbols =
{
    // This is kind of a secret one, used in the desugaring of strings.
    "Foreign.String.unpack_cpp_string",

    // These are all Prelude symbols used in desugaring.
    // Modid.name should be equivalent to Prelude.name.
    "Compiler.Error.error",
    "Data.OldList.concatMap",
    "Control.Monad.fail",
    "Compiler.Enum.enumFrom",
    "Compiler.Enum.enumFromTo",
    "Compiler.Enum.enumFromThen",
    "Compiler.Enum.enumFromThenTo",
    "Compiler.Enum.enumFrom",
    "Prelude.undefined"
};

bool special_prelude_symbol(const string& name)
{
    return special_prelude_symbols.count(name) > 0;
}
