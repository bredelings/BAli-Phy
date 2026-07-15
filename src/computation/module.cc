#include <set>
#include <regex>
#include <tuple>
#include <fstream>
#include <algorithm>
#include <cstdint>
#include <cstring>
#include <limits>
#include <string_view>
#include "computation/module.H"
#include "computation/record_utils.H"
#include "computation/preprocess.H"
#include "util/myexception.H"
#include "util/variant.H"
#include "util/io.H"
#include "util/log-level.H"
#include "range/v3/all.hpp"
#include "util/range.H"
#include "util/set.H"   // for add( , )
#include "util/string/split.H"
#include "util/string/join.H"
#include "util/file-paths.H" // for exe_mtime()
#include "program.H"
#include "operations.H"
#include "computation/machine/graph_register.H" // for graph_normalize( )
#include "computation/operations.H"
#include "rename/rename.H"
#include "desugar/desugar.H"
#include "typecheck/typecheck.H"
#include "computation/loader.H"
#include "core/convert.H" // for load_builtins( )
#include "computation/optimization/simplifier.H"
#include "computation/optimization/call-arity.H"
#include "computation/optimization/occurrence.H"
#include "computation/optimization/float-out.H"
#include "computation/optimization/inliner.H"
#include "computation/haskell/haskell.H"
#include "computation/haskell/ids.H"
#include "computation/core/func.H"
#include "util/assert.hh"
#include <fmt/chrono.h>

#include <cereal/archives/binary.hpp>
#define CEREAL_FUTURE_EXPERIMENTAL 1
#include <cereal/archives/adapters.hpp>

#include <xxhash.h>
#include <zstd.h>

#if defined(__ELF__) && defined(__linux__)
#include <elf.h>
#include <link.h>
#endif

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

namespace
{
    bool contains_constructor(const vector<string>& constructors, const string& name)
    {
        return std::ranges::find(constructors, name) != constructors.end();
    }

    enum class ClassChildKind
    {
        method,
        associated_type_family,
        associated_data_family
    };

    struct ClassChild
    {
        string name;
        string resolved_name;
        ClassChildKind kind;
    };

    // Visit the value and type children that belong to a class export/import item.
    // The child name matches C(x) syntax; resolved_name identifies the entity.
    template <typename F>
    void for_each_class_child(const type_info& class_type, F&& f)
    {
        auto class_info = class_type.is_class();
        assert(class_info);

        auto class_module = get_module_name(class_type.name);
        for(const auto& method: class_info->methods)
        {
            auto method_name = get_unqualified_name(method);
            auto resolved_name = is_qualified_symbol(method) ? method : class_module + "." + method_name;
            f(ClassChild{method_name, resolved_name, ClassChildKind::method});
        }

        if (not class_info->info)
            return;

        for(const auto& [type_family, _]: class_info->info->associated_type_families)
            f(ClassChild{get_unqualified_name(type_family.name), type_family.name, ClassChildKind::associated_type_family});

        for(const auto& data_family: class_info->info->associated_data_families)
            f(ClassChild{get_unqualified_name(data_family.name), data_family.name, ClassChildKind::associated_data_family});
    }

    // Look up a named child in the class child set used by C(...) and C(..).
    // This accepts methods and associated families.
    optional<ClassChild> class_child_named(const type_info& class_type, const string& name)
    {
        optional<ClassChild> result;
        for_each_class_child(class_type, [&](const ClassChild& child) { if (not result and child.name == name) result = child; });
        return result;
    }
}

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

        if (S3->symbol_type == symbol_type_t::unknown)
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
    assert(T.arity);

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
        declare_symbol({s, symbol_type_t::unknown, {}, {}, {}});

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

// Add qualified and, when allowed, unqualified aliases for an imported record field label.
void Module::import_field(const FieldInfo& field, const string& modid, bool qualified)
{
    auto name = get_unqualified_name(field.name);
    field_aliases.insert({modid + "." + name, field});
    if (not qualified)
        field_aliases.insert({name, field});
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

    fresh_var_state().after(M2->fresh_var_state());

    transitively_imported_modules.insert({M2->name(), M2});
    for(auto& [mod_name, mod]: M2->transitively_imported_modules())
        transitively_imported_modules.insert({mod_name, mod});

    auto modid = unloc(impdecl.modid);
    if (impdecl.as)
        modid = unloc(*impdecl.as);
    bool qualified = impdecl.qualified;
    assert(modid != name);

    auto& m2_exported_values = M2->exported_symbols();
    auto& m2_exported_fields = M2->exported_fields();
    auto& m2_exported_types = M2->exported_types();

    // Import an exported field label only when it belongs to the imported type child list.
    auto import_exported_field_for_type = [&](const type_info& type, const string& field_name)
    {
        bool imported = false;
        for(const auto& field: record_fields_for_type(type))
        {
            auto name = get_unqualified_name(field.name);
            if (name != field_name)
                continue;

            auto range = m2_exported_fields.equal_range(name);
            for(auto i = range.first; i != range.second; ++i)
            {
                if (i->second.name == field.name and i->second.parent_type == field.parent_type)
                {
                    import_field(i->second, modid, qualified);
                    imported = true;
                }
            }
        }
        return imported;
    };

    // import modid
    if (not impdecl.impspec)
    {
        for(const auto& [_,S]: m2_exported_values)
            import_symbol(S, modid, qualified);

        for(const auto& [_,field]: m2_exported_fields)
            import_field(field, modid, qualified);

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
                if (not m2_exported_values.count(id))
                {
                    messages.push_back( error(loc, Note()<<"variable `"<<id<<"` not exported.") );
                    continue;
                }

                if (s.subspec)
                    messages.push_back( error(loc, Note()<<"variable `"<<id<<"` can't have export subspec") );

                auto variable = m2_exported_values.at(id);
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
                    else if (type->is_class())
                    {
                        if (not s.subspec->names)
                        {
                            // Import only the class children that the source module exported.
                            // A re-export may expose a subset of the original class children.
                            for_each_class_child(*type, [&](const ClassChild& child)
                            {
                                if (child.kind == ClassChildKind::method)
                                {
                                    if (auto exported = m2_exported_values.find(child.name); exported != m2_exported_values.end() and exported->second->name == child.resolved_name)
                                        import_symbol(exported->second, modid, qualified);
                                }
                                else
                                {
                                    if (auto exported = m2_exported_types.find(child.name); exported != m2_exported_types.end() and exported->second->name == child.resolved_name)
                                        import_type(exported->second, modid, qualified);
                                }
                            });
                        }
                        else
                        {
                            for(auto& [loc,name]: *s.subspec->names)
                            {
                                auto child = class_child_named(*type, name);
                                if (not child)
                                    messages.push_back( error(loc, Note()<<"`"<<name<<"` is not a method or associated family for class `"<<id<<"`") );
                                else if (child->kind == ClassChildKind::method)
                                {
                                    if (auto exported = m2_exported_values.find(name); exported != m2_exported_values.end() and exported->second->name == child->resolved_name)
                                        import_symbol(exported->second, modid, qualified);
                                    else
                                        messages.push_back( error(loc, Note()<<"`"<<name<<"` was not exported") );
                                }
                                else
                                {
                                    if (auto exported = m2_exported_types.find(name); exported != m2_exported_types.end() and exported->second->name == child->resolved_name)
                                        import_type(exported->second, modid, qualified);
                                    else
                                        messages.push_back( error(loc, Note()<<"`"<<name<<"` was not exported") );
                                }
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
				if (m2_exported_values.contains(name))
				    import_symbol(m2_exported_values.at( name ), modid, qualified);
			    }
                            for(const auto& field: record_fields_for_type(*type))
                            {
                                auto name = get_unqualified_name(field.name);
                                if (m2_exported_values.contains(name))
                                    import_symbol(m2_exported_values.at( name ), modid, qualified);
                                import_exported_field_for_type(*type, name);
                            }
                        }
                        else
                        {
                            auto type_modid = get_module_name(type->name);
                            for(auto& [loc,name]: *s.subspec->names)
                            {
                                if (is_haskell_conid(name) and not contains_constructor(d->constructors, type_modid + "." + name))
                                {
                                    messages.push_back( error(loc, Note()<<"`"<<name<<"` is not a constructor for data type `"<<id<<"`"));
                                    continue;
                                }
                                if (is_haskell_varid(name) and not type_has_record_field(*type, name))
                                {
                                    messages.push_back( error(loc, Note()<<"`"<<name<<"` is not a field for data type `"<<id<<"`"));
                                    continue;
                                }

				if (is_haskell_conid(name) and not m2_exported_values.contains(name))
				{
                                    messages.push_back( error(loc, Note()<<"`"<<name<<"` was not exported"));
                                    continue;
				}

                                if (is_haskell_conid(name))
                                    import_symbol(m2_exported_values.at(name), modid, qualified);
                                else
                                {
                                    auto imported_field = import_exported_field_for_type(*type, name);
                                    if (m2_exported_values.contains(name))
                                        import_symbol(m2_exported_values.at(name), modid, qualified);
                                    else if (not imported_field)
                                        messages.push_back( error(loc, Note()<<"`"<<name<<"` was not exported"));
                                }
                            }
                        }
                    }
                    else if (auto d = type->is_data_fam())
                    {
                        if (not s.subspec->names)
                        {
                            for(auto& constructor: d->constructors)
			    {
				auto name = get_unqualified_name(constructor);
				if (m2_exported_values.contains(name))
				    import_symbol(m2_exported_values.at( name ), modid, qualified);
			    }
                            for(const auto& field: record_fields_for_type(*type))
                            {
                                auto name = get_unqualified_name(field.name);
                                if (m2_exported_values.contains(name))
                                    import_symbol(m2_exported_values.at( name ), modid, qualified);
                                import_exported_field_for_type(*type, name);
                            }
                        }
                        else
                        {
                            auto type_modid = get_module_name(type->name);
                            for(auto& [loc,name]: *s.subspec->names)
                            {
                                if (is_haskell_conid(name) and not d->constructors.count(type_modid + "." + name))
                                {
                                    messages.push_back( error(loc, Note()<<"`"<<name<<"` is not a constructor for data family `"<<id<<"`"));
                                    continue;
                                }
                                if (is_haskell_varid(name) and not type_has_record_field(*type, name))
                                {
                                    messages.push_back( error(loc, Note()<<"`"<<name<<"` is not a field for data family `"<<id<<"`"));
                                    continue;
                                }

				if (is_haskell_conid(name) and not m2_exported_values.contains(name))
				{
                                    messages.push_back( error(loc, Note()<<"`"<<name<<"` was not exported"));
                                    continue;
				}

                                if (is_haskell_conid(name))
                                    import_symbol(m2_exported_values.at(name), modid, qualified);
                                else
                                {
                                    auto imported_field = import_exported_field_for_type(*type, name);
                                    if (m2_exported_values.contains(name))
                                        import_symbol(m2_exported_values.at(name), modid, qualified);
                                    else if (not imported_field)
                                        messages.push_back( error(loc, Note()<<"`"<<name<<"` was not exported"));
                                }
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
        set<string> hidden_values;
        set<string> hidden_types;

        for(const auto& [_,s]: impdecl.impspec->imports)
        {
            string id = unloc(s.symbol);

            if (s.is_value())
                hidden_values.insert(id);
            else if (s.is_type())
            {
                hidden_types.insert(id);

                auto exported_type = m2_exported_types.find(id);
                if (exported_type == m2_exported_types.end())
                    continue;

                auto class_type = exported_type->second;
                if (not class_type->is_class() or not s.subspec)
                    continue;

                // Add class children from hiding subspecs without hiding unrelated
                // entities that happen to have the same unqualified name.
                auto hide_child = [&](const ClassChild& child)
                {
                    if (child.kind == ClassChildKind::method)
                    {
                        if (auto exported = m2_exported_values.find(child.name); exported != m2_exported_values.end() and exported->second->name == child.resolved_name)
                            hidden_values.insert(child.name);
                    }
                    else if (auto exported = m2_exported_types.find(child.name); exported != m2_exported_types.end() and exported->second->name == child.resolved_name)
                        hidden_types.insert(child.name);
                };

                if (not s.subspec->names)
                    for_each_class_child(*class_type, hide_child);
                else
                    for(auto& [_,name]: *s.subspec->names)
                        if (auto child = class_child_named(*class_type, name))
                            hide_child(*child);
            }
        }

        for(const auto& [_,S]: m2_exported_values)
        {
            auto unqualified_name = get_unqualified_name(S->name);

            if (hidden_values.contains(unqualified_name)) continue;

            import_symbol(S, modid, qualified);
        }
        for(const auto& [_,field]: m2_exported_fields)
        {
            auto unqualified_name = get_unqualified_name(field.name);

            if (hidden_values.contains(unqualified_name)) continue;

            import_field(field, modid, qualified);
        }
        for(const auto& [_,T]: m2_exported_types)
        {
            auto unqualified_name = get_unqualified_name(T->name);

            if (hidden_types.contains(unqualified_name)) continue;

            import_type(T, modid, qualified);
        }
    }
}

vector<Hs::LImpDecl> Module::imports() const
{
    // 1. Copy the imports list
    auto imports_list = module_AST.impdecls;

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

set<string> Module::hidden_dependencies() const
{
    set<string> modules;
    if (language_extensions.has_extension(LangExt::RecursiveDo))
        modules.insert(control_monad_fix_module_name);

    if (module_AST.topdecls)
    {
        Hs::ModuleDecls declarations(*module_AST.topdecls);
        for(const auto& foreign_decl: declarations.foreign_decls)
            if (unloc(foreign_decl.call_conv) == "trcall")
            {
                modules.insert("Compiler.FFI.Import");
                break;
            }
    }

    return modules;
}

set<string> Module::dependencies() const
{
    set<string> modules = hidden_dependencies();
    for(auto& [loc,impdecl]: imports() )
        modules.insert( unloc(impdecl.modid) );
    return modules;
}

std::string xxhash_to_hex(uint64_t hash) {
    std::stringstream ss;
    ss << std::hex << std::setw(16) << std::setfill('0') << hash;
    return ss.str();
}

std::string xxhash64_hex(const std::string& s) {
    uint64_t hash = XXH3_64bits(s.data(), s.size());
    std::stringstream ss;
    ss << std::hex << std::setw(16) << std::setfill('0') << hash;
    return ss.str();
}

// This is independent of the executable-keyed cache directory: it makes the
// archive self-describing and prevents an older reader from silently treating
// newly appended compiled-module metadata as a valid artifact.
static constexpr std::uint32_t compiled_module_cache_format = 4;

std::string extract_xxhash(std::string& data)
{
    // 1. Check that we have 16 chars followed by a newline.
    if (data.size() < 17 or data[16] != '\n')
        throw myexception()<<"archive failed integrity check: failed to read stored integrity hash";

    // 2. Get the 16 chars and check that they are all hex digits.
    string stored_archive_hash = data.substr(0,16);
    stored_archive_hash.resize(16);
    for(char c: stored_archive_hash)
	if (not std::isxdigit(c)) throw myexception()<<"archive failed integrity check: failed to read stored integrity hash";

    // 3. Drop the first 17 chars -- 16 hex digits plus newline.
    data = data.substr(17);

    return stored_archive_hash;
}

// Compress one complete compile artifact using zstd's fast level 1.
static string compress_compile_artifact(std::string_view data)
{
    string compressed(ZSTD_compressBound(data.size()), '\0');
    auto compressed_size = ZSTD_compress(compressed.data(), compressed.size(),
                                         data.data(), data.size(), 1);
    if (ZSTD_isError(compressed_size))
        throw myexception()<<"failed to compress compile artifact: "
                           <<ZSTD_getErrorName(compressed_size);
    compressed.resize(compressed_size);
    return compressed;
}

// Decompress a complete compile artifact whose original size is in its frame.
static string decompress_compile_artifact(std::string_view compressed)
{
    auto data_size = ZSTD_getFrameContentSize(compressed.data(), compressed.size());
    if (data_size == ZSTD_CONTENTSIZE_ERROR)
        throw myexception()<<"compile artifact is not a zstd frame";
    if (data_size == ZSTD_CONTENTSIZE_UNKNOWN)
        throw myexception()<<"compile artifact does not record its uncompressed size";
    if (data_size > std::numeric_limits<std::size_t>::max())
        throw myexception()<<"compile artifact is too large to decompress";

    string data(static_cast<std::size_t>(data_size), '\0');
    auto decompressed_size = ZSTD_decompress(data.data(), data.size(),
                                             compressed.data(), compressed.size());
    if (ZSTD_isError(decompressed_size))
        throw myexception()<<"failed to decompress compile artifact: "
                           <<ZSTD_getErrorName(decompressed_size);
    if (decompressed_size != data.size())
        throw myexception()<<"compile artifact decompressed to an unexpected size";
    return data;
}

std::shared_ptr<CompiledModule> read_cached_module(const module_loader& loader, const std::string& modid, const std::string& required_xxhash)
{
    if (loader.recompile_all or loader.recompile_modules.count(modid)) return {};

    if (auto path = loader.find_cached_module(modid))
    {
        try
        {
            std::ifstream infile(*path, std::ios::binary);
            std::string compressed = std::string(std::istreambuf_iterator<char>(infile), std::istreambuf_iterator<char>());
            std::string data = decompress_compile_artifact(compressed);
            string stored_archive_xxhash = extract_xxhash(data);

            string computed_archive_xxhash = xxhash64_hex(data);
            if (log_verbose >= 4)
                std::cerr<<"    Read archive for "<<modid<<":    length = "<<data.size()<<"    stored_archive_hash = "<<stored_archive_xxhash<<"     computed_archive_hash = "<<computed_archive_xxhash<<"\n";

            if (stored_archive_xxhash != computed_archive_xxhash)
                throw myexception()<<"archive failed integrity check: stored and computed archive integrity hash did not match.";

            std::istringstream data2(data);
            cereal::UserDataAdapter<const module_loader, cereal::BinaryInputArchive> archive( loader, data2 );

            std::shared_ptr<CompiledModule> M;

            std::uint32_t format = 0;
            archive(format);
            if (format != compiled_module_cache_format)
                throw myexception()<<"compiled module cache format "<<format
                                   <<" is not supported (expected "
                                   <<compiled_module_cache_format<<")";

            std::string hash;
            archive(hash);

            if (hash == required_xxhash)
            {
                archive(M);

                if (hash == M->all_inputs_hash())
                    return M;

                throw myexception()<<"Beginning and ending integrity hash inside the archive do not match!";
            }
        }
        catch (std::exception& e)
        {
            if (log_verbose >= 2)
                std::cerr<<"Failure loading cached compile artifact for "<<modid<<".\n   File = "<<*path<<"\n  exception = "<<e.what()<<"\n";
        }
        catch (...)
        {
            if (log_verbose >= 2)
                std::cerr<<"Failure loading cached compile artifact for "<<modid<<".\n   File = "<<*path<<"\n";
        }
    }

    return {};
}

#ifdef _WIN32
#include <processthreadsapi.h>
#else
#include <unistd.h>
#endif

bool write_compile_artifact(const Program& P, std::shared_ptr<CompiledModule>& CM)
{
    namespace fs = std::filesystem;
    /* We need to avoid the situation where two processes write the module at the same time.
     * We were getting aliases to partial names - e.g. 'Compiler.RealFloat.RealFloa'
     * We tried to handle this by truncating the file when it is open.

     * But we also need to avoid trying to load a cereal archive that isn't finished being written.
     * So we first write the archive to a temporary file and the move it into place.
     * We can rename from /tmp on Linux, because that is renaming across filesystems.
     */

#ifdef _WIN32
    int pid = GetCurrentProcessId();
#else
    int pid = getpid();
#endif

    auto modid = CM->name();

    if (auto mod_path = P.get_module_loader()->cache_path_for_module(modid))
    {
        // Get a tmp file to write to that is unique to this process.
        fs::path tmp_path = *mod_path;
        tmp_path +="-" + std::to_string(pid);

        try
        {
            std::ostringstream buffer;
            {
                cereal::BinaryOutputArchive archive( buffer );
                archive(compiled_module_cache_format);
                archive(CM->all_inputs_hash());
                archive(CM);
            }
            string data = buffer.str();
            string archive_hash = xxhash64_hex(data);
            string artifact = archive_hash + "\n" + data;
            string compressed = compress_compile_artifact(artifact);

            if (log_verbose >= 4)
                std::cerr<<"    Writing archive for "<<modid<<":    length = "<<data.size()<<"    compressed length = "<<compressed.size()<<"    hash = "<<archive_hash<<"\n";

            // Create parent directories if needed.
            fs::create_directories(mod_path->parent_path());

            // Create and open the temporary file.
            std::ofstream tmp_file(tmp_path, std::ios::binary | std::ios::trunc);

            if (not tmp_file) throw myexception()<<"Could not open file!";

            // Write the archive to the temporary file.
            tmp_file.write(compressed.data(), compressed.size());
            tmp_file.close();

            // Move the temporary file to the correct location.
            fs::rename(tmp_path, *mod_path);

            return true;
        }
        catch (std::exception& e)
        {
            if (log_verbose >= 2)
                std::cerr<<"  Failure writing cached compile artifact for "<<modid<<": "<<e.what()<<"\n    file = "<<*mod_path<<"\n";
        }
        catch (...)
        {
            if (log_verbose >= 2)
                std::cerr<<"  Failure writing cached compile artifact for "<<modid<<"\n    file = "<<*mod_path<<"\n";
        }
    }

    return false;
}

void mark_exported_decls(Core::Decls<>& decls,
                         const map<string,const_symbol_ptr>& exports,
                         const Module& M)
{
    // Record exports
    set<string> exported;
    for(auto& [name,symbol]: exports)
        if (get_module_name(symbol->name) == M.name)
            exported.insert(symbol->name);

    // Mark exported vars as exported
    for(auto& [x,_]: decls)
    {
        if (exported.count(x.name))
        {
            x.is_exported = true;
            exported.erase(x.name);
        }
        else if (is_qualified_symbol(x.name))
        {
            // We don't have a way to find all the exported symbols
            // and check that we have a top-level declaration for them.
            if (auto S = M.lookup_resolved_symbol(x.name); S and S->symbol_type >= symbol_type_t::superclass_selector)
            {
                x.is_exported = true;
            }
        }
    }

    // Check that we don't export things that don't exist
    if (false and not exported.empty())
    {
        // FIXME: class members don't have a value def, and so this doesn't work.
        myexception e;
        e<<"Module '"<<M.name<<"' exports undefined symbols:\n";
        for(auto& name: exported)
            e<<"  "<<name;
        throw e;
    }
}

std::optional<Core::Var<>> find_first_duplicate_var(const Core::Decls<>& decls)
{
    set<Core::Var<>> vars;
    for(auto& [x,_]: decls)
    {
	if (vars.count(x))
	    return x;
	else
            vars.insert(x);
    }
    return {};
}

void check_duplicate_var(const Core::Decls<>& decls)
{
    auto var = find_first_duplicate_var(decls);
    if (var)
	throw myexception()<<"variable '"<<var->print()<<"' occurs twice!";
}

template <typename T>
void erase_one(multiset<T>& mset, const T& elem)
{
    auto it = mset.find(elem);
    assert(it != mset.end());
    mset.erase(it);
}

set<Core::Var<>> vars_in_pattern(const Core::Pattern<>& p)
{
    set<Core::Var<>> vars;
    for(auto& arg: p.args)
        vars.insert(arg);
    return vars;
}

Core::Var<> rename_var(const Core::Var<>& x, const map<Core::Var<>,Core::Var<>>& substitution, multiset<Core::Var<>>& bound)
{
    // 1.1 If there's a substitution x -> E
    if (not bound.count(x) and substitution.count(x))
        return substitution.at(x);
    else
        return x;
}

Core::Exp<> rename(const Core::Exp<>& E, const map<Core::Var<>,Core::Var<>>& substitution, multiset<Core::Var<>>& bound)
{
    assert(not E.empty());

    // 1. Var (x)
    if (auto x = E.to_var())
        return rename_var(*x, substitution, bound);
    // 2. Lambda (E = \x -> body)
    else if (auto L = E.to_lambda())
    {
        bound.insert(L->x);
        auto body = rename(L->body, substitution, bound);
        erase_one(bound, L->x);

        return Core::Lambda<>{L->x,body};
    }
    // 3. Apply
    else if (auto A = E.to_apply())
    {
        auto head = rename(A->head, substitution, bound);

        auto arg = rename(A->arg, substitution, bound);

        return Core::Apply<>{head, arg};
    }
    // 4. Let (let {x[i] = F[i]} in body)
    else if (auto L = E.to_let())
    {
        if (auto nonrec = L->to_nonrec())
        {
            auto decl = nonrec->decl;
            decl.body = rename(decl.body, substitution, bound);
            bound.insert(decl.x);
            auto body = rename(L->body, substitution, bound);
            erase_one(bound, decl.x);
            return Core::Let<>{Core::NonRec<>{std::move(decl)}, std::move(body)};
        }

        auto decls = L->to_rec()->decls;
        for(auto& [x,_]: decls)
            bound.insert(x);

        auto body = rename(L->body, substitution, bound);

        for(auto& [_,e]: decls)
            e = rename(e, substitution, bound);

        for(auto& [x,e]: decls)
            erase_one(bound, x);

        return Core::Let<>{Core::Rec<>{std::move(decls)}, std::move(body)};
    }
    // 5. Case
    else if (auto C = E.to_case())
    {
        // Analyze the object
        auto object = rename(C->object, substitution, bound);
        auto alts = C->alts;
        for(auto& [pattern, body]: alts)
        {
            auto pat_vars = vars_in_pattern(pattern);
            for(auto& x: pat_vars)
                bound.insert(x);

            body = rename(body, substitution, bound);

            for(auto& x: pat_vars)
                erase_one(bound, x);
        }
        return Core::Case<>{object, alts};
    }
    // 6. ConApp
    else if (auto C = E.to_conApp())
    {
        auto args = C->args;
        for(auto& arg: args)
            arg = rename(arg, substitution, bound);
        return Core::ConApp<>{C->head, args};
    }
    // 7. BuiltinOp
    else if (auto B = E.to_builtinOp())
    {
        auto args = B->args;
        for(auto& arg: args)
            arg = rename(arg, substitution, bound);

        return Core::BuiltinOp<>(B->lib_name, B->func_name, B->call_conv, args, B->op);
    }
    // 8. Constant
    else if (E.to_constant())
        return E;
    else if (E.to_runtimeOnly())
        return E;
    else
        std::abort();
}


std::optional<string> get_new_name(const Core::Var<>& x, const string& module_name)
{
    assert(not is_haskell_builtin_con_name(x.name));

    if (is_qualified_symbol(x.name))
    {
        // Allow adding suffixes like #1 to qualified names IF they are not exported.
        // Such suffixes can be added by renaming inside of let-floating.
        assert(x.index == 0 or not x.is_exported);
        return {};
    }

    return module_name + "." + x.name + "#" + convertToString(x.index);
}

Core::Decls<> rename_top_level(const Core::Decls<>& decls, const string& module_name)
{
    map<Core::Var<>, Core::Var<>> substitution;

    set<Core::Var<>> top_level_vars;

    Core::Decls<> decls2;

#ifndef NDEBUG
    check_duplicate_var(decls);
#endif

    for(int i = 0; i< decls.size(); i++)
    {
        auto& [x,rhs] = decls[i];
        auto x2 = x;
        assert(not substitution.count(x));

        if (auto new_name = get_new_name(x, module_name))
        {
            x2 = Core::Var<>(*new_name);
            assert(not substitution.count(x2));
            substitution.insert({x,x2});
        }

        decls2.push_back({x2,rhs});

        // None of the renamed vars should have the same name;
        assert(not top_level_vars.count(x2));
        top_level_vars.insert(x2);
    }

    multiset<Core::Var<>> bound;
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

std::shared_ptr<CompiledModule> compile(const Program& P, std::shared_ptr<Module> MM)
{
    auto& loader = *P.get_module_loader();
    simplifier_options& opts = loader;

    if (auto C = read_cached_module(loader, MM->name, MM->all_inputs_hash(P)))
    {
	if (log_verbose) std::cerr<<"[ Loading "<<MM->name<<" ]\n";
	C->inflate(P);
	return C;
    }

    if (log_verbose)
	std::cerr<<"[ Compiling "<<MM->name<<" ]\n";

    // Scans imported modules and modifies symbol table and type table
    MM->perform_imports(P);

    Hs::ModuleDecls M;
    if (MM->module_AST.topdecls)
        M = Hs::ModuleDecls(*MM->module_AST.topdecls);

    // We should create a "local fixity environment" mapping from var and Module.var -> fixity info.
    // This can supplement the symbols that we imported from other modules.
    // Then we can AUGMENT this local fixity environment when we encounter fixities at lower levels.
    // Currently rename_infix has to handle UNresolved top-level names, because we do it BEFORE renaming.
    MM->declare_fixities(M);

    // We should be able to build these as we go, in rename!
    // We can merge them into a global symbol table (if we want) afterwards.

    // calls def_function, def_ADT, def_constructor, def_type_class, def_type_synonym, def_type_family
    MM->add_local_symbols(M.type_decls);

    for(const auto& [_, field]: MM->local_synthesizable_record_fields())
        MM->def_record_selector(field);

    // Disambiguate parsed syntax after local type symbols are available, so
    // record field metadata is available for constructor-directed resolution.
    M = disambiguate_module(M);

    // calls def_function, def_ADT, def_constructor, def_type_class, def_type_synonym, def_type_family
    MM->add_local_symbols(M.value_decls[0]);

    for(auto& f: M.foreign_decls)
        MM->def_function( unloc(f.function).name );

    // Currently we do "renaming" here.
    // That just means (1) qualifying top-level declarations and (2) desugaring rec statements.
    M = MM->rename(opts, M);

    for(auto& fdecl: M.foreign_decls)
    {
        auto& call_conv = unloc(fdecl.call_conv);
        auto& fname = unloc(fdecl.function).name;
        auto loc = fdecl.function.loc;

        if (call_conv == "bpcall")
        {
            // inject signature 
            M.value_decls.signatures.insert({fdecl.function, fdecl.type});
        }
        else if (call_conv == "ecall")
        {
            // inject signature 
            M.value_decls.signatures.insert({fdecl.function, fdecl.type});
        }
        else if (call_conv == "trcall")
        {
            string raw_name = fname +"$raw";
            MM->def_function( get_unqualified_name(raw_name) );

            Hs::LVar fromCImport = {noloc, Hs::Var("Compiler.FFI.Import.fromCImport")};
            Hs::LVar raw   = {noloc, Hs::Var(raw_name)};
            Hs::LVar lhs   = fdecl.function;
            Hs::LExp rhs   = {noloc, Hs::Apply(fromCImport, raw)};
            Hs::Decls decls;
            decls.push_back({noloc, Hs::simple_decl(lhs,rhs)});

            // fname :: <type>
            // fname = fromCImport fname$raw
            M.value_decls.signatures.insert({fdecl.function, fdecl.type});
            M.value_decls.push_back(decls);

            // fname$raw :: RawImport <type>
            Hs::TypeCon RawImport("Compiler.FFI.Import.RawImport");
            Hs::Type raw_type = Hs::TypeApp({loc,RawImport},fdecl.type);
            M.value_decls.signatures.insert({raw, {loc, raw_type}});
        }
    }

    // Set the inline pragma -- must happen after renaming.
    for(auto& [lvar, ip]: M.value_decls.inline_sigs)
    {
        auto S = MM->lookup_local_symbol(unloc(lvar).name);
        S->inline_pragma = ip;
    }

    auto tc_result = std::make_shared<TypeChecker>( *MM )->typecheck_module( M );

    auto [hs_decls, core_decls] = tc_result.all_binds();

    if (opts.dump_typechecked)
    {
        std::cerr<<"\nType-checked:\n";
        for(auto& [name, sym]: MM->symbols)
        {
            std::cerr<<name<<" :: "<<sym->type.print()<<"\n";
        }
        for(auto& decl: hs_decls)
        {
            std::cerr<<decl.print()<<"\n";
        }
        std::cerr<<core_decls.print()<<"\n";
    }

    // Updates exported_symbols_ + exported_types_
    MM->perform_exports();

    // look only in value_decls now
    // FIXME: how to handle functions defined in instances and classes?

    auto value_decls = MM->desugar(opts, MM->fresh_var_state(), hs_decls);

    value_decls += core_decls;

    value_decls += MM->load_builtins(loader, tc_result.foreign_decls);

    value_decls += MM->load_constructors(M.type_decls);

    // Check for duplicate top-level names.
    check_duplicate_var(value_decls);

    mark_exported_decls(value_decls, MM->exported_symbols(), *MM);

    if (opts.dump_desugared)
    {
        std::cerr<<"\nCore:\n";
        for(auto& [x,rhs] : value_decls)
        {
            if (x.is_exported)
                std::cerr<<"[*] ";
            else
                std::cerr<<"    ";
            std::cerr<<"["<<simple_size(rhs)<<"] "<<x.print()<<" = "<<rhs.print()<<"\n";
        }
        std::cerr<<"\n\n";
    }

    value_decls = MM->optimize(opts, MM->fresh_var_state(), value_decls);

    value_decls = rename_top_level(value_decls, MM->name);

    if (opts.dump_optimized)
    {
        std::cerr<<"\nOptimized Core:\n";
        for(auto& [x,rhs] : value_decls)
        {
            if (x.is_exported)
                std::cerr<<"[*] ";
            else
                std::cerr<<"    ";
            std::cerr<<"["<<simple_size(rhs)<<"] "<<x.print()<<" = "<<rhs.print()<<"\n";
        }
        std::cerr<<"\n\n";
    }

    // this records unfoldings.
    MM->export_small_decls(opts, value_decls);

    auto CM = std::make_shared<CompiledModule>(MM);

    std::vector<Hs::CompiledForeignInfo> foreign_infos;
    for(const auto& declaration: tc_result.foreign_decls)
    {
        if (not declaration.foreign_info) continue;

        const auto& checked = *declaration.foreign_info;
        Hs::CompiledForeignInfo info;
        info.public_name = get_unqualified_name(unloc(declaration.function).name);
        info.raw_name = info.public_name;
        info.plugin_name = declaration.plugin_name;
        info.symbol_name = declaration.symbol_name;
        info.call_conv = unloc(declaration.call_conv);
        if (info.call_conv == "trcall") info.raw_name += "$raw";

        info.haskell_type = checked.public_type.value_or(checked.haskell_type);
        info.foreign_type = checked.foreign_type;
        info.abi_type = checked.abi_type;
        info.layout = checked.layout;
        foreign_infos.push_back(std::move(info));
    }
    CM->set_foreign_infos(std::move(foreign_infos));

    CM->finish_value_decls(value_decls);

    bool ok = write_compile_artifact(P, CM);

    // Check the compile artifact
    if (log_verbose)
    {
	if (auto CM2 = read_cached_module(loader, MM->name, MM->all_inputs_hash(P)))
	{
	    CM2->inflate(P);
	    if (CM2->all_inputs_hash() != CM->all_inputs_hash())
		std::cerr<<"Compiled module "<<MM->name<<" changed between writing and rereading!";
	}
	else if (ok)
	    std::cerr<<" Failed to read compiled module "<<MM->name<<"!\n";
    }

    CM->inflate(P);
    return CM;
}

void Module::perform_imports(const Program& P)
{
    for(auto& impdecl: imports() )
        import_module(P, impdecl);

    for(const auto& module_name: hidden_dependencies())
    {
        // Hidden dependencies are semantic dependencies for compiler-generated
        // names.  Do not call import_module(): that would add source aliases.
        auto M2 = P.get_module(module_name);
        fresh_var_state().after(M2->fresh_var_state());
        transitively_imported_modules.insert({M2->name(), M2});
        for(auto& [mod_name, mod]: M2->transitively_imported_modules())
            transitively_imported_modules.insert({mod_name, mod});
    }

    show_messages(file, std::cerr, messages);
    exit_on_error(messages);
}

void Module::export_symbol(const const_symbol_ptr& S)
{
    assert(is_qualified_symbol(S->name));

    if (S->symbol_type == symbol_type_t::default_method)
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

// Export a record field label independently of any ordinary selector binding.
void Module::export_field(const FieldInfo& field)
{
    auto uname = get_unqualified_name(field.name);
    auto range = exported_fields_.equal_range(uname);
    for(auto i = range.first; i != range.second; ++i)
        if (i->second.name == field.name and i->second.parent_type == field.parent_type)
            return;

    exported_fields_.insert({uname, field});
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
        assert(value_info->symbol_type == symbol_type_t::constructor or not value_info->type.empty());
    }

    // Currently we just export the local symbols
    if (not module_AST.exports or module_AST.exports->size() == 0)
    {
        export_module(name);
        for(const auto& [_, type]: types)
            if (is_local_qualified_name(type->name))
                for(const auto& field: record_fields_for_type(*type))
                    export_field(field);
    }
    else
    {
        for(auto& [loc,ex]: *module_AST.exports)
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
                        if (t->is_class())
                        {
                            // Export class value children as symbols and associated families as types.
                            // This makes C(..) include associated type and data families.
                            for_each_class_child(*t, [&](const ClassChild& child)
                            {
                                if (child.kind == ClassChildKind::method)
                                {
                                    if (symbol_in_scope_with_name(child.resolved_name, child.name))
                                        export_symbol(lookup_resolved_symbol(child.resolved_name));
                                }
                                else if (type_in_scope_with_name(child.resolved_name, child.name))
                                    export_type(lookup_resolved_type(child.resolved_name));
                            });
                        }
                        else if (auto d = t->is_data())
                        {
                            for(auto& constructor: d->constructors)
                                export_symbol(lookup_symbol(constructor));
                            for(const auto& field: record_fields_for_type(*t))
                            {
                                export_field(field);
                                if (auto selector = lookup_resolved_symbol(field.name))
                                    export_symbol(selector);
                            }
                        }
                        else if (auto d = t->is_data_fam())
                        {
                            for(auto& constructor: d->constructors)
                                export_symbol(lookup_symbol(constructor));
                            for(const auto& field: record_fields_for_type(*t))
                            {
                                export_field(field);
                                if (auto selector = lookup_resolved_symbol(field.name))
                                    export_symbol(selector);
                            }
                        }
                    }
                    else
                    {
                        // all children
                        if (t->is_class())
                        {
                            for(auto& [loc,name]: *ex.subspec->names)
                            {
                                auto child = class_child_named(*t, name);
                                if (not child)
                                    messages.push_back( error(loc, Note()<<"`"<<name<<"` is not a method or associated family for class `"<<id<<"`") );
                                else if (child->kind == ClassChildKind::method)
                                {
                                    if (symbol_in_scope_with_name(child->resolved_name, name))
                                        export_symbol(lookup_resolved_symbol(child->resolved_name));
                                    else
                                        messages.push_back( error(loc, Note()<<"`"<<name<<"` is not in scope") );
                                }
                                else
                                {
                                    if (type_in_scope_with_name(child->resolved_name, name))
                                        export_type(lookup_resolved_type(child->resolved_name));
                                    else
                                        messages.push_back( error(loc, Note()<<"`"<<name<<"` is not in scope") );
                                }
                            }
                        }
                        else if (auto d = t->is_data())
                        {
                            for(auto& [loc,name]: *ex.subspec->names)
                            {
                                auto qualified_name = get_module_name(t->name) + "." + name;
                                if (is_haskell_conid(name) and not contains_constructor(d->constructors, qualified_name))
                                {
                                    messages.push_back( error(loc, Note()<<"`"<<name<<"` is not a constructor for data type `"<<id<<"`"));
                                    continue;
                                }
                                if (is_haskell_varid(name) and not type_has_record_field(*t, name))
                                {
                                    messages.push_back( error(loc, Note()<<"`"<<name<<"` is not a field for data type `"<<id<<"`") );
                                    continue;
                                }

                                if (is_haskell_conid(name))
                                    export_symbol(lookup_symbol(name));
                                else
                                {
                                    for(const auto& field: record_fields_for_type(*t))
                                        if (record_utils::record_field_name_matches(field.name, name))
                                            export_field(field);
                                    if (auto selector = lookup_resolved_symbol(get_module_name(t->name) + "." + name))
                                        export_symbol(selector);
                                }
                            }
                        }
                        else if (auto d = t->is_data_fam())
                        {
                            for(auto& [loc,name]: *ex.subspec->names)
                            {
                                auto qualified_name = get_module_name(t->name) + "." + name;
                                if (is_haskell_conid(name) and not d->constructors.count( qualified_name ))
                                {
                                    messages.push_back( error(loc, Note()<<"`"<<name<<"` is not a constructor for data family `"<<id<<"`"));
                                    continue;
                                }
                                if (is_haskell_varid(name) and not type_has_record_field(*t, name))
                                {
                                    messages.push_back( error(loc, Note()<<"`"<<name<<"` is not a field for data family `"<<id<<"`") );
                                    continue;
                                }

                                if (is_haskell_conid(name))
                                    export_symbol(lookup_symbol(name));
                                else
                                {
                                    for(const auto& field: record_fields_for_type(*t))
                                        if (record_utils::record_field_name_matches(field.name, name))
                                            export_field(field);
                                    if (auto selector = lookup_resolved_symbol(get_module_name(t->name) + "." + name))
                                        export_symbol(selector);
                                }
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
    field_aliases.clear();
    types.clear();
    type_aliases.clear();
    exported_symbols_.clear();
    exported_fields_.clear();
    exported_types_.clear();

    transitively_imported_modules.clear();
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

Core::Decls<> Module::desugar(const simplifier_options& /*opts*/, FreshVarState& state, const Hs::Binds& topdecls)
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
        add_symbol({var_name, symbol_type_t::variable, {}, {}, {}});
        S = lookup_local_symbol(var_name);
        assert(S);
        S->visible = false;
    }

    assert(S);

    return S;
}


void Module::export_small_decls(const inliner_options& options, const Core::Decls<>& decls)
{
    // Determine which decls are loop breakers, and give them empty unfoldings.
    set<Occ::Var> occ_free_vars;
    Core::Binds<> binds;
    if (not decls.empty())
        binds.push_back(Core::Rec<>{decls});
    auto occ_binds = occurrence_analyze_binds(*this, binds, occ_free_vars);
    set<Core::Var<>> loop_breakers;
    for(const auto& bind: occ_binds)
    {
        if (auto nonrec = std::get_if<Occ::NonRec>(&bind))
        {
            if (nonrec->decl.x.info.is_loop_breaker)
                loop_breakers.insert(to_core_var(nonrec->decl.x));
            continue;
        }

        for(const auto& [x,_]: std::get<Occ::Rec>(bind).decls)
            if (x.info.is_loop_breaker)
                loop_breakers.insert(to_core_var(x));
    }

    // FIXME: add a wrapper for EVERY constructor!
    for(auto& [x,rhs]: decls)
    {
        assert(not x.name.empty());
        assert(get_module_name(x.name) == name);

        // Add the unfolding for this variable.
        auto S = lookup_make_local_symbol(x.name);
        S->id_arity = x.id.arity;

        if (loop_breakers.count(x))
        {
            S->unfolding = {};
            assert(not S->unfolding.valueless_by_exception());
        }
        else if (simple_size(rhs) <= 75 and to<std::monostate>(S->unfolding))
        {
            // Label vars with whether they are used or not, and collect free vars.
            auto [occ_rhs, free_vars] = occurrence_analyzer(*this, rhs);

            // The unfolding need to be occurrence analyzed.
            S->unfolding = make_core_unfolding(*this, options, x.id, occ_rhs);

            // Check that we have local symbols for everything that we've put in an unfolding.
            for(auto& y: free_vars)
                if (is_qualified_symbol(y.name) and get_module_name(y.name) == name)
                    lookup_make_local_symbol(y.name);
        }
    }
}

Core::Decls<> Module::optimize(const simplifier_options& opts, FreshVarState& fvstate, Core::Decls<> decls)
{
    if (not opts.optimize) return decls;
    if (decls.empty()) return decls;

    Core::Binds<> core_binds = {Core::Rec<>{std::move(decls)}};

    // Pass: Simplify gently, Static argument

    // Pass: Simplify Gently
    core_binds = simplify_module_gently(opts, fvstate, *this, core_binds);

    // Pass: specialize

    // Pass: Full Laziness (FloatLambdas = 0, FloatConstants)
    if (opts.fully_lazy) float_out_from_module(fvstate, core_binds);

    // Pass: Simplifier*3
    core_binds = simplify_module(opts, fvstate, *this, core_binds);

    // Pass: Float In

    // Pass: Call Arity, Simplify
    core_binds = call_arity_analyze(*this, core_binds);
    core_binds = simplify_module(opts, fvstate, *this, core_binds);

    // Pass: Demand Analysis

    // Pass: Exitification

    // Pass: Full Laziness (Float Lambdas = ??, FloutConstants, FloatOverSatApps, FloatJoins)
    if (opts.fully_lazy) float_out_from_module(fvstate, core_binds);
    // QUESTION!  We need to avoid floating things into case alternatives as well as lambda expressions.
    //            Should this be an option to the simplifier?

    // Pass: CSE -- See note Implementing CSE.
    // Pass: Float In
    // Pass: Simplify (final)

    // NOTE: -O2 passes:
    // Pass: Liberate case, Simplify
    // Pass: SpecConstr, Simplify.
    // Pass: Late Specialize, Simplify.
    // Pass: CSE, Simplify (but only if (liberate_case or spec_constr))
    // End of -O2 passes.

    // Late Demand Analysis, Simplify
    // Late Demand Analysis
    // Pass: add caller ccs?
    // Pass: add late ccs?

    Core::Decls<> result;
    for(auto& bind: core_binds)
    {
        if (auto nonrec = std::get_if<Core::NonRec<>>(&bind))
            result.push_back(std::move(nonrec->decl));
        else
        {
            auto& rec = std::get<Core::Rec<>>(bind);
            result.insert(result.end(), std::make_move_iterator(rec.decls.begin()),
                          std::make_move_iterator(rec.decls.end()));
        }
    }
    return result;
}

/* Note: Implementing CSE
 * (See ghc/compiler/GHC/Core/Opt/CSE.hs)
 *
 * It relies on the ability to map from Core expressions to variables.
 * (See GHC.Core.Map.Expr)
 *
 * To avoid being sensitive to binder names, this map transforms to DeBruijn indices on the fly!
 * (See GHC.Core.Map.Type)
 *
 * Apparently handling recursive binder groups is hard, but for self-recursive bindings like
 *    f = \x -> ...f ...
 * They can store then as \f \x -> ... f ...
 * Then the deBruijn indices allow noting the equivalence.
 * A separate looking environment is used for these.
 *
 * The CSE environment (CSEnv) has three parts:
 * - cs_subst :: in_var -> out_var (or trivial out-expression?)
 *               This substitution is applied before cs_map
 * - cs_map :: out_exp -> out_var (or trivial out-expression?)
 * - cs_rec_map :: out_exp -> out_exp.  This is a separate map for self-recursive bindings.
 */

Core::Exp<> load_builtin(const module_loader& loader, const string& plugin_name, const string& symbol_name, const string& call_conv, int n)
{
    assert(not call_conv.empty());
    auto fn = loader.load_builtin_ptr(plugin_name, symbol_name, call_conv);

    auto args = make_vars<>(n);
    auto args_exp = args | ranges::to<vector<Core::Exp<>>>;

    Core::Exp<> body = Core::BuiltinOp<>(plugin_name, symbol_name, call_conv, args_exp, fn);
    return lambda_quantify(args, body);
}

Core::Exp<> parse_builtin(const Haskell::ForeignDecl& B, int n_args, const module_loader& L)
{
    auto call_conv = unloc(B.call_conv);
    assert(not call_conv.empty());
    return load_builtin(L, B.plugin_name, B.symbol_name, call_conv, n_args);
}

Core::Decls<> Module::load_builtins(const module_loader& L, const std::vector<Hs::ForeignDecl>& foreign_decls)
{
    Core::Decls<> decls;
    for(const auto& decl: foreign_decls)
    {
        if (not decl.foreign_info)
            throw myexception()<<"Foreign import '"<<unloc(decl.function).name
                               <<"' has no checked ABI type";

        const auto& foreign_info = *decl.foreign_info;
        int abi_arity = gen_type_arity(foreign_info.abi_type);
        auto function_name = unloc(decl.function).name;

        Core::Exp<> body;

        if (unloc(decl.call_conv) == "bpcall" or unloc(decl.call_conv) == "ecall")
        {
            auto S = lookup_symbol(function_name);
            
            function_name = S->name;

            if (foreign_info.needs_io_wrapper)
            {
                assert(abi_arity > 0);
                auto builtin = parse_builtin(decl, abi_arity, L);
                auto xs = make_vars<>(abi_arity - 1);
                auto f1 = Core::Var<>("f1");
                auto f2 = Core::Var<>("f2");
                auto makeIO = Core::Var<>("Compiler.IO.makeIO");

                body = Core::Let<>{Core::Rec<>({{f1, builtin},                          // let f1 = builtin
                                                {f2, make_apply(Core::Exp<>(f1),xs)}}), //     f2 = f1 x1 .. xn
                    Core::Apply<>{makeIO, {f2}}};                          // in makeIO f2

                body = lambda_quantify(xs, body);  // \x1 .. xn -> let {f1 = builtin; f2 = f1 x1 .. xn} in makeIO f2
            }
            else
                body = parse_builtin(decl, abi_arity, L);
        }
        else if (unloc(decl.call_conv) == "trcall")
        {
            function_name = unloc(decl.function).name + "$raw";
            body = parse_builtin(decl, abi_arity, L);
        }

        decls.push_back( { Core::Var<>(function_name), body} );
    }

    return decls;
}

string get_constructor_name(const Hs::LType& constr)
{
    auto [con,_] = Hs::decompose_type_apps(constr);
    auto tc = unloc(con).to<Hs::TypeCon>();
    assert(tc);
    return tc->name;
}

Core::Exp<> make_constructor(const std::string& con_name, const DataConInfo& info)
{
    assert(info.field_strictness.size() == info.arity());
    int arity = info.dict_arity() + info.arity();

    auto args = make_vars<>(arity);
    auto args_exp = args | ranges::to<vector<Core::Exp<>>>;

    if (info.is_newtype_constructor)
    {
        assert(info.arity() == 1);
        assert(not args.empty());
        return lambda_quantify(args, Core::Exp<>(args.back()));
    }

    Core::Exp<> body = Core::ConApp<>{con_name, args_exp};

    // Force strict fields
    for(int i = info.arity()-1;i >= 0; i--)
    {
        int j = i + info.dict_arity();
        if (info.field_strictness[i])
            body = Core::Case<>{args[j],{{/*wildcard pat*/{}, body}}};
    }

    return lambda_quantify(args, body);
}


Core::Decls<> Module::load_constructors(const Hs::Decls& topdecls)
{
    Core::Decls<> decls;

    auto load_constructor = [&](const std::string& con_name)
    {
        auto info = lookup_resolved_symbol(con_name)->con_info;
        assert(info);
        auto exp = make_constructor(con_name, *info);
        auto var = Core::Var<>(con_name);

        // Nullary constructors are used as canonical runtime values by let-floating.
        // Keep the wrapper alive even when it is not source-exported.
        if (info->dict_arity() + info->arity() == 0)
            var.is_exported = true;

        decls.push_back( Core::Decl<>{ var, exp} );
    };

    auto load_data_family_instance_constructors = [&](const Hs::DataFamilyInstanceDecl& d)
    {
        if (d.rhs.is_regular_decl())
        {
            for(const auto& constr: d.rhs.get_constructors())
                load_constructor(unloc(*constr.con).name);
        }
        else if (d.rhs.is_gadt_decl())
        {
            for(const auto& cons_decl: d.rhs.get_gadt_constructors())
                for(auto& lcon_name: cons_decl.con_names)
                    load_constructor(unloc(lcon_name));
        }
    };

    for(const auto& [_,decl]: topdecls)
    {
        if (auto d = decl.to<Haskell::DataOrNewtypeDecl>())
        {
            if (d->is_regular_decl())
            {
                for(const auto& constr: d->get_constructors())
                    load_constructor(unloc(*constr.con).name);
            }
            else if (d->is_gadt_decl())
            {
                for(const auto& cons_decl: d->get_gadt_constructors())
                    for(auto& lcon_name: cons_decl.con_names)
                        load_constructor(unloc(lcon_name));
            }
        }
        else if (auto d = decl.to<Haskell::DataFamilyInstanceDecl>())
        {
            load_data_family_instance_constructors(*d);
        }
        else if (auto i = decl.to<Haskell::InstanceDecl>())
        {
            for(auto& d: i->data_inst_decls)
                load_data_family_instance_constructors(d);
        }
    }
    return decls;
}

bool Module::is_refutable_pattern(const Hs::LPat& lpat) const
{
    auto& [loc,pat] = lpat;
    auto constructor_is_refutable = [&](const Hs::LCon& head, const Hs::LPats& args) -> bool
    {
        auto C = lookup_resolved_symbol(unloc(head).name);
        if (not C or not C->con_info)
            return true;

        if (C->con_info and C->con_info->is_newtype_constructor)
        {
            if (args.size() == 1)
                return is_refutable_pattern(args[0]);
            else
                return false;
        }

        // If there's > 1 constructor this this if refutable.
        auto T = lookup_resolved_type(*C->parent);
        assert(T);
        const auto* data_info = T->is_data();
        assert(data_info);
        if (data_info and data_info->constructors.size() >= 2) return true;

        // If any of the argument patterns are refutable, then this is refutable.
        for(auto& arg: args)
            if (is_refutable_pattern(arg)) return true;

        // Otherwise it is irrefutable.
        return false;
    };

    if (auto con_pat = pat.to<Hs::ConPattern>())
    {
        return constructor_is_refutable(con_pat->head, con_pat->args);
    }
    else if (auto rec_pat = pat.to<Hs::RecordPattern>())
    {
        Hs::LPats field_patterns;
        for(auto& field: unloc(rec_pat->fbinds).fields)
            field_patterns.push_back(unloc(field).pattern);
        return constructor_is_refutable(rec_pat->head, field_patterns);
    }
    else if (auto tuple_pat = pat.to<Hs::TuplePattern>())
    {
        // If any of the argument patterns are refutable, then this is refutable.
        for(auto& arg: tuple_pat->elements)
            if (is_refutable_pattern(arg)) return true;

        // Otherwise it is irrefutable.
        return false;
    }
    // Typed patterns are refutable if the underlying pattern is.
    else if (auto typed_pat = pat.to<Hs::TypedPattern>())
    {
        return is_refutable_pattern(typed_pat->pat);
    }
    // Strict patterns are refutable if the underlying pattern is.
    else if (auto strict_pat = pat.to<Hs::StrictPattern>())
    {
        return is_refutable_pattern(strict_pat->pattern);
    }
    // As patterns are refutable if the underlying pattern is.
    else if (auto as_pat = pat.to<Hs::AsPattern>())
    {
        return is_refutable_pattern(as_pat->pattern);
    }
    // Literal patterns are refutable
    else if (pat.is_a<Hs::LiteralPattern>())
    {
        return true;
    }
    // List patterns are refutable
    else if (pat.is_a<Hs::ListPattern>())
    {
        return true;
    }
    // Var patterns are irrefutable
    else if (pat.is_a<Hs::VarPattern>())
    {
        return false;
    }
    // Wildcard patterns are irrefutable
    else if (pat.is_a<Hs::WildcardPattern>())
    {
        return false;
    }
    // Lazy patterns are irrefutable
    else if (pat.is_a<Hs::LazyPattern>())
    {
        return false;
    }
    else
        std::abort();
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
    Core::Exp<> U;
    if (name == "()")
    {
        S = std::make_shared<symbol_info>("()", symbol_type_t::constructor, "()", 0);
        U = Core::ConApp("()",{});
    }
    else if (name == "[]")
    {
        S = std::make_shared<symbol_info>("[]", symbol_type_t::constructor, "[]", 0);
        U = Core::ConApp("[]",{});
    }
    else if (name == ":")
    {
        symbol_info cons(":", symbol_type_t::constructor, "[]", 2, {{right_fix,5}});
        S = std::make_shared<symbol_info>(cons);
        auto args = make_vars<>(2,'l');
        auto args_exp = args | ranges::to<vector<Core::Exp<>>>;
        U = lambda_quantify(args, Core::Exp<>(Core::ConApp(":", args_exp)));
    }
    else if (is_tuple_name(name))
    {
        int arity = name.size() - 1;
        S = std::make_shared<symbol_info>(name, symbol_type_t::constructor, name, arity);
        auto args = make_vars<>(arity,'t');
        auto args_exp = args | ranges::to<vector<Core::Exp<>>>;
        U = lambda_quantify(args, Core::Exp<>(Tuple(args_exp)));
    }
    else
        throw myexception()<<"Symbol 'name' is not a builtin (constructor) symbol.";

    Module empty("Empty");

    auto [occ_U, free_vars] = occurrence_analyzer(empty, U);
    S->unfolding = CoreUnfolding(occ_U, UnfoldWhen());
    assert(free_vars.empty());
    return S;
}

set<string> special_prelude_symbols =
{
    // This is kind of a secret one, used in the desugaring of strings.
    "Foreign.String.unpack_cpp_string",

    // These are all Prelude symbols used in desugaring.
    // Modid.name should be equivalent to Prelude.name.
    "Compiler.Error.error",
    "Data.OldList.filter",
    "Data.OldList.map",
    "Data.OldList.concatMap",
    "Control.Monad.fail",
    "Compiler.Enum.enumFrom",
    "Compiler.Enum.enumFromTo",
    "Compiler.Enum.enumFromThen",
    "Compiler.Enum.enumFromThenTo",
    "Prelude.undefined"
};

bool special_prelude_symbol(const string& name)
{
    return special_prelude_symbols.count(name) > 0;
}

const_symbol_ptr lookup_magic_symbol(const std::string& name)
{
    // See special_prelude_symbols
    symbol_ptr S;

    TypeVar a("a", kind_type());

    if (name == "Compiler.Error.error")
    {
        S = std::make_shared<symbol_info>(symbol_info(name, symbol_type_t::variable, {}, 1));
        // forall a.[Char] -> a
        S->type = add_forall_vars({a}, make_arrow_type(list_type(char_type()),a));
    }
    else if (name == "Foreign.String.unpack_cpp_string")
    {
        S = std::make_shared<symbol_info>(symbol_info(name, symbol_type_t::variable, {}, 1));
        // forall a.[Char] -> a
        S->type = make_arrow_type(TypeCon("CPPString"), list_type(char_type()));
    }
    else if (name == "Data.OldList.concatMap")
    {
        S = std::make_shared<symbol_info>(symbol_info(name, symbol_type_t::variable, {}, 1));
        // forall a b.(a -> [b]) -> [a] -> [b]
        TypeVar b("b", kind_type());
        S->type = add_forall_vars({a}, make_arrow_type(make_arrow_type(a,list_type(b)), make_arrow_type(list_type(a),list_type(b))));
    }
    else if (name == "Data.OldList.map")
    {
        S = std::make_shared<symbol_info>(symbol_info(name, symbol_type_t::variable, {}, 1));
        // forall a b.(a -> b) -> [a] -> [b]
        TypeVar b("b", kind_type());
        S->type = add_forall_vars({a,b}, make_arrow_type(make_arrow_type(a,b), make_arrow_type(list_type(a),list_type(b))));
    }
    else if (name == "Data.OldList.filter")
    {
        S = std::make_shared<symbol_info>(symbol_info(name, symbol_type_t::variable, {}, 1));
        // forall a.(a -> Bool) -> [a] -> [a]
        S->type = add_forall_vars({a}, make_arrow_type(make_arrow_type(a, TypeCon(bool_type_name)), make_arrow_type(list_type(a),list_type(a))));
    }
    else if (name == "Control.Monad.fail")
    {
        S = std::make_shared<symbol_info>(symbol_info(name, symbol_type_t::variable, {}, 1));
        // forall m a.MonadFail m => String -> m a
        TypeVar m("m", kind_type());
        S->type = add_forall_vars({m,a}, ConstrainedType(Context({Type(TypeApp(TypeCon("Monad"),m))}), make_arrow_type(list_type(char_type()), TypeApp(m,a))));
    }
    else if (name == "Compiler.Enum.enumFrom")
    {
        S = std::make_shared<symbol_info>(symbol_info(name, symbol_type_t::variable, {}, 1));
        // forall a.Enum a => a -> [a]
        S->type = add_forall_vars({a}, ConstrainedType(Context({Type(TypeApp(TypeCon("Enum"),a))}), make_arrow_type(a, list_type(a))));
    }
    else if (name == "Compiler.Enum.enumFromTo")
    {
        S = std::make_shared<symbol_info>(symbol_info(name, symbol_type_t::variable, {}, 1));
        // forall a.Enum a => a -> a -> [a]
        S->type = add_forall_vars({a}, ConstrainedType(Context({Type(TypeApp(TypeCon("Enum"),a))}), make_arrow_type(a, make_arrow_type(a, list_type(a)))));
    }
    else if (name == "Compiler.Enum.enumFromThen")
    {
        S = std::make_shared<symbol_info>(symbol_info(name, symbol_type_t::variable, {}, 1));
        // forall a.Enum a => a -> a -> [a]
        S->type = add_forall_vars({a}, ConstrainedType(Context({Type(TypeApp(TypeCon("Enum"),a))}), make_arrow_type(a, make_arrow_type(a, list_type(a)))));
    }
    else if (name == "Compiler.Enum.enumFromThenTo")
    {
        S = std::make_shared<symbol_info>(symbol_info(name, symbol_type_t::variable, {}, 1));
        // forall a.Enum a => a -> a -> a -> [a]
        S->type = add_forall_vars({a}, ConstrainedType(Context({Type(TypeApp(TypeCon("Enum"),a))}), make_arrow_type(a, make_arrow_type(a, make_arrow_type(a, list_type(a))))));
    }
    else if (name == "Prelude.undefined")
    {
        S = std::make_shared<symbol_info>(symbol_info(name, symbol_type_t::variable, {}, 1));
        // forall a.a
        S->type = add_forall_vars({a}, a);
    }

    return S;
}

// Outdated: Holding the symbols here permanently ensures that there is always a
// reference to the VarInfo for these symbols.
std::map<std::string, const_symbol_ptr> builtin_symbols_cache;

const_symbol_ptr lookup_builtin_symbol(const std::string& name)
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

const_type_ptr lookup_builtin_type(const std::string& name)
{
    // Build semantic data metadata for builtin algebraic types.
    auto builtin_data_info = [](const string& type_name, vector<string> constructors, vector<TypeVar> type_vars) {
        auto info = std::make_shared<DataInfo>();
        info->name = type_name;
        info->type_vars = std::move(type_vars);
        info->constructors = constructors;
        return type_info::data_info{std::move(constructors), {}, info};
    };

    // Attach role metadata to builtin type constructors that can appear in Coercible.
    auto with_roles = [](type_info T, vector<Role> roles) {
        T.roles = std::move(roles);
        return const_type_ptr(new type_info(std::move(T)));
    };

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
        return const_type_ptr(new type_info{"()", builtin_data_info("()", {"()"}, {}), {}, 0, kind_type()});
    }
    else if (name == "[]")
        return with_roles(type_info{"[]", builtin_data_info("[]", {"[]",":"}, {TypeVar("a", kind_type())}), {}, 1, make_n_args_kind(1)}, {Role::Representational});
    else if (name == "->")
    {
        return with_roles(type_info{"->", {}, {{right_fix,0}}, 2, make_n_args_kind(2)}, {Role::Representational, Role::Representational});
    }
    else if (name == "~" or name == "~#" or name == "~R#" or name == "~P#")
    {
        return const_type_ptr(new type_info{name, {}, {}, 2, make_n_args_constraint_kind(2)});
    }
    // This doesn't include ()
    else if (is_tuple_name(name))
    {
        int n = tuple_arity(name);
        vector<TypeVar> type_vars;
        for(int i=0; i<n; i++)
            type_vars.push_back(TypeVar("a" + std::to_string(i), kind_type()));

        return with_roles(type_info{name, builtin_data_info(name, {name}, std::move(type_vars)), {}, n, make_n_args_kind(n)}, vector<Role>(n, Role::Representational));
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
    else if (is_qualified_symbol(symbol_name))
    {
        if (auto result = lookup_external_symbol(symbol_name))
            return result;
    }

    // 4. Handle magic symbols
    return lookup_magic_symbol(symbol_name);
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
        info.name = ":";
        TypeVar a("a", kind_type());
        info.uni_tvs = { a };
        info.field_types = { a, list_type(a) };
        info.data_type = list_tycon();
        return info;
    }
    else if (con_name == "[]")
    {
        DataConInfo info;
        info.name = "[]";
        TypeVar a("a", kind_type());
        info.uni_tvs = { a };
        info.data_type = list_tycon();
        return info;
    }
    else if (is_tuple_name(con_name) or con_name == "()")
    {
        DataConInfo info;
        info.name = con_name;
        int n = tuple_arity(con_name);
        for(int i=0;i<n;i++)
        {
            TypeVar tv("a"+std::to_string(i+1), kind_type());
            info.uni_tvs.push_back( tv );
            info.field_types.push_back( tv );
        }
        info.data_type = tuple_tycon(n);
        return info;
    }

    auto C = lookup_resolved_symbol(con_name);

    if (not C or not C->con_info) return {};

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

namespace
{
    using record_utils::gadt_constructor_field_names;
    using record_utils::record_field_name_matches;

    std::vector<FieldInfo> record_fields_for_type_info(const type_info& type)
    {
        std::vector<FieldInfo> fields;
        if (auto data = type.is_data())
        {
            for(const auto& [_, field]: data->field_info)
                fields.push_back(field);
        }
        else if (auto data_fam = type.is_data_fam())
        {
            for(const auto& [_, field]: data_fam->field_info)
                fields.push_back(field);
        }
        return fields;
    }

    FieldSelectorStatus merge_field_selector_status(FieldSelectorStatus status1, FieldSelectorStatus status2)
    {
        if (status1 == FieldSelectorStatus::SourceHidden or status2 == FieldSelectorStatus::SourceHidden)
            return FieldSelectorStatus::SourceHidden;
        return FieldSelectorStatus::SourceVisible;
    }

    // Merge duplicate field metadata from local and imported sources by field/type identity.
    void merge_record_field_candidate(std::map<std::pair<std::string,std::string>, FieldInfo>& candidates, const FieldInfo& field_info)
    {
        auto key = std::pair{field_info.name, field_info.parent_type};
        auto [iter, inserted] = candidates.insert({key, field_info});
        if (inserted)
            return;

        iter->second.selector_status = merge_field_selector_status(iter->second.selector_status, field_info.selector_status);
        for(int i=0; i<field_info.constructors.size(); i++)
        {
            const auto& constructor = field_info.constructors[i];
            if (std::ranges::find(iter->second.constructors, constructor) != iter->second.constructors.end())
                continue;

            iter->second.constructors.push_back(constructor);
            iter->second.positions.push_back(field_info.positions[i]);
        }
    }

    // Add local field metadata entries that match an already resolved field name.
    void add_record_field_candidates(std::map<std::pair<std::string,std::string>, FieldInfo>& candidates, const std::map<std::string, type_ptr>& types, const std::string& resolved_field_name)
    {
        for(const auto& [_, type]: types)
            for(const auto& field: record_fields_for_type_info(*type))
                if (field.name == resolved_field_name)
                    merge_record_field_candidate(candidates, field);
    }

    // Add local field metadata entries that can be denoted by a source field name.
    void add_record_field_candidates_matching(std::map<std::pair<std::string,std::string>, FieldInfo>& candidates, const std::map<std::string, type_ptr>& types, const std::string& field_name)
    {
        for(const auto& [_, type]: types)
            for(const auto& field: record_fields_for_type_info(*type))
                if (record_field_name_matches(field.name, field_name))
                    merge_record_field_candidate(candidates, field);
    }

    // Add pre-collected field metadata entries to a candidate map.
    void add_record_field_candidates(std::map<std::pair<std::string,std::string>, FieldInfo>& candidates, const std::vector<FieldInfo>& fields)
    {
        for(const auto& field: fields)
            merge_record_field_candidate(candidates, field);
    }

    // Reconstruct the declared field order for a resolved record constructor from FieldInfo.
    void add_record_field_names_for_constructor(std::vector<std::optional<std::string>>& field_names, const std::vector<FieldInfo>& fields, const std::string& constructor_name)
    {
        for(const auto& field: fields)
        {
            for(int i=0; i<field.constructors.size(); i++)
            {
                if (field.constructors[i] != constructor_name)
                    continue;

                auto position = field.positions[i];
                if (position < 0)
                    continue;

                if (field_names.size() <= position)
                    field_names.resize(position + 1);
                field_names[position] = field.name;
            }
        }
    }

    // Reconstruct constructor field order from local type metadata.
    std::optional<std::vector<std::string>> record_field_names_for_constructor(const std::map<std::string, type_ptr>& types, const std::string& constructor_name)
    {
        std::vector<std::optional<std::string>> field_names;
        for(const auto& [_, type]: types)
            add_record_field_names_for_constructor(field_names, record_fields_for_type_info(*type), constructor_name);

        if (field_names.empty())
            return {};

        std::vector<std::string> names;
        for(const auto& name: field_names)
            if (name)
                names.push_back(*name);
        return names;
    }

    // Collect local and imported field metadata candidates for a resolved field name.
    template<class ImportedModules>
    std::vector<FieldInfo> record_field_candidates_for_resolved_name(
        const std::map<std::string, type_ptr>& types,
        const ImportedModules& imported_modules,
        const std::string& field_name)
    {
        std::map<std::pair<std::string,std::string>, FieldInfo> candidates;
        add_record_field_candidates(candidates, types, field_name);
        for(const auto& [_, mod]: imported_modules)
        {
            auto imported_fields = mod->record_field_candidates_for_resolved_name(field_name);
            add_record_field_candidates(candidates, imported_fields);
        }

        std::vector<FieldInfo> fields;
        for(auto& [_, field]: candidates)
            fields.push_back(field);
        return fields;
    }

    // Find constructor field order locally first, then through imported modules.
    template<class ImportedModules>
    std::optional<std::vector<std::string>> record_field_names_for_constructor(
        const std::map<std::string, type_ptr>& types,
        const ImportedModules& imported_modules,
        const std::string& constructor_name)
    {
        auto fields = record_field_names_for_constructor(types, constructor_name);
        if (fields)
            return fields;

        for(const auto& [_, mod]: imported_modules)
        {
            fields = mod->record_field_names_for_constructor(constructor_name);
            if (fields)
                return fields;
        }

        return {};
    }

}

std::vector<FieldInfo> Module::record_fields_for_type(const type_info& type) const
{
    return record_fields_for_type_info(type);
}

bool Module::type_has_record_field(const type_info& type, const std::string& field_name) const
{
    for(const auto& field: record_fields_for_type(type))
        if (record_field_name_matches(field.name, field_name))
            return true;
    return false;
}

std::map<std::string, FieldInfo> Module::local_synthesizable_record_fields() const
{
    std::map<std::string, std::map<std::pair<std::string,std::string>, FieldInfo>> fields_by_label;
    for(const auto& [type_name, type]: types)
    {
        if (not is_local_qualified_name(type_name))
            continue;

        if (auto data = type->is_data())
        {
            for(const auto& [_, field]: data->field_info)
                if (source_visible_selector(field))
                    merge_record_field_candidate(fields_by_label[field.name], field);
        }
        else if (auto data_fam = type->is_data_fam())
        {
            // Field selectors are currently ordinary value bindings.  When
            // shared family metadata would require overloaded selectors, keep
            // concrete data-instance metadata and use family metadata only as
            // a fallback until record selectors get a richer representation.
            for(const auto& [_, field]: data_fam->field_info)
                if (source_visible_selector(field))
                    merge_record_field_candidate(fields_by_label[field.name], field);
        }
    }

    std::map<std::string, FieldInfo> fields;
    for(auto& [field_name, fields_for_label]: fields_by_label)
        if (fields_for_label.size() == 1)
            fields.emplace(field_name, fields_for_label.begin()->second);
    return fields;
}

std::vector<symbol_ptr> Module::local_record_selectors()
{
    // Return local selector symbols whose callability is computed after constructor typechecking.
    std::vector<symbol_ptr> selectors;
    for(auto& [_, symbol]: symbols)
        if (symbol->record_selector)
            selectors.push_back(symbol);
    return selectors;
}

std::vector<FieldInfo> Module::lookup_record_field_candidates(const std::string& field_name) const
{
    std::map<std::pair<std::string,std::string>, FieldInfo> candidates;
    add_record_field_candidates_matching(candidates, types, field_name);
    auto range = field_aliases.equal_range(field_name);
    for(auto i = range.first; i != range.second; ++i)
        merge_record_field_candidate(candidates, i->second);

    try
    {
        auto S = lookup_symbol(field_name);
        add_record_field_candidates(candidates, types, S->name);
        for(const auto& [_, mod]: transitively_imported_modules)
            add_record_field_candidates(candidates, mod->record_field_candidates_for_resolved_name(S->name));
    }
    catch (myexception&)
    {
        if (candidates.empty())
            throw;
    }

    std::vector<FieldInfo> fields;
    for(auto& [_, field]: candidates)
        fields.push_back(field);
    return fields;
}

std::vector<FieldInfo> Module::record_field_candidates_for_resolved_name(const std::string& field_name) const
{
    return ::record_field_candidates_for_resolved_name(types, transitively_imported_modules, field_name);
}

std::optional<std::vector<std::string>> Module::record_field_names_for_constructor(const std::string& constructor_name) const
{
    return ::record_field_names_for_constructor(types, transitively_imported_modules, constructor_name);
}

std::vector<FieldInfo> CompiledModule::lookup_record_field_candidates(const std::string& field_name) const
{
    std::map<std::pair<std::string,std::string>, FieldInfo> candidates;
    auto range = field_aliases.equal_range(field_name);
    for(auto i = range.first; i != range.second; ++i)
        merge_record_field_candidate(candidates, i->second);

    try
    {
        auto S = lookup_symbol(field_name);
        add_record_field_candidates(candidates, types, S->name);
        for(const auto& [_, mod]: transitively_imported_modules_)
            add_record_field_candidates(candidates, mod->record_field_candidates_for_resolved_name(S->name));
    }
    catch (myexception&)
    {
        if (candidates.empty())
            throw;
    }

    std::vector<FieldInfo> fields;
    for(auto& [_, field]: candidates)
        fields.push_back(field);
    return fields;
}

std::vector<FieldInfo> CompiledModule::record_field_candidates_for_resolved_name(const std::string& field_name) const
{
    return ::record_field_candidates_for_resolved_name(types, transitively_imported_modules_, field_name);
}

std::optional<std::vector<std::string>> CompiledModule::record_field_names_for_constructor(const std::string& constructor_name) const
{
    return ::record_field_names_for_constructor(types, transitively_imported_modules_, constructor_name);
}

// Here we do only phase 1 -- we only parse the decls enough to
//   determine which is a variable, and which are e.g. constructors.
// We can't determine function bodies at all, since we can't even handle op definitions
//   before we know fixities!

void Module::def_function(const std::string& fname)
{
    if (is_qualified_symbol(fname))
        throw myexception()<<"Locally defined symbol '"<<fname<<"' should not be qualified in function declaration.";

    declare_symbol({fname, symbol_type_t::variable, {}, {}, {}});
}

void Module::def_constructor(const string& cname, int arity, const string& type_name)
{
    if (is_qualified_symbol(cname))
        throw myexception()<<"Locally defined symbol '"<<cname<<"' should not be qualified.";

//    if (not is_qualified_symbol(type_name))
//        throw myexception()<<"Locally defined symbol '"<<type_name<<"' should not be qualified.";

    auto S = symbol_info(cname, symbol_type_t::constructor, qualify_local_name(type_name), arity, {});
    S.id_arity = arity;
    declare_symbol( S );
}

void Module::def_ADT(const std::string& tname, int arity, const type_info::data_info& info)
{
    if (is_qualified_symbol(tname))
        throw myexception()<<"Locally defined type '"<<tname<<"' should not be qualified.";

    declare_type( {tname, info, {}, arity, /*kind*/ {}} );
}

void Module::def_ADT(const std::string& tname, int arity, const fixity_info& fixity, const type_info::data_info& info)
{
    if (is_qualified_symbol(tname))
        throw myexception()<<"Locally defined symbol '"<<tname<<"' should not be qualified.";

    declare_type( {tname, info, fixity, arity, /*kind*/ {}} );
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

void Module::def_data_family(const std::string& fname, int arity)
{
    if (is_qualified_symbol(fname))
        throw myexception()<<"Locally defined type '"<<fname<<"' should not be qualified.";

    declare_type( {fname, type_info::data_fam_info(), {}, arity, /*kind*/ {}} );
}

void Module::def_type_class(const std::string& cname, int arity, const type_info::class_info& info)
{
    if (is_qualified_symbol(cname))
        throw myexception()<<"Locally defined type '"<<cname<<"' should not be qualified.";

    declare_type( {cname, info, {}, arity, /*kind*/ {}} );
}

void Module::def_type_class_method(const string& method_name, const string& class_name)
{
    if (is_qualified_symbol(method_name))
        throw myexception()<<"Locally defined type class method '"<<method_name<<"' should not be qualified.";

//    if (not is_qualified_symbol(class_name))
//        throw myexception()<<"Locally defined type class '"<<class_name<<"' should be qualified.";

    declare_symbol( {method_name, symbol_type_t::class_method, class_name, {}, {}} );
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
        if (S->symbol_type == symbol_type_t::unknown)
            S->symbol_type = symbol_type_t::variable;
    }
    else
        def_function(var_name);
}

void Module::mark_record_selector(const FieldInfo& field)
{
    auto selector_name = get_unqualified_name(field.name);
    auto qualified_selector_name = qualify_local_name(selector_name);
    auto iter = symbols.find(qualified_selector_name);

    if (iter == symbols.end())
        throw myexception()<<"Could not mark record selector '"<<selector_name<<"' because it is not declared.";

    auto& selector = *iter->second;
    if (selector.symbol_type != symbol_type_t::variable)
        throw myexception()<<"Could not mark record selector '"<<selector_name<<"' because it is not a value symbol.";

    selector.record_selector = RecordSelectorInfo{field.name, field.parent_type};
}

void Module::def_record_selector(const FieldInfo& field)
{
    // Declare the selector name early while leaving its body to be generated after renaming.
    def_function(get_unqualified_name(field.name));
    mark_record_selector(field);
}

void Module::add_local_symbols(const Hs::Decls& topdecls)
{
    int next_data_family_instance_id = 0;
    auto record_field_owner = [&](const string& field_name) -> optional<string>
    {
        for(const auto& [_, type]: types)
            for(const auto& field_info: record_fields_for_type_info(*type))
                if (record_field_name_matches(field_info.name, field_name))
                    return field_info.parent_type;
        return {};
    };

    auto record_field_name = [&](const string& field_name, const string& owner_name)
    {
        auto qualified_owner_name = qualify_local_name(owner_name);
        if (auto existing_owner = record_field_owner(field_name))
            if (*existing_owner != qualified_owner_name)
            {
                if (not language_extensions.has_extension(LangExt::DuplicateRecordFields))
                    throw myexception()<<"Record field '"<<field_name<<"' is declared for both data type '"<<get_unqualified_name(*existing_owner)<<"' and data type '"<<get_unqualified_name(qualified_owner_name)<<"'. Enable DuplicateRecordFields to allow duplicate record labels.";
            }
    };

    auto record_field_info = [&](auto& fields, const string& field_name, const string& owner_name, const string& constructor_name, int position)
    {
        auto qualified_field_name = qualify_local_name(field_name);
        auto& field = fields[qualified_field_name];
        field.name = qualified_field_name;
        field.parent_type = qualify_local_name(owner_name);
        field.selector_status = language_extensions.has_extension(LangExt::FieldSelectors)
            ? FieldSelectorStatus::SourceVisible
            : FieldSelectorStatus::SourceHidden;
        field.constructors.push_back(qualify_local_name(constructor_name));
        field.positions.push_back(position);
    };

    auto add_data_family_instance_symbols = [&](const Hs::DataFamilyInstanceDecl& data_inst)
    {
        auto family_name = unloc(data_inst.con).name;
        auto family_type = lookup_local_type(qualify_local_name(family_name));
        auto data_fam = family_type ? family_type->is_data_fam() : nullptr;
        auto instance_type_name = "$data-family-instance$" + family_name + "$" + std::to_string(next_data_family_instance_id++);
        type_info::data_info instance_info;

        auto add_constructor = [&](const string& cname, int arity)
        {
            instance_info.constructors.push_back(qualify_local_name(cname));
            if (data_fam)
                data_fam->constructors.insert(qualify_local_name(cname));
            def_constructor(cname, arity, instance_type_name);
        };

        if (data_inst.rhs.is_regular_decl())
        {
            for(const auto& constr: data_inst.rhs.get_constructors())
            {
                auto cname = unloc(*constr.con).name;
                add_constructor(cname, constr.arity());

                if (auto fields = to<Hs::FieldDecls>(constr.fields))
                {
                    int field_index = 0;
                    for(auto& field_decl: fields->field_decls)
                        for(auto& [loc,var]: field_decl.field_names)
                        {
                            record_field_name(var.name, family_name);
                            record_field_info(instance_info.field_info, var.name, family_name, cname, field_index);
                            if (data_fam)
                                record_field_info(data_fam->field_info, var.name, family_name, cname, field_index);
                            field_index++;
                        }
                }
            }
        }
        else if (data_inst.rhs.is_gadt_decl())
        {
            for(const auto& cons_decl: data_inst.rhs.get_gadt_constructors())
            {
                auto field_names = gadt_constructor_field_names(cons_decl.type);
                if (field_names)
                {
                    int field_index = 0;
                    for(const auto& field_name: *field_names)
                    {
                        record_field_name(field_name, family_name);
                        for(const auto& con_name: cons_decl.con_names)
                        {
                            auto cname = unloc(con_name);
                            record_field_info(instance_info.field_info, field_name, family_name, cname, field_index);
                            if (data_fam)
                                record_field_info(data_fam->field_info, field_name, family_name, cname, field_index);
                        }
                        field_index++;
                    }
                }

                for(auto& con_name: cons_decl.con_names)
                {
                    int arity = Hs::gen_type_arity(expand_constructor_record_fields(cons_decl.type));
                    auto cname = unloc(con_name);
                    add_constructor(cname, arity);
                }
            }
        }

        add_type({qualify_local_name(instance_type_name), instance_info, {}, (int)instance_info.constructors.size(), /*kind*/ {}});
    };

    // 0. Get names that are being declared.
    for(const auto& [_,decl]: topdecls)
    {
        if (auto pd = decl.to<Hs::PatDecl>())
        {
            for(const auto& var: Hs::vars_in_pattern( pd->lhs ))
                maybe_def_function( unloc(var).name );
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
            std::optional<int> arity;
            if (data_decl->is_regular_decl())
            {
                arity = data_decl->get_constructors().size();
                for(const auto& constr: data_decl->get_constructors())
                {
                    auto cname = unloc(*constr.con).name;
                    def_constructor(cname, constr.arity(), unloc(data_decl->con).name);
                    info.constructors.push_back( qualify_local_name(cname) );
                    if (auto fields = to<Hs::FieldDecls>(constr.fields))
                    {
                        int field_index = 0;
                        for(auto& field_decl: fields->field_decls)
                            for(auto& [loc,var]: field_decl.field_names)
                            {
                                record_field_name(var.name, unloc(data_decl->con).name);
                                record_field_info(info.field_info, var.name, unloc(data_decl->con).name, cname, field_index);
                                field_index++;
                            }
                    }
                }
            }
            else if (data_decl->is_gadt_decl())
            {
                arity = data_decl->get_gadt_constructors().size();
                for(const auto& cons_decl: data_decl->get_gadt_constructors())
                {
                    auto field_names = gadt_constructor_field_names(cons_decl.type);
                    if (field_names)
                    {
                        int field_index = 0;
                        for(const auto& field_name: *field_names)
                        {
                            record_field_name(field_name, unloc(data_decl->con).name);
                            for(auto& con_name: cons_decl.con_names)
                                record_field_info(info.field_info, field_name, unloc(data_decl->con).name, unloc(con_name), field_index);
                            field_index++;
                        }
                    }

                    for(auto& con_name: cons_decl.con_names)
                    {
                        int arity = Hs::gen_type_arity(expand_constructor_record_fields(cons_decl.type));
                        auto cname = unloc(con_name);
                        def_constructor(cname, arity, unloc(data_decl->con).name);
                        info.constructors.push_back( qualify_local_name(cname) );
                    }
                }
            }

            def_ADT(unloc(data_decl->con).name, *arity, info);
        }
        else if (auto Class = decl.to<Haskell::ClassDecl>())
        {
            type_info::class_info info;

            for(auto& fam_decl: Class->fam_decls)
		if (fam_decl.is_type_family())
		    def_type_family( unloc(fam_decl.con).name, fam_decl.arity() );
		else
		    def_data_family( unloc(fam_decl.con).name, fam_decl.arity() );

            for(auto& sig_decl: Class->sig_decls)
                for(auto& v: sig_decl.vars)
                {
                    def_type_class_method(unloc(v).name, unloc(Class->con).name);
                    info.methods.insert(unloc(v).name);
                }

            def_type_class(unloc(Class->con).name, Class->type_vars.size(), info);
        }
        else if (auto S = decl.to<Haskell::TypeSynonymDecl>())
        {
            def_type_synonym(unloc(S->con).name, S->arity());
        }
        else if (auto TF = decl.to<Hs::FamilyDecl>())
        {
	    if (TF->is_type_family())
		def_type_family( unloc(TF->con).name, TF->arity() );
	    else
		def_data_family( unloc(TF->con).name, TF->arity() );
        }
        else if (auto data_inst = decl.to<Hs::DataFamilyInstanceDecl>())
        {
            add_data_family_instance_symbols(*data_inst);
        }
        else if (auto inst = decl.to<Hs::InstanceDecl>())
        {
            for(auto& data_inst: inst->data_inst_decls)
                add_data_family_instance_symbols(data_inst);
        }
    }
}

#if defined(__ELF__) && defined(__linux__)
namespace
{
    struct ExecutableBuildId
    {
        vector<unsigned char> bytes;
    };

    // Copy the GNU build ID from the main executable's loaded ELF note.
    int find_executable_build_id(dl_phdr_info* info, size_t, void* data)
    {
        if (info->dlpi_name and info->dlpi_name[0] != '\0') return 0;

        auto& result = *static_cast<ExecutableBuildId*>(data);
        for(ElfW(Half) i = 0; i < info->dlpi_phnum; ++i)
        {
            const auto& header = info->dlpi_phdr[i];
            if (header.p_type != PT_NOTE) continue;

            auto note = reinterpret_cast<const unsigned char*>(info->dlpi_addr + header.p_vaddr);
            size_t offset = 0;
            while(offset + sizeof(ElfW(Nhdr)) <= header.p_filesz)
            {
                auto note_header = reinterpret_cast<const ElfW(Nhdr)*>(note + offset);
                offset += sizeof(ElfW(Nhdr));

                auto name_size = (static_cast<size_t>(note_header->n_namesz) + 3) & ~size_t(3);
                auto description_size = (static_cast<size_t>(note_header->n_descsz) + 3) & ~size_t(3);
                if (name_size > header.p_filesz - offset) break;

                auto name = note + offset;
                offset += name_size;
                if (description_size > header.p_filesz - offset) break;

                auto description = note + offset;
                offset += description_size;
                if (note_header->n_type == NT_GNU_BUILD_ID and
                    note_header->n_namesz == 4 and
                    std::memcmp(name, "GNU", 4) == 0)
                {
                    result.bytes.assign(description,
                                        description + note_header->n_descsz);
                    return 1;
                }
            }
        }
        return 0;
    }

    // Hash the linker's executable build identity down to the cache key size.
    optional<string> executable_build_id_hash()
    {
        ExecutableBuildId build_id;
        dl_iterate_phdr(find_executable_build_id, &build_id);
        if (build_id.bytes.empty()) return {};

        return xxhash_to_hex(XXH3_64bits(build_id.bytes.data(),
                                        build_id.bytes.size()));
    }
}
#endif

const string& module_loader::executable_hash()
{
    static optional<string> str;
    if (not str)
    {
#if defined(__ELF__) && defined(__linux__)
        str = executable_build_id_hash();
#endif
    }

    if (not str)
    {
        // NOTE: Platforms without a native build-ID reader hash the executable
        // in full.  Replace this fallback when their exact native ID is exposed.
        auto exe_path = find_exe_path();
        std::ifstream exe_file(exe_path, std::ios::binary);

        XXH3_state_t* state = XXH3_createState();
        XXH3_64bits_reset(state);

        vector<char> buffer(1024 * 1024);
        while (exe_file)
        {
            exe_file.read(buffer.data(), buffer.size());
            std::streamsize n = exe_file.gcount();
            XXH3_64bits_update(state, buffer.data(), n);
        }
        uint64_t hash = XXH3_64bits_digest(state);
        str = xxhash_to_hex(hash);

        XXH3_freeState(state);
    }

    return *str;
}

std::string Module::all_inputs_hash(const Program& P) const
{
    if (not _cached_hash)
    {
        XXH3_state_t* state = XXH3_createState();
        XXH3_64bits_reset(state);

        auto exe = module_loader::executable_hash();
        XXH3_64bits_update(state, exe.data(), exe.size());
        XXH3_64bits_update(state, file.contents.data(), file.contents.size());

	for(auto& dep_modid: dependencies())
	{
	    auto M2 = P.get_module( dep_modid );
            auto input_hash = M2->all_inputs_hash();
            XXH3_64bits_update(state, input_hash.data(), input_hash.size());
	}
        uint64_t hash = XXH3_64bits_digest(state);
	_cached_hash = xxhash_to_hex(hash);

        XXH3_freeState(state);
    }

    return _cached_hash.value();
}


// A name of "" means that we are defining a top-level program, or a piece of a top-level program.
Module::Module(const string& n)
    :fresh_var_state_(std::make_shared<FreshVarState>()),
     name(n)
{
    if (not name.size())
        throw myexception()<<"Module name may not be empty!";
}

Module::Module(const char *n)
    :Module(string(n))
{ }

Module::Module(const Haskell::Module& M, const LanguageExtensions& le, const FileContents& f)
    :fresh_var_state_(std::make_shared<FreshVarState>()),
     language_extensions(le),
     module_AST(M),
     name(unloc(module_AST.modid)),
     file(f)
{
    if (not name.size())
        throw myexception()<<"Module name may not be empty!";
}

std::ostream& operator<<(std::ostream& o, const Module& M)
{
    if (M.module_AST.topdecls)
        for(const auto& decls: *M.module_AST.topdecls)
            o<<decls.print()<<"\n";

    return o;
}

map<Core::Var<>,Runtime::Exp> CompiledModule::prepared_code_defs() const
{
    map<Core::Var<>,Runtime::Exp> code;

    for(const auto& [x,rhs]: prepared_value_decls)
    {
        assert(is_qualified_symbol(x.name));

        if (name() == get_module_name(x.name))
        {
            assert(rhs);
            Runtime::check_no_reg_refs(rhs);

            code[x] = rhs;
        }
    }

    return code;
}

void CompiledModule::finish_value_decls( const Core::Decls<>& decls )
{
    prepared_value_decls.clear();

    FreshVarSource source(*fresh_var_state_);
    for(const auto& [x,rhs]: decls)
    {
        prepared_value_decls[x] = prepare_for_translation(source, rhs);
        Runtime::check_no_reg_refs(prepared_value_decls[x]);
    }
}

const_symbol_ptr CompiledModule::lookup_local_symbol(const std::string& symbol_name) const
{
    assert( get_module_name(symbol_name) == name());
    auto iter = symbols.find(symbol_name);
    if (iter == symbols.end())
        return nullptr;
    else
        return iter->second;
}

const_type_ptr CompiledModule::lookup_local_type(const std::string& type_name) const
{
    assert( get_module_name(type_name) == name());
    auto iter = types.find(type_name);
    if (iter == types.end())
        return nullptr;
    else
        return iter->second;
}

const_symbol_ptr CompiledModule::lookup_symbol(const std::string& name) const
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

void CompiledModule::clear_symbol_table()
{
    dependencies_.clear();

    // modid = keep

    symbols.clear();

    types.clear();

    exported_symbols_.clear();

    exported_fields_.clear();

    exported_types_.clear();

    // language_extensions_ = keep

    local_instances_.clear();

    local_eq_instances_.clear();

    // all_inputs_sha_ = keep

    transitively_imported_modules_.clear();
}

void CompiledModule::inflate(const Program& P)
{
    for(auto& dep_modid: dependencies())
    {
	auto M2 = P.get_module(dep_modid);
	transitively_imported_modules_.insert({M2->name(), M2});
	for(auto& [mod_name, mod]: M2->transitively_imported_modules())
	    transitively_imported_modules_.insert({mod_name, mod});
    }
}

CompiledModule::CompiledModule(const std::shared_ptr<Module>& m)
    :modid(m->name),
     dependencies_(m->dependencies()),
     fresh_var_state_(m->fresh_var_state_ptr())
{
    assert(fresh_var_state_);

    std::swap(symbols, m->symbols);

    std::swap(types, m->types);

    std::swap(exported_symbols_, m->exported_symbols_);

    std::swap(exported_fields_, m->exported_fields_);

    std::swap(exported_types_, m->exported_types_);

    std::swap(aliases, m->aliases);

    std::swap(field_aliases, m->field_aliases);

    std::swap(type_aliases, m->type_aliases);

    // Why can't we clear the language extensions on m?
    language_extensions_ = m->language_extensions;

    std::swap(local_instances_, m->local_instances);

    std::swap(local_eq_instances_, m->local_eq_instances);

    if (m->_cached_hash)
	all_inputs_hash_ = m->_cached_hash.value();
    else
	throw myexception()<<"Module "<<m->name<<" has no integrity hash!";

    m->clear_symbol_table();
}
