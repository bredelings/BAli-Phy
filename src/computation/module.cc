#include <set>
#include <regex>
#include <tuple>
#include <fstream>
#include "computation/module.H"
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
#include "expression/convert.H" // for load_builtins( )
#include "computation/optimization/simplifier.H"
#include "computation/optimization/occurrence.H"
#include "computation/optimization/float-out.H"
#include "computation/optimization/inliner.H"
#include "computation/haskell/haskell.H"
#include "computation/haskell/ids.H"
#include "computation/core/func.H"
#include "util/assert.hh"
#include <fmt/chrono.h>

#include <cereal/archives/binary.hpp>

#include <xxhash.h>

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
    auto& m2_exported_types = M2->exported_types();

    // import modid
    if (not impdecl.impspec)
    {
        for(const auto& [_,S]: m2_exported_values)
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
                    else if (auto c = type->is_class())
                    {
                        if (not s.subspec->names)
                        {
                            for(auto& method: c->methods)
                                import_symbol( m2_exported_values.at(method), modid, qualified );
                        }
                        else
                        {
                            for(auto& [loc,name]: *s.subspec->names)
                            {
                                if (not c->methods.count(name))
                                    messages.push_back( error(loc, Note()<<"`"<<name<<"` is not a method for class `"<<id<<"`") );
                                else
                                    import_symbol( m2_exported_values.at(name), modid, qualified );
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
                            for(auto& field: d->fields)
			    {
				auto name = get_unqualified_name(field);
				if (m2_exported_values.contains(name))
				    import_symbol(m2_exported_values.at( name ), modid, qualified);
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

				if (not m2_exported_values.contains(name))
				{
                                    messages.push_back( error(loc, Note()<<"`"<<name<<"` was not exported"));
                                    continue;
				}
				    
                                import_symbol(m2_exported_values.at(name), modid, qualified);
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
        for(const auto& [_,S]: m2_exported_values)
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

set<string> Module::dependencies() const
{
    set<string> modules;
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

std::string extract_xxhash(std::string& data)
{
    // 1. Check that we have 40 chars followed by a newline.
    if (data[16] != '\n') throw myexception()<<"archive failed integrity check: failed to read stored integrity hash";

    // 2. Get the 40 chars and check that they are all hex digits.
    string stored_archive_sha = data.substr(0,16);
    stored_archive_sha.resize(16);
    for(char c: stored_archive_sha)
	if (not std::isxdigit(c)) throw myexception()<<"archive failed integrity check: failed to read stored integrity hash";

    // 3. Drop the first 17 chars -- 16 hex digits plus newline.
    data = data.substr(17);

    return stored_archive_sha;
}

std::shared_ptr<CompiledModule> read_cached_module(const module_loader& loader, const std::string& modid, const std::string& required_xxhash)
{
    if (loader.recompile_all or loader.recompile_modules.count(modid)) return {};

    if (auto path = loader.find_cached_module(modid))
    {
        try
        {
            std::ifstream infile(*path, std::ios::binary);
            std::string data = std::string(std::istreambuf_iterator<char>(infile), std::istreambuf_iterator<char>());
            string stored_archive_xxhash = extract_xxhash(data);

            string computed_archive_xxhash = xxhash64_hex(data);
            if (log_verbose >= 4)
                std::cerr<<"    Read archive for "<<modid<<":    length = "<<data.size()<<"    stored_archive_hash = "<<stored_archive_xxhash<<"     computed_archive_hash = "<<computed_archive_xxhash<<"\n";

            if (stored_archive_xxhash != computed_archive_xxhash)
                throw myexception()<<"archive failed integrity check: stored and computed archive integrity hash did not match.";

            std::istringstream data2(data);
            cereal::BinaryInputArchive archive( data2 );

            std::shared_ptr<CompiledModule> M;

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
                archive(CM->all_inputs_hash());
                archive(CM);
            }
            string data = buffer.str();
            string archive_hash = xxhash64_hex(data);

            if (log_verbose >= 4)
                std::cerr<<"    Writing archive for "<<modid<<":    length = "<<data.size()<<"    hash = "<<archive_hash<<"\n";

            // Create parent directories if needed.
            fs::create_directories(mod_path->parent_path());

            // Create and open the temporary file.
            std::ofstream tmp_file(tmp_path, std::ios::binary | std::ios::trunc);

            if (not tmp_file) throw myexception()<<"Could not open file!";

            // Write the archive to the temporary file.
            tmp_file<<archive_hash<<"\n";
            tmp_file.write(data.c_str(),data.size());
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

void mark_exported_decls(Core2::Decls<>& decls,
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

std::optional<Core2::Var<>> find_first_duplicate_var(const Core2::Decls<>& decls)
{
    set<Core2::Var<>> vars;
    for(auto& [x,_]: decls)
    {
	if (vars.count(x))
	    return x;
	else
            vars.insert(x);
    }
    return {};
}

void check_duplicate_var(const Core2::Decls<>& decls)
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

set<Core2::Var<>> vars_in_pattern(const Core2::Pattern<>& p)
{
    set<Core2::Var<>> vars;
    for(auto& arg: p.args)
        vars.insert(arg);
    return vars;
}

Core2::Var<> rename_var(const Core2::Var<>& x, const map<Core2::Var<>,Core2::Var<>>& substitution, multiset<Core2::Var<>>& bound)
{
    // 1.1 If there's a substitution x -> E
    if (not bound.count(x) and substitution.count(x))
        return substitution.at(x);
    else
        return x;
}

Core2::Exp<> rename(const Core2::Exp<>& E, const map<Core2::Var<>,Core2::Var<>>& substitution, multiset<Core2::Var<>>& bound)
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

        return Core2::Lambda<>{L->x,body};
    }
    // 3. Apply
    else if (auto A = E.to_apply())
    {
        auto head = rename(A->head, substitution, bound);

        auto arg = rename(A->arg, substitution, bound);

        return Core2::Apply<>{head, arg};
    }
    // 4. Let (let {x[i] = F[i]} in body)
    else if (auto L = E.to_let())
    {
        for(auto& [x,_]: L->decls)
            bound.insert(x);

        auto body = rename(L->body, substitution, bound);

        auto decls = L->decls;
        for(auto& [_,e]: decls)
            e = rename(e, substitution, bound);

        for(auto& [x,e]: L->decls)
            erase_one(bound, x);

        return Core2::Let<>{decls, body};
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
        return Core2::Case<>{object, alts};
    }
    // 6. ConApp
    else if (auto C = E.to_conApp())
    {
        auto args = C->args;
        for(auto& arg: args)
            arg = rename(arg, substitution, bound);
        return Core2::ConApp<>{C->head, args};
    }
    // 7. BuiltinOp
    else if (auto B = E.to_builtinOp())
    {
        auto args = B->args;
        for(auto& arg: args)
            arg = rename(arg, substitution, bound);

        return Core2::BuiltinOp<>(B->lib_name, B->func_name, B->call_conv, args, B->op);
    }
    // 8. Constant
    else if (E.to_constant())
        return E;
    else
        std::abort();
}


std::optional<string> get_new_name(const Core2::Var<>& x, const string& module_name)
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

Core2::Decls<> rename_top_level(const Core2::Decls<>& decls, const string& module_name)
{
    map<Core2::Var<>, Core2::Var<>> substitution;

    set<Core2::Var<>> top_level_vars;

    Core2::Decls<> decls2;

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
            x2 = Core2::Var<>(*new_name);
            assert(not substitution.count(x2));
            substitution.insert({x,x2});
        }

        decls2.push_back({x2,rhs});

        // None of the renamed vars should have the same name;
        assert(not top_level_vars.count(x2));
        top_level_vars.insert(x2);
    }

    multiset<Core2::Var<>> bound;
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

    if (MM->language_extensions.has_extension(LangExt::FieldSelectors))
    {
        auto field_accessors = synthesize_field_accessors(M.type_decls);
        M.value_decls[0].insert(M.value_decls[0].end(), field_accessors.begin(), field_accessors.end());
    }

    // FIXME - merge with rename() below.
    // Currently this (1) translates field-decls into function declarations
    //                (2) rewrites @ f x y -> f x y (where f is the head) using unapply( ).
    //                (3) rewrites infix expressions through desugar_infix( )
    //                (4) merges adjacent function declaration lines into a Match.
    M = ::rename_infix(*MM, M);

    // We should be able to build these as we go, in rename!
    // We can merge them into a global symbol table (if we want) afterwards.

    // calls def_function, def_ADT, def_constructor, def_type_class, def_type_synonym, def_type_family
    MM->add_local_symbols(M.type_decls);

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
            MM->def_function( raw_name );

            Hs::LVar fromC = {noloc, Hs::Var("Compiler.FFI.ToFromC.fromC")};
            Hs::LVar raw   = {noloc, Hs::Var(raw_name)};
            Hs::LVar lhs   = fdecl.function;
            Hs::LExp rhs   = {noloc, Hs::ApplyExp(fromC, raw)};
            Hs::Decls decls;
            decls.push_back({noloc, Hs::simple_decl(lhs,rhs)});

            // fname :: <type>
            // fname = fromC fname$builtin
            M.value_decls.signatures.insert({fdecl.function, fdecl.type});
            M.value_decls.push_back(decls);

            // fname$builtin :: ToC <type>
            Hs::TypeCon ToC("Compiler.FFI.ToFromC.ToC");
            Hs::Type raw_type = Hs::TypeApp({loc,ToC},fdecl.type);
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

    value_decls += MM->load_builtins(loader, M.foreign_decls);

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
        export_module(name);
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

Core2::Decls<> Module::desugar(const simplifier_options& /*opts*/, FreshVarState& state, const Hs::Binds& topdecls)
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


void Module::export_small_decls(const inliner_options& options, const Core2::Decls<>& decls)
{
    // Determine which decls are loop breakers, and give them empty unfoldings.
    set<Occ::Var> occ_free_vars;
    auto occ_decl_groups = occurrence_analyze_decl_groups(*this, {decls}, occ_free_vars);
    set<Core2::Var<>> loop_breakers;
    for(auto decl_group: occ_decl_groups)
        for(auto& [x,_]: decl_group)
            if (x.info.is_loop_breaker)
            {
                loop_breakers.insert(to_core_var(x));
            }

    // FIXME: add a wrapper for EVERY constructor!
    for(auto& [x,rhs]: decls)
    {
        assert(not x.name.empty());
        assert(get_module_name(x.name) == name);

        // Add the unfolding for this variable.
        auto S = lookup_make_local_symbol(x.name);

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
            S->unfolding = make_core_unfolding(*this, options, occ_rhs);

            // Check that we have local symbols for everything that we've put in an unfolding.
            for(auto& y: free_vars)
                if (is_qualified_symbol(y.name) and get_module_name(y.name) == name)
                    lookup_make_local_symbol(y.name);
        }
    }
}

vector<expression_ref> peel_lambdas(expression_ref& E)
{
    vector<expression_ref> args;
    while(E.head().type() == type_constant::lambda_type)
    {
        args.push_back(E.sub()[0]);
        E = E.sub()[1];
    }
    return args;
}

Core2::Decls<> Module::optimize(const simplifier_options& opts, FreshVarState& fvstate, Core2::Decls<> decls)
{
    if (not opts.optimize) return decls;

    vector<Core2::Decls<>> core_decl_groups = {decls};

    // Pass: Simplify gently, Static argument

    // Pass: Simplify Gently
    core_decl_groups = simplify_module_gently(opts, fvstate, *this, core_decl_groups);

    // Pass: specialize

    // Pass: Full Laziness (FloatLambdas = 0, FloatConstants)
    if (opts.fully_lazy) float_out_from_module(fvstate, core_decl_groups);

    // Pass: Simplifier*3
    core_decl_groups = simplify_module(opts, fvstate, *this, core_decl_groups);

    // Pass: Float In

    // Pass: Call Arity, Simplify

    // Pass: Demand Analysis

    // Pass: Exitification

    // Pass: Full Laziness (Float Lambdas = ??, FloutConstants, FloatOverSatApps, FloatJoins)
    if (opts.fully_lazy) float_out_from_module(fvstate, core_decl_groups);
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

    return flatten(core_decl_groups);
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

Core2::Exp<> load_builtin(const module_loader& loader, const string& plugin_name, const string& symbol_name, const string& call_conv, int n)
{
    assert(not call_conv.empty());
    auto fn = loader.load_builtin_ptr(plugin_name, symbol_name, call_conv);

    auto args = make_vars<>(n);
    auto args_exp = args | ranges::to<vector<Core2::Exp<>>>;

    Core2::Exp<> body = Core2::BuiltinOp<>(plugin_name, symbol_name, call_conv, args_exp, fn);
    return lambda_quantify(args, body);
}

Core2::Exp<> parse_builtin(const Haskell::ForeignDecl& B, int n_args, const module_loader& L)
{
    auto call_conv = unloc(B.call_conv);
    assert(not call_conv.empty());
    return load_builtin(L, B.plugin_name, B.symbol_name, call_conv, n_args);
}

/*
 * IN theory we could use the type class in Compiler.Translate
 * - we would need to write `f :: declared_type; f = Compiler.Translate.toC f$raw` BEFORE we do type-checking.
 * - we would need to rewrite `Tr <declared type>` to find out how many lambda arguments to give parse_builtin.
 * - we may need to complain about arguments that have no Translate instance.
 * - we would need to write `f$raw :: Compiler.Translate.Tr <declared_type>`.
 *   (if the result is IO a, then its n+1, otherwise n)
 * - we also need to ensure that Compiler.Translate is loaded into the program
 *   (we could add it to imports() if there are foreign declarations).
 * - we need to ensure that the simplifier actually inlines all the many calls to fromC and toC.
 * This seems like the way to go -- but its a fair amount of work.
 */

/*
 * IN theory we could also write C++ functions
 * - wrapper ToC(const Type& a);
 * - wrapper FromC(const Type& a);
 * However, we would need access to a unique name source.  For example, for ToC(a->b), we would have
 *   f :: \x -> fromC (f (ToC x))
 * But in core that would actually be
 *   f :: \x -> fromC (let y = ToC x in f y)
 * with y fresh.
 */

Core2::Decls<> Module::load_builtins(const module_loader& L, const std::vector<Hs::ForeignDecl>& foreign_decls)
{
    Core2::Decls<> decls;
    for(const auto& decl: foreign_decls)
    {
        auto function_name = unloc(decl.function).name;

        Core2::Exp<> body;

        if (unloc(decl.call_conv) == "bpcall" or unloc(decl.call_conv) == "ecall")
        {
            auto S = lookup_symbol(function_name);
            
            function_name = S->name;

            int n_args = gen_type_arity(S->type);

            // Type synonyms have already been expanded during type checking.
            auto [arg_types, result_type] = gen_arg_result_types(S->type);

            if (is_IO_type(result_type))
            {
                auto builtin = parse_builtin(decl, n_args+1, L);
                auto xs = make_vars<>(n_args);
                auto f1 = Core2::Var<>("f1");
                auto f2 = Core2::Var<>("f2");
                auto makeIO = Core2::Var<>("Compiler.IO.makeIO");

                body = Core2::Let<>{ {{f1, builtin},                          // let f1 = builtin
                                      {f2, make_apply(Core2::Exp<>(f1),xs)}}, //     f2 = f1 x1 .. xn
                    Core2::Apply<>{makeIO, {f2}}};                          // in makeIO f2

                body = lambda_quantify(xs, body);  // \x1 .. xn -> let {f1 = builtin; f2 = f1 x1 .. xn} in makeIO f2
            }
            else
                body = parse_builtin(decl, n_args, L);
        }
        else if (unloc(decl.call_conv) == "trcall")
        {
            function_name = unloc(decl.function).name + "$raw";
            auto S = lookup_symbol(function_name);
            int n_args = gen_type_arity(S->type);
            body = parse_builtin(decl, n_args, L);
        }

        decls.push_back( { Core2::Var<>(function_name), body} );
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

Core2::Exp<> make_constructor(const std::string& con_name, const DataConInfo& info)
{
    assert(info.field_strictness.size() == info.arity());
    int arity = info.dict_arity() + info.arity();

    auto args = make_vars<>(arity);
    auto args_exp = args | ranges::to<vector<Core2::Exp<>>>;

    Core2::Exp<> body = Core2::ConApp<>{con_name, args_exp};

    // Force strict fields
    for(int i = info.arity()-1;i >= 0; i--)
    {
        int j = i + info.dict_arity();
        if (info.field_strictness[i])
            body = Core2::Case<>{args[j],{{/*wildcard pat*/{}, body}}};
    }

    return lambda_quantify(args, body);
}


Core2::Decls<> Module::load_constructors(const Hs::Decls& topdecls)
{
    Core2::Decls<> decls;

    for(const auto& [_,decl]: topdecls)
    {
        auto d = decl.to<Haskell::DataOrNewtypeDecl>();
        if (not d) continue;

        if (d->is_regular_decl())
        {
            for(const auto& constr: d->get_constructors())
            {
                auto con_name = unloc(*constr.con).name;
                auto info = lookup_resolved_symbol(con_name)->con_info;
                assert(info);
                auto exp = make_constructor(con_name, *info);
                decls.push_back( Core2::Decl<>{ Core2::Var<>(con_name) , exp} );
            }
        }
        else if (d->is_gadt_decl())
        {
            for(const auto& cons_decl: d->get_gadt_constructors())
                for(auto& lcon_name: cons_decl.con_names)
                {
                    auto con_name = unloc(lcon_name);
                    auto info = lookup_resolved_symbol(con_name)->con_info;
                    assert(info);
                    auto exp = make_constructor(con_name, *info);
                    decls.push_back( Core2::Decl<>{ Core2::Var<>(con_name) , exp} );
                }
        }
    }
    return decls;
}

bool Module::is_refutable_pattern(const Hs::LPat& lpat) const
{
    auto& [loc,pat] = lpat;
    if (auto con_pat = pat.to<Hs::ConPattern>())
    {
        // If there's > 1 constructor this this if refutable.
        auto C = lookup_resolved_symbol(unloc(con_pat->head).name);
        assert(C);
        auto T = lookup_resolved_type(*C->parent);
        assert(T);
        auto D = T->is_data();
        assert(D);
        if (D->constructors.size() >= 2) return true;

        // If any of the argument patterns are refutable, then this is refutable.
        for(auto& arg: con_pat->args)
            if (is_refutable_pattern(arg)) return true;

        // Otherwise it is irrefutable.
        return false;
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
    Core2::Exp<> U;
    if (name == "()")
    {
        S = std::make_shared<symbol_info>("()", symbol_type_t::constructor, "()", 0);
        U = Core2::ConApp("()",{});
    }
    else if (name == "[]")
    {
        S = std::make_shared<symbol_info>("[]", symbol_type_t::constructor, "[]", 0);
        U = Core2::ConApp("[]",{});
    }
    else if (name == ":")
    {
        symbol_info cons(":", symbol_type_t::constructor, "[]", 2, {{right_fix,5}});
        S = std::make_shared<symbol_info>(cons);
        auto args = make_vars<>(2,'l');
        auto args_exp = args | ranges::to<vector<Core2::Exp<>>>;
        U = lambda_quantify(args, Core2::Exp<>(Core2::ConApp(":", args_exp)));
    }
    else if (is_tuple_name(name))
    {
        int arity = name.size() - 1;
        S = std::make_shared<symbol_info>(name, symbol_type_t::constructor, name, arity);
        auto args = make_vars<>(arity,'t');
        auto args_exp = args | ranges::to<vector<Core2::Exp<>>>;
        U = lambda_quantify(args, Core2::Exp<>(Tuple(args_exp)));
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
    else if (auto result = lookup_external_symbol(symbol_name))
        return result;

    // 4. Handle magic symbols
    else
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
        TypeVar a("a", kind_type());
        info.uni_tvs = { a };
        info.field_types = { a, list_type(a) };
        info.data_type = list_tycon();
        return info;
    }
    else if (con_name == "[]")
    {
        DataConInfo info;
        TypeVar a("a", kind_type());
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
            TypeVar tv("a"+std::to_string(i+1), kind_type());
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

    declare_symbol({fname, symbol_type_t::variable, {}, {}, {}});
}

void Module::def_constructor(const string& cname, int arity, const string& type_name)
{
    if (is_qualified_symbol(cname))
        throw myexception()<<"Locally defined symbol '"<<cname<<"' should not be qualified.";

//    if (not is_qualified_symbol(type_name))
//        throw myexception()<<"Locally defined symbol '"<<type_name<<"' should not be qualified.";

    auto S = symbol_info(cname, symbol_type_t::constructor, qualify_local_name(type_name), arity, {});
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

void Module::add_local_symbols(const Hs::Decls& topdecls)
{
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
                    info.constructors.insert( qualify_local_name(cname) );
                    if (auto fields = to<Hs::FieldDecls>(constr.fields))
                        for(auto& field_decl: fields->field_decls)
                            for(auto& [loc,var]: field_decl.field_names)
                                info.fields.insert( qualify_local_name(var.name) );
                }
            }
            else if (data_decl->is_gadt_decl())
            {
                arity = data_decl->get_gadt_constructors().size();
                for(const auto& cons_decl: data_decl->get_gadt_constructors())
                    for(auto& con_name: cons_decl.con_names)
                    {
                        int arity = Hs::gen_type_arity( cons_decl.type );
                        auto cname = unloc(con_name);
                        def_constructor(cname, arity, unloc(data_decl->con).name);
                        info.constructors.insert( qualify_local_name(cname) );

                        // FIXME: handle GADT fielddecls Constr :: { name1 :: ArgType1, name2 :: ArgType2 } -> ResultType
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
		    throw myexception()<<"data families not handled";

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
    }
}

const string& exe_str()
{
    static optional<string> str;
    if (not str)
    {
        auto exe_path = find_exe_path();
        std::ifstream exe_file(exe_path);

        XXH3_state_t* state = XXH3_createState();
        XXH3_64bits_reset(state);

        vector<char> buffer(4096);
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

        auto exe = exe_str();
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

map<Core2::Var<>,Core2::Exp<>> CompiledModule::code_defs() const
{
    map<Core2::Var<>,Core2::Exp<>> code;

    for(const auto& [x,rhs]: value_decls)
    {
        assert(is_qualified_symbol(x.name));

        if (name() == get_module_name(x.name))
        {
            assert(not rhs.empty());

            code[x] = rhs;
        }
    }

    return code;
}

void CompiledModule::finish_value_decls( const Core2::Decls<>& decls )
{
    value_decls = decls;
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

    // value_decls = keep, for now

    symbols.clear();

    types.clear();

    exported_symbols_.clear();

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

    value_decls = load_builtins(*P.get_module_loader(), value_decls);
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

    std::swap(exported_types_, m->exported_types_);

    std::swap(aliases, m->aliases);

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

