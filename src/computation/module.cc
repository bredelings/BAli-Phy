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

    if (M2.tc_state)
    {
        // So.. if we import a data type declaration, do we ALSO have to import any types that its constructors reference?

        // 1. Import info about arity and kind of type constructors..
        for(auto& [tycon,info]: M2.tc_state->tycon_info())
        {
            if (not tc_state->tycon_info().count(tycon))
                tc_state->tycon_info().insert({tycon,info});
        }

        // 2. Import information about the type of constructors
        for(auto& [cname,ctype]: M2.tc_state->con_info())
        {
            if (not tc_state->con_info().count(cname))
                tc_state->con_info() = tc_state->con_info().insert({cname,ctype});
        }

        // 3. Import information about class ops
        for(auto& [tname,tinfo]: M2.tc_state->type_syn_info())
        {
            if (not tc_state->type_syn_info().count(tname))
                tc_state->type_syn_info().insert({tname,tinfo});
        }

        // 4. Import information about class ops
        for(auto& [cname,cinfo]: M2.tc_state->class_env())
        {
            if (not tc_state->class_env().count(cname))
                tc_state->class_env().insert({cname,cinfo});
        }

        // 5. Import information about instances
        for(auto& [dfun, dfun_type]: M2.tc_state->instance_env())
        {
            if (not tc_state->instance_env().count(dfun))
                tc_state->instance_env().insert({dfun, dfun_type});
        }

        // 6. Import information about type families
        for(auto& [tf_con, tf_info]: M2.tc_state->type_fam_info())
        {
            if (not tc_state->type_fam_info().count(tf_con))
                tc_state->type_fam_info().insert({tf_con, tf_info});
            else
            {
                auto& info = tc_state->type_fam_info().at(tf_con);
                assert(tf_info.args.size() == info.args.size());
                for(auto& [id, eqn_info]: tf_info.equations)
                    info.equations.insert({id, eqn_info});
            }
        }

        // 7. Import types for values.
        for(auto& [name, type]: M2.tc_state->gve)
        {
            if (not tc_state->gve.count(name))
                tc_state->imported_gve = tc_state->imported_gve.insert({name,type});
        }

        for(auto& [name, type]: M2.tc_state->imported_gve)
        {
            if (not tc_state->gve.count(name))
                tc_state->imported_gve = tc_state->imported_gve.insert({name,type});
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

    tc_state = std::make_shared<TypeChecker>( P.fresh_var_state(), name, *this);

    // Scans imported modules and modifies symbol table and type table
    perform_imports(P);

    Hs::ModuleDecls M;
    if (module.topdecls)
        M = Hs::ModuleDecls(*module.topdecls);

    // We should create a "local fixity environment" mapping from var and Module.var -> fixity info.
    // This can supplement the symbols that we imported from other modules.
    // Then we can AUGMENT this local fixity environment when we encounter fixities at lower levels.
    declare_fixities(M);

    auto field_accessors = synthesize_field_accessors(M.type_decls);
    M.value_decls[0].insert(M.value_decls[0].end(), field_accessors.begin(), field_accessors.end());

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
        def_function(f.function_name);

    // Currently we do "renaming" here.
    // That just means (1) qualifying top-level declarations and (2) desugaring rec statements.
    M = rename(opts, M);

//    std::cerr<<"-------- module "<<name<<"--------\n";
    auto tc_result = typecheck(M);

//    std::cerr<<"All decls:\n";
//    for(auto& [name,type]: tc_state->gve)
//        std::cerr<<name<<" :: "<<type<<"\n";

    auto [hs_decls, core_decls] = tc_result.all_binds();

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
        std::cerr<<"\n\nCore:\n";
        for(auto& [x,rhs] : value_decls)
            std::cerr<<x.print()<<" = "<<rhs.print()<<"\n";
        std::cerr<<"\n\n";
    }

    // Check for duplicate top-level names.
    check_duplicate_var(value_decls);

    std::tie(small_decls_in, small_decls_in_free_vars) = import_small_decls(P);

    value_decls = optimize(opts, P.fresh_var_state(), value_decls, small_decls_in, small_decls_in_free_vars);

    // result returned in this->small_decls_out, this->small_decls_out_free_vars
    std::tie(small_decls_out, small_decls_out_free_vars) = export_small_decls(value_decls, small_decls_in);
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
    if (tc_state)
    {
        // Record kinds on the type symbol table
        for(auto& [typecon,info]: tc_state->tycon_info())
        {
            if (get_module_name(typecon) == name)
            {
                auto& T = types.at(typecon);
                assert(not T.kind);
                T.kind = info.kind;
            }
        }

        // Record types on the value symbol table
        for(auto& [value,type]: tc_state->gve)
        {
            if (get_module_name(value) == name)
            {
                if (not symbols.count(value))
                    ; //                std::cerr<<"'"<<value<<"' is not a symbol!\n";
                else
                {
                    auto& V = symbols.at(value);
                    V.type = type;
                }
            }
        }
    }

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

void add_constructor(map<var,expression_ref>& decls, const constructor& con)
{
    var x(con.name());
    expression_ref body = lambda_expression(con);
    auto res = occurrence_analyzer(body);
    decls.insert({x,res.first});
}

pair< map<var, expression_ref>, set<var> > Module::import_small_decls(const Program& P)
{
    map<var, expression_ref> small_decls_in;

    set<var> small_decls_in_free_vars;

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

    return {small_decls_in, small_decls_in_free_vars};
}

pair<map<var,expression_ref>, set<var>> Module::export_small_decls(const CDecls& cdecls, const map<var,expression_ref>& small_decls_in)
{
    // Modules that we imported should have their small_decls transitively inherited
    map<var, expression_ref> small_decls_out = small_decls_in;

    set<var> small_decls_out_free_vars;

    for(auto& [x,rhs]: cdecls)
    {
        assert(not x.name.empty());

        if (simple_size(rhs) <= 5)
            small_decls_out.insert({x, rhs});
    }

    // Find free vars in the decls that are not bound by *other* decls.
    for(auto& [_,F]: small_decls_out)
    {
        auto [E, free_vars] = occurrence_analyzer(F);
        F = E;

        for(auto& x: free_vars)
            if (not small_decls_out.count(x))
                small_decls_out_free_vars.insert(x);
    }

    return {small_decls_out, small_decls_out_free_vars};
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
                         const shared_ptr<TypeChecker>& tc_state,
                         const map<string,symbol_info>& exports,
                         const string& module_name)
{
    // Record exports
    set<string> exported;
    for(auto& [name,symbol]: exports)
        if (get_module_name(symbol.name) == module_name)
            exported.insert(symbol.name);

    // Mark some generated functions as exported
    if (tc_state)
    {
        // Instances are exported
        for(auto& [dvar, _]: tc_state->instance_env())
            exported.insert(dvar.name);

        for(auto& [cname,cinfo]: tc_state->class_env())
        {
            for(auto& [dvar, _]: cinfo.superclass_extractors)
                exported.insert(dvar.name);

            // Default methods are exported
            for(auto& [method, dm]: cinfo.default_methods)
                exported.insert(unloc(dm.name));
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

CDecls Module::optimize(const simplifier_options& opts, FreshVarState& fvstate, CDecls cdecls, const map<var, expression_ref>& small_decls_in, const set<var>& small_decls_in_free_vars)
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
        mark_exported_decls(cdecls, tc_state, exported_symbols(), name);

        vector<CDecls> decl_groups = {cdecls};

        decl_groups = simplify_module_gently(opts, fvstate, small_decls_in, small_decls_in_free_vars, decl_groups);

        if (opts.fully_lazy)
            float_out_from_module(fvstate, decl_groups);

        decl_groups = simplify_module(opts, fvstate, small_decls_in, small_decls_in_free_vars, decl_groups);

        if (opts.fully_lazy)
            float_out_from_module(fvstate, decl_groups);

        cdecls = flatten(decl_groups);
    }

    return rename_top_level(cdecls, name);
}

pair<string,expression_ref> parse_builtin(const Haskell::ForeignDecl& B, const module_loader& L)
{
    const string builtin_prefix = "builtin_function_";

    string operation_name = B.plugin_name+":"+B.symbol_name;

    auto body = load_builtin(L, builtin_prefix + B.symbol_name, B.plugin_name, B.n_args(), operation_name);

    return {B.function_name, body};
}

CDecls Module::load_builtins(const module_loader& L, const std::vector<Hs::ForeignDecl>& foreign_decls, CDecls cdecls)
{
    for(const auto& decl: foreign_decls)
    {
        auto [function_name, body] = parse_builtin(decl, L);

        function_name = lookup_symbol(function_name).name;

        cdecls.push_back( { var(function_name), body} );
    }

    return cdecls;
}

string get_constructor_name(const Hs::Type& constr)
{
    auto [con,_] = Hs::decompose_type_apps(constr);
    assert(con.is_a<Hs::TypeCon>());
    return unloc(con.as_<Hs::TypeCon>().name);
}

CDecls Module::load_constructors(const Hs::Decls& topdecls, CDecls cdecls)
{
    for(const auto& decl: topdecls)
    {
        auto d = decl.to<Haskell::DataOrNewtypeDecl>();
        if (not d) continue;

        if (d->is_regular_decl())
        {
            for(const auto& constr: d->get_constructors())
            {
                auto cname = constr.name;
                auto info = tc_state->con_info().at(cname);
                int arity = info.dict_arity() + info.arity();

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
                    auto info = tc_state->con_info().at(cname);
                    int arity = info.dict_arity() + info.arity();

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
        return {"Char", type_name_category::ADT, {}, 0, kind_star()};
    else if (name == "Double")
        return {"Double", type_name_category::ADT, {}, 0, kind_star()};
    else if (name == "Int")
        return {"Int", type_name_category::ADT, {}, 0, kind_star()};
    else if (name == "Integer")
        return {"Integer", type_name_category::ADT, {}, 0, kind_star()};
    else if (name == "()")
        return {"()", type_name_category::ADT, {}, 0, kind_star()};
    else if (name == "[]")
        return {"[]", type_name_category::ADT, {}, 1, make_n_args_kind(1)};
    else if (name == "->")
    {
        return {"->", type_name_category::type_func, {{right_fix,0}}, 2, make_n_args_kind(2)};
    }
    else if (name == "~")
    {
        return {"~", type_name_category::type_func, {}, 2, make_n_args_constraint_kind(2)};
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

    declare_type( {tname, type_name_category::ADT, {}, /*arity*/ -1, /*kind*/ {}} );
}

void Module::def_ADT(const std::string& tname, const fixity_info& fixity)
{
    if (is_qualified_symbol(tname))
        throw myexception()<<"Locally defined symbol '"<<tname<<"' should not be qualified.";

    declare_type( {tname, type_name_category::ADT, fixity, /*arity*/ -1, /*kind*/ {}} );
}

void Module::def_type_synonym(const std::string& sname, int arity)
{
    if (is_qualified_symbol(sname))
        throw myexception()<<"Locally defined type '"<<sname<<"' should not be qualified.";

    declare_type( {sname, type_name_category::type_syn, {}, arity, /*kind*/ {}} );
}

void Module::def_type_family(const std::string& fname, int arity)
{
    if (is_qualified_symbol(fname))
        throw myexception()<<"Locally defined type '"<<fname<<"' should not be qualified.";

    declare_type( {fname, type_name_category::type_fam, {}, arity, /*kind*/ {}} );
}

void Module::def_type_class(const std::string& cname)
{
    if (is_qualified_symbol(cname))
        throw myexception()<<"Locally defined type '"<<cname<<"' should not be qualified.";

    declare_type( {cname, type_name_category::type_class, {}, /*arity*/ -1, /*kind*/ {}} );
}

void Module::def_type_class_method(const string& method_name, const string& class_name)
{
    if (is_qualified_symbol(method_name))
        throw myexception()<<"Locally defined type class method '"<<method_name<<"' should not be qualified.";

//    if (not is_qualified_symbol(class_name))
//        throw myexception()<<"Locally defined type class '"<<class_name<<"' should be qualified.";

    declare_symbol( {method_name, class_method_symbol, class_name, {}, {unknown_fix, -1}} );
}

void Module::declare_fixities_(const Haskell::FixityDecl& FD)
{
    // Determine precedence.
    int precedence = (FD.precedence)?*FD.precedence:9;

    // Find op names and declare fixity and precedence.
    for(const auto& name: FD.names)
        declare_fixity(name, precedence, FD.fixity);
}

void Module::declare_fixities_(const Haskell::Decls& decls)
{
    // 0. Get names that are being declared.
    for(const auto& decl: decls)
        if (auto FD = decl.to<Haskell::FixityDecl>())
            declare_fixities_(*FD);
}

void Module::declare_fixities(const Hs::ModuleDecls& M)
{
    // At the top level, we have removed fixities from value_decls.
    for(auto& FD: M.fixity_decls)
        declare_fixities_(FD);

    for(const auto& type_decl: M.type_decls)
        if (auto C = type_decl.to<Haskell::ClassDecl>())
            for(auto& fixity_decl: C->fixity_decls)
                declare_fixities_(fixity_decl);
}

void Module::maybe_def_function(const string& var_name)
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

void Module::add_local_symbols(const Hs::Decls& topdecls)
{
    // 0. Get names that are being declared.
    for(const auto& decl: topdecls)
        if (auto pd = decl.to<Hs::PatDecl>())
        {
            for(const auto& var_name: find_bound_vars( unloc(pd->lhs) ))
                maybe_def_function( var_name );
        }
        else if (auto fd = decl.to<Hs::FunDecl>())
        {
            for(const auto& var_name: find_bound_vars( fd->v ))
                maybe_def_function( var_name );
        }
        else if (auto data_decl = decl.to<Haskell::DataOrNewtypeDecl>())
        {
            def_ADT(data_decl->name);

            // Why are we recording the arity here?  This is too early...
            // It looks like we use it when renaming patterns, but...

            if (data_decl->is_regular_decl())
            {
                for(const auto& constr: data_decl->get_constructors())
                    def_constructor(constr.name, constr.arity(), data_decl->name);
            }
            else if (data_decl->is_gadt_decl())
            {
                for(const auto& cons_decl: data_decl->get_gadt_constructors())
                    for(auto& con_name: cons_decl.con_names)
                    {
                        int arity = Hs::gen_type_arity( unloc(cons_decl.type) );
                        def_constructor(unloc(con_name), arity, data_decl->name);
                    }
            }
        }
        else if (decl.is_a<Haskell::ClassDecl>())
        {
            auto& Class = decl.as_<Haskell::ClassDecl>();

            def_type_class(Class.name);

            for(auto& tf: Class.type_fam_decls)
                def_type_family( unloc(tf.con.name), tf.arity() );

            for(auto& sig_decl: Class.sig_decls)
                for(auto& v: sig_decl.vars)
                    def_type_class_method(unloc(v.name), Class.name);
        }
        else if (auto S = decl.to<Haskell::TypeSynonymDecl>())
        {
            def_type_synonym(S->name, S->arity());
        }
        else if (auto TF = decl.to<Hs::TypeFamilyDecl>())
        {
            def_type_family( unloc(TF->con.name), TF->arity() );
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
        for(const auto& decls: *M.module.topdecls)
            o<<decls.print()<<"\n";

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
