#include <set>
#include <boost/optional.hpp>
#include "computation/module.H"
#include "myexception.H"
#include "program.H"
#include "operations.H"
#include "computation/machine/graph_register.H" // for graph_normalize( )
#include "computation/operations.H"
#include "parser/rename.H"
#include "parser/desugar.H"
#include "computation/loader.H"
#include "expression/AST_node.H"
#include "expression/expression.H"
#include "expression/var.H"
#include "expression/lambda.H"
#include "expression/let.H"
#include "expression/case.H"
#include "expression/tuple.H"
#include "expression/substitute.H"
#include "computation/optimization/simplifier.H"
#include "computation/optimization/occurrence.H"
#include "computation/optimization/let-float.H"

using std::pair;
using std::map;
using std::set;
using std::multiset;
using std::string;
using std::vector;

symbol_info::symbol_info(const std::string& s, symbol_type_t st, int a)
    :name(s), symbol_type(st), arity(a)
{ }

symbol_info::symbol_info(const std::string& s, symbol_type_t st, int a, int p, fixity_t f)
    :name(s), symbol_type(st), arity(a), precedence(p), fixity(f)
{ }

symbol_info::symbol_info(const std::string& s, symbol_type_t st, int a, int p, fixity_t f, const expression_ref& t)
    :name(s), symbol_type(st), arity(a), precedence(p), fixity(f), type(t)
{ }

bool operator==(const symbol_info&S1, const symbol_info& S2)
{
    return (S1.name == S2.name) and (S1.symbol_type == S2.symbol_type) and 
	(S1.arity == S2.arity) and (S1.precedence == S2.precedence) and (S1.fixity == S2.fixity) and
	(S1.type.ptr() == S2.type.ptr());
}

bool operator!=(const symbol_info&S1, const symbol_info& S2)
{
    return not (S1 == S2);
}

symbol_info lookup_symbol(const string& name, const Program& P);

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

    std::pair<string,string> element(identifier_name,resolved_name);

    auto range = aliases.equal_range(identifier_name);
    for(auto it = range.first; it != range.second; it++)
	if (it->second == resolved_name)
	    return;

    aliases.insert( std::pair<string,string>(identifier_name, resolved_name) );
}

void Module::declare_symbol(const symbol_info& S)
{
    if (is_qualified_symbol(S.name))
	throw myexception()<<"Locally defined symbol '"<<S.name<<"' should not be qualified in declaration.";

    symbol_info S2 = S;
    S2.name = name + "." + S.name;

    if (symbols.count(S2.name))
	throw myexception()<<"Trying to declare '"<<S.name<<"' twice in module '"<<name<<"'";

    // Add the symbol first.
    add_symbol(S2);
    // Add the alias for qualified name: S.name -> S2.name;
    add_alias(S2.name, S2.name);
    // Add the alias for unqualified name: S.name -> S2.name;
    add_alias(S.name, S2.name);
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
	declare_symbol({s, unknown_symbol, -1, -1, unknown_fix, {}});

    symbol_info& S = symbols.find(s2)->second;

    S.precedence = precedence;
    S.fixity = fixity;
}

void Module::add_import(bool qualified, const string& modid)
{
    vector<expression_ref> sub;
    if (qualified)
	sub.push_back(String("qualified"));
    sub.push_back(String(modid));
  
    add_impdecl(expression_ref{AST_node("impdecl"),sub});
}

void Module::add_import_as(bool qualified, const string& modid, const string& modid2)
{
    vector<expression_ref> sub;
    if (qualified)
	sub.push_back(String("qualified"));
    sub.push_back(String(modid));
    sub.push_back(String("as"));
    sub.push_back(String(modid2));
  
    add_impdecl(expression_ref{AST_node("impdecl"),sub});
}

void Module::add_impdecl(const expression_ref& impdecl)
{
    vector<expression_ref> sub;
    if (impdecls)
	sub = impdecls.sub();
    sub.push_back(impdecl);
    impdecls = expression_ref{AST_node("impdecls"),sub};
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

void Module::import_module(const Module& M2, const string& modid, bool qualified)
{
    assert(modid != name);

    // Right now 'exports' only has functions, not data types or constructors

    for(const auto& p: M2.exported_symbols())
    {
	const symbol_info& S = p.second;

	import_symbol(S, modid, qualified);
    }
}

void Module::import_module(const Module& M2, bool qualified)
{
    import_module(M2, M2.name, qualified);
}

module_import parse_import(const expression_ref& impdecl)
{
    module_import mi;

    int i=0;
    mi.qualified = impdecl.sub()[0].as_<String>() == "qualified";
    if (mi.qualified) i++;
	    
    mi.name = impdecl.sub()[i++].as_<String>();
	    
    if (i < impdecl.size() and impdecl.sub()[i++].as_<String>() == "as")
	mi.as = impdecl.sub()[i++].as_<String>();
    else
	mi.as = mi.name;

    // only:   handle import qualified A as B (x, y)
    // hiding:  handle import qualified A as B hiding (x, y)

    assert(i == impdecl.size());
    return mi;
}

map<string,module_import> Module::imports() const
{
    std::map<string, module_import> imports_map;

    if (impdecls)
	for(const auto& impdecl:impdecls.sub())
	{
	    auto import = parse_import(impdecl);
	    if (imports_map.count(import.name))
		throw myexception()<<"Module '"<<name<<"': importing module '"<<import.name<<"' twice!";
	    imports_map[import.name] = import;
	}

    // Import the Prelude if it wasn't explicitly mentioned in the import list.
    if (not imports_map.count("Prelude") and name != "Prelude" and not language_options.count("NoImplicitPrelude"))
	imports_map["Prelude"] = module_import("Prelude");

    return imports_map;
}

set<string> Module::dependencies() const
{
    set<string> modules;
    for(auto& mi: imports())
	modules.insert(mi.second.name);
    return modules;
}

void Module::compile(const Program& P)
{
    assert(not resolved);
    resolved = true;

    perform_imports(P);

    declare_fixities();

    rename_infix(P);  // FIXME - merge with rename() below.

    add_local_symbols();

    perform_exports();

    // Currently we do renaming here, including adding prefixes to top-level decls.
    rename(P);

    desugar(P);

    load_builtins(*P.get_module_loader());

    load_constructors();

    get_types(P);

    // Get rid of declarations that are not Decl
    if (topdecls)
    {
	vector<expression_ref> decls;
	for(auto& decl: topdecls.sub())
	    if (is_AST(decl,"Decl"))
		decls.push_back(decl);
	topdecls = expression_ref{AST_node("TopDecls"),decls};
    }

    // Check for duplicate top-level names.
    if (topdecls)
	check_duplicate_var(topdecls);

    import_small_decls(P);

    optimize(P);

    export_small_decls();
}

void Module::perform_imports(const Program& P)
{
    for(auto& i: imports())
    {
	auto& I = P.get_module(i.second.name);
	import_module(I, i.second.as, i.second.qualified);
    }
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

void Module::export_module(const string& modid)
{
    if (modid != name and not imports().count(modid))
	throw myexception()<<"module '"<<modid<<"' is exported but not imported";

    for(auto& s: symbols)
	if (get_module_name(s.second.name) == modid)
	{
	    auto& qname = s.second.name;
	    auto varid = get_unqualified_name(qname);
	    auto S1 = lookup_symbol(varid);
	    auto S2 = lookup_symbol(qname);
	    if (S1.name == S2.name)
		export_symbol(S1);
	}
}

void Module::perform_exports()
{
    // Currently we just export the local symbols
    if (not exports or exports.sub().size() == 0)
	export_module(name);
    else
    {
	assert(is_AST(exports,"Exports"));
	for(auto& ex: exports.sub())
	{
	    if (is_AST(ex,"qvar"))
	    {
		string qvarid = ex.sub()[0].as_<String>(); // This ignores export subspec - see grammar.
		if (aliases.count(qvarid))
		    export_symbol(lookup_symbol(qvarid));
		else
		    throw myexception()<<"trying to export variable '"<<qvarid<<"', which is not in scope.";
	    }
	    else if (is_AST(ex,"module"))
	    {
		string modid = ex.sub()[0].as_<String>();
		export_module(modid);
	    }
	    else
		throw myexception()<<"I don't understand export '"<<ex<<"'";
	}
    }
}

map<string,expression_ref> Module::code_defs() const
{
    if (not topdecls) return map<string,expression_ref>();

    map<string, expression_ref> code;

    for(const auto& decl: topdecls.sub())
    {
	assert(is_AST(decl,"Decl"));
	auto& x = decl.sub()[0].as_<var>();
	assert(is_qualified_symbol(x.name));

	if (this->name == get_module_name(x.name))
	{
	    // get the body for the  decl
	    auto& body = decl.sub()[1];
	    assert(body);

	    code[x.name] = body;
	}
    }

    return code;
}

void Module::rename_infix(const Program&)
{
    if (topdecls)
    {
	assert(is_AST(topdecls,"TopDecls"));
	topdecls = ::rename_infix(*this,topdecls);
    }
}

void Module::rename(const Program&)
{
    if (topdecls)
    {
	assert(is_AST(topdecls,"TopDecls"));
	topdecls = ::rename(*this,topdecls);
    }
}

void Module::desugar(const Program&)
{
    if (topdecls)
    {
	assert(is_AST(topdecls,"TopDecls"));
	topdecls = ::desugar(*this,topdecls);
    }
}

int nodes_size(const expression_ref& E);

void add_constructor(map<var,expression_ref>& decls, const constructor& con)
{
    var x(con.name());
    expression_ref body = lambda_expression(con);
    auto res = occurrence_analyzer(body);
    decls.insert({x,res.first});
}

void Module::import_small_decls(const Program& P)
{
    if (not topdecls) return;

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
    if (not topdecls) return;

    assert(small_decls_out.empty());

    // Modules that we imported should have their small_decls transitively inherited
    small_decls_out = small_decls_in;

    for(auto& decl: topdecls.sub())
    {
	auto& x = decl.sub()[0].as_<var>();
	assert(not x.name.empty());

	auto& body = decl.sub()[1];
	if (nodes_size(body) < 15)
	    small_decls_out.insert({x, body});
    }

    // Find free vars in the decls that are not bound by *other* decls.
    for(auto& decl: small_decls_out)
    {
	set<var> free_vars;
	tie(decl.second, free_vars) = occurrence_analyzer(decl.second);
	for(auto& x: free_vars)
	    if (not small_decls_out.count(x))
		small_decls_out_free_vars.insert(x);
    }
}

void parse_module(const expression_ref& M, string& name, expression_ref& exports, expression_ref& body, expression_ref& impdecls, expression_ref& topdecls)
{
    assert(is_AST(M, "Module"));
    if (M.size() == 1)
    {
	name = "Main";
	exports = {};
	body = M.sub()[0];
    }
    else if (M.size() == 2)
    {
	name = M.sub()[0].as_<String>();
	exports = {};
	body = M.sub()[1];
    }
    else if (M.size() == 3)
    {
	name = M.sub()[0].as_<String>();
	exports = M.sub()[1];
	assert(is_AST(exports,"Exports"));
	body = M.sub()[2];
    }
    assert(is_AST(body,"Body"));

    // 2. body = impdecls + [optional topdecls]
    for(const auto& E: body.sub())
	if (is_AST(E,"TopDecls"))
	    topdecls = E;
	else if (is_AST(E,"impdecls"))
	    impdecls = E;
}

expression_ref create_module(const string& name, const expression_ref& exports, const expression_ref& impdecls, const expression_ref& topdecls)
{
    expression_ref body = AST_node("Body");
    if (impdecls)
    {
	assert(is_AST(impdecls, "impdecls"));
	body = body + impdecls;
    }
    if (topdecls)
    {
	assert(is_AST(topdecls, "TopDecls"));
	body = body + topdecls;
    }
    expression_ref module = AST_node("Module");
    if (not name.empty())
	module = module + String(name);
    if (exports)
    {
	assert(is_AST(exports,"Exports"));
	module = module + exports;
    }
    module = module + body;
    return module;
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
    if (not E) return E;

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

    // 5. (partial) Literal constant.  Treat as 0-arg constructor.
    if (not E.size()) return E;

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

    // 4. Constructor or Operation or Apply
    if (E.head().is_a<constructor>() or E.head().is_a<Operation>())
    {
	object_ptr<expression> E2 = E.as_expression().clone();
	for(int i=0;i<E.size();i++)
	    E2->sub[i] = rename(E2->sub[i], substitution, bound);
	return E2;
    }

    // 5. Let (let {x[i] = F[i]} in body)
    if (is_let_expression(E))
    {
	auto body  = let_body(E);
	auto decls = let_decls(E);

	for(auto& decl: decls)
	    bound.insert(decl.first);

	body = rename(body, substitution, bound);

	for(auto& decl: decls)
	    decl.second = rename(decl.second, substitution, bound);

	for(auto& decl: decls)
	    erase_one(bound, decl.first);

        // 5.2 Simplify the let-body
	return let_expression(decls, body);
    }

    std::abort();
    return E;
}


boost::optional<string> get_new_name(const var& x, const string& module_name)
{
    assert(not is_haskell_builtin_con_name(x.name));

    if (is_qualified_var(x))
    {
	assert(x.index == 0);
	return boost::none;
    }

    return module_name + "." + x.name + "#" + convertToString(x.index);
}

expression_ref rename_top_level(const expression_ref& decls, const string& module_name)
{
    assert(is_AST(decls, "TopDecls"));

    map<var, var> substitution;

    set<var> top_level_vars;

    CDecls decls2;

#ifndef NDEBUG
    check_duplicate_var(decls);
#endif

    for(int i = 0; i< decls.size(); i++)
    {
	auto x = decls.sub()[i].sub()[0].as_<var>();
	var x2 = x;
	assert(not substitution.count(x));

	if (auto new_name = get_new_name(x, module_name))
	{
	    x2 = var(*new_name);
	    assert(not substitution.count(x2.name));
	    substitution.insert({x,x2});
	}

	decls2.push_back({x2,decls.sub()[i].sub()[1]});

	// None of the renamed vars should have the same name;
	assert(not top_level_vars.count(x2));
	top_level_vars.insert(x2);
    }

    multiset<var> bound;
    for(auto& decl: decls2)
    {
	assert(bound.empty());
	decl.second = rename(decl.second, substitution, bound);
	assert(bound.empty());
    }

#ifndef NDEBUG
    check_duplicate_var(decls2);
#endif

    return make_topdecls(decls2);
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

int nodes_size(const expression_ref& E)
{
    int total = 1;

    if (E.is_expression())
	for(const auto& e:E.sub())
	    total += nodes_size(e);

    return total;
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
    if (not exported.empty())
    {
	myexception e;
	e<<"Module '"<<module_name<<"' exports undefined symbols:\n";
	for(auto& name: exported)
	    e<<"  "<<name;
	throw e;
    }
}

void Module::optimize(const Program& P)
{
    // why do we keep on re-optimizing the same module?
    if (optimized) return;
    optimized = true;

    if (topdecls)
    {
	vector<expression_ref> new_decls;
	for(auto& decl: topdecls.sub())
	{
	    if (not is_AST(decl,"Decl"))
		; //new_decls.push_back(decl);
	    else
	    {
		// This won't float things to the top level!
		auto name = decl.sub()[0].as_<var>().name;
		auto body = decl.sub()[1];
		body = let_float(body);
		body = graph_normalize(body);

		new_decls.push_back(AST_node("Decl") + decl.sub()[0] + body);
	    }
	}
	topdecls = expression_ref{AST_node("TopDecls"),new_decls};

	if (do_optimize)
	{
	    auto decls = parse_decls(topdecls);
	    mark_exported_decls(decls, exported_symbols(), name);

	    vector<CDecls> decl_groups = {decls};
	    for(int i=0;i<P.get_module_loader()->max_iterations;i++)
	    {
		decl_groups = simplify_module(*P.get_module_loader(), small_decls_in, small_decls_in_free_vars, decl_groups);
	    }

	    decls = flatten(std::move(decl_groups));
	    topdecls = make_topdecls(decls);

	    module = create_module(name, exports, impdecls, topdecls);
	}
    }

    if (topdecls)
	topdecls = rename_top_level(topdecls, name);
}

pair<string,expression_ref> parse_builtin(const expression_ref& decl, const module_loader& L)
{
    const string builtin_prefix = "builtin_function_";

    assert(is_AST(decl, "Builtin"));

    string function_name = decl.sub()[0].as_<String>();
    int n_args           = decl.sub()[1].as_int();
    string symbol_name   = decl.sub()[2].as_<String>();
    string plugin_name   = decl.sub()[3].as_<String>();

    string operation_name = plugin_name+":"+symbol_name;

    auto body = load_builtin(L, builtin_prefix + symbol_name, plugin_name, n_args, operation_name);

    return {function_name, body};
}

void Module::load_builtins(const module_loader& L)
{
    if (not topdecls) return;

    vector<expression_ref> new_decls;
    for(const auto& decl: topdecls.sub())
	if (is_AST(decl,"Builtin"))
	{
	    auto x = parse_builtin(decl, L);
	    auto function_name = x.first;
	    auto body = x.second;

	    function_name = lookup_symbol(function_name).name;

	    new_decls.push_back(AST_node("Decl") + var(function_name) + body);
	}
	else
	    new_decls.push_back(decl);
    topdecls = expression_ref{AST_node("TopDecls"), new_decls};
}

void Module::load_constructors()
{
    if (not topdecls) return;

    vector<expression_ref> new_decls;

    for(const auto& decl: topdecls.sub())
	if (is_AST(decl,"Decl:data"))
	{
	    if (decl.size() >= 2)
	    {
		expression_ref constrs = decl.sub()[1];
		assert(is_AST(constrs,"constrs"));
		for(const auto& constr: constrs.sub())
		{
		    int arity = 0;
		    expression_ref type = constr;
		    if (is_AST(type,"TypeApply"))
		    {
			arity = type.size()-1;
			type = type.sub()[0];
		    }
		    assert(is_AST(type,"type_id"));
		    string cname = type.head().as_<AST_node>().value;
		    string qualified_name = name+"."+cname;
		    expression_ref body = lambda_expression( constructor(qualified_name, arity) );
		    new_decls.push_back(AST_node("Decl") + var(qualified_name) + body);
		}
	    }
            // Strip out the constructor definition here new_decls.push_back(decl);
	}
	else
	    new_decls.push_back(decl);

    topdecls = expression_ref{AST_node("TopDecls"), new_decls};
}

bool Module::is_declared(const std::string& name) const
{
    return is_haskell_builtin_con_name(name) or (aliases.count(name) > 0);
}

pair<symbol_info,expression_ref> Module::lookup_builtin_symbol(const std::string& name)
{
    if (name == "()")
	return {symbol_info("()", constructor_symbol, 0), constructor("()",0)};
    else if (name == "[]")
	return {symbol_info("[]", constructor_symbol, 0), constructor("[]",0)};
    else if (name == ":")
	return {symbol_info(":", constructor_symbol, 2, 5, right_fix), lambda_expression( right_assoc_constructor(":",2) )};
    else if (is_tuple_name(name))
    {
	int arity = name.size() - 1;
	expression_ref body = lambda_expression( tuple_head(arity) );
	return {symbol_info(name, constructor_symbol, arity), body};
    }
    throw myexception()<<"Symbol 'name' is not a builtin (constructor) symbol.";
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
    if (S.precedence == -1 or S.fixity == unknown_fix) 
    {
	// If either is unset, then both must be unset!
	assert(S.precedence == -1 and S.fixity == unknown_fix);
	S.precedence = 9;
	S.fixity = left_fix;
    }

    return S;
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

vector<string> haskell_name_path(const std::string& s)
{
    if (s == ".") return {s};
    else if (s.size() >= 2 and s.substr(s.size()-2,2) == "..")
    {
	vector<string> path = split(s.substr(0,s.size()-2),'.');
	path.push_back(".");
	return path;
    }
    else
	return split(s,'.');
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

bool is_haskell_varid(const std::string& s)
{
    if (s.empty()) return false;

    if (not haskell_is_lower(s[0])) return false;
    for(int i=1;i<s.size();i++)
    {
	char c = s[i];
	if (not (isupper(c) or haskell_is_lower(c) or isdigit(c) or c=='\''))
	    return false;
    }
    return true;
}

bool is_haskell_conid(const std::string& s)
{
    if (s.empty()) return false;

    if (not isupper(s[0])) return false;
    for(int i=1;i<s.size();i++)
    {
	char c = s[i];
	if (not (isupper(c) or haskell_is_lower(c) or isdigit(c) or c=='\''))
	    return false;
    }
    return true;
}

bool is_haskell_varsym(const string& s)
{
    static const string symbols = "!#$%&*+./<=>?@\\^|-~:";

    if (not s.size()) return false;

    for(int i=0;i<s.size();i++)
	if (symbols.find(s[i]) == -1)
	    return false;

    if (s[0] == ':') return false;

    return true;
}

bool is_haskell_consym(const string& s)
{
    static const string symbols = "!#$%&*+./<=>?@\\%|-~:";

    if (not s.size()) return false;

    for(int i=0;i<s.size();i++)
	if (symbols.find(s[i]) == -1)
	    return false;

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

bool is_haskell_normal_con_name(const std::string& s)
{
    vector<string> path = haskell_name_path(s);
    if (path.empty()) return false;
    if (not is_haskell_conid(path.back()) and not is_haskell_consym(path.back())) return false;
    for(int i=0;i<path.size()-1;i++)
	if (not is_haskell_conid(path[i])) return false;
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

    string qualified_name = name+"."+fname;
    auto loc = symbols.find(qualified_name);

    if (loc != symbols.end())
    {
	symbol_info& S = loc->second;
	// Only the fixity has been declared!
	if (S.symbol_type == unknown_symbol)
	{
	    S.symbol_type = variable_symbol;
	}
	else
	    throw myexception()<<"Can't add function with name '"<<fname<<"': that name is already used!";
    }
    else
	declare_symbol({fname, variable_symbol, -1, -1, unknown_fix, {}});
}

void Module::def_constructor(const std::string& cname, int arity)
{
    if (is_qualified_symbol(cname))
	throw myexception()<<"Locally defined symbol '"<<cname<<"' should not be qualified.";

    string qualified_name = name+"."+cname;

    auto loc = symbols.find(qualified_name);
    if (loc != symbols.end())
    {
	symbol_info& S = loc->second;
	// Only the fixity has been declared!
	if (S.symbol_type == unknown_symbol and not S.type)
	{
	    S.symbol_type = constructor_symbol;
	    return;
	}
    }

    declare_symbol( {cname, constructor_symbol, arity, -1, unknown_fix, {}} );
}

void Module::declare_fixities()
{
    if (not topdecls) return;

    assert(is_AST(topdecls,"TopDecls"));

    // 0. Get names that are being declared.
    for(const auto& decl: topdecls.sub())
	if (is_AST(decl,"FixityDecl"))
	{
	    // Determine fixity.
	    string f = decl.sub()[0].as_<String>();
	    fixity_t fixity = unknown_fix;
	    if (f == "infixl")
		fixity = left_fix;
	    else if (f == "infixr")
		fixity = right_fix;
	    else if (f == "infix")
		fixity = non_fix;
	    else
		std::abort();

	    // Determine precedence.
	    int precedence = 9;
	    if (decl.size() == 3)
		precedence = decl.sub()[1].as_int();

	    // Find op names and declare fixity and precedence.
	    for(const auto& op: decl.sub().back().sub())
	    {
		string name = op.as_<String>();
		declare_fixity(name, precedence, fixity);
	    }
	}
}

void Module::add_local_symbols()
{
    if (not topdecls) return;

    assert(is_AST(topdecls,"TopDecls"));

    // 0. Get names that are being declared.
    for(const auto& decl: topdecls.sub())
	if (is_AST(decl,"Decl"))
	{
	    set<string> vars;
	    if (is_function_binding(decl))
		vars.insert( get_func_name(decl) );
	    else
	    {
		auto& lhs = decl.sub()[0];
		vars = find_bound_vars(lhs);
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
		    if (S.symbol_type == unknown_symbol and not S.type)
			S.symbol_type = variable_symbol;
		}
		else
		    def_function(var_name);
	    }
	}
	else if (is_AST(decl,"Builtin"))
	{
	    string bname = decl.sub()[0].as_<String>();
	    def_function(bname);
	}
	else if (is_AST(decl,"Decl:data"))
	{
	    if (decl.size() >= 2)
	    {
		expression_ref constrs = decl.sub()[1];
		for(const auto& constr: constrs.sub())
		{
		    int arity = 0;
		    auto type = constr;
		    if (is_AST(type,"TypeApply"))
		    {
			arity = type.size()-1;
			type = type.sub()[0];
		    }
		    assert(is_AST(type,"type_id"));
		    string cname = type.head().as_<AST_node>().value;
		    def_constructor(cname,arity);
		}
	    }
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

Module::Module(const expression_ref& E, const set<string>& lo)
    :language_options(lo)
{
    assert(is_AST(E,"Module"));
    assert(not module);
    module = E;

    parse_module(module, name, exports, body, impdecls, topdecls);

    assert(module);
}

std::ostream& operator<<(std::ostream& o, const Module& M)
{
    for(const auto& decl: M.topdecls.sub())
	o<<decl.sub()[0]<<" = "<<decl.sub()[1]<<")\n";
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

