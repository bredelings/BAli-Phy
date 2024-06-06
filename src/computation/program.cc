#include "computation/program.H"
#include "computation/haskell/ids.H"
#include "computation/module.H"
#include "computation/loader.H"
#include "models/model.H"
#include "expression/var.H"
#include "util/myexception.H"
#include "util/string/join.H"
#include "util/mapping.H"
#include "util/log-level.H"
#include "computation/varinfo.H"
#include "computation/expression/lambda.H"
#include "computation/expression/case.H"
#include "computation/typecheck/kind.H"
#include "computation/optimization/occurrence.H"
#include "computation/machine/graph_register.H"
#include "util/assert.hh"

using std::vector;
using std::set;
using std::pair;
using std::map;
using std::string;
using std::shared_ptr;
using std::optional;

vector<shared_ptr<CompiledModule>>& Program::modules()
{
    return *this;
}

const std::shared_ptr<module_loader>& Program::get_module_loader() const
{
    return loader;
}

symbol_info seq_info()
{
    // 1. seq = \x y -> case x of _ -> y
    auto x = Core::Var("x");
    auto y = Core::Var("y");
    Core::Exp code = lambda_quantify(vector{x,y},make_case_expression(x,{{var(-1)}},{{y}}));

    // 2. seq :: forall a b. a -> b -> b
    TypeVar a({noloc,"a"});
    a.kind = kind_type();
    TypeVar b({noloc,"b"});
    b.kind = kind_type();
    Type type = ForallType({a,b},function_type({a,b},b));

    // 3. infixr 0 seq
    fixity_info fixity{Hs::Fixity::infixr, 0};

    // 4. always unfold to code.
    auto info = std::make_shared<VarInfo>();
    info->always_unfold = true;
    info->unfolding = code;

    // 5. create the symbol
    auto seq = symbol_info{"seq", symbol_type_t::variable, {}, 2, fixity};
    seq.type = type;
    seq.var_info = info;

    return seq;
}

shared_ptr<CompiledModule> compiler_prim_module()
{
    // 1. Create module Compiler.Prim
    auto m = std::make_shared<Module>("Compiler.Prim");

    // 2. No implicit Prelude
    m->language_extensions.set_extension(LangExt::ImplicitPrelude, false);

    // 3. Add seq.
    auto seq = seq_info();
    CDecls value_decls;
    if (seq.var_info and seq.var_info->unfolding)
    {
        auto& code = seq.var_info->unfolding;
        value_decls.push_back({var("Compiler.Prim.seq"), code});
        // Unfoldings must be occurrence-analyzed so that we can inline them.
        auto [code2, _] = occurrence_analyzer(*m, code);
        code = code2;
    }
    m->declare_symbol(seq);

    // 4. Copy symbols to the for-export maps.
    m->perform_exports();

    auto cm = std::make_shared<CompiledModule>(m);
    cm->finish_value_decls(value_decls);

    return cm;
}


Program::Program(const std::shared_ptr<module_loader>& L)
    :loader(L)
{
    modules().push_back( compiler_prim_module() );
}

optional<int> Program::find_module(const string& module_name) const
{
    int i=0;
    for(auto& m: (*this))
    {
        if (m->name() == module_name)
            return i;
        i++;
    }

    return {};
}

bool Program::contains_module(const string& module_name) const
{
    return find_module(module_name).has_value();
}

shared_ptr<const CompiledModule> Program::get_module(const string& module_name) const
{
    for(auto& m: (*this))
        if (m->name() == module_name)
            return m;

    throw myexception()<<"Program does not contain module '"<<module_name<<"'";
}

/*
shared_ptr<Module> Program::get_module(const string& module_name)
{
    for(auto& m: (*this))
        if (m->name == module_name)
            return m;

    throw myexception()<<"Program does not contain module '"<<module_name<<"'";
}
*/

vector<string> Program::module_names() const
{
    vector<string> names;
    for(const auto& mod: *this)
	names.push_back(mod->name());
    return names;
}

string Program::module_names_path() const
{
    return "["+join(module_names(),",") +"]";
}

set<string> Program::module_names_set() const
{
    set<string> names;
    for(const auto& mod: *this)
	names.insert(mod->name());
    return names;
}

int Program::count_module(const string& module_name) const
{
    int count = 0;
    for(const auto& mod: *this)
	if (mod->name() == module_name)
	    count++;
    return count;
}

#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/topological_sort.hpp>
#include <boost/graph/strong_components.hpp>

vector<string> sort_modules_by_dependencies(const module_loader& L, vector<string>& new_module_names)
{
    using namespace boost;

    typedef adjacency_list< boost::vecS, boost::vecS, boost::bidirectionalS> Graph;
    typedef graph_traits<Graph>::vertex_descriptor Vertex;

    // Construct the dependency graph.  (i,j) means that i imports j
    Graph graph;
    vector<adjacency_list<>::vertex_descriptor> vertices;
    for(int i=0; i<new_module_names.size(); i++)
	vertices.push_back( add_vertex(graph) );

    for(int i=0;i<new_module_names.size();i++)
    {
	auto& name = new_module_names[i];
	for(auto& import_name: L.load_module(name)->dependencies())
	    if (auto j = find_index(new_module_names, import_name))
		boost::add_edge(vertices[i], vertices[*j], graph);
    }

    // Find connected components
    vector<int> component(new_module_names.size());
    int num = strong_components(graph, make_iterator_property_map(component.begin(), get(vertex_index, graph)));
    vector<vector<int>> components(num);
    for(int i=0;i<new_module_names.size();i++)
    {
	int c = component[i];
	components[c].push_back(i);
    }

    // Check for cyclic module dependencies
    for(auto& vs: components)
    {
	if (vs.size() == 1) continue;

	myexception e;
	e<<"Cycle in module dependencies:";
	for(auto& m: vs)
	    e<<" "<<new_module_names[m];
	throw e;
    }

    // 5. Sort the vertices
    vector<Vertex> sorted_vertices;
    topological_sort(graph, std::back_inserter(sorted_vertices));

    // 6. Make a program in the right order
    vector<string> new_module_names2;
    for(int i=0;i<new_module_names.size();i++)
    {
	int j = get(vertex_index,graph,sorted_vertices[i]);
	new_module_names2.push_back(new_module_names[j]);
    }

    return new_module_names2;
}

void Program::check_dependencies()
{
    for(int i=0;i<modules().size();i++)
    {
	auto& M = modules()[i];
	for(auto& name: M->dependencies())
	{
	    auto index = find_module(name);
	    assert(index.has_value());
	    assert(*index < i);
	}
    }
}

set<string> new_module_names(const module_loader& L, const set<string>& old_module_names, const set<string>& modules_to_import)
{
    vector<string> modules_to_consider;
    for(auto& module: modules_to_import)
	modules_to_consider.push_back(module);

    set<string> new_module_names;
    for(int i=0; i<modules_to_consider.size(); i++)
    {
	auto& module = modules_to_consider[i];

	// This one is already included
	if (old_module_names.count(module) or new_module_names.count(module)) continue;

	// Add it to the list of new modules
	new_module_names.insert(module);

	for(auto& import: L.load_module(module)->dependencies())
	    modules_to_consider.push_back(import);
    }

    return new_module_names;
}

void Program::add(const std::string& name)
{
    auto new_names = new_module_names(*loader, module_names_set(), {name});

    vector<string> new_names1;
    for(auto& name: new_names)
	new_names1.push_back(name);

    vector<string> new_names2 = sort_modules_by_dependencies(*loader, new_names1);

    // Add the new modules, processing them as we go.
    for(auto& name: new_names2)
	add(loader->load_module(name));
}

void Program::add(const vector<string>& module_names)
{
    for(const auto& name: module_names)
	add(name);
}

void Program::add(const vector<shared_ptr<Module>>& modules)
{
    for(const auto& M: modules)
	add(M);
}

void Program::add(const shared_ptr<Module>& M)
{
    check_dependencies();

    for(auto& name: M->dependencies())
	add(name);

    // 1. Check that the program doesn't already contain this module name.
    if (contains_module(M->name))
	throw myexception()<<"Trying to add duplicate module '"<<M->name<<"' to program "<<module_names_path();

    // 2. Actually add the module.
    try {
	push_back( compile(*this, M) );
	check_dependencies();
    }
    catch (myexception& e)
    {
	std::ostringstream o;
	o<<"In module '"<<M->name<<"':\n";
	e.prepend(o.str());
	throw;
    }

#ifndef NDEBUG
    // 3. Assert that every module exists only once in the list.
    for(const auto& mod: modules())
	assert(count_module(mod->name()) == 1);
#endif
}

map<string,string> project_unambiguous_names(const set<string>& names, const std::function<string(const string&)>& project)
{
    // 1. Construct mapping from unqualified names to qualified names.
    std::multimap<string,string> aliases;
    for(const string& name: names)
	aliases.insert({project(name), name});

    // 2. Invert the mapping if the unqualified name maps to only 1 qualified name.
    map<string,string> simplified;
    for(auto current = aliases.begin();current != aliases.end();)
    {
	int count = 1;
	auto next = current;
	next++;
	while(next != aliases.end() and next->first == current->first)
	{
	    // The same qualified name should not occur twice.
	    assert(next->second != current->second);
	    count++;
	    next++;
	}

	if (count == 1)
	    simplified[current->second] = current->first;

	current = next;
    }

    return simplified;
}

map<string,string> compose(const map<string,string>& m1, const map<string,string>& m2)
{
    auto m3 = m2;
    for(auto& [name1,name2]: m1)
    {
	if (m2.contains(name1))
	    m3.erase(name1);
	    
	if (m2.contains(name2))
	    m3.insert({name1,m2.at(name2)});
	else
	    m3.insert({name1,name2});
    }
    return m3;
}

set<string> range(const map<string,string>& m)
{
    set<string> S;
    for(auto& [_,s]: m)
	S.insert(s);
    return S;
}

string remove_suffix(const string& s, char c)
{
    if (s.empty()) return s;

    int i = s.size()-1;
    while(std::isdigit(s[i]) and i > 0)
	i--;

    // We stripped the suffix, and there's something left.
    if (s[i] == c and i > 0)
    {
	if (s[i-1] == '.' and i > 1 and is_haskell_id_char(s[i-2]))
	{
	    return s;
	}
	else
	    return s.substr(0,i);
    }
    else
	return s;
}

map<string,string> get_simplified_names(const set<string>& names)
{
    auto m1 = project_unambiguous_names(names, [](const string& s) {return remove_suffix(s,'#');});
    auto m2 = project_unambiguous_names(range(m1), [](const string& s) {return remove_suffix(s,'_');});
    auto m3 = project_unambiguous_names(range(m2), [](const string & s)
	{
	    auto n = get_unqualified_name(s);
	    if (n.starts_with('$') or n.starts_with('#'))
		return s;
	    else
		return n;
	});

    return compose(m1, compose(m2,m3));
}


expression_ref map_symbol_names(const expression_ref& E, const std::map<string,string>& simplify)
{
    if (not E.size())
    {
	if (is_qualified_var(E))
	{
	    auto x = E.as_<var>();
	    auto loc = simplify.find(x.name);
	    if (loc != simplify.end())
		return var(loc->second);
	}
	return E;
    }

    object_ptr<expression> V = E.as_expression().clone();
    for(int i=0;i<E.size();i++)
	V->sub[i] = map_symbol_names(V->sub[i], simplify);
    return V;
}


void execute_program(std::unique_ptr<Program> P)
{
    // Creating an object pointer initializes the refcount to 1.
    object_ptr<reg_heap> R(new reg_heap( std::move(P) ) );
#ifdef NDEBUG
    R->program.reset();
    R->identifiers.clear();
#endif
    R->run_main();
}

std::unique_ptr<Program> load_program_from_file(const std::shared_ptr<module_loader>& L, const std::filesystem::path& filename)
{
    auto m = L->load_module_from_file(filename);

    auto P = std::make_unique<Program>(L);
    P->add(m);
    P->main = m->name + ".main";

    return P;
}

void execute_file(const std::shared_ptr<module_loader>& L, const std::filesystem::path& filename)
{
    execute_program( load_program_from_file(L, filename) );
}
