#include "computation/program.H"
#include "computation/module.H"
#include "myexception.H"
#include "computation/loader.H"
#include "models/model.H"
#include "expression/expression.H"
#include "expression/dummy.H"

using std::vector;
using std::set;
using std::pair;
using std::map;
using std::string;

bool contains_module(const vector<Module>& P, const string& module_name)
{
    return find_module(P,module_name) != -1;
}

int find_module(const vector<Module>& P, const string& module_name)
{
    for(int i=0;i<P.size();i++)
	if (P[i].name == module_name)
	    return i;
    return -1;
}

const Module& get_module(const vector<Module>& P, const string& module_name)
{
    int index = find_module(P,module_name);
    if (index == -1)
	throw myexception()<<"Progam does not contain module '"<<module_name<<"'";
    return P[index];
}

vector<string> module_names(const vector<Module>& P)
{
    vector<string> names;
    for(const auto& module: P)
	names.push_back(module.name);
    return names;
}

string module_names_path(const vector<Module>& P)
{
    return "["+join(module_names(P),",") +"]";
}

set<string> module_names_set(const vector<Module>& P)
{
    set<string> names;
    for(const auto& module: P)
	names.insert(module.name);
    return names;
}

static int count_module(const vector<Module>& P,const string& module_name)
{
    int count = 0;
    for(const auto& module: P)
	if (module.name == module_name)
	    count++;
    return count;
}

set<string> unresolved_imports(const vector<Module>& P)
{
    set<string> modules_to_add;

    // Add dependencies on modules
    for(const auto& module: P)
	for(const string& module_name: module.dependencies())
	    if (not contains_module(P, module_name))
		modules_to_add.insert(module_name);

    return modules_to_add;
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
	for(auto& imp_name: L.load_module(name).dependencies())
	{
	    int j = find_index(new_module_names, imp_name);
	    if (j != -1)
		boost::add_edge(vertices[i], vertices[j], graph);
	}
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

void check_dependencies(const module_loader& L, std::vector<Module>& P)
{
    for(int i=0;i<P.size();i++)
    {
	auto& M = P[i];
	for(auto& name: M.dependencies())
	{
	    int index = find_module(P, name);
	    assert(index != -1);
	    assert(index < i);
	}
    }
}

void desugar_and_optimize(const module_loader& L, vector<Module>& P)
{
    check_dependencies(L, P);

    // 2. Load missing modules, desugar, resolve names
    for(auto& module: P)
	try {
	    module.resolve_symbols(L, P);
	}
	catch (myexception& e)
	{
	    std::ostringstream o;
	    o<<"In module '"<<module.name<<"': ";
	    e.prepend(o.str());
	    throw e;
	}

    // 3. Load small declarations
    for(auto& module: P)
	try {
	    module.get_small_decls(P);
	}
	catch (myexception& e)
	{
	    std::ostringstream o;
	    o<<"In module '"<<module.name<<"': ";
	    e.prepend(o.str());
	    throw e;
	}

    // 4. Optimize
    for(auto& module: P)
	try {
	    module.optimize(L, P);
	}
	catch (myexception& e)
	{
	    std::ostringstream o;
	    o<<"In module '"<<module.name<<"': ";
	    e.prepend(o.str());
	    throw e;
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

	for(auto& import: L.load_module(module).dependencies())
	    modules_to_consider.push_back(import);
    }

    return new_module_names;
}

void add(const module_loader& L, std::vector<Module>& P, const std::string& name)
{
    auto new_names = new_module_names(L, module_names_set(P), {name});

    vector<string> new_names1;
    for(auto& name: new_names)
	new_names1.push_back(name);

    vector<string> new_names2 = sort_modules_by_dependencies(L, new_names1);

    for(auto& name: new_names2)
    {
	check_dependencies(L, P);
	P.push_back(L.load_module(name));
	check_dependencies(L, P);
    }

    // 4. Desugar and optimize modules
    desugar_and_optimize(L, P);
}

void add(const module_loader& L, vector<Module>& P, const vector<string>& module_names)
{
    for(const auto& name: module_names)
	add(L, P, name);
}

void add(const module_loader& L, vector<Module>& P, const vector<Module>& modules)
{
    for(const auto& M: modules)
	add(L, P, M);
}

void add(const module_loader& L, vector<Module>& P, const Module& M)
{
    for(auto& name: M.dependencies())
	add(L, P, name);

    // 1. Check that the program doesn't already contain this module name.
    if (contains_module(P, M.name))
	throw myexception()<<"Trying to add duplicate module '"<<M.name<<"' to program "<<module_names_path(P);

    // 2. Actually add the module.
    P.push_back( M );

#ifndef NDEBUG
    // 3. Assert that every module exists only once in the list.
    for(const auto& module: P)
	assert(count_module(P, module.name) == 1);
#endif

    // 4. Import any modules that are (transitively) implied by the ones we just loaded.
    desugar_and_optimize(L, P);
}

bool is_declared(const vector<Module>& modules, const string& qvar)
{
    if (is_haskell_builtin_con_name(qvar)) return true;

    if (not is_qualified_symbol(qvar))
	throw myexception()<<"Can't search program for non-builtin, unqualified varid '"<<qvar<<"'";

    for(const auto& module: modules)
	if (module.is_declared_local(qvar))
	    return true;

    return false;
}

map<string,string> get_simplified_names(const set<string>& names)
{
    // 1. Construct mapping from unqualified names to qualified names.
    std::multimap<string,string> aliases;
    for(const string& name: names)
	aliases.insert({get_unqualified_name(name), name});

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


expression_ref map_symbol_names(const expression_ref& E, const std::map<string,string>& simplify)
{
    if (not E.size())
    {
	if (is_qualified_dummy(E))
	{
	    auto x = E.as_<dummy>();
	    auto loc = simplify.find(x.name);
	    if (loc != simplify.end())
		return dummy(loc->second);
	}
	return E;
    }

    object_ptr<expression> V = E.as_expression().clone();
    for(int i=0;i<E.size();i++)
	V->sub[i] = map_symbol_names(V->sub[i], simplify);
    return V;
}
