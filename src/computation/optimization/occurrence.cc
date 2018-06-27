#include <iostream>
#include <unordered_map>
#include <list>
#include "computation/operations.H"
#include "computation/expression/expression.H"
#include "computation/expression/let.H"
#include "computation/expression/case.H"
#include "computation/expression/var.H"
#include "computation/expression/apply.H"
#include "computation/expression/lambda.H"
#include "computation/expression/trim.H"
#include "computation/expression/indexify.H"
#include "computation/expression/AST_node.H"
#include "let-float.H"
#include "occurrence.H"

#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/topological_sort.hpp>
#include <boost/graph/strong_components.hpp>

#include <boost/optional.hpp>
#include "simplifier.H"

// TODO: split out dependency analysis into own file.

typedef boost::adjacency_list< boost::vecS, boost::vecS, boost::bidirectionalS> Graph; 
typedef boost::graph_traits<Graph>::vertex_descriptor Vertex;
typedef boost::graph_traits<Graph>::edge_descriptor Edge_t;

using std::string;
using std::vector;
using std::list;
using std::pair;
using std::set;
using std::map;

using std::cerr;
using std::endl;


amount_t operator+(amount_t a1, amount_t a2)
{
    assert(a1 != amount_t::Unknown);
    assert(a2 != amount_t::Unknown);
    if (a1 == amount_t::None) return a2;
    if (a2 == amount_t::None) return a1;
    if (a1 == amount_t::Many or a2 == amount_t::Many) return amount_t::Many;
    if (a1 == amount_t::Once and a2 == amount_t::Once) return amount_t::Many;
    std::abort();
}

amount_t max(amount_t a1, amount_t a2)
{
    assert(a1 != amount_t::Unknown);
    assert(a2 != amount_t::Unknown);
    if (a1 == amount_t::None) return a2;
    if (a2 == amount_t::None) return a1;
    if (a1 == amount_t::Many or a2 == amount_t::Many) return amount_t::Many;
    if (a1 == amount_t::Once and a2 == amount_t::Once) return amount_t::Once; // <- This is the only difference
    std::abort();
}

// Merge_branch should MAX the work done, but ADD the code size.
void merge_occurrences_into(set<var>& free_vars1, const set<var>& free_vars2, bool alternate_branches = false)
{
    // Then consider free_vars2
    for(auto x: free_vars2)
    {
	auto it = free_vars1.find(x);

	// If the var is in both groups, we must modify its occurrence info
	if (it != free_vars1.end())
	{
	    x.is_loop_breaker = x.is_loop_breaker or it->is_loop_breaker;
	    if (alternate_branches)
		x.work_dup = max(x.work_dup, it->work_dup);
	    else
		x.work_dup = x.work_dup + it->work_dup;
	    x.code_dup = x.code_dup + it->code_dup;
	    if (x.context == var_context::argument or it->context == var_context::argument)
		x.context = var_context::argument;
	    else
		x.context = var_context::unknown;

	    free_vars1.erase(x);
	}

	free_vars1.insert(x);
    }
}

var remove_var_and_set_occurrence_info(var x, set<var>& free_vars)
{
    assert(not is_wildcard(x));

    // 1. Copy occurrence info
    auto x_iter = free_vars.find(x);
    if (x_iter == free_vars.end())
    {
	x.work_dup = amount_t::None;
	x.code_dup = amount_t::None;
	x.is_loop_breaker = false;
	x.context = var_context::unknown;
    }
    else
    {
	bool is_exported = x.is_exported;
	static_cast<occurrence_info&>(x) = *x_iter;
	x.is_exported = is_exported;
    }

    // 2. Remove var from set
    free_vars.erase(x);

    assert(x.code_dup != amount_t::Unknown and x.work_dup != amount_t::Unknown);

    return x;
}

var remove_var_and_set_occurrence_info(const expression_ref& E, set<var>& free_vars)
{
    var x = E.as_<var>();
    return remove_var_and_set_occurrence_info(x, free_vars);
}
// occur:: Expression -> (marked free_variables, marked Expression)

bool is_alive(const occurrence_info& x)
{
    return (x.is_exported or x.code_dup != amount_t::None);
}

Graph construct_directed_reference_graph(CDecls& decls, set<var>& free_vars)
{
    using namespace boost;
    const int L = decls.size();

    // 0. Initialize the graph and decls
    Graph graph(L);

    // 1. Mark vars referenced in the body as being alive by setting x.code_dup = Unknown
    vector<int> work;
    for(int i=0;i<L;i++)
    {
	auto& x = decls[i].first;
	x.code_dup = free_vars.count(x)?(amount_t::Unknown):(amount_t::None);
	if (is_alive(x))
	    work.push_back(i);
    }

    // 3. Discover reachable variables, analyze them, and record references between variables
    map<var,int> index_for_var;
    for(int i=0;i<L;i++)
	index_for_var.insert({decls[i].first,i});

    for(int k=0;k<work.size();k++)
    {
	int i = work[k];
	// 3.1 Analyze the bound statement
	set<var> free_vars_i;
	tie(decls[i].second, free_vars_i) = occurrence_analyzer(decls[i].second);

	// 3.2 Record occurrences
	merge_occurrences_into(free_vars, free_vars_i);

	// 3.3. Check if other variables j are referenced from the i-th variable.
	for(auto& x: free_vars_i)
	{
	    auto it = index_for_var.find(x);
	    if (it != index_for_var.end())
	    {
		int j = it->second;

		// 3.3.2 Add an edge from i -> j meaning "i references j"
		boost::add_edge(i, j, graph);

		// 3.3.3 Add variable j to the work list if we haven't put it on the list already
		if (not is_alive(decls[j].first))
		{
		    decls[j].first.code_dup = amount_t::Unknown;
		    work.push_back(j);
		}
	    }
	}
    }

    return graph;
}

Graph get_subgraph(const vector<int> vertices, const Graph& graph)
{
    Graph subgraph(vertices.size());
    for(int i=0;i<vertices.size();i++)
	for(int j=0;j<vertices.size();j++)
	    if (edge(vertices[i],vertices[j],graph).second)
		boost::add_edge(i, j, subgraph);
    return subgraph;
}

vector<vector<int>> get_ordered_strong_components(const Graph& graph)
{
    using namespace boost;
    const int L = num_vertices(graph);

    // 1. Label each vertex with its component
    vector<int> component_for_index(L);
    int C = strong_components(graph, make_iterator_property_map(component_for_index.begin(), get(vertex_index, graph)));

    // find live variables in each component
    vector<vector<int>> components(C);

    for(int i=0;i<L;i++)
    {
	int c = component_for_index[i];
	components[c].push_back(i);
    }

    return components;
}

vector<int> get_live_vars(const vector<int>& vars, const CDecls& decls)
{
    vector<int> live_vars;
    for(int var: vars)
    {
	if (is_alive(decls[var].first))
	    live_vars.push_back(var);
    }
    return live_vars;
}

vector<pair<vector<int>,Graph>> get_ordered_live_components(const Graph& graph, const CDecls& decls)
{
    vector<pair<vector<int>,Graph>> live_components;
    for(auto& component: get_ordered_strong_components(graph))
    {
	auto live_component = get_live_vars(component, decls);
	if (live_component.empty()) continue;

	live_components.push_back(pair<vector<int>,Graph>(std::move(live_component), get_subgraph(live_component, graph)));
    }

    return live_components;
}

int get_score(const CDecl& decl)
{
    auto& x = decl.first;
    auto& F = decl.second;

    if (is_reglike(F))
	return 4;
    else if (is_constructor(F.head()) or F.size() == 0)
	return 3;
    else if (x.pre_inline())
	return 1;
    else
	return 0;
}

// Find element of component with smallest score in sub_component.
int select_loop_breaker(const vector<int>& sub_component, const vector<int>& component, const CDecls& decls)
{
    std::function<int(int)> score_fn = [&](int k) {return get_score(decls[component[sub_component[k]]]);};
    return sub_component[argmin(sub_component.size(), score_fn)];
}

vector<int> topo_sort(const Graph& graph)
{
    using namespace boost;

    vector<Vertex> sorted_vertices;
    topological_sort(graph, std::back_inserter(sorted_vertices));

    vector<int> sorted_indices(sorted_vertices.size());
    for(int i=0;i<sorted_indices.size();i++)
	sorted_indices[i] = get(vertex_index, graph, sorted_vertices[i]);

    return sorted_indices;
}


// free_vars: (in) vars that are free in the body of the let statement.
//            (out) vars that are free in the let statement.
//
// maybe fixme: avoid doing any work that is O(#decls), only do work that is O(#live decls)?
//
//              HOWEVER. probably doing work that is C*#decls is OK if we don't analyze the bodies of the dead decls,
//               because we already did some work that is O(#decls to create the list of decls).
//
//              On the other hand, doing work that is O(#decls) in EACH iteration, makes that work O(#decls * #simplifier_passes)
//
//              Deal with this later.
//

vector<CDecls>
occurrence_analyze_decl_groups(const vector<CDecls>& decl_groups, set<var>& free_vars)
{
    vector<vector<CDecls>> output;
    for(const auto& decls: reverse(decl_groups))
	output.push_back(occurrence_analyze_decls(decls, free_vars));

    std::reverse(output.begin(), output.end());
    return flatten(std::move(output));
}

vector<CDecls>
occurrence_analyze_decls(CDecls decls, set<var>& free_vars)
{
    // 1. Determine which vars are alive or dead..
    // 2. Construct reference graph between (live) vars.
    auto graph = construct_directed_reference_graph(decls, free_vars);

    // 3. Copy use information into dummies in decls
    // 4. Remove declared vars from free_vars.
    for(int i=0;i<decls.size();i++)
    {
	auto& x = decls[i].first;
	if (is_alive(x))
	{
	    x = remove_var_and_set_occurrence_info(x, free_vars);
	    assert(is_alive(x));
	}
	else
	    assert(not free_vars.count(x));
    }

    // 5. Find strongly connected components
    vector<pair<vector<int>,Graph>> ordered_components = get_ordered_live_components(graph, decls);

    // 6. Break cycles in each component
    for(auto& component: ordered_components)
    {
	auto& component_indices = component.first;
	auto& component_graph = component.second;

        // 6.1 Break cycles in this component
	bool changed = true;
	while(changed)
	{
	    changed = false;

	    // find strongly connected components: every node is reachable from every other node
	    vector<vector<int>> sub_components = get_ordered_strong_components(component_graph);

	    // 6.1.1.  Break cycles in the first sub_component with a cycle
	    for(auto& sub_component: sub_components)
	    {
		// If the component is a single with no loop to itself, then we don't need to break any loops
		if (sub_component.size() == 1 and not edge(sub_component[0], sub_component[0], component_graph).second) continue;

		// Find the element of the component with the lowest score in the sub_component.
		int loop_breaker_component_index = select_loop_breaker(sub_component, component_indices, decls);
		int loop_breaker_index = component_indices[loop_breaker_component_index];

		// Remove incoming edges to the loop breaker from the component graph
		clear_in_edges(loop_breaker_component_index, component_graph);

		// Mark the variable as a loop breaker
		decls[loop_breaker_index]. first. is_loop_breaker = true;

		// Start
		changed = true;
		break;
	    }
	}

	// 6.2. Topo-sort elements of the component now that loops are broken
	component_indices = apply_indices(component_indices, topo_sort(component_graph));
    }

    // 7. Flatten the decl groups
    vector<CDecls> decls2;
    for(auto& component: ordered_components)
    {
	CDecls a_decls;
	for(int i: component.first)
	    a_decls.push_back(decls[i]);
	decls2.push_back(a_decls);
    }
    return decls2;
}

set<var> dup_work(set<var>& vars)
{
    set<var> vars2;
    for(auto x: vars)
    {
	if (x.work_dup == amount_t::Once)
	    x.work_dup = amount_t::Many;
	vars2.insert(x);
    }
    return vars2;
}

pair<expression_ref,set<var>> occurrence_analyzer(const expression_ref& E, var_context context)
{
    if (not E) return {E,set<var>{}};

    // 1. Var
    if (is_var(E))
    {
	var x = E.as_<var>();
	x.work_dup = amount_t::Once;
	x.code_dup = amount_t::Once;
	x.is_loop_breaker = false;
	x.context = context;
	return {E,{x}};
    }

    // 5. (partial) Literal constant.  Treat as 0-arg constructor.
    if (not E.size()) return {E,set<var>{}};

    // 2. Lambda (E = \x -> body)
    if (is_lambda(E.head()))
    {
	assert(E.size() == 2);

	// 1. Analyze the body and marks its variables
	set<var> free_vars;
	expression_ref body;
	tie(body, free_vars) = occurrence_analyzer(E.sub()[1]);

	// 2. Mark bound variable with occurrence info from the body
	// 3. Remove variable from free variables
	var x = remove_var_and_set_occurrence_info(E.sub()[0], free_vars);

	// 4. change Once -> OnceInLam / work=Many, code=Once
	return {lambda_quantify(x,body), dup_work(free_vars)};
    }

    // 6. Case
    expression_ref object;
    vector<expression_ref> patterns;
    vector<expression_ref> bodies;
    if (parse_case_expression(E, object, patterns, bodies))
    {
	// Analyze the object
	set<var> free_vars;
	tie(object, free_vars) = occurrence_analyzer(object);

	const int L = patterns.size();
	// Just normalize the bodies

	set<var> alts_free_vars;
	for(int i=0;i<L;i++)
	{
	    // Analyze the i-ith branch
	    set<var> alt_i_free_vars;
	    tie(bodies[i], alt_i_free_vars) = occurrence_analyzer(bodies[i]);

	    // Remove pattern vars from free variables
	    // Copy occurrence info into pattern variables
	    if (patterns[i].size())
	    {
		object_ptr<expression> pattern2 = patterns[i].as_expression().clone();
		for(int j=0;j < pattern2->size(); j++)
		{
		    if (not is_wildcard(pattern2->sub[j]))
		    {
			var x = remove_var_and_set_occurrence_info(pattern2->sub[j], alt_i_free_vars);
			pattern2->sub[j] = x; // use temporary to avoid deleting pattern2->sub[j]
		    }
		}
		patterns[i] = pattern2;
	    }

	    // Merge occurrences for this pattern into the occurrence for the whole set of alts.
	    merge_occurrences_into(alts_free_vars, alt_i_free_vars, true);
	}
	// We can avoid inlining directly into alternatives, since this might duplicate work.
        // merge_occurrences_into(free_vars, dup_work(alts_free_vars));
	merge_occurrences_into(free_vars, alts_free_vars);
	return {make_case_expression(object,patterns,bodies),free_vars};
    }

    // 4. Constructor, Operation (including Apply)
    if (is_constructor(E.head()) or is_apply(E.head()) or is_non_apply_operation(E.head()))
    {
	set<var> free_vars;
	object_ptr<expression> F = new expression(E.head());
	for(int i=0;i<E.size();i++)
	{
	    set<var> free_vars_i;
	    expression_ref arg_i;
	    auto context = (i==0 and is_apply(E.head())) ? var_context::unknown : var_context::argument;
	    tie(arg_i,free_vars_i) = occurrence_analyzer(E.sub()[i], context);
	    F->sub.push_back(arg_i);
	    merge_occurrences_into(free_vars, free_vars_i);
	}
	return {F,free_vars};
    }


    // 5. Let (let {x[i] = F[i]} in body)
    if (is_let_expression(E))
    {
	auto decls = let_decls(E);
	auto body = let_body(E);

	// 1. Analyze the body
	set<var> free_vars;
	tie(body, free_vars) = occurrence_analyzer(body);

	auto decls_groups = occurrence_analyze_decls(decls, free_vars);

	return {let_expression(decls_groups, body), free_vars};
    }

    throw myexception()<<"occurrence_analyzer: I don't recognize expression '"+ E.print() + "'";
}
