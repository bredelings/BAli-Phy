#include <iostream>
#include <unordered_map>
#include "computation/operations.H"
#include "computation/loader.H"
#include "computation/expression/expression.H"
#include "computation/expression/let.H"
#include "computation/expression/case.H"
#include "computation/expression/dummy.H"
#include "computation/expression/apply.H"
#include "computation/expression/lambda.H"
#include "computation/expression/trim.H"
#include "computation/expression/indexify.H"
#include "computation/expression/AST_node.H"
#include "let-float.H"

#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/topological_sort.hpp>
#include <boost/graph/strong_components.hpp>

#include <boost/optional.hpp>
#include "simplifier.H"

// TODO: split out dependency analyses into own file.
// TODO: split out occurrence analysis into own file.

typedef boost::adjacency_list< boost::vecS, boost::vecS, boost::bidirectionalS> Graph; 
typedef boost::graph_traits<Graph>::vertex_descriptor Vertex;
typedef boost::graph_traits<Graph>::edge_descriptor Edge_t;

using std::string;
using std::vector;
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

int simple_size(const expression_ref& E)
{
    if (is_dummy(E))
	return 0;

    else if (E.size() == 0)
	return 1;

    else if (is_constructor(E.head()))
    {
	for(auto& x: E.sub())
	    assert(is_dummy(x));
	return 1;
    }

    else if (is_apply(E.head()))
	return E.size() + simple_size(E.sub()[0]);

    else if (is_lambda(E.head()))
	return simple_size(E.sub()[1]);

    else if (is_let_expression(E))
    {
	int size = 1 + simple_size(E.sub()[1]);

	for(auto& decl: E.sub()[0].sub())
	    size += simple_size(decl.sub()[1]);

	return size;
    }
    else if (is_case(E))
    {
	expression_ref object;
	vector<expression_ref> patterns;
	vector<expression_ref> bodies;
	parse_case_expression(E, object, patterns, bodies);
	int alts_size = simple_size(bodies[0]);
	for(int i=1;i<bodies.size();i++)
	    alts_size = std::max(alts_size, simple_size(bodies[i]));
	return 1 + simple_size(object) + alts_size;
    }
    else if (is_non_apply_operation(E.head()))
    {
	for(auto& x: E.sub())
	    assert(is_dummy(x));
	return 1;
    }

    std::abort();
}

// Merge_branch should MAX the work done, but ADD the code size.
void merge_occurrences_into(set<dummy>& free_vars1, const set<dummy>& free_vars2, bool alternate_branches = false)
{
    // Then consider free_vars2
    for(auto var: free_vars2)
    {
	auto it = free_vars1.find(var);

	// If the var is in both groups, we must modify its occurrence info
	if (it != free_vars1.end())
	{
	    var.is_loop_breaker = var.is_loop_breaker or it->is_loop_breaker;
	    if (alternate_branches)
		var.work_dup = max(var.work_dup, it->work_dup);
	    else
		var.work_dup = var.work_dup + it->work_dup;
	    var.code_dup = var.code_dup + it->code_dup;
	    if (var.context == var_context::argument or it->context == var_context::argument)
		var.context = var_context::argument;
	    else
		var.context = var_context::unknown;

	    free_vars1.erase(var);
	}

	free_vars1.insert(var);
    }
}

dummy remove_var_and_set_occurrence_info(dummy x, set<dummy>& free_vars)
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

dummy remove_var_and_set_occurrence_info(const expression_ref& var, set<dummy>& free_vars)
{
    dummy x = var.as_<dummy>();
    return remove_var_and_set_occurrence_info(x, free_vars);
}
// occur:: Expression -> (marked free_variables, marked Expression)

bool is_alive(const occurrence_info& x)
{
    return (x.is_exported or x.code_dup != amount_t::None);
}

Graph construct_directed_reference_graph(CDecls& decls, set<dummy>& free_vars)
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
    map<dummy,int> index_for_var;
    for(int i=0;i<L;i++)
	index_for_var.insert({decls[i].first,i});

    for(int k=0;k<work.size();k++)
    {
	int i = work[k];
	// 3.1 Analyze the bound statement
	set<dummy> free_vars_i;
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
occurrence_analyze_decls(CDecls decls, set<dummy>& free_vars)
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

set<dummy> dup_work(set<dummy>& vars)
{
    set<dummy> vars2;
    for(auto var: vars)
    {
	if (var.work_dup == amount_t::Once)
	    var.work_dup = amount_t::Many;
	vars2.insert(var);
    }
    return vars2;
}

pair<expression_ref,set<dummy>> occurrence_analyzer(const expression_ref& E, var_context context)
{
    if (not E) return {E,set<dummy>{}};

    // 1. Var
    if (is_dummy(E))
    {
	dummy var = E.as_<dummy>();
	var.work_dup = amount_t::Once;
	var.code_dup = amount_t::Once;
	var.is_loop_breaker = false;
	var.context = context;
	return {E,{var}};
    }

    // 5. (partial) Literal constant.  Treat as 0-arg constructor.
    if (not E.size()) return {E,set<dummy>{}};

    // 2. Lambda (E = \x -> body)
    if (is_lambda(E.head()))
    {
	assert(E.size() == 2);

	// 1. Analyze the body and marks its variables
	set<dummy> free_vars;
	expression_ref body;
	tie(body, free_vars) = occurrence_analyzer(E.sub()[1]);

	// 2. Mark bound variable with occurrence info from the body
	// 3. Remove variable from free variables
	dummy x = remove_var_and_set_occurrence_info(E.sub()[0], free_vars);

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
	set<dummy> free_vars;
	tie(object, free_vars) = occurrence_analyzer(object);

	const int L = patterns.size();
	// Just normalize the bodies

	set<dummy> alts_free_vars;
	for(int i=0;i<L;i++)
	{
	    // Analyze the i-ith branch
	    set<dummy> alt_i_free_vars;
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
			dummy x = remove_var_and_set_occurrence_info(pattern2->sub[j], alt_i_free_vars);
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
	set<dummy> free_vars;
	object_ptr<expression> F = new expression(E.head());
	for(int i=0;i<E.size();i++)
	{
	    set<dummy> free_vars_i;
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
	set<dummy> free_vars;
	tie(body, free_vars) = occurrence_analyzer(body);

	auto decls_groups = occurrence_analyze_decls(decls, free_vars);

	return {let_expression(decls_groups, body), free_vars};
    }

    throw myexception()<<"occurrence_analyzer: I don't recognize expression '"+ E.print() + "'";
}

expression_ref analyze_occurrence(const expression_ref& E)
{
    auto result = occurrence_analyzer(E);
    return result.first;
}


struct substitution_range;
struct substitution: public map<dummy, substitution_range>
{
    using map::map;
};

struct substitution_range
{
    expression_ref E;
    const substitution* S = nullptr;
    substitution_range(const expression_ref& e):E(e) {}
    substitution_range(const expression_ref& e, const substitution& s):E(e),S(&s) {}
};

typedef pair<expression_ref,occurrence_info> bound_variable_info;

typedef map<dummy, bound_variable_info> in_scope_set;

bool is_trivial(const expression_ref& E)
{
    return is_reglike(E);
}

void bind_var(in_scope_set& bound_vars, const dummy& x, const expression_ref& E)
{
    assert(not bound_vars.count(x));
    assert(x.work_dup != amount_t::Unknown);
    assert(x.code_dup != amount_t::Unknown);
    bound_vars.insert({x,{E,x}});
}

void unbind_var(in_scope_set& bound_vars, const dummy& x)
{
    assert(bound_vars.count(x));
    bound_vars.erase(x);
}

bound_variable_info rebind_var(in_scope_set& bound_vars, const dummy& x, const expression_ref& E)
{
    bound_variable_info old_binding = bound_vars.at(x);
    unbind_var(bound_vars,x);
    dummy x2 = x;
    static_cast<occurrence_info&>(x2) = old_binding.second;
    bind_var(bound_vars,x2,E);
    return old_binding;
}

void bind_decls(in_scope_set& bound_vars, const CDecls& decls)
{
    for(const auto& decl: decls)
	bind_var(bound_vars, decl.first, decl.second);
}

void unbind_decls(in_scope_set& bound_vars, const CDecls& decls)
{
    for(const auto& decl: decls)
	unbind_var(bound_vars, decl.first);
}

class inline_context
{
    expression_ref data;
public:
    inline_context prev_context() const {
	if (data)
	    assert(data.head().is_a<AST_node>());
	return inline_context(data.sub()[1]);
    }

    bool is_case_object() const {return is_AST(data,"case_object");}
    bool is_apply_object() const {return is_AST(data,"apply_object");}
    bool is_argument() const {return is_AST(data,"argument");}
    bool is_unknown() const {return is_AST(data,"unknown");}
    bool is_null() const {return not data;}
    expression_ref get_expression() const {return data.sub()[0];};

    inline_context() {};
    inline_context(const expression_ref& E):data(E) {}
    inline_context(const string& s, const expression_ref E, const inline_context& context)
	:data({AST_node(s),{E, context.data}}) {}
};

int num_arguments(inline_context context)
{
    int num = 0;
    while(context.is_apply_object())
    {
	num++;
	context = context.prev_context();
    }
    return num;
}

inline_context case_object_context(const expression_ref E, const inline_context& context)
{
    assert(is_case(E));
    return inline_context("case_object",E,context);
}

inline_context apply_object_context(const expression_ref E, inline_context context)
{
    assert(E.head().is_a<Apply>());
    for(int i=E.size()-1;i>=1;i--)
    {
	context = inline_context("apply_object", E.sub()[i], context);
    }
    return context;
}

inline_context argument_context(const inline_context& context)
{
    return inline_context("argument", {}, context);
}

inline_context unknown_context()
{
    return inline_context("unknown", {}, {});
}

inline_context remove_arguments(inline_context context, int n)
{
    for(int i=0;i<n;i++)
    {
	if (not context.is_apply_object())
	    throw myexception()<<"Trying to remove "<<n<<" applications from context, but it only had "<<i<<".";
	context = context.prev_context();
    }
    return context;
}

int get_n_lambdas1(const expression_ref& E)
{
    expression_ref E2 = E;
    int n = 0;
    assert(E2.head().type() != lambda2_type);
    while(E2.head().type() == lambda_type)
    {
	E2 = E2.sub()[1];
	n++;
    }
    return n;
}

expression_ref peel_n_lambdas1(const expression_ref& E, int n)
{
    expression_ref E2 = E;
    for(int i=0;i<n;i++)
    {
	assert(E2.head().type() == lambda_type);
	assert(E2.head().type() != lambda2_type);
	E2 = E2.sub()[1];
    }
    return E2;
}
      
int nodes_size(const expression_ref& E);

bool no_size_increase(const expression_ref& rhs, const inline_context& context)
{
    // If rhs is a variable, then there's no size increase
    if (is_trivial(rhs)) return true;

    // If we are inlining a constant into a case object, then there will eventually be no size increase... right?
    if (context.is_case_object() and is_WHNF(rhs))
    {
	assert(not rhs.is_a<lambda>());
	return true;
    }

    // If we are inlining a function body that is smaller than its call (e.g. @ . f x ===> @ (\a b -> @ a b) f x ===> let {a=f;b=x} in @ a b ===> @ f x)
    // (e.g. @ $ f ===> @ (\a b -> @ a b) f ===> let a=f in (\b -> @ a b)  ===> (\b -> @ f b) ... wait isn't that the same as f?
    if (context.is_apply_object() and is_WHNF(rhs))
    {
	int n_args_supplied = num_arguments(context);
	assert(n_args_supplied >= 1);
	int n_args_needed = get_n_lambdas1(rhs);
	int n_args_used = std::min(n_args_needed, n_args_supplied);

	int size_of_call = 1 + n_args_supplied;
	auto body = peel_n_lambdas1(rhs, n_args_used);
	int size_of_body = simple_size(body);

	if (size_of_body <= size_of_call) return true;
    }

    return false;
}

bool very_boring(const inline_context& context)
{
    if (context.is_case_object()) return false;

    if (context.is_apply_object()) return false;

    // This avoids substituting into constructor (and function) arguments.
    return true;
}


bool boring(const expression_ref& rhs, const inline_context& context)
{
    // if the rhs is applied only to variables with unknown value AND ...

    // ... after consuming all the arguments we need, the result is very_boring.
    {
	int n_args_needed = get_n_lambdas1(rhs);
	if (num_arguments(context) >= n_args_needed)
	{
	    auto context2 = remove_arguments(context, n_args_needed);
	    if (not very_boring(context2)) return false;
	}
    }


    return true;
}

bool small_enough(const simplifier_options& options,const expression_ref& rhs, const inline_context& context)
{
    double body_size = simple_size(rhs);

    double size_of_call = 1 + num_arguments(context);

    int discounts = 0;

    return (body_size - size_of_call - options.keenness*discounts <= options.inline_threshhold);
}

bool do_inline_multi(const simplifier_options& options, const expression_ref& rhs, const inline_context& context)
{
    if (no_size_increase(rhs,context)) return true;

    if (boring(rhs,context)) return false;

    return small_enough(options, rhs,context);
}

bool evaluates_to_bottom(const expression_ref& rhs)
{
    return false;
}

bool whnf_or_bottom(const expression_ref& rhs)
{
    return is_WHNF(rhs) or evaluates_to_bottom(rhs);
}

bool do_inline(const simplifier_options& options, const expression_ref& rhs, const occurrence_info& occur, const inline_context& context)
{
    // LoopBreaker
    if (occur.is_loop_breaker)
	return false;
    
    // Function and constructor arguments
    else if (context.is_argument() and not is_trivial(rhs))
	return false;

    // OnceSafe
    else if (occur.pre_inline())
    {
	if (options.pre_inline_unconditionally and not occur.is_exported)
	    throw myexception()<<"Trying to inline OnceSafe variable!";
	else
	    return true;
    }

    // If its "trivial" but not a variable, we should substitute if we can.
    if (is_WHNF(rhs) and rhs.size() == 0)
	return true;

    // MultiSafe
    else if (occur.work_dup == amount_t::Once and occur.code_dup == amount_t::Many)
	return do_inline_multi(options, rhs, context);

    // OnceUnsafe
    else if (occur.work_dup == amount_t::Many and occur.code_dup == amount_t::Once)
	return whnf_or_bottom(rhs) and (no_size_increase(rhs,context) or not very_boring(context));

    // OnceUnsafe
    else if (occur.work_dup == amount_t::Once and occur.code_dup == amount_t::Once and occur.context == var_context::argument)
	return whnf_or_bottom(rhs) and (no_size_increase(rhs,context) or not very_boring(context));

    // MultiUnsafe
    else if (occur.work_dup == amount_t::Many and occur.code_dup == amount_t::Many)
	return whnf_or_bottom(rhs) and do_inline_multi(options, rhs, context);

    std::abort();
}

expression_ref simplify(const simplifier_options& options, const expression_ref& E, const substitution& S, in_scope_set& bound_vars, const inline_context& context);

// Do we have to explicitly skip loop breakers here?
expression_ref consider_inline(const simplifier_options& options, const expression_ref& E, in_scope_set& bound_vars, const inline_context& context)
{
    dummy x = E.as_<dummy>();

    const auto& binding = bound_vars.at(x);

//    std::cerr<<"Considering inlining "<<E.print()<<" -> "<<binding.first<<" in context "<<context.data<<std::endl;
    
    // 1. If there's a binding x = E, and E = y for some variable y
    if (binding.first and do_inline(options, binding.first, binding.second, context))
	return simplify(options, binding.first, {}, bound_vars, context);
    else
	return E;
}

dummy get_new_name(dummy x, const in_scope_set& bound_vars)
{
    auto it = bound_vars.find(x);

    // If bound_vars doesn't contain x, then no need to change anything.
    if (it == bound_vars.end()) return x;

    x.index = bound_vars.size();
    while(bound_vars.count(x) and x.index > 0)
	x.index++;
    
    if (x.index <= 0) abort();
    
    return x;
}

dummy get_new_name(const expression_ref& var, const in_scope_set& bound_vars)
{
    return get_new_name(var.as_<dummy>(), bound_vars);
}

dummy rename_and_bind_var(const expression_ref& var, substitution& S, in_scope_set& bound_vars)
{
    dummy x = var.as_<dummy>();
    assert(x.code_dup != amount_t::Unknown);
    assert(not is_wildcard(x));
    expression_ref var2 = get_new_name(var, bound_vars);

    // 1. If x is NOT in the bound set, then erase x from the substitution (if it's there)
    if (var == var2)
	S.erase(x);
    // 2. If x IS in the bound set, add a substitution from x --> x2then erase x from the substitution (if it's there)
    else
    {
	S.erase(x);
	S.insert({x, var2});
    }

    dummy x2 = var2.as_<dummy>();
    bind_var(bound_vars, x2, {});

    return x2;
}

bool is_identity_case(const expression_ref& object, const vector<expression_ref>& patterns, const vector<expression_ref>& bodies)
{
    assert(patterns.size() == bodies.size());
    for(int i=0;i<patterns.size();i++)
    {
	if (is_wildcard(patterns[i]))
	{
	    if (bodies[i] != object) return false;
	}
	else if (is_dummy(patterns[i].head()))
	{
	    assert(not patterns[i].size());
	    if (bodies[i] != patterns[i] and bodies[i] != object) return false;
	}
	else if (patterns[i] != bodies[i]) return false;
    }

    return true;
}


void get_pattern_dummies(const expression_ref& pattern, set<dummy>& vars)
{
    if (is_dummy(pattern))
    {
	auto& x = pattern.as_<dummy>();
	if (not x.is_wildcard())
	    vars.insert(x);
    }
    else if (pattern.size() > 0)
	for(auto& y: pattern.sub())
	    get_pattern_dummies(y, vars);
}


// Get a list of patterns.size() names for let-bound variables that don't alias any bound vars  in patterns or patterns2
vector<dummy> get_body_function_names(in_scope_set& bound_vars, const vector<expression_ref>& patterns, const vector<expression_ref>& patterns2)
{
    vector<dummy> lifted_names;

    int orig_size = bound_vars.size();

    // 1. Get the list of dummies to avoid
    set<dummy> avoid;
    for(auto& pattern: patterns)
	get_pattern_dummies(pattern, avoid);
    for(auto& pattern: patterns2)
	get_pattern_dummies(pattern, avoid);

    // 2. Bind the unbound ones into scope
    set<dummy> added;
    for(auto& x:avoid)
	if (not bound_vars.count(x))
	{
	    added.insert(x);
	    bind_var(bound_vars, x, {});
	}

    // 3. Make some new names, putting them into scope as we go.
    dummy z("_cc");
    z.work_dup = amount_t::Many;
    z.code_dup = amount_t::Many;
    for(int i=0;i<patterns.size();i++)
    {
	z = get_new_name(z, bound_vars);
	lifted_names.push_back(z);
	bind_var(bound_vars, z, {});
    }
    assert(bound_vars.size() == orig_size + added.size() + patterns.size());

    // 4. Unbind the new names from scope
    for(int i=0;i<lifted_names.size();i++)
	unbind_var(bound_vars, lifted_names[i]);

    // 5. Unbind the names from the patterns from scope too.
    for(auto& var: added)
	unbind_var(bound_vars, var);

    // 6. The scope size should now be the same size as when we started.
    assert(bound_vars.size() == orig_size);

    return lifted_names;
}

bool is_used_var(const dummy& x)
{
    return (not x.is_wildcard() and x.code_dup != amount_t::None);
}

bool is_used_var(const expression_ref& x)
{
    if (not is_dummy(x)) return false;
    return is_used_var(x.as_<dummy>());
}

vector<dummy> get_used_vars(const expression_ref& pattern)
{
    vector<dummy> used;

    if (is_used_var(pattern))
	used.push_back(pattern.as_<dummy>());
    else if (pattern.is_expression())
	for(auto& var: pattern.sub())
	    if (is_used_var(var.as_<dummy>()))
		used.push_back(var.as_<dummy>());

    return used;
}

bool has_used_vars(const expression_ref& pattern)
{
    if (is_used_var(pattern))
	return true;
    else if (pattern.is_expression())
	for(auto& var: pattern.sub())
	    if (is_used_var(var.as_<dummy>()))
		return true;

    return false;
}

// Check if all case branches refer to the same constant expression that does not reference any pattern variables.
bool is_constant_case(const vector<expression_ref>& patterns, const vector<expression_ref>& bodies)
{
    assert(patterns.size() == bodies.size());
    for(int i=0;i<patterns.size();i++)
    {
	if (has_used_vars(patterns[i])) return false;
	if (i > 0 and bodies[i] != bodies[0]) return false;
    }
    return true;
}

// case E of alts.  Here E has been simplified, but the alts have not.
expression_ref rebuild_case(const simplifier_options& options, const expression_ref& E, const substitution& S, in_scope_set& bound_vars, const inline_context& context)
{
    expression_ref object;
    vector<expression_ref> patterns;
    vector<expression_ref> bodies;
    parse_case_expression(E, object, patterns, bodies);
    const int L = patterns.size();

    auto decls = strip_let(object);
    bind_decls(bound_vars, decls);

    // NOTE: Any thing that relies on occurrence info for pattern vars should be done here, before
    //       we simplify alternatives, because that simplification can introduce new uses of the pattern vars.
    // Example: case #1 of {x:xs -> case #1 of {y:ys -> ys}} ==> case #1 of {x:xs -> xs} 
    //       We set #1=x:xs in the alternative, which means that a reference to #1 can reference xs.

    // 0. If all alternatives are the same expression that doesn't depend on any bound pattern variables.
    if (is_constant_case(patterns,bodies))
	return simplify(options, bodies[0], S, bound_vars, context);

    // 1. Simplify each alternative
    for(int i=0;i<L;i++)
    {
	auto S2 = S;
	CDecls pat_decls;

	// 2. Rename and bind pattern variables
	if (patterns[i].size())
	{
	    object_ptr<expression> pattern2 = patterns[i].as_expression().clone();
	    for(int j=0; j<pattern2->size(); j++)
	    {
		expression_ref& var = pattern2->sub[j];
		assert(is_dummy(var));

		// Create an unused variable for wildcards.  This is for if we add a x=pattern binding.
		if (is_wildcard(var)) {
		    dummy wild("__",1);
		    wild.code_dup = amount_t::None;
		    wild.work_dup = amount_t::None;
		    wild.is_loop_breaker = false;
		    wild.context = var_context::unknown;
		    var = wild;
		}

		dummy x2 = rename_and_bind_var(var, S2, bound_vars);
		var = x2;
		pat_decls.push_back({x2, {}});
	    }
	    patterns[i] = pattern2;
	}

	// 3. If we know something extra about the value (or range, theoretically) of the object in this case branch, then record that.
	bound_variable_info original_binding;
	if (is_dummy(object)) original_binding = rebind_var(bound_vars, object.as_<dummy>(), patterns[i]);

	// 4. Simplify the alternative body
	bodies[i] = simplify(options, bodies[i], S2, bound_vars, unknown_context());

	// 5. Restore informatation about an object variable to information outside this case branch.
	if (is_dummy(object)) rebind_var(bound_vars, object.as_<dummy>(), original_binding.first);

	unbind_decls(bound_vars, pat_decls);
    }

    // 6. Take a specific branch if the object is a constant
    expression_ref E2;
    if (is_WHNF(object))
    {
	for(int i=0; i<L; i++)
	{
	    if (is_dummy(patterns[i]))
	    {
		assert(is_wildcard(patterns[i]));
		E2 = bodies[i];
	    }
	    else if (patterns[i].head() == object.head())
	    {
		for(int j=0;j<patterns[i].size();j++)
		{
		    auto obj_var = object.sub()[j];
		    auto pat_var = patterns[i].sub()[j];
		    assert(is_dummy(obj_var) and not is_wildcard(obj_var));
		    assert(is_dummy(pat_var) and not is_wildcard(pat_var));
		    auto& x = pat_var.as_<dummy>();
		    decls.push_back({x, obj_var});
		    bind_var(bound_vars, x, obj_var);
		}
		E2 =  bodies[i];
	    }
	}
	if (not E2)
	    throw myexception()<<"Case object doesn't many any alternative in '"<<make_case_expression(object, patterns, bodies)<<"'";
    }
    // 7. If the case is an identity transformation
    // Hmmm... this might not be right, because leaving out the default could cause a match failure, which this transformation would eliminate.
    else if (is_identity_case(object, patterns, bodies))
	E2 = object;
    // 8. case-of-case
    else if (is_case(object) and options.case_of_case)
    {
	expression_ref object2;
	vector<expression_ref> patterns2;
	vector<expression_ref> bodies2;
	parse_case_expression(object, object2, patterns2, bodies2);

        // 1. Find names for the lifted case bodies.
	vector<dummy> lifted_names = get_body_function_names(bound_vars, patterns, patterns2);

	// 2. Lift case bodies into let-bound functions, and replace the bodies with calls to these functions.
	for(int i=0;i<patterns.size();i++)
	{
	    // Don't both factoring out trivial expressions
	    if (is_trivial(patterns[i])) continue;

	    vector<dummy> used_vars = get_used_vars(patterns[i]);

	    auto f = lifted_names[i];
	    expression_ref f_body = bodies[i];
	    expression_ref f_call = f;
	    for(int j=0;j<used_vars.size();j++)
	    {
		f_call = (f_call,used_vars[j]);
		f_body = lambda_quantify(used_vars[used_vars.size()-1-j],f_body);
	    }

	    decls.push_back({f,f_body});
	    bind_var(bound_vars, f, f_body);

	    bodies[i] = f_call;
	}

	// 3. The actual case-of-case transformation.
	//
	//      case (case object2 of patterns2 -> bodies2) of patterns => bodies
	//                         to
	//      case object2 of patterns2 -> case bodies2 of patterns => bodies
        //
	auto alts = make_alts(patterns,bodies);
	for(int i=0;i<patterns2.size();i++)
	    bodies2[i] = make_case_expression(bodies2[i],alts);

	E2 = make_case_expression(object2,patterns2,bodies2);
    }

    unbind_decls(bound_vars, decls);

    if (not E2)
	return let_expression(decls, make_case_expression(object, patterns, bodies));
    else
	return let_expression(decls, E2);
}

// @ E x1 .. xn.  The E and the x[i] have already been simplified.
expression_ref rebuild_apply(const simplifier_options& options, expression_ref E, const substitution& S, in_scope_set& bound_vars, inline_context context)
{
    expression_ref object = E.sub()[0];

    // 1. Optionally float let's out of the apply object
    CDecls decls;
    if (options.let_float_from_apply)
	decls = strip_let(object);

    // 2. Determine how many arguments we can apply
    int supplied_arguments = E.size() - 1;
    int required_arguments = get_n_lambdas1(object);
    int applied_arguments = std::min(supplied_arguments, required_arguments);
    if (not options.beta_reduction) applied_arguments = 0;

    // 3. For each applied argument, peel the lambda and add {var=argument} to the decls
    for(int i=0;i<supplied_arguments;i++)
    {
	auto argument = E.sub()[1+i];
	if (i<applied_arguments)
	{
	    auto x = object.sub()[0].as_<dummy>();
	    decls.push_back({x, argument});
	    object = peel_n_lambdas1(object,1);
	}
	else
	    object = (object, argument);
    }

    // 5. Rebuild the application with floated-lets and let-bound arguments outside any remaining applications.
    return let_expression(decls, object);
}

// let {x[i] = E[i]} in body.  The x[i] have been renamed and the E[i] have been simplified, but body has not yet been handled.
expression_ref rebuild_let(const simplifier_options& options, const CDecls& decls, expression_ref E, const substitution& S, in_scope_set& bound_vars)
{
    // If the decl is empty, then we don't have to do anythign special here.
    if (decls.empty()) return simplify(options, E, S, bound_vars, unknown_context());
    bind_decls(bound_vars, decls);

    E = simplify(options, E, S, bound_vars, unknown_context());

    unbind_decls(bound_vars, decls);

    // FIXME! Why can't we remove this?
    CDecls decls2 = decls;
    strip_let(E, decls2);

    return let_expression(decls2, E);
}


substitution
simplify_decls(const simplifier_options& options, CDecls& orig_decls, const substitution& S, in_scope_set& bound_vars, bool is_top_level)
{
    auto S2 = S;

    const int n_decls = orig_decls.size();

    CDecls new_decls;
    vector<dummy> new_names;

    // 5.1 Rename and bind all variables.
    //     Binding all variables ensures that we avoid shadowing them, which helps with let-floating.
    //     Renaming them is necessary to correctly simplify the bodies.
    for(int i=0;i<n_decls;i++)
    {
	dummy x = orig_decls[i].first;
	dummy x2 = rename_and_bind_var(x, S2, bound_vars);
	new_names.push_back(x2);
    }

    // 5.2 Iterate over decls, renaming and binding vars as we go, and simplifying them and adding substitutions for unconditional inlines.
    for(int i=0;i<n_decls;i++)
    {
	// If x[i] is not a loop breaker, then x[i] can only BE referenced by LATER E[k] (since loop breakers are later), while
	//                                     E[i] can only reference EARLIER x[k] and loop breakers.
	dummy x  = orig_decls[i].first;
	auto F   = orig_decls[i].second;

	dummy x2 = new_names[i];

	if (x.is_exported) assert(x == x2);

	// 1. Any references to x in F must be to the x bound in this scope.
	// 2. F can only contain references to x if x is a loop breaker.
	// 3. If x is a loop breaker, then S2 already contains substitutions for x -> x2 if needed.
	// 4. Therefore S2 is a good substitution for F.
	assert(x.is_loop_breaker or not get_free_indices(F).count(x));

	// A. Suspended substitutions created by pre-inlining won't be affected if we include unconditionally inlining later-occurring variables.
	//   A.1 This is because substitutions for later-occuring variables that are loop-breakers has already been done.
	//   A.2 Non-loop cannot occur in the bodies F that the suspended substitutions will be applied to.
	// B. Therefore, we can create a single substitution object for an entire decl scope, and just include pointers to it.
	// C. The lifetime of the substitution is just the duration of this scope, so raw pointers are fine.
	if (x.pre_inline() and options.pre_inline_unconditionally and not x.is_exported)
	{
	    S2.erase(x);
	    S2.insert({x,{F,S2}});
	}
	else
	{
	    // 5.1.2 Simplify F.
	    F = simplify(options, F, S2, bound_vars, unknown_context());

	    // Float lets out of decl x = F
	    if (options.let_float_from_let and is_let_expression(F) and (is_constructor(F.sub()[0].head()) or is_top_level))
	    {
		for(auto& decl: strip_let(F))
		{
		    bind_var(bound_vars, decl.first, decl.second);
		    new_names.push_back(decl.first);
		    new_decls.push_back(decl);
		}
	    }

	    // what are the conditions for post-inlining unconditionally?
	    if (is_trivial(F) and options.post_inline_unconditionally and not x.is_exported and not x.is_loop_breaker)
	    {
		S2.erase(x);
		S2.insert({x,F});
	    }
	    else
	    {
		new_decls.push_back({x2,F});

		// Any later occurrences will see the bound value of x[i] when they are simplified.
		rebind_var(bound_vars, x2, F);
	    }
	}
    }
    for(auto& new_name: new_names)
	unbind_var(bound_vars, new_name);

    std::swap(orig_decls, new_decls);
    return S2;
}

// Q1. Where do we handle beta-reduction (@ constant x1 x2 ... xn)?
// Q2. Where do we handle case-of-constant (case constant of alts)?
// Q3. How do we handle local let-floating from (i) case objects, (ii) apply-objects, (iii) let-bound statements?
expression_ref simplify(const simplifier_options& options, const expression_ref& E, const substitution& S, in_scope_set& bound_vars, const inline_context& context)
{
    assert(E);

    // 1. Var (x)
    if (is_dummy(E))
    {
	dummy x = E.as_<dummy>();
	// 1.1 If there's a substitution x -> E
	if (S.count(x))
	{
	    auto it = S.find(x);
	    // 1.1.1 If x -> SuspEx E S, then call the simplifier on E with its substitution S
	    if (it->second.S)
		return simplify(options, it->second.E, *(it->second.S), bound_vars, context);
	    // 1.1.2 If x -> DoneEx E, then call the simplifier on E but with no substitution.
	    else
		return simplify(options, it->second.E, {}, bound_vars, context);
	}
	// 1.2 If there's no substitution determine whether to inline at call site.
	else
	{
	    if (not bound_vars.count(x))
		throw myexception()<<"Variable '"<<x.print()<<"' not bound!";

	    return consider_inline(options, E, bound_vars, context);
	}
    }

     // Do we need something to handle WHNF variables?
    
    // 5. (partial) Literal constant.  Treat as 0-arg constructor.
    if (not E.size()) return E;

    // 2. Lambda (E = \x -> body)
    if (is_lambda(E.head()))
    {
	assert(E.size() == 2);

	auto S2 = S;

	auto var = E.sub()[0];
	// 2.1. Get the new name, possibly adding a substitution
	dummy x2 = rename_and_bind_var(var, S2, bound_vars);

	// 2.3 Simplify the body with x added to the bound set.
	auto new_body = simplify(options, E.sub()[1], S2, bound_vars, unknown_context());

	// 2.4 Remove x_new from the bound set.
	unbind_var(bound_vars,x2);

	// 2.5 Eta-reduction : Lx2.@ f x2 ==> f.  (Let-floating should let us ignore if f is a let-expression).
	if (new_body.is_expression() and is_apply(new_body.head()) and (new_body.as_expression().sub.back() == x2))
	{
	    if (new_body.size() == 2)
		return new_body.sub()[0];
	    else
	    {
		assert(new_body.size() > 2);
		object_ptr<expression> new_body2 = new_body.as_expression().clone();
		new_body2->sub.pop_back();
		return new_body2;
	    }
	}
	// 2.6 Recreate the lambda expression with x->x2 and simplified new body.
	else
	    return lambda_quantify(x2, new_body);
    }

    // 6. Case
    expression_ref object;
    vector<expression_ref> patterns;
    vector<expression_ref> bodies;
    if (parse_case_expression(E, object, patterns, bodies))
    {
	// Analyze the object
	object = simplify(options, object, S, bound_vars, case_object_context(E, context));
	auto E2 = make_case_expression(object, patterns, bodies);

	return rebuild_case(options, E2, S, bound_vars, context);
    }

    // ?. Apply
    if (is_apply(E.head()))
    {
	object_ptr<expression> V2 = E.as_expression().clone();
	
	// 1. Simplify the object.
	V2->sub[0] = simplify(options, V2->sub[0], S, bound_vars, apply_object_context(E, context));

	// 2. Simplify the arguments
	for(int i=1;i<E.size();i++)
	{
	    assert(is_trivial(V2->sub[i]));
	    V2->sub[i] = simplify(options, V2->sub[i], S, bound_vars, argument_context(context));
	}
	
	return rebuild_apply(options, V2, S, bound_vars, context);
    }

    // 4. Constructor or Operation
    if (is_constructor(E.head()) or is_non_apply_operation(E.head()))
    {
	object_ptr<expression> E2 = E.as_expression().clone();
	for(int i=0;i<E.size();i++)
	{
	    assert(is_trivial(E2->sub[i]));
	    E2->sub[i] = simplify(options, E2->sub[i], S, bound_vars, argument_context(context));
	}
	return E2;
    }


    // 5. Let (let {x[i] = F[i]} in body)
    //
    // Here we know that F[i] can only mention x[j<i] unless F[i] is a loop-breaker.
    // 
    if (is_let_expression(E))
    {
	auto body       = let_body(E);
	auto decls = let_decls(E);

	auto S2 = simplify_decls(options, decls, S, bound_vars, false);

        // 5.2 Simplify the let-body
	return rebuild_let(options, decls, body, S2, bound_vars);
    }

    std::abort();
}


CDecls simplify_module(const simplifier_options& options, const map<dummy,expression_ref>& small_decls_in,const set<dummy>& small_decls_in_free_vars, CDecls decls)
{
    set<dummy> free_vars;

    // Decompose the decls, remove unused decls, and occurrence-analyze the decls.
    auto decl_groups = occurrence_analyze_decls(decls, free_vars);

    in_scope_set bound_vars;

    for(auto& decl: small_decls_in)
    {
	dummy x = decl.first;
	x.work_dup = amount_t::Many;
	x.code_dup = amount_t::Many;
	bound_vars.insert({x,{decl.second,x}});
    }

    for(auto& var: small_decls_in_free_vars)
	bound_vars.insert({var,{}});

    for(auto& var: free_vars)
	bound_vars.insert({var,{}});

    vector<substitution> S(1);
    for(auto& decls: decl_groups)
    {
	auto s = simplify_decls(options, decls, S.back(), bound_vars, true);
	S.push_back( s );
	bind_decls(bound_vars, decls);
    }

    return flatten(decl_groups);
}


