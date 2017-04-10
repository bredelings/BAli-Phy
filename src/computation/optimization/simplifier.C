#include <iostream>
#include <unordered_map>
#include "computation/graph_register.H"
#include "computation/operations.H"
#include "computation/let-float.H"
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

#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/topological_sort.hpp>
#include <boost/graph/strong_components.hpp>

#include <boost/optional.hpp>
#include "simplifier.H"

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
	    size += simple_size(E.sub()[1]);

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

vector<pair<dummy,expression_ref>> occurrence_analyze_decls(vector<pair<dummy,expression_ref>> decls, set<dummy>& free_vars)
{
    using namespace boost;
    const int L = decls.size();

    // 0. Initialize the graph and decls
    Graph graph;
    vector<adjacency_list<>::vertex_descriptor> vertices;
    for(int i=0; i<L; i++)
	vertices.push_back( add_vertex(graph) );

    // 2. Mark vars referenced in the body as being alive
    vector<bool> alive(L,0);
    vector<int> work;
    for(int i=0;i<L;i++)
    {
	const auto& x = decls[i].first;
	if (x.is_exported or free_vars.count(x))
	{
	    alive[i] = true;
	    work.push_back(i);
	}
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
		boost::add_edge(vertices[i], vertices[j], graph);

		// 3.3.3 Add variable j to the work list if we haven't put it on the list already
		if (not alive[j])
		{
		    alive[j] = true;
		    work.push_back(j);
		}
	    }
	}
    }

    // 4. Set occurrence info for let vars in the decl, and remove bound vars from free_vars
    for(int i=0;i<decls.size();i++)
    {
	decls[i].first = remove_var_and_set_occurrence_info(decls[i].first, free_vars);

	// Variable is alive if and only if the variable is never executed.
	assert(alive[i] == (decls[i].first.code_dup != amount_t::None or decls[i].first.is_exported));
    }

    // 5. Break cycles
    vector<int> component(L);
    bool changed = true;
    while(changed)
    {
	changed = false;

	// find strongly connected components: every node is reachable from every other node
	int num = strong_components(graph, make_iterator_property_map(component.begin(), get(vertex_index, graph)));

	// find variables in each component
	vector<vector<int>> components(num);
	for(int i=0;i<L;i++)
	{
	    int c = component[i];
	    components[c].push_back(i);
	}

	for(int c=0;c<num and not changed;c++)
	{
	    int first = components[c][0];

	    // If the component is a single with no loop to itself, then it is fine.
	    if (components[c].size() == 1 and not edge(vertices[first],vertices[first],graph).second) continue;

	    vector<int> score(components[c].size());
	    for(int k = 0; k < score.size(); k++)
	    {
		int i = components[c][k];
		auto x = decls[i].first;
		auto F = decls[i].second;
		if (is_reglike(F)) score[k] = 4;
		else if (is_constructor(F.head()) or F.size() == 0) score[k] = 3;
		else if (x.pre_inline()) score[k] = 1;
	    }
	    int loop_breaker_index_in_component = argmin(score);
	    int loop_breaker_index = components[c][loop_breaker_index_in_component];

	    // delete incoming edges to the loop breaker
	    clear_in_edges(vertices[loop_breaker_index], graph);
	    changed = true;

	    // mark the variable as a loop breaker
	    decls[loop_breaker_index].first.is_loop_breaker = true;
	}
    }

    // 5. Sort the vertices
    vector<Vertex> sorted_vertices;
    topological_sort(graph, std::back_inserter(sorted_vertices));

    vector<int> sorted_indices(sorted_vertices.size());
    for(int i=0;i<sorted_indices.size();i++)
	sorted_indices[i] = get(vertex_index,graph,sorted_vertices[i]);

    // 7. Sort the live decls
    vector<pair<dummy,expression_ref>> decls2;
    for(int i: sorted_indices)
	if (alive[i])
	    decls2.push_back(decls[i]);
    return decls2;
}

void export_decls(vector<pair<dummy,expression_ref>>& decls, const expression_ref& exports, const string& name)
{
    // Record exports
    set<string> exported;
    for(auto& ex: exports.sub())
	exported.insert(ex.as_<dummy>().name);

    // Mark exported vars as exported
    for(auto& decl: decls)
    {
	if (exported.count(decl.first.name))
	{
	    decl.first.is_exported = true;
	    exported.erase(decl.first.name);
	}
    }

    // Check that we don't export things that don't exist
    if (not exported.empty())
    {
	myexception e;
	e<<"Module '"<<name<<"' exports undefined symbols:\n";
	for(auto& name: exported)
	    e<<"  "<<name;
	throw e;
    }
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
    if (not E) return {E,{}};

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
    if (not E.size()) return {E,{}};

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

	auto decls2 = occurrence_analyze_decls(decls, free_vars);

	return {let_expression(decls2, body), free_vars};
    }

    if (is_AST(E,"Module"))
    {
	string name;
	expression_ref exports;
	expression_ref impdecls;
	expression_ref topdecls;
	parse_module(E, name, exports, impdecls, topdecls);

	// Record exports
	auto decls = parse_decls(topdecls);
	export_decls(decls, exports, name);

	// Analyze the decls
	set<dummy> free_vars;
	auto decls2 = occurrence_analyze_decls(decls, free_vars);
	topdecls = make_topdecls(decls2);

	auto module = create_module(name, exports, impdecls, topdecls);

	return {module, free_vars};
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

void bind_decls(in_scope_set& bound_vars, const vector<pair<dummy,expression_ref>>& decls)
{
    for(const auto& decl: decls)
	bind_var(bound_vars, decl.first, decl.second);
}

void unbind_decls(in_scope_set& bound_vars, const vector<pair<dummy,expression_ref>>& decls)
{
    for(const auto& decl: decls)
	unbind_var(bound_vars, decl.first);
}

struct inline_context
{
    expression_ref data;

    inline_context prev_context() const {
	if (data)
	    assert(data.head().is_a<AST_node>());
	return inline_context(data.sub()[0]);
    }

    bool is_case_object() const {return is_AST(data,"case_object");}
    bool is_apply_object() const {return is_AST(data,"apply_object");}
    bool is_argument() const {return is_AST(data,"argument");}
    bool is_unknown() const {return is_AST(data,"unknown");}
    bool is_null() const {return not data;}
    expression_ref get_expression() const {return data.sub()[1];};

    inline_context() {};
    inline_context(const expression_ref& E):data(E) {}
    inline_context(const string& s, const expression_ref E, const inline_context& context)
	:data({AST_node(s),{context.data,E}}) {}
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
	int size_of_body = nodes_size(body);

	if (size_of_body <= size_of_call) return true;
    }

    return false;
}

bool boring(const expression_ref& rhs, const inline_context& context)
{
    // if the rhs is applied only to variables with unknown value AND ...

    // ... after consuming all the arguments we need, the result is very_boring.

    return true;
}

bool small_enough(const expression_ref& rhs, const inline_context& context)
{
    if (is_trivial(rhs)) return true;

    return false;
}

bool do_inline_multi(const expression_ref& rhs, const inline_context& context)
{
    if (no_size_increase(rhs,context)) return true;

    if (boring(rhs,context)) return false;

    return small_enough(rhs,context);
}

bool evaluates_to_bottom(const expression_ref& rhs)
{
    return false;
}

bool whnf_or_bottom(const expression_ref& rhs)
{
    return is_WHNF(rhs) or evaluates_to_bottom(rhs);
}

bool very_boring(inline_context context)
{
    if (context.is_case_object()) return false;

    if (context.is_apply_object()) return false;

    // This avoids substituting into constructor (and function) arguments.
    return true;
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
	return do_inline_multi(rhs, context);

    // OnceUnsafe
    else if (occur.work_dup == amount_t::Many and occur.code_dup == amount_t::Once)
	return whnf_or_bottom(rhs) and (no_size_increase(rhs,context) or not very_boring(context));

    // OnceUnsafe
    else if (occur.work_dup == amount_t::Once and occur.code_dup == amount_t::Once and occur.context == var_context::argument)
	return whnf_or_bottom(rhs) and (no_size_increase(rhs,context) or not very_boring(context));

    // MultiUnsafe
    else if (occur.work_dup == amount_t::Many and occur.code_dup == amount_t::Many)
	return whnf_or_bottom(rhs) and do_inline_multi(rhs, context);

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


expression_ref get_new_name(const expression_ref& var, const in_scope_set& bound_vars)
{
    dummy x = var.as_<dummy>();
    
    auto it = bound_vars.find(x);

    // If bound_vars doesn't contain x, then no need to change anything.
    if (it == bound_vars.end()) return var;

    x.index = bound_vars.size();
    while(bound_vars.count(x) and x.index > 0)
	x.index++;
    
    if (x.index <= 0) abort();
    
    return x;
}

dummy rename_and_bind_var(const expression_ref& var, substitution& S, in_scope_set& bound_vars)
{
    dummy x = var.as_<dummy>();
    assert(x.code_dup != amount_t::Unknown);
    assert(not is_wildcard(x));
    auto var2 = get_new_name(var, bound_vars);

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

    // 1. Simplify each alternative
    for(int i=0;i<L;i++)
    {
	auto S2 = S;
	vector<pair<dummy, expression_ref>> pat_decls;

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
    unbind_decls(bound_vars, decls);

    // 6. Take a specific branch if the object is a constant
    if (is_WHNF(object))
    {
	for(int i=0; i<L; i++)
	{
	    if (is_dummy(patterns[i]))
	    {
		assert(is_wildcard(patterns[i]));
		return let_expression(decls, bodies[i]);
	    }
	    else if (patterns[i].head() == object.head())
	    {
		for(int j=0;j<patterns[i].size();j++)
		{
		    auto obj_var = object.sub()[j];
		    auto pat_var = patterns[i].sub()[j];
		    assert(is_dummy(obj_var) and not is_wildcard(obj_var));
		    assert(is_dummy(pat_var) and not is_wildcard(pat_var));
		    decls.push_back({pat_var.as_<dummy>(), obj_var});
		}
		return let_expression(decls, bodies[i]);
	    }
	}
	throw myexception()<<"Case object doesn't many any alternative in '"<<make_case_expression(object, patterns, bodies)<<"'";
    }

    return let_expression(decls,make_case_expression(object, patterns, bodies));
}

// @ E x1 .. xn.  The E and the x[i] have already been simplified.
expression_ref rebuild_apply(const simplifier_options& options, expression_ref E, const substitution& S, in_scope_set& bound_vars, inline_context context)
{
    expression_ref object = E.sub()[0];

    // 1. Optionally float let's out of the apply object
    vector<pair<dummy,expression_ref>> decls;
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
expression_ref rebuild_let(const simplifier_options& options, const vector<pair<dummy, expression_ref>>& decls, expression_ref E, const substitution& S, in_scope_set& bound_vars)
{
    // If the decl is empty, then we don't have to do anythign special here.
    if (decls.empty()) return simplify(options, E, S, bound_vars, unknown_context());
    bind_decls(bound_vars, decls);

    E = simplify(options, E, S, bound_vars, unknown_context());

    unbind_decls(bound_vars, decls);

    // Only put the bound variables in decls2.
    vector<pair<dummy,expression_ref>> decls2;
    for(auto& decl: decls)
	if (decl.second)
	    decls2.push_back(decl);

    strip_let(E, decls2);

    return let_expression(decls2, E);
}


substitution
simplify_decls(const simplifier_options& options, vector<pair<dummy, expression_ref>>& orig_decls, const substitution& S, in_scope_set& bound_vars, bool is_top_level)
{
    auto S2 = S;

    const int n_decls = orig_decls.size();

    vector<pair<dummy,expression_ref>> decls;

    // 5.1 Rename and bind all variables.
    //     Binding all variables ensures that we avoid shadowing them, which helps with let-floating.
    //     Renaming them is necessary to correctly simplify the bodies.
    for(int i=0;i<n_decls;i++)
    {
	dummy x = orig_decls[i].first;
	dummy x2 = rename_and_bind_var(x, S2, bound_vars);
	decls.push_back({x2,{}});
    }

    // 5.2 Iterate over decls, renaming and binding vars as we go, and simplifying them and adding substitutions for unconditional inlines.
    for(int i=0;i<n_decls;i++)
    {
	// If x[i] is not a loop breaker, then x[i] can only BE referenced by LATER E[k] (since loop breakers are later), while
	//                                     E[i] can only reference EARLIER x[k] and loop breakers.
	dummy x  = orig_decls[i].first;
	auto F   = orig_decls[i].second;

	dummy x2 = decls[i].first;

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
		    decls.push_back(decl);
		}
	    }

	    if (is_trivial(F) and options.post_inline_unconditionally and not x.is_exported)
	    {
		S2.erase(x);
		S2.insert({x,F});
	    }
	    else
	    {
		decls[i].second = F;

		// Any later occurrences will see the bound value of x[i] when they are simplified.
		rebind_var(bound_vars, x2, F);
	    }
	}
    }
    unbind_decls(bound_vars, decls);

    orig_decls = decls;
    return S2;
}

// Q1. Where do we handle beta-reduction (@ constant x1 x2 ... xn)?
// Q2. Where do we handle case-of-constant (case constant of alts)?
// Q3. How do we handle local let-floating from (i) case objects, (ii) apply-objects, (iii) let-bound statements?
expression_ref simplify(const simplifier_options& options, const expression_ref& E, const substitution& S, in_scope_set& bound_vars, const inline_context& context)
{
    if (not E) return E;

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

	// 2.5 If we changed x, or simplified the body, then 
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


    if (is_AST(E, "Module"))
    {
	string name;
	expression_ref exports;
	expression_ref impdecls;
	expression_ref topdecls;
	parse_module(E, name, exports, impdecls, topdecls);

	auto decls = parse_decls(topdecls);
	export_decls(decls, exports, name);

	simplify_decls(options, decls, S, bound_vars,true);
	topdecls = make_topdecls(decls);

	return create_module(name, exports, impdecls, topdecls);
    }

    std::abort();
}


expression_ref simplifier(const simplifier_options& options, const map<dummy,expression_ref>& small_decls_in,
			  const set<dummy>& small_decls_in_free_vars, const expression_ref& E1)
{
    set<dummy> free_vars;
    expression_ref E2;
    tie(E2, free_vars) = occurrence_analyzer(E1);

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

    return simplify(options, E2, {}, bound_vars, {});
}


