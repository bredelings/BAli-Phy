#include <iostream>
#include <unordered_map>
#include "graph_register.H"
#include "operations.H"
#include "let-float.H"
#include "computation/expression/expression.H"
#include "computation/expression/let.H"
#include "computation/expression/case.H"
#include "computation/expression/dummy.H"
#include "computation/expression/lambda.H"
#include "computation/expression/trim.H"
#include "computation/expression/indexify.H"

#include <boost/graph/graph_traits.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/topological_sort.hpp>
#include <boost/graph/strong_components.hpp>

#include <boost/optional.hpp>

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
    if (a1 == amount_t::None) return a2;
    if (a2 == amount_t::None) return a1;
    if (a1 == amount_t::Many or a2 == amount_t::Many) return amount_t::Many;
    if (a1 == amount_t::Once and a2 == amount_t::Once) return amount_t::Many;
    std::abort();
}

amount_t max(amount_t a1, amount_t a2)
{
    if (a1 == amount_t::None) return a2;
    if (a2 == amount_t::None) return a1;
    if (a1 == amount_t::Many or a2 == amount_t::Many) return amount_t::Many;
    if (a1 == amount_t::Once and a2 == amount_t::Once) return amount_t::Once; // <- This is the only difference
    std::abort();
}

// Merge_branch should MAX the work done, but ADD the code size.
set<dummy> merge_occurrences(const set<dummy>& free_vars1, const set<dummy>& free_vars2, bool alternate_branches = false)
{
    // Start with free_vars1
    set<dummy> free_vars = free_vars1;

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

	    free_vars.erase(var);
	}

	free_vars.insert(var);
    }
    return free_vars;
}

// occur:: Expression -> (marked free_variables, marked Expression)

pair<expression_ref,set<dummy>> occurrence_analyzer(const expression_ref& E, var_context context=var_context::unknown)
{
    if (not E) return {E,{}};

    // 1. Var
    if (E.is_a<dummy>())
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
    if (E.head().is_a<lambda>())
    {
	assert(E.size() == 2);

	// 1. Analyze the body and marks its variables
	set<dummy> free_vars;
	expression_ref body;
	tie(body, free_vars) = occurrence_analyzer(E.sub()[1]);

	// 2. Mark bound variable with occurrence info from the body
	dummy x = E.sub()[0].as_<dummy>();

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
	    x.work_dup = x_iter->work_dup;
	    x.code_dup = x_iter->code_dup;
	    x.is_loop_breaker = x_iter->is_loop_breaker;
	    x.context = x_iter->context;
	}


	// 3. remove x from free variables of the body
	free_vars.erase(x);

	// 4. change Once -> OnceInLam / work=Many, code=Once
	set<dummy> free_vars2;
	for(auto var: free_vars)
	{
	    if (var.work_dup == amount_t::Once)
		var.work_dup = amount_t::Many;
	    free_vars2.insert(var);
	}

	return {lambda_quantify(x,body),free_vars2};
    }

    // 6. Case
    if (E.head().is_a<Case>())
    {
	// Analyze the object
	expression_ref object;
	set<dummy> obj_free_vars;
	tie(object, obj_free_vars) = occurrence_analyzer(E.sub()[0], var_context::argument);

	const int L = (E.size()-1)/2;
	// Just normalize the bodies

	set<dummy> alts_free_vars;
	vector<expression_ref> patterns(L);
	vector<expression_ref> bodies(L);
	for(int i=0;i<L;i++)
	{
	    // Analyze the i-ith branch
	    set<dummy> alt_i_free_vars;
	    tie(bodies[i], alt_i_free_vars) = occurrence_analyzer(E.sub()[2+2*i]);

	    // Remove vars bound by the pattern
	    patterns[i] = E.sub()[1+2*i];
	    auto alt_i_pattern_vars = get_free_indices(patterns[i]);

	    // Remove pattern variables from free_vars if it contains them.
	    for(const auto& d: alt_i_pattern_vars)
		alt_i_free_vars.erase(d);

	    // Merge occurrences for this pattern into the occurrence for the whole set of alts.
	    alts_free_vars = merge_occurrences(alts_free_vars, alt_i_free_vars, true);
	}
	set<dummy> free_vars = merge_occurrences(obj_free_vars, alts_free_vars);
	return {make_case_expression(object,patterns,bodies),free_vars};
    }

    // 4. Constructor
    if (E.head().is_a<constructor>() or E.head().is_a<Operation>())
    {
	set<dummy> free_vars;
	expression_ref F = E.head();
	for(int i=0;i<E.size();i++)
	{
	    set<dummy> free_vars_i;
	    expression_ref arg_i;
	    tie(arg_i,free_vars_i) = occurrence_analyzer(E.sub()[i], var_context::argument);
	    F = F + arg_i;
	    free_vars = merge_occurrences(free_vars, free_vars_i);
	}
	return {F,free_vars};
    }


    // 5. Let (let {x[i] = F[i]} in body)
    if (E.head().is_a<let_obj>())
    {
	using namespace boost;
	const int L = (E.size()-1)/2;

	// 0. Initialize the graph
	Graph graph;
	vector<adjacency_list<>::vertex_descriptor> vertices;
	for(int i=0; i<L; i++)
	    vertices.push_back( add_vertex(graph) );
	vector<expression_ref> vars(L);
	vector<expression_ref> bodies(L);

	// 1. Analyze the body
	set<dummy> free_vars;
	expression_ref body;
	tie(body, free_vars) = occurrence_analyzer(E.sub()[0]);

	// 2. Mark vars referenced in the body as being alive
	vector<bool> alive(L,0);
	vector<int> work;
	for(int i=0;i<L;i++)
	{
	    auto var = E.sub()[1 + 2*i].as_<dummy>();
	    if (free_vars.count(var) and not alive[i])
	    {
		alive[i] = true;
		work.push_back(i);
	    }
	}

	// 3. Discover reachable variables, analyze them, and record references between variables
	for(int k=0;k<work.size();k++)
	{
	    int i = work[k];
	    // 3.1 Analyze the bound statement
	    set<dummy> free_vars_i;
	    tie(bodies[i],free_vars_i) = occurrence_analyzer(E.sub()[2 + 2*i]);
	    vars[i] = E.sub()[1 + 2*i];

	    // 3.2 Record occurrences
	    free_vars = merge_occurrences(free_vars, free_vars_i);

	    // 3.3. Check if other variables j are referenced from the i-th variable.
	    for(int j=0;j<L;j++)
	    {
		// 3.3.1 Check if variable i references variable j
		auto varj = E.sub()[1 + 2*j].as_<dummy>();
		if (free_vars_i.count(varj))
		{
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

	// 4. Break cycles
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
		    if (E.sub()[2 + 2*i].is_a<dummy>()) score[k] = 4;
		    else if (E.sub()[2 + 2*i].is_a<constructor>() or E.sub()[2 + 2*i].size() == 0) score[k] = 3;
		    else if (free_vars.find(E.sub()[1 + 2*i].as_<dummy>())->pre_inline()) score[k] = 1;
		}
		int loop_breaker_index_in_component = argmin(score);
		int loop_breaker_index = components[c][loop_breaker_index_in_component];

		// delete incoming edges to the loop breaker
		clear_in_edges(vertices[loop_breaker_index], graph);
		changed = true;

		// mark the variable as a loop breaker
		{
		    dummy var = *free_vars.find(E.sub()[1 + 2*loop_breaker_index].as_<dummy>());
		    var.is_loop_breaker = true;
		    free_vars.erase(var);
		    free_vars.insert(var);
		}
	    }
	}

	// 5. Sort the vertices
	vector<Vertex> sorted_vertices;
	topological_sort(graph, std::back_inserter(sorted_vertices));

	vector<int> sorted_indices(sorted_vertices.size());
	for(int i=0;i<sorted_indices.size();i++)
	    sorted_indices[i] = get(vertex_index,graph,sorted_vertices[i]);

	// 6. Reconstruct the let statement
	vector<expression_ref> vars2;
	vector<expression_ref> bodies2;
	for(int i: sorted_indices)
	{
	    // Skip dead vars
	    if (not bodies[i]) continue;

	    vars2.push_back(*free_vars.find(vars[i].as_<dummy>()));
	    bodies2.push_back(bodies[i]);
	}

	// 7. Remove let-vars from free_vars if they are in it.
	for(int i=0;i<L;i++)
	    free_vars.erase(E.sub()[1 + 2*i].as_<dummy>());


	return {let_expression(vars2, bodies2, body), free_vars};
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
    boost::optional<substitution> S;
    substitution_range(const expression_ref& e):E(e) {}
    substitution_range(const expression_ref& e, const boost::optional<substitution> s):E(e),S(s) {}
};

typedef map<dummy, pair<expression_ref,occurrence_info>> in_scope_set;

void bind_var(in_scope_set& bound_vars, const dummy& x, const expression_ref& E)
{
    assert(not bound_vars.count(x));
    bound_vars.insert({x,{E,x}});
}

void unbind_var(in_scope_set& bound_vars, const dummy& x)
{
    assert(bound_vars.count(x));
    bound_vars.erase(x);
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

enum class inline_context {case_object, apply_object, unknown};

bool no_size_increase(const expression_ref& rhs, inline_context context)
{
    return false;
}

bool boring(const expression_ref& rhs, inline_context context)
{
    return true;
}

bool small_enough(const expression_ref& rhs, inline_context context)
{
    return false;
}

bool do_inline_multi(const expression_ref& rhs, inline_context context)
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
    if (context == inline_context::case_object) return false;

    if (context == inline_context::apply_object) return false;

    // This avoids substituting into constructor (and function) arguments.
    return true;
}


bool do_inline(const expression_ref& rhs, const occurrence_info& occur, inline_context context)
{
    // LoopBreaker
    if (occur.is_loop_breaker)
	return false;
    
    // OnceSafe
    else if (occur.pre_inline())
	return true;
//	throw myexception()<<"Trying to inline OnceSafe variable!";

    // MultiSafe
    else if (occur.work_dup == amount_t::Once and occur.code_dup == amount_t::Many)
	return do_inline_multi(rhs, context);

    // OnceUnsafe
    else if (occur.work_dup == amount_t::Many and occur.code_dup == amount_t::Once)
	return whnf_or_bottom(rhs) and not very_boring(context);

    // OnceUnsafe
    else if (occur.work_dup == amount_t::Once and occur.code_dup == amount_t::Once and occur.context == var_context::argument)
	return whnf_or_bottom(rhs) and not very_boring(context);

    // MultiUnsafe
    else if (occur.work_dup == amount_t::Many and occur.code_dup == amount_t::Many)
	return whnf_or_bottom(rhs) and do_inline_multi(rhs, context);

    std::abort();
}

bool is_trivial(const expression_ref& E)
{
    return E.is_a<dummy>();
}

expression_ref simplify(const expression_ref& E, const substitution& S, in_scope_set& bound_vars, inline_context context);

// Do we have to explicitly skip loop breakers here?
expression_ref consider_inline(const expression_ref& E, in_scope_set& bound_vars, inline_context context)
{
    dummy x = E.as_<dummy>();

    const auto& binding = bound_vars.at(x);

    // 1. If there's a binding x = E, and E = y for some variable y
    if (binding.first and do_inline(binding.first, binding.second, context))
	return simplify(binding.first, {}, bound_vars, context);
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

dummy maybe_rename_var(const expression_ref& var, substitution& S, const in_scope_set& bound_vars)
{
    dummy x = var.as_<dummy>();
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

    return var2.as_<dummy>();
}

expression_ref simplify(const expression_ref& E, const substitution& S, in_scope_set& bound_vars, inline_context context);

// case E of alts.  Here E has been simplified, but the alts have not.
expression_ref rebuild_case(const expression_ref& E, const substitution& S, in_scope_set& bound_vars, inline_context context)
{
    object_ptr<expression> E2 = E.as_expression().clone();
    const int L = (E.size()-1)/2;

    // 1. Walk each alternative
    for(int i=0;i<L;i++)

    {
	auto S2 = S;
	vector<pair<dummy, expression_ref>> decls;

	expression_ref pattern = E2->sub[1 + 2*i];
	if (pattern.is_expression())
	{
	    // 2. Walk the pattern vars and rename them, rewriting the pattern as we go.
	    object_ptr<expression> pattern2 = pattern.as_expression().clone();
	    for(int j=0; j<pattern2->size(); j++)
	    {
		expression_ref var = pattern2->sub[j];
		if (not is_wildcard(var))
		{
		    dummy x2 = maybe_rename_var(var, S2, bound_vars);
		    pattern2->sub[j] = x2;
		    decls.push_back({x2, {}});
		    bind_var(bound_vars,x2,{});
		}
	    }
	    // 3. Use the rewritten pattern
	    E2->sub[1 + 2*i] = pattern2;
	}

	// 4. Simplify the alternative body
	E2->sub[2 + 2*i] = simplify(E2->sub[2 + 2*i], S2, bound_vars, inline_context::unknown);
	unbind_decls(bound_vars, decls);
    }
    return E2;
}

// @ E x1 .. xn.  The E and the x[i] have already been simplified.
expression_ref rebuild_apply(const expression_ref& E, const substitution& S, in_scope_set& bound_vars, inline_context context)
{
    return E;
}

// let {x[i] = E[i]} in body.  The x[i] have been renamed and the E[i] have been simplified, but body has not yet been handled.
expression_ref rebuild_let(const vector<pair<dummy, expression_ref>>& decls, expression_ref E, const substitution& S, in_scope_set& bound_vars)
{
    // If the decl is empty, then we don't have to do anythign special here.
    if (decls.empty()) return simplify(E, S, bound_vars, inline_context::unknown);

    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    for(auto& decl: decls)
    {
	bind_var(bound_vars, decl.first, decl.second);
	vars.push_back(decl.first);
	bodies.push_back(decl.second);
    }

    E = simplify(E, S, bound_vars, inline_context::unknown);

    for(auto& decl: decls)
	unbind_var(bound_vars, decl.first);

    return let_expression(vars, bodies, E);
}

// Q1. Where do we handle beta-reduction (@ constant x1 x2 ... xn)?
// Q2. Where do we handle case-of-constant (case constant of alts)?
// Q3. How do we handle local let-floating from (i) case objects, (ii) apply-objects, (iii) let-bound statements?
expression_ref simplify(const expression_ref& E, const substitution& S, in_scope_set& bound_vars, inline_context context)
{
    if (not E) return E;

    // 1. Var (x)
    if (E.is_a<dummy>())
    {
	dummy x = E.as_<dummy>();
	// 1.1 If there's a substitution x -> E
	if (S.count(x))
	{
	    auto it = S.find(x);
	    // 1.1.1 If x -> SuspEx E S, then call the simplifier on E with its substitution S
	    if (it->second.S)
		return simplify(it->second.E, *(it->second.S), bound_vars, context);
	    // 1.1.2 If x -> DoneEx E, then call the simplifier on E but with no substitution.
	    else
		return simplify(it->second.E, {}, bound_vars, context);
	}
	// 1.2 If there's no substitution determine whether to inline at call site.
	else
	{
	    if (not bound_vars.count(x))
		throw myexception()<<"Variable '"<<x.print()<<"' not bound!";

	    return consider_inline(E, bound_vars, context);
	}
    }

     // Do we need something to handle WHNF variables?
    
    // 5. (partial) Literal constant.  Treat as 0-arg constructor.
    if (not E.size()) return E;

    // 2. Lambda (E = \x -> body)
    if (E.head().is_a<lambda>())
    {
	assert(E.size() == 2);

	auto S2 = S;

	auto var = E.sub()[0];
	// 2.1. Get the new name, possibly adding a substitution
	dummy x2 = maybe_rename_var(var, S2, bound_vars);

	// 2.2 Mark x2 as bound to nothing.  Occurrence info should be propagated.
	bind_var(bound_vars, x2, {});

	// 2.3 Simplify the body with x added to the bound set.
	auto new_body = simplify(E.sub()[1], S2, bound_vars, inline_context::unknown);

	// 2.4 Remove x_new from the bound set.
	unbind_var(bound_vars,x2);

	// 2.5 If we changed x, or simplified the body, then 
	return lambda_quantify(x2, new_body);
    }

    // 6. Case
    if (E.head().is_a<Case>())
    {
	// Analyze the object
	object_ptr<expression> E2 = E.as_expression().clone();
	E2->sub[0] = simplify(E2->sub[0], S, bound_vars, inline_context::case_object);

	return rebuild_case(E2, S, bound_vars, context);
    }

    // ?. Apply
    if (E.head().is_a<Apply>())
    {
	object_ptr<expression> V2 = E.as_expression().clone();
	
	// 1. Simplify the object.
	V2->sub[0] = simplify(V2->sub[0], S, bound_vars, inline_context::apply_object);

	// 2. Simplify the arguments
	for(int i=1;i<E.size();i++)
	{
	    assert(is_trivial(V2->sub[i]));
	    V2->sub[i] = simplify(V2->sub[i], S, bound_vars, inline_context::unknown);
	}
	
	return rebuild_apply(V2, S, bound_vars, context);
    }

    // 4. Constructor or Operation
    if (E.head().is_a<constructor>() or E.head().is_a<Operation>())
    {
	object_ptr<expression> E2 = E.as_expression().clone();
	for(int i=0;i<E.size();i++)
	{
	    assert(is_trivial(E2->sub[i]));
	    E2->sub[i] = simplify(E2->sub[i], S, bound_vars, inline_context::unknown);
	}
	return E2;
    }


    // 5. Let (let {x[i] = F[i]} in body)
    //
    // Here we know that F[i] can only mention x[j<i] unless F[i] is a loop-breaker.
    // 
    if (E.head().is_a<let_obj>())
    {
	auto S2 = S;

	// 5.1 Iterate over decls, simplifying them and adding substitutions for unconditional inlines.
	const int n_decls = (E.size()-1)/2;
	vector<pair<dummy,expression_ref>> new_decls;
	for(int i=0;i<n_decls;i++)
	{
	    auto var = E.sub()[1 + 2*i];
	    auto F   = E.sub()[2 + 2*i];
	    dummy x = var.as_<dummy>();
	    dummy x2 = maybe_rename_var(var, S2, bound_vars);
	    
	    if (x.pre_inline())
		S2.insert({x, {F, S2}});  	         // 5.1.1 Preinline unconditionally:  Substitute x[i] --> SuspEx F S2
	    else
	    {
		                                         // 5.1.2 Simplify F. We won't run across x2 (or any other unbound variable) here unless its a loop breaker.  But let's allow unbound loopbreakers ...
		F = simplify(F, S2, bound_vars, inline_context::unknown); 

		// FIXME! Pull let statements out of F, and add them before F in decls.

		if (is_trivial(F) and not x.is_loop_breaker)
		    S2.insert({x,F});                    // 5.1.3 Postinline unconditionally: Substitute x[i] --> DoneEx F S2
		else
		{
		    new_decls.push_back({x2,F});             // 5.1.4 Maybe inline at call site: note x_new = F
		    bind_var(bound_vars, x2, F);
		}
	    }
	}
	unbind_decls(bound_vars, new_decls);

	// 5.2 Simplify the let-body
	return rebuild_let(new_decls, E.sub()[0], S2, bound_vars);
    }

    std::abort();
}


expression_ref simplifier(const expression_ref& E1)
{
    set<dummy> free_vars;
    expression_ref E2;
    tie(E2, free_vars) = occurrence_analyzer(E1);

    in_scope_set bound_vars;
    for(auto& var: free_vars)
	bound_vars.insert({var,{}});

    return simplify(E2, {}, bound_vars, inline_context::unknown);
}

expression_ref graph_normalize(const expression_ref& E)
{
    if (not E) return E;

    // 1. Var
    // 5. (partial) Literal constant.  Treat as 0-arg constructor.
    if (not E.size()) return E;
  
    // 2. Lambda
    if (E.head().is_a<lambda>())
    {
	assert(E.size() == 2);
	object_ptr<expression> V = E.as_expression().clone();
	V->sub[1] = graph_normalize(E.sub()[1]);

	if (V->sub[1].ptr() == E.sub()[1].ptr())
	    return E;
	else
	    return V;
    }

    // 6. Case
    if (E.head().is_a<Case>())
    {
	object_ptr<expression> V = E.as_expression().clone();

	// Normalize the object
	V->sub[0] = graph_normalize(V->sub[0]);

	const int L = (V->sub.size()-1)/2;
	// Just normalize the bodies
	for(int i=0;i<L;i++)
	    V->sub[2+2*i] = graph_normalize(V->sub[2+2*i]);
    
	if (is_reglike(V->sub[0]))
	    return object_ptr<const expression>(V);
	else
	{
	    int var_index = get_safe_binder_index(E);
	    expression_ref x = dummy(var_index);
	    expression_ref obj = V->sub[0];
	    V->sub[0] = x;

	    return let_expression(x,obj,V);
	}
    }

    // 4. Constructor
    if (E.head().is_a<constructor>() or E.head().is_a<Operation>())
    {
	int var_index = get_safe_binder_index(E);

	object_ptr<expression> E2 = E.as_expression().clone();

	// Actually we probably just need x[i] not to be free in E.sub()[i]
	vector<expression_ref> vars;
	vector<expression_ref> bodies;
	for(int i=0;i<E2->size();i++)
	{
	    E2->sub[i] = graph_normalize(E.sub()[i]);

	    if (not is_reglike(E2->sub[i]))
	    {
		expression_ref var = dummy( var_index++ );

		// 1. Let-bind the argument expression
		vars.push_back( var );
		bodies.push_back( E2->sub[i] );

		// 2. Replace the argument expression with the let var.
		E2->sub[i] = var;
	    }
	}

	return let_expression(vars, bodies, object_ptr<const expression>(E2));
    }

    // 5. Let 
    if (E.head().is_a<let_obj>())
    {
	object_ptr<expression> V = E.as_expression().clone();

	// Normalize the object
	V->sub[0] = graph_normalize(V->sub[0]);

	const int L = (V->sub.size()-1)/2;

	// Just normalize the bodies, not the vars
	for(int i=0;i<L;i++)
	    V->sub[2 + 2*i] = graph_normalize(V->sub[2 + 2*i]);

	return V;
    }

    throw myexception()<<"graph_normalize: I don't recognize expression '"+ E.print() + "'";
}


// Deleted: Fun_normalize( ).  Perhaps reintroduce later.
// Translate from language Basic to language Fun by introducing letrec expressions.  (arguments are already vars)
// See "From Natural Semantics to C: A Formal Derivation of two STG machines."
//      by Alberto de la Encina and Ricardo Pena.



closure let_float(closure&& C)
{
    C.exp = let_float(expression_ref(C.exp));
    return C;
}

closure graph_normalize(closure&& C)
{
    C.exp = graph_normalize(expression_ref(C.exp));
    return C;
}

closure indexify(closure&& C)
{
    C.exp = indexify(expression_ref(C.exp));
    return C;
}

closure trim_normalize(closure&& C)
{
    C.exp = trim_normalize(expression_ref(C.exp));
    return C;
}

closure resolve_refs(const vector<Module>& P, closure&& C)
{
    C.exp = resolve_refs(P, C.exp);
    return C;
}

closure reg_heap::preprocess(const closure& C)
{
    assert(C.exp);
    assert(let_float(C.exp).print() == let_float(let_float(C.exp)).print());
    //  return trim_normalize( indexify( Fun_normalize( graph_normalize( let_float( translate_refs( closure(C) ) ) ) ) ) );
    return trim_normalize( indexify( graph_normalize( let_float( translate_refs( resolve_refs(*P, closure(C) ) ) ) ) ) );
}

expression_ref reg_heap::translate_refs(const expression_ref& E, closure::Env_t& Env)
{
    int reg = -1;

    // Replace parameters with the appropriate reg_var: of value parameter( )
    if (E.is_a<parameter>())
    {
	string name = E.as_<parameter>().parameter_name;
	string qualified_name = name;

	int param_index = find_parameter(qualified_name);
    
	if (param_index == -1)
	    throw myexception()<<"Can't translate undefined parameter '"<<qualified_name<<"' ('"<<name<<"') in expression!";

	reg = parameters[param_index].second;
    }

    // Replace parameters with the appropriate reg_var: of value whatever
    if (E.is_a<identifier>())
    {
	string name = E.as_<identifier>().name;
	string qualified_name = name;
	assert(is_qualified_symbol(qualified_name) or is_haskell_builtin_con_name(qualified_name));
	auto loc = identifiers.find( qualified_name );
	if (loc == identifiers.end())
	{
	    if (is_haskell_builtin_con_name(name))
	    {
		symbol_info S = Module::lookup_builtin_symbol(name);
		add_identifier(S.name);
      
		// get the root for each identifier
		loc = identifiers.find(S.name);
		assert(loc != identifiers.end());
	
		int R = loc->second;
	
		assert(R != -1);
		set_C(R, preprocess(S.body) );
	    }
	    else
		throw myexception()<<"Can't translate undefined identifier '"<<name<<"' in expression!";
	}

	reg = loc->second;
    }

    // Replace parameters with the appropriate reg_var: of value whatever
    if (E.is_a<reg_var>())
	reg = E.as_<reg_var>().target;

    if (reg != -1)
    {
	int index = Env.size();
	Env.insert(Env.begin(), reg);

	return index_var(index);
    }

    // Other constants have no parts, and don't need to be translated
    if (not E.size()) return E;

    // Translate the parts of the expression
    object_ptr<expression> V = E.as_expression().clone();
    for(int i=0;i<V->size();i++)
	V->sub[i] = translate_refs(V->sub[i], Env);

    return V;
}

closure reg_heap::translate_refs(closure&& C)
{
    closure C2 = C;
    C2.exp = translate_refs(C2.exp, C2.Env);
    return C2;
}

