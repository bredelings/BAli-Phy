#include <iostream>
#include <unordered_map>
#include <list>
#include "occurrence.H"

#include "computation/module.H"
#include "computation/haskell/ids.H"

#include "util/range.H"
#include "util/mapping.H"
#include "util/graph.H"

#include "simplifier.H"

using std::string;
using std::vector;
using std::list;
using std::pair;
using std::tuple;
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
void merge_occurrences_into(set<Occ::Var>& free_vars1, const set<Occ::Var>& free_vars2, bool alternate_branches = false)
{
    // Then consider free_vars2
    for(auto x_: free_vars2)
    {
        auto x = x_;
	auto it = free_vars1.find(x);

	// If the var is in both groups, we must modify its occurrence info
	if (it != free_vars1.end())
	{
	    x.info.is_loop_breaker = x.info.is_loop_breaker or it->info.is_loop_breaker;
	    if (alternate_branches)
		x.info.work_dup = max(x.info.work_dup, it->info.work_dup);
	    else
		x.info.work_dup = x.info.work_dup + it->info.work_dup;
	    x.info.code_dup = x.info.code_dup + it->info.code_dup;
	    if (x.info.context == var_context::argument or it->info.context == var_context::argument)
		x.info.context = var_context::argument;
	    else
		x.info.context = var_context::unknown;

	    free_vars1.erase(x_);
	}

	free_vars1.insert(x);
    }
}

Occ::Var remove_var_and_set_occurrence_info(const Core2::Var<>& x_in, set<Occ::Var>& free_vars)
{
    Occ::Var x{x_in.name, x_in.index, {}, x_in.is_exported};

    // 1. Copy occurrence info
    auto x_iter = free_vars.find(x);
    if (x_iter == free_vars.end())
    {
	x.info.work_dup = amount_t::None;
	x.info.code_dup = amount_t::None;
	x.info.is_loop_breaker = false;
	x.info.context = var_context::unknown;
    }
    else
    {
	x.info = x_iter->info;
    }

    // 2. Remove var from set
    free_vars.erase(x);

    assert(x.info.code_dup != amount_t::Unknown and x.info.work_dup != amount_t::Unknown);

    return x;
}

Occ::Var remove_var_and_set_occurrence_info(const Core2::Exp<>& E, set<Occ::Var>& free_vars)
{
    return remove_var_and_set_occurrence_info(*E.to_var(), free_vars);
}

// occur:: Expression -> (marked free_variables, marked Expression)
bool is_alive(const Occ::Var& x)
{
    return (x.is_exported or x.info.code_dup != amount_t::None);
}

tuple<Occ::Decls, Graph> construct_directed_reference_graph(const Module& m, const Core2::Decls<>& decls_in, set<Occ::Var>& free_vars)
{
    const int L = decls_in.size();
    Occ::Decls decls(L);

    // 0. Initialize the graph and decls
    Graph graph(L);

    // 1. Mark vars referenced in the body as being alive by setting x.code_dup = Unknown
    vector<int> work;
    for(int i=0;i<L;i++)
    {
        auto& x = decls_in[i].x;

	decls[i].x = Occ::Var{x.name, x.index, {}, x.is_exported};
	decls[i].x.info.code_dup = free_vars.count(decls[i].x)?(amount_t::Unknown):(amount_t::None);
	if (is_alive(decls[i].x))
	    work.push_back(i);
    }

    // 3. Discover reachable variables, analyze them, and record references between variables
    map<Occ::Var,int> index_for_var;
    for(int i=0;i<L;i++)
	index_for_var.insert({decls[i].x, i});

    for(int k=0;k<work.size();k++)
    {
	int i = work[k];
	// 3.1 Analyze the bound statement
	auto [occ_body, free_vars_i] = occurrence_analyzer(m, decls_in[i].body);
        decls[i].body = occ_body;

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
		if (not is_alive(decls[j].x))
		{
		    decls[j].x.info.code_dup = amount_t::Unknown;
		    work.push_back(j);
		}
	    }
	}
    }

    return {decls, graph};
}

vector<int> get_live_vars(const vector<int>& vars, const Occ::Decls& decls)
{
    vector<int> live_vars;
    for(int var: vars)
    {
	if (is_alive(decls[var].x))
	    live_vars.push_back(var);
    }
    return live_vars;
}

vector<pair<vector<int>,Graph>> get_ordered_live_components(const Graph& graph, const Occ::Decls& decls)
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

int get_score(const Occ::Decl& decl)
{
    if (decl.body.to_var())
	return 4;
    else if (decl.body.to_conApp() or decl.body.to_constant())
	return 3;
    else if (decl.x.info.pre_inline())
	return 1;
    else
	return 0;
}

// Find element of component with smallest score in sub_component.
int select_loop_breaker(const vector<int>& sub_component, const vector<int>& component, const Occ::Decls& decls)
{
    std::function<int(int)> score_fn = [&](int k) {return get_score(decls[component[sub_component[k]]]);};
    return sub_component[argmin(sub_component.size(), score_fn)];
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

vector<Occ::Decls>
occurrence_analyze_decl_groups(const Module& m, const std::vector<Core2::Decls<>>& decl_groups, set<Occ::Var>& free_vars)
{
    vector<vector<Occ::Decls>> output;
    for(const auto& decls: reverse(decl_groups))
        output.push_back(occurrence_analyze_decls(m, decls, free_vars));

    std::reverse(output.begin(), output.end());
    return flatten(std::move(output));
}

vector<Occ::Decls>
occurrence_analyze_decls(const Module& m, const Core2::Decls<>& decls_in, set<Occ::Var>& free_vars)
{
    // 1. Determine which vars are alive or dead.
    // 2. Construct reference graph between (live) vars.
    auto [decls, graph] = construct_directed_reference_graph(m, decls_in, free_vars);

    // 3. Copy use information into dummies in decls
    // 4. Remove declared vars from free_vars.
    for(int i=0;i<decls.size();i++)
    {
        auto& x = decls[i].x;
        if (is_alive(x))
        {
            x = remove_var_and_set_occurrence_info(decls_in[i].x, free_vars);
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
                decls[loop_breaker_index].x.info.is_loop_breaker = true;

                // Start
                changed = true;
                break;
            }
        }

        // 6.2. Topo-sort elements of the component now that loops are broken
        component_indices = apply_indices(component_indices, topo_sort(component_graph));
    }

    // 7. Flatten the decl groups
    vector<Occ::Decls> decls2;
    for(auto& component: ordered_components)
    {
        Occ::Decls a_decls;
        for(int i: component.first)
            a_decls.push_back(decls[i]);
        decls2.push_back(a_decls);
    }
    return decls2;
}

set<Occ::Var> dup_work(set<Occ::Var>& vars)
{
    set<Occ::Var> vars2;
    for(auto x: vars)
    {
	if (x.info.work_dup == amount_t::Once)
	    x.info.work_dup = amount_t::Many;
	vars2.insert(x);
    }
    return vars2;
}

// Eta-reduction : if f does not reference x2 then
//                     \x2 ->               ($) f x2  ===>              f
//                 We don't do this (could perform more allocation):
//                     \x2 -> (let decls in ($) f x2) ===> let decls in f
std::optional<Occ::Exp>
maybe_eta_reduce(const Occ::Lambda& L)
{
    if (L.x.info.code_dup != amount_t::Once)
        return {};

    auto A = L.body.to_apply();
    if (not A)
        return {};

    if (A->arg != L.x)
        return {};

    // f x  ==> f
    return A->head;
}

pair<Occ::Var, set<Occ::Var>> occurrence_analyze_var(const Module& m, Core2::Var<> x_in, var_context context)
{
    Occ::Var x{x_in.name, x_in.index, {}, x_in.is_exported};

    // 1. Var
    x.info.is_loop_breaker = false;
    x.info.context = context;
    if (is_local_symbol(x.name, m.name))
    {
        x.info.work_dup = amount_t::Once;
        x.info.code_dup = amount_t::Once;
        return {x, {x}};
    }
    else
    {
        if (not is_haskell_builtin_con_name(x.name))
        {
            assert(is_qualified_symbol(x.name));
            assert(special_prelude_symbol(x.name) or m.lookup_external_symbol(x.name));
        }
        return {x, {}};
    }
}

pair<Occ::Exp,set<Occ::Var>> occurrence_analyzer(const Module& m, const Core2::Exp<>& E, var_context context)
{
    assert(not E.empty());

    // 1. Var
    if (auto V = E.to_var())
    {
	auto [x, free_vars] = occurrence_analyze_var(m, *V, context);
        return {x, free_vars};
    }

    // 2. Lambda (E = \x -> body)
    else if (auto L = E.to_lambda())
    {
	// 1. Analyze the body and marks its variables
	auto [body, free_vars] = occurrence_analyzer(m, L->body);

	// 2. Mark bound variable with occurrence info from the body
	// 3. Remove variable from free variables
	auto x = remove_var_and_set_occurrence_info(L->x, free_vars);

        // 4. Quantify and maybe eta-reduce.
        //    Note that we also eta-reduce in simplifier.cc
        auto unreduced = Occ::Lambda{x,body};
        if (auto reduced = maybe_eta_reduce(unreduced))
            return {*reduced, free_vars};
        else
            // change Once -> OnceInLam / work=Many, code=Once
            return {unreduced, dup_work(free_vars)};
    }
    // 3. Apply
    else if (auto A = E.to_apply())
    {
        set<Occ::Var> free_vars;
        auto [head, head_free_vars] = occurrence_analyzer(m, A->head, var_context::unknown);
        merge_occurrences_into(free_vars, head_free_vars);

        auto [arg, arg_free_vars] = occurrence_analyze_var(m, A->arg, var_context::argument);
        merge_occurrences_into(free_vars, arg_free_vars);

        return {Occ::Apply{head, arg}, free_vars};
    }
    // 4. Let (let {x[i] = F[i]} in body)
    else if (auto L = E.to_let())
    {
	// A. Analyze the body
        auto [F, free_vars] = occurrence_analyzer(m, L->body);

        // B. Analyze the decls
        auto decls_groups = occurrence_analyze_decls(m, L->decls, free_vars);

        // C. Wrap the decls around the body
        for(auto& decls: reverse(decls_groups))
            F = Occ::Let{decls,F};

	return {F, free_vars};
    }
    // 5. Case
    else if (auto C = E.to_case())
    {
        // Analyze the object
        auto [object, free_vars] = occurrence_analyzer(m, C->object);

        // Just normalize the bodies
        set<Occ::Var> alts_free_vars;

        Occ::Alts alts;
        for(auto& [pattern, body]: C->alts)
        {
            // Analyze the i-ith branch
            auto [occ_body, alt_free_vars] = occurrence_analyzer(m, body);

            // Remove pattern vars from free variables
            // Copy occurrence info into pattern variables
            Occ::Pattern occ_pattern;
            occ_pattern.head = pattern.head;
            for(auto& arg: pattern.args)
            {
                auto x = remove_var_and_set_occurrence_info(arg, alt_free_vars);
                occ_pattern.args.push_back(x);
            }

            // Merge occurrences for this pattern into the occurrence for the whole set of alts.
            merge_occurrences_into(alts_free_vars, alt_free_vars, true);

            alts.push_back({occ_pattern, occ_body});
        }

        // We can avoid inlining directly into alternatives, since this might duplicate work.
        // merge_occurrences_into(free_vars, dup_work(alts_free_vars));
        merge_occurrences_into(free_vars, alts_free_vars);

        return {Occ::Case{object,alts}, free_vars};
    }
    // 6. ConApp
    else if (auto C = E.to_conApp())
    {
        set<Occ::Var> free_vars;

        vector<Occ::Var> args;

        for(auto& arg: C->args)
        {
            auto [occ_arg, arg_free_vars] = occurrence_analyze_var(m, arg, var_context::argument);
            args.push_back(occ_arg);
            merge_occurrences_into(free_vars, arg_free_vars);
        }

        return {Occ::ConApp{C->head, args}, free_vars};
    }
    // 7. BuiltinOp
    else if (auto B = E.to_builtinOp())
    {
        set<Occ::Var> free_vars;

        vector<Occ::Var> args;

        for(auto& arg: B->args)
        {
            auto [occ_arg, arg_free_vars] = occurrence_analyze_var(m, arg, var_context::argument);
            args.push_back(occ_arg);
            merge_occurrences_into(free_vars, arg_free_vars);
        }

        return {Occ::BuiltinOp{B->lib_name, B->func_name, args}, free_vars};
    }
    // 8. Constant
    else if (auto C = E.to_constant())
        return {*C, {}};
    else
        throw myexception()<<"occurrence_analyzer: I don't recognize expression '"+ E.print() + "'";
}
