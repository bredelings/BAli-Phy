#include <vector>
#include <set>
#include "let-float.H"
#include "computation/expression/expression.H" // for is_reglike( )
#include "computation/expression/substitute.H"
#include "computation/expression/var.H"
#include "computation/expression/lambda.H"
#include "computation/expression/let.H"
#include "computation/expression/apply.H"
#include "computation/expression/constructor.H"
#include "computation/expression/case.H"
#include "computation/expression/operator.H"
#include "computation/operation.H"
#include "computation/module.H"
#include "util/set.H"

#include "free-vars.H"
#include "immer/map.hpp" // for immer::map

#include "range/v3/all.hpp"
namespace view = ranges::view;
namespace action = ranges::action;

using std::vector;
using std::set;
using std::pair;
using std::string;

// ?- Determine which let statements have bound vars (bound_indices) and which do not (unbound_indices).
//    (The ones with no bound vars can be floated.)
bool find_let_statements_with_bound_vars(const vector<pair<var,expression_ref>>& decls,
					 const set<var>& bound,
					 vector<int>& bound_indices, vector<int>& unbound_indices)
{
    // Find the set of bound variables that could be free in let_bodies
    set<var> visible_bound = bound;
    set<var> let_bound;
    for(int i=0;i<decls.size();i++)
    {
	auto x = decls[i].first;
	let_bound.insert(x);
	visible_bound.erase(x);
    }

    vector< set<var> > free_vars;
    for(int i=0;i<decls.size();i++)
	free_vars.push_back( get_free_indices( decls[i].second ) );

    bound_indices.clear();
    unbound_indices.clear();
    for(int i=0;i<decls.size();i++)
	unbound_indices.push_back(i);

    // Find the indices that are not bound (directly or indirectly) by the bound variables
    set<var> new_bound = visible_bound;
    while (not new_bound.empty())
    {
	set<var> new_bound_next;
	for(int i=unbound_indices.size()-1;i>=0;i--)
	{
	    int index = unbound_indices[i];
	    auto x = decls[index].first;
	    if (not intersection(free_vars[index], new_bound).empty())
	    {
		new_bound_next.insert(x);
		bound_indices.push_back(index);
		unbound_indices.erase( unbound_indices.begin() + i);
	    }
	}
	new_bound = new_bound_next;
    }

    return (not unbound_indices.empty());
}

//question: is move_lets supposed to be called with empty vars?
//answer: yes, sometimes.

/// Given let vars=bodies in (<binder bound> (let E_vars=E_bodies in T)), 
///  move some of the E_vars=E_bodies up to vars=bodies.
expression_ref move_lets(bool scope, const expression_ref E,
			 vector<pair<var,expression_ref>>& decls,
			 const set<var>& bound, const set<var>& free)
{
    assert(E);

    vector<pair<var,expression_ref>> E_decls;
    expression_ref E2 = E;

    if (not parse_let_expression(E, E_decls, E2))
	E2 = E;

    // Find the set of variables to avoid renaming over: free + bound + let-bound-just-above
    //    (Hmm... should the let-bound-just-above be in 'bound'?)
    set<var> avoid = free;
    for(int i=0;i<decls.size();i++)
    {
	var x = decls[i].first;
	avoid.insert(x);
	add(avoid, get_free_indices(decls[i].second));
    }
    add(avoid, get_free_indices(E));
    add(avoid, bound);

    int new_index = max_index(avoid) + 1;
    

    // Determine which of the let-statements in E we can float.
    vector<int> unbound_indices;
    vector<int> bound_indices;
    if (find_let_statements_with_bound_vars(E_decls, bound, bound_indices, unbound_indices))
    {
	// Adjust the new indices to avoid hitting any of the other let-binder-variables in E
	for(int i=0;i<E_decls.size();i++)
	{
	    var x = E_decls[i].first;
	    new_index = std::max(new_index, x.index + 1);
	}

	/******************** alpha-rename E -> EE ********************/
    
	object_ptr<expression> EE = E.as_expression().clone();                  // Make a copy of E that we can alpha-rename.
    
	for(int index: unbound_indices)
	{
	    var D = E_decls[index].first;
	    if (includes(avoid, D))
	    {
		var D2(new_index++);
		assert(not includes(avoid,D2));
		alpha_rename(EE, D, D2);
		avoid.insert(D2);
	    }
	}
  
	// Recompute E_vars and E_bodies from the alpha-renamed version of E
	parse_let_expression(EE, E_decls, E2);            
    
	/*********** move free lets to higher-level environment **********/
	for(int index: unbound_indices)
	{
#ifndef NDEBUG
	    // Check that we aren't duplicating any variables in the higher-level environment
	    //      for(int j=0;j<vars.size();j++)
	    //	assert(not includes(vars, E_vars[index]));
#endif
	    decls.push_back( E_decls[index] );
	}

	// Construct the remainder expression
	vector<pair<var,expression_ref>> E_decls2;
	for(int i=0;i<bound_indices.size();i++)
	{
	    int index = bound_indices[i];
      
	    E_decls2.push_back(E_decls[index]);
	}

	E2 = let_expression(E_decls2, E2);
    }
    // If nothing is moveable, then just return the original statement.
    else
	E2 = E;

    // We can't float this out because its bound, or because there's no bound to float it through.
    if ((not scope) or (not intersection(get_free_indices(E2), bound).empty()))
    {
	assert(E2);
	return E2;
    }

    // Since we only substitute reg_vars into var's (for let, lambda, and case) these are all OK.
    // change to is_reg_like
    if (is_reglike(E2))
    {
	assert(E2);
	return E2;
    }


    // If E2 is not bound, and its not a let-bound var, then create a new expression for it.
    var D2(new_index++);
    decls.push_back({D2, E2});
    return D2;
}

template <typename T>
bool operator==(const std::set<T>& S1, const std::set<T>& S2)
{
    return includes(S1,S2) and includes(S2,S2);
}

// When we let_float \x.\y.x, we should float out x, even though its a var

// However, if we have let {z=2} in \x.\y.z, we should not introduce a let var
// for z, because its already let bound.

expression_ref let_float(const expression_ref& E)
{
    // 0. NULL
    if (not E) return E;

    // 1. Dummy variable
    // 2. Literal constants.  Treat as 0-arg constructor.
    if (not E.size()) return E;
  
    set<var> free_in_E = get_free_indices(E);

    vector<pair<var, expression_ref>> decls;
    vector<expression_ref> patterns;
    vector<expression_ref> branches;
    expression_ref object;
    expression_ref T;
    expression_ref E2;
    
    // 3. Lambda expressions
    if (E.head().is_a<lambda>())
    {
	// Find the new let-bound set.
	var x= E.sub()[0].as_<var>();

	// First float lets in sub-expressions
	expression_ref M = let_float(E.sub()[1]);

	// Determine the bound indices
	set<var> bound = {x};

	// Move lets across the lambda
	M = move_lets(true, M, decls, bound, free_in_E);

	// Reassemble the expression
	E2 = let_expression(decls, lambda_quantify(x, M) );

	assert(free_in_E == get_free_indices(E2));
    }

    // 4. Case expressions
    else if (parse_case_expression(E,object,patterns,branches))
    {
	// First float out of case object (bound = {}, free = fv(E))
	object = let_float(object);
	object = move_lets(true, object, decls, set<var>(), free_in_E);

	for(int i=0;i<branches.size();i++)
	{
	    // Find the bound variables in the i-th constructor
	    set<var> bound = get_free_indices(patterns[i]);

	    // Second float out of the case alternative branches (bound = fv(patterns[i]), free = fv(E))
	    // (Note: free = fv(E) is a bit conservative.)
	    branches[i] = let_float(branches[i]);
	    branches[i] = move_lets(true, branches[i], decls, bound, free_in_E);
	}

	E2 = let_expression(decls, make_case_expression(object, patterns, branches));

	assert(free_in_E == get_free_indices(E2));
    }

    // 5. Let expressions
    else if (parse_let_expression(E,decls,T))
    {
	// Return let_float(T) if T doesn't mention any of the newly let-bound variables
	set<var> bound_vars_let;
	for(int i=0;i<decls.size();i++)
	    bound_vars_let.insert(decls[i].first);

	set<var> free_vars_T = get_free_indices(T);
	if (intersection(bound_vars_let, free_vars_T).empty()) 
	    return let_float(T);

	// First float lets in sub-expressions
	T = let_float(T);
	for(auto& decl: decls)
	    decl.second = let_float(decl.second);

	// Move lets out of T and into vars
	T = move_lets(false, T, decls, set<var>(), free_in_E);

	// Move lets out of bodies and into vars
	for(int i=0;i<decls.size();i++)
	{
	    // Note that bodies[i] might refer to a different object, if bodies is resized during move_lets.
	    expression_ref E = move_lets(false, decls[i].second, decls, set<var>(), free_in_E);
	    // Therefore ensure that bodies[i] refer to the ith element of bodies AFTER any possible resize.
	    decls[i].second = E;
	}

	E2 = let_expression(decls, T);
    }

    // 6. Handle application, constructors, and operations.
    else if (E.head().is_a<Operator>())
    {
	decls.clear();
	// First float lets in sub-expressions
	object_ptr<expression> V = E.as_expression().clone();
    
	// Move lets from arguments into (vars,bodies)
	for(int i=0;i<E.size();i++)
	{
	    V->sub[i] = let_float(V->sub[i]);
	    V->sub[i] = move_lets(true, V->sub[i], decls, set<var>(), free_in_E);
	}
      
	E2 = let_expression(decls, object_ptr<const expression>(V));

	assert(free_in_E == get_free_indices(E2));
    }
    else
	throw myexception()<<"let_float: I don't understand expression '"<<E<<"'";

#ifndef NDEBUG
    set<var> S2 = get_free_indices(E2);
    assert(free_in_E == S2);
#endif

    return E2;
}

const expression_ref un_fv(const expression_ref& AE)
{
    return AE.as_<annot_expression_ref<FreeVarSet>>().exp;
}

// This maps the in-name to (i) the out-name and (ii) the level, which is stored on the out-name.
typedef immer::map<var,var> level_env_t;

int max_level(const level_env_t& env, const FreeVarSet& free_vars)
{
    // Global variables that are free will not be in the env, so just ignore them.
    int level = 0;
    for(auto& x: free_vars)
        if (auto x_out = env.find(x))
        {
            assert(x_out->level);
            level = std::max(level, *x_out->level);
        }
    return level;
}

struct let_floater_state
{
//    const Module& m;
    std::map<string,int> used_index_for_name;

    var new_unique_var(const var& x, int level);
    var new_unique_var(const string& name, int level);

    expression_ref set_level(const expression_ref& AE, int level, const level_env_t& env);
    expression_ref set_level_maybe_MFE(const expression_ref& AE, int level, const level_env_t& env);

    level_env_t set_level_decl_group(CDecls& decls, const level_env_t& env);

//    let_floater_state(const Module& m_):m(m_) {}
};

var let_floater_state::new_unique_var(const var& x, int level)
{
    assert(not x.is_exported);
    return new_unique_var(x.name, level);
}


var let_floater_state::new_unique_var(const string& name, int level)
{
    assert(not is_haskell_builtin_con_name(name));
    // qualified names are actually allowed here, as long as they are not exported.

    int index = 1;
    auto iter = used_index_for_name.find(name);
    if (iter == used_index_for_name.end())
        used_index_for_name.insert({name,index});
    else
    {
        iter->second++;
        index = iter->second;
    }
    auto x2 = var(name,index);
    x2.level = level;
    return x2;
}

var strip_level(var x)
{
    x.level.reset();
    return x;
}

expression_ref strip_level_from_pattern(const expression_ref& pattern)
{
    if (is_var(pattern))
        return strip_level(pattern.as_<var>());
    else if (not pattern.size())
        return pattern;
    else
    {
        object_ptr<expression> pattern2 = pattern.as_expression().clone();

        for(auto& Ex: pattern2->sub)
        {
            assert(is_var(Ex));

            Ex = strip_level_from_pattern(Ex);
        }
        return pattern2;
    }
}

var subst_var(var x, const level_env_t& env)
{
    // We should handle this in ... desugar?
    if (is_wildcard(x)) return x; 

    auto record = env.find(x);
    assert(record);

    x.level = record->level;
    x.name = record->name;
    x.index = record->index;

    return x;
}

var subst_var(const expression_ref& E, const level_env_t& env)
{
    const auto& x = E.as_<var>();
    return subst_var(x, env);
}

expression_ref subst_pattern(const expression_ref& pattern, const level_env_t& env)
{
    // I THINK that these should never be VARs in the current paradigm... but we should fix that.

    if (is_var(pattern))
        return strip_level(subst_var(pattern, env));
    else if (not pattern.size())
        return pattern;
    else
    {
        object_ptr<expression> pattern2 = pattern.as_expression().clone();

        for(auto& Ex: pattern2->sub)
        {
            assert(is_var(Ex));

            Ex = subst_pattern(Ex, env);
        }
        return pattern2;
    }
}

level_env_t let_floater_state::set_level_decl_group(CDecls& decls, const level_env_t& env)
{
    FreeVarSet free_vars;
    vector<var> binders;
    for(auto& [x,rhs]: decls)
    {
        free_vars = get_union(free_vars, get_free_vars(rhs));
        binders.push_back(x);
    }
    free_vars = erase(free_vars, binders);

    int level2 = max_level(env, free_vars);

    auto env2 = env;
    for(auto& [x,rhs]: decls)
    {
        if (not x.is_exported)
        {
            auto x2 = new_unique_var(x, level2);
            env2 = env2.insert({x,x2});
            x = x2;
        }
    }

    for(auto& [var,rhs]: decls)
        rhs = set_level(rhs, level2, env2);

    return env2;
}

expression_ref let_floater_state::set_level(const expression_ref& AE, int level, const level_env_t& env)
{
    const auto& E = AE.as_<annot_expression_ref<FreeVarSet>>().exp;

    // 1. Var
    if (is_var(E))
    {
        const auto& x = E.as_<var>();
        // Top-level symbols from this module and other modules won't be in the env.
        if (auto x_out = env.find(x))
            return strip_level(*x_out);
        else
            return E;
    }

    // 2. Constant
    else if (not E.size())
        return E;

    // 3. Apply or constructor or Operation
    else if (is_apply_exp(E) or is_constructor_exp(E) or is_non_apply_op_exp(E))
    {
        object_ptr<expression> V2 = E.as_expression().clone();

        // All the arguments except the first one are supposed to be vars .... I think?
        for(int i=0;i<E.size();i++)
            V2->sub[i] = set_level(V2->sub[i], level, env);

        return V2;
    }

    // 4. Lambda
    else if (is_lambda_exp(E))
    {
        int level2 = level + 1;
        auto env2 = env;

        vector<var> args;
        auto AE2 = AE;
        while(is_lambda_exp(un_fv(AE2)))
        {
            auto& E2 = un_fv(AE2);

            auto x = E2.sub()[0].as_<var>();

            // assert that none of the other args have the same name!
            // we should check this in the renamer, I think.

            auto x2 = new_unique_var(x, level2);
            env2 = env2.insert({x,x2});

            args.push_back(x2);
            AE2 = E2.sub()[1];
        }

        auto E2 = set_level_maybe_MFE(AE2, level2, env2);

        for(auto x2 : args | view::reverse)
            E2 = lambda_quantify(strip_level(x2),E2);

        return E2;
    }

    // 4. Case
    else if (is_case(E))
    {
        expression_ref object;
        vector<expression_ref> patterns;
        vector<expression_ref> bodies;
        parse_case_expression(E, object, patterns, bodies);

        auto object2 = set_level_maybe_MFE(object, level, env);

        vector<expression_ref> patterns2(patterns.size());
        vector<expression_ref> bodies2(bodies.size());
        for(int i=0;i<bodies2.size();i++)
        {
            int level2 = level+1; // Increment level, since we're going to float out of case alternatives.
            auto binders = get_vars(patterns[i]);
            auto env2 = env;
            for(auto binder: binders)
            {
                auto binder2 = new_unique_var(binder, level2);
                env2 = env2.insert({binder,binder2});
            }
            patterns2[i] = subst_pattern(patterns[i], env2);
            bodies2[i] = set_level_maybe_MFE(bodies[i], level2, env2);
        }

        return make_case_expression(object2,patterns2,bodies2);
    }

    // 5. Let
    else if (is_let_expression(E))
    {
        auto decls = let_decls(E);
        auto env2 = set_level_decl_group(decls, env);

        auto body = let_body(E);
        auto body2 = set_level_maybe_MFE(body, level, env2);

        return let_expression(decls,body2);
    }

    std::abort();
}

expression_ref let_floater_state::set_level_maybe_MFE(const expression_ref& AE, int level, const level_env_t& env)
{
    int level2 = max_level(env, get_free_vars(AE));
    const auto& E = un_fv(AE);
    if (level2 < level and not is_var(E) and not is_WHNF(E))
    {
        auto E = set_level(AE, level2, env);
        var v = new_unique_var("$v", level2);
        return let_expression({{v,E}},v);
    }
    else
        return set_level(AE, level, env);
}

void set_level_for_module(vector<CDecls>& module)
{
    vector<CDecls> module_out;

    let_floater_state state;
    level_env_t env;
    for(auto& decls: module)
    {
        for(auto& [x,rhs]: decls)
            rhs = add_free_variable_annotations(rhs);
        env = state.set_level_decl_group(decls, env);
    }
}

int get_level(const CDecls& decl_group)
{
    assert( not decl_group.empty() );
    assert( decl_group[0].first.level );

    auto level = *decl_group[0].first.level;

#ifndef NDEBUG
    for(int i=1; i<decl_group.size(); i++)
    {
        assert( decl_group[i].first.level );
        assert( *decl_group[i].first.level == level );
    }
#endif

    return level;
}

typedef std::map<int,vector<CDecls>> float_binds_t;

vector<CDecls> get_decl_groups_at_level(float_binds_t& float_binds, int level)
{
    auto iter = float_binds.find(level);
    if (iter == float_binds.end())
        return {};

    vector<CDecls> decl_groups;
    std::swap(decl_groups, iter->second);
    float_binds.erase(iter);

    return decl_groups;
}

pair<vector<var>,expression_ref> get_lambda_binders(expression_ref E)
{
    assert(is_lambda_exp(E));
    vector<var> binders;
    while(is_lambda_exp(E))
    {
        auto x = E.sub()[0].as_<var>();
        binders.push_back(x);
        E = E.sub()[1];
    }
    return {std::move(binders), E};
}

expression_ref make_lambda(const vector<var>& args, expression_ref E)
{
    for(auto x : args | view::reverse)
        E = lambda_quantify(x,E);
    return E;
}

float_binds_t
float_lets(expression_ref& E, int level);

expression_ref install_current_level(float_binds_t& float_binds, int level, const expression_ref& E)
{
    auto decl_groups_here = get_decl_groups_at_level(float_binds, level);
    return let_expression(decl_groups_here, E);
}

float_binds_t
float_lets_install_current_level(expression_ref& E, int level)
{
    auto float_binds = float_lets(E,level);
    E = install_current_level(float_binds, level, E);
    return float_binds;
}

void append(vector<CDecls>& decl_groups1, vector<CDecls>& decl_groups2)
{
    assert(&decl_groups1 != &decl_groups2);
    for(auto& decls: decl_groups2)
        decl_groups1.push_back(std::move(decls));
}

void append(float_binds_t& float_binds1, float_binds_t& float_binds2)
{
    assert(&float_binds1 != &float_binds2);
    for(auto& [level,decl_groups]: float_binds2)
    {
        if (auto iter = float_binds1.find(level); iter != float_binds1.end())
            append(float_binds1[level], decl_groups);
        else
            float_binds1[level] = std::move(decl_groups);
    }
}

pair<float_binds_t,int> float_out_from_decl_group(CDecls& decls)
{
    int level2 = get_level(decls);

    float_binds_t float_binds;
    for(auto& [x,rhs]: decls)
    {
        x = strip_level(x);
        auto float_binds_x = float_lets_install_current_level(rhs, level2);

        append(float_binds, float_binds_x);
    }

    return pair<float_binds_t,int>(std::move(float_binds), level2);
}

float_binds_t
float_lets(expression_ref& E, int level)
{
    // 1. Var
    if (is_var(E))
        return {};

    // 2. Constant
    else if (not E.size())
        return {};

    // 3. Constructor or Operation
    else if (is_constructor_exp(E) or is_non_apply_op_exp(E))
        return {};

    // 4. Apply @ E x1 x2 x3 ... x[n-1];
    else if (is_apply_exp(E))
    {
        object_ptr<expression> V2 = E.as_expression().clone();
        auto float_binds = float_lets(V2->sub[0], level);
#ifndef NDEBUG
        for(int i=1;i<V2->sub.size();i++)
                assert(is_var(V2->sub[i]));
#endif
        E = V2;
        return float_binds;
    }

    // 5. Lambda
    else if (is_lambda_exp(E))
    {
        auto [binders,body] = get_lambda_binders(E);
        for(auto& x: binders)
            x = strip_level(x);

        int level2 = level + 1;

        auto float_binds = float_lets_install_current_level(body, level2);

        E = make_lambda(binders,body);

        return float_binds;
    }

    // 6. Case
    else if (is_case(E))
    {
        expression_ref object;
        vector<expression_ref> patterns;
        vector<expression_ref> bodies;
        parse_case_expression(E, object, patterns, bodies);

        auto float_binds = float_lets(object, level);
        int level2 = level + 1;
        for(int i=0; i<patterns.size(); i++)
        {
            patterns[i] = strip_level_from_pattern(patterns[i]);
            auto float_binds_alt = float_lets_install_current_level(bodies[i],level2);

            append(float_binds, float_binds_alt);
        }

        E = make_case_expression(object,patterns,bodies);
        return float_binds;
    }

    // 7. Let
    else if (is_let_expression(E))
    {
        auto decls = let_decls(E);
        auto body = let_body(E);

        auto [float_binds, level2] = float_out_from_decl_group(decls);
        assert(level2 <= level);

        auto float_binds_from_body = float_lets(body, level);

        append(float_binds, float_binds_from_body);

        if (level2 < level)
        {
            // The decls here have to go BEFORE the decls from the (i) the body and (ii) the decl rhs's.
            float_binds_t float_binds_first;
            float_binds_first[level2].push_back(std::move(decls));
            append(float_binds_first, float_binds);
            std::swap(float_binds_first, float_binds);
            E = body;
        }
        // Prevents floated bindings at the same level from getting installed HIGHER than
        // bindings that they reference.
        // Does this place floated bindings as deep as possible at the correct level?
        else
            E = let_expression(decls, install_current_level(float_binds, level, body));

        return float_binds;
    }

    std::abort();
}

expression_ref float_lets(const expression_ref& E)
{
    auto E2 = E;
    auto float_binds = float_lets(E2,0);

    assert(float_binds.size() <= 1);
    if (float_binds.size() == 1)
    {
        return let_expression(float_binds.at(0),E2);
    }
    else
        return E2;
}

void float_out_from_module(vector<CDecls>& decl_groups)
{
    set_level_for_module(decl_groups);

    for(auto& decl_group: decl_groups)
        for(auto& [x,rhs]: decl_group)
            rhs = float_lets(rhs);
}
