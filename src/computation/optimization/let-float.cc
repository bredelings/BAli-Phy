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

#include "immer/map.hpp" // for immer::map

#include "util/string/join.H"

#include "range/v3/all.hpp"
namespace view = ranges::view;

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

namespace std
{
    template <>
    class hash < var >{
    public :
        size_t operator()(const var &x) const
        {
            size_t h =   std::hash<int>()(x.index) ^ std::hash<std::string>()(x.name);
            return  h ;
        }
    };
}

template <>
string annot_expression_ref<FreeVarSet>::print() const
{
    if (note.empty())
        return exp.print();

    vector<string> xs;
    for(auto& x: note)
        xs.push_back(x.print());

    string s = "{"+join(xs,",")+"|" + exp.print()+"}";
    return s;
}


const FreeVarSet& get_free_vars(const annot_expression_ref<FreeVarSet>& e)
{
    return e.note;
}

const FreeVarSet& get_free_vars(const expression_ref& E)
{
    return get_free_vars(E.as_<annot_expression_ref<FreeVarSet>>());
}

const expression_ref un_fv(const expression_ref& AE)
{
    return AE.as_<annot_expression_ref<FreeVarSet>>().exp;
}

// from simplifier.cc
vector<var> get_used_vars(const expression_ref& pattern);

FreeVarSet get_union(const FreeVarSet& s1, const FreeVarSet& s2)
{
    if (s1.size() >= s2.size())
    {
        FreeVarSet s3 = s1;
        for(auto& x: s2)
            s3 = s3.insert(x);
        return s3;
    }
    else
    {
        FreeVarSet s3 = s2;
        for(auto& x: s1)
            s3 = s3.insert(x);
        return s3;
    }
}

[[nodiscard]] FreeVarSet erase(const FreeVarSet& s, const vector<var>& xs)
{
    FreeVarSet s2 = s;
    for(auto& x: xs)
        s2 = s2.erase(x);
    return s2;
}

annot_expression_ref<FreeVarSet>
add_free_variable_annotations(const expression_ref& E)
{
    // 1. Var
    if (is_var(E))
    {
        const auto& x = E.as_<var>();
        FreeVarSet free_vars;
        return {free_vars.insert(x), E};
    }

    // 2. Constant
    else if (not E.size())
    {
        return {FreeVarSet{},E};
    }

    // 3. Lambda
    else if (is_lambda_exp(E))
    {
        const auto& x = E.sub()[0].as_<var>();
        const auto& body = E.sub()[1];

        auto body2 = add_free_variable_annotations(body);
        auto free_vars = get_free_vars(body2);

        return {free_vars.erase(x), lambda_quantify(x, body2)};
    }

    // 4. Case
    else if (is_case(E))
    {
        expression_ref object;
        vector<expression_ref> patterns;
        vector<expression_ref> bodies;
        parse_case_expression(E, object, patterns, bodies);

        auto object2 = add_free_variable_annotations(object);
        auto free_vars = get_free_vars(object2);

        vector<expression_ref> bodies2(bodies.size());
        for(int i=0;i<bodies2.size();i++)
        {
            auto b2 = add_free_variable_annotations(bodies[i]);
            auto free_vars_i = erase(get_free_vars(b2), get_used_vars(patterns[i]));
            free_vars = get_union(free_vars, free_vars_i);
            bodies2[i] = b2;
        }

        return {free_vars,make_case_expression(object2,patterns,bodies2)};
    }

    // 5. Let
    else if (is_let_expression(E))
    {
        auto decls = let_decls(E);
        auto body = let_body(E);

        auto body2 = add_free_variable_annotations(body);
        FreeVarSet free_vars = get_free_vars(body2);
        vector<var> binders;

        for(auto& [var,rhs]: decls)
        {
            binders.push_back(var);
            rhs = add_free_variable_annotations(rhs);
            free_vars = get_union(free_vars, get_free_vars(rhs));
        }
        free_vars = erase(free_vars, binders);

        return {free_vars, let_expression(decls,body2)};
    }

    // 6. Apply or constructor or Operation
    else if (is_apply_exp(E) or is_constructor_exp(E) or is_non_apply_op_exp(E))
    {
        object_ptr<expression> V2 = E.as_expression().clone();

        FreeVarSet free_vars;
        for(int i=0;i<E.size();i++)
        {
            V2->sub[i] = add_free_variable_annotations(V2->sub[i]);
            auto free_vars_i = get_free_vars(V2->sub[i]);
            free_vars = get_union(free_vars, free_vars_i);
        }
        return {free_vars,V2};
    }

    std::abort();
}

// This maps the in-name to (i) the out-name and (ii) the level, which is stored on the out-name.
typedef immer::map<var,var> level_env_t;

int max_level(const level_env_t& env, const FreeVarSet& free_vars)
{
    // Global variables that are free will not be in the env, so just ignore them.
    int level = 0;
    for(auto& x: free_vars)
        if (auto x_out = env.find(x))
            level = std::max(level, *x_out->level);
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

//    let_floater_state(const Module& m_):m(m_) {}
};

var let_floater_state::new_unique_var(const var& x, int level)
{
    return new_unique_var(x.name, level);
}


var let_floater_state::new_unique_var(const string& name, int level)
{
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

var subst_var(const var& x, const level_env_t& env)
{
    // We should handle this in ... desugar?
    if (is_wildcard(x)) return x; 

    auto record = env.find(x);
    assert(record);
    return *record;
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
            int level2 = level; // only minor level incremented.
            auto binders = get_used_vars(patterns[i]);
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
        auto body = let_body(E);

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
            auto x2 = new_unique_var(x, level2);
            env2 = env2.insert({x,x2});
        }

        auto body2 = set_level_maybe_MFE(body, level2, env2);

        for(auto& [var,rhs]: decls)
        {
            var = subst_var(var, env2);
            rhs = set_level(rhs, level2, env2);
        }

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

expression_ref set_level(const expression_ref& E)
{
    let_floater_state l_f_s;
    return l_f_s.set_level(add_free_variable_annotations(E), 0, {});
}
