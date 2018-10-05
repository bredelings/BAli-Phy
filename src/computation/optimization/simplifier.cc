#include <iostream>
#include <unordered_map>
#include <boost/optional.hpp>
#include "computation/operations.H"
#include "computation/loader.H"
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
#include "inliner.H"

#include "simplifier.H"
#include "util/assert.hh"

// TODO: when building let expressions to bind variables, pass those expressions into the simplifier
//       * 1. stop translating wildcards to named variables.
//       * 2. make the occurrence analyzer replace dead variables with wildcards in case patterns and lambdas.
//       * 3. move case-of-constant analysis ahead of normal case analysis, so that we can simplify the let-statement we create.
//       * 4. share as much code as possible between the case-of-constant and normal case analysis.
// TODO: make simplify_decls actually take a decl_groups, and return a new one by reference.
// TODO: pass in decl_groups to simplify_module instead of just decls.  Treat later decls as body of early one for aliveness purposes.
//  - so, for the occurrence analyzer, we process a series of decl_groups in REVERSE order.
// TODO: Can we process earlier topdecl groups until done instead of passing over the whole thing multiple times.
// - I *think* so.  as we 5

using std::string;
using std::vector;
using std::pair;
using std::set;
using std::map;

using std::cerr;
using std::endl;


struct substitution_range;
struct substitution: public map<var, substitution_range>
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

bool is_trivial(const expression_ref& E)
{
    return is_reglike(E);
}

void bind_var(in_scope_set& bound_vars, const var& x, const expression_ref& E)
{
    assert(not bound_vars.count(x));
    assert(x.work_dup != amount_t::Unknown);
    assert(x.code_dup != amount_t::Unknown);
    bound_vars.insert({x,{E,x}});
}

void unbind_var(in_scope_set& bound_vars, const var& x)
{
    assert(bound_vars.count(x));
    bound_vars.erase(x);
}

bound_variable_info rebind_var(in_scope_set& bound_vars, const var& x, const expression_ref& E)
{
    bound_variable_info old_binding = bound_vars.at(x);
    unbind_var(bound_vars,x);
    var x2 = x;
    static_cast<occurrence_info&>(x2) = old_binding.second;
    bind_var(bound_vars,x2,E);
    return old_binding;
}

void bind_decls(in_scope_set& bound_vars, const CDecls& decls)
{
    for(const auto& decl: decls)
	bind_var(bound_vars, decl.first, decl.second);
}

void bind_decls(in_scope_set& bound_vars, const vector<CDecls>& decl_groups)
{
    for(auto& decls: decl_groups)
	bind_decls(bound_vars, decls);
}

void unbind_decls(in_scope_set& bound_vars, const CDecls& decls)
{
    for(const auto& decl: decls)
	unbind_var(bound_vars, decl.first);
}

void unbind_decls(in_scope_set& bound_vars, const vector<CDecls>& decl_groups)
{
    for(auto& decls: reverse(decl_groups))
	unbind_decls(bound_vars, decls);
}

expression_ref simplify(const simplifier_options& options, const expression_ref& E, const substitution& S, in_scope_set& bound_vars, const inline_context& context);

// Do we have to explicitly skip loop breakers here?
expression_ref consider_inline(const simplifier_options& options, const expression_ref& E, in_scope_set& bound_vars, const inline_context& context)
{
    var x = E.as_<var>();

    const auto& binding = bound_vars.at(x);

//    std::cerr<<"Considering inlining "<<E.print()<<" -> "<<binding.first<<" in context "<<context.data<<std::endl;
    
    // 1. If there's a binding x = E, and E = y for some variable y
    if (binding.first and do_inline(options, binding.first, binding.second, context))
	return simplify(options, binding.first, {}, bound_vars, context);
    else
	return E;
}

var get_new_name(var x, const in_scope_set& bound_vars)
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

var rename_and_bind_var(const expression_ref& Evar, substitution& S, in_scope_set& bound_vars)
{
    var x = Evar.as_<var>();
    assert(x.code_dup != amount_t::Unknown);
    assert(not is_wildcard(x));
    var x2 = get_new_name(x, bound_vars);

    // 1. If x is NOT in the bound set, then erase x from the substitution (if it's there)
    if (x == x2)
	S.erase(x);
    // 2. If x IS in the bound set, add a substitution from x --> x2 then erase x from the substitution (if it's there)
    else
    {
	S.erase(x);
	S.insert({x, expression_ref(x2)});
    }

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
	else if (is_var(patterns[i].head()))
	{
	    assert(not patterns[i].size());
	    if (bodies[i] != patterns[i] and bodies[i] != object) return false;
	}
	else if (patterns[i] != bodies[i]) return false;
    }

    return true;
}


void get_pattern_dummies(const expression_ref& pattern, set<var>& vars)
{
    if (is_var(pattern))
    {
	auto& x = pattern.as_<var>();
	if (not x.is_wildcard())
	    vars.insert(x);
    }
    else if (pattern.size() > 0)
	for(auto& y: pattern.sub())
	    get_pattern_dummies(y, vars);
}


// Get a list of patterns.size() names for let-bound variables that don't alias any bound vars  in patterns or patterns2
vector<var> get_body_function_names(in_scope_set& bound_vars, const vector<expression_ref>& patterns, const vector<expression_ref>& patterns2)
{
    vector<var> lifted_names;

    int orig_size = bound_vars.size();

    // 1. Get the list of dummies to avoid
    set<var> avoid;
    for(auto& pattern: patterns)
	get_pattern_dummies(pattern, avoid);
    for(auto& pattern: patterns2)
	get_pattern_dummies(pattern, avoid);

    // 2. Bind the unbound ones into scope
    set<var> added;
    for(auto& x:avoid)
	if (not bound_vars.count(x))
	{
	    added.insert(x);
	    bind_var(bound_vars, x, {});
	}

    // 3. Make some new names, putting them into scope as we go.
    var z("_cc");
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
    for(auto& x: added)
	unbind_var(bound_vars, x);

    // 6. The scope size should now be the same size as when we started.
    assert(bound_vars.size() == orig_size);

    return lifted_names;
}

bool is_used_var(const var& x)
{
    return (not x.is_wildcard() and x.code_dup != amount_t::None);
}

bool is_used_var(const expression_ref& x)
{
    if (not is_var(x)) return false;
    return is_used_var(x.as_<var>());
}

vector<var> get_used_vars(const expression_ref& pattern)
{
    vector<var> used;

    if (is_used_var(pattern))
	used.push_back(pattern.as_<var>());
    else if (pattern.is_expression())
	for(auto& Evar: pattern.sub())
	    if (is_used_var(Evar.as_<var>()))
		used.push_back(Evar.as_<var>());

    return used;
}

bool has_used_vars(const expression_ref& pattern)
{
    if (is_used_var(pattern))
	return true;
    else if (pattern.is_expression())
	for(auto& Evar: pattern.sub())
	    if (is_used_var(Evar.as_<var>()))
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

    auto decls = strip_multi_let(object);
    bind_decls(bound_vars, decls);

    // NOTE: Any thing that relies on occurrence info for pattern vars should be done here, before
    //       we simplify alternatives, because that simplification can introduce new uses of the pattern vars.
    // Example: case #1 of {x:xs -> case #1 of {y:ys -> ys}} ==> case #1 of {x:xs -> xs} 
    //       We set #1=x:xs in the alternative, which means that a reference to #1 can reference xs.

    // 0. If all alternatives are the same expression that doesn't depend on any bound pattern variables.
    if (is_constant_case(patterns,bodies))
	return simplify(options, bodies[0], S, bound_vars, context);

    // 6. Take a specific branch if the object is a constant
    expression_ref E2;
    if (is_WHNF(object) and not is_var(object))
    {
	for(int i=0; i<L and not E2; i++)
	{
	    if (is_var(patterns[i]))
	    {
		assert(is_wildcard(patterns[i]));
		E2 = simplify(options, bodies[i], S, bound_vars, context);
	    }
	    else if (patterns[i].head() == object.head())
	    {
		// 2. Rename and bind pattern variables
                // case (x[1], x[2], ..., x[n]) of {(y[1], y[2], ..., y[n]) -> f y[1] y[2] ... y[n]}
		//   a. add substitutions y[i] -> z[i] for z[i] fresh (i.e. not overlapping x[i])
		//   b. let {z[i] = x[i]} in f y[1] y[2] ... y[n]  => let {z[i] = x[i]} in f' z[1] z[2] ... z[n]
		// In the next round of simplification,
		//   a. we simplify all the x[i], possibly replacing them with another variable, or a larger expression.  But lets assume they are left unchanged.
		//   b. we should do EITHER pre- or post- inline substitution for each variable.  Replacing the z[i] by x[i].
		// Since the x[i] are already simplified during this round, it SEEMS like we should be able to just add substitutions from y[i] -> x[i]!

		auto S2 = S;
		for(int j=0;j<patterns[i].size();j++)
		{
		    auto pat_var = patterns[i].sub()[j];
		    auto& x = pat_var.as_<var>();
		    if (not is_wildcard(pat_var))
		    {
			auto obj_var = object.sub()[j];
			assert(is_var(obj_var) and not is_wildcard(obj_var));
			assert(is_var(pat_var) and not is_wildcard(pat_var));
			S2.erase(x);
			S2.insert({x,obj_var});
		    }
		}
		E2 = simplify(options, bodies[i], S2, bound_vars, context);
	    }
	}
	if (not E2)
	    throw myexception()<<"Case object doesn't match any alternative in '"<<make_case_expression(object, patterns, bodies)<<"'";

	unbind_decls(bound_vars, decls);

	return let_expression(decls,E2);
    }


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
		expression_ref& Evar = pattern2->sub[j];
		assert(is_var(Evar));

		// Create an unused variable for wildcards.  This is for if we add a x=pattern binding.
		if (is_wildcard(Evar)) {
		    var wild("__",1);
		    wild.code_dup = amount_t::None;
		    wild.work_dup = amount_t::None;
		    wild.is_loop_breaker = false;
		    wild.context = var_context::unknown;
		    Evar = wild;
		}

		var x2 = rename_and_bind_var(Evar, S2, bound_vars);
		Evar = x2;
		pat_decls.push_back({x2, {}});
	    }
	    patterns[i] = pattern2;
	}

	// 3. If we know something extra about the value (or range, theoretically) of the object in this case branch, then record that.
	bound_variable_info original_binding;
	if (is_var(object)) original_binding = rebind_var(bound_vars, object.as_<var>(), patterns[i]);

	// 4. Simplify the alternative body
	bodies[i] = simplify(options, bodies[i], S2, bound_vars, unknown_context());

	// 5. Restore informatation about an object variable to information outside this case branch.
	if (is_var(object)) rebind_var(bound_vars, object.as_<var>(), original_binding.first);

	unbind_decls(bound_vars, pat_decls);
    }

    // 7. If the case is an identity transformation
    // Hmmm... this might not be right, because leaving out the default could cause a match failure, which this transformation would eliminate.
    if (is_identity_case(object, patterns, bodies))
	E2 = object;
    // 8. case-of-case
    else if (is_case(object) and options.case_of_case)
    {
	expression_ref object2;
	vector<expression_ref> patterns2;
	vector<expression_ref> bodies2;
	parse_case_expression(object, object2, patterns2, bodies2);

        // 1. Find names for the lifted case bodies.
	vector<var> lifted_names = get_body_function_names(bound_vars, patterns, patterns2);

	// 2. Lift case bodies into let-bound functions, and replace the bodies with calls to these functions.
	CDecls cc_decls;
	for(int i=0;i<patterns.size();i++)
	{
	    // Don't both factoring out trivial expressions
	    if (is_trivial(patterns[i])) continue;

	    vector<var> used_vars = get_used_vars(patterns[i]);

	    auto f = lifted_names[i];
	    expression_ref f_body = bodies[i];
	    expression_ref f_call = f;
	    for(int j=0;j<used_vars.size();j++)
	    {
		f_call = {f_call,used_vars[j]};
		f_body = lambda_quantify(used_vars[used_vars.size()-1-j],f_body);
	    }

	    cc_decls.push_back({f,f_body});

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

	E2 = let_expression(cc_decls, make_case_expression(object2,patterns2,bodies2));
    }

    if (not E2) E2 = make_case_expression(object, patterns, bodies);

    unbind_decls(bound_vars, decls);

    return let_expression(decls, E2);
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
      
// @ E x1 .. xn.  The E and the x[i] have already been simplified.
expression_ref rebuild_apply(const simplifier_options& options, expression_ref E, const substitution& /*S*/, in_scope_set& /*bound_vars*/, inline_context /*context*/)
{
    expression_ref object = E.sub()[0];

    // 1. Optionally float let's out of the apply object
    vector<CDecls> decls;
    if (options.let_float_from_apply)
	decls = strip_multi_let(object);

    // 2. Determine how many arguments we can apply
    int applied_arguments = E.size() - 1;
    int lambda_arguments = get_n_lambdas1(object);
    int used_arguments = std::min(applied_arguments, lambda_arguments);
    if (not options.beta_reduction) applied_arguments = 0;

    // 3. For each applied argument, peel the lambda and add {var=argument} to the decls
    CDecls apply_decls;
    for(int i=0;i<applied_arguments;i++)
    {
	auto argument = E.sub()[1+i];
	if (i<used_arguments)
	{
	    auto x = object.sub()[0].as_<var>();
	    apply_decls.push_back({x, argument});
	    object = peel_n_lambdas1(object,1);
	}
	else
	    object = {object, argument};
    }
    object = let_expression(apply_decls, object);

    // 5. Rebuild the application with floated-lets and let-bound arguments outside any remaining applications.
    return let_expression(decls, object);
}

// let {x[i] = E[i]} in body.  The x[i] have been renamed and the E[i] have been simplified, but body has not yet been handled.
expression_ref rebuild_let(const simplifier_options& options, const CDecls& decls, expression_ref E, const substitution& S, in_scope_set& bound_vars, const inline_context& context)
{
    // If the decl is empty, then we don't have to do anythign special here.
    bind_decls(bound_vars, decls);

    E = simplify(options, E, S, bound_vars, context);

    unbind_decls(bound_vars, decls);

    return let_expression(decls, E);
}

// FIXME - Until we can know that decls are non-recursive, we can't simplify an Decls into more than one Decls - we have to merge them.

substitution
simplify_decls(const simplifier_options& options, CDecls& orig_decls, const substitution& S, in_scope_set& bound_vars, bool is_top_level)
{
    auto S2 = S;

    const int n_decls = orig_decls.size();

    CDecls new_decls;
    vector<var> new_names;

    // 5.1 Rename and bind all variables.
    //     Binding all variables ensures that we avoid shadowing them, which helps with let-floating.
    //     Renaming them is necessary to correctly simplify the bodies.
    for(int i=0;i<n_decls;i++)
    {
	var x = orig_decls[i].first;
	var x2 = rename_and_bind_var(x, S2, bound_vars);
	new_names.push_back(x2);
    }

    // 5.2 Iterate over decls, renaming and binding vars as we go, and simplifying them and adding substitutions for unconditional inlines.
    for(int i=0;i<n_decls;i++)
    {
	// If x[i] is not a loop breaker, then x[i] can only BE referenced by LATER E[k] (since loop breakers are later), while
	//                                     E[i] can only reference EARLIER x[k] and loop breakers.
	var x  = orig_decls[i].first;
	auto F   = orig_decls[i].second;

	var x2 = new_names[i];

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
	    /* See the paper on inlining for why we should NOT consider if F is trivial until after we have
                 simplified F.

	       One case that seems kind of problematic is let {x=3,y=4,q=let {z=x,y=w} in (z,w)} in ...
               If we simplify the bodies of z and w then we get let {x=3,y=4,q=let {z=3,w=4} in (z,w)} in ...
                 thus (a) duplicating the bodies of x and y, and also making q not look like a constructor.
               This is kind of problematic since we will never be able to resolve `case q of (a,b) -> a`
                 unless we can expose that q is in fact a pair tuple.

               However, consider that we might have decided to unconditionally pre-inline x.  In that case,
                 replacing z with x is bad, since we would then replace z with 3 and get (3,w), which is illegal.
               This the warning about not replacing z with x until after we simplify F (below) applies to this case.

               In practice, it seems that we get around this problem by floating the let {z=w,y=w} up since (z,w)
                 is a constructor.  However, if we could somehow avoid substituting x unconditionally in these cases,
                 then we could indeed replace z with x.  Maybe we could avoid marking x as OnceSafe if its only occurrence
                 is in the body of a let?  But then we would have to make sure that we never CREATE the situation where x
                 is the body of a let.  So, maybe we just hope that floating lets out of less is sufficient for now.
	    */

	    // 5.1.2 Simplify F.
	    F = simplify(options, F, S2, bound_vars, unknown_context());

	    // Should we also float lambdas in addition to constructors?  We could apply them if so...

	    // Float lets out of decl x = F
	    if (options.let_float_from_let and (is_constructor_exp(multi_let_body(F)) or is_lambda_exp(multi_let_body(F)) or is_top_level))
		for(auto& decls: strip_multi_let(F))
		    for(auto& decl: decls)
		    {
			bind_var(bound_vars, decl.first, decl.second);
			new_names.push_back(decl.first);
			new_decls.push_back(decl);
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

// Eta-reduction : if f does not reference x2 then
//                     \x2 ->               ($) f x2  ===>              f
//                 We don't do this (could perform more allocation):
//                     \x2 -> (let decls in ($) f x2) ===> let decls in f
expression_ref eta_reduce(const expression_ref& E)
{
    assert(is_lambda_exp(E));
    auto& x    = E.sub()[0].as_<var>();
    auto& body = E.sub()[1];

    if (x.code_dup == amount_t::Once and
	body.is_expression() and is_apply_exp(body) and
	(body.as_expression().sub.back() == x))
    {
	// ($) f x  ==> f
	if (body.size() == 2)
	    return body.sub()[0];
	// ($) f y x ==> ($) f y
	else
	{
	    assert(body.size() > 2);
	    object_ptr<expression> body2 = body.as_expression().clone();
	    body2->sub.pop_back();
	    return body2;
	}
    }
    else
	return E;
}

// Q1. Where do we handle beta-reduction (@ constant x1 x2 ... xn)?
// Q2. Where do we handle case-of-constant (case constant of alts)?
// Q3. How do we handle local let-floating from (i) case objects, (ii) apply-objects, (iii) let-bound statements?
expression_ref simplify(const simplifier_options& options, const expression_ref& E, const substitution& S, in_scope_set& bound_vars, const inline_context& context)
{
    assert(E);

    // 1. Var (x)
    if (is_var(E))
    {
	var x = E.as_<var>();
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
    if (is_lambda_exp(E))
    {
	assert(E.size() == 2);

	auto S2 = S;

	auto Evar = E.sub()[0];
	// 2.1. Get the new name, possibly adding a substitution
	var x2 = rename_and_bind_var(Evar, S2, bound_vars);

	// 2.3 Simplify the body with x added to the bound set.
	auto new_body = simplify(options, E.sub()[1], S2, bound_vars, unknown_context());

	// 2.4 Remove x2 from the bound set.
	unbind_var(bound_vars,x2);

	// 2.5 Return (\x2 -> new_body) after eta-reduction
	return eta_reduce(lambda_quantify(x2, new_body));
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
    if (is_apply_exp(E))
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
    if (is_constructor_exp(E) or is_non_apply_op_exp(E))
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
	auto body  = let_body(E);
	auto decls = let_decls(E);

	auto S2 = simplify_decls(options, decls, S, bound_vars, false);

        // 5.2 Simplify the let-body
	return rebuild_let(options, decls, body, S2, bound_vars, context);
    }

    std::abort();
}


vector<CDecls> simplify_module(const simplifier_options& options, const map<var,expression_ref>& small_decls_in,const set<var>& small_decls_in_free_vars,
			       const vector<CDecls>& decl_groups_in)
{
    set<var> free_vars;

    // Decompose the decls, remove unused decls, and occurrence-analyze the decls.
    auto decl_groups = occurrence_analyze_decl_groups(decl_groups_in, free_vars);

    in_scope_set bound_vars;

    for(auto& decl: small_decls_in)
    {
	var x = decl.first;
	x.work_dup = amount_t::Many;
	x.code_dup = amount_t::Many;
	bound_vars.insert({x,{decl.second,x}});
    }

    for(auto& x: small_decls_in_free_vars)
	bound_vars.insert({x,{}});

    for(auto& x: free_vars)
	bound_vars.insert({x,{}});

    vector<substitution> S(1);
    for(auto& decls: decl_groups)
    {
	auto s = simplify_decls(options, decls, S.back(), bound_vars, true);
	S.push_back( s );
	bind_decls(bound_vars, decls);
    }

    return decl_groups;
}


