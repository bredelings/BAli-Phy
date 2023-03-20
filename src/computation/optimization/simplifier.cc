#include <iostream>
#include "util/range.H" // for reverse( )
#include <unordered_map>
#include "computation/operations.H"
#include "computation/loader.H"
#include "computation/expression/expression.H" // is_reglike( ) and is_WHNF( )
#include "computation/expression/let.H"
#include "computation/expression/case.H"
#include "computation/expression/var.H"
#include "computation/expression/apply.H"
#include "computation/expression/lambda.H"
#include "computation/expression/trim.H"
#include "computation/expression/indexify.H"
#include "computation/expression/constructor.H"
#include "occurrence.H"
#include "computation/varinfo.H"
#include "computation/module.H"
#include "computation/haskell/ids.H"

#include "simplifier.H"
#include "inliner.H"
#include "simplifier_state.H"

#include "util/assert.hh"

using namespace simplifier;

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
using std::tuple;
using std::optional;

using std::cerr;
using std::endl;

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

typedef pair<expression_ref,occurrence_info> bound_variable_info;

// This should be "variables and literals", but we don't currently allow literals as function arguments.
// So, we only allow variables to be "trivial".
bool is_trivial(const expression_ref& E)
{
    return is_reglike(E);
}

void bind_var(in_scope_set& bound_vars, const var& x, const expression_ref& E)
{
    assert(x.index >= 0);
    assert(not is_wildcard(E));
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
    for(const auto& [x,rhs]: decls)
	bind_var(bound_vars, x, rhs);
}

void bind_decls(in_scope_set& bound_vars, const vector<CDecls>& decl_groups)
{
    for(auto& decls: decl_groups)
	bind_decls(bound_vars, decls);
}

void unbind_decls(in_scope_set& bound_vars, const CDecls& decls)
{
    for(const auto& [x, _]: decls)
	unbind_var(bound_vars, x);
}

void unbind_decls(in_scope_set& bound_vars, const vector<CDecls>& decl_groups)
{
    for(auto& decls: reverse(decl_groups))
	unbind_decls(bound_vars, decls);
}

set<string> special_prelude_symbols =
{
    // This is kind of a secret one, used in the desugaring of strings.
    "Foreign.String.unpack_cpp_string",

    // These are all Prelude symbols used in desugaring.
    // Modid.name should be equivalent to Prelude.name.
    "Compiler.Error.error",
    "Data.OldList.concatMap",
    "Control.Monad.fail",
    "Compiler.Enum.enumFrom",
    "Compiler.Enum.enumFromTo",
    "Compiler.Enum.enumFromThen",
    "Compiler.Enum.enumFromThenTo",
    "Compiler.Enum.enumFrom",
    "Prelude.undefined"
};

bool special_prelude_symbol(const string& name)
{
    return special_prelude_symbols.count(name) > 0;
}

// Do we have to explicitly skip loop breakers here?
expression_ref SimplifierState::consider_inline(const expression_ref& E, in_scope_set& bound_vars, const inline_context& context)
{
    var x = E.as_<var>();

    if (is_local_symbol(x.name, this_mod.name))
    {
        const auto& [rhs,occ_info] = bound_vars.at(x);

//    std::cerr<<"Considering inlining "<<E.print()<<" -> "<<binding.first<<" in context "<<context.data<<std::endl;

        // 1. If there's a binding x = E, and E = y for some variable y
        if (rhs and do_inline(rhs, occ_info, context))
            return simplify(rhs, {}, bound_vars, context);
        else
            return rebuild(E, bound_vars, context);
    }

    assert(not x.name.empty());

    // FIXME -- why is x.info->var_info empty?  Why can't we just use x.info->var_info->unfolding?
    expression_ref unfolding;

    if (is_haskell_builtin_con_name(x.name))
    {
        auto S = this_mod.lookup_builtin_symbol(x.name);
        assert(S);
        unfolding = S->var_info->unfolding;
    }
    else
    {
        assert(is_qualified_symbol(x.name) and get_module_name(x.name) != this_mod.name);

        if (auto S = this_mod.lookup_external_symbol(x.name))
            unfolding = S->var_info->unfolding;
        else if (not special_prelude_symbol(x.name))
            throw myexception()<<"Symbol '"<<x.name<<"' not transitively included in module '"<<this_mod.name<<"'";
    }

    occurrence_info occ_info;
    occ_info.work_dup = amount_t::Many;
    occ_info.code_dup = amount_t::Many;

    // FIXME -- pass var_info to do_inline( ).
    auto info = x.info.lock();
    if (unfolding and do_inline(unfolding, occ_info, context))
        return simplify(unfolding, {}, bound_vars, context);
    else if (info and info->always_unfold and (not context.is_stop_context() or is_trivial(info->unfolding)))
        return simplify(info->unfolding, {}, bound_vars, context);
    else
        return rebuild(x, bound_vars, context);
}

var SimplifierState::get_new_name(var x, const in_scope_set& bound_vars)
{
    if (bound_vars.count(x))
    {
        x = get_fresh_var_copy(x);

        assert(not bound_vars.count(x));
    }

    return x;
}

var SimplifierState::rename_var(const expression_ref& Evar, substitution& S, const in_scope_set& bound_vars)
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

    if (x.is_exported) assert(x == x2);

    return x2;
}

var SimplifierState::rename_and_bind_var(const expression_ref& Evar, substitution& S, in_scope_set& bound_vars)
{
    var x2 = rename_var(Evar, S, bound_vars);

    bind_var(bound_vars, x2, {});

    return x2;
}

bool is_identity_case(const expression_ref& object, const Core::Alts& alts)
{
    for(auto& [pattern, body]: alts)
    {
	if (is_wildcard(pattern))
	{
	    if (body != object) return false;
	}
	else if (is_var(pattern.head()))
	{
	    assert(not pattern.size());
	    if (body != pattern and body != object) return false;
	}
	else if (pattern != body) return false;
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


optional<tuple<expression_ref,substitution>>
find_constant_case_body(const expression_ref& object, const Core::Alts& alts, const substitution& S)
{
    for(auto& [pattern, body]: alts)
    {
        if (is_var(pattern))
        {
            assert(is_wildcard(pattern));
            return {{body, S}};
        }
        else if (pattern.head() == object.head())
        {
            assert(pattern.size() == object.size());

            // 2. Rename and bind pattern variables
            // case (x[1], x[2], ..., x[n]) of {(y[1], y[2], ..., y[n]) -> f y[1] y[2] ... y[n]}
            //   a. add substitutions y[i] -> z[i] for z[i] fresh (i.e. not overlapping x[i])
            //   b. let {z[i] = x[i]} in f y[1] y[2] ... y[n]  => let {z[i] = x[i]} in f' z[1] z[2] ... z[n]
            // In the next round of simplification,
            //   a. we simplify all the x[i], possibly replacing them with another variable, or a larger expression.  But lets assume they are left unchanged.
            //   b. we should do EITHER pre- or post- inline substitution for each variable.  Replacing the z[i] by x[i].
            // Since the x[i] are already simplified during this round, it SEEMS like we should be able to just add substitutions from y[i] -> x[i]!

            auto S2 = S;       
            for(int j=0;j<pattern.size();j++)
            {
                auto pat_var = pattern.sub()[j];
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
            return {{body, S2}};
        }
    }

    return {};
}

expression_ref case_of_case(const expression_ref& object, Core::Alts alts, FreshVarSource& fresh_vars)
{
    auto C = parse_case_expression(object);
    auto [object2, alts2] = *C;

    // 1. Lift case bodies into let-bound functions, and replace the bodies with calls to these functions.
    //
    // case object2 of C x y -> E   => let f x y = E in case object2 of C x y -> f x y
    //
    CDecls cc_decls;
    for(auto& [pattern, body]: alts)
    {
        // Don't both factoring out trivial expressions
        if (is_trivial(pattern)) continue;

        vector<var> used_vars = get_used_vars(pattern);

        auto f = fresh_vars.get_fresh_var("_cc");
        f.work_dup = amount_t::Many;
        f.code_dup = amount_t::Many;

        // f = \x y .. -> body
        cc_decls.push_back({f, lambda_quantify(used_vars, body)});

        // body = f x y ...
        body = ::apply(f, used_vars);
    }

    // 2. The actual case-of-case transformation.
    //
    //      case (case object2 of patterns2 -> bodies2) of patterns => bodies
    //                         to
    //      case object2 of patterns2 -> case bodies2 of patterns => bodies
    //
    for(auto& [pattern2, body2]: alts2)
        body2 = make_case_expression(body2,alts);

    // 3. Reconstruct the case expression and add lets.
    return let_expression(cc_decls, make_case_expression(object2,alts2));
}

tuple<CDecls,simplifier::substitution> SimplifierState::rename_and_bind_pattern_vars(expression_ref& pattern, const substitution& S, in_scope_set& bound_vars)
{
    auto S2 = S;
    CDecls pat_decls;

    if (pattern.size())
    {
        object_ptr<expression> pattern2 = pattern.as_expression().clone();
        for(int j=0; j<pattern2->size(); j++)
        {
            expression_ref& Evar = pattern2->sub[j];
            assert(is_var(Evar));

            // Create an unused variable for wildcards.  This is for if we add a x=pattern binding.
            if (is_wildcard(Evar))
            {
                auto wild = get_fresh_var("__");
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
        pattern = pattern2;
    }

    return {pat_decls, S2};
}

bool redundant_pattern(const Core::Alts& alts, const expression_ref& pattern)
{
    for(auto& [p,_]: alts)
    {
        if (is_var(p) and is_var(pattern))
            return true;

        if (not is_var(p) and p.head() == pattern.head())
            return true;
    }
    return false;
}


// case object of alts.  Here the object has been simplified, but the alts have not.
expression_ref SimplifierState::rebuild_case_inner(expression_ref object, Core::Alts alts, const substitution& S, in_scope_set& bound_vars)
{
    assert(not is_let_expression(object));

    //  Core is strict in the case object, so any optimizations must ensure that the object is evaluated.

    // NOTE: Any thing that relies on occurrence info for pattern vars should be done here, before
    //       we simplify alternatives, because that simplification can introduce new uses of the pattern vars.
    // Example: case #1 of {x:xs -> case #1 of {y:ys -> ys}} ==> case #1 of {x:xs -> xs} 
    //       We set #1=x:xs in the alternative, which means that a reference to #1 can reference xs.

//    // 0. If all alternatives are the same expression that doesn't depend on any bound pattern variables.
//    //    This transformation uses occurrence info.
//    if (is_constant_case(patterns,bodies))
//    {
//        // We can ignore any let bindings inside the object, since we don't depend on the object.
//	return simplify(bodies[0], S, bound_vars, context);
//    }

    // 1. Take a specific branch if the object is a constant
    if (is_WHNF(object) and not is_var(object))
    {
        if (auto found = find_constant_case_body(object, alts, S))
        {
            auto& [body, S2] = *found;
            return simplify(body, S2, bound_vars, make_ok_context());            
        }
        else
	    throw myexception()<<"Case object doesn't match any alternative in '"<<make_case_expression(object, alts)<<"'";
    }

    // 2. Simplify each alternative
    std::optional<int> last_index;
    int index = 0;
    for(auto& [pattern, body]: alts)
    {
	// 2.1. Rename and bind pattern variables
	auto [pat_decls, S2] = rename_and_bind_pattern_vars(pattern, S, bound_vars);

        // 2.2 Define x = pattern in this branch only
        auto bound_vars2 = bound_vars;
	if (is_var(object) and not is_wildcard(pattern))
        {
            auto x = object.as_<var>();
            if (is_local_symbol(x.name, this_mod.name))
                rebind_var(bound_vars2, x, pattern);
            else
            {
                assert(special_prelude_symbol(x.name) or this_mod.lookup_external_symbol(x.name));
                x.work_dup = amount_t::Many;
                x.code_dup = amount_t::Many;
                if (bound_vars2.count(x))
                    rebind_var(bound_vars2, x, pattern);
                else
                    bind_var(bound_vars2, x, pattern);
            }
        }

	// 2.3. Simplify the alternative body
	body = simplify(body, S2, bound_vars2, make_ok_context());

        // 2.4 Unbind the pattern vars.
	unbind_decls(bound_vars, pat_decls);

        if (is_var(pattern))
        {
            last_index = index;
            break;
        }

        index++;
    }
    if (last_index and *last_index + 1 < alts.size())
        alts.resize(*last_index);

    // 3. Merge case x of {...; _ -> let default_decls in case x of ...}
    vector<CDecls> default_decls;
    if (is_var(alts.back().pattern))
    {
        assert(is_wildcard(alts.back().pattern));
        auto& body = alts.back().body;

        default_decls = strip_multi_let( body );

        expression_ref object2;
        Core::Alts alts2;
        if (auto C = parse_case_expression(body))
        {
            auto& [object2, alts2] = *C;
            if (is_var(object2) and object2 == object)
            {
                alts.pop_back();
                for(auto& [pattern2,body2]: alts2)
                {
                    if (not redundant_pattern(alts, pattern2))
                        alts.push_back({pattern2, body2});
                }
            }
        }
    }

    // 4. If the case is an identity transformation: case obj of {[] -> []; (y:ys) -> (y:ys); z -> z; _ -> obj}
    // NOTE: this might not be right, because leaving out the default could cause a match failure, which this transformation would eliminate.
    // NOTE: this preserves strictness, because the object is still evaluated.
    expression_ref E2;
    if (is_identity_case(object, alts))
	E2 = object;
    // 5. case-of-case: case (case obj1 of alts1) -> alts2  => case obj of alts1*alts2
    else if (is_case(object) and options.case_of_case)
        E2 = case_of_case(object, alts, *this);
    else
        E2 = make_case_expression(object, alts);

    // 6. If we floated anything out, put it here.
    return let_expression(default_decls, E2);
}

expression_ref SimplifierState::rebuild_case(expression_ref object, const Core::Alts& alts, const substitution& S, in_scope_set& bound_vars, const inline_context& context)
{
    // These lets should already be simplified, since we are rebuilding.
    auto decls = strip_multi_let(object);

    bind_decls(bound_vars, decls);
    
    auto E2 = rebuild_case_inner(object, alts, S, bound_vars);

    unbind_decls(bound_vars, decls);

    // Instead of re-generating the let-expressions, could we pass the decls to rebuild?
    E2 = let_expression(decls, E2);

    return rebuild(E2, bound_vars, context);
}

// let {x[i] = E[i]} in body.  The x[i] have been renamed and the E[i] have been simplified, but body has not yet been handled.
expression_ref SimplifierState::rebuild_let(const CDecls& decls, expression_ref E, const substitution& S, in_scope_set& bound_vars, const inline_context& context)
{
    // If the decl is empty, then we don't have to do anything special here.
    bind_decls(bound_vars, decls);

    E = simplify(E, S, bound_vars, context);

    unbind_decls(bound_vars, decls);

    return let_expression(decls, E);
}

// FIXME - Until we can know that decls are non-recursive, we can't simplify an Decls into more than one Decls - we have to merge them.

substitution
SimplifierState::simplify_decls(CDecls& orig_decls, const substitution& S, in_scope_set& bound_vars, bool is_top_level)
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
	    F = simplify(F, S2, bound_vars, make_ok_context());

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

// NOTE: See maybe_eta_reduce( ) in occurrence.cc
//       That version depends on occurrence info.
// NOTE: GHC says that for some reason the simplifier only eta-reduces
//       when it produces a "trivial" expression  (See CoreSyn/CorePrep.hs)
//       Why?  Is it for correctness, or because it is not always faster (for GHC)?

// Eta-reduction : if f does not reference x2 then
//                     \x2 ->               ($) f x2  ===>              f
//                 We don't do this (could perform more allocation):
//                     \x2 -> (let decls in ($) f x2) ===> let decls in f
expression_ref maybe_eta_reduce2(const expression_ref& E)
{
    assert(is_lambda_exp(E));
    auto& x    = E.sub()[0].as_<var>();
    auto& body = E.sub()[1];

    // 1. Check that we have \x -> ($) ...
    if (not is_apply_exp(body)) return E;

    // 2. Check that we have \x -> ($) ... x
    if (body.as_expression().sub.back() != x) return E;

    // 3. Check that the function is a variable (and so cannot contain x)
    if (not is_var(body.sub()[0])) return E;

    // 4. Check that all entries are vars and are not x: \x -> ($) f a b c x
    for(int i=0;i<body.size()-1;i++)
    {
        assert(is_var(body.sub()[i]));

        if (body.sub()[i].as_<var>() == x) return E;
    }

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

expression_ref SimplifierState::rebuild(const expression_ref& E, in_scope_set& bound_vars, const inline_context& context)
{
    if (auto cc = context.is_case_context())
    {
        return rebuild_case(E, cc->alts, cc->subst, bound_vars, cc->next);
    }
    else if (auto ac = context.is_apply_context())
    {
        // FIXME: Should we lift let's out of E here?

        auto x = simplify(ac->arg, ac->subst, bound_vars, make_stop_context());
        return rebuild({E,x}, bound_vars, ac->next);
    }
    else
        return E;
}

// Q1. Where do we handle beta-reduction (@ constant x1 x2 ... xn)?
// Q2. Where do we handle case-of-constant (case constant of alts)?
// Q3. How do we handle local let-floating from (i) case objects, (ii) apply-objects, (iii) let-bound statements?
expression_ref SimplifierState::simplify(const expression_ref& E, const substitution& S, in_scope_set& bound_vars, const inline_context& context)
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
		return simplify(it->second.E, *(it->second.S), bound_vars, context);
	    // 1.1.2 If x -> DoneEx E, then call the simplifier on E but with no substitution.
	    else
		return simplify(it->second.E, {}, bound_vars, context);
	}
	// 1.2 If there's no substitution determine whether to inline at call site.
	else
	{
            if (is_haskell_builtin_con_name(x.name))
                ;
            else if (is_qualified_symbol(x.name) and get_module_name(x.name) != this_mod.name)
                ;
	    else if (not bound_vars.count(x))
		throw myexception()<<"Variable '"<<x.print()<<"' not bound!";

	    return consider_inline(E, bound_vars, context);
	}
    }

    // 2. Lambda (E = \x -> body)
    if (is_lambda_exp(E))
    {
	assert(E.size() == 2);

        // NOTE: This was having a problem with "\\#5 -> let {k = #5} in let {a = #4} in SModel.Nucleotides.tn93_sym a k k"
        //       That was getting changed into  "\\#5 -> SModel.Nucleotides.tn93_sym #4 #5 #5", but keeping the work_dup:Once mark on #5.

	// 2.1 eta reduction: (\x -> @ f a1 ...... an x) => (@ f a1 ....... an)
	// if (auto E2 = maybe_eta_reduce(E))
	// {
	    // Simplifying the body in (\x -> let {y=x} in f y y) can result in f x x even if x is originally only used once.
	    // Since maybe_eta_reduce( ) uses occurrence info to check that x is only used once, we have to do this *before* we simplify E (below).
            // return simplify(E2, S, bound_vars, context);
        // }

	auto Evar = E.sub()[0];
        auto Ebody = E.sub()[1];

        auto S2 = S;

        if (auto ac = context.is_apply_context())
        {
            auto x = Evar.as_<var>();
            auto arg = simplify(ac->arg, ac->subst, bound_vars, make_stop_context());
            if (x.pre_inline() and options.pre_inline_unconditionally)
            {
                S2.erase(x);
                S2.insert({x,arg});
                return simplify(Ebody, S2, bound_vars, ac->next);
            }
            else
            {
                auto x2 = rename_var(Evar, S2, bound_vars);
                return rebuild_let({{x2,arg}}, Ebody, S2, bound_vars, ac->next);
            }
        }

	// 2.2. Get the new name, possibly adding a substitution
	var x2 = rename_and_bind_var(Evar, S2, bound_vars);

	// 2.3 Simplify the body with x added to the bound set.
	auto new_body = simplify(Ebody, S2, bound_vars, make_ok_context());

	// 2.4 Remove x2 from the bound set.
	unbind_var(bound_vars,x2);

	// 2.5 Return (\x2 -> new_body) after eta-reduction
        auto E2 = lambda_quantify(x2, new_body);

        // 2.6 Maybe eta reduce
        //     I don't think there can be any substitutions that make the function body or other arguments
        //     depend on x here, so this SHOULD be safe...
        E2 = maybe_eta_reduce2( E2 );

        return rebuild(E2, bound_vars, context);
    }

    // 6. Case
    if (is_case(E))
    {
	// Simplfy the object
        auto object = E.sub()[0];

	return simplify(object, S, bound_vars, make_case_context(E, S, context));
    }

    // ?. Apply
    else if (is_apply_exp(E))
    {
        // Simplify the function
        auto f = E.sub()[0];

	return simplify(f, S, bound_vars, make_apply_context(E, S, context));
    }

    // 5. Let (let {x[i] = F[i]} in body)
    //
    // Here we know that F[i] can only mention x[j<i] unless F[i] is a loop-breaker.
    // 
    else if (is_let_expression(E))
    {
        auto L = E.as_<let_exp>();

	auto S2 = simplify_decls(L.binds, S, bound_vars, false);

        // 5.2 Simplify the let-body
	return rebuild_let(L.binds, L.body, S2, bound_vars, context);
    }

     // Do we need something to handle WHNF variables?

    // 5. (partial) Literal constant.  Treat as 0-arg constructor.
    else if (not E.size())
        return rebuild(E, bound_vars, context);

    // 4. Constructor or Operation
    else if (is_constructor_exp(E) or is_non_apply_op_exp(E))
    {
	object_ptr<expression> E2 = E.as_expression().clone();
	for(int i=0;i<E.size();i++)
	{
	    assert(is_trivial(E2->sub[i]));
	    E2->sub[i] = simplify(E2->sub[i], S, bound_vars, make_stop_context());
	}
	return rebuild(E2, bound_vars, context);
    }

    std::abort();
}

/*
 * Data.List.concatMap is used in desugaring list comprehensions,
 *   so it can be used before it is defined in Data.List.
 * (This happens in Data.Ord, which does not include Data.List).
 * This can make it part of small_decls_in_free_vars, which means that
 *  it gets added to the list of bound vars for the module Data.List.
 *
 * We haven't really solved this, but the problem currently isn't biting
 * us because the simple_size( ) function in inliner.cc apparently isn't
 * computing a small enough size for us to inline Data.Ord.compare_all
 */


vector<CDecls>
SimplifierState::simplify_module_one(const map<var,expression_ref>& small_decls_in,const set<var>& small_decls_in_free_vars,
                                     const vector<CDecls>& decl_groups_in)
{
    set<var> free_vars;

    // Decompose the decls, remove unused decls, and occurrence-analyze the decls.
    auto decl_groups = occurrence_analyze_decl_groups(this_mod, decl_groups_in, free_vars);

    for(auto& x: free_vars)
    {
        if (is_qualified_symbol(x.name) and not is_qualified_by_module(x.name, this_mod.name))
        {
            auto S = this_mod.lookup_external_symbol(x.name);
            assert(special_prelude_symbol(x.name) or S);
        }
    }

    in_scope_set bound_vars;

    vector<substitution> S(1);
    for(auto& decls: decl_groups)
    {
	auto s = simplify_decls(decls, S.back(), bound_vars, true);
	S.push_back( s );
	bind_decls(bound_vars, decls);
    }

    return decl_groups;
}


vector<CDecls> simplify_module_gently(const simplifier_options& options, FreshVarState& fresh_var_state, Module& m,
                                      const map<var,expression_ref>& small_decls_in,const set<var>& small_decls_in_free_vars,
                                      const vector<CDecls>& decl_groups_in)
{
    simplifier_options options_gentle = options;
    options_gentle.case_of_case = false;
    options_gentle.inline_threshhold = -100;
//    options_gentle.beta_reduction = false;  This breaks the inliner.  Should probably fix!

    SimplifierState state(options_gentle, fresh_var_state, m);
    return state.simplify_module_one(small_decls_in, small_decls_in_free_vars, decl_groups_in);
}

vector<CDecls> simplify_module(const simplifier_options& options, FreshVarState& fresh_var_state, Module& m,
                               const map<var,expression_ref>& small_decls_in,const set<var>& small_decls_in_free_vars,
                               const vector<CDecls>& decl_groups_in)
{
    SimplifierState state(options, fresh_var_state, m);
    auto decl_groups = decl_groups_in;

    for(int i = 0; i < options.max_iterations; i++)
    {
        decl_groups = state.simplify_module_one(small_decls_in, small_decls_in_free_vars, decl_groups);
    }

    return decl_groups;
}


SimplifierState::SimplifierState(const simplifier_options& opts, FreshVarState& state, Module& m)
    :FreshVarSource(state), options(opts), this_mod(m)
{
}
