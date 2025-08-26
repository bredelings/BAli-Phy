#include <iostream>
#include "util/range.H" // for reverse( )
#include "util/set.H" // for add( )
#include "computation/operations.H"
#include "computation/expression/convert.H" // for to_core(Occ::Exp)
#include "occurrence.H"
#include "computation/varinfo.H"
#include "computation/module.H"
#include "core/func.H"

#include "simplifier.H"

#include "util/assert.hh"

#include "range/v3/all.hpp"

namespace views = ranges::views;

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

int get_n_lambdas1(Occ::Exp E)
{
    int n = 0;
    while(auto lam = E.to_lambda())
    {
	n++;
	auto tmp = E;
	E = lam->body;
    }
    return n;
}

Occ::Exp peel_n_lambdas1(Occ::Exp E, int n)
{
     for(int i=0;i<n;i++)
     {
	 auto tmp = E;
	 E = E.to_lambda()->body;
     }
     return E;
}

[[nodiscard]] in_scope_set bind_var(const in_scope_set& bound_vars, const Occ::Var& x, const std::optional<Occ::Exp>& E)
{
    assert(x.index >= 0);
    assert(not bound_vars.count(x));
    assert(x.info.work_dup != amount_t::Unknown);
    assert(x.info.code_dup != amount_t::Unknown);
    return bound_vars.insert({x,{E,x.info}});
}

[[nodiscard]] in_scope_set rebind_var(in_scope_set bound_vars, const Occ::Var& x, const Occ::Exp& E)
{
    bound_variable_info old_binding = bound_vars.at(x);
    bound_vars = bound_vars.erase(x);
    Occ::Var x2 = x;
    x2.info = old_binding.second;
    return bind_var(bound_vars,x2,E);
}

[[nodiscard]] in_scope_set bind_decls(in_scope_set bound_vars, const Occ::Decls& decls)
{
    for(const auto& [x,rhs]: decls)
	bound_vars = bind_var(bound_vars, x, rhs);
    return bound_vars;
}

[[nodiscard]] in_scope_set bind_decls(in_scope_set bound_vars, const vector<Occ::Decls>& decl_groups)
{
    for(auto& decls: decl_groups)
	bound_vars = bind_decls(bound_vars, decls);
    return bound_vars;
}

set<Occ::Var> get_free_vars(const Occ::Pattern& pattern)
{
    if (auto cp = pattern.to_con_pat())
    {
	set<Occ::Var> vars;
	for(auto& arg: cp->args)
            vars.insert(arg);
	return vars;
    }
    else
	return {};
}

set<Occ::Var> get_free_vars(const Occ::Exp& E)
{
    // fv x = { x }
    if (auto v = E.to_var())
    {
        return { *v };
    }

    // for case expressions get_bound_indices doesn't work correctly.
    // .. we need to handle each Alt separately.
    else if (auto C = E.to_case())
    {
        auto free = get_free_vars(C->object);
 
        for(auto& [pattern, body]: C->alts)
        {
            auto free_alt = get_free_vars(body);
            for(auto& x: get_free_vars(pattern))
                free_alt.erase(x);
            add(free, free_alt);
        }

        return free;
    }
    else if (auto lam = E.to_lambda())
    {
        auto free = get_free_vars(lam->body);
        free.erase(lam->x);
        return free;
        
    }
    else if (auto let = E.to_let())
    {
        auto free = get_free_vars(let->body);
        for(auto& [x,body]: let->decls)
            add(free, get_free_vars(body));
        for(auto& [x,_]: let->decls)
            free.erase(x);
        return free;
    }
    else if (auto app = E.to_apply())
    {
        auto free = get_free_vars(app->head);
        for(auto& x: app->args)
            free.insert(x);
        return free;
    }
    else if (auto con = E.to_conApp())
    {
        set<Occ::Var> free;
        for(auto& x: con->args)
            free.insert(x);
        return free;
    }
    else if (auto builtin = E.to_builtinOp())
    {
        set<Occ::Var> free;
        for(auto& x: builtin->args)
            free.insert(x);
        return free;
    }
    else if (E.to_constant())
        return {};
    else
        std::abort();
}

// Do we have to explicitly skip loop breakers here?
Occ::Exp SimplifierState::consider_inline(const Occ::Var& x, const in_scope_set& bound_vars, const inline_context& context)
{
    if (is_local_symbol(x.name, this_mod.name))
    {
        const auto& [rhs, occ_info] = bound_vars.at(x);

//    std::cerr<<"Considering inlining "<<E.print()<<" -> "<<binding.first<<" in context "<<context.data<<std::endl;

        // 1. If there's a binding x = E, and E = y for some variable y
        if (rhs and do_inline(*rhs, occ_info, context))
            return simplify(*rhs, {}, bound_vars, context);
        else
            return rebuild(x, bound_vars, context);
    }

    assert(not x.name.empty());

    // FIXME -- why is x.info->var_info empty?  Why can't we just use x.info->var_info->unfolding?

    std::shared_ptr<VarInfo> var_info;
    if (is_haskell_builtin_con_name(x.name))
    {
        auto S = lookup_builtin_symbol(x.name);
        assert(S);
	var_info = S->var_info;
    }
    else
    {
        assert(is_qualified_symbol(x.name) and get_module_name(x.name) != this_mod.name);

        if (auto S = this_mod.lookup_external_symbol(x.name))
	    var_info = S->var_info;
        else if (not special_prelude_symbol(x.name))
            throw myexception()<<"Symbol '"<<x.name<<"' not transitively included in module '"<<this_mod.name<<"'";
    }

    optional<Occ::Exp> unfolding;
    if (var_info)
	unfolding = var_info->unfolding;

    occurrence_info occ_info;
    occ_info.work_dup = amount_t::Many;
    occ_info.code_dup = amount_t::Many;

    // FIXME -- pass var_info to do_inline( ).
    if (unfolding and do_inline(*unfolding, occ_info, context))
        return simplify(*unfolding, {}, bound_vars, context);
    else if (var_info and var_info->always_unfold and (not context.is_stop_context() or is_trivial(*unfolding)))
        return simplify(*unfolding, {}, bound_vars, context);
    else
        return rebuild(x, bound_vars, context);
}

Occ::Var SimplifierState::get_new_name(Occ::Var x, const in_scope_set& bound_vars)
{
    if (bound_vars.count(x))
    {
        x = get_fresh_var_copy(x);

        assert(not bound_vars.count(x));
    }

    return x;
}

Occ::Var SimplifierState::rename_var(const Occ::Var& x, substitution& S, const in_scope_set& bound_vars)
{
    assert(x.info.code_dup != amount_t::Unknown);
    auto x2 = get_new_name(x, bound_vars);

    // 1. If x is NOT in the bound set, then erase x from the substitution (if it's there)
    if (x == x2)
	S = S.erase(x);
    // 2. If x IS in the bound set, add a substitution from x --> x2 then erase x from the substitution (if it's there)
    else
    {
	S = S.erase(x);
	S = S.insert({x, {x2}});
    }

    if (x.is_exported) assert(x == x2);

    return x2;
}

Occ::Var SimplifierState::rename_and_bind_var(const Occ::Var& x1, substitution& S, in_scope_set& bound_vars)
{
    auto x2 = rename_var(x1, S, bound_vars);

    bound_vars = bind_var(bound_vars, x2, {});

    return x2;
}

// Convert the pattern to an expression if it contains no wildcards.
Occ::Exp pattern_to_expression(const Occ::ConPat& con_pat)
{
    return Occ::ConApp{con_pat.head, con_pat.args};
}

// Convert the pattern to an expression if it contains no wildcards.
std::optional<Occ::Exp> pattern_to_expression(const Occ::Pattern& pattern)
{
    if (pattern.is_wildcard_pat())
	return {};
    else
    {
	auto con_pat = pattern.to_con_pat();
	assert(con_pat);
	return pattern_to_expression(*con_pat);
    }
}

bool is_identity_case(const Occ::Exp& object, const Occ::Alts& alts)
{
    for(auto& [pattern, body]: alts)
    {
	// For each branch, the body must equal the object or the pattern.

	// If the object equals the body, then this branch is OK.
	if (object == body) continue;

	// If the pattern has no wildcards and equations the body, then this branch is OK.
	if (auto pattern_expression = pattern_to_expression(pattern))
	    if (*pattern_expression == body) continue;

	// Otherwise this branch is not OK.
	return false;
    }

    return true;
}

bool is_used_var(const Occ::Var& x)
{
    return (x.info.code_dup != amount_t::None);
}

bool is_used_var(const var& x)
{
    return (not x.is_wildcard() and x.code_dup != amount_t::None);
}

vector<Occ::Var> get_used_vars(const Occ::Pattern& pattern)
{
    vector<Occ::Var> used;

    if (auto con_pat = pattern.to_con_pat())
    {
	for(auto& x: con_pat->args)
	    if (is_used_var(x))
		used.push_back(x);
    }

    return used;
}

optional<tuple<Occ::Exp,substitution>>
find_constant_case_body(const Occ::Exp& object, const Occ::Alts& alts, const substitution& S)
{
    auto obj_con = object.to_conApp();

    for(auto& [pattern, body]: alts)
    {
        if (pattern.is_wildcard_pat())
        {
            return {{body, S}};
        }
        else if (auto pat_con = pattern.to_con_pat(); pat_con and obj_con and pat_con->head == obj_con->head)
        {
            int N = pat_con->args.size();
            assert(N == obj_con->args.size());

            // 2. Rename and bind pattern variables
            // case (x[1], x[2], ..., x[n]) of {(y[1], y[2], ..., y[n]) -> f y[1] y[2] ... y[n]}
            //   a. add substitutions y[i] -> z[i] for z[i] fresh (i.e. not overlapping x[i])
            //   b. let {z[i] = x[i]} in f y[1] y[2] ... y[n]  => let {z[i] = x[i]} in f' z[1] z[2] ... z[n]
            // In the next round of simplification,
            //   a. we simplify all the x[i], possibly replacing them with another variable, or a larger expression.  But lets assume they are left unchanged.
            //   b. we should do EITHER pre- or post- inline substitution for each variable.  Replacing the z[i] by x[i].
            // Since the x[i] are already simplified during this round, it SEEMS like we should be able to just add substitutions from y[i] -> x[i]!

            auto S2 = S;
            for(int j=0;j<N;j++)
            {
                auto x = pat_con->args[j];
                auto obj_var = obj_con->args[j];
                S2 = S2.erase(x);
                S2 = S2.insert({x, {obj_var}});
            }
            return {{body, S2}};
        }
    }

    return {};
}

Occ::Exp case_of_case(const Occ::Case& object, Occ::Alts alts, FreshVarSource& fresh_vars)
{
    auto& object2 = object.object;
    auto alts2 = object.alts;

    // 1. Lift case bodies into let-bound functions, and replace the bodies with calls to these functions.
    //
    // case object2 of C x y -> E   => let f x y = E in case object2 of C x y -> f x y
    //
    Occ::Decls cc_decls;
    for(auto& [pattern, body]: alts)
    {
        // Don't both factoring out trivial expressions
        if (pattern.is_irrefutable()) continue;

        vector<Occ::Var> used_vars2 = get_used_vars(pattern);

        auto f = fresh_vars.get_fresh_occ_var("_cc");
        f.info.work_dup = amount_t::Many;
        f.info.code_dup = amount_t::Many;

        // f = \x y .. -> body
        cc_decls.push_back({f, lambda_quantify(used_vars2, body)});

        // body = f x y ...
        body = make_apply(Occ::Exp(f), used_vars2);
    }
    
    // 2. The actual case-of-case transformation.
    //
    //      case (case object2 of patterns2 -> bodies2) of patterns => bodies
    //                         to
    //      case object2 of patterns2 -> case bodies2 of patterns => bodies
    //
    for(auto& [pattern2, body2]: alts2)
        body2 = Occ::Case{body2,alts};

    Occ::Exp E = Occ::Case{object2,alts2};
    if (not cc_decls.empty())
	E = Occ::Let{cc_decls,E};

    // 3. Reconstruct the case expression and add lets.
    return E;
}

tuple<substitution, in_scope_set>
SimplifierState::rename_and_bind_pattern_vars(Occ::Pattern& pattern, const substitution& S, const in_scope_set& bound_vars_in)
{
    auto S2 = S;
    auto bound_vars = bound_vars_in;

    if (auto con_pat = pattern.to_con_pat())
    {
	Occ::ConPat pattern2 = *con_pat;
	for(auto& arg: pattern2.args)
            arg = rename_and_bind_var(arg, S2, bound_vars);
        pattern = pattern2;
    }

    return {S2, bound_vars};
}

// If we add pattern2 add the end of alts, would we never get to it?
bool redundant_pattern(const Occ::Alts& alts, const Occ::Pattern& pattern2)
{
    auto con_pat2 = pattern2.to_con_pat();

    for(auto& [pattern1,_]: alts)
    {
        // If any pattern in alts is irrefutable, we could never get past the end.
        if (pattern1.is_irrefutable())
            return true;
        // If any pattern in alts is is a ConPat with the same head, then we would never get to pattern2
        else
        {
            auto con_pat1 = pattern1.to_con_pat();
            assert(con_pat1);
            if (con_pat2 and con_pat1->head == con_pat2->head)
                return true;
        }
    }
    return false;
}

Occ::Exp multi_let_body(Occ::Exp E)
 {
    while(auto let = E.to_let())
    {
	auto tmp = E;
	E = let->body;
    }
    return E;
}
 
std::vector<Occ::Decls> strip_multi_let(Occ::Exp& E)
{
    std::vector<Occ::Decls> decl_groups;
    while(auto let = E.to_let())
    {
	decl_groups.push_back(let->decls);
	auto tmp = E;
	E = let->body;
    }
    return decl_groups;
}

// case object of alts.  Here the object has been simplified, but the alts have not.
Occ::Exp SimplifierState::rebuild_case_inner(Occ::Exp object, Occ::Alts alts, const substitution& S, const in_scope_set& bound_vars)
{
    assert(not object.to_let());

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
    if (is_WHNF(object) and not object.to_var())
    {
        if (auto found = find_constant_case_body(object, alts, S))
        {
            auto& [body, S2] = *found;
            return simplify(body, S2, bound_vars, make_ok_context()); 
        }
        else
	    throw myexception()<<"Case object doesn't match any alternative in '"<<Occ::Case{object,alts}.print()<<"'";
    }

    // 2. Simplify each alternative
    std::optional<int> last_index;
    int index = 0;
    optional<string> object_type;
    set<string> seen_constructors;
    set<string> unseen_constructors;
    for(auto& [pattern, body]: alts)
    {
	// 2.1. Rename and bind pattern variables
	auto [S2, bound_vars2] = rename_and_bind_pattern_vars(pattern, S, bound_vars);

        auto con_pat = pattern.to_con_pat();

        if (con_pat)
        {
	    // Get type and its constructors
            if (auto C = this_mod.lookup_resolved_symbol(con_pat->head))
            {
                string pattern_type = *C->parent;
                if (object_type)
                    assert(*object_type == pattern_type);
                else
                {
                    object_type = pattern_type;
                    auto T = this_mod.lookup_resolved_type(*object_type);
                    assert(T);
                    auto D = T->is_data();
                    assert(D);
                    unseen_constructors = D->constructors;
                    assert(not unseen_constructors.empty());
                }

                // Remove this constructors from the total list of constructors
                if (unseen_constructors.count(con_pat->head))
                {
                    unseen_constructors.erase(con_pat->head);
                    seen_constructors.insert(con_pat->head);
                }
                else if (seen_constructors.count(con_pat->head))
                {
                    // we should ignore this branch!
                    // and this shouldn't happen.
                }
                else
                    std::abort();
            }
            else
            {
                auto T = this_mod.lookup_resolved_type(con_pat->head);
                assert(T);
                assert(T->is_class());
                assert(alts.size() == 1);
            }
        }

        // 2.2 Define x = pattern in this branch only
	if (auto v = object.to_var(); v and not pattern.is_irrefutable())
        {
	    auto pattern_expression = pattern_to_expression(pattern).value();
            auto x = *v;
            if (is_local_symbol(x.name, this_mod.name))
                bound_vars2 = rebind_var(bound_vars2, x, pattern_expression);
            else
            {
                assert(special_prelude_symbol(x.name) or this_mod.lookup_external_symbol(x.name));
                x.info.work_dup = amount_t::Many;
                x.info.code_dup = amount_t::Many;
                if (bound_vars2.count(x))
                    bound_vars2 = rebind_var(bound_vars2, x, pattern_expression);
                else
                    bound_vars2 = bind_var(bound_vars2, x, pattern_expression);
            }
        }

	// 2.3. Simplify the alternative body
	body = simplify(body, S2, bound_vars2, make_ok_context());

        if (pattern.is_irrefutable() or unseen_constructors.empty())
        {
            last_index = index;
            break;
        }

        index++;
    }
    if (last_index and *last_index + 1 < alts.size())
        alts.resize(*last_index + 1);

    // 3. If the _ branch cases on the same object, then we can lift
    //    out any cases not covered into the upper case and drop the others.
    vector<Occ::Decls> default_decls;
    if (alts.back().pat.is_wildcard_pat())
    {
        auto& body = alts.back().body;

        // We can always lift any declarations out of the case body because they can't contain any pattern variables.
        default_decls = strip_multi_let( body );

        // We could do this even if the object isn't a variable, right?
        if (auto C = body.to_case(); C and C->object.to_var() and C->object == object)
        {
            alts.pop_back();
            for(auto& [pattern2,body2]: C->alts)
            {
                if (not redundant_pattern(alts, pattern2))
                    alts.push_back({pattern2, body2});
            }
        }
    }

    // 4. If the case is an identity transformation: case obj of {[] -> []; (y:ys) -> (y:ys); z -> z; _ -> obj}
    // NOTE: this might not be right, because leaving out the default could cause a match failure, which this transformation would eliminate.
    // NOTE: this preserves strictness, because the object is still evaluated.
    Occ::Exp E2;
    if (is_identity_case(object, alts))
	E2 = object;
    // 5. case-of-case: case (case obj1 of alts1) -> alts2  => case obj of alts1*alts2
    else if (auto C = object.to_case(); C and options.case_of_case)
        E2 = case_of_case(*C, alts, *this);
    else
        E2 = Occ::Case{object, alts};

    // 6. If we floated anything out, put it here.
    for(auto& d: default_decls | views::reverse)
	E2 = Occ::Let{d,E2};

    return E2;
}

Occ::Exp SimplifierState::rebuild_case(Occ::Exp object, const Occ::Alts& alts, const substitution& S, const in_scope_set& bound_vars, const inline_context& context)
{
    // These lets should already be simplified, since we are rebuilding.
    auto decls = strip_multi_let(object);

    auto bound_vars2 = bind_decls(bound_vars, decls);
    
    auto E2 = rebuild_case_inner(object, alts, S, bound_vars2);

    // Instead of re-generating the let-expressions, could we pass the decls to rebuild?
    for(auto& d: decls | views::reverse)
	E2 = Occ::Let{d, E2};

    return rebuild(E2, bound_vars, context);
}

Occ::Exp make_sink_apply(const Occ::Exp& E, const Occ::Var& x)
{
    if (auto C = E.to_case())
    {
        auto alts2 = C->alts;
        for(auto& [pat,body]: alts2)
            body = make_sink_apply(body,x);
        return Occ::Case{C->object, alts2};
    }
    else
        return make_apply(E, x);
}


Occ::Exp SimplifierState::rebuild_apply(Occ::Exp E, const Occ::Var& arg, const substitution& S, const in_scope_set& bound_vars, const inline_context& context)
{
    // These lets should already be simplified, since we are rebuilding.
    auto decls = strip_multi_let(E);

    auto bound_vars2 = bind_decls(bound_vars, decls);

    auto x = *simplify(arg, S, bound_vars2, make_stop_context()).to_var();

    auto E2 = make_sink_apply(E, x);

    // Instead of re-generating the let-expressions, could we pass the decls to rebuild?
    for(auto& d: decls | views::reverse)
        E2 = Occ::Let{d, E2};

    return rebuild(E2, bound_vars, context);
}

// let {x[i] = E[i]} in body.  The x[i] have been renamed and the E[i] have been simplified, but body has not yet been handled.
Occ::Exp SimplifierState::rebuild_let(const Occ::Decls& decls, Occ::Exp E, const substitution& S, const in_scope_set& bound_vars, const inline_context& context)
{
    // If the decl is empty, then we don't have to do anything special here.
    auto bound_vars2 = bind_decls(bound_vars, decls);

    auto E2 = simplify(E, S, bound_vars2, context);

    return make_let(decls, E2);
}

// FIXME - Until we can know that decls are non-recursive, we can't simplify an Decls into more than one Decls - we have to merge them.

// FIXME - Cache free vars on expressions!

tuple<Occ::Decls, substitution>
SimplifierState::simplify_decls(const Occ::Decls& orig_decls, const substitution& S, in_scope_set bound_vars, bool is_top_level)
{
    auto S2 = S;

    const int n_decls = orig_decls.size();

    Occ::Decls new_decls;
    vector<Occ::Var> new_names;

    // 5.1 Rename and bind all variables.
    //     Binding all variables ensures that we avoid shadowing them, which helps with let-floating.
    //     Renaming them is necessary to correctly simplify the bodies.
    for(int i=0;i<n_decls;i++)
    {
	auto& x = orig_decls[i].x;
	auto x2 = rename_and_bind_var(x, S2, bound_vars);
	new_names.push_back(x2);
    }

    // 5.2 Iterate over decls, renaming and binding vars as we go, and simplifying them and adding substitutions for unconditional inlines.
    for(int i=0;i<n_decls;i++)
    {
	// If x[i] is not a loop breaker, then x[i] can only BE referenced by LATER E[k] (since loop breakers are later), while
	//                                     E[i] can only reference EARLIER x[k] and loop breakers.
	auto x  = orig_decls[i].x;
	auto F  = orig_decls[i].body;

	auto x2 = new_names[i];

	if (x.is_exported) assert(x == x2);

	// 1. Any references to x in F must be to the x bound in this scope.
	// 2. F can only contain references to x if x is a loop breaker.
	// 3. If x is a loop breaker, then S2 already contains substitutions for x -> x2 if needed.
	// 4. Therefore S2 is a good substitution for F.
	assert(x.info.is_loop_breaker or not get_free_vars(F).count(x));

	// A. Suspended substitutions created by pre-inlining won't be affected if we include unconditionally inlining later-occurring variables.
	//   A.1 This is because substitutions for later-occuring variables that are loop-breakers has already been done.
	//   A.2 Non-loop cannot occur in the bodies F that the suspended substitutions will be applied to.
	// B. Therefore, we can create a single substitution object for an entire decl scope, and just include pointers to it.
	// C. The lifetime of the substitution is just the duration of this scope, so raw pointers are fine.
	if (x.info.pre_inline() and options.pre_inline_unconditionally and not x.is_exported)
	{
	    S2 = S2.erase(x);
	    S2 = S2.insert({x,{F,S2}});
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
	    if (options.let_float_from_let and (multi_let_body(F).to_conApp() or multi_let_body(F).to_lambda() or is_top_level))
		for(auto& decls: strip_multi_let(F))
		    for(auto& decl: decls)
		    {
			bound_vars = bind_var(bound_vars, decl.x, decl.body);
			new_names.push_back(decl.x);
			new_decls.push_back(decl);
		    }

	    // what are the conditions for post-inlining unconditionally?
	    if (is_trivial(F) and options.post_inline_unconditionally and not x.is_exported and not x.info.is_loop_breaker)
	    {
		S2 = S2.erase(x);
		S2 = S2.insert({x,F});
	    }
	    else
	    {
		new_decls.push_back({x2,F});

		// Any later occurrences will see the bound value of x[i] when they are simplified.
		bound_vars = rebind_var(bound_vars, x2, F);
	    }
	}
    }

    return {new_decls, S2};
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
Occ::Exp maybe_eta_reduce2(const Occ::Lambda& L)
{
    // 1. Check that we have \x -> ($) ...
    auto app = L.body.to_apply();
    if (not app) return L;

    // 2. Check that we have \x -> ($) ... x
    if (app->args.back() != L.x) return L;

    // 3. Check that the function is a variable different from x (and so cannot contain x)
    if (not app->head.to_var() or app->head == L.x) return L;

    // 4. Check that all entries are vars and are not x: \x -> ($) f a b c x
    for(int i=0;i<app->args.size()-1;i++)
        if (app->args[i] == L.x) return L;

    // ($) f x  ==> f
    if (app->args.size() == 1)
        return app->head;
    // ($) f y x ==> ($) f y
    else
    {
        auto app2 = *app;
        app2.args.pop_back();
        return app2;
    }
}

Occ::Exp SimplifierState::rebuild(const Occ::Exp& E, const in_scope_set& bound_vars, const inline_context& context)
{
    if (auto cc = context.is_case_context())
    {
        return rebuild_case(E, cc->alts, cc->subst, bound_vars, cc->next);
    }
    else if (auto ac = context.is_apply_context())
    {
        return rebuild_apply(E, ac->arg, ac->subst, bound_vars, ac->next);
    }
    else
        return E;
}

// Q1. Where do we handle beta-reduction (@ constant x1 x2 ... xn)?
// Q2. Where do we handle case-of-constant (case constant of alts)?
// Q3. How do we handle local let-floating from (i) case objects, (ii) apply-objects, (iii) let-bound statements?
Occ::Exp SimplifierState::simplify(const Occ::Exp& E, const substitution& S, const in_scope_set& bound_vars, const inline_context& context)
{
    assert(not E.empty());

    // 1. Var (x)
    if (auto x = E.to_var())
    {
	// 1.1 If there's a substitution x -> E
	if (S.count(*x))
	{
	    auto it = S.find(*x);
	    // 1.1.1 If x -> SuspEx E S, then call the simplifier on E with its substitution S
	    if (it->S)
		return simplify(it->E, *(it->S), bound_vars, context);
	    // 1.1.2 If x -> DoneEx E, then call the simplifier on E but with no substitution.
	    else
		return simplify(it->E, {}, bound_vars, context);
	}
	// 1.2 If there's no substitution determine whether to inline at call site.
	else
	{
            if (is_haskell_builtin_con_name(x->name))
                ; // OK
            else if (is_qualified_symbol(x->name) and get_module_name(x->name) != this_mod.name)
                ; // OK
	    else if (not bound_vars.count(*x))
		throw myexception()<<"Variable '"<<x->print()<<"' not bound!";

	    return consider_inline(*x, bound_vars, context);
	}
    }

    // 2. Lambda (E = \x -> body)
    if (auto lam = E.to_lambda())
    {
        // NOTE: This was having a problem with "\\#5 -> let {k = #5} in let {a = #4} in SModel.Nucleotides.tn93_sym a k k"
        //       That was getting changed into  "\\#5 -> SModel.Nucleotides.tn93_sym #4 #5 #5", but keeping the work_dup:Once mark on #5.

	// 2.1 eta reduction: (\x -> @ f a1 ...... an x) => (@ f a1 ....... an)
	// if (auto E2 = maybe_eta_reduce(E))
	// {
	    // Simplifying the body in (\x -> let {y=x} in f y y) can result in f x x even if x is originally only used once.
	    // Since maybe_eta_reduce( ) uses occurrence info to check that x is only used once, we have to do this *before* we simplify E (below).
            // return simplify(E2, S, bound_vars, context);
        // }

        auto S2 = S;

        if (auto ac = context.is_apply_context())
        {
            auto x = lam->x;
            auto arg = simplify(ac->arg, ac->subst, bound_vars, make_stop_context());
            if (x.info.pre_inline() and options.pre_inline_unconditionally)
            {
                S2 = S2.erase(x);
                S2 = S2.insert({x,arg});
                return simplify(lam->body, S2, bound_vars, ac->next);
            }
            else
            {
                auto x2 = rename_var(lam->x, S2, bound_vars);
                return rebuild_let({{x2,arg}}, lam->body, S2, bound_vars, ac->next);
            }
        }

	// 2.2. Get the new name, possibly adding a substitution
        auto bound_vars_x = bound_vars;
	auto x2 = rename_and_bind_var(lam->x, S2, bound_vars_x);

	// 2.3 Simplify the body with x added to the bound set.
	auto new_body = simplify(lam->body, S2, bound_vars_x, make_ok_context());

	// 2.4 Return (\x2 -> new_body) after eta-reduction
        auto L = Occ::Lambda{x2, new_body};

        // 2.5 Maybe eta reduce
        //     I don't think there can be any substitutions that make the function body or other arguments
        //     depend on x here, so this SHOULD be safe...
        auto E2 = maybe_eta_reduce2( L );

        return rebuild(E2, bound_vars, context);
    }

    // 6. Case
    if (auto C = E.to_case())
    {
	// Simplfy the object
	return simplify(C->object, S, bound_vars, make_case_context(*C, S, context));
    }

    // ?. Apply
    else if (auto app = E.to_apply())
    {
        // Simplify the function
	return simplify(app->head, S, bound_vars, make_apply_context(*app, S, context));
    }

    // 5. Let (let {x[i] = F[i]} in body)
    //
    // Here we know that F[i] can only mention x[j<i] unless F[i] is a loop-breaker.
    // 
    else if (auto let = E.to_let())
    {
	auto decls = let->decls;
	auto [decls2, S2] = simplify_decls(decls, S, bound_vars, false);

        // 5.2 Simplify the let-body
	return rebuild_let(decls2, let->body, S2, bound_vars, context);
    }

     // Do we need something to handle WHNF variables?

    // 5. Literal constant.  Treat as 0-arg constructor.
    else if (E.to_constant())
        return rebuild(E, bound_vars, context);

    // 4. Constructor
    else if (auto con = E.to_conApp())
    {
	Occ::ConApp C = *con;
	for(auto& arg: C.args)
	    arg = *simplify(arg, S, bound_vars, make_stop_context()).to_var();

	return rebuild(C, bound_vars, context);
    }

    // 4. Builtin
    else if (auto builtin = E.to_builtinOp())
    {
	Occ::BuiltinOp builtin2;
	builtin2.lib_name = builtin->lib_name;
	builtin2.func_name = builtin->func_name;
	builtin2.op = builtin->op;

	for(auto& arg: builtin->args)
 	{
	    auto arg2 = *simplify(arg, S, bound_vars, make_stop_context()).to_var();
	    builtin2.args.push_back(arg2);
 	}

	return rebuild(builtin2, bound_vars, context);
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


vector<Core2::Decls<>>
SimplifierState::simplify_module_one(const vector<Core2::Decls<>>& decl_groups_in)
{
    set<Occ::Var> free_vars;

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
	auto [decls2, s] = simplify_decls(decls, S.back(), bound_vars, true);
        decls = decls2;
	S.push_back( s );
	bound_vars = bind_decls(bound_vars, decls);
    }

    vector<Core2::Decls<>> decl_groups_out;
    for(auto& decls: decl_groups)
        decl_groups_out.push_back(to_core(decls));

    return decl_groups_out;
}


vector<Core2::Decls<>> simplify_module_gently(const simplifier_options& options, FreshVarState& fresh_var_state, Module& m,
                                              const vector<Core2::Decls<>>& decl_groups_in)
{
    simplifier_options options_gentle = options;
    options_gentle.case_of_case = false;
    options_gentle.inline_threshhold = -100;
//    options_gentle.beta_reduction = false;  This breaks the inliner.  Should probably fix!

    SimplifierState state(options_gentle, fresh_var_state, m);
    return state.simplify_module_one(decl_groups_in);
}

vector<Core2::Decls<>> simplify_module(const simplifier_options& options, FreshVarState& fresh_var_state, Module& m,
                               const vector<Core2::Decls<>>& decl_groups_in)
{
    SimplifierState state(options, fresh_var_state, m);
    auto decl_groups = decl_groups_in;

    for(int i = 0; i < options.max_iterations; i++)
    {
        decl_groups = state.simplify_module_one(decl_groups);
    }

    return decl_groups;
}


SimplifierState::SimplifierState(const simplifier_options& opts, FreshVarState& state, Module& m)
    :FreshVarSource(state), options(opts), this_mod(m)
{
}
