#include <iostream>
#include "util/range.H" // for reverse( )
#include "util/set.H" // for add( )
#include "util/variant.H" // for to<type>(val)
#include "computation/operations.H"
#include "computation/expression/convert.H" // for to_core(Occ::Exp)
#include "occurrence.H"
#include "computation/module.H"
#include "core/func.H"
#include "core/subst.H"

#include "simplifier.H"

#include "util/assert.hh"

#include "range/v3/all.hpp"

namespace views = ranges::views;

using namespace simplifier;

/* TODO:
   1. Collect surrounding floats without re-applying them.
   2. Pass the continuation to rebuild_case( ) and just return its result.
 */

/*
  rebuild :: returns (Floats, Expr)
  rebuild_case :: returns (Floats, Expr)
  
 */

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

[[nodiscard]] in_scope_set bind_var(const in_scope_set& bound_vars, const Occ::Var& x, const Unfolding& unfolding)
{
    assert(x.index >= 0);
    assert(not bound_vars.count(x));
    assert(x.info.work_dup != amount_t::Unknown);
    assert(x.info.code_dup != amount_t::Unknown);

    return bound_vars.insert({x,{unfolding,x.info}});
}

[[nodiscard]] in_scope_set rebind_var(in_scope_set bound_vars, const Occ::Var& x, const Unfolding& U)
{
    bound_variable_info old_binding = bound_vars.at(x);
    bound_vars = bound_vars.erase(x);
    Occ::Var x2 = x;
    x2.info = old_binding.second;
    return bind_var(bound_vars,x2,U);
}

[[nodiscard]] in_scope_set bind_decls(const Module& m, const inliner_options& opts, in_scope_set bound_vars, const Occ::Decls& decls)
{
    for(const auto& [x,rhs]: decls)
	bound_vars = bind_var(bound_vars, x, make_core_unfolding(m, opts,rhs));
    return bound_vars;
}

[[nodiscard]] in_scope_set bind_decls(const Module& m, const inliner_options& opts, in_scope_set bound_vars, const vector<Occ::Decls>& decl_groups)
{
    for(auto& decls: decl_groups)
	bound_vars = bind_decls(m, opts, bound_vars, decls);
    return bound_vars;
}

set<Occ::Var> get_free_vars(const Occ::Pattern& pattern)
{
    set<Occ::Var> vars;
    for(auto& arg: pattern.args)
        vars.insert(arg);
    return vars;
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
        add(free, get_free_vars(app->arg));
        return free;
    }
    else if (auto con = E.to_conApp())
    {
        set<Occ::Var> free;
        for(auto& arg: con->args)
            add(free, get_free_vars(arg));
        return free;
    }
    else if (auto builtin = E.to_builtinOp())
    {
        set<Occ::Var> free;
        for(auto& arg: builtin->args)
            add(free, get_free_vars(arg));
        return free;
    }
    else if (E.to_constant())
        return {};
    else
        std::abort();
}

ConCont add_app(const Occ::Exp& arg, const ConCont& cont)
{
    return std::make_shared<ConContObj>(arg, cont);
}

bool safe_to_inline(const Occ::Var& x)
{
    if (x.info.is_loop_breaker) return false;
    if (x.info.code_dup == amount_t::None and x.info.work_dup == amount_t::None) return true;
    if (x.info.code_dup == amount_t::Once and x.info.work_dup == amount_t::Once) return true;
    return false;
}

bool do_beta_by_substitution(const Occ::Var& x, const Occ::Exp& rhs)
{
    return rhs.to_var() or safe_to_inline(x);
}

bool is_data_con_wrapper(const Occ::Exp& fun)
{
    if (fun.to_conApp()) return true;
    else if (auto L = fun.to_lambda())
        return is_data_con_wrapper(L->body);
    else
        return false;
}

int count_cont_args(const ConCont& cont)
{
    if (not cont)
        return 0;
    else
        return 1 + count_cont_args(cont->next);
}

void SimplFloats::append(const Module& m, const inliner_options& opts, const Occ::Decls& d)
{
    if (not d.empty())
    {
        bound_vars = bind_decls(m, opts, bound_vars, d);
        decls.push_back(d);
    }
}

void SimplFloats::append(const Module& m, const inliner_options& opts, const vector<Occ::Decls>& decl_groups)
{
    for(auto& decls: decl_groups)
        append(m, opts, decls);
}

void SimplFloats::append(const Module& m, const inliner_options& opts, const SimplFloats& F)
{
    append(m, opts, F.decls);
}

Occ::Exp wrap(const SimplFloats& F, Occ::Exp E)
{
    // Instead of re-generating the let-expressions, could we pass the decls to rebuild?
    for(auto& d: F.decls | views::reverse)
        E = Occ::Let{d, E};

    return E;
}

Occ::Exp wrap(const std::tuple<SimplFloats,Occ::Exp>& FE)
{
    auto& [F,E] = FE;
    return wrap(F,E);
}

// GHC has an InScopeEnv -- different from an InScopeSet -- that contains the unfolding?

std::optional<std::tuple<in_scope_set, std::string, std::vector<Occ::Exp>>>
SimplifierState::exprIsConApp_worker(const in_scope_set& S, std::vector<Float>& floats, const Occ::Exp& E, const ConCont& cont)
{
    if (auto app = E.to_apply())
    {
        return exprIsConApp_worker(S, floats, app->head, add_app( app->arg, cont));
    }
    else if (auto lam = E.to_lambda(); lam and cont and false)
    {
        // TODO: We to rename lam->x to a fresh name.
        //       But to be efficient, that requires a substitution.
        //       GHC combines the set of active variables with the substitution --> perhaps that makes it easy to mint new names?
        // auto [S2, in_scope_set2, x2] = rename_and_bind_var(x, CoreUnfolding(cont->arg));
        // Float f = FloatLet{{{x2, cont->arg}}};
        // floats.push_back(f);
        // return exprIsConApp_worker(in_scope_set2, S2, floats, lam->body, cont->next);

        auto S2 = bind_var(S, lam->x, make_core_unfolding(this_mod, options, cont->arg));
        Float f = FloatLet{{{lam->x, cont->arg}}};
        floats.push_back(f);
        return exprIsConApp_worker(S2, floats, lam->body, cont->next);
    }
    else if (auto let = E.to_let(); let and false)
    {
        Float f = FloatLet{let->decls};
        floats.push_back(f);
        auto S2 = S;
        for(auto& [x,e]: let->decls)
            S2 = bind_var(S, x, make_core_unfolding(this_mod, options,e));
            
        return exprIsConApp_worker(S2, floats, let->body, cont);
    }
    else if (auto C = E.to_case(); C and C->alts.size() == 1 and false)
    {
        auto& [pat,body] = C->alts[0];

        Float f = FloatCase{C->object, pat};
        floats.push_back(f);
        auto S2 = S;
        for(auto& x: pat.args)
            S2 = bind_var(S, x, {});

        return exprIsConApp_worker(S2, floats, body, cont);
    }
    else if (auto C = E.to_conApp())
    {
        // There had better not be arguments applied to the con app!
        assert(not cont);
        vector<Occ::Exp> args;
        for(auto& arg: C->args)
            args.push_back(arg);
        return {{S, C->head, args}};
    }
    else if (auto V = E.to_var())
    {
        // should we do something special for string literals?

        auto& x = *V;
        auto [unfolding, occ_info] = get_unfolding(x, S);

        if (auto cu = to<CoreUnfolding>(unfolding); cu and not x.info.is_loop_breaker)
        {
            // Ideally we would only do this if x has arity zero (i.e. its not a function)
            return exprIsConApp_worker(S, floats, cu->expr, cont);
        }
        else if (auto du = to<DFunUnfolding>(unfolding); du and count_cont_args(cont) == du->binders.size())
        {
            auto C = cont;
            Occ::subst_t subst;
            for(auto& x: du->binders)
            {
                subst = subst.insert({x, C->arg});
                C = C->next;
            }
            assert(not C);

            vector<Occ::Exp> args;
            for(auto& arg: du->args)
                args.push_back(Core2::subst(subst, arg));

            return {{S, du->head, args}};
        }
    }

    return {};    
}

std::optional<std::tuple<in_scope_set, std::vector<Float>, std::string, std::vector<Occ::Exp>>>
SimplifierState::exprIsConApp_maybe(const Occ::Exp& E,  const in_scope_set& bound_vars)
{
    vector<Float> floats;
    auto result = exprIsConApp_worker(bound_vars, floats, E, {});

    if (not result)
        return {};
    else
    {
        auto [S, con, args] = *result;
        return {{S, floats, con, args}};
    }
}


Occ::Exp apply_floats(const vector<Float>& floats, Occ::Exp E)
{
    for(auto& f: floats | views::reverse)
    {
        if (auto lf = to<FloatLet>(f))
            E = Occ::Let(lf->decls,E);
        else
        {
            auto cf = to<FloatCase>(f);
            E = Occ::Case{cf->object,{Occ::Alt{cf->pattern,E}}};
        }
    }
    return E;
}


// Do we have to explicitly skip loop breakers here?
tuple<SimplFloats,Occ::Exp>
SimplifierState::simplify_out_var(const Occ::Var& x, const in_scope_set& bound_vars, const inline_context& context)
{
    auto [unfolding, occ_info] = get_unfolding(x, bound_vars);

    // Try and handling a method applied to a dfun
    if (auto mu = to<MethodUnfolding>(unfolding); mu)
    {
        if (auto app = context.is_apply_context())
        {
            // Apply substitution to the argument.
            auto [_, __, arg] = simplifyArg(bound_vars, app->dup_status, app->subst, app->bound_vars, app->arg);

            if (auto constant = exprIsConApp_maybe(arg, bound_vars))
            {
                auto [bound_vars2, floats, con, args] = *constant;

                auto expr = args[mu->index];

                return simplify(apply_floats(floats, expr), {}, bound_vars, app->next);
            }
        }
    }

    if (auto e = try_inline(unfolding, occ_info, context))
        return simplify(*e, {}, bound_vars, context);
    else
        return rebuild(x, bound_vars, context);
}

Occ::Var SimplifierState::get_new_name(Occ::Var x, const in_scope_set& bound_vars)
{
    if (not x.is_exported) //bound_vars.count(x))
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
std::optional<Occ::Exp> pattern_to_expression(const Occ::Pattern& pattern)
{
    if (pattern.is_wildcard_pat())
	return {};
    else
    {
        auto args = pattern.args | ranges::to<vector<Occ::Exp>>;
        return Occ::ConApp{*pattern.head, args};
    }
}

bool is_identity_case(const Occ::Exp& object, const vector<Occ::Alt>& alts)
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

    for(auto& x: pattern.args)
        if (is_used_var(x))
            used.push_back(x);

    return used;
}

std::optional<Occ::Alt> select_case_alternative(const string& con, const vector<Occ::Alt>& alts)
{
    for(auto& alt: alts)
    {
        auto& [pattern,_] = alt;
        if (not pattern.head or *pattern.head == con)
            return alt;
    }

    return {};
}

tuple<Occ::Decls,Occ::Exp> case_of_case(const Occ::Case& object, vector<Occ::Alt> alts, FreshVarSource& fresh_vars)
{
    auto& object2 = object.object;
    auto alts2 = object.alts;

    // 1. Lift case bodies into let-bound functions, and replace the bodies with calls to these functions.
    //
    // case object2 of C x y -> E   => let f x y = E in case object2 of C x y -> f x y
    //
    Occ::Decls cc_decls;
    int join_point_index = 1;
    for(auto& [pattern, body]: alts)
    {
        // Don't both factoring out trivial expressions
        if (pattern.is_irrefutable()) continue;

        vector<Occ::Var> used_vars2 = get_used_vars(pattern);

        auto f = fresh_vars.get_fresh_occ_var("$j"+std::to_string(join_point_index++));
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

    // 3. Reconstruct the case expression and add lets.
    return {cc_decls, E};
}

tuple<substitution, in_scope_set>
SimplifierState::rename_and_bind_pattern_vars(Occ::Pattern& pattern, const substitution& S, const in_scope_set& bound_vars_in)
{
    auto S2 = S;
    auto bound_vars = bound_vars_in;

    for(auto& arg: pattern.args)
        arg = rename_and_bind_var(arg, S2, bound_vars);

    return {S2, bound_vars};
}

// If we add pattern2 add the end of alts, would we never get to it?
bool redundant_pattern(const vector<Occ::Alt>& alts, const Occ::Pattern& pattern2)
{
    for(auto& [pattern1,_]: alts)
    {
        // If any pattern in alts is irrefutable, we could never get past the end.
        if (pattern1.is_irrefutable())
            return true;
        // If any pattern in alts is is a ConPat with the same head, then we would never get to pattern2
        else if (pattern2.is_con_pat() and *pattern1.head == *pattern2.head)
            return true;
    }
    return false;
}

/*
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
        else if (pattern.is_con_pat() and obj_con and *pattern.head == obj_con->head)
        {
            int N = pattern.args.size();
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
                auto x = pattern.args[j];
                auto obj_var = obj_con->args[j];
                S2 = S2.erase(x);
                S2 = S2.insert({x, {obj_var}});
            }
            return {{body, S2}};
        }
    }

    return {};
}
*/

bool all_dead_binders(const Occ::Pattern& pat)
{
    for(auto& binder: pat.args)
        if (binder.info.work_dup != amount_t::None or binder.info.code_dup != amount_t::None)
            return false;
    return true;
}

const OtherConUnfolding* is_evaluated_var(const Occ::Exp& e, const in_scope_set& bound_vars)
{
    if (auto v = e.to_var())
    {
        if (auto found = bound_vars.find(*v))
            return to<OtherConUnfolding>(found->first);
    }

    return nullptr;
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

Occ::Exp make_lets(const vector<Occ::Decls>& decls, Occ::Exp E)
{
    for(auto& d: decls | views::reverse)
        E = Occ::Let{d,E};

    return E;
}

Occ::Exp make_case_1(const Occ::Exp& object, vector<Occ::Alt> alts)
{
    // 5. If the case is an identity transformation: case obj of {A -> A; B y -> B y; C z -> obj; _ -> obj}
    if (is_identity_case(object, alts))
    {
        // NOTE: this preserves strictness, because the object is still evaluated.
        return object;
    }
    else
        // TODO: If its a constant case (i.e. all the bodies are 'r'), then mkCase can change it to (case object of _ -> r)
        return Occ::Case{object, alts};
}

Occ::Exp make_case(const Occ::Exp& object, vector<Occ::Alt> alts)
{
    // 4. If the _ branch cases on the same object, then we can lift
    //    out any cases not covered into the upper case and drop the others.
    vector<Occ::Decls> default_decls;
    if (alts.back().pat.is_wildcard_pat() and object.to_var())
    {
        // Make a copy to ensure a reference after we drop the default pattern.
        auto default_body = alts.back().body;
        auto default_decls = strip_multi_let(default_body);

        // We could do this even if the object isn't a variable, right?
        if (auto C = default_body.to_case(); C and C->object.to_var() and C->object == object)
        {
            alts.pop_back();
            for(auto& [pattern2,body2]: C->alts)
            {
                assert(not redundant_pattern(alts, pattern2));
                alts.push_back({pattern2, body2});
            }

            return make_lets(default_decls, make_case_1(object, alts));
        }
    }

    return make_case_1(object,alts);
}

tuple<set<string>,vector<Occ::Alt>>
SimplifierState::prepare_alts(const in_scope_set& bound_vars, const Occ::Exp& object, const vector<Occ::Alt>& alts)
{
    // 1. Determine the object type
    optional<string> object_type;
    set<string> seen_constructors;
    set<string> unseen_constructors;
    for(auto& [pattern, body]: alts)
    {
        // 2.2 Look up the data type or constraint type from the constructor head.
        //     FIXME: Can we just record the type in the Case object?
        if (pattern.is_con_pat())
        {
	    // Get type and its constructors
            if (auto C = this_mod.lookup_resolved_symbol(*pattern.head))
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
            }
            else
            {
                auto T = this_mod.lookup_resolved_type(*pattern.head);
                assert(T);
                assert(T->is_class());
                assert(alts.size() == 1);

                // This is a dictionary for a class.
                object_type = *pattern.head;
                unseen_constructors.insert(*pattern.head);
            }
        }
    }

    // 2. Move OtherCon constructors from the "unseen" to the "seen" set.
    if (auto ocu = is_evaluated_var(object, bound_vars))
    {
        for(auto& con: ocu->constructors_not_taken)
        {
            // If the alts are _ -> body, unseen_constructors might not be set.
            assert(not object_type or unseen_constructors.count(con));
            unseen_constructors.erase(con);
            seen_constructors.insert(con);
        }
    }

    // 3. Drop unreachable alternatives
    vector<Occ::Alt> alts2;
    for(auto& [pattern, body]: alts)
    {
        if (pattern.is_con_pat())
        {
            assert(object_type);

            // Remove this constructors from the total list of constructors
            if (unseen_constructors.count(*pattern.head))
            {
                unseen_constructors.erase(*pattern.head);
                seen_constructors.insert(*pattern.head);
            }
            else 
            {
                assert(seen_constructors.count(*pattern.head));
                continue;
            }
        }
        alts2.push_back({pattern,body});

        // Currently if the alts contain a single irrefutable pattern, both seen and unseen will be empty.
        // Doing this at the end makes sure the single alternative is retained in that case.
        if (pattern.is_irrefutable() or unseen_constructors.empty()) break;
    }

    return {seen_constructors, alts2};
}

// case object of alts.  Here the object has been simplified, but the alts have not.
std::tuple<SimplFloats, Occ::Exp> SimplifierState::rebuild_case_inner(Occ::Exp object, vector<Occ::Alt> alts, const substitution& S, const in_scope_set& bound_vars)
{
    assert(not object.to_let());

    //  Core is strict in the case object, so any optimizations must ensure that the object is evaluated.

    // NOTE: Any thing that relies on occurrence info for pattern vars should be done here, before
    //       we simplify alternatives, because that simplification can introduce new uses of the pattern vars.
    // Example: case #1 of {x:xs -> case #1 of {y:ys -> ys}} ==> case #1 of {x:xs -> xs} 
    //       We set #1=x:xs in the alternative, which means that a reference to #1 can reference xs.

    // 1. Drop impossible alternatives.
    auto [seen_constructors, alts2] = prepare_alts(bound_vars, object, alts);

    // 2. Simplify each alternative
    SimplFloats F({}, bound_vars);
    for(auto& [pattern, body]: alts2)
    {
	// 2.1. Rename and bind pattern variables
	auto [S2, bound_vars2] = rename_and_bind_pattern_vars(pattern, S, bound_vars);

        // 2.3 Set unfolding for x in this branch only.
	if (auto v = object.to_var())
        {
            // Compute the unfolding
            auto x = *v;
            Unfolding unfolding;
            if (pattern.is_irrefutable())
            {
                auto other_cons = seen_constructors | ranges::to<vector>;
                unfolding = OtherConUnfolding(other_cons);
            }
            else
            {
                auto pattern_expression = pattern_to_expression(pattern).value();
                unfolding = make_core_unfolding(this_mod, options, pattern_expression);
            }

            // Set the unfolding
            if (is_local_symbol(x.name, this_mod.name))
                bound_vars2 = rebind_var(bound_vars2, x, unfolding);
            else
            {
                assert(special_prelude_symbol(x.name) or this_mod.lookup_external_symbol(x.name));
                x.info.work_dup = amount_t::Many;
                x.info.code_dup = amount_t::Many;
                if (bound_vars2.count(x))
                    bound_vars2 = rebind_var(bound_vars2, x, unfolding);
                else
                    bound_vars2 = bind_var(bound_vars2, x, unfolding);
            }
        }

	// 2.4. Simplify the alternative body
	auto [body_floats, body2] = simplify(body, S2, bound_vars2, make_ok_context());

        // 2.5 Lift lets out of the default alternative, so that we can merge case statements.
        // In theory we could lift out floats if
        // (i)  all_dead_binders(pattern)
        // (ii) we don't substitute any of the binders into the body by putting them into the unfolding
        if (pattern.is_irrefutable())
        {
            F.append(this_mod, options, body_floats);
            body = body2;
        }
        else
            body = wrap(body_floats, body2);
    }

    return { F, make_case(object, alts2) };
}

bool alts_would_dup(const vector<Occ::Alt>& alts)
{
    return alts.size() > 1;
}

std::tuple<SimplFloats, inline_context> SimplifierState::make_dupable_case_cont(const substitution& S, const in_scope_set& bound_vars, const vector<Occ::Alt>& alts, const inline_context& cont)
{
    if (alts_would_dup(alts))
    {
        return make_dupable_cont(S, bound_vars, cont);
    }
    else
    {
        return {SimplFloats(bound_vars), cont};
    }
        
}

bool is_dupable(const inline_context& cont)
{
    if (cont.is_ok_context())
    {
        return true;
    }
    else if (auto ac = cont.is_apply_context())
    {
        return ac->dup_status == DupStatus::OkToDup and is_dupable(ac->next);
    }
    else if (auto cc = cont.is_case_context())
    {
        return cc->dup_status == DupStatus::OkToDup and is_dupable(cc->next);
    }
    else
        std::abort();
}

std::tuple<DupStatus, in_scope_set, Occ::Exp>
SimplifierState::simplifyArg(const in_scope_set& bound_vars, DupStatus dup_status, const substitution& arg_S, const in_scope_set& arg_bound_vars, const Occ::Exp& arg)
{
    if (dup_status == DupStatus::Simplified or dup_status == DupStatus::OkToDup)
        return {dup_status, arg_bound_vars, arg};
    else
    {
        // combine substitution from arg_env with in_scope_set from env
        auto arg2 = wrap(simplify(arg, arg_S, bound_vars, make_ok_context()));
        return {DupStatus::Simplified, bound_vars, arg2};
    }
}

std::tuple<SimplFloats, inline_context>
SimplifierState::make_dupable_cont(const substitution& S, const in_scope_set& bound_vars, const inline_context& cont)
{
    if (is_dupable(cont))
        return {SimplFloats(bound_vars), cont};

    // ok_context handled above

    else if (auto ac = cont.is_apply_context())
    {
        auto [floats1, cont1] = make_dupable_cont(S, bound_vars, ac->next);
        // env' = (S, floats1.bound_vars)
        auto [dup, bound_vars3, arg2] = simplifyArg(floats1.bound_vars, ac->dup_status, ac->subst, ac->bound_vars, ac->arg);
        // makeTrivial env 
        auto k = get_fresh_occ_var("karg");
        k.info.work_dup = amount_t::Many;
        k.info.code_dup = amount_t::Many;
        Occ::Decls decls{{k,arg2}};
        floats1.append(this_mod, options, decls);
        auto ac2 = std::make_shared<apply_context>(k, substitution(), floats1.bound_vars, ac->next);
        ac2->dup_status = DupStatus::OkToDup;
        return {floats1, ac2};
    }
    else if (auto cc = cont.is_case_context())
    {
    }
    else
        std::abort();
}

std::tuple<SimplFloats, Occ::Exp> SimplifierState::rebuild_case(Occ::Exp object, const vector<Occ::Alt>& alts, const substitution& S, const in_scope_set& bound_vars, const inline_context& context)
{
    assert(not object.to_let());

    SimplFloats F(bound_vars);

    // 1. Take a specific branch if the object is a constant
    if (auto constant = exprIsConApp_maybe(object, bound_vars))
    {
        auto& [bound_vars2, floats, con, args] = *constant;
        auto found = select_case_alternative(con, alts);
        if (not found)
            throw myexception()<<"Case object '"<<con<<"' doesn't match any alternative in '"<<Occ::Case{object,alts}.print()<<"'";

        auto& [pattern, body] = *found;
        assert(pattern.is_wildcard_pat() or pattern.args.size() == args.size());

        // NOTE: the previous approach used substitutions, which worked because the 
        auto S2 = S;
        Occ::Decls decls;
        for(int i=0;i<pattern.args.size();i++)
        {
            auto arg2 = rename_var(pattern.args[i], S2, bound_vars);
            // args[i] is already simplified, arg2 is already renamed
            decls.push_back({arg2, args[i]});
        }

        F.append(this_mod, options, decls);

        auto [F2, E2] = simplify(apply_floats(floats,body), S2, F.bound_vars, context);

        F.append(this_mod, options, F2);

        return { F, E2 };
    }
    else if (object.to_constant())
    {
        assert(alts.size() == 1 and alts[0].pat.is_wildcard_pat());
        return simplify(alts[0].body, S, bound_vars, context);
    }

    // Everything that the old approach caught should be caught by exprIsConApp_maybe.
    assert(not is_WHNF(object) or object.to_var());

    if (alts.size() == 1)
    {
        if (all_dead_binders(alts[0].pat) and (bool)is_evaluated_var(object, bound_vars))
            return simplify(alts[0].body, S, bound_vars, context);
    }

    if (options.case_of_case)
    {
//        auto [F1, context2] = make_dupable_case_cont(S, bound_vars, alts, context);

        // FIXME2: We should be passing the continuation into here.
        auto [F2, E2] = rebuild_case_inner(object, alts, S, F.bound_vars);

        F.append(this_mod, options, F2);
    
        auto [F3,E3] = rebuild(E2, F.bound_vars, context);

        F.append(this_mod, options, F3);
    
        return {F, E3};
    }
    else
    {
        // FIXME2: We should be passing the continuation into here.
        auto [F2,E2] = rebuild_case_inner(object, alts, S, F.bound_vars);

        F.append(this_mod, options, F2);
    
        auto [F3,E3] = rebuild(E2, F.bound_vars, context);

        F.append(this_mod, options, F3);
    
        return {F, E3};
    }
}


std::tuple<SimplFloats, Occ::Exp> SimplifierState::rebuild_apply(Occ::Exp E, const Occ::Exp& arg, const substitution& S, const in_scope_set& bound_vars, const inline_context& context)
{
    assert(not E.to_let());

    SimplFloats F(bound_vars);
    
    auto [arg_floats, arg2] = simplify(arg, S, F.bound_vars, make_ok_context());

    F.append(this_mod, options, arg_floats);
    
    auto [F2, E2] = rebuild(Occ::Apply{E, arg2}, F.bound_vars, context);
    
    F.append(this_mod, options, F2);

    return {F, E2};
}

// QUESTION: Should I avoid inlining into cases because they might get re-executed?
//           Right now I think I do not avoid it.
//           However, I might try and let-float things out of cases.

bool pre_inline(const Occ::Var& x, const Occ::Exp& rhs)
{
    // Don't eliminate exported variables!
    if (x.is_exported) return false;

    // Unlike in post_inline, here we DO need to keep occurrence info up-to-date.
    if (x.info.is_loop_breaker) return false;

    if (x.info.work_dup != amount_t::Once and x.info.work_dup != amount_t::None) return false;
    if (x.info.code_dup != amount_t::Once and x.info.code_dup != amount_t::None) return false;

    return true;
}

bool post_inline(const Occ::Var& x, const Occ::Exp& rhs)
{
    // Unlike in pre_inline, here we don't need to keep occurrence info up-to-date.
    if (x.is_exported) return false;
    if (x.info.is_loop_breaker) return false;
    if (is_trivial(rhs)) return true;

    // once -> smallEnoughToInline and ((not top-level and not in_lambda) or (isCheap and interesting_context))
    //         smallEnoughToInline = CoreUnfolding of UnfoldIfGoodArgs with size < 8
    //         interesting_context = case object of application head
    // dead -> true
    
    return false;
}


// FIXME - Until we can know that decls are non-recursive, we can't simplify an Decls into more than one Decls - we have to merge them.

// FIXME - Cache free vars on expressions!

tuple<SimplFloats, substitution>
SimplifierState::simplify_decls(const Occ::Decls& orig_decls, const substitution& S, in_scope_set bound_vars, bool is_top_level)
{
    auto S2 = S;

    const int n_decls = orig_decls.size();

    Occ::Decls new_decls;
    vector<Occ::Var> new_names;

    // 5.1 Rename and bind all variables.
    //     Binding all variables ensures that we avoid shadowing them, which helps with let-floating.
    //     Renaming them is necessary to correctly simplify the bodies.
    for(auto& [x,_]: orig_decls)
    {
	auto x2 = rename_and_bind_var(x, S2, bound_vars);
	if (x.is_exported) assert(x == x2);

	new_names.push_back(x2);
    }

    // 5.2 Iterate over decls, renaming and binding vars as we go, and simplifying them and adding substitutions for unconditional inlines.
    for(int i=0;i<n_decls;i++)
    {
	// If x[i] is not a loop breaker, then x[i] can only BE referenced by LATER E[k] (since loop breakers are later), while
	//                                     E[i] can only reference EARLIER x[k] and loop breakers.
	auto x  = orig_decls[i].x;
	auto rhs  = orig_decls[i].body;

	auto x2 = new_names[i];

	// 1. Any references to x in rhs must be to the x bound in this scope.
	// 2. rhs can only contain references to x if x is a loop breaker.
	// 3. If x is a loop breaker, then S2 already contains substitutions for x -> x2 if needed.
	// 4. Therefore S2 is a good substitution for rhs.
	assert(x.info.is_loop_breaker or not get_free_vars(rhs).count(x));

	// A. Suspended substitutions created by pre-inlining won't be affected if we include unconditionally inlining later-occurring variables.
	//   A.1 This is because substitutions for later-occuring variables that are loop-breakers has already been done.
	//   A.2 Non-loop cannot occur in the bodies rhs that the suspended substitutions will be applied to.
	// B. Therefore, we can create a single substitution object for an entire decl scope, and just include pointers to it.
	// C. The lifetime of the substitution is just the duration of this scope, so raw pointers are fine.
	if (pre_inline(x,rhs))
	{
	    S2 = S2.erase(x);
	    S2 = S2.insert({x,{rhs,S2}});
	}
	else
	{
	    /* See the paper on inlining for why we should NOT consider if rhs is trivial until after we have
                 simplified rhs.

	       One case that seems kind of problematic is let {x=3,y=4,q=let {z=x,y=w} in (z,w)} in ...
               If we simplify the bodies of z and w then we get let {x=3,y=4,q=let {z=3,w=4} in (z,w)} in ...
                 thus (a) duplicating the bodies of x and y, and also making q not look like a constructor.
               This is kind of problematic since we will never be able to resolve `case q of (a,b) -> a`
                 unless we can expose that q is in fact a pair tuple.

               However, consider that we might have decided to unconditionally pre-inline x.  In that case,
                 replacing z with x is bad, since we would then replace z with 3 and get (3,w), which is illegal.
               This the warning about not replacing z with x until after we simplify rhs (below) applies to this case.

               In practice, it seems that we get around this problem by floating the let {z=w,y=w} up since (z,w)
                 is a constructor.  However, if we could somehow avoid substituting x unconditionally in these cases,
                 then we could indeed replace z with x.  Maybe we could avoid marking x as OnceSafe if its only occurrence
                 is in the body of a let?  But then we would have to make sure that we never CREATE the situation where x
                 is the body of a let.  So, maybe we just hope that floating lets out of less is sufficient for now.
	    */

	    // 5.1.2 Simplify rhs.
	    auto [F2, rhs2] = simplify(rhs, S2, bound_vars, make_ok_context());

	    // Should we also float lambdas in addition to constructors?  We could apply them if so...

	    // Float lets out of decl x = rhs
	    if (options.let_float_from_let and (rhs2.to_conApp() or rhs2.to_lambda() or is_top_level))
            {
                rhs = rhs2;
		for(auto& decls: F2.decls)
		    for(auto& decl: decls)
		    {
			bound_vars = bind_var(bound_vars, decl.x, make_core_unfolding(this_mod, options, decl.body));
			new_names.push_back(decl.x);
			new_decls.push_back(decl);
		    }
            }
            else
                rhs = wrap(F2,rhs2);

	    // what are the conditions for post-inlining unconditionally?
	    if (post_inline(x,rhs))
	    {
		S2 = S2.erase(x);
		S2 = S2.insert({x,rhs});
	    }
	    else
	    {
		new_decls.push_back({x2,rhs});

		// Any later occurrences will see the bound value of x[i] when they are simplified.
                Unfolding unfolding;
                bool noinline = false;
                if (is_top_level and is_qualified_by_module(x2.name, this_mod.name))
                {
                    if (auto S = this_mod.lookup_resolved_symbol(x2.name))
                    {
                        noinline = S->inline_pragma == Hs::inline_pragma_t::NOINLINE;

                        if (not to<std::monostate>(S->unfolding) and not noinline)
                            unfolding = S->unfolding;
                    }
                }

                if (to<std::monostate>(unfolding) and not noinline)
                    unfolding = make_core_unfolding(this_mod, options, rhs);

                bound_vars = rebind_var(bound_vars, x2, unfolding);
	    }
	}
    }

    if (new_decls.empty())
        return {SimplFloats({}, bound_vars), S2};
    else
        return {SimplFloats({new_decls}, bound_vars), S2};
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
    // 1. Check that we have \x -> (...)  x
    auto app = L.body.to_apply();
    if (not app or app->arg != L.x) return L;

    // 2. Check that L->body has the form (( ) a b c x) and all other arguments are not x.
    Occ::Exp E = app->head;
    while(auto A = E.to_apply())
    {
        E = A->head;
        if (A->arg == L.x) return L;
    }

    // 3. Check that the head is a var that is not x.
    auto V = E.to_var();
    if (not V or *V == L.x) return L;
    
    // f x  ==> f
    return app->head;
}

tuple<SimplFloats,Occ::Exp> SimplifierState::rebuild(const Occ::Exp& E, const in_scope_set& bound_vars, const inline_context& context)
{
    if (auto cc = context.is_case_context())
    {
        return rebuild_case(E, cc->alts, cc->subst, bound_vars, cc->next);
    }
    else if (auto ac = context.is_apply_context())
    {
        auto [_, __, arg2] = simplifyArg(bound_vars, ac->dup_status, ac->subst, ac->bound_vars, ac->arg);

        return rebuild(Occ::Apply{E, arg2}, bound_vars, ac->next);
    }
    else
        return {SimplFloats(), E};
}

// Q1. Where do we handle beta-reduction (@ constant x1 x2 ... xn)?
// Q2. Where do we handle case-of-constant (case constant of alts)?
// Q3. How do we handle local let-floating from (i) case objects, (ii) apply-objects, (iii) let-bound statements?
std::tuple<SimplFloats,Occ::Exp> SimplifierState::simplify(const Occ::Exp& E, const substitution& S, const in_scope_set& bound_vars, const inline_context& context)
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

	    return simplify_out_var(*x, bound_vars, context);
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
            SimplFloats F(bound_vars);
            auto [_,__, arg] = simplifyArg(bound_vars, ac->dup_status, ac->subst, ac->bound_vars, ac->arg);

            if (pre_inline(x,arg))
            {
                S2 = S2.erase(x);
                S2 = S2.insert({x,arg});
                return simplify(lam->body, S2, bound_vars, ac->next);
            }
            else
            {
                auto x2 = rename_var(lam->x, S2, bound_vars);
                SimplFloats F(bound_vars);
                Occ::Decls decls{{x2,arg}};
                F.append(this_mod, options, decls);
                auto [F2,E2] = simplify(lam->body, S2, F.bound_vars, ac->next);

                F.append(this_mod, options, F2);
                return { F, E2 };
            }
        }

	// 2.2. Get the new name, possibly adding a substitution
        auto bound_vars_x = bound_vars;
	auto x2 = rename_and_bind_var(lam->x, S2, bound_vars_x);

	// 2.3 Simplify the body with x added to the bound set.
	auto new_body = wrap(simplify(lam->body, S2, bound_vars_x, make_ok_context()));

	// 2.4 Return (\x2 -> new_body) after eta-reduction
        auto L = Occ::Lambda{x2, new_body};

        // 2.5 Maybe eta reduce
        //     I don't think there can be any substitutions that make the function body or other arguments
        //     depend on x here, so this SHOULD be safe...
        //
        //     BROKEN: Now that the arguments can be expressions, there's no fast way to check if x occurs only once.
        //     It seems like we should be able to look at the occurrence info... but that seems to be incorrect?
        // auto E2 = maybe_eta_reduce2( L );

        return rebuild(L, bound_vars, context);
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
	return simplify(app->head, S, bound_vars, make_apply_context(*app, S, bound_vars, context));
    }

    // 5. Let (let {x[i] = F[i]} in body)
    //
    // Here we know that F[i] can only mention x[j<i] unless F[i] is a loop-breaker.
    // 
    else if (auto let = E.to_let())
    {
	auto decls = let->decls;
	auto [F, S2] = simplify_decls(decls, S, bound_vars, false);

        auto [F2, E2] = simplify(let->body, S2, F.bound_vars, context);
        F.append(this_mod, options, F2);

        // 5.2 Simplify the let-body
        return {F, E2};
    }

     // Do we need something to handle WHNF variables?

    // 5. Literal constant.  Treat as 0-arg constructor.
    else if (E.to_constant())
        return rebuild(E, bound_vars, context);

    // 4. Constructor
    else if (auto con = E.to_conApp())
    {
        SimplFloats F(bound_vars);

	Occ::ConApp C = *con;
	for(auto& arg: C.args)
        {
            auto [arg_floats, arg2] = simplify(arg, S, F.bound_vars, make_ok_context());
            arg = arg2;
            F.append(this_mod, options, arg_floats);
        }

        auto [F2,E2] = rebuild(C, F.bound_vars, context);

        F.append(this_mod, options, F2);

        return {F, E2};
    }

    // 4. Builtin
    else if (auto builtin = E.to_builtinOp())
    {
        SimplFloats F(bound_vars);

	Occ::BuiltinOp builtin2;
	builtin2.lib_name = builtin->lib_name;
	builtin2.func_name = builtin->func_name;
	builtin2.op = builtin->op;

	for(auto& arg: builtin->args)
 	{
	    auto [argF, arg2] = simplify(arg, S, F.bound_vars, make_ok_context());
            F.append(this_mod, options, argF);
	    builtin2.args.push_back(arg2);
 	}

	auto [F2, E2] = rebuild(builtin2, F.bound_vars, context);

        F.append(this_mod, options, F2);

        return {F, E2};
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
    vector<Occ::Decls> decl_groups2;
    for(auto& decls: decl_groups)
    {
	auto [F, s] = simplify_decls(decls, S.back(), bound_vars, true);
        decl_groups2.insert(decl_groups2.end(), F.decls.begin(), F.decls.end());
	S.push_back( s );
        bound_vars = F.bound_vars;
    }

    vector<Core2::Decls<>> decl_groups_out;
    for(auto& decls: decl_groups2)
        decl_groups_out.push_back(to_core(decls));

    return decl_groups_out;
}


vector<Core2::Decls<>> simplify_module_gently(const simplifier_options& options, FreshVarState& fresh_var_state, const Module& m,
                                              const vector<Core2::Decls<>>& decl_groups_in)
{
    simplifier_options options_gentle = options;
    options_gentle.case_of_case = false;
    options_gentle.inline_threshhold = -100;
//    options_gentle.beta_reduction = false;  This breaks the inliner.  Should probably fix!

    SimplifierState state(options_gentle, fresh_var_state, m);
    return state.simplify_module_one(decl_groups_in);
}

vector<Core2::Decls<>> simplify_module(const simplifier_options& options, FreshVarState& fresh_var_state, const Module& m,
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


SimplifierState::SimplifierState(const simplifier_options& opts, FreshVarState& state, const Module& m)
    :FreshVarSource(state), options(opts), this_mod(m)
{
}
