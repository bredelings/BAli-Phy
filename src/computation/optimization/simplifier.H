#ifndef SIMPLIFIER_H
#define SIMPLIFIER_H

#include <set>
#include <map>
#include <utility>
#include <variant>

#include "computation/optimization/simplifier_env.H"
#include "computation/optimization/occurrence.H"
#include "computation/fresh_vars.H"
#include "immer/map.hpp"
#include "simplifier_options.H"
#include "substitution.H"
#include "inliner.H"
#include "unfolding.H"

class module_loader;

int get_n_lambdas1(Occ::Exp E);

Occ::Exp peel_n_lambdas1(Occ::Exp E, int n);

class Module;


struct FloatLet
{
    Occ::Decls decls;
};

struct FloatCase
{
    Occ::Exp object;
    // No case binder.
    Occ::Pattern pattern;
};

typedef std::variant<FloatLet,FloatCase> Float;

/* Here we make a continuation structure similar to the one used by the
   simplifier but
   * representing only function application.
   * representing arguments as expressions, not variables.
 */
struct ConContObj;
typedef std::shared_ptr<const ConContObj> ConCont;
struct ConContObj
{
    Occ::Exp arg;
    ConCont next;
    ConContObj(const Occ::Exp& a, const ConCont& n):arg(a), next(n){}
};

std::vector<Core2::Decls<>> simplify_module_gently(const simplifier_options&, FreshVarState&, const Module&, const std::vector<Core2::Decls<>>& decl_groups);
std::vector<Core2::Decls<>> simplify_module(const simplifier_options&, FreshVarState&, const Module&, const std::vector<Core2::Decls<>>& decl_groups);

struct SimplFloats
{
    std::vector<Occ::Decls> decls;
    in_scope_set bound_vars;

    SimplFloats() {}
    SimplFloats(const in_scope_set& bv):bound_vars(bv) {}
    SimplFloats(const std::vector<Occ::Decls>& d, const in_scope_set& bv):decls(d),bound_vars(bv) {}
};


class SimplifierState: FreshVarSource
{
    simplifier_options options;

    const Module& this_mod;

public:

    std::tuple<Unfolding, occurrence_info> get_unfolding(const Occ::Var& x, const in_scope_set& bound_vars);

    Occ::Var get_new_name(Occ::Var x, const in_scope_set& bound_vars);

    Occ::Var rename_var(const Occ::Var& x, simplifier::substitution& S, const in_scope_set& bound_vars);
    Occ::Var rename_and_bind_var(const Occ::Var& Evar, simplifier::substitution& S, in_scope_set& bound_vars);

    std::optional<std::tuple<in_scope_set, std::string, std::vector<Occ::Exp>>>
    exprIsConApp_worker(const in_scope_set& S, std::vector<Float>& floats, const Occ::Exp& E, const ConCont& cont);

    std::optional<std::tuple<in_scope_set, std::vector<Float>, std::string, std::vector<Occ::Exp>>>
    exprIsConApp_maybe(const Occ::Exp& E,  const in_scope_set& bound_vars);

    std::tuple<bool, std::vector<arg_info>, inline_context>
    continuation_args(const inline_context& context_in);

    std::optional<Occ::Exp> try_inline(const Unfolding& unfolding, const occurrence_info& occur, const inline_context& context);

    arg_info interesting_arg(const Occ::Exp& E, const simplifier::substitution& S, const in_scope_set& bound_vars, int n);

    Occ::Exp
    simplify_out_var(const Occ::Var& x, const in_scope_set& bound_vars, const inline_context& context);

    Occ::Exp
    simplify(const Occ::Exp& E, const simplifier::substitution& S, const in_scope_set& bound_vars, const inline_context& context);

    Occ::Exp
    rebuild(const Occ::Exp& E, const in_scope_set& bound_vars, const inline_context& context);

    std::tuple<simplifier::substitution,in_scope_set> rename_and_bind_pattern_vars(Occ::Pattern& pattern, const simplifier::substitution& S, const in_scope_set& bound_vars);

    std::tuple<SimplFloats,Occ::Exp>
    rebuild_case(Occ::Exp object, const std::vector<Occ::Alt>& alts, const simplifier::substitution& S, const in_scope_set& bound_vars, const inline_context& context);

    std::tuple<std::vector<Occ::Decls>,Occ::Exp>
    rebuild_case_inner(Occ::Exp object, std::vector<Occ::Alt> alts, const simplifier::substitution& S, const in_scope_set& bound_vars);

    std::tuple<SimplFloats,Occ::Exp>
    rebuild_apply(Occ::Exp E, const Occ::Exp& arg, const simplifier::substitution& S, const in_scope_set& bound_vars, const inline_context& context);

    std::tuple<Occ::Decls, simplifier::substitution, in_scope_set>
    simplify_decls(const Occ::Decls& orig_decls, const simplifier::substitution& S, in_scope_set bound_vars, bool is_top_level);

    std::vector<Core2::Decls<>>
    simplify_module_one(const std::vector<Core2::Decls<>>& decl_groups);

    SimplifierState(const simplifier_options& opts, FreshVarState& s, const Module& m);
};

bool pre_inline(const Occ::Var& x, const Occ::Exp& e);

#endif
