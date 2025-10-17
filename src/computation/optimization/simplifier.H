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

    void append(const Module& m, const inliner_options& opts, const Occ::Decls& d);
    void append(const Module& m, const inliner_options& opts, const std::vector<Occ::Decls>& d);
    void append(const Module& m, const inliner_options& opts, const SimplFloats& F);

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

    std::tuple<SimplFloats,Occ::Exp>
    simplify_out_var(const Occ::Var& x, const in_scope_set& bound_vars, const inline_context& context);

    std::tuple<SimplFloats,Occ::Exp>
    simplify(const Occ::Exp& E, const simplifier::substitution& S, const in_scope_set& bound_vars, const inline_context& context);

    std::tuple<DupStatus, in_scope_set, Occ::Exp>
    simplifyArg(const in_scope_set& bound_vars, DupStatus dup_status, const simplifier::substitution& arg_S, const in_scope_set& arg_bound_vars, const Occ::Exp& arg);

    std::tuple<std::set<std::string>,std::vector<Occ::Alt>>
    prepare_alts(const in_scope_set& bound_vars, const Occ::Exp& object, const std::vector<Occ::Alt>& alts);

    Occ::Alt
    simplify_alt(const std::optional<Occ::Exp>& object, const std::set<std::string>& seen_constructors, const simplifier::substitution& S, const in_scope_set& bound_vars, Occ::Alt alt, const inline_context& context);

     std::tuple<SimplFloats,Occ::Exp>
    rebuild(const Occ::Exp& E, const in_scope_set& bound_vars, const inline_context& context);

    std::tuple<simplifier::substitution,in_scope_set> rename_and_bind_pattern_vars(Occ::Pattern& pattern, const simplifier::substitution& S, const in_scope_set& bound_vars);

    std::tuple<SimplFloats,Occ::Exp>
    rebuild_case(Occ::Exp object, const std::vector<Occ::Alt>& alts, const simplifier::substitution& S, const in_scope_set& bound_vars, const inline_context& context);

    Occ::Exp
    rebuild_case_inner(Occ::Exp object, std::vector<Occ::Alt> alts, const simplifier::substitution& S, const in_scope_set& bound_vars);

    std::tuple<SimplFloats,Occ::Exp>
    rebuild_apply(Occ::Exp E, const Occ::Exp& arg, const simplifier::substitution& S, const in_scope_set& bound_vars, const inline_context& context);

    std::tuple<SimplFloats, inline_context>
    make_dupable_cont( const simplifier::substitution& S, const in_scope_set& bound_vars, const inline_context& cont);

    std::tuple<SimplFloats, inline_context>
    make_dupable_case_cont( const simplifier::substitution& S, const in_scope_set& bound_vars, const std::vector<Occ::Alt>& alts, const inline_context& cont);

    std::tuple<SimplFloats, simplifier::substitution>
    simplify_decls(const Occ::Decls& orig_decls, const simplifier::substitution& S, in_scope_set bound_vars, bool is_top_level);

    std::vector<Core2::Decls<>>
    simplify_module_one(const std::vector<Core2::Decls<>>& decl_groups);

    SimplifierState(const simplifier_options& opts, FreshVarState& s, const Module& m);
};

bool pre_inline(const Occ::Var& x, const Occ::Exp& e);

#endif
