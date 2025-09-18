#include <iostream>
#include <unordered_map>
#include "computation/operations.H"
#include "computation/loader.H"
#include "occurrence.H"
#include "inliner.H"
#include "simplifier.H"
#include "util/assert.hh"
#include "util/set.H"    // for includes(vector,elem)
#include "util/variant.H" // for to<type>(val)
#include "computation/module.H"

#include "simplifier.H"

#include "range/v3/all.hpp"

namespace views = ranges::views;

using std::string;
using std::vector;
using std::pair;
using std::set;
using std::map;
using std::shared_ptr;
using std::optional;

using std::cerr;
using std::endl;

optional<inline_context> inline_context::prev_context() const
{
    if (auto c = is_case_context())
        return {c->next};
    else if (auto a = is_apply_context())
        return {a->next};
    else
        return {};
}

int num_arguments(inline_context context)
{
    int num = 0;
    while(auto a = context.is_apply_context())
    {
	num++;
	context = a->next;
    }
    return num;
}

inline_context make_case_context(const Occ::Case& C, const simplifier::substitution& S, const inline_context& context)
{
    return std::make_shared<const case_context>(C.alts, S, context);
}

inline_context make_apply_context_one_arg(const Occ::Exp& arg, const simplifier::substitution& S, const inline_context& context)
{
    return std::make_shared<const apply_context>(arg, S, context);
}

inline_context make_apply_context(const Occ::Apply& A,  const simplifier::substitution& S, inline_context context)
{
    return make_apply_context_one_arg(A.arg, S, context);
}

inline_context make_ok_context()
{
    return std::make_shared<const ok_context>();
}

int simple_size(const Occ::Exp& E)
{
    if (E.to_var())
	return 0;

    else if (auto app = E.to_apply())
	return simple_size(app->head) + 1 + simple_size(app->arg);

    else if (auto lam = E.to_lambda())
	return simple_size(lam->body);

    else if (auto let = E.to_let())
    {
	int size = simple_size(let->body) + let->decls.size();

	for(auto& [x,e]: let->decls)
	    size += simple_size(e);

	return size;
    }
    else if (auto C = E.to_case())
    {
        int alts_size = 0;
	for(auto& [pattern, body]: C->alts)
            alts_size = std::max(alts_size, simple_size(body));

	return 1 + simple_size(C->object) + alts_size;
    }
    else if (auto C = E.to_conApp())
    {
        int S = 0;
        for(auto& arg: C->args)
            S += (1+simple_size(arg));
        return S;
    }
    else if (auto B = E.to_builtinOp())
    {
        int S = 0;
        for(auto& arg: B->args)
            S += (1+simple_size(arg));
        return S;
    }
    else
	return 1;
}

int simple_size(const Core2::Exp<>& E)
{
    if (E.to_var())
	return 0;

    else if (auto app = E.to_apply())
	return simple_size(app->head) + 1 + simple_size(app->arg);

    else if (auto lam = E.to_lambda())
	return simple_size(lam->body);

    else if (auto let = E.to_let())
    {
	int size = simple_size(let->body) + let->decls.size();

	for(auto& [x,e]: let->decls)
	    size += simple_size(e);

	return size;
    }
    else if (auto C = E.to_case())
    {
        int alts_size = 0;
	for(auto& [pattern, body]: C->alts)
            alts_size = std::max(alts_size, simple_size(body));

	return 1 + simple_size(C->object) + alts_size;
    }
    else if (auto C = E.to_conApp())
    {
        int S = 0;
        for(auto& arg: C->args)
            S += (1+simple_size(arg));
        return S;
    }
    else if (auto B = E.to_builtinOp())
    {
        int S = 0;
        for(auto& arg: B->args)
            S += (1+simple_size(arg));
        return S;
    }
    else
	return 1;
}

ExprSize zero_size{{0},{},0};

ExprSize sizeN(int n)
{
    return {{n},{},0};
}

inline ExprSize operator+(ExprSize s1, int s2)
{
    s1.size += s2;
    return s1;
}

inline ExprSize operator+(const ExprSize& s1, ExprSize s2)
{
    // combine the sizes
    s2.size += s1.size;

    // symmetric combination of the arg discounts
    for(auto& [var, discounts1]: s1.arg_discounts)
    {
        auto it = s2.arg_discounts.find(var);
        if (not it)
            s2.arg_discounts = s2.arg_discounts.insert({var,discounts1});
        else
        {
            discounts discounts2 = *it;
            s2.arg_discounts = s2.arg_discounts.erase(var);
            s2.arg_discounts = s2.arg_discounts.insert({var, discounts1 + discounts2});
        }
    }

    // ignore the inspection discount of s1

    return s2;
}

int call_size(int n_args)
{
    return 1 + n_args;
}

ExprSize fun_size(const inliner_options& opts, int max_size, const std::vector<Occ::Var>& top_args, const Occ::Var& fun, int n_args)
{
    bool some_args = n_args > 0;

    // 1. Size
    int size = some_args ? 0 : call_size(n_args);

    // 2. Argument discounts
    immer::map<Occ::Var, discounts> arg_discounts;
    if (some_args and includes(top_args, fun))
        arg_discounts = arg_discounts.insert({fun, discounts(opts.fun_app_discount)});
    
    // 3. Inspection discount
    int res_discount = 0;
    // Discount for partial application?  Why?
    // if (fun.arity > n_args)
    //    res_discount = opts.fun_app_discount

    return {{size}, arg_discounts, res_discount};
}

ExprSize size_of_call(const inliner_options& opts, int max_size, const std::vector<Occ::Var>& top_args, const Occ::Var& fun,
                      const vector<Occ::Exp>& args)
{
    // FCall   -> sizeN (call_size(args))
    // DataCon -> conSize con args.size()
    // PrimOp  -> primOpSize op args.size()
    // build (?)
    // augment (?)
    // otherwise -> funSize opts top_args fun args.size()

    return fun_size(opts, max_size, top_args, fun, args.size());
}

ExprSize size_of_app(const inliner_options& opts, int max_size, const std::vector<Occ::Var>& top_args,
                     const Occ::Exp& E, vector<Occ::Exp> args)
{
    if (auto A = E.to_apply())
    {
        // Convert to loop
        args.push_back(A->arg);
        return size_of_expr(opts, max_size, top_args, A->arg) + size_of_app(opts, max_size, top_args, E, args);
    }
    else if (auto V = E.to_var())
        return size_of_call(opts, max_size, top_args, *V, args);
    else
        return size_of_expr(opts, max_size, top_args, E) + call_size(args.size());
}


ExprSize size_of_expr(const inliner_options& opts, int max_size, const std::vector<Occ::Var>& top_args, const Occ::Exp& E)
{
    // NOTE: operator+ is asymmetric!
    //       It keeps the inspection discount for the right argument.

    // QUESTION: ZeroBit Ids have no representation.
    // In theory this would be true for (), for ((),()), etc.
    if (auto V = E.to_var())
    {
        // Var f -> size_up_call x []
        return size_of_call(opts, max_size, top_args, *V, {});
    }
    else if (auto A = E.to_apply())
    {
        // App fun arg -> size_up arg +NSD size_up_app fun [arg] (if zero_bit_id then 1 else 0)
        return size_of_expr(opts, max_size, top_args, A->arg) + size_of_app(opts, max_size, top_args, A->head, {A->arg});
    }
    else if (auto L = E.to_lambda())
    {
        // Lam b e -> size_up e +N 10
        auto body_size = size_of_expr(opts, max_size, top_args, L->body);
        body_size.inspect_discount = opts.fun_app_discount;

        return body_size + 1;
    }
    else if (auto L = E.to_let())
    {
        //size_up rhs1 `addSizeNDS` size_up rhs2 `addSizeNDS` (size_up_body body `addSizeN` number of heap bindings)
        auto size = size_of_expr(opts, max_size, top_args, L->body) + L->decls.size();
        for(auto& [x,e]: L->decls)
        {
            size = size_of_expr(opts, max_size, top_args, e) + size;
        }
        return size;
    }
    else if (auto C = E.to_case())
    {
        // Case e of alts
        // * empty alts -> size_up e

        // * e in top_args -> ???

        // * otherwise ->  size_up e +NSD (size_up_alt alt1 `addAltSize` size_up_alt alt2 `addAltSize` case_size)
    }
    else if (auto C = E.to_constant())
    {
        // sizeN (litSize lit)
    }
    else if (auto B = E.to_builtinOp())
    {
        // Handled by Apply + Var?
    }
    else if (auto C = E.to_conApp())
    {
        // Handled by Apply + Var?
    }
    else
        std::abort();
}

inline_context remove_arguments(inline_context context, int n)
{
    for(int i=0;i<n;i++)
    {
	if (auto a = context.is_apply_context())
            context = a->next;
        else
	    throw myexception()<<"Trying to remove "<<n<<" applications from context, but it only had "<<i<<".";
    }
    return context;
}

bool is_WHNF(const Occ::Exp& E)
{
    // Should we allow variables?  The expression_ref definition does not.
    return (E.to_lambda() or E.to_conApp() or E.to_constant());
}

// Can be substituted into a function argument position -- variable or unlifted constant like 1#.
bool is_trivial(const Occ::Exp& E)
{
    // I think unlifted constants could be trivial as well, but we don't have those.
    return (E.to_var());
}

bool no_size_increase(const Occ::Exp& rhs, const inline_context& context)
{
    // If rhs is a variable, then there's no size increase
    if (is_trivial(rhs)) return true;

    // If we are inlining a constant into a case object, then there will eventually be no size increase... right?
    if (context.is_case_context() and is_WHNF(rhs))
    {
	assert(not rhs.to_lambda());
	return true;
    }

    // If we are inlining a function body that is smaller than its call (e.g. @ . f x ===> @ (\a b -> @ a b) f x ===> let {a=f;b=x} in @ a b ===> @ f x)
    // (e.g. @ $ f ===> @ (\a b -> @ a b) f ===> let a=f in (\b -> @ a b)  ===> (\b -> @ f b) ... wait isn't that the same as f?
    if (context.is_apply_context() and is_WHNF(rhs))
    {
	int n_args_supplied = num_arguments(context);
	assert(n_args_supplied >= 1);
	int n_args_needed = get_n_lambdas1(rhs);
	int n_args_used = std::min(n_args_needed, n_args_supplied);

	int size_of_call = n_args_supplied;
	auto body = peel_n_lambdas1(rhs, n_args_used);
	int size_of_body = simple_size(body);

	if (size_of_body <= size_of_call) return true;
    }

    return false;
}

bool very_boring(const inline_context& context)
{
    if (context.is_case_context()) return false;

    if (context.is_apply_context()) return false;

    // This avoids substituting into constructor (and function) arguments.
    return true;
}


bool boring(const Occ::Exp& rhs, const inline_context& context)
{
    // if the rhs is applied only to variables with unknown value AND ...

    // ... after consuming all the arguments we need, the result is very_boring.
    {
	int n_args_needed = get_n_lambdas1(rhs);
	if (num_arguments(context) >= n_args_needed)
	{
	    auto context2 = remove_arguments(context, n_args_needed);
	    if (not very_boring(context2)) return false;
	}
    }


    return true;
}

bool SimplifierState::small_enough(const Occ::Exp& rhs, const inline_context& context)
{
    double body_size = simple_size(rhs);

    double size_of_call = 1 + num_arguments(context);

    int discounts = 0;

    return (body_size - size_of_call - discounts <= options.inline_threshhold);
}

optional<Occ::Exp> SimplifierState::try_inline_multi(const Occ::Exp& rhs, const inline_context& context)
{
    if (no_size_increase(rhs,context)) return rhs;

    if (boring(rhs,context)) return {};

    if(small_enough(rhs, context))
        return rhs;
    else
        return {};
}

bool evaluates_to_bottom(const Occ::Exp& /* rhs */)
{
    return false;
}

bool whnf_or_bottom(const Occ::Exp& rhs)
{
    return is_WHNF(rhs) or evaluates_to_bottom(rhs);
}

optional<Occ::Exp> SimplifierState::try_inline(const Unfolding& unfolding, const occurrence_info& occur, const inline_context& context)
{
    auto cu = to<CoreUnfolding>(unfolding);
    if (not cu) return {};

    auto& rhs = cu->expr;

    // If always_unfold
    if (cu->always_unfold)
    {
        assert(not occur.is_loop_breaker);
        return rhs;
    }

    // LoopBreaker
    if (occur.is_loop_breaker)
	return {};

    // OnceSafe
    else if (occur.pre_inline())
    {
//	if (options.pre_inline_unconditionally and not occur.is_exported and false)
//	    throw myexception()<<"Trying to inline OnceSafe variable!";
        return rhs;
    }

    // If its "trivial" but not a variable, we should substitute if we can.
    if (rhs.to_constant() or rhs.to_conApp())
	return rhs;

    // MultiSafe
    else if (occur.work_dup == amount_t::Once and occur.code_dup == amount_t::Many)
	return try_inline_multi(rhs, context);

    // OnceUnsafe
    else if (occur.work_dup == amount_t::Many and occur.code_dup == amount_t::Once)
    {
	if (whnf_or_bottom(rhs) and (no_size_increase(rhs,context) or not very_boring(context)))
            return rhs;
        else
            return {};

    }
    // OnceUnsafe
    else if (occur.work_dup == amount_t::Once and occur.code_dup == amount_t::Once and occur.context == var_context::argument)
    {
	if (whnf_or_bottom(rhs) and (no_size_increase(rhs,context) or not very_boring(context)))
            return rhs;
        else
            return {};
    }

    // MultiUnsafe
    else if (occur.work_dup == amount_t::Many and occur.code_dup == amount_t::Many)
    {
	if (whnf_or_bottom(rhs))
            return try_inline_multi(rhs, context);
        else
            return {};
    }

    std::abort();
}

std::tuple<Unfolding,occurrence_info> SimplifierState::get_unfolding(const Occ::Var& x, const in_scope_set& bound_vars)
{
    Unfolding unfolding;
    occurrence_info occ_info;
    if (is_local_symbol(x.name, this_mod.name))
    {
        const auto& [unfolding2, occ_info2] = bound_vars.at(x);
        unfolding = unfolding2;
        occ_info = occ_info2;
    }
    else
    {
        assert(not x.name.empty());

        if (auto S = this_mod.lookup_resolved_symbol(x.name))
            unfolding = S->unfolding;
        else
            throw myexception()<<"Symbol '"<<x.name<<"' not transitively included in module '"<<this_mod.name<<"'";

        occ_info.work_dup = amount_t::Many;
        occ_info.code_dup = amount_t::Many;
    }
    return {unfolding, occ_info};
}

arg_info SimplifierState::interesting_arg(const Occ::Exp& E, const simplifier::substitution& S, const in_scope_set& bound_vars, int n)
{
    if (auto x = E.to_var())
    {
        // Interesting, GHC ALWAYS replaces variables during the substitution.  Is that how it sets unfoldings everywhere?
        if (auto it = S.find(*x))
        {
            if (it->S) // ContEx
                return interesting_arg(it->E, *(it->S), bound_vars, n);
            else // DoneEx
                return interesting_arg(it->E, {}, bound_vars, n);
        }
        else // DoneId
        {
            if (is_haskell_con_name(x->name))
                return arg_info::value;
            // This is (?) for primops, which I can't really use?
            // else if (x->arity > n)
            //    return arg_info::value;
            else if (n > 0)
                return arg_info::non_trivial;
            else
            {
                auto [unfolding,_] = get_unfolding(*x, bound_vars);
                // OtherCon [] -> arg_info::non_trivial;
                // OtherCon _  -> arg_info::value;
                if (to<DFunUnfolding>(unfolding))
                    return arg_info::value;
                else if (auto cu = to<CoreUnfolding>(unfolding))
                {
                    if (cu->expr.to_conApp()) // uf_is_conlike
                        return arg_info::value;
                    else if (cu->expr.to_lambda()) // uf_is_value
                        return arg_info::non_trivial;
                    else
                        return arg_info::trivial;
                }
                else
                    return arg_info::trivial;
            } 
        }
    }
    else if (E.to_constant())
    {
        // We don't actually know how to use literals in the simplifier!
        // So there is no benefit (I think) to knowing a literal value for a function.
        return arg_info::trivial;
        // ?? return arg_info::non_trivial;

        // If we could actually simplify literals, then we could use this...
        // return arg_info::value; 
    }
    else if (auto app = E.to_apply())
    {
        return interesting_arg(app->head, S, bound_vars, n+1);
    }
    else if (E.to_lambda())
    {
        if (n > 0)
            return arg_info::non_trivial;
        else
            return arg_info::value;
    }
    else if (E.to_case())
        return arg_info::non_trivial;
    else if (E.to_builtinOp())
    {
        // I don't think we can use this to simplify things.. but maybe there is still some value to including them?
        return arg_info::non_trivial;
    }
    else if (auto let = E.to_let())
    {
        // GHC adds the let-binders to the "env" .... to the substitution?
        return interesting_arg(let->body, S, bound_vars, n);
    }
    else if (E.to_conApp())
        return arg_info::non_trivial;
    else
        std::abort();
}
