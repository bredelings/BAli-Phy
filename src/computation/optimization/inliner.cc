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

var_discounts union_var_discounts(const var_discounts& d1, var_discounts d2)
{
    if (d1.size() <= d2.size())
    {
        // symmetric combination of the arg discounts
        for(auto& [var, discounts1]: d1)
        {
            auto it = d2.find(var);
            if (not it)
                d2 = d2.insert({var,discounts1});
            else
            {
                discounts discounts2 = *it;
                d2 = d2.erase(var);
                d2 = d2.insert({var, discounts1 + discounts2});
            }
        }
        return d2;
    }
    else
        return union_var_discounts(d2,d1);
}

inline ExprSize operator+(const ExprSize& s1, ExprSize s2)
{
    // combine the sizes
    s2.size += s1.size;

    // symmetric combination of the arg discounts
    s2.arg_discounts = union_var_discounts(s1.arg_discounts, s2.arg_discounts);

    // ignore the inspection discount of s1

    return s2;
}

inline ExprSize add_alts_size(const ExprSize& s1, ExprSize s2)
{
    // combine the sizes
    s2.size += s1.size;

    // symmetric combination of the arg discounts
    s2.arg_discounts = union_var_discounts(s1.arg_discounts, s2.arg_discounts);

    // include discounts for all case branches
    s2.inspect_discount += s1.inspect_discount;
    
    return s2;
}

int call_size(int n_args)
{
    return 1 + n_args;
}

ExprSize con_size(const std::string& /*con_name*/, int n_args)
{
    ExprSize csize;
    csize.inspect_discount = 1;

    // if unary (i.e. (), ((),()), ?) then return sizeN(0)

    if (n_args > 0) csize.size = 1;

    return csize;
}

int lit_size(const Core2::Constant& lit)
{
    if (auto s = to<string>(lit.value))
        return 1 + (s->size()+3)/40;
    else
        return 0; // Like a nullary constructor
}

ExprSize fun_size(const inliner_options& opts, const std::vector<Occ::Var>& top_args, const Occ::Var& fun, int n_args)
{
    bool some_args = n_args > 0;

    // 1. Size
    int size = some_args ? call_size(n_args) : 0;

    // 2. Argument discounts
    immer::map<Occ::Var, discounts> arg_discounts;
    if (some_args and includes(top_args, fun))
        arg_discounts = arg_discounts.insert({fun, discounts(opts.fun_app_discount, 0)});

    // 3. Inspection discount
    int res_discount = 0;
    // Discount for partial application?  Why?
    // if (fun.arity > n_args)
    //    res_discount = opts.fun_app_discount

    return {{size}, arg_discounts, res_discount};
}

ExprSize class_op_size(const inliner_options& opts, const std::vector<Occ::Var>& top_args, const Occ::Var& fun, const vector<Occ::Exp>& args)
{
    // if the class is a unary class then return sizeN(0);

    if (args.empty()) return sizeN(0);

    var_discounts arg_discounts;
    if (args.size() > 1)
    {
        if (auto x = args[0].to_var(); x and includes(top_args,*x))
        {
            // Dictionary discounts are case discounts, because dictionaries are objects.
            discounts dict_discount(0, opts.dict_discount);
            arg_discounts = arg_discounts.insert({*x, dict_discount});
        }
    }
    
    return {1 + (int)args.size(), arg_discounts, 0};
}

bool is_class_op(const Module& m, const Occ::Var& fun)
{
    if (fun.index != 0) return false;

    if (not is_qualified_symbol(fun.name)) return false;

    auto S = m.lookup_resolved_symbol(fun.name);

    if (not S) return false;

    return S->symbol_type == symbol_type_t::superclass_selector or S->symbol_type == symbol_type_t::class_method;
}


ExprSize size_of_call(const Module& m,
                      const inliner_options& opts, int /*max_size*/, const std::vector<Occ::Var>& top_args, const Occ::Var& fun,
                      const vector<Occ::Exp>& args)
{
    // FCall   -> sizeN (call_size(args))
    // DataCon -> conSize con args.size()
    // PrimOp  -> primOpSize op args.size()
    // build (?)
    // augment (?)
    // otherwise -> funSize opts top_args fun args.size()

    if (is_haskell_conid(fun.name))
        return con_size(fun.name, args.size());
    else if (is_class_op(m, fun))
        return class_op_size(opts, top_args, fun, args);
    else
        return fun_size(opts, top_args, fun, args.size());
}

ExprSize size_of_alt(const Module& m, const inliner_options& opts, int max_size, const std::vector<Occ::Var>& top_args, const Occ::Alt& alt)
{
    return size_of_expr(m, opts, max_size, top_args, alt.body) + 1;
}

ExprSize size_max(const ExprSize& s1, const ExprSize& s2)
{
    if (s1.size > s2.size)
        return s1;
    else
        return s2;
}

ExprSize size_sum(const Module& m, const inliner_options& opts, int max_size, const std::vector<Occ::Var>& top_args, const vector<Occ::Exp>& Es)
{
    ExprSize esize;
    for(auto& E: Es)
        esize = esize + size_of_expr(m, opts, max_size, top_args, E);
    return esize;
}

ExprSize size_of_expr(const Module& m, const inliner_options& opts, int max_size, const std::vector<Occ::Var>& top_args, const Occ::Exp& E)
{
    // NOTE: operator+ is asymmetric!
    //       It keeps the inspection discount for the right argument.

    // NOTE: s1 + sizeN(n) != s1 + n.
    //       The second one is right.
    
    // QUESTION: ZeroBit Ids have no representation.
    // In theory this would be true for (), for ((),()), etc.
    if (auto V = E.to_var())
    {
        // Var f -> size_up_call x []
        return size_of_call(m, opts, max_size, top_args, *V, {});
    }
    else if (auto A = E.to_apply())
    {
        vector<Occ::Exp> args;
        args.push_back(A->arg);
        while(A->head.to_apply())
        {
            A = A->head.to_apply();
            args.push_back(A->arg);
        }
        std::reverse(args.begin(), args.end());

        auto head = A->head;

        auto args_size = size_sum(m, opts, max_size, top_args, args);

        if (auto V = A->head.to_var())
            return args_size + size_of_call(m, opts, max_size, top_args, *V, args);
        else
            return args_size + (size_of_expr(m, opts, max_size, top_args, head) + call_size(args.size()));
    }
    else if (auto L = E.to_lambda())
    {
        // Lam b e -> size_up e +N 10
        auto body_size = size_of_expr(m, opts, max_size, top_args, L->body);
        body_size.inspect_discount = opts.fun_app_discount;

        return body_size + 1;
    }
    else if (auto L = E.to_let())
    {
        //size_up rhs1 `addSizeNDS` size_up rhs2 `addSizeNDS` (size_up_body body `addSizeN` number of heap bindings)
        ExprSize rhs_sizes;
        for(auto& [x,e]: L->decls)
            rhs_sizes = rhs_sizes + size_of_expr(m, opts, max_size, top_args, e);
        return rhs_sizes + (size_of_expr(m, opts, max_size, top_args, L->body) + L->decls.size());
    }
    else if (auto C = E.to_case())
    {
        if (C->alts.empty())
            return size_of_expr(m, opts, max_size, top_args, C->object);
        else if (auto x = C->object.to_var(); x and includes(top_args, *x))
        {
            ExprSize sizeSum;
            ExprSize sizeMax;
            
            for(auto& alt: C->alts)
            {
                auto size = size_of_expr(m, opts, max_size, top_args, alt.body);
                sizeSum = add_alts_size(sizeSum, size);
                sizeMax = size_max(sizeMax, size);
            }

            // Use sizeSum, but add var-discount that takes it from the sum down to the max
            var_discounts object_discount;
            object_discount = object_discount.insert({*x, discounts(0, 2 + sizeSum.size - sizeMax.size)});
            sizeSum.arg_discounts = union_var_discounts(sizeSum.arg_discounts, object_discount);

            return sizeSum;
        }
        else
        {
            ExprSize alts_size;
            for(auto& alt: C->alts)
                alts_size = add_alts_size(alts_size, size_of_expr(m, opts, max_size, top_args, alt.body));
            return size_of_expr(m, opts, max_size, top_args, C->object) + alts_size;
        }
    }
    else if (auto C = E.to_constant())
    {
        // sizeN (litSize lit)
        return sizeN(lit_size(*C));
    }
    else if (auto B = E.to_builtinOp())
    {
        // Like fun_size, but no discount for applying top_args.
        return size_sum(m, opts, max_size, top_args, B->args) + B->args.size();
    }
    else if (auto CA = E.to_conApp())
    {
        // Like size_of_call(...);
        return size_sum(m, opts, max_size, top_args, CA->args) + con_size(CA->head, CA->args.size());
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

bool is_trivial(const Occ::Exp& e)
{
    // It doesn't seem like ghc wants to inline ConApps.  Perhaps because we can do case-of-constant w/o actually inlining them.  

    // Should we inline simple literals, like `1`?

    return e.to_var();
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

vector<Occ::Var> compute_top_binders(Occ::Exp E)
{
    vector<Occ::Var> args;
    while (auto L = E.to_lambda())
    {
        args.push_back(L->x);
        E = L->body;
    }

    return args;
}

bool unconditionally_inline(const Occ::Exp& e, int arity, int size)
{
    if (arity > 0)
        return (size <= arity + 1);
    else
        return is_trivial(e); // arity == 0, so rhs==expr
}

/*
 * Problem: we don't know if arguments are functions or not.
 * - We don't store information on variable types in Core.
 * - So, if we case a function multiple times but never apply it, then we won't know if its a function or not.
 * - Now, this can only happen when if we do `case x of _ -> `.
 * 
 */


UnfoldingGuidance make_unfolding_guidance(const Module& m, const inliner_options& opts, const Occ::Exp& e)
{
    auto top_binders = compute_top_binders(e);
    int n_binders = top_binders.size();
    int max_size = opts.creation_threshold;
    auto size = size_of_expr(m, opts, max_size, top_binders, e);

    // We should actually be looking for an empty optional<ExprSize> here.
    if (size.size > max_size) return UnfoldNever();
    
    if (unconditionally_inline(e, n_binders, size.size))
        return UnfoldWhen(true, true, n_binders);
    
    // If this is a top bottoming expression return UnfoldNever()
    else
    {
        std::vector<int> arg_discounts;
        for(auto& x: top_binders)
        {
            int d = 0;
            auto dd = size.arg_discounts.find(x);
            if (dd)
            {
                if (dd->max > 0)
                    d = dd->max;
                else
                    d = dd->sum;
            }
            arg_discounts.push_back(d);
        }

        return UnfoldIfGoodArgs(arg_discounts, size.size, size.inspect_discount);
    }
}

CoreUnfolding make_core_unfolding(const Module& m, const inliner_options& opts, const Occ::Exp& e)
{
    return CoreUnfolding(e, make_unfolding_guidance(m, opts,e));
}
