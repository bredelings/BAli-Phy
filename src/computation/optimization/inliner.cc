#include <iostream>
#include <unordered_map>
#include "computation/operations.H"
#include "computation/loader.H"
#include "computation/expression/constructor.H"
#include "computation/expression/let.H"
#include "computation/expression/case.H"
#include "computation/expression/var.H"
#include "computation/expression/apply.H"
#include "computation/expression/lambda.H"
#include "computation/expression/trim.H"
#include "computation/expression/indexify.H"
#include "computation/expression/expression.H" // for is_WHNF( )
#include "computation/expression/convert.H" // for Occ::Exp, to_occ_exp
#include "occurrence.H"
#include "inliner.H"
#include "simplifier.H"
#include "util/assert.hh"

#include "simplifier.H"

#include "range/v3/all.hpp"

namespace views = ranges::views;

using std::string;
using std::vector;
using std::pair;
using std::set;
using std::map;
using std::shared_ptr;

using std::cerr;
using std::endl;

std::optional<inline_context> inline_context::prev_context() const
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

inline_context make_apply_context_one_arg(const Occ::Var& x, const simplifier::substitution& S, const inline_context& context)
{
    return std::make_shared<const apply_context>(x, S, context);
}

inline_context make_apply_context(const Occ::Apply& A,  const simplifier::substitution& S, inline_context context)
{
    for(auto& x: A.args | views::reverse)
	context = make_apply_context_one_arg(x, S, context);

    return context;
}

inline_context make_stop_context()
{
    return std::make_shared<const stop_context>();
}

inline_context make_ok_context()
{
    return std::make_shared<const ok_context>();
}

int nodes_size(const expression_ref& E)
{
    int total = 1;

    if (E.is_expression())
	for(const auto& e:E.sub())
	    total += nodes_size(e);

    return total;
}

int simple_size(const Occ::Exp& E)
{
    if (E.to_var())
	return 0;

    else if (auto app = E.to_apply())
    {
	int n_args = app->args.size();
	assert(n_args > 0);
	return simple_size(app->head) + n_args;
    }

    else if (auto lam = E.to_lambda())
	return simple_size(lam->body);

    else if (auto let = E.to_let())
    {
	int size = simple_size(let->body);

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
    else
	return 1;
}

int simple_size(const expression_ref& E)
{
    return simple_size(to_occ_exp(E));
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

bool no_size_increase(const expression_ref& rhs, const inline_context& context)
{
    // If rhs is a variable, then there's no size increase
    if (is_trivial(to_occ_exp(rhs))) return true;

    // If we are inlining a constant into a case object, then there will eventually be no size increase... right?
    if (context.is_case_context() and is_WHNF(rhs))
    {
	assert(not rhs.is_a<lambda>());
	return true;
    }

    // If we are inlining a function body that is smaller than its call (e.g. @ . f x ===> @ (\a b -> @ a b) f x ===> let {a=f;b=x} in @ a b ===> @ f x)
    // (e.g. @ $ f ===> @ (\a b -> @ a b) f ===> let a=f in (\b -> @ a b)  ===> (\b -> @ f b) ... wait isn't that the same as f?
    if (context.is_apply_context() and is_WHNF(rhs))
    {
	int n_args_supplied = num_arguments(context);
	assert(n_args_supplied >= 1);
	int n_args_needed = get_n_lambdas1(to_occ_exp(rhs));
	int n_args_used = std::min(n_args_needed, n_args_supplied);

	int size_of_call = 1 + n_args_supplied;
	auto body = peel_n_lambdas1(to_occ_exp(rhs), n_args_used);
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


bool boring(const expression_ref& rhs, const inline_context& context)
{
    // if the rhs is applied only to variables with unknown value AND ...

    // ... after consuming all the arguments we need, the result is very_boring.
    {
	int n_args_needed = get_n_lambdas1(to_occ_exp(rhs));
	if (num_arguments(context) >= n_args_needed)
	{
	    auto context2 = remove_arguments(context, n_args_needed);
	    if (not very_boring(context2)) return false;
	}
    }


    return true;
}

bool SimplifierState::small_enough(const expression_ref& rhs, const inline_context& context)
{
    double body_size = simple_size(rhs);

    double size_of_call = 1 + num_arguments(context);

    int discounts = 0;

    return (body_size - size_of_call - options.keenness*discounts <= options.inline_threshhold);
}

bool SimplifierState::do_inline_multi(const expression_ref& rhs, const inline_context& context)
{
    if (no_size_increase(rhs,context)) return true;

    if (boring(rhs,context)) return false;

    return small_enough(rhs, context);
}

bool evaluates_to_bottom(const expression_ref& /* rhs */)
{
    return false;
}

bool whnf_or_bottom(const expression_ref& rhs)
{
    return is_WHNF(rhs) or evaluates_to_bottom(rhs);
}

bool evaluates_to_bottom(const Occ::Exp& /* rhs */)
{
    return false;
}

bool whnf_or_bottom(const Occ::Exp& rhs)
{
    return is_WHNF(rhs) or evaluates_to_bottom(rhs);
}

bool SimplifierState::do_inline(const expression_ref& rhs, const occurrence_info& occur, const inline_context& context)
{
    auto rhs2 = to_occ_exp(rhs);

    // LoopBreaker
    if (occur.is_loop_breaker)
	return false;

    // Function and constructor arguments
    else if (context.is_stop_context() and not is_trivial(to_occ_exp(rhs)))
	return false;

    // OnceSafe
    else if (occur.pre_inline())
    {
	if (options.pre_inline_unconditionally and not occur.is_exported and false)
	    throw myexception()<<"Trying to inline OnceSafe variable!";
	else
	    return true;
    }

    // If its "trivial" but not a variable, we should substitute if we can.
    if (is_literal_type(rhs.head().type()) or is_constructor_exp(rhs))
	return true;

    // MultiSafe
    else if (occur.work_dup == amount_t::Once and occur.code_dup == amount_t::Many)
	return do_inline_multi(rhs, context);

    // OnceUnsafe
    else if (occur.work_dup == amount_t::Many and occur.code_dup == amount_t::Once)
	return whnf_or_bottom(rhs) and (no_size_increase(rhs,context) or not very_boring(context));

    // OnceUnsafe
    else if (occur.work_dup == amount_t::Once and occur.code_dup == amount_t::Once and occur.context == var_context::argument)
	return whnf_or_bottom(rhs) and (no_size_increase(rhs,context) or not very_boring(context));

    // MultiUnsafe
    else if (occur.work_dup == amount_t::Many and occur.code_dup == amount_t::Many)
	return whnf_or_bottom(rhs) and do_inline_multi(rhs, context);

    std::abort();
}

