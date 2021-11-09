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
#include "computation/expression/AST_node.H"
#include "computation/expression/expression.H" // for is_WHNF( )
#include "occurrence.H"
#include "inliner.H"
#include "simplifier.H"
#include "util/assert.hh"

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

shared_ptr<const case_context> make_case_context(const expression_ref E, const inline_context& context)
{
    assert(is_case(E));
    return std::make_shared<const case_context>(E.sub()[1], context);
}

shared_ptr<const apply_context> make_apply_context_one_arg(const expression_ref arg, const inline_context& context)
{
    return std::make_shared<const apply_context>(arg, context);
}

shared_ptr<const apply_context> make_apply_context(const expression_ref E, inline_context context)
{
    assert(E.head().is_a<Apply>());
    auto A = make_apply_context_one_arg(E.sub().back(), context);
    for(int i=E.size()-2;i>=1;i--)
	A = make_apply_context_one_arg(E.sub()[i], inline_context(A));
    return A;
}

shared_ptr<const stop_context> make_stop_context()
{
    return std::make_shared<const stop_context>();
}

int nodes_size(const expression_ref& E)
{
    int total = 1;

    if (E.is_expression())
	for(const auto& e:E.sub())
	    total += nodes_size(e);

    return total;
}

int simple_size(const expression_ref& E)
{
    if (is_var(E))
	return 0;

    else if (is_apply_exp(E))
    {
	int n_args = (int)E.size()-1;
	assert(n_args > 0);
	return simple_size(E.sub()[0]) + n_args;
    }

    else if (is_lambda_exp(E))
	return simple_size(E.sub()[1]);

    else if (is_let_expression(E))
    {
        auto& L = E.as_<let_exp>();
	int size = simple_size(L.body);

	for(auto& [x,e]: L.binds)
	    size += simple_size(e);

	return size;
    }
    else if (is_case(E))
    {
	expression_ref object;
	vector<expression_ref> patterns;
	vector<expression_ref> bodies;
	parse_case_expression(E, object, patterns, bodies);
	int alts_size = simple_size(bodies[0]);
	for(int i=1;i<bodies.size();i++)
	    alts_size = std::max(alts_size, simple_size(bodies[i]));
	return 1 + simple_size(object) + alts_size;
    }
    else if (is_non_apply_op_exp(E))
    {
	for(auto& x: E.sub())
	    assert(is_var(x));
	return 1;
    }
    else if (E.size() == 0)
	return 1;
    else if (is_constructor_exp(E))
    {
	for(auto& x: E.sub())
	    assert(is_var(x));
	return 1;
    }

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

bool no_size_increase(const expression_ref& rhs, const inline_context& context)
{
    // If rhs is a variable, then there's no size increase
    if (is_trivial(rhs)) return true;

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
	int n_args_needed = get_n_lambdas1(rhs);
	int n_args_used = std::min(n_args_needed, n_args_supplied);

	int size_of_call = 1 + n_args_supplied;
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


bool boring(const expression_ref& rhs, const inline_context& context)
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

bool small_enough(const simplifier_options& options,const expression_ref& rhs, const inline_context& context)
{
    double body_size = simple_size(rhs);

    double size_of_call = 1 + num_arguments(context);

    int discounts = 0;

    return (body_size - size_of_call - options.keenness*discounts <= options.inline_threshhold);
}

bool do_inline_multi(const simplifier_options& options, const expression_ref& rhs, const inline_context& context)
{
    if (no_size_increase(rhs,context)) return true;

    if (boring(rhs,context)) return false;

    return small_enough(options, rhs,context);
}

bool evaluates_to_bottom(const expression_ref& /* rhs */)
{
    return false;
}

bool whnf_or_bottom(const expression_ref& rhs)
{
    return is_WHNF(rhs) or evaluates_to_bottom(rhs);
}

bool do_inline(const simplifier_options& options, const expression_ref& rhs, const occurrence_info& occur, const inline_context& context)
{
    // LoopBreaker
    if (occur.is_loop_breaker)
	return false;

    // Function and constructor arguments
    else if (context.is_stop_context() and not is_trivial(rhs))
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
    if (is_literal_type(rhs.type()) or is_constructor(rhs))
	return true;

    // MultiSafe
    else if (occur.work_dup == amount_t::Once and occur.code_dup == amount_t::Many)
	return do_inline_multi(options, rhs, context);

    // OnceUnsafe
    else if (occur.work_dup == amount_t::Many and occur.code_dup == amount_t::Once)
	return whnf_or_bottom(rhs) and (no_size_increase(rhs,context) or not very_boring(context));

    // OnceUnsafe
    else if (occur.work_dup == amount_t::Once and occur.code_dup == amount_t::Once and occur.context == var_context::argument)
	return whnf_or_bottom(rhs) and (no_size_increase(rhs,context) or not very_boring(context));

    // MultiUnsafe
    else if (occur.work_dup == amount_t::Many and occur.code_dup == amount_t::Many)
	return whnf_or_bottom(rhs) and do_inline_multi(options, rhs, context);

    std::abort();
}

