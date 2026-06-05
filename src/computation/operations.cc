#include "operations.H"
#include "computation/machine/args.H"
#include "computation/machine/graph_register.H"
#include "computation/runtime/ast.H"
#include "math/exponential.H"
#include "util/myexception.H"
#include <type_traits>

using std::vector;
using std::string;

// Q: When is there a benefit to preserving the seq operation, instead of just
//    returning a reference to arg #2?
// A: Firstly, the benefit we are talking about is entirely a space benefit.
//    This only occurs when pre-evaluating arg#1 decreases the size of the
//    expression tree stored in memory.
//
//    Now, if we are re-running the computation because arg#2 is changeable,
//    then we might simplify some subtree of arg#1 that was an unevaluated
//    branch the first time around, *if* arg#1 is changeable and arg#2 is
//    changeable.
//
//    However, in order to allow chains of seq to be eliminated, lets make
//    seq x y just evaluate to y, instead of copying y's result.  (The cost
//    of this is that any additional space cost that could be eliminated
//    during a *second* evaluate of x (from a second evaluation of seq x y)
//    will not be eliminated through a seq.
//
//    So, basically, seqs will always be unchangeable, and therefore will
//    always be eliminated in favor of index_var references to y.
//

// NOTE: (a) Both seq and $ need to look up the memory location of one of their arguments.
//       (b) Both seq and $ avoid evaluating the reg at this location.
// Thus: seq and $ look up the relevant regs, while case examines Runtime::Case
//       alternatives directly.

using boost::dynamic_pointer_cast;

// Multi-argument lambda-calculus reduction rules.
//
// 1.   /\x[1]...x[n].A             =>   /\x[1]...x[n].A
// 2. @ /\x[1]...x[n].A p[1]...p[m] => @ /\x[1]...x[2].A p[1]...p[m]            if 1<=m<n
// 3. @ /\x[1]...x[n].A p[1]...p[m] => A{x[i]->p[i]}                            if 1<=m=n
// 4. @ /\x[1]...x[n].A p[1]...p[m] => let y=A{x[i]->p[i]} in y p[n+1] ... p[m] if 1<=n<m
//
// 5. @ (@ /\x[1]...x[n].A p[1]...p[m]) q[1]...q[o] => @ /\x[1]...x[n].A p[1]...p[m] ++ q[1]...q[o]
//
// 6. @ B p[1]...p[m] => B' [1]...p[m]  (if B => B').
//
// Note that the @ in rules #2 and #5 is a PAP (partial application).

/* To perform a multi-argument apply in the current framework, we
   don't need to worry about adding PAP nodes, since partially applied functions
   are fine.  We just need to remove n levels of lambdas if there are n levels to remove.

   We could in theory recognize when a function is an apply.  If it doesn't have enough arguments
   we could then add some.  However, the simplest thing to do would just be to evaluate it,
   thus applying any arguments that it has.  We can then add ours to the list.

   @ x y1 y2 y3 ... yn
*/

closure apply_op(OperationArgs& Args)
{
    closure C = Args.evaluate_slot_to_closure(0);
    int n_args_given = Args.n_args()-1;

    assert(C.has_code());
    int n_args_needed = Runtime::count_lambdas(C.get_code());
    if (n_args_needed == 0)
	throw myexception()<<"Trying to apply non-lambda '"<<C.get_code()<<"'";
    assert(n_args_needed >= 1);
    assert(n_args_given >= 1);

    int n_args_applied = std::min(n_args_given, n_args_needed);
    C.set_code(Runtime::peel_lambdas(C.get_code(), n_args_applied));
    for(int i=0;i<n_args_applied;i++)
    {
	int arg = Args.current_closure().reg_for_slot(i+1);
	C.Env.push_back(arg);
    }

    // 1. We can apply all the args
    if (n_args_given <= n_args_needed)
	return C;

    // 2. We can only apply some of the args
    else
    {
	int new_head_ref = Args.allocate(std::move(C));
	closure::Env_t Env = {new_head_ref};
	for(int i=n_args_needed;i<n_args_given;i++)
	{
	    int arg = Args.current_closure().reg_for_slot(i+1);
	    Env.push_back(arg);
	}

        vector<Runtime::Exp> runtime_args;
        for(int i=n_args_needed;i<n_args_given;i++)
            runtime_args.push_back(Runtime::IndexVar(n_args_given - i - 1));

        return closure(Runtime::apply(Runtime::IndexVar(n_args_given - n_args_needed),
                                      std::move(runtime_args)),
                       Env);
    }
}

static const Runtime::ConstructorTag* constructor_value(const Runtime::Exp& E)
{
    if (auto c = E.to<Runtime::Constructor>())
        return &c->value;
    else if (auto app = E.to<Runtime::App>())
    {
        if (auto constructor_app = std::get_if<Runtime::ConstructorApp>(&app->head))
            return &constructor_app->head;
    }

    return nullptr;
}

static int constructor_n_args(const Runtime::Exp& E)
{
    if (auto c = E.to<Runtime::Constructor>())
        return c->value.n_args();
    else if (auto app = E.to<Runtime::App>())
    {
        if (std::holds_alternative<Runtime::ConstructorApp>(app->head))
            return app->args.size();
    }

    std::abort();
}

static bool matches_pattern(const closure& object, const Runtime::Pattern& pattern)
{
    return std::visit([&](const auto& p) -> bool
    {
        using T = std::decay_t<decltype(p)>;

        if constexpr (std::is_same_v<T, Runtime::WildcardPattern>)
            return true;
        else if constexpr (std::is_same_v<T, Runtime::ConstructorPattern>)
        {
            const Runtime::ConstructorTag* c = constructor_value(object.get_code());
            return c and c->name() == p.head.name() and c->n_args() == p.head.n_args();
        }
    }, pattern);
}

static closure alts_op(OperationArgs& Args, const closure::Env_t& Env, const closure& object, const Runtime::Case& runtime_case)
{
    int L = runtime_case.alts.size();

#ifndef NDEBUG
    if (object.get_code().to<Runtime::Lambda>())
	throw myexception()<<"Case argument is a lambda in '"<<Runtime::print(Runtime::Exp(runtime_case))<<"'";
#endif

    closure result;
    result.Env = Env;

    for(int i=0;i<L and not result;i++)
    {
        if (matches_pattern(object, runtime_case.alts[i].pattern))
        {
#ifndef NDEBUG
            if (auto object_constructor = constructor_value(object.get_code()))
                assert(constructor_n_args(object.get_code()) == object_constructor->n_args());
#endif	
            result.set_code(runtime_case.alts[i].body);

            int n_args = Runtime::pattern_arity(runtime_case.alts[i].pattern);
            for(int j=0;j<n_args;j++)
            {
                auto field = object.slot(j);
                if (auto reg_ref = field.to<Runtime::RegRef>())
                    result.Env.push_back(reg_ref->target);
                else
                    result.Env.push_back(Args.allocate(closure(std::move(field))));
            }
        }
    }

    if (not result)
#ifdef NDEBUG
	throw myexception()<<"Case: object '"<<object.get_code()<<"' doesn't match any alternative";
#else
        throw myexception()<<"Case: object '"<<object.get_code()<<"' doesn't match any alternative in '"<<Runtime::print(Runtime::Exp(runtime_case))<<"'";
#endif

    // Trim the result.
    return get_trimmed(result);
}

bool is_seq(const Runtime::Case& C)
{
    return C.alts.size() == 1 and std::holds_alternative<Runtime::WildcardPattern>(C.alts[0].pattern);
}

// Should we do this transformation before runtime?
closure seq_op(OperationArgs& Args, const Runtime::Case& runtime_case)
{
    assert(is_seq(runtime_case));
    Runtime::Exp runtime_body = runtime_case.alts[0].body;

    // Force x
    Args.evaluate_code_force(runtime_case.object);

    // Get the current Env -- AFTER we force x, so GC can't invalidate it.
    closure result;
    result.Env = Args.current_closure().Env;
    result.set_code(std::move(runtime_body));

    // Trim the result.
    return get_trimmed( std::move(result) );
}

closure case_op(OperationArgs& Args)
{
    extern long total_case_op;
    total_case_op++;

    assert(Args.current_closure().has_code());
    auto runtime_case_ptr = Args.current_closure().get_code().to<Runtime::Case>();
    assert(runtime_case_ptr);
    Runtime::Case runtime_case = *runtime_case_ptr;

    // Handle case x of _ -> E = x `seq` E
    {
        if (is_seq(runtime_case))
            return seq_op(Args, runtime_case);
    }

    const auto& in_object = runtime_case.object;

    // Resizing of the memory can occur here, invalidating previously computed pointers
    // to closures.  The *index* within the memory shouldn't change, though.
    const closure object = is_eop_exp(in_object) ? closure(evaluate_e_op(Args, in_object)) : Args.evaluate_code_to_closure(in_object);
    closure::Env_t Env = Args.current_closure().Env;

    return alts_op(Args, Env, object, runtime_case);
}

/*
 * Let's are not really 'changeable', but we'd like to stop replacing (contingent) let's with their call.
 * - instead create a 'step' for them, and record created regs.
 * - we'd also like to avoid creating a new reg for the let body.
 *
 * Hmm... suppose we have let x=1;y=2 in x+y
 * - this does perform allocation.
 * - however, it is not 'changeable', so it shouldn't make the input to anything else changeable.
 * - although there will be a 'step', that step can never be invalidated by changed inputs.  So it would be valid in all contexts. 
 * - however, ultimately the step should be present in a context precisely if the reg is executed in that context.
 * - furthermore, we'd like to update the reg with the value '3', as we do currently... what else would we do?
 * - it seems that we would need to AVOID this for contingent regs.
 * - if we garbage-collect both x and y, then we could actually replace the reg by its value.
 *
 * Part of the goal here is to handle infinite lists, where the amount we examine depends on a variable.
 * - we want to deallocate parts of the list that are no longer accessed.
 *
 * Then there is optimization, we do not want to allocate a new reg for the 'call' when executing a let:
 * - perform the let's and then continue executing the let body 
 *
 * Then there is the issue of contingent execution.
 * - If the execution is non-contingent, then we would actually like to update the reg 
 *
 * Update: 8/26/2017 - So, currently when we have E=let DECLS in changeable, we perform the let, and then 
 *                     update E to E=changeable.  Then if changeable changes, we do not need to re-perform the let.
 *                     If we merge the let and the changeable, then we are basically keepint E=let Decls in changeable,
 *                     but not creating a call from E=let Decls in changeable to F=changeable.  However, this merger is still
 *                     bad, because it means that if F changes, then we re-allocate the Decls, which unshares them, and also
 *                     re-allocates them.
 */

closure let_op(OperationArgs& Args)
{
    extern long total_let_op;
    total_let_op++;

    reg_heap& M = Args.memory();

    auto C  = Args.current_closure();

    while (true)
    {
	int start = C.Env.size();

        assert(C.has_code());
        if (auto runtime_let = C.get_code().to<Runtime::Let>())
        {
            int n_binds = runtime_let->binds.size();

            // 1. Allocate the new vars on the heap
            for(int i=0;i<n_binds;i++)
                C.Env.push_back( Args.allocate_reg() );

            // 2. Substitute the new heap vars for the var vars in expression T and in the bodies
            for(int i=0;i<n_binds;i++)
                M.set_C(C.Env[start+i], get_trimmed(closure(runtime_let->binds[i], C.Env)));

            C.set_code(runtime_let->body);
            do_trim(C);
            continue;
        }

        break;
    }

    return C;
}
