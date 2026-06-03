#include "operations.H"
#include "computation/machine/args.H"
#include "computation/machine/graph_register.H"
#include "math/exponential.H"
#include "util/myexception.H"
#include "expression/index_var.H"
#include "expression/constructor.H"
#include "expression/lambda.H"
#include "expression/case.H"
#include "expression/var.H"
#include "expression/apply.H"
#include "util/string/join.H"

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
// Thus: Every instead of reference(slot) actually looks up the relevant reg, except for
//       the case operation, which instead uses reference to determine the branches.

string Let::print() const
{
    vector<string> bind_strings;
    for(auto& bind: binds)
        bind_strings.push_back(bind.print());
    return "let {"+join(bind_strings,";")+"} in "+body.print();
}

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

int get_n_lambdas(const expression_ref& E)
{
    expression_ref E2 = E;
    int n = 0;
    while(E2.head().type() == type_constant::lambda2_type)
    {
	E2 = E2.sub()[0];
	n++;
    }
    return n;
}

closure apply_op(OperationArgs& Args)
{
    closure C = Args.evaluate_slot_to_runtime_closure(0);
    int n_args_given = Args.n_args()-1;

    if (not C.exp.head().is_a<lambda2>())
	throw myexception()<<"Trying to apply non-lambda '"<<C.exp.head()<<"'";
    int n_args_needed = get_n_lambdas(C.exp);
    assert(n_args_needed >= 1);
    assert(n_args_given >= 1);

    int n_args_applied = std::min(n_args_given, n_args_needed);
    C.peel_lambdas(n_args_applied);

    for(int i=0;i<n_args_applied;i++)
    {
	int arg = Args.current_closure().runtime_reg_for_slot(i+1);
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
	vector<expression_ref> args;
        vector<Runtime::Exp> runtime_args;
	for(int i=n_args_needed;i<n_args_given;i++)
	{
	    int arg = Args.current_closure().runtime_reg_for_slot(i+1);
	    Env.push_back(arg);

            int index = n_args_given - i - 1;
	    args.push_back(index_var(index));
            runtime_args.push_back(Runtime::IndexVar(index));
	}
	expression_ref E2 = apply_expression( index_var(n_args_given - n_args_needed), args );
        Runtime::Exp runtime_E2 = Runtime::apply(Runtime::IndexVar(n_args_given - n_args_needed), runtime_args);
        assert(Runtime::to_expression_ref(runtime_E2) == E2);

        closure result;
        result.Env = std::move(Env);
        result.set_expression(std::move(runtime_E2));
	return result;
    }
}

static closure alts_op(const closure::Env_t& Env, const closure& object, const Expression::CaseAlts& alts, const Runtime::Case* runtime_case)
{
    int L = alts.size();
    assert(not runtime_case or runtime_case->alts.size() == L);

#ifndef NDEBUG
    vector<expression_ref> cases(L);
    vector<expression_ref> bodies(L);
    for(int i=0;i<L;i++)
    {
	cases[i]  = alts[i].pattern;
	bodies[i] = alts[i].body;
    }

    if (object.exp.head().is_a<lambda2>())
	throw myexception()<<"Case argument is a lambda '"<<make_case_expression(object.exp, cases, bodies)<<"'";
#endif

    closure result;
    result.Env = Env;

    for(int i=0;i<L and not result;i++)
    {
	const expression_ref& this_case = alts[i].pattern;
	const expression_ref& this_body = alts[i].body;

	// If its _, then match it.
	if (is_var(this_case))
	{
	    // We standardize to avoid case x of v -> f(v) so that f cannot reference v.
	    assert(is_wildcard(this_case));
	    assert(i == L-1);
      
            if (runtime_case)
            {
                assert(std::holds_alternative<Runtime::WildcardPattern>(runtime_case->alts[i].pattern));
                result.set_expression(runtime_case->alts[i].body);
            }
            else
                result.set_expression(Runtime::ObjectValue(this_body));
	}
	else
	{
	    // FIXME! Convert every pattern head to an integer...

	    // If we are a constructor, then match iff the the head matches.
	    if (object.exp.head() == this_case.head())
	    {
#ifndef NDEBUG
		if (object.exp.size())
		{
		    // The number of constructor fields is the same the for case pattern and the case object.
		    assert(object.exp.size() == object.exp.head().as_<constructor>().n_args());
		}
#endif	
                if (runtime_case)
                {
#ifndef NDEBUG
                    auto pattern = std::get_if<Runtime::ConstructorPattern>(&runtime_case->alts[i].pattern);
                    assert(pattern);
                    const auto& legacy_pattern = this_case.head().as_<constructor>();
                    assert(pattern->head.name() == legacy_pattern.name());
                    assert(pattern->head.n_args() == legacy_pattern.n_args());
#endif
                    result.set_expression(runtime_case->alts[i].body);
                }
                else
                    result.set_expression(Runtime::ObjectValue(this_body));
	
		for(int j=0;j<object.exp.size();j++)
		    result.Env.push_back( object.runtime_reg_for_slot(j) );
	    }
	}
    }

    if (not result)
#ifdef NDEBUG
	throw myexception()<<"Case: object '"<<object.exp<<"' doesn't match any alternative";
#else
        throw myexception()<<"Case: object '"<<object.exp<<"' doesn't match any alternative in '"<<make_case_expression(object.exp, cases, bodies)<<"'";
#endif

    // Trim the result.
    return get_trimmed(result);
}

bool is_seq(const expression_ref& E)
{
    assert(is_case(E));
    auto& alts = E.sub()[1].as_<Expression::CaseAlts>();
    return alts.size() == 1 and is_var(alts[0].pattern);
}

// Should we do this transformation before runtime?
closure seq_op(OperationArgs& Args, Runtime::Exp runtime_body = {})
{
    auto& alts = Args.reference(1).as_<Expression::CaseAlts>();
    assert(is_wildcard(alts[0].pattern));

    // Force x
    Args.evaluate_slot_force(0);

    // Get the current Env -- AFTER we force x, so GC can't invalidate it.
    closure result;
    result.Env = Args.current_closure().Env;
    if (runtime_body)
        result.set_expression(std::move(runtime_body));
    else
        result.set_expression(Runtime::ObjectValue(alts[0].body));

    // Trim the result.
    return get_trimmed( std::move(result) );
}

closure case_op(OperationArgs& Args)
{
    extern long total_case_op;
    total_case_op++;

    // Handle case x of _ -> E = x `seq` E
    {
        if (is_seq(Args.current_closure().exp))
        {
            Runtime::Exp runtime_body;
            if (auto runtime_case = Args.current_closure().runtime_expression().to<Runtime::Case>())
            {
                assert(runtime_case->alts.size() == 1);
                assert(std::holds_alternative<Runtime::WildcardPattern>(runtime_case->alts[0].pattern));
                runtime_body = runtime_case->alts[0].body;
            }

            return seq_op(Args, std::move(runtime_body));
        }
    }

    auto& in_object = Args.reference(0);
    Runtime::Case runtime_case;
    const Runtime::Case* runtime_case_ptr = nullptr;
    if (auto runtime_case_in = Args.current_closure().runtime_expression().to<Runtime::Case>())
    {
        runtime_case = *runtime_case_in;
        runtime_case_ptr = &runtime_case;
    }

    // Resizing of the memory can occur here, invalidating previously computed pointers
    // to closures.  The *index* within the memory shouldn't change, though.
    const closure object = is_eop_exp(in_object)
                          ? closure::legacy_expression(evaluate_e_op(Args, in_object))
                          : Args.evaluate_slot_to_runtime_closure(0);

    auto& C = Args.current_closure();

    // Therefore, we must compute this *after* we do the computation above, since
    // we're going to hold on to it.  Otherwise the held reference would become
    // *invalid* after the call above!
    auto& alts = Args.reference(1).as_<Expression::CaseAlts>();

    return alts_op(C.Env, object, alts, runtime_case_ptr);
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

    do
    {
	int start = C.Env.size();

	auto& L = C.exp.as_<Let>();
        const Runtime::Let* runtime_let = C.runtime_expression().to<Runtime::Let>();
        assert(not C.has_structured_runtime_expression() or runtime_let);

	int n_binds = L.binds.size();
        assert(not runtime_let or runtime_let->binds.size() == n_binds);

	// 1. Allocate the new vars on the heap
	for(int i=0;i<n_binds;i++)
	    C.Env.push_back( Args.allocate_reg() );
      
	// 2. Substitute the new heap vars for the var vars in expression T and in the bodies
        if (runtime_let)
        {
            Runtime::Let runtime_L = *runtime_let;

            for(int i=0;i<n_binds;i++)
            {
                closure bind;
                bind.Env = C.Env;
                bind.set_expression(runtime_L.binds[i]);
                M.set_C(C.Env[start+i], get_trimmed(std::move(bind)));
            }

            C.set_expression(runtime_L.body);
        }
        else
        {
            for(int i=0;i<n_binds;i++)
                M.set_C(C.Env[start+i], get_trimmed(closure::legacy_expression(L.binds[i], C.Env)));

            C.set_expression(Runtime::ObjectValue(L.body));
        }
	do_trim(C);
    }
    while (C.exp.head().type() == type_constant::let2_type);

    return C;
}
