//#ifdef NDEBUG
//#undef NDEBUG
//#endif

#include "util/log-level.H"
#include "graph_register.H"
#include "effect.H"
#include "error_exception.H"
#include "computation/expression/expression.H" // is_WHNF( )
#include "computation/expression/let.H"
#include "computation/expression/trim.H"
#include "computation/expression/index_var.H"
#include "computation/operations.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;

#include "args.H"

long total_reductions = 0;
long total_changeable_reductions = 0;
long total_changeable_eval = 0;
long total_changeable_eval_with_result = 0;
long total_changeable_eval_with_call = 0;

long total_reductions2 = 0;
long total_changeable_reductions2 = 0;
long total_changeable_eval2 = 0;
long total_changeable_eval_with_result2 = 0;
long total_changeable_eval_with_call2 = 0;

long total_case_op = 0;
long total_let_op = 0;
long total_index_op = 0;

expression_ref compact_graph_expression(const reg_heap& C, int R, const map<string, int>&);
expression_ref untranslate_vars(const expression_ref& E, const map<std::string, int>& ids);
expression_ref untranslate_vars(const expression_ref& E, const map<int,string>& ids);
map<int,string> get_constants(const reg_heap& C, int t);

void throw_reg_exception(reg_heap& M, int t, const closure& C, myexception& e)
{
    string SSS = unlet(untranslate_vars(
                           untranslate_vars(deindexify(trim_unnormalize(C)), M.get_identifiers()),
                           get_constants(M,t)
                           )
        ).print();
    std::ostringstream o;
    o<<"evaluating: "<<SSS<<"\n\n";
    e.prepend(o.str());
    throw e;
}

void throw_reg_exception(reg_heap& M, int t, int R, myexception& e, bool changeable)
{
    string SSS = unlet(untranslate_vars(
                           untranslate_vars(deindexify(trim_unnormalize(M[R])), M.get_identifiers()),
                           get_constants(M,t)
                           )
        ).print();
    std::ostringstream o;
    o<<"evaluating reg # "<<R;
    if (not changeable)
        o<<" (unchangeable)";
    o<<": "<<SSS<<"\n\n";
    e.prepend(o.str());
    throw e;
}

/// These are LAZY operation args! They don't evaluate arguments until they are evaluated by the operation (and then only once).
class RegOperationArgs1 final: public OperationArgs
{
    const int s;

    const bool first_eval;

    bool evaluate_changeables() const override {return true;}

    /// Evaluate the reg r2, record dependencies, and return the reg following call chains.
    int evaluate_reg_force(int r2) override
        {
            auto [r3, result] = M.incremental_evaluate1(r2);

            if (M.reg_is_changeable_or_forcing(r3))
            {
                if (first_eval)
                    M.set_forced_reg(r, r3);
            }

            return result;
        }

    /// Evaluate the reg r2, record a dependency on r2, and return the reg following call chains.
    int evaluate_reg_use(int r2) override
        {
            // Compute the value, and follow index_var chains (which are not changeable).
            auto [r3, result] = M.incremental_evaluate1(r2);

            // Note that although r2 is newly used, r3 might be already used if it was 
            // found from r2 through a non-changeable reg_var chain.
            if (M.reg_is_to_changeable(r3))
            {
                make_changeable();
                if (first_eval)
                    M.set_used_reg(r, r3);
            }
            else if (M.reg_is_changeable_or_forcing(r3))
            {
                if (first_eval)
                    M.set_forced_reg(r, r3);
            }

            return result;
        }

public:

    bool used_changeable = false;

    void make_changeable() override
    {
        used_changeable = true;
	creator_step = s;
    }

    // If we unreference regs that evaluate to a variable, then we unreference p->let q=2 in q
    // and point references to q instead of p.  But then it would not be true that a variable can
    // only be referenced if the slot that created it is still referenced.

    void set_effect(int r) override
        {
            make_changeable();
            M.mark_step_with_effect(s);
            M._register_effect_at_reg(r, s);
        }

    RegOperationArgs1(int r_, int s_, reg_heap& m)
        :OperationArgs(m, r_), s(s_), first_eval(m.reg_is_unevaluated(r))
        { }
};

/// Evaluate r and look through index_var chains to return the first reg that is NOT a reg_var.
/// The returned reg is guaranteed to be (a) in WHNF (a lambda or constructor) and (b) not an reg_var.
pair<int,int> reg_heap::incremental_evaluate1(int r)
{
    assert(execution_allowed_at_root());

#ifndef NDEBUG
    if (reg_is_on_stack(r))
        throw myexception()<<"Evaluating reg "<<r<<" that is already on the stack!";
#endif
    regs[r].flags.set(reg_is_on_stack_bit);
    stack.push_back(r);

    auto result = incremental_evaluate1_(r);
    assert(not reg_is_index_var_no_force(result.first));
    assert(not reg_is_unevaluated(result.first));
    assert(not reg_is_unevaluated(r));
    assert(reg_is_on_stack(r));

    stack.pop_back();
    regs[r].flags.reset(reg_is_on_stack_bit);

    return result;
}

vector<expression_ref> e_value_stack;

expression_ref evaluate_e_op(OperationArgs& Args, const expression_ref& E)
{
    int initial_size = e_value_stack.size();

    // Can we stop referencing the current closure?
    // The problem is that after each evaluation, closure pointers might be modified
    //   to point to a non-index-var.
    // If we could assume that closures that are currently being executed will not
    //   be modified, that would be helpful for writing simpler code I expect.
    int n_args = E.size();
    auto f = E.head().as_ptr_to<Operation>()->e_op;

    // Reserve space for the n_args arguments.
    e_value_stack.resize(initial_size + n_args);

    // Evaluate the arguments in left-to-right order.
    for(int i=0;i<n_args;i++)
    {
        auto arg = E.sub()[i];
        if (arg.is_reg_var())
        {
            int r_arg = arg.as_reg_var();

            arg = Args.evaluate_reg_to_object( r_arg );
        }
        else if (arg.is_index_var())
        {
            // In theory r_arg could change if r_i initially points to an index_var.
            int r_i = arg.as_index_var();
            int r_arg = lookup_in_env(Args.current_closure().Env, r_i);

            arg = Args.evaluate_reg_to_object( r_arg );
        }
        else if (auto O = arg.head().to<Operation>(); O and O->e_op)
        {
            arg = evaluate_e_op(Args, arg);
        }
        else
        {
            assert(arg.is_double() or arg.is_int() or arg.is_char() or arg.is_a<Integer>() or arg.is_a<String>());
        }

        e_value_stack[initial_size+n_args-1-i] = arg;
    }

    // Any new args pushed only the stack by evaluating arguments should be popped before we get here.
    assert(e_value_stack.size() == initial_size + n_args);

    // Compute the result.
    expression_ref result = f(e_value_stack);

    // After evaluating f, the n_args arguments that we added should also be popped.
    assert(e_value_stack.size() == initial_size);

    return result;
}

closure evaluate_e_op_to_c(OperationArgs& Args)
{
    // Make a copy here because the location of Args.current_closure() can change if the heap grows.
    auto E = Args.current_closure().exp;
    return evaluate_e_op(Args, E);
}

pair<int,int> reg_heap::incremental_evaluate1_(int r)
{
    assert(regs.is_valid_address(r));
    assert(regs.is_used(r));

#ifndef NDEBUG
    if (reg_has_value(r))
    {
        expression_ref E = access_value_for_reg(r).exp;
        assert(is_WHNF(E));
        assert(not E.head().is_a<expression>());
        assert(not reg_is_index_var_no_force(r));
        assert(not reg_is_unevaluated(r));
    }
    if (unevaluated_reg_is_index_var_no_force(r))
        assert(not has_result1(r));
#endif

    if (reg_is_constant(r)) return {r,r};

    else if (reg_is_changeable(r))
    {
        total_changeable_eval++;
        int result = result_for_reg(r);

        if (result > 0)
        {
            total_changeable_eval_with_result++;

            return {r, result};
        }

        // If we know what to call, then call it and use it to set the value
        if (int s = step_index_for_reg(r); s > 0)
        {
            // Evaluate S, looking through unchangeable redirections
            auto [call, result] = incremental_evaluate1(steps[s].call);

	    // If the back-edge has not been set, then we need to update the call to look through
	    // index-var-no-force and set the back-edge on the first non-index-var if it is changeable.
            if (not steps[s].call_edge)
            {
                // This should only ever happen for modifiable values set with set_reg_value( ).
                // In such cases, we need this, because the value we set could evaluate to an index_var.
                // Otherwise, we set the call AFTER we have evaluated to a changeable or a constant.
                clear_call_for_reg(r);
                set_call(s, call);
            }
	    else
		assert(call == call_for_reg(r));

            // r gets its value from S.
            set_result_for_reg( r );
            if (not tokens[root_token].children.empty())
            {
                int t = tokens[root_token].children[0];
                tokens[t].vm_result.add_value(r, non_computed_index);
            }

            total_changeable_eval_with_call++;
            assert(not reg_is_unevaluated(r));
            return {r, result};
        }
    }
    else if (unevaluated_reg_is_index_var_no_force(r))
    {
        int r2 = closure_at(r).reg_for_index_var();
        return incremental_evaluate1(r2);
    }
    else if (reg_is_index_var_with_force(r))
    {
	// We don't have to force the forced regs in evaluate1.
	int r2 = closure_at(r).reg_for_index_var();
	auto [r3, result3] = incremental_evaluate1(r2);

	assert(not reg_is_unevaluated(r));
	return {r, result3};
    }

    while (1)
    {
        assert(expression_at(r));

#ifndef NDEBUG
        //    std::cerr<<"   statement: "<<r<<":   "<<regs[r].E.print()<<std::endl;
#endif

        /*---------- Below here, there is no call, and no value. ------------*/
        if (expression_at(r).is_index_var())
        {
            assert( not reg_is_changeable(r) );

            assert( not has_result1(r) );

            assert( not has_step1(r) );

            int r2 = closure_at(r).reg_for_index_var();

            auto [r3, result] = incremental_evaluate1(r2);

            // Update the index_var to point to the end of the index_var_no_force chain.
            if (r2 != r3) regs[r].C.Env[0] = r3;

            if (regs[r].forced_regs.empty())
            {
                mark_reg_index_var_no_force(r);
                return {r3,result};
            }

	    int r4 = follow_index_var(r3);
	    if (r3 != r4) regs[r].C.Env[0] = r4;

	    mark_reg_index_var_with_force(r);

            if (not reg_is_constant(r3))
		set_forced_reg(r,r3);

            assert(not has_result1(r));

            assert(not prog_unshare[r].test(unshare_result_bit));
            assert(not reg_is_unevaluated(r));
            return {r, result};
        }

        // Check for WHNF *OR* heap variables
        else if (is_WHNF(expression_at(r)))
        {
            assert(not expression_at(r).is_a<Operation>());
            if (regs[r].forced_regs.empty())
	    {
                mark_reg_constant(r);

		assert( not has_step1(r) );
		assert( not has_result1(r) );
		assert( not reg_is_unevaluated(r) );

		return {r,r};
	    }
	    else
	    {
		// Allocate a new reg
		int r2 = allocate();
		auto creator_step = creator_step_for_reg(r);
		if (creator_step)
		    mark_reg_created_by_step(r2, *creator_step);

		// Copy the constant from r -> r2
		closure C2 = closure_at(r);  // do this after allocation - allocation could modify the environment of C2!
		set_C(r2, std::move(C2));

		// Make the current reg into an index-var that points to it.
		closure C1(index_var(0),{r2});
		set_C(r, std::move(C1));
	    }

        }

#ifndef NDEBUG
        else if (expression_at(r).head().is_a<Trim>())
            std::abort();
#endif

        // 3. Reduction: Operation (includes @, case, +, etc.)
        else
        {
            assert( not has_step1(r) );
            // The only we reason we are getting this here is to store created_regs on it,
            // if we perform allocations AFTER using/forcing something changeable.
            int s = get_shared_step(r);

            try
            {
                RegOperationArgs1 Args(r, s, *this);
                auto O = expression_at(r).head().as_ptr_to<Operation>()->op;
                closure value = (*O)(Args);
                total_reductions++;

                // If the reduction doesn't depend on modifiable, then replace E with the value.
                if (not Args.used_changeable)
                {
                    assert( not reg_has_call(r) );
                    assert( not has_result1(r) );
                    assert( regs[r].used_regs.empty() );
                    assert( steps[s].created_regs.empty() ); // Any allocations should have gone to creator_step
                    set_C( r, std::move(value) );
                    steps.reclaim_used(s);
                }
                else
                {
                    total_changeable_reductions++;
                    mark_reg_changeable(r);

                    int r2;
                    if (value.exp.is_index_var())
                    {
                        r2 = value.reg_for_index_var();
                    }
                    else
                    {
                        r2 = Args.allocate( std::move(value) ) ;
                        assert(regs[r2].created_by_step.value().first == s);
                        assert(not has_step1(r2));
                    }

                    auto [call,result] = incremental_evaluate1(r2);

                    set_call(s, call);

                    prog_steps[r] = s;
                    set_result_for_reg(r);

                    if (not tokens[root_token].children.empty())
                    {
                        int t = tokens[root_token].children[0];
                        tokens[t].vm_result.add_value(r, non_computed_index);
                        tokens[t].vm_step.add_value(r, non_computed_index);
                    }

                    assert(not reg_is_unevaluated(r));
                    return {r, result};
                }
            }
            catch (error_exception& e)
            {
                if (log_verbose)
                    throw_reg_exception(*this, root_token, r, e, true);
                else
                    throw;
            }
            catch (myexception& e)
            {
                throw_reg_exception(*this, root_token, r, e, true);
            }
            catch (const std::exception& ee)
            {
                myexception e;
                e<<ee.what();
                throw_reg_exception(*this, root_token, r, e, true);
            }
        }
    }

    std::cerr<<"incremental_evaluate: unreachable?";
    std::abort();
}

/// Evaluate regs when called from a changeable reg.
/// Since the reg has already been marked changable, dependencies have already been recorded.
/// Used regs and forced regs should already be up-to-date, having been forced by force_regs_check_same_inputs( ).
class RegOperationArgs2Changeable final: public OperationArgs
{
    const int s;

    bool evaluate_changeables() const {return true;}

    /// We don't need to evaluate r2 or record dependencies -- this should already have happened.
    int evaluate_reg_force(int r2)
        {
            r2 = M.follow_index_var_no_force(r2);

            assert(M.reg_is_constant(M.follow_index_var_target(r2)) or M.has_result2(M.follow_index_var_target(r2)));

            return M.value_for_reg(r2);
        }

    /// We don't need to evaluate r2 or record dependencies -- this should already have happened.
    int evaluate_reg_use(int r2)
        {
            r2 = M.follow_index_var_no_force(r2);

            assert(M.reg_is_constant(M.follow_index_var_target(r2)) or M.has_result2(M.follow_index_var_target(r2)));

            return M.value_for_reg(r2);
        }

public:

    void make_changeable() {}

    // If we unreference regs that evaluate to a variable, then we unreference p->let q=2 in q
    // and point references to q instead of p.  But then it would not be true that a variable can
    // only be referenced if the slot that created it is still referenced.

    void set_effect(int r)
        {
            memory().mark_step_with_effect(s);
            M._register_effect_at_reg(r, s);
        }

    RegOperationArgs2Changeable(int r_, int s_, reg_heap& m)
        :OperationArgs(m, r_), s(s_)
        {
	    creator_step = s;
        }
};

/// These are LAZY operation args! They don't evaluate arguments until they are evaluated by the operation (and then only once).
class RegOperationArgs2Unevaluated final: public OperationArgs
{
    const int s;

    bool evaluate_changeables() const override {return true;}

    /// Evaluate the reg r2, record dependencies, and return the reg following call chains.
    int evaluate_reg_force(int r2) override
        {
            auto [r3, result] = M.incremental_evaluate2(r2, false);

            if (M.reg_is_changeable_or_forcing(r3))
	    {
                int r4 = M.set_forced_reg(r, r3);

                // Case 1: r4 == r3 -> new force edge to r3!
                // Case 2: r3 != r4, r3 is forced -> new force edge to r4!
                // Case 3: r3 != r4  r3 is not forced -> new force edge to r4, force edge from r3->r4 stops counting.
                if (r4 == r3 or M.reg_is_forced(r3))
                    M.inc_count(r4);
                else
                    assert(M.reg_is_forced(r4));
	    }

            return result;
        }

    /// Evaluate the reg r2, record a dependency on r2, and return the reg following call chains.
    int evaluate_reg_use(int r2) override
        {
            // Compute the value, and follow index_var chains (which are not changeable).
            auto [r3, result] = M.incremental_evaluate2(r2, false);

            // Note that although r2 is newly used, r3 might be already used if it was 
            // found from r2 through a non-changeable reg_var chain.
            if (M.reg_is_to_changeable(r3))
            {
                make_changeable();
                M.inc_count(r3);
                M.set_used_reg(r, r3);
            }
            else if (M.reg_is_changeable_or_forcing(r3))
	    {
                int r4 = M.set_forced_reg(r, r3);

                // Case 1: r4 == r3 -> new force edge to r3!
                // Case 2: r3 != r4, r3 is forced -> new force edge to r4!
                // Case 3: r3 != r4  r3 is not forced -> new force edge to r4, force edge from r3->r4 stops counting.
                if (r4 == r3 or M.reg_is_forced(r3))
                    M.inc_count(r4);
                else
                    assert(M.reg_is_forced(r4));
	    }

            return result;
        }

public:

    bool used_changeable = false;

    void make_changeable() override
    {
        used_changeable = true;
	creator_step = s;
    }

    // If we unreference regs that evaluate to a variable, then we unreference p->let q=2 in q
    // and point references to q instead of p.  But then it would not be true that a variable can
    // only be referenced if the slot that created it is still referenced.

    void set_effect(int r) override
        {
            make_changeable();
            memory().mark_step_with_effect(s);
            M._register_effect_at_reg(r, s);
        }

    RegOperationArgs2Unevaluated(int r_, int s_, reg_heap& m)
        :OperationArgs(m, r_), s(s_)
        {
            assert(not M.reg_is_forced(r));
        }
};

int reg_heap::inc_count(int r)
{
    assert(reg_is_changeable_or_forcing(r));
    assert(tokens[root_token].children.size() == 1);

    if (not prog_unshare[r].test(unshare_count_bit))
    {
        prog_unshare[r].set(unshare_count_bit);
        int t = tokens[root_token].children[0];
        tokens[t].vm_force_count.add_value(r, prog_force_counts[r]);
    }

    int before = prog_force_counts[r];
    assert(before >= 0);
    prog_force_counts[r]++;
    return before;
}

int reg_heap::dec_count(int r)
{
    assert(reg_is_changeable_or_forcing(r));
    assert(tokens[root_token].children.size() == 1);

    if (not prog_unshare[r].test(unshare_count_bit))
    {
        prog_unshare[r].set(unshare_count_bit);
        int t = tokens[root_token].children[0];
        tokens[t].vm_force_count.add_value(r, prog_force_counts[r]);
    }

    prog_force_counts[r]--;
    int end = prog_force_counts[r];
    assert(end >= 0);
    return end;
}

/// Evaluate r and look through index_var chains to return the first reg that is NOT a reg_var.
/// The returned reg is guaranteed to be (a) in WHNF (a lambda or constructor) and (b) not an reg_var.
pair<int,int> reg_heap::incremental_evaluate2(int r, bool do_count)
{
    assert(execution_allowed_at_root());

#ifndef NDEBUG
    if (reg_is_on_stack(r))
        throw myexception()<<"Evaluating reg "<<r<<" that is already on the stack!";
#endif
    regs[r].flags.set(reg_is_on_stack_bit);
    stack.push_back(r);

    auto result = incremental_evaluate2_(r);
    assert(not reg_is_index_var_no_force(result.first));
    assert(not reg_is_unevaluated(result.first));
    assert(not reg_is_unevaluated(r));

    assert(reg_is_on_stack(r));
    stack.pop_back();
    regs[r].flags.reset(reg_is_on_stack_bit);

    int r2 = result.first;
    if (do_count and reg_is_changeable_or_forcing(r2))
        inc_count(r2);
    return result;
}

pair<int,int> reg_heap::incremental_evaluate2_(int r)
{
    assert(regs.is_valid_address(r));
    assert(regs.is_used(r));

#ifndef NDEBUG
    if (reg_has_value(r))
    {
        expression_ref E = access_value_for_reg(r).exp;
        assert(is_WHNF(E));
        assert(not E.head().is_a<expression>());
        assert(not reg_is_index_var_no_force(r));
        assert(not reg_is_unevaluated(r));
    }
    if (unevaluated_reg_is_index_var_no_force(r))
        assert(not has_result1(r));
#endif

    if (reg_is_constant(r))
	return {r,r};
    else if (reg_is_changeable(r))
        return incremental_evaluate2_changeable_(r);
    else if (unevaluated_reg_is_index_var_no_force(r))
    {
        int r2 = closure_at(r).reg_for_index_var();
        return incremental_evaluate2(r2, false);
    }
    else if (reg_is_index_var_with_force(r))
        return incremental_evaluate2_index_var_with_force_(r);
    else
        return incremental_evaluate2_unevaluated_(r);

    std::cerr<<"incremental_evaluate2: unreachable?";
    std::abort();
}

pair<int,int> reg_heap::incremental_evaluate2_unevaluated_(int r)
{
    assert(regs.is_valid_address(r));
    assert(regs.is_used(r));
    assert(reg_is_unevaluated(r));

#ifndef NDEBUG
    if (reg_has_value(r))
    {
        expression_ref E = access_value_for_reg(r).exp;
        assert(is_WHNF(E));
        assert(not E.head().is_a<expression>());
        assert(not reg_is_index_var_no_force(r));
        assert(not reg_is_unevaluated(r));
    }
    if (unevaluated_reg_is_index_var_no_force(r))
        assert(not has_result1(r));
#endif

    while (1)
    {
        assert(expression_at(r));

        /*---------- Below here, there is no call, and no value. ------------*/
        if (expression_at(r).is_index_var())
        {
            assert( not reg_is_changeable(r) );

            assert( not has_result1(r) );

            assert( not has_step1(r) );

            int r2 = closure_at(r).reg_for_index_var();
            auto [r3, result] = incremental_evaluate2(r2, false);

            // Update the index_var to point to the end of the index_var_no_force chain.
            if (r2 != r3) regs[r].C.Env[0] = r3;

            if (regs[r].forced_regs.empty())
            {
                mark_reg_index_var_no_force(r);
                return {r3,result};
            }

	    int r4 = follow_index_var(r3);
	    if (r3 != r4) regs[r].C.Env[0] = r4;

	    mark_reg_index_var_with_force(r);

            if (not reg_is_constant(r3))
	    {
		int r5 = set_forced_reg(r,r3);

                // Case 1: r5 == r3 -> new force edge to r3!
                // Case 2: r3 != r5, r3 is forced -> new force edge to r5!
                // Case 3: r3 != r5  r3 is not forced -> new force edge to r5,
		//         but we already incremented the force count when evaluating r3->r5
		//         when r3 calls force_reg_no_call( ).
		if (r5 == r3 or reg_is_forced(r3))
		    inc_count(r5);
		else
		    assert(reg_is_forced(r5));
	    }

            assert(not has_result1(r));
            assert(not prog_unshare[r].test(unshare_result_bit));
            assert(reg_is_evaluated(r));
            return {r, result};
        }

        // Check for WHNF *OR* heap variables
        else if (is_WHNF(expression_at(r)))
        {
            if (regs[r].forced_regs.empty())
	    {
                mark_reg_constant(r);

		assert( not has_step1(r) );
		assert( not has_result1(r) );
		assert( not reg_is_unevaluated(r) );

		return {r,r};
	    }
	    else
	    {
		// Allocate a new reg
		int r2 = allocate();
		auto creator_step = creator_step_for_reg(r);
		if (creator_step)
		    mark_reg_created_by_step(r2, *creator_step);

		// Copy the constant from r -> r2
		closure C2 = closure_at(r);  // do this after allocation - allocation could modify the environment of C2!
		set_C(r2, std::move(C2));

		// Make the current reg into an index-var that points to it.
		closure C1(index_var(0),{r2});
		set_C(r, std::move(C1));
	    }
        }

#ifndef NDEBUG
        else if (expression_at(r).head().is_a<Trim>())
            std::abort();
#endif

        // 3. Reduction: Operation (includes @, case, +, etc.)
        else
        {
            assert( not has_step2(r) );
            // The only we reason we are getting this here is to store created_regs on it,
            // if we perform allocations AFTER using/forcing something changeable.
            int s = get_shared_step(r);

            try
            {
                RegOperationArgs2Unevaluated Args(r, s, *this);
                auto O = expression_at(r).head().as_ptr_to<Operation>()->op;
                closure value = (*O)(Args);
                total_reductions2++;

                // If the reduction doesn't depend on modifiable, then replace E with the value.
                if (not Args.used_changeable)
                {
                    assert( not reg_has_call(r) );
                    assert( not has_result1(r) );
                    assert( regs[r].used_regs.empty() );
                    assert( steps[s].created_regs.empty() ); // Any allocations should have gone to sp
                    set_C( r, std::move(value) );
                    steps.reclaim_used(s);
                }
                else
                {
                    total_changeable_reductions2++;
                    mark_reg_changeable(r);

                    int r2;
                    if (value.exp.is_index_var())
                    {
                        r2 = value.reg_for_index_var();
                    }
                    else
                    {
                        r2 = Args.allocate( std::move(value) ) ;
                        assert(regs[r2].created_by_step.value().first == s);
                        assert(not has_step1(r2));
                    }

                    auto [call,result] = incremental_evaluate2(r2, true);

                    int t = tokens[root_token].children[0];
                    set_call(s, call);

                    int s_bumped = prog_steps[r];
                    tokens[t].vm_step.add_value(r, s_bumped);
                    if (s_bumped > 0) note_step_not_in_root(s_bumped);
                    prog_steps[r] = s;

                    tokens[t].vm_result.add_value(r, prog_results[r]);
                    set_result_for_reg(r);

                    return {r, result};
                }
            }
            catch (error_exception& e)
            {
                if (log_verbose)
                    throw_reg_exception(*this, root_token, r, e, true);
                else
                    throw;
            }
            catch (myexception& e)
            {
                throw_reg_exception(*this, root_token, r, e, true);
            }
            catch (const std::exception& ee)
            {
                myexception e;
                e<<ee.what();
                throw_reg_exception(*this, root_token, r, e, true);
            }
        }
    }

}

pair<int,int> reg_heap::incremental_evaluate2_index_var_with_force_(int r)
{
    if (not reg_is_forced(r))
	force_reg_no_call(r);

    assert(not has_result1(r));
    assert(not has_result2(r));

    int r2 = closure_at(r).reg_for_index_var();

    /*
     * NOTE: If we are going to evaluate the same reg twice, once do_count=true
     *       and once with do_count=false, then we have to do the do_count=true one first.
     *
     *       If we don't, the children will get their counts incremented twice.
     *       This is because incrementing of child counts is done whenever the parent starts
     *         out unforced.
     */

    auto [r3, result3] = incremental_evaluate2(r2, false);

    assert(not reg_is_unevaluated(r));

    return {r, result3};
}

pair<int,int> reg_heap::incremental_evaluate2_changeable_(int r)
{
    total_changeable_eval2++;

    if (has_result2(r))
    {
        total_changeable_eval_with_result2++;

        if (not reg_is_forced(r))
            force_reg_with_call(r);

        int result = result_for_reg(r);
        return {r, result};
    }

    // If we know what to call, then call it and use it to set the value
    if (has_step2(r))
    {
        int s = step_index_for_reg(r);
        // Evaluate S, looking through unchangeable redirections
        auto [call, result] = incremental_evaluate2(steps[s].call, not reg_is_forced(r));

	// If the back-edge has not been set, then we need to update the call to look through
	// index-var-no-force and set the back-edge on the first non-index-var if it is changeable.
        if (not steps[s].call_edge)
        {
            // This should only ever happen for modifiable values set with set_reg_value( ).
            // In such cases, we need this, because the value we set could evaluate to an index_var.
            // Otherwise, we set the call AFTER we have evaluated to a changeable or a constant.
            clear_call_for_reg(r);
            set_call(s, call);
        }
	else
	    assert(call == call_for_reg(r));

        // I think we can only get get here if
        // (i) the step is valid, but the result is invalid.
        // (ii) there was never a value result here to begin with because the unforgettable step was
        //      backward-shared, but the result was not.
        assert(prog_unshare[r].test(unshare_result_bit) or reg_is_unforgettable(r));

        // If the result has changed, mark it with a bit.
        if (prog_unshare[r].test(unshare_result_bit) and regs_maybe_different_value(prog_results[r],result))
            prog_unshare[r].set(different_result_bit);

        // FIXME: How to avoid resharing results of changed modifiables?  Since the step is not shared, we should not reshare.
        //        Perhaps we could mark modifiables with the different_result bit.
        bool reshare_result = prog_results[r] == result;
        if (not reshare_result)
        {
            int t = tokens[root_token].children[0];
            tokens[t].vm_result.add_value(r, prog_results[r]);
            set_result_for_reg( r );
        }
        prog_unshare[r].reset(unshare_result_bit);

        if (not reg_is_forced(r))
            force_reg_no_call(r);

        total_changeable_eval_with_call2++;
        return {r, result};
    }

    assert( not has_step2(r) );

    // Evaluate the regs from non-changeable reduction steps leading to this changeable step.
    bool same_inputs = force_regs_check_same_inputs(r);
    if (same_inputs)
    {
        // FIXME -- merge with has_step2( )?

        assert(prog_unshare[r].test(unshare_step_bit));
        assert(prog_unshare[r].test(unshare_result_bit));
        assert(has_step1(r));
        int s = prog_steps[r];
        int r2 = steps[s].call;

        // Since the call is unchanged, we only need to increment the call count if its been decremented
        auto [call,result] = incremental_evaluate2(r2, prog_unshare[r].test(call_decremented_bit));

        if (prog_unshare[r].test(unshare_result_bit) and regs_maybe_different_value(prog_results[r],result))
            prog_unshare[r].set(different_result_bit);

        bool reshare_result = prog_results[r] == result;
        if (not reshare_result)
        {
            int t = tokens[root_token].children[0];
            tokens[t].vm_result.add_value(r, prog_results[r]);
            set_result_for_reg(r);
        }

        prog_unshare[r].reset(unshare_result_bit);
        prog_unshare[r].reset(unshare_step_bit);
        prog_unshare[r].reset(call_decremented_bit);

        return {r, result};
    }

    try
    {
        int s = get_shared_step(r);
        RegOperationArgs2Changeable Args(r, s, *this);
        auto O = expression_at(r).head().as_ptr_to<Operation>()->op;
        closure value = (*O)(Args);
        total_reductions2++;
        total_changeable_reductions2++;

        int r2;
        if (value.exp.is_index_var())
        {
            r2 = value.reg_for_index_var();
        }
        else
        {
            r2 = Args.allocate( std::move(value) ) ;
            assert(regs[r2].created_by_step.value().first == s);
            assert(not has_step1(r2));
        }

        auto [call,result] = incremental_evaluate2(r2, true);

        int t = tokens[root_token].children[0];
        set_call(s, call);

        int s_bumped = prog_steps[r];
        tokens[t].vm_step.add_value(r, s_bumped);
        if (s_bumped > 0) note_step_not_in_root(s_bumped);
        prog_steps[r] = s;

        if (prog_unshare[r].test(unshare_result_bit) and regs_maybe_different_value(prog_results[r], result))
            prog_unshare[r].set(different_result_bit);

        tokens[t].vm_result.add_value(r, prog_results[r]);
        set_result_for_reg(r);

        prog_unshare[r].reset(unshare_result_bit);
        prog_unshare[r].reset(unshare_step_bit);
        prog_unshare[r].reset(call_decremented_bit);

        return {r, result};
    }
    catch (error_exception& e)
    {
        if (log_verbose)
            throw_reg_exception(*this, root_token, r, e, true);
        else
            throw;
    }
    catch (myexception& e)
    {
        throw_reg_exception(*this, root_token, r, e, true);
    }
    catch (const std::exception& ee)
    {
        myexception e;
        e<<ee.what();
        throw_reg_exception(*this, root_token, r, e, true);
    }

    std::abort();
}

/// These are LAZY operation args! They don't evaluate arguments until they are evaluated by the operation (and then only once).
class RegOperationArgsUnchangeable final: public OperationArgs
{
    bool evaluate_changeables() const override {return false;}

    // common worker called when either using or forcing.
    int evaluate_reg(int r2)
        {
            int r3 = memory().incremental_evaluate_unchangeable(r2);
            if (M.reg_is_changeable_or_forcing(r3) or M.reg_is_unevaluated(r3))
                throw no_context();
            return r3;
        }

    /// Evaluate the reg r2, record dependencies, and return the reg following call chains.
    int evaluate_reg_force(int r2) override
        {
            return evaluate_reg(r2);
        }

    /// Evaluate the reg r2, record a dependency on r2, and return the reg following call chains.
    int evaluate_reg_use(int r2) override
        {
            return evaluate_reg(r2);
        }

public:

    void make_changeable() override
    {
        throw no_context();
    }

    RegOperationArgsUnchangeable(int r_, reg_heap& m)
        :OperationArgs(m, r_)
        {
	}
};

int reg_heap::incremental_evaluate_unchangeable(int r)
{
#ifndef NDEBUG
    if (reg_is_on_stack(r))
        throw myexception()<<"Evaluating reg "<<r<<" that is already on the stack!";
#endif

    regs[r].flags.set(reg_is_on_stack_bit);
    stack.push_back(r);

    auto result = incremental_evaluate_unchangeable_(r);
    assert(reg_is_on_stack(r));

    stack.pop_back();
    regs[r].flags.reset(reg_is_on_stack_bit);

    return result;
}

int reg_heap::incremental_evaluate_unchangeable_(int r)
{
    assert(regs.is_valid_address(r));
    assert(regs.is_used(r));

#ifndef NDEBUG
    assert(not expression_at(r).head().is_a<expression>());
#endif

    while (1)
    {
        assert(expression_at(r));

        if (reg_is_constant(r) or reg_is_changeable_or_forcing(r))
            break;

        else if (unevaluated_reg_is_index_var_no_force(r))
        {
            int r2 = closure_at(r).reg_for_index_var();
            return incremental_evaluate_unchangeable(r2);
        }
        else
            assert(reg_is_unevaluated(r));

        /*---------- Below here, there is no call, and no value. ------------*/
        if (expression_at(r).is_index_var())
        {
            mark_reg_index_var_no_force(r);

            int r2 = closure_at(r).reg_for_index_var();

            int r3 = incremental_evaluate_unchangeable( r2 );

            // If we point to r3 through an intermediate index_var chain, then change us to point to the end
            if (r3 != r2)
                set_C(r, closure(index_var(0),{r3}));

            return r3;
        }

        // Check for WHNF *OR* heap variables
        else if (is_WHNF(expression_at(r)))
            mark_reg_constant(r);

#ifndef NDEBUG
        else if (expression_at(r).head().is_a<Trim>())
            std::abort();
#endif

        // 3. Reduction: Operation (includes @, case, +, etc.)
        else
        {
            auto O = expression_at(r).head().as_ptr_to<Operation>()->op;

            // Although the reg itself is not a modifiable, it will stay changeable if it ever computes a changeable value.
            // Therefore, we cannot do "assert(not result_for_reg(t,r).changeable);" here.

#if defined(DEBUG_MACHINE) && DEBUG_MACHINE>2
            string SS = "";
            SS = compact_graph_expression(*this, r, get_identifiers()).print();
            string SSS = untranslate_vars(deindexify(trim_unnormalize(closure_at(r))),
                                          get_identifiers()).print();
            if (log_verbose >= 3)
                write_dot_graph(*this);
#endif

            try
            {
                RegOperationArgsUnchangeable Args(r, *this);
                closure value = (*O)(Args);
                total_reductions++;
        
                set_C(r, std::move(value) );
            }
            catch (no_context&)
            {
                return r;
            }
            catch (error_exception& e)
            {
                throw;
            }
            catch (myexception& e)
            {
                throw_reg_exception(*this, root_token, r, e, false);
            }
            catch (const std::exception& ee)
            {
                myexception e;
                e<<ee.what();
                throw_reg_exception(*this, root_token, r, e, false);
            }

#if defined(DEBUG_MACHINE) && DEBUG_MACHINE > 2
            //      std::cerr<<"   + recomputing "<<SS<<"\n\n";
            std::cerr<<"   + Executing statement {"<<O<<"}:  "<<SS<<"\n\n";
#endif
        }
    }

    return r;
}

