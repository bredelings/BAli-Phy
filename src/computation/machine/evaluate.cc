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
expression_ref untranslate_vars(const expression_ref& E, const map<string, int>& ids);
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
class RegOperationArgs final: public OperationArgs
{
    const int r;

    const int s;

    const int sp;  // creator step

    const bool first_eval;

    const closure& current_closure() const {return memory().closure_stack.back();}

    bool evaluate_changeables() const {return true;}

    /// Evaluate the reg r2, record dependencies, and return the reg following call chains.
    int evaluate_reg_force(int r2)
        {
            auto [r3, value] = M.incremental_evaluate1(r2);

            if (M.reg_is_changeable_or_forcing(r3))
            {
                if (first_eval)
                    M.set_forced_reg(r, r3);
            }

            return value;
        }

    /// Evaluate the reg r2, record a dependency on r2, and return the reg following call chains.
    int evaluate_reg_use(int r2)
        {
            // Compute the value, and follow index_var chains (which are not changeable).
            auto [r3, value] = M.incremental_evaluate1(r2);

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

            return value;
        }

    const closure& evaluate_reg_to_closure(int r2)
        {
            int r3 = evaluate_reg_use(r2);
            return M[r3];
        }

    const closure& evaluate_reg_to_closure_(int r2)
        {
            int r3 = evaluate_reg_force(r2);
            return M[r3];
        }

public:

    bool used_changeable = false;

    void make_changeable()
    {
        used_changeable = true;
    }

    // If we unreference regs that evaluate to a variable, then we unreference p->let q=2 in q
    // and point references to q instead of p.  But then it would not be true that a variable can
    // only be referenced if the slot that created it is still referenced.

    int allocate_reg()
        {
            int s_alloc = used_changeable?s:sp;
            int r_alloc = OperationArgs::allocate_reg();
            if (s_alloc > 0)
                M.mark_reg_created_by_step(r_alloc, s_alloc);
            return r_alloc;
        }

    void set_effect(const effect& e)
        {
            make_changeable();
            memory().mark_step_with_effect(s);
            e.register_effect(M, s);
        }

    RegOperationArgs* clone() const {return new RegOperationArgs(*this);}

    RegOperationArgs(int r_, int s_, int sp_, reg_heap& m)
        :OperationArgs(m), r(r_), s(s_), sp(sp_), first_eval(m.reg_is_unevaluated(r))
        { }
};

/// Evaluate r and look through index_var chains to return the first reg that is NOT a reg_var.
/// The returned reg is guaranteed to be (a) in WHNF (a lambda or constructor) and (b) not an reg_var.
pair<int,int> reg_heap::incremental_evaluate1(int r)
{
    assert(execution_allowed());

#ifndef NDEBUG
    if (regs.access(r).flags.test(3))
        throw myexception()<<"Evaluating reg "<<r<<" that is already on the stack!";
    else
        regs.access(r).flags.set(3);
#endif
    stack.push_back(r);
    auto result = incremental_evaluate1_(r);
    assert(not reg_is_index_var_no_force(result.first));
    assert(not reg_is_unevaluated(result.first));
    assert(not reg_is_unevaluated(r));
    stack.pop_back();
#ifndef NDEBUG
    assert(regs.access(r).flags.test(3));
    regs.access(r).flags.flip(3);
#endif
    return result;
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
        assert(not reg_has_value(r));
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
            auto [call, value] = incremental_evaluate1(steps[s].call);

            // If computation_for_reg(r).call can be evaluated to refer to S w/o moving through any changable operations,
            // then it should be safe to change computation_for_reg(r).call to refer to S, even if r is changeable.
            if (call != call_for_reg(r))
            {
                // This should only ever happen for modifiable values set with set_reg_value( ).
                // In such cases, we need this, because the value we set could evaluate to an index_var.
                // Otherwise, we set the call AFTER we have evaluated to a changeable or a constant.
                clear_call_for_reg(r);
                set_call(s, call);
            }

            // r gets its value from S.
            set_result_for_reg( r );
            if (not tokens[root_token].children.empty())
            {
                int t = tokens[root_token].children[0];
                tokens[t].vm_result.add_value(r, non_computed_index);
            }

            total_changeable_eval_with_call++;
            assert(not reg_is_unevaluated(r));
            return {r, value};
        }
    }
    else if (unevaluated_reg_is_index_var_no_force(r))
    {
        int r2 = closure_at(r).reg_for_index_var();
        return incremental_evaluate1(r2);
    }
    else if (reg_is_index_var_with_force(r))
    {
        int r2 = closure_at(r).reg_for_index_var();

        if (has_result1(r))
        {
            int result = result_for_reg(r);
            return {r,result};
        }
        else
        {
            auto [r3, result3] = incremental_evaluate1(r2);

            // r gets its value from S.
            if (not tokens[root_token].children.empty())
            {
                int t = tokens[root_token].children[0];
                tokens[t].vm_result.add_value(r, non_computed_index);
            }
            prog_results[r] = result3;

            assert(not reg_is_unevaluated(r));
            return {r, result3};
        }
    }

    while (1)
    {
        assert(expression_at(r));

#ifndef NDEBUG
        //    std::cerr<<"   statement: "<<r<<":   "<<regs.access(r).E.print()<<std::endl;
#endif

        /*---------- Below here, there is no call, and no value. ------------*/
        if (expression_at(r).is_index_var())
        {
            assert( not reg_is_changeable(r) );

            assert( not reg_has_value(r) );

            assert( not reg_has_call(r) );

            assert( not has_step1(r) );

            int r2 = closure_at(r).reg_for_index_var();

            auto [r3, result] = incremental_evaluate1(r2);

            if (regs[r].forced_regs.empty())
            {
                // Return the end of the index_var chain.
                // We used to update the index_var to point to the end of the chain.

                mark_reg_index_var_no_force(r);
                return {r3,result};
            }

            if (reg_is_to_changeable(r3))
                mark_reg_index_var_with_force_to_changeable(r);
            else
                mark_reg_index_var_with_force_to_nonchangeable(r);

            if (not reg_is_constant_no_force(r3))
                set_index_var_ref(r,r3);

            assert(not has_result1(r));

            if (not tokens[root_token].children.empty())
            {
                int t = tokens[root_token].children[0];
                tokens[t].vm_result.add_value(r, non_computed_index);
            }
            prog_results[r] = result;

            assert(not prog_unshare[r].test(unshare_result_bit));
            assert(not reg_is_unevaluated(r));
            return {r, result};
        }

        // Check for WHNF *OR* heap variables
        else if (is_WHNF(expression_at(r)))
        {
            if (regs[r].forced_regs.empty())
                mark_reg_constant_no_force(r);
            else
                mark_reg_constant_with_force(r);

            assert( not has_step1(r) );
            assert( not has_result1(r) );
            assert( not reg_is_unevaluated(r) );

            return {r,r};
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

            int sp = regs.access(r).created_by.first;

            try
            {
                bool first_eval = reg_is_unevaluated(r);
                int n_forces = first_eval ? regs[r].forced_regs.size() : 0;

                closure_stack.push_back( closure_at(r) );
                RegOperationArgs Args(r, s, sp, *this);
                auto O = expression_at(r).head().assert_is_a<Operation>()->op;
                closure value = (*O)(Args);
                closure_stack.pop_back();
                total_reductions++;

                // If the reduction doesn't depend on modifiable, then replace E with the value.
                if (not Args.used_changeable)
                {
                    assert( not reg_has_call(r) );
                    assert( not reg_has_value(r) );
                    assert( regs[r].used_regs.empty() );
                    assert( steps[s].created_regs.empty() ); // Any allocations should have gone to sp
                    set_C( r, std::move(value) );
                    steps.reclaim_used(s);
                }
                else
                {
                    total_changeable_reductions++;
                    mark_reg_changeable(r);
                    closure_stack.push_back(value);

                    int r2;
                    if (closure_stack.back().exp.head().is_index_var())
                    {
                        r2 = closure_stack.back().reg_for_index_var();
                    }
                    else
                    {
                        r2 = Args.allocate( std::move(closure_stack.back()) ) ;
                        assert(regs.access(r2).created_by.first == s);
                        assert(not has_step1(r2));
                    }

                    auto [call,value] = incremental_evaluate1(r2);
                    closure_stack.pop_back();

                    set_call(s, call);

                    prog_steps[r] = s;
                    set_result_for_reg(r);

                    if (not tokens[root_token].children.empty())
                    {
                        int t = tokens[root_token].children[0];
                        tokens[t].vm_result.add_value(r, non_computed_index);
                        tokens[t].vm_step.add_value(r, non_computed_index);
                    }

                    if (first_eval)
                        regs[r].n_regs_forced_before_step = n_forces;

                    assert(not reg_is_unevaluated(r));
                    return {r, value};
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

/// These are LAZY operation args! They don't evaluate arguments until they are evaluated by the operation (and then only once).
class RegOperationArgs2 final: public OperationArgs
{
    const int r;

    const int s;

    const int sp;  // creator step

    const bool first_eval;

    const bool zero_count;

    const closure& current_closure() const {return memory().closure_stack.back();}

    bool evaluate_changeables() const {return true;}

    /// Evaluate the reg r2, record dependencies, and return the reg following call chains.
    int evaluate_reg_force(int r2)
        {
            auto [r3, value] = M.incremental_evaluate2(r2, zero_count);

            if (M.reg_is_changeable_or_forcing(r3))
            {
                if (first_eval)
                    M.set_forced_reg(r, r3);
            }

            return value;
        }

    /// Evaluate the reg r2, record a dependency on r2, and return the reg following call chains.
    int evaluate_reg_use(int r2)
        {
            // Compute the value, and follow index_var chains (which are not changeable).
            auto [r3, value] = M.incremental_evaluate2(r2, zero_count);

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

            return value;
        }

    const closure& evaluate_reg_to_closure(int r2)
        {
            int r3 = evaluate_reg_use(r2);
            return M[r3];
        }

    const closure& evaluate_reg_to_closure_(int r2)
        {
            int r3 = evaluate_reg_force(r2);
            return M[r3];
        }

public:

    bool used_changeable = false;

    void make_changeable()
    {
        used_changeable = true;
    }

    // If we unreference regs that evaluate to a variable, then we unreference p->let q=2 in q
    // and point references to q instead of p.  But then it would not be true that a variable can
    // only be referenced if the slot that created it is still referenced.

    int allocate_reg()
        {
            int s_alloc = used_changeable?s:sp;
            int r_alloc = OperationArgs::allocate_reg();
            if (s_alloc > 0)
                M.mark_reg_created_by_step(r_alloc, s_alloc);
            return r_alloc;
        }

    void set_effect(const effect& e)
        {
            make_changeable();
            memory().mark_step_with_effect(s);
            e.register_effect(M, s);
        }

    RegOperationArgs2* clone() const {return new RegOperationArgs2(*this);}

    RegOperationArgs2(int r_, int s_, int sp_, reg_heap& m)
        :OperationArgs(m), r(r_), s(s_), sp(sp_), first_eval(m.reg_is_unevaluated(r)), zero_count(not M.reg_is_forced(r))
        { }
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
    assert(execution_allowed());

#ifndef NDEBUG
    if (regs.access(r).flags.test(3))
        throw myexception()<<"Evaluating reg "<<r<<" that is already on the stack!";
    else
        regs.access(r).flags.set(3);
#endif
    stack.push_back(r);
    auto result = incremental_evaluate2_(r);
    assert(not reg_is_index_var_no_force(result.first));
    assert(not reg_is_unevaluated(result.first));
    assert(not reg_is_unevaluated(r));
    stack.pop_back();
#ifndef NDEBUG
    assert(regs.access(r).flags.test(3));
    regs.access(r).flags.flip(3);
#endif
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
        assert(not reg_has_value(r));
#endif

    if (reg_is_constant_no_force(r)) return {r,r};

    else if (reg_is_constant_with_force(r))
    {
        if (not reg_is_forced(r))
            force_reg_no_call(r);

        return {r, r};
    }
    else if (reg_is_changeable(r))
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
            auto [call, value] = incremental_evaluate2(steps[s].call, not reg_is_forced(r));

            // If computation_for_reg(r).call can be evaluated to refer to S w/o moving through any changable operations,
            // then it should be safe to change computation_for_reg(r).call to refer to S, even if r is changeable.
            if (call != call_for_reg(r))
            {
                // This should only ever happen for modifiable values set with set_reg_value( ).
                // In such cases, we need this, because the value we set could evaluate to an index_var.
                // Otherwise, we set the call AFTER we have evaluated to a changeable or a constant.
                clear_call_for_reg(r);
                set_call(s, call);
            }

            // r gets its value from S.
            int t = tokens[root_token].children[0];

            // FIXME: How to avoid resharing results of changed modifiables?  Since the step is not shared, we should not reshare.
            bool reshare_result = prog_unshare[r].test(unshare_result_bit) and (prog_results[r] == value);
            if (not reshare_result)
            {
                tokens[t].vm_result.add_value(r, prog_results[r]);
                set_result_for_reg( r );
            }
            prog_unshare[r].reset(unshare_result_bit);

            if (not reg_is_forced(r))
                force_reg_no_call(r);

            total_changeable_eval_with_call2++;
            return {r, value};
        }
    }
    else if (unevaluated_reg_is_index_var_no_force(r))
    {
        int r2 = closure_at(r).reg_for_index_var();
        return incremental_evaluate2(r2, false);
    }
    else if (reg_is_index_var_with_force(r))
    {
        int r2 = closure_at(r).reg_for_index_var();

        if (has_result2(r))
        {
            if (not reg_is_forced(r))
            {
                force_reg_no_call(r);
                incremental_evaluate2(r2, true);
            }

            int result = result_for_reg(r);
            return {r,result};
        }
        else
        {
            auto [r3, result3] = incremental_evaluate2(r2, not reg_is_forced(r));

            bool reshare_result = prog_unshare[r].test(unshare_result_bit) and prog_results[r] == result3;
            if (not reshare_result)
            {
                int t = tokens[root_token].children[0];
                tokens[t].vm_result.add_value(r, prog_results[r]);
                prog_results[r] = result3;
            }
            prog_unshare[r].reset(unshare_result_bit);

            if (not reg_is_forced(r))
                force_reg_no_call(r);

            assert(not reg_is_unevaluated(r));
            return {r, result3};
        }
    }
    else
        assert(reg_is_unevaluated(r));

    while (1)
    {
        assert(expression_at(r));

#ifndef NDEBUG
        //    std::cerr<<"   statement: "<<r<<":   "<<regs.access(r).E.print()<<std::endl;
#endif

        /*---------- Below here, there is no call, and no value. ------------*/
        if (expression_at(r).is_index_var())
        {
            assert( not reg_is_changeable(r) );

            assert( not reg_has_value(r) );

            assert( not reg_has_call(r) );

            assert( not has_step1(r) );

            int r2 = closure_at(r).reg_for_index_var();

            if (regs[r].forced_regs.empty())
            {
                // Return the end of the index_var chain.
                // We used to update the index_var to point to the end of the chain.

                auto [r3, result] = incremental_evaluate2(r2, false);

                mark_reg_index_var_no_force(r);
                return {r3,result};
            }

            auto [r3, result] = incremental_evaluate2(r2, true);

            if (reg_is_to_changeable(r3))
                mark_reg_index_var_with_force_to_changeable(r);
            else
                mark_reg_index_var_with_force_to_nonchangeable(r);

            if (not reg_is_constant_no_force(r3))
                set_index_var_ref(r,r3);

            assert(not has_result1(r));

            int t = tokens[root_token].children[0];
            tokens[t].vm_result.add_value(r, prog_results[r]);

            prog_results[r] = result;

            assert(not prog_unshare[r].test(unshare_result_bit));
            assert(not reg_is_unevaluated(r));
            return {r, result};
        }

        // Check for WHNF *OR* heap variables
        else if (is_WHNF(expression_at(r)))
        {
            if (regs[r].forced_regs.empty())
                mark_reg_constant_no_force(r);
            else
                mark_reg_constant_with_force(r);

            assert( not has_step1(r) );
            assert( not has_result1(r) );
            assert( not reg_is_unevaluated(r) );

            return {r,r};
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

            int sp = regs.access(r).created_by.first;

            try
            {
                bool first_eval = reg_is_unevaluated(r);
                int n_forces = first_eval ? regs[r].forced_regs.size() : 0;

                closure_stack.push_back( closure_at(r) );
                RegOperationArgs2 Args(r, s, sp, *this);
                auto O = expression_at(r).head().assert_is_a<Operation>()->op;
                closure value = (*O)(Args);
                closure_stack.pop_back();
                total_reductions2++;

                // If the reduction doesn't depend on modifiable, then replace E with the value.
                if (not Args.used_changeable)
                {
                    assert( not reg_has_call(r) );
                    assert( not reg_has_value(r) );
                    assert( regs[r].used_regs.empty() );
                    assert( steps[s].created_regs.empty() ); // Any allocations should have gone to sp
                    set_C( r, std::move(value) );
                    steps.reclaim_used(s);
                }
                else
                {
                    total_changeable_reductions2++;
                    mark_reg_changeable(r);
                    closure_stack.push_back(value);

                    int r2;
                    if (closure_stack.back().exp.head().is_index_var())
                    {
                        r2 = closure_stack.back().reg_for_index_var();
                    }
                    else
                    {
                        r2 = Args.allocate( std::move(closure_stack.back()) ) ;
                        assert(regs.access(r2).created_by.first == s);
                        assert(not has_step1(r2));
                    }

                    auto [call,value] = incremental_evaluate2(r2, true);
                    closure_stack.pop_back();

                    assert(not prog_unshare[r].test(unshare_step_bit) or prog_steps[r] > 0);
                    bool reshare_step = prog_unshare[r].test(unshare_step_bit) and (steps[prog_steps[r]].call == call);

                    int t = tokens[root_token].children[0];
                    if (not reshare_step)
                    {
                        set_call(s, call);

                        tokens[t].vm_step.add_value(r, prog_steps[r]);
                        prog_steps[r] = s;
                    }
                    else
                    {
                        // Can we decrement the count for the called reg here?  Unless the original call count has already been subtracted?
#ifndef NDEBUG
                        for(auto cr: steps[s].created_regs)
                            assert(not reg_is_changeable(cr));
#endif
                        if (not prog_unshare[r].test(call_decremented_bit) and reg_is_changeable_or_forcing(call))
                        {
                            assert(prog_force_counts[call] >= 2);
                            assert(prog_unshare[call].test(unshare_count_bit));
                            prog_force_counts[call]--;
                        }

                        destroy_step_and_created_regs(s);
                    }

                    bool reshare_result = reshare_step and prog_results[r] == value;
                    if (not reshare_result)
                    {
                        tokens[t].vm_result.add_value(r, prog_results[r]);
                        set_result_for_reg(r);
                    }

                    prog_unshare[r].reset(unshare_result_bit);
                    prog_unshare[r].reset(unshare_step_bit);
                    prog_unshare[r].reset(call_decremented_bit);

                    // Evaluate the regs from non-changeable reduction steps leading to this changeable step.
                    if (first_eval)
                        regs[r].n_regs_forced_before_step = n_forces;
                    else if (not reg_is_forced(r))
                        force_regs_before_step(r);

                    return {r, value};
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

    std::cerr<<"incremental_evaluate2: unreachable?";
    std::abort();
}

/// These are LAZY operation args! They don't evaluate arguments until they are evaluated by the operation (and then only once).
class RegOperationArgsUnchangeable final: public OperationArgs
{
    const int r;

    const closure& current_closure() const {return memory()[r];}

    bool evaluate_changeables() const {return false;}

    // common worker called when either using or forcing.
    int evaluate_reg(int r2)
        {
            int r3 = memory().incremental_evaluate_unchangeable(r2);
            if (M.reg_is_changeable_or_forcing(r3) or M.reg_is_unevaluated(r3))
                throw no_context();
            return r3;
        }

    /// Evaluate the reg r2, record dependencies, and return the reg following call chains.
    int evaluate_reg_force(int r2)
        {
            return evaluate_reg(r2);
        }

    /// Evaluate the reg r2, record a dependency on r2, and return the reg following call chains.
    int evaluate_reg_use(int r2)
        {
            return evaluate_reg(r2);
        }

    const closure& evaluate_reg_to_closure(int r2)
        {
            int r3 = evaluate_reg_use(r2);
            assert(M.reg_is_constant_no_force(r3));
            return M[r3];
        }

    const closure& evaluate_reg_to_closure_(int r2)
        {
            int r3 = evaluate_reg_use(r2);
            assert(M.reg_is_constant_no_force(r3));
            return M[r3];
        }

public:

    void make_changeable()
    {
        throw no_context();
    }

    RegOperationArgsUnchangeable* clone() const {return new RegOperationArgsUnchangeable(*this);}

    RegOperationArgsUnchangeable(int r_, reg_heap& m)
        :OperationArgs(m),r(r_)
        { }
};

int reg_heap::incremental_evaluate_unchangeable(int R)
{
#ifndef NDEBUG
    if (regs.access(R).flags.test(3))
        throw myexception()<<"Evaluating reg "<<R<<" that is already on the stack!";
    else
        regs.access(R).flags.set(3);
#endif
    stack.push_back(R);
    auto result = incremental_evaluate_unchangeable_(R);
    stack.pop_back();
#ifndef NDEBUG
    assert(regs.access(R).flags.test(3));
    regs.access(R).flags.flip(3);
#endif
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

        if (reg_is_constant_no_force(r) or reg_is_changeable_or_forcing(r))
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
            mark_reg_constant_no_force(r);

#ifndef NDEBUG
        else if (expression_at(r).head().is_a<Trim>())
            std::abort();
#endif

        // 3. Reduction: Operation (includes @, case, +, etc.)
        else
        {
            auto O = expression_at(r).head().assert_is_a<Operation>()->op;

            // Although the reg itself is not a modifiable, it will stay changeable if it ever computes a changeable value.
            // Therefore, we cannot do "assert(not result_for_reg(t,r).changeable);" here.

#if defined(DEBUG_MACHINE) && DEBUG_MACHINE>2
            string SS = "";
            SS = compact_graph_expression(*this, r, get_identifiers()).print();
            string SSS = untranslate_vars(deindexify(trim_unnormalize(closure_at(r))),
                                          get_identifiers()).print();
            if (log_verbose >= 3)
                dot_graph_for_token(*this, root_token);
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

