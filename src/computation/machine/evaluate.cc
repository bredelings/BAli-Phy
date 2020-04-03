//#ifdef NDEBUG
//#undef NDEBUG
//#endif

#include "util/log-level.H"
#include "graph_register.H"
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

long total_case_op = 0;
long total_let_op = 0;
long total_index_op = 0;

expression_ref compact_graph_expression(const reg_heap& C, int R, const map<string, int>&);
expression_ref untranslate_vars(const expression_ref& E, const map<string, int>& ids);
expression_ref untranslate_vars(const expression_ref& E, const map<int,string>& ids);
map<int,string> get_constants(const reg_heap& C, int t);

void throw_reg_exception(reg_heap& M, int t, const closure& C, myexception& e)
{
    dot_graph_for_token(M, t);
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
    dot_graph_for_token(M, t);
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

    const bool reforce;

    const closure& current_closure() const {return memory().closure_stack.back();}

    bool evaluate_changeables() const {return true;}

    /// Evaluate the reg r2, record dependencies, and return the reg following call chains.
    int evaluate_reg(int r2)
        {
            auto [_, value] = M.incremental_evaluate(r2, reforce);
            return value;
        }

    /// Evaluate the reg r2, record dependencies, and return the reg following call chains.
    int evaluate_reg_force(int r2)
        {
            auto [r3, value] = M.incremental_evaluate(r2, reforce);

            if (M.reg_is_changeable(r3))
            {
                used_changeable = true;
                if (first_eval)
                    M.set_forced_reg(r, r3);
            }

            return value;
        }

    /// Evaluate the reg r2, record a dependency on r2, and return the reg following call chains.
    int evaluate_reg_to_reg(int r2)
        {
            // Compute the value, and follow index_var chains (which are not changeable).
            auto [r3, value] = M.incremental_evaluate(r2, reforce);

            // Note that although r2 is newly used, r3 might be already used if it was 
            // found from r2 through a non-changeable reg_var chain.
            if (M.reg_is_changeable(r3))
            {
                used_changeable = true;
                if (first_eval)
                    M.set_used_reg(r, r3);
            }

            return value;
        }

    const closure& evaluate_reg_to_closure(int r2)
        {
            int r3 = evaluate_reg_to_reg(r2);
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
            used_changeable = true;
            memory().mark_step_with_nonforce_effect(s);
            OperationArgs::set_effect(e);
        }

    RegOperationArgs* clone() const {return new RegOperationArgs(*this);}

    RegOperationArgs(int r_, int s_, int sp_, bool rf, reg_heap& m)
        :OperationArgs(m), r(r_), s(s_), sp(sp_), first_eval(m.reg_is_unevaluated(r)), reforce(rf)
        { }
};

/// Evaluate r and look through index_var chains to return the first reg that is NOT a reg_var.
/// The returned reg is guaranteed to be (a) in WHNF (a lambda or constructor) and (b) not an reg_var.
pair<int,int> reg_heap::incremental_evaluate(int r, bool reforce)
{
    assert(execution_allowed());

#ifndef NDEBUG
    if (regs.access(r).flags.test(3))
        throw myexception()<<"Evaluating reg "<<r<<" that is already on the stack!";
    else
        regs.access(r).flags.set(3);
#endif
    stack.push_back(r);
    auto result = incremental_evaluate_(r, reforce);
    stack.pop_back();
#ifndef NDEBUG
    assert(regs.access(r).flags.test(3));
    regs.access(r).flags.flip(3);
#endif
    return result;
}

pair<int,int> reg_heap::incremental_evaluate_(int r, bool reforce)
{
    assert(regs.is_valid_address(r));
    assert(regs.is_used(r));

#ifndef NDEBUG
    if (reg_has_value(r))
    {
        expression_ref E = access_value_for_reg(r).exp;
        assert(is_WHNF(E));
        assert(not E.head().is_a<expression>());
        assert(not E.is_index_var());
    }
    if (expression_at(r).is_index_var())
        assert(not reg_has_value(r));
#endif

    while (1)
    {
        assert(expression_at(r));

#ifndef NDEBUG
        //    std::cerr<<"   statement: "<<r<<":   "<<regs.access(r).E.print()<<std::endl;
#endif

        reg::type_t reg_type = regs.access(r).type;

        if (reg_type == reg::type_t::constant) return {r,r};

        else if (reg_type == reg::type_t::changeable)
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
                auto [call, value] = incremental_evaluate(steps[s].call, reforce);

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
                return {r, value};
            }
        }
        else if (reg_type == reg::type_t::index_var)
        {
            int r2 = closure_at(r).reg_for_index_var();
            return incremental_evaluate(r2, reforce);
        }
        else
            assert(reg_type == reg::type_t::unevaluated);

        /*---------- Below here, there is no call, and no value. ------------*/
        if (expression_at(r).is_index_var())
        {
            assert( not reg_is_changeable(r) );

            assert( not reg_has_value(r) );

            assert( not reg_has_call(r) );

            assert( not has_step(r) );

            regs.access(r).type = reg::type_t::index_var;

            int r2 = closure_at(r).reg_for_index_var();

            // Return the end of the index_var chain.
            // We used to update the index_var to point to the end of the chain.

            return incremental_evaluate(r2, reforce);
        }

        // Check for WHNF *OR* heap variables
        else if (is_WHNF(expression_at(r)))
        {
            regs.access(r).type = reg::type_t::constant;
            assert( not has_step(r) );
            return {r,r};
        }

#ifndef NDEBUG
        else if (expression_at(r).head().is_a<Trim>())
            std::abort();
#endif

        // 3. Reduction: Operation (includes @, case, +, etc.)
        else
        {
            assert( prog_steps[r] <=0 );
            // The only we reason we are getting this here is to store created_regs on it,
            // if we perform allocations AFTER using/forcing something changeable.
            int s = get_shared_step(r);

            int sp = regs.access(r).created_by.first;

            try
            {
                closure_stack.push_back( closure_at(r) );
                RegOperationArgs Args(r, s, sp, reforce, *this);
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
                    assert( regs[r].forced_regs.empty() );
                    assert( steps[s].created_regs.empty() ); // Any allocations should have gone to sp
                    set_C( r, std::move(value) );
                    steps.reclaim_used(s);
                }
                else
                {
                    total_changeable_reductions++;
                    make_reg_changeable(r);
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
                        assert(not has_step(r2));
                    }

                    auto [call,value] = incremental_evaluate(r2, reforce);
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
class RegOperationArgsUnchangeable final: public OperationArgs
{
    const int r;

    const closure& current_closure() const {return memory()[r];}

    bool evaluate_changeables() const {return false;}

    /// Evaluate the reg r2, record dependencies, and return the reg following call chains.
    int evaluate_reg(int r2)
        {
            int r3 = memory().incremental_evaluate_unchangeable(r2);
            if (M.reg_type(r3) == reg::type_t::changeable)
                throw no_context();
            return r3;
        }

    /// Evaluate the reg r2, record dependencies, and return the reg following call chains.
    int evaluate_reg_force(int r2)
        {
            return evaluate_reg(r2);
        }

    /// Evaluate the reg r2, record a dependency on r2, and return the reg following call chains.
    int evaluate_reg_to_reg(int r2)
        {
            return evaluate_reg(r2);
        }

    const closure& evaluate_reg_to_closure(int r2)
        {
            int r3 = evaluate_reg_to_reg(r2);
            assert(M.reg_type(r3) == reg::type_t::constant);
            return M[r3];
        }

    const closure& evaluate_reg_to_closure_(int r2)
        {
            int r3 = evaluate_reg_to_reg(r2);
            assert(M.reg_type(r3) == reg::type_t::constant);
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

        reg::type_t reg_type = regs.access(r).type;

        if (reg_type == reg::type_t::constant or reg_type == reg::type_t::changeable)
            break;

        else if (reg_type == reg::type_t::index_var)
        {
            int r2 = closure_at(r).reg_for_index_var();
            return incremental_evaluate_unchangeable(r2);
        }
        else
            assert(reg_type == reg::type_t::unevaluated);

        /*---------- Below here, there is no call, and no value. ------------*/
        const int type = expression_at(r).head().type();
        if (type == index_var_type)
        {
            regs.access(r).type = reg::type_t::index_var;

            int r2 = closure_at(r).reg_for_index_var();

            int r3 = incremental_evaluate_unchangeable( r2 );

            // If we point to r3 through an intermediate index_var chain, then change us to point to the end
            if (r3 != r2)
                set_C(r, closure(index_var(0),{r3}));

            return r3;
        }

        // Check for WHNF *OR* heap variables
        else if (is_WHNF(expression_at(r)))
            regs.access(r).type = reg::type_t::constant;

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
                regs.access(r).type = reg::type_t::changeable;
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

