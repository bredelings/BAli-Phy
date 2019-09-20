//#ifdef NDEBUG
//#undef NDEBUG
//#endif
#define COMBINE_STEPS
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
class RegOperationArgs: public OperationArgs
{
    const int S;

    const int P;

    const closure& current_closure() const {return memory().closure_stack.back();}

    bool evaluate_changeables() const {return true;}

    /// Evaluate the reg R2, record dependencies, and return the reg following call chains.
    int evaluate_reg_no_record(int R2)
	{
	    auto [R3, value] = memory().incremental_evaluate(R2);

	    if (M.reg_is_changeable(R3))
	    {
		used_changeable = true;
		M.set_forced_input(S, R3);
	    }

	    return value;
	}

    /// Evaluate the reg R2, record a dependency on R2, and return the reg following call chains.
    int evaluate_reg_to_reg(int R2)
	{
	    // Compute the value, and follow index_var chains (which are not changeable).
	    auto [R3, value] = M.incremental_evaluate(R2);

	    // Note that although R2 is newly used, R3 might be already used if it was 
	    // found from R2 through a non-changeable reg_var chain.
	    if (M.reg_is_changeable(R3))
	    {
		used_changeable = true;
		M.set_used_input(S, R3);
	    }

	    return value;
	}

    const closure& evaluate_reg_to_closure(int R2)
	{
	    int R3 = evaluate_reg_to_reg(R2);
	    return M[R3];
	}
  
    const closure& evaluate_reg_to_closure_(int R2)
	{
	    int R3 = evaluate_reg_no_record(R2);
	    return M[R3];
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
	    int s = used_changeable?S:P;
	    int r = OperationArgs::allocate_reg();
	    if (s>0)
		M.mark_reg_created_by_step(r,s);
	    return r;
	}
  
    RegOperationArgs* clone() const {return new RegOperationArgs(*this);}

    RegOperationArgs(int s, int p, reg_heap& m)
	:OperationArgs(m), S(s), P(p)
	{ }
};

/*
 * incremental_eval R1
 *
 *   Note: index_var's never have a value, or call, and are never changeable.
 *   Note: only operations can have a call, and only if the operation uses values of changeable parameters.
 * 
 *   while(not R1.value) do:
 *
 *   If R1.E = (Op or parameter) with call
 *      assert(R1.changeable == true)
 *      R1.call = incremental_evaluate(R1.call)
 *      R1.value = R1.call.value
 *      <break>
 *
 *   If R1.E = <R2>
 *      assert(not R1.changeable)
 *      assert(not R1.call)
 *      R3 = incremental_evaluate(R2)
 *      if (R3 != R2)
 *         R1.E = <R3>
 *      return R3
 *      <break>
 *
 *   If (R1.E is WHNF)
 *      R1.value = R1.E
 *      <break>
 *
 *   If R1.E = modifiable and no call
 *      Complain: modifiable should always have a call!
 *  
 *   If R1.E = Op args (no call)
 *      **Execute reduction**
 *      R1.changeable = reduction changeable
 *      If (changeable)
 *         R1.call = new reg (reduction value)
 *      Else
 *         R1.E = reduction value
 *      <continue>
 *
 *   If R1.E = let expression
 *      R1.E = reduction value
 *      assert(not changeable)
 *      assert(no call)
 *      assert(no value)
 *      <continue>
 *
 *   assert(R1 has a value)
 *   assert(R1.value is WHNF)
 *   assert(R1.value is not a reg_var <*>)
 *   assert(R1.value is not an index_var <*>)
 *   return R1
 */


pair<int,int> reg_heap::incremental_evaluate(int R)
{
#ifndef NDEBUG
    if (regs.access(R).flags.test(3))
        throw myexception()<<"Evaluating reg "<<R<<" that is already on the stack!";
    else
        regs.access(R).flags.set(3);
#endif
    stack.push_back(R);
    auto result = incremental_evaluate_(R);
    stack.pop_back();
#ifndef NDEBUG
    assert(regs.access(R).flags.test(3));
    regs.access(R).flags.flip(3);
#endif
    return result;
}

// Perhaps rewrite the expression system to
// (a) DONE: Separate the head (object_ref) from the other args (expression_ref)
// (b) Make a constructor take some number of arguments.
// (c) Change the interpretation of closure constructors so that they are always C n n-1 ... 1 0.
//     I guess if we don't then we have to actually look into the constructor expression.
// (d) DONE: Remove Operation::evaluate( ) and just use lazy_evaluate( ).
// (e) Make translate_refs use only one names for refs that occur twice.
// (f) Make a pretty printer for expression_ref?

/// Evaluate R and look through reg_var chains to return the first reg that is NOT a reg_var.
/// The returned reg is guaranteed to be (a) in WHNF (a lambda or constructor) and (b) not a reg_var.
pair<int,int> reg_heap::incremental_evaluate_(int R)
{
    assert(is_completely_dirty(root_token));
    assert(regs.is_valid_address(R));
    assert(regs.is_used(R));

#ifndef NDEBUG
    if (reg_has_value(R))
    {
	expression_ref E = access_value_for_reg(R).exp;
	assert(is_WHNF(E));
	assert(not E.head().is_a<expression>());
	assert(not E.is_index_var());
    }
    if (expression_at(R).is_index_var())
	assert(not reg_has_value(R));
#endif

    while (1)
    {
	assert(expression_at(R));

#ifndef NDEBUG
	//    std::cerr<<"   statement: "<<R<<":   "<<regs.access(R).E.print()<<std::endl;
#endif

	reg::type_t reg_type = regs.access(R).type;

	if (reg_type == reg::type_t::constant) return {R,R};

	else if (reg_type == reg::type_t::changeable)
	{
	    total_changeable_eval++;
	    int rc = result_index_for_reg(R);

	    // If we have a value, then we are done.
	    if (rc > 0)
	    {
		int value = results[rc].value;

		if (value)
		{
		    total_changeable_eval_with_result++;
		    return {R, value};
		}
	    }

	    // If we know what to call, then call it and use it to set the value
	    if (reg_has_call(R))
	    {
		// Evaluate S, looking through unchangeable redirections
		auto [call, value] = incremental_evaluate(call_for_reg(R));

		// If computation_for_reg(R).call can be evaluated to refer to S w/o moving through any changable operations, 
		// then it should be safe to change computation_for_reg(R).call to refer to S, even if R is changeable.
		if (call != call_for_reg(R))
		{
		    clear_call_for_reg(R);
		    set_call(R, call);
		}

		// R gets its value from S.
		set_result_value_for_reg( R);
		total_changeable_eval_with_call++;
		return {R, value};
	    }
	}
	else if (reg_type == reg::type_t::index_var)
	{
	    int R2 = closure_at(R).reg_for_index_var();
	    return incremental_evaluate(R2);
	}
	else
	    assert(reg_type == reg::type_t::unknown);

	/*---------- Below here, there is no call, and no value. ------------*/
	if (expression_at(R).is_index_var())
	{
	    assert( not reg_is_changeable(R) );

	    assert( not reg_has_value(R) );

	    assert( not reg_has_call(R) );

	    regs.access(R).type = reg::type_t::index_var;

	    clear_result(R);
	    int s = step_index_for_reg(R);
	    if (s > 0)
		clear_back_edges_for_step(s);
	    clear_step(R);

	    int R2 = closure_at(R).reg_for_index_var();

	    // Return the end of the index_var chain.
	    // We used to update the index_var to point to the end of the chain.

	    return incremental_evaluate(R2);
	}

	// Check for WHNF *OR* heap variables
	else if (is_WHNF(expression_at(R)))
	{
	    regs.access(R).type = reg::type_t::constant;
	    clear_result(R);
	    int s = step_index_for_reg(R);
	    if (s > 0)
		clear_back_edges_for_step(s);
	    clear_step(R);
	    return {R,R};
	}

#ifndef NDEBUG
	else if (expression_at(R).head().is_a<Trim>())
	    std::abort();
	else if (expression_at(R).type() == parameter_type)
	    std::abort();
#endif

	// 3. Reduction: Operation (includes @, case, +, etc.)
	else
	{
	    // We keep the (same) computation here, until we prove that we don't need one.
	    // We don't need one if we evaluate to WHNF, and then we remove it.
	    if (not has_step(R))
		add_shared_step(R);
	    int S = step_index_for_reg(R);
	    // FIXME - check that this agrees with our caller!
	    int P = regs.access(R).created_by.first;

	    try
	    {
		closure_stack.push_back (closure_at(R) );
		RegOperationArgs Args(S, P, *this);
		auto O = expression_at(R).head().assert_is_a<Operation>()->op;
		closure value = (*O)(Args);
		closure_stack.pop_back();
		total_reductions++;
		if (not steps[S].used_inputs.empty())
		    total_changeable_reductions++;

		// If the reduction doesn't depend on modifiable, then replace E with the value.
		if (not Args.used_changeable)
		{
		    // The old used_input slots are not invalid, which is OK since none of them are changeable.
		    assert(not reg_has_call(R) );
		    assert(not reg_has_value(R));
		    assert(step_for_reg(R).used_inputs.empty());
		    assert(step_for_reg(R).created_regs.empty());
		    set_C(R, std::move(value) );
		}
		else
		{
		    make_reg_changeable(R);
		    closure_stack.push_back(value);

#ifndef COMBINE_STEPS
		    int r2;
		    if (closure_stack.back().exp.head().is_index_var())
		    {
			r2 = closure_stack.back().reg_for_index_var();
		    }
		    else
		    {
			r2 = Args.allocate( std::move(closure_stack.back()) ) ;
			assert(regs.access(r2).created_by.first == S);
			assert(not has_step(r2));
		    }

		    auto p = incremental_evaluate(r2);
#else
		    incremental_evaluate_from_call_(S);

		    pair<int,int> p;
		    if (closure_stack.back().exp.head().is_index_var())
		    {
			int r2 = closure_stack.back().reg_for_index_var();
			p = incremental_evaluate(r2);
		    }
		    else
		    {
			int r2 = Args.allocate( std::move(closure_stack.back()) );
			assert(not has_step(r2));
			regs.access(r2).type = reg::type_t::constant;
			// assert(is_WHNF(expression_at(r2))) ?
			p = {r2,r2};
		    }
#endif
		    closure_stack.pop_back();

		    auto [r3,value] = p;

		    set_call(R, r3);
		    set_result_value_for_reg(R);
		    return {R, value};
		}
	    }
	    catch (error_exception& e)
	    {
		if (log_verbose)
		    throw_reg_exception(*this, root_token, R, e, true);
		else
		    throw;
	    }
	    catch (myexception& e)
	    {
		throw_reg_exception(*this, root_token, R, e, true);
	    }
	    catch (const std::exception& ee)
	    {
		myexception e;
		e<<ee.what();
		throw_reg_exception(*this, root_token, R, e, true);
	    }
	}
    }

    std::cerr<<"incremental_evaluate: unreachable?";
    std::abort();
}

void reg_heap::incremental_evaluate_from_call(int S, closure& value)
{
    closure_stack.push_back( value );

    incremental_evaluate_from_call_(S);
}

// The goal here is to merge steps S->S2->S3 => S.  At this point S is known to be changeable.
//
// However, S->S2 and S has performed allocations, then we don't want S2 to unshare those
// if it is unshared.  Therefore once allocations have been performed we don't want to merge
// with any downstream regs that are changeable.
//
// Therefore, if we have any allocations performed in S, create a new reg and delegate to it.
void reg_heap::incremental_evaluate_from_call_(int S)
{
    assert(is_completely_dirty(root_token));

    assert(S > 0);

    while (not closure_stack.back().exp.head().is_index_var() and
	   not is_WHNF(closure_stack.back().exp))
    {
#ifndef NDEBUG
	assert(not closure_stack.back().exp.head().is_a<Trim>());
	assert(closure_stack.back().exp.type() != parameter_type);
#endif

	// If any allocations have been performed by any of the steps merged into S
	// then don't merge any children into S.
	if (not steps[S].created_regs.empty())
	{
	    int r2 = allocate_reg_from_step(S,std::move(closure_stack.back()));
	    closure_stack.back() = closure(index_var(0),{r2});
	    return;
	}

	try
	{
	    RegOperationArgs Args(S, S, *this);
	    auto O = closure_stack.back().exp.head().assert_is_a<Operation>()->op;
	    closure_stack.back() = (*O)(Args);
	
	    total_reductions++;
	    if (Args.used_changeable)
		total_changeable_reductions++;
	}
	catch (error_exception& e)
	{
	    if (log_verbose)
		throw_reg_exception(*this, root_token, closure_stack.back(), e);
	    else
		throw;
	}
	catch (myexception& e)
	{
	    throw_reg_exception(*this, root_token, closure_stack.back(), e);
	}
	catch (const std::exception& ee)
	{
	    myexception e;
	    e<<ee.what();
	    throw_reg_exception(*this, root_token, closure_stack.back(), e);
	}
    }
}

/// These are LAZY operation args! They don't evaluate arguments until they are evaluated by the operation (and then only once).
class RegOperationArgsUnchangeable: public OperationArgs
{
    const int R;

    const closure& current_closure() const {return memory()[R];}

    bool evaluate_changeables() const {return false;}

    /// Evaluate the reg R2, record dependencies, and return the reg following call chains.
    int evaluate_reg_no_record(int R2)
	{
	    return memory().incremental_evaluate_unchangeable(R2);
	}

    /// Evaluate the reg R2, record a dependency on R2, and return the reg following call chains.
    int evaluate_reg_to_reg(int R2)
	{
	    // Compute the value, and follow index_var chains (which are not changeable).
	    return memory().incremental_evaluate_unchangeable(R2);
	}

    const closure& evaluate_reg_to_closure(int R2)
	{
	    int R3 = evaluate_reg_to_reg(R2);
	    if (M.reg_type(R3) == reg::type_t::changeable)
		throw no_context();
	    assert(M.reg_type(R3) == reg::type_t::constant);
	    return M[R3];
	}

    const closure& evaluate_reg_to_closure_(int R2)
	{
	    return evaluate_reg_to_closure(R2);
	}

public:

    void make_changeable()
    {
	throw no_context();
    }

    RegOperationArgsUnchangeable* clone() const {return new RegOperationArgsUnchangeable(*this);}

    RegOperationArgsUnchangeable(int r, reg_heap& m)
	:OperationArgs(m),R(r)
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

int reg_heap::incremental_evaluate_unchangeable_(int R)
{
    assert(regs.is_valid_address(R));
    assert(regs.is_used(R));

#ifndef NDEBUG
    assert(not expression_at(R).head().is_a<expression>());
#endif

    while (1)
    {
	assert(expression_at(R));

	reg::type_t reg_type = regs.access(R).type;

	if (reg_type == reg::type_t::constant or reg_type == reg::type_t::changeable)
	    break;

	else if (reg_type == reg::type_t::index_var)
	{
	    int R2 = closure_at(R).reg_for_index_var();
	    return incremental_evaluate_unchangeable(R2);
	}
	else
	    assert(reg_type == reg::type_t::unknown);

	/*---------- Below here, there is no call, and no value. ------------*/
	const int type = expression_at(R).head().type();
	if (type == index_var_type)
	{
	    regs.access(R).type = reg::type_t::index_var;

	    int R2 = closure_at(R).reg_for_index_var();

	    int R3 = incremental_evaluate_unchangeable( R2 );

	    // If we point to R3 through an intermediate index_var chain, then change us to point to the end
	    if (R3 != R2)
		set_C(R, closure(index_var(0),{R3}));

	    return R3;
	}

	// Check for WHNF *OR* heap variables
	else if (is_WHNF(expression_at(R)))
	    regs.access(R).type = reg::type_t::constant;

#ifndef NDEBUG
	else if (expression_at(R).head().is_a<Trim>())
	    std::abort();
	else if (expression_at(R).type() == parameter_type)
	    std::abort();
#endif

	// 3. Reduction: Operation (includes @, case, +, etc.)
	else
	{
	    auto O = expression_at(R).head().assert_is_a<Operation>()->op;

	    // Although the reg itself is not a modifiable, it will stay changeable if it ever computes a changeable value.
	    // Therefore, we cannot do "assert(not result_for_reg(t,R).changeable);" here.

#ifdef DEBUG_MACHINE
	    string SS = "";
	    SS = compact_graph_expression(*this, R, get_identifiers()).print();
	    string SSS = untranslate_vars(deindexify(trim_unnormalize(closure_at(R))),  
					  get_identifiers()).print();
	    if (log_verbose >= 3)
		dot_graph_for_token(*this, root_token);
#endif

	    try
	    {
		RegOperationArgsUnchangeable Args(R, *this);
		closure value = (*O)(Args);
		total_reductions++;
	
		set_C(R, std::move(value) );
	    }
	    catch (no_context&)
	    {
		regs.access(R).type = reg::type_t::changeable;
		return R;
	    }
	    catch (error_exception& e)
	    {
		throw;
	    }
	    catch (myexception& e)
	    {
		throw_reg_exception(*this, root_token, R, e, false);
	    }
	    catch (const std::exception& ee)
	    {
		myexception e;
		e<<ee.what();
		throw_reg_exception(*this, root_token, R, e, false);
	    }

#ifdef DEBUG_MACHINE
	    //      std::cerr<<"   + recomputing "<<SS<<"\n\n";
	    std::cerr<<"   + Executing statement {"<<O<<"}:  "<<SS<<"\n\n";
#endif
	}
    }

    return R;
}

