#include "graph_register.H"

using std::string;
using std::vector;
using std::map;
using std::set;

#include "operations.H"
#include "computation.H"

/// These are LAZY operation args! They don't evaluate arguments until they are evaluated by the operation (and then only once).
class RegOperationArgs: public OperationArgs
{
  const int R;

  reg_heap& M;

  const int t;

  int n_allocated;

  int current_token() const {return t;}

  reg_heap& memory() {return M;}

  const closure& current_closure() const {return M[R].C;}

  bool evaluate_changeables() const {return t != 0;}

  /// Evaluate the reg R2, record dependencies, and return the reg following call chains.
  int evaluate_reg_no_record(int R2)
  {
    return M.incremental_evaluate(R2, t);
  }

  /// Evaluate the reg R2, record a dependency on R2, and return the reg following call chains.
  int evaluate_reg_to_reg(int R2)
  {
    // Compute the result, and follow index_var chains (which are not changeable).
    int R3 = M.incremental_evaluate(R2, t);

    if (M.reg_is_changeable(R3) and t)
    {
      // If R2 -> result was changeable, then R -> result will be changeable as well.
      if (not M.reg_is_changeable(R))
      {
	assert( M.access(R).type == reg::type_t::unknown );
	M.make_reg_changeable(R);
      }

      // Note that although R2 is newly used, R3 might be already used if it was 
      // found from R2 through a non-changeable reg_var chain.
      M.set_used_input(t, R, R3);
    }

    return R3;
  }

public:

  int allocate(closure&& C)
  {
    if (C.exp->head->type() == index_var_type)
    {
      int index = convert<const index_var>(C.exp->head)->index;

      int r = C.lookup_in_env( index );
    
      assert(M.is_used(r));

      return r;
    }

    int r = M.push_temp_head();
    M.set_C(r, std::move(C) );
    n_allocated++;
    return r;
  }

  RegOperationArgs* clone() const {return new RegOperationArgs(*this);}

  RegOperationArgs(int r, reg_heap& m, int T)
    :R(r),M(m),t(T), n_allocated(0)
  { 
    // I think these should already be cleared.
    assert(M.computation_for_reg(t,R).used_inputs.empty());
  }

  ~RegOperationArgs()
  {
    for(int i=0;i<n_allocated;i++)
      M.pop_temp_head();
  }
};

expression_ref compact_graph_expression(const reg_heap& C, int R, const map<string, int>&);
expression_ref untranslate_vars(const expression_ref& E, const map<string, int>& ids);
expression_ref untranslate_vars(const expression_ref& E, const map<int,string>& ids);
map<int,string> get_constants(const reg_heap& C, int t);

  /*
   * incremental_eval R1
   *
   *   Note: index_var's never have a result, or call, and are never changeable.
   *   Note: only operations can have a call, and only if the operation uses values of changeable parameters.
   * 
   *   while(not R1.result) do:
   *
   *   If R1.E = (Op or parameter) with call
   *      assert(R1.changeable == true)
   *      R1.call = incremental_evaluate(R1.call)
   *      R1.result = R1.call.result
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
   *      R1.result = R1.E
   *      <break>
   *
   *   If R1.E = modifiable and no call
   *      Complain: modifiable should always have a call!
   *  
   *   If R1.E = Op args (no call)
   *      **Execute reduction**
   *      R1.changeable = reduction changeable
   *      If (changeable)
   *         R1.call = new reg (reduction result)
   *      Else
   *         R1.E = reduction result
   *      <continue>
   *
   *   If R1.E = let expression
   *      R1.E = reduction result
   *      assert(not changeable)
   *      assert(no call)
   *      assert(no result)
   *      <continue>
   *
   *   assert(R1 has a result)
   *   assert(R1.result is WHNF)
   *   assert(R1.result is not a reg_var <*>)
   *   assert(R1.result is not an index_var <*>)
   *   return R1
   */

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
int reg_heap::incremental_evaluate(int R, int t)
{
  assert(not t or is_root_token(t));
  assert(not t or is_completely_dirty(t));
  assert(is_valid_address(R));
  assert(is_used(R));

#ifndef NDEBUG
  assert(not is_a<expression>(access(R).C.exp));
  if (t and reg_has_result(t,R))
  {
    expression_ref E = access_result_for_reg(t,R).exp;
    assert(is_WHNF(E));
    assert(not is_a<expression>(E));
    assert(not is_a<index_var>(E));
  }
  if (is_index_var(access(R).C.exp))
    assert(not reg_has_result(t,R));
#endif

#ifndef NDEBUG
  //  if (not reg_has_result(t,R)) std::cerr<<"Statement: "<<R<<":   "<<access(R).E->print()<<std::endl;
#endif

  while (1)
  {
    assert(access(R).C.exp);

#ifndef NDEBUG
    //    std::cerr<<"   statement: "<<R<<":   "<<access(R).E->print()<<std::endl;
#endif

    if (access(R).type == reg::type_t::constant) break;

    else if (reg_is_changeable(R))
    {
      // Changeable is a normal form, so we are done.
      if (not t) break;

      // We have a result, so we are done.
      if (reg_has_result(t,R)) break;

      // If we know what to call, then call it and use it to set the result
      if (reg_has_call(t,R))
      {
	// This should only be an Operation or a modifiable.
	assert(reg_is_changeable(R));
	
	// Only changeable regs have calls, and changeable regs are in normal form unless evaluate_changeable==true.
	assert(t);
	
	// Evaluate S, looking through unchangeable redirections
	int call = incremental_evaluate(call_for_reg(t,R), t);

	// If computation_for_reg(t,R).call can be evaluated to refer to S w/o moving through any changable operations, 
	// then it should be safe to change computation_for_reg(t,R).call to refer to S, even if R is changeable.
	if (call != call_for_reg(t,R))
	{
	  clear_call_for_reg(t,R);
	  set_call(t, R, call);
	}
	
	// R gets its result from S.
	set_computation_result_for_reg(t, R);
	break;
      }
    }

    /*---------- Below here, there is no call, and no result. ------------*/
    const int type = access(R).C.exp->head->type();
    if (type == index_var_type)
    {
      assert( not reg_is_changeable(R) );

      assert( not reg_has_result(t,R) );

      assert( not reg_has_call(t,R) );

      int index = assert_is_a<index_var>(access(R).C.exp)->index;

      int R2 = access(R).C.lookup_in_env( index );

      int R3 = incremental_evaluate(R2, t);

      // If we point to R3 through an intermediate index_var chain, then change us to point to the end
      if (R3 != R2)
	set_C(R, closure(index_var(0),{R3}));

      return R3;
    }

    // Check for WHNF *OR* heap variables
    else if (is_WHNF(access(R).C.exp))
    {
      access(R).type = reg::type_t::constant;
      if (has_computation(t,R))
	remove_shared_computation(t,R);
    }

#ifndef NDEBUG
    else if (is_a<Trim>(access(R).C.exp))
      std::abort();
    else if (access(R).C.exp->head->type() == parameter_type)
      std::abort();
#endif

    // If we are not evaluating changeable regs, then we shouldn't even get here.
    // A modifiable has a result that is not computed by reducing an expression.
    //       The result must be set.  Therefore, complain if the result is missing.
    else if (type == modifiable_type)
      throw myexception()<<"Reg "<<R<<": Modifiable '"<<access(R).C.exp<<"' with no result?! (token = "<<t<<"   Changeable = "<<reg_is_changeable(R)<<")";

    // Reduction: let expression
    else if (type == let2_type)
    {
      assert( not reg_is_changeable(R) );

      vector<int>& local_env = get_scratch_list();

      const vector<expression_ref>& A = access(R).C.exp->sub;

      const expression_ref& T = A[0];

      int n_bodies = int(A.size())-1;

      local_env = access(R).C.Env;

      int start = local_env.size();

      for(int i=0;i<n_bodies;i++)
	local_env.push_back( push_temp_head() );
      
      // Substitute the new heap vars for the dummy vars in expression T and in the bodies
      for(int i=0;i<n_bodies;i++)
	set_C(local_env[start+i], get_trimmed({A[i+1],local_env}));

      set_C(R, get_trimmed({T, local_env}));

      assert( not reg_is_changeable(R) );

      // Remove the new heap vars from the list of temp heads in reverse order.
      for(int i=0;i<n_bodies; i++)
	pop_temp_head();
      
      release_scratch_list();

      assert(not t or not reg_has_call(t,R) );
      assert(not t or not reg_has_result(t,R) );
    }
    
    // 3. Reduction: Operation (includes @, case, +, etc.)
    else
    {
      if (not has_computation(t,R))
	add_shared_computation(t,R);

      object_ptr<const Operation> O = assert_is_a<Operation>( access(R).C.exp );

      // Although the reg itself is not a modifiable, it will stay changeable if it ever computes a changeable result.
      // Therefore, we cannot do "assert(not computation_for_reg(t,R).changeable);" here.

#ifdef DEBUG_MACHINE
      string SS = "";
      SS = compact_graph_expression(*this, R, get_identifiers())->print();
      string SSS = untranslate_vars(deindexify(trim_unnormalize(access(R).C)),  
				    get_identifiers())->print();
      if (log_verbose)
	dot_graph_for_token(*this, t);
#endif

      try
      {
	RegOperationArgs Args(R, *this, t);
	closure result = (*O)(Args);
	
	// NOTE: While not all used_inputs are E-children, they SHOULD all be E-descendents.
	//       How could we assert that?
	
	// If the reduction doesn't depend on modifiable, then replace E with the result.
	if (not reg_is_changeable(R))
	{
	  // The old used_input slots are not invalid, which is OK since none of them are changeable.
	  assert(not reg_has_call(t,R) );
	  assert(not reg_has_result(t,R));
	  assert(computation_for_reg(t,R).used_inputs.empty());
	  set_C(R, std::move(result) );
	}
	// Otherwise, set the reduction result.
	else
	{
	  // If the reg is changeable and this is token 0, then it is in normal form, and we are done.
	  if (not t)
	  {
	    remove_shared_computation(t,R);
	    return R;
	  }
	  
	  bool result_is_index_var = result.exp->head->type() == index_var_type;

	  int r2=0;
	  if (result_is_index_var)
	  {
	    int index = convert<const index_var>(result.exp->head)->index;

	    r2 = result.lookup_in_env( index );
    
	    assert(is_used(r2));
	  }
	  else
	  {
	    r2 = push_temp_head();
	    set_C(r2, std::move(result) );
	  }
	  int r3 = incremental_evaluate(r2, t);

	  set_call(t, R, r3);
	  set_computation_result_for_reg(t, R);

	  if (not result_is_index_var)
	    pop_temp_head();
	}
      }
      catch (no_context&)
      {
	access(R).type = reg::type_t::changeable;
	return R;
      }
      catch (myexception& e)
      {
	dot_graph_for_token(*this, t);

	string SS  = compact_graph_expression(*this, R, get_identifiers())->print();
	string SSS = unlet(untranslate_vars(
					    untranslate_vars(deindexify(trim_unnormalize(access(R).C)), get_identifiers()),
					    get_constants(*this,t)
					    )
			   )->print();
	std::ostringstream o;
	o<<"evaluating reg # "<<R<<" in token "<<t<<": "<<SSS<<"\n\n";
	e.prepend(o.str());
	throw e;
      }
      catch (const std::exception& e)
      {
	std::cerr<<"evaluating reg # "<<R<<" in token "<<t<<std::endl;
	dot_graph_for_token(*this, t);
	throw e;
      }

#ifdef DEBUG_MACHINE
      //      std::cerr<<"   + recomputing "<<SS<<"\n\n";
      std::cerr<<"   + Executing statement {"<<O<<"}:  "<<SS<<"\n\n";
#endif
    }
  }


#ifndef NDEBUG
  assert(not is_a<index_var>(access(R).C.exp));
  if (reg_has_result(t,R))
  {
    expression_ref E = access_result_for_reg(t,R).exp;
    assert(not is_a<index_var>(E));
    assert(not is_a<expression>(E));
    assert(is_WHNF(E));
  }
#endif

  return R;
}

