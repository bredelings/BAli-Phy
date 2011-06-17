#include "context.H"
#include "computation.H"
#include "formula.H"
#include "myexception.H"
#include "util.H"
#include "operation.H"

using boost::shared_ptr;
using std::vector;
using std::string;
using std::pair;
using std::ostream;

bool Context::index_may_affect_index(int index1, int index2) const
{
  if (values[index2]->computed) 
    return includes(values[index2]->computation->slots_used_order, index1);
  else
    return includes(F->input_indices(index2), index1);
}

shared_ptr<const Object> Context::evaluate(int index)
{
  value& V = *values[index];

  const vector<int>& input_indices = F->input_indices(index);

  if (V.computation) assert(V.result);

  // If we are a constant, or named_parameter_expression, or.... ?
  if (input_indices.size() == 0)
  {
    if (not V.computed)
      throw myexception()<<"Evaluating term "<<F->terms[index].name<<" (index = "<<index<<"): leaf node is not marked up-to-date!";
    if (not V.result)
      throw myexception()<<"Evaluating term "<<F->terms[index].name<<" (index = "<<index<<"): leaf node value has not been set!";

    return V.result;
  }

  // If the expression is a function expression...
  shared_ptr<const Function> f = F->function(index);
  if (f)
  {
    if (not V.computed)
    {
      vector<shared_ptr<const expression> > args(input_indices.size());
      for(int i=0;i<args.size();i++)
      {
	shared_ptr<const Object> arg_result = evaluate(input_indices[i]);
	shared_ptr<const expression> exp_result = dynamic_pointer_cast<const expression>(arg_result);
	if (exp_result)
	  args[i] = exp_result;
	else
	  args[i] = shared_ptr<const expression>(new constant_expression(arg_result));
      }

      V.result = shared_ptr<const Object>(new function_expression(f,args));
      V.computed = true;
    }

    assert(V.computed);
    assert(V.result);
    return V.result;
  }

  // Hey, how about a model expression?
  // Hey, how about a tuple expression?

  // Otherwise the expression must be a op expression
  shared_ptr<const Operation> O = F->operation(index);
  assert(O);
  
  // First try to validate our old computation, if possible
  if (not V.computed and V.computation)
  {
    const vector<int>& slots_used = V.computation->slots_used_order;

    // The computation is assumed true, unless any of the slots end up
    // having different values
    V.computed = true;

    for(int i=0;V.computed and i < slots_used.size();i++)
    {
      int slot = slots_used[i];

      // We must first evaluate each used argument.

      // By evaluating them in the same order in which they were used, we guarantee
      //  that this evaluation will not be wasted.
      shared_ptr<const Object> v = evaluate(input_indices[slot]);

      // If the value is not the same as the value used to compute the previous result
      //   they we have to redo the computation.
      if (v != V.computation->used_values[slot])
      //FIXME: Should we use v->maybe_not_equals( ) above?
	V.computed = false;
    }
    if (V.computed)
      std::cerr<<"revalidating computation "<<F->terms[index].name<<"\n";
  }

  // If the result is not yet marked as computed, then we must run the computation
  // to get a new result.
  if (not V.computed)
  {
    ContextOperationArgs Args(*this, index);

    // recursive calls to evaluate happen in here.
    shared_ptr<const Object> new_result = (*O)(Args);
    V.computation = Args.computation;
    V.computed = true;

    // Only replace the result if (a) the value is different or (b) we can't check that.
    if (not V.result or new_result->maybe_not_equals(*V.result))
      V.result = new_result;

    std::cerr<<"recomputing "<<F->terms[index].name<<"\n";
  }
    
  assert(V.result);
  return V.result;
}


void Context::set_value(int index, const Object& O)
{
  shared_ptr<const Object> O2 ( O.clone() );
  set_value(index, O2);
}

// A node is "computed" (i.e. V.computed) iff we know the value for this node.
// A value can only be "computed" iff all its (used) upstream nodes are, too.
//     Because if one of its (used) upstream nodes wasn't up-to-date, then we wouldn't KNOW if
//     this node's computation could be re-used, and therefore we wouldn't know if was up-to-date.

// A node can be shared only if we know that the computations are the same and the values are the same.

// Q1. Can a node's computation be reused if an upstream node's computation is not re-used?
// A1. Yes.  If any intermediate node has the same value, then these values may lead to re-using
//     the computation of the current node.
//
// Q2. Can a node be shared when an upstream index is unshared?
// A2. Yes.  If any intermediate node has all inputs with the same value, then that
//     node could be re-shared.

void Context::set_value(int index, shared_ptr<const Object> O)
{
  if (F->has_inputs(index))
    throw myexception()<<"Cannot overwrite computed nodes!";

  if (F->is_constant(index))
    throw myexception()<<"Cannot overwrite constant value!";

  // FIXME: improve function by checking first if the new value is different?
  // If so, then return here.

  // Change the value of the leaf node
  unshare(values[index]);

  values[index]->result = O;

  values[index]->computed = true;

  // A list of indices that cannot (w/o recomputing) be known to be unchanged
  vector<int> NOT_known_value_unchanged;

  // A random access version of NOT_known_value_unchanged
  vector<int> mask(F->size(),0);

  // The index that we just altered cannot be known to be unchanged.
  NOT_known_value_unchanged.push_back(index);
  mask[index] = 1;

  // For each index1 that cannot (w/o recomputing) be known to be unchanged...
  for(int i=0;i<NOT_known_value_unchanged.size();i++)
  {
    int index1 = NOT_known_value_unchanged[i];

    // ... consider each downstream index2 that has index1 in slot2 of its computation (possibly unused).
    for(int j=0;j<F->n_affected_indices(index1);j++)
    {
      pair<int,int> index_slot2 = F->affected_slots(index1)[j];
      int index2 = index_slot2.first;
      int slot2 = index_slot2.second;

      // This one already marked NOT known_value_unchanged
      if (mask[index2]) continue;

      // If index2 is not known to have identical USED inputs ...
      if (not values[index2]->computed or values[index2]->computation->used_values[slot2])
      {
	// ... then it is not known to have identical outputs
	NOT_known_value_unchanged.push_back(index2);
	mask[index2] = 1;

	// Since the computation may be different, it can't be shared.
	unshare(values[index2]);

	// Since the computation may be different, we don't know if the value has changed.
	values[index2]->computed = false;
      }

      // FIXME - Could we recompute indices whose value may have changed because the inputs may be different?

      // If we recompute index2, and values[index2]->computed was originally true, then we
      // might set values[index2]->computed back to true.

      // If we recompute this value, then we should not yet consider any of the ancestors
      // of index2 until we know that index2's value has  really changed.

      // We will only recompute values if the old value was valid.
    }
  }
}

void Context::set_value(const std::string& var, const Object& O)
{
  int index = F->find_expression(var);
  return set_value(index,O);
}

void Context::set_value(const std::string& var, shared_ptr<const Object> O)
{
  int index = F->find_expression(var);
  return set_value(index,O);
}

int Context::size() const
{
  return F->size();
}

Context::Context(const polymorphic_cow_ptr<Formula>& F_)
 :F(F_),
  values(F->size()) 
{
  for(int index=0;index<values.size();index++)
  {
    values[index] = shared_ptr<value>(new value);

    if (F->terms[index].default_value) {
      assert(not F->has_inputs(index));
      values[index]->result = shared_ptr<Object>(F->terms[index].default_value->clone());
      values[index]->computed = true;
    }
  }
}

ostream& operator<<(ostream& o, const Context& C)
{
  for(int index=0;index<C.size();index++)
  {
    o<<index<<" "<<C.F->terms[index].name<<" = ";
    if (C.values[index]->result)
      o<<C.values[index]->result->print();
    else
      o<<"null";
    if (C.F->is_constant(index))
      o<<" [constant]";
    else
      o<<"           ";
    if (not C.values[index].unique())
      o<<" [shared]";
    else
      o<<"         ";
    if (C.values[index]->computed)
      o<<" [computed]";
    else
      o<<"           ";
    o<<"\n";
  }
  return o;
}

