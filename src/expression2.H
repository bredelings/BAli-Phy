/*---------------------------------------------------------------------*/
#include <boost/shared_ptr.hpp>
#include <vector>
#include <string>
#include "cow-ptr.H"
#include "object.H"
#include "util.H"
#include "myexception.H"

using boost::shared_ptr;
using std::vector;
using std::string;


struct Computation;
struct Operation;

struct Formula: public Object
{
  struct Term
  {
    boost::shared_ptr<Operation> op;
    std::vector<int> input_indices;
    std::vector<int> affected_indices;
    std::string name;
    bool constant;
    Term():constant(false) {}
  };

  std::vector<Term> terms;

public:
  Formula* clone() const {return new Formula(*this);}

  int size() const {return terms.size();}

  const std::vector<int>& input_indices(int s) const {return terms[s].input_indices;}
  int n_input_indices(int s) const {return input_indices(s).size();}
  boost::shared_ptr<const Operation> operation(int s) const {return terms[s].op;}

  void add_computed_node(const Operation& o, const std::vector<int>& indices);

  void add_state_node(const std::string& name);
};

// Values
struct Context: public Object
{
  // member type
  struct value
  {
    bool computed;
    boost::shared_ptr<const Computation> computation;
    boost::shared_ptr<const Object> result;

    value():computed(false) { }
  };

  // data members
  polymorphic_cow_ptr<Formula> F;

  std::vector<value> values;

  // member functions
  Context* clone() const {return new Context(*this);}

  boost::shared_ptr<const Object> evaluate(int slot_to_eval);

  void set_value(int index, boost::shared_ptr<const Object> O);

  void set_value(int index, const Object& O);

  Context(const polymorphic_cow_ptr<Formula>& F_):F(F_),values(F->size()) { }
};


struct Computation: public Object
{
  Computation* clone() const {return new Computation(*this);}

  vector<shared_ptr<const Object> > used_values;
  vector<int> slots_used_order;

  Computation(int n_inputs):used_values(n_inputs) { }
};

struct OperationArgs
{
  Context& CTX;
  shared_ptr<Computation> computation;
  int index_of_caller;

  OperationArgs(Context& A, int i)
    :CTX(A), index_of_caller(i) 
  { 
    int n_input_slots = CTX.F->n_input_indices(index_of_caller);

    computation = boost::shared_ptr<Computation>( new Computation(n_input_slots) );
  }

  shared_ptr<const Object> evaluate(int slot)
  {
    int index_to_evaluate = CTX.F->input_indices(index_of_caller)[slot];
    if (not computation->used_values[slot])
    {
      computation->used_values[slot] = CTX.evaluate(index_to_evaluate);
      computation->slots_used_order.push_back(slot);
    }

    assert( computation->used_values[slot] );

    return computation->used_values[slot];
  }

  template <typename T>
  shared_ptr<const T> evaluate_as(int slot)
  {
    return boost::dynamic_pointer_cast<const T>(evaluate(slot));
  }

};

struct Operation: public Object
{
  virtual Operation* clone() const=0;
  virtual shared_ptr<const Object> operator()(OperationArgs& Args) const =0;
};


struct Multiply: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual Multiply* clone() const {return new Multiply(*this);}

  shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    shared_ptr<const Double> x = Args.evaluate_as<Double>(0);
    shared_ptr<const Double> y = Args.evaluate_as<Double>(1);

    shared_ptr<Double> r ( new Double );

    *r = (*x) * (*y);

    return r;
  }
};

struct Add: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual Add* clone() const {return new Add(*this);}

  shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    shared_ptr<const Double> x = Args.evaluate_as<Double>(0);
    shared_ptr<const Double> y = Args.evaluate_as<Double>(1);

    shared_ptr<Double> r ( new Double );

    *r = (*x) + (*y);

    return r;
  }
};

/*-----------------------------------------------------------------------------*/


bool can_coalesce(shared_ptr<const Object> O1, shared_ptr<const Object> O2)
{
  return not O1->possibly_different_from(*O2);
}

void Formula::add_computed_node(const Operation& o, const vector<int>& indices)
{
  Term t;
  t.op = shared_ptr<Operation>(o.clone());
  t.input_indices = indices;

  // FIXME - check that these indices actually exist

  terms.push_back(t);
  for(int i=0;i<indices.size();i++)
  {
    int j = indices[i];
    if (not includes(terms[j].affected_indices, j))
      terms[j].affected_indices.push_back(j);
  }
}

void Formula::add_state_node(const string& name)
{
  Term t;
  t.name = name;
  terms.push_back(t);
}

shared_ptr<const Object> Context::evaluate(int index)
{
  value& V = values[index];
  const vector<int>& input_indices = F->input_indices(index);

  if (V.computation) assert(V.result);

  shared_ptr<const Operation> O = F->terms[index].op;

  if (input_indices.size() == 0)
  {
    assert(V.computed);
    assert(not O);
    return V.result;
  }

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
	V.computed = false;
    }
  }

  // If the result is not yet marked as computed, then we must run the computation
  // to get a new result.
  if (not V.computed)
  {
    OperationArgs Args(*this, index);

    // recursive calls to evaluate happen in here.
    shared_ptr<const Object> new_result = (*O)(Args);
    V.computation = Args.computation;

    // Only replace the result if (a) the value is different or (b) we can't check that.
    if (not V.result or not can_coalesce(new_result, V.result))
      V.result = new_result;
  }
    
  assert(V.result);
  return V.result;
}


void Context::set_value(int index, const Object& O)
{
  shared_ptr<const Object> O2 ( O.clone() );
  set_value(index, O2);
}

void Context::set_value(int index, shared_ptr<const Object> O)
{
  if (F->n_input_indices(index) > 0)
    throw myexception()<<"Cannot overwrite computed nodes!";

  if (F->terms[index].constant and values[index].result) 
    throw myexception()<<"Cannot overwrite constant value!";
  values[index].result = O;
  values[index].computed = true;
  assert(not values[index].computation);

  vector<int> invalidate = F->terms[index].affected_indices;

  for(int i=0;i<invalidate.size();i++)
  {
    int index2 = invalidate[i];
    if (values[index2].computed)
    {
      values[index2].computed = false;
      
      // add all of F->terms[index2].affected_indices 

      // FIXME - in this framework, how do two different objects SHARE
      // a computation??
      
    }
  }
}
