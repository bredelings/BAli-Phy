#include "expression2.H"

using boost::shared_ptr;
using std::vector;
using std::string;
using std::pair;
using std::ostream;

string Formula::name_for_index(int index) const
{
  return terms[index].name;
}

bool Formula::has_inputs(int index) const 
{
  bool is_internal = (n_input_indices(index) > 0);
  if (not is_internal)
    assert(not terms[index].op);
  else
    assert(terms[index].op);

  return is_internal;
}

bool Formula::is_constant(int index) const
{
  if (terms[index].constant)
  {
    assert(not is_computed(index));
    return true;
  }
  else
    return false;
}

bool Formula::is_state(int index) const
{
  return (not is_computed(index) and not is_constant(index));
}

bool Formula::is_computed(int index) const
{
  if (has_inputs(index))
  {
    assert(terms[index].op);
    return true;
  }
  else
  {
    assert(not terms[index].op);
    return false;
  }
}

bool Context::index_may_affect_index(int index1, int index2) const
{
  if (values[index2]->computed) 
    return includes(values[index2]->computation->slots_used_order, index1);
  else
    return includes(F->input_indices(index2), index1);
}

bool can_coalesce(shared_ptr<const Object> O1, shared_ptr<const Object> O2)
{
  return not O1->possibly_different_from(*O2);
}

bool Formula::directly_affects(int index1, int index2) const
{
  return includes(affected_indices(index1), index2);
}

bool Formula::directly_affects_in_slot(int index1, int index2, int slot) const
{
  return terms[index2].input_indices[slot] == index1;
}

void Formula::set_directly_affects_in_slot(int index1, int index2, int slot)
{
  if (not directly_affects(index1,index2))
    terms[index1].affected_indices.push_back(index2);

  pair<int,int> p(index2,slot);
  if (not includes(terms[index1].affected_slots, p))
    terms[index1].affected_slots.push_back(p);
}

int Formula::add_term(const Term& t)
{
  int new_index = terms.size();
  terms.push_back(t);
  return new_index;
}

int Formula::add_computed_node(const Operation& o, const vector<int>& indices)
{
  Term t;
  t.op = shared_ptr<Operation>(o.clone());
  t.input_indices = indices;

  // FIXME - check that these indices actually exist

  int new_index = add_term(t);

  for(int slot=0;slot<indices.size();slot++)
  {
    int input_index = indices[slot];
    set_directly_affects_in_slot(input_index,new_index,slot);
  }

  vector<string> input_names;
  for(int slot=0;slot<indices.size();slot++)
    input_names.push_back(terms[indices[slot]].name);
  terms[new_index].name = o.expression(input_names);

  return new_index;
}

int Formula::add_state_node(const string& name)
{
  Term t;
  t.name = name;
  return add_term(t);
}

int Formula::add_state_node(const string& name, const Object& value)
{
  Term t;
  t.name = name;
  return add_term(t);
}

int Formula::add_state_node(const string& name, shared_ptr<const Object> value)
{
  Term t(value);
  t.name = name;
  return add_term(t);
}

int Formula::add_constant_node(const string& name, const Object& value)
{
  return add_constant_node(name, shared_ptr<const Object>(value.clone()));
}

int Formula::add_constant_node(const string& name, shared_ptr<const Object> value)
{
  for(int index=0;index<size();index++)
  {
    if (is_constant(index) and not value->possibly_different_from(*terms[index].default_value))
      return index;
  }

  Term t(value);
  t.name = name;
  t.constant = true;
  return add_term(t);
}

shared_ptr<const Object> Context::evaluate(int index)
{
  value& V = *values[index];

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
    if (V.computed)
      std::cerr<<"revalidating computation "<<F->terms[index].name<<"\n";
  }

  // If the result is not yet marked as computed, then we must run the computation
  // to get a new result.
  if (not V.computed)
  {
    OperationArgs Args(*this, index);

    // recursive calls to evaluate happen in here.
    shared_ptr<const Object> new_result = (*O)(Args);
    V.computation = Args.computation;
    V.computed = true;

    // Only replace the result if (a) the value is different or (b) we can't check that.
    if (not V.result or not can_coalesce(new_result, V.result))
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

  if (F->terms[index].constant) 
    throw myexception()<<"Cannot overwrite constant value!";

  // Change the value of the leaf node
  unshare(values[index]);

  values[index]->result = O;

  vector<int> NOT_known_value_unchanged;
  NOT_known_value_unchanged.push_back(index);
  vector<int> mask(F->size(),0);
  mask[index] = 1;

  for(int i=0;i<NOT_known_value_unchanged.size();i++)
  {
    int index1 = NOT_known_value_unchanged[i];

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

	// ... and it is not known to have the same computation.
	// known_same_computation[index2] = F;

	// Since the computation may be different, it can't be shared.
	unshare(values[index2]);

	// Since the computation may be different, we don't know if the value has changed.
	values[index2]->computed = false;
      }

      // FIXME - recomputation?

      // If we recompute index2, and values[index2]->computed was originally true, then we
      // might set values[index2]->computed back to true.

      // If we recompute this value, then we should not yet consider any of the ancestors
      // of index2 until we know that index2's value has  really changed.

      // We will only recompute values if the old value was valid.
    }
  }
}

Context::Context(const polymorphic_cow_ptr<Formula>& F_)
 :F(F_),
  values(F->size()) 
{
  for(int index=0;index<values.size();index++)
  {
    values[index] = shared_ptr<value>(new value);
    if (not F->has_inputs(index))
      values[index]->computed = true;

    if (F->terms[index].default_value)
      values[index]->result = shared_ptr<Object>(F->terms[index].default_value->clone());
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

string function_expression(const string& name, const vector<string>& arguments)
{
  string output = name;
  output += "(" + join(arguments,',') + ")";
  return output;
}

string Operation::expression(const vector<string>& inputs) const
{
  return function_expression("[unknown]",inputs);
}

string IfThenElse::expression(const vector<string>& inputs) const
{
  if (inputs.size() != 3)
    throw myexception()<<"IfThenElse::expression - got "<<inputs.size()<<" arguments instead of 3.";

  return function_expression("if",inputs);
}

string Add::expression(const vector<string>& inputs) const
{
  if (inputs.size() != 2)
    throw myexception()<<"Add::expression - got "<<inputs.size()<<" arguments instead of 2.";

  return inputs[0] + "+" + inputs[1];
}


int main()
{
  Formula f;
  polymorphic_cow_ptr<Formula> F(f);
  int x = F->add_state_node("X");
  int y = F->add_state_node("Y");
  int z = F->add_state_node("Z");
  int w = F->add_state_node("W");
  int one = F->add_constant_node("1",Double(1));
  F->add_constant_node("1",Double(1));

  int x_times_y = -1;
  {
    vector<int> indices1;
    indices1.push_back(x);
    indices1.push_back(y);
    
    x_times_y = F->add_computed_node(Multiply<Double,Double,Double>(),indices1);
  }

  int x_times_y_plus_one = -1;
  {
    vector<int> indices2;
    indices2.push_back(x_times_y);
    indices2.push_back(one);
    
    x_times_y_plus_one = F->add_computed_node(Add(),indices2);
  }

  int z_gt_1 = -1;
  {
    vector<int> indices2;
    indices2.push_back(z);
    indices2.push_back(one);
    
    z_gt_1 = F->add_computed_node(GreaterThan<Double,Double>(),indices2);
  }

  int x_plus_y = -1;
  {
    vector<int> indices2;
    indices2.push_back(x);
    indices2.push_back(y);
    
    x_plus_y = F->add_computed_node(Add(),indices2);
  }

  int w_2 = -1;
  {
    vector<int> indices2;
    indices2.push_back(w);
    indices2.push_back(w);
    
    w_2 = F->add_computed_node(Multiply<Int,Int,Int>(),indices2);
  }

  int cond = -1;
  {
    vector<int> indices2;
    indices2.push_back(z_gt_1);
    indices2.push_back(x_times_y_plus_one);
    indices2.push_back(w_2);
    
    cond = F->add_computed_node(IfThenElse(),indices2);
  }

  Context CTX1(F);

  CTX1.set_value(x,Double(2));
  CTX1.set_value(y,Double(3));
  CTX1.set_value(z,Double(4));
  CTX1.set_value(w,Int(5));

  std::cout<<"CTX1 = \n"<<CTX1<<"\n";

  Context CTX2 = CTX1;

  std::cout<<"CTX1 = \n"<<CTX1<<"\n";

  shared_ptr<const Object> result = CTX1.evaluate(x_times_y_plus_one);
  CTX1.evaluate(cond);

  std::cout<<"CTX1 = \n"<<CTX1<<"\n";
  std::cout<<"CTX2 = \n"<<CTX2<<"\n";
  std::cout<<"Fiddling X and Y in CTX1...\n";
  CTX1.set_value(x,Double(3));
  CTX1.set_value(y,Double(2));
  std::cout<<"CTX1 = \n"<<CTX1<<"\n";
  std::cout<<"CTX2 = \n"<<CTX2<<"\n";

  result = CTX1.evaluate(x_times_y_plus_one);

  std::cout<<"Fiddling W in CTX2...\n";
  CTX2.set_value(w,Int(-1));
  std::cout<<"CTX2 = \n"<<CTX2<<"\n";

  std::cout<<"Fiddling Z in CTX2...\n";
  CTX2.set_value(z,Double(0));
  result = CTX2.evaluate(cond);
  std::cout<<"CTX2 = \n"<<CTX2<<"\n";
}
