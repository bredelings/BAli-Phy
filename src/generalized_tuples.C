#include "generalized_tuples.H"
#include "util.H"
#include <iostream>
#include <cmath>
using namespace std;

int ParameterBase::total=0;

ParameterBase::ParameterBase()
  :id(ParameterBase::total++)
{
}

FreeParameterBase::FreeParameterBase()
{ }

FreeParameterBase::FreeParameterBase(const ValueBase& V, const std::vector<polymorphic_cow_ptr<ParameterBase> >& i)
  :exemplar(V),
   inputs(i)
{
}

Parameter<Double> operator*(Parameter<Double>& p1,Parameter<Double>& p2)
{
  std::vector<polymorphic_cow_ptr<ParameterBase> > pp;
  pp.push_back(p1.node);
  pp.push_back(p2.node);
  return Expression<Double>(MultiplyValue(2), pp);
}

Parameter<Double> apply(const string& name,double (f)(double, double), Parameter<Double>& p1,Parameter<Double>& p2)
{
  std::vector<polymorphic_cow_ptr<ParameterBase> > pp;
  pp.push_back(p1.node);
  pp.push_back(p2.node);
  return Expression<Double>(FunctionValue(name,f), pp);
}

node_type_t Formula::node_type(int i) const
{
  return Nodes[i]->node_type();
}

polymorphic_cow_ptr<ValueBase> Formula::get_new_entry_value(int i) const
{
  return polymorphic_cow_ptr<ValueBase>(Nodes[i]->clone());
}

string Formula::expression_for_entry(int i) const
{
  if (not n_inputs(i)) return "";

  vector<string> input_names;
  for(int j=0;j<n_inputs(i);j++)
    input_names.push_back(entry_name(input_index(i,j)));

  return Nodes[i]->formula_expression(input_names);
}

int Formula::add_entry(const string& name, const ValueBase& V)
{
  std::vector<int> empty;
  return add_entry(name,V,empty);
}

int Formula::add_entry(const string& name, const ValueBase& V, const std::vector<int>& inputs)
{
  for(int i=0;i<size();i++)
    if (entry_name(i) == name)
      throw myexception()<<"Command add node with name '"<<name<<"': a node with that name already exists.";

  Nodes.push_back(polymorphic_cow_ptr<ValueBase>(V));
  Node_names.push_back(name);
  Node_inputs.push_back(inputs);
  Nodes_affected.push_back(vector<affected_index_t>());

  // get index of new entry
  int k = size()-1;

  for(int i=0;i<inputs.size();i++)
  {
    int j = inputs[i];
    if (j > k)
      throw myexception()<<"New entry '"<<name<<"' ["<<k<<"] cannot depend on non-existent "<<j<<"-th entry!";
    if (j == k)
      throw myexception()<<"New entry '"<<name<<"' ["<<k<<"] cannot depend on itself!";
  }

  // mark all the nodes used as input that this node is their output
  for(int i=0;i<inputs.size();i++)
  {
    int j = inputs[i];
    Nodes_affected[j].push_back(affected_index_t(k,i));
  }

  return k;
}

bool Formula::is_input_entry(int i) const
{
  return Nodes[i]->is_input_node();
}

bool Formula::is_state_entry(int i) const
{
  return Nodes[i]->is_input_node();
}

bool Formula::is_constant_entry(int i) const
{
  return Nodes[i]->is_input_node();
}

bool Formula::is_computed_entry(int i) const
{
  return Nodes[i]->is_input_node();
}

vector<string> ValueBase::input_names() const 
{
  vector<string> names;
  for(int i=0;i<n_inputs();i++) {
    names.push_back(string("$")+convertToString(i+1));
  }
  return names;
}

void Values::record_changes_no_deliver(int x, message_list_t& x_changes)
{
  // If x has changed completely, then just record a single "everything has changed" message.
  if (completely_out_of_date(x))
  {
    owned_ptr<out_of_date_message_t> mp = claim(new out_of_date_message_t);
    unprocessed_messages[x].clear();
    unprocessed_messages[x].push_back(mp);
  }
  else
  // Collect the changes from this message.
    unprocessed_messages[x].splice(unprocessed_messages[x].end(), x_changes);
}

void Values::notify_x_of_change_in_slot_y(int x, int slot, const out_of_date_message_t& m)
{
  // if x is already of date, don't bother sending any more message about how its inputs have changed
  if (completely_out_of_date(x)) return;

  // Don't unshare x by calling notify_inputs_out_of_date( ) unless x will change!
  {
    // If we don't cast 'values' to a constant, then values[x] will call the non-const operator[]
    // and unshare values[x], which defeats the whole purpose of checking 'ignored'
    const std::vector<polymorphic_cow_ptr<ValueBase> >& const_values = values;

    // Quit before generating a non-const reference to values[x] if values[x] will not change value.
    if (const_values[x]->ignored(m,slot)) return;
  }

  // Notify x of the changes, and find out how x has changed.
  message_list_t x_changes = values[x]->notify_input_out_of_date(*this, m, slot);

  record_changes_no_deliver(x, x_changes);
}

void Values::process_messages()
{
  // For each index1 ...
  for(int index1=0; index1<size(); index1++)
  {
    const vector<affected_index_t>& Nodes_affected = F->affected_indices(index1);

    // ... with unprocessed messages...
    if (unprocessed_messages[index1].empty()) continue;

    // ... consider each index2 that is directly downstream ...
    for(int j=0;j<Nodes_affected.size();j++)
    {
      affected_index_t index2 = Nodes_affected[j];

      assert(index2.index > index1);

      // ... and isn't already out of date.
      if (completely_out_of_date(index2.index)) continue;
      
      message_list_t& index1_all_changes = unprocessed_messages[index1];

      message_list_t index2_all_changes; // or at least all changes resulting from from index1

      for(message_list_t::const_iterator m = index1_all_changes.begin(); 
	  m != index1_all_changes.end() and not completely_out_of_date(index2.index); m++)
      {
	notify_x_of_change_in_slot_y(index2.index, index2.slot, *(*m));
      }
    }
  }
}

void Values::mark_out_of_date(int i)
{
  if (completely_out_of_date(i))
    return;

  values[i]->mark_self_out_of_date();

  unprocessed_messages[i].clear();
  owned_ptr<out_of_date_message_t> mp = claim(new out_of_date_message_t);
  unprocessed_messages[i].push_back(mp);

  process_messages();
}

void Values::calculate_value(int index2)
{
  vector<int> indices_to_validate(1,index2);

  while(not indices_to_validate.empty())
  {
    int index1 = indices_to_validate.back();

    if (completely_up_to_date(index1)) {
      indices_to_validate.pop_back();
      continue;
    }

    const vector<int>& input_indices = F->input_indices(index1);
    if (input_indices.empty())
      throw myexception()<<"State expression "<<F->entry_name(index1)<<" ["<<index1<<"] not up-to-date during computation of "<<F->entry_name(index2)<<" ["<<index2<<"]!";

    bool inputs_ok = true;
    for(int i=0;i<input_indices.size();i++)
    {
      int j = input_indices[i];
      if (not completely_up_to_date(j))
      {
	indices_to_validate.push_back(j);
	inputs_ok = false;
      }
    }

    if (inputs_ok) {
      values[index1]->update(*this,input_indices);
      indices_to_validate.pop_back();
    }
  }
}

string Values::expression() const
{
  ostringstream o;
  for(int i=0;i<size();i++)
  {
    o<<F->entry_name(i)<<" = "<<values[i]->result_expression()<<"   ";
    if (completely_up_to_date(i))
      o<<"[*]";
    else if (completely_out_of_date(i))
      o<<"[!]";
    else
      o<<"[*!]";
    if (not values[i].unique())
      o<<"  shared";
    o<<"\n";
    if (F->n_inputs(i))
      o<<" ["<<F->expression_for_entry(i)<<"]\n";
  }
  return o.str();
}

bool Values::is_input_entry(int i) const
{
  return F->is_input_entry(i);
}

bool Values::is_state_entry(int i) const
{
  return F->is_state_entry(i);
}

bool Values::is_constant_entry(int i) const
{
  return F->is_constant_entry(i);
}

bool Values::is_computed_entry(int i) const
{
  return F->is_computed_entry(i);
}

Values::Values(const Formula& f)
  :values(f.size()),
   F(f),
   unprocessed_messages(f.size())
{
  for(int i=0;i<F->size();i++)
    values[i] = F->get_new_entry_value(i);
}

string MultiplyValue::formula_expression(const vector<string>& args) const
{
  assert(args.size() == n_inputs());

  return join(args,'*');
}

void MultiplyValue::update(const Values& V, const std::vector<int>& mapping)
{
  assert(mapping.size()==n);
  double value = 1;
  for(int i=0;i<mapping.size();i++)
  {
    int j = mapping[i];
    if (not V.completely_up_to_date(j)) return;

    value *= V.get_value_as<Double>(j);
  }

  data = Double(value);

  up_to_date = true;
}

string FunctionValue::formula_expression(const vector<string>& args) const
{
  assert(args.size() == n_inputs());

  return name + "("+join(args,',')+")";
}

void FunctionValue::update(const Values& V, const std::vector<int>& mapping)
{
  double arg1 = V.get_value_as<Double>(mapping[0]);
  double arg2 = V.get_value_as<Double>(mapping[1]);

  double value = function(arg1, arg2);

  data = Double(value);

  up_to_date = true;
}

FunctionValue::FunctionValue(const string& s, double (*f)(double,double))
  :name(s),
   function(f)
{ }

// Some Node's are STATE nodes: that is, these are the inputs to the computed tuple as a function.
// Only these will (ideally) marked as being updated.
// (a) I could insist that every STATE node is part of the tuple...
// (b) Or I could not insist.
//
// (i) I could insist that the STATE node's storage is owned by the tuple...
// (ii) Or I could not insist.


// Questions
// Q1. Should I allow NULL values (e.g. unset states)?  
// A1: Yes.
//
// Q2: How can I do type checking on the inputs?
// A2: 
//
// Q3: Should I use boost::any ?
// Q3-Discussion: Well, this would allow me to make sure that every 'Value' struct is kind of the same...
//
// Q4: Would it be possible to make a TEMPLATE FUNCTION to check the arguments?
// A4: I would think so: check_arguments<double,double,alignment&>(args)....
//
// Q5: Would it be possible to do this automatically?
//
// Q6: How do I deal with variable number of arguments?
// A6a: Well, you could take a 'vector' which is (of course) of variable length...
// A6b: How does this relate to making the (fixed,integer) number of arguments part a member variable,
//      as in MultiplyValue?
//
// Q7: Should I make a non-const accessor that automagically invalidates values??
// A7...

//FIXME - Make sure that each statenode can display its value, at least for debugging, if not for serialization...

// Issues
// - How do we handle conversion operators?
// - Should I make all nodes take the form: Value<T>?
//   - Then it would not be hard to access 
// - What is the *conceptual* interface -- independent of the implementation of
//   * modifying individual objects (unsharing)
//   * 
// - Would it be possible to define a new node (e.g. both Value and the FormulaNode) at once?
// - Is there an elegant way of handling constants?
// - If I have to dynamically case ValueBase to Value<T> then, I have to know T (the stored type)
//   not just that it can be converted to some type Y.

// Could I make a template function that take a c++ function (or function object)
// and makes a Value node and/or a 
int main(int argc,char* argv[])
{
  boost::shared_ptr<Formula> F(new Formula);

  State<Double> X("X",F);
  Input<Double> Y("Y",F);
  Parameter<Double> Z = X*Y;

  vector<polymorphic_cow_ptr<ParameterBase> > inputs;
  inputs.push_back(X.node);
  inputs.push_back(Z.node);
  Expression<Double> W(FunctionValue("pow",pow),inputs);

  // So... what would I make a FreeParameter into a BOundParameter after it was created?

  // Should I make this return a 'FormulaEntry' that could be used in (say) the expression X*Y
  //   or an extra function Multiply(X,Y,Z)?
  // Hmm... How would I handle Multiply(Plus(X,2),Pow(Y,3))?
  //   This kind of expression 
   {
    std::vector<int> inputs(2);
    inputs[0] = 0; // X
    inputs[1] = 1; // Y
    F->add_entry("Z",MultiplyValue(2),inputs);
  }
  {
    std::vector<int> inputs(2);
    inputs[0] = 0; // X
    inputs[1] = 2; // Y
    F->add_entry("W",MultiplyValue(2),inputs);
  }
  {
    std::vector<int> inputs(2);
    inputs[0] = 0; // X
    inputs[1] = 1; // Y
    F->add_entry("U",FunctionValue("pow",pow),inputs);
  }

  Values V1(*F);

  cout<<"V1 = \n"<<V1.expression()<<endl;

  // set the value of the single state node
  V1.get_value_as<Double>(0) = 2;
  // state nodes need to be marked up-to-date, and are then assumed to stay that way.
  // FIXME - their should be a general method for marking only StateNodes & InputNodes up-to-date
  V1.mark_up_to_date(0);

  // set the value of the single state node
  V1.get_value_as<Double>(1) = 3;
  V1.mark_up_to_date(1);

  cout<<"V1 = \n"<<V1.expression()<<endl;
  
  // Try to compute "Z"
  // What if the base values are not up-to-date?
  V1.calculate_value(3);
  V1.calculate_value(4);

  cout<<"V1 = \n"<<V1.expression()<<endl;

  Values V2 = V1;

  cout<<"V2 = \n"<<V2.expression()<<endl;

  V2.get_value_as<Double>(0) = 3;
  
  cout<<"V2 = \n"<<V2.expression()<<endl;
  V2.calculate_value(3);
  cout<<"V2 = \n"<<V2.expression()<<endl;
  V2.calculate_value(4);
  cout<<"V2 = \n"<<V2.expression()<<endl;
  cout<<"V1 = \n"<<V1.expression()<<endl;

  return 0;
}


/* The user should be able to
 *
 * Create State and Input objects that exist in a specific formula.
 *
 * Create Expression objects that do NOT exist in a specific formula.
 *
 * (All of these objects should have unique global IDs.)
 *
 * Add an expression object to a formula, and get a new object for the bound version.
 *
 * Determine if a formula contains a version of an expression
 *
 * Add an expression object to a formula, but ONLY if the formula does not already contain that expression!
 * 
 */

// Do we really want unbound expressions?
// These should be able to express things like X[f1] * Y[f2].
// 
// On the other hand, having unbound expressions should prevent wasting indices on objects that
// we end up not using.
//
// Now, it would seem that each object has exactly one Formula that it can live in, given that all of its
// state nodes && input nodes must be in the same formula.

/*
 * The user should be able to
 * 1. [OK] Do something to an object: it will then be marked out of date, along w/ its descendants.
 *
 * 2. [OK] Do something to an object: it will then NOT be marked out of date.
 *     The user will send a specific message about what has (and, by implication, has not) changed.
 *     For example, we might wish to do change x and y, but x*y is NOT going to be
 *      out of date.
 *     So... we might wish to supress signals from x to specific downstream targets z.
 *
 * 3. Save an old value of an object (e.g. conditional likelihood).
 *    Restore the old value, and assert that the object is now up-to-date.
 *    (How about restoring downstream effects?  Typically we handle this by copying the ENTIRE OBJECT.)
 *    Perhaps we could set an entry value to equal an entry value in another object,
 *     thus RE-SHARING that StateValue... and possibly all RE-SHARING all the downstream effects as well!
 * 
 * 4. Dynamically change what depends on what.
 *    For example, when computing likelihoods, the model actually changes!
 *
 * 5. Use Formula's and their associated Values in an invisible way, inside an object.
 *    Example 1: I could have a Tree that is a function of (a) a topology and (b) a branch-length vector.
 *    Example 2: I could use the Computed Tuple framework to implement cached 
 *
 * 6. I need a modify_no_invalidate accessor for the values!
 * 
 * 7. The user should be able to manage invalidation and caching entirely on their own, if they want to!
 *
 * 8. Add one formula as an entry in another.  This will be useful for submodels.
 */

/*
 * The user should NOT be able to
 *
 * 1. Access the ValueBase or  Value<T> objects directly, unless...
 *    (a) this is needed for saving & restoring.
 *
 */

/* Re: not duplicating functionality between an XNode and an XValue
  So, if I use the functionnode entirely as a placeholder, then I would only have to define one object per
  function.

  How would I handle computed nodes?
  - the computed value object would hold any necessary state: it would not need to be duplicated in the formulanode.
  - 

  But, how would I handle the create_new_object type?
  Well, I could make each functionnode actually keep around 1 object of the appropriate type: then it would
   hold all of its own state that way.



 This would allow the Constant node an obvious way to hold a constant.  It would allow a MultiplyNode a way of
  determining the number of inputs that it should have.

  But, how would it handle statenodes 
 */


/* Re: parameter<T> objects, and free-floating expressions.
 *
 * For parameter nodes, I note that storing the relationships between expressions using pointers and a graph 
 * would not really cause problems with unsharing, since we aren't sharing, here.
 *
 * However, eventually there must be an assignment of finalized indices to any free-floating expressions.
 * Therefore, let us say that statenodes and input nodes must be firmly rooted in a formula - therefore,
 * they all have finalized indices, and must be internal associated with a specific formula expression.
 */


/* Re: lambda functions
 * 
 * Supposing I have a tuple
 *
 * g=(pi, v=(v[1],...,v[n]), f= (lambda k) HKY(k,pi), models=f(v))
 *
 * Q1. Now, suppose I change v[1] only.  Is it possible to recompute f(v[1]) only?
 * A1. Sure, actually, this wouldn't be very hard.  No matter what f is, if only v1
 *     changes, then only f(v1) needs to be invalidated.
 *     Now, this could be implemented either 
 *     (i) by making f(v1),f(v2),.etc separate objects
 *     (ii) by making v1 pass a message "only element 1 has changed".
 *
 * Q2. What kind of object is f?  
 * A2. Well, presumably a lambda k is ALSO a formula.  It needs to be an Object too.
 *     And as a formula, we could have k be an input node, whereas pi would be 
 *     locally a state node, and globally, a reference to pi in the larger tuple g.
 *
 * Q3. The real question, then is how Values of f relate to values of g.
 *     
 *     Suppose that instead of HKY(k,pi) we had HKY(k+(x+y),pi), and x and y are in g
 *     but x+y is not.  It would make sense to say that f depends on x+y, and make an x+y
 *     entry in g.
 *
 *     Now, how do we *represent* values of f, as well as results of applying f to (say) 2, n
 *     along with their cached intermedate results like (in this case) k+(x+y)?
 *
 *     The k+(x+y) value isn't in g at all, since it depends on k.
 *
 *     How do we represent values of f?  How about values of f(2)?
 *
 */
