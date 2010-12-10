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

Parameter<Double> operator*(const Parameter<Double>& p1,const Parameter<Double>& p2)
{
  std::vector<polymorphic_cow_ptr<ParameterBase> > pp;
  pp.push_back(p1.node);
  pp.push_back(p2.node);
  return Expression<Double>(MultiplyValue(2), pp);
}

// Do we need this anymore?
template <typename T, typename U>
Parameter<T> Convert(const Parameter<U>& P)
{
  std::vector<polymorphic_cow_ptr<ParameterBase> > pp;
  pp.push_back(P.node);
  Expression<T>(MultiplyValue(2), pp);
  return Expression<T>(ConversionValue<T,U>(),pp);
}


Parameter<Double> apply(const string& name,double (f)(double, double), const Parameter<Double>& p1,const Parameter<Double>& p2)
{
  std::vector<polymorphic_cow_ptr<ParameterBase> > pp;
  pp.push_back(p1.node);
  pp.push_back(p2.node);
  return Expression<Double>(FunctionValue(name,f), pp);
}

bool Formula::entry_has_name(int i) const
{
  return Node_names[i].size();
}

string Formula::entry_name(int i) const
{
  if (entry_has_name(i))
    return Node_names[i];
  else
    return string("[")+convertToString(i)+"]";
}

node_type_t Formula::node_type(int i) const
{
  return Nodes[i]->node_type();
}

polymorphic_cow_ptr<ValueBase> Formula::get_new_entry_value(int i) const
{
  return polymorphic_cow_ptr<ValueBase>(Nodes[i]->clone());
}

int Formula::get_id_for_index(int index) const
{
  return ids[index];
}

int Formula::get_index_for_id(int i) const
{
  for(int index=0;index<size();index++)
    if (ids[index] == i)
      return index;
  return -1;
}

int Formula::get_index_for_name(const string& name) const
{
  if (not name.size())
    throw myexception()<<"You can't search for any entry via an empty name!";

  for(int index=0;index<size();index++)
    if (entry_name(index) == name)
      return index;
  return -1;
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

int Formula::add_entry(const string& name, const ValueBase& V,int id)
{
  std::vector<int> empty;
  return add_entry(name,V,empty,id);
}

int Formula::add_entry(const string& name, const ValueBase& V, const std::vector<int>& inputs)
{
  return add_entry(name,V,inputs,-1);
}

int Formula::add_entry(const string& name, const ValueBase& V, const std::vector<int>& inputs,int id)
{
  if (name.size() and get_index_for_name(name) != -1)
    throw myexception()<<"Command add node with name '"<<name<<"': a node with that name already exists.";

  Nodes.push_back(polymorphic_cow_ptr<ValueBase>(V));
  Node_names.push_back(name);
  Node_inputs.push_back(inputs);
  Nodes_affected.push_back(vector<affected_index_t>());
  ids.push_back(id);

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

int Formula::add_entry(const std::string& name, const ValueBase& V, const polymorphic_cow_ptr<BoundParameterBase>& P)
{
  return add_entry(name,V,P->id);
}

int Formula::add_entry(const std::string& name, const polymorphic_cow_ptr<FreeParameterBase>& P)
{
  if (name.size() and get_index_for_name(name) != -1)
    throw myexception()<<"Command add node with name '"<<name<<"': a node with that name already exists.";

  if (get_index_for_id(P->id) != -1)
    throw myexception()<<"Cannot add entry '"<<name<<"' with id="<<P->id<<": that id is already present.";

  vector<polymorphic_cow_ptr<FreeParameterBase> > entries_to_add;
  entries_to_add.push_back(P);

  while(not entries_to_add.empty())
  {
    polymorphic_cow_ptr<FreeParameterBase> P2 = entries_to_add.back();
    int id = get_index_for_id(P2->id);

    // work finished if we are already added: nothing to do.  Just remove from stack of remaining work.
    if (id != -1) {
      entries_to_add.pop_back();
      continue;
    }

    // Find the indices for the inputs, and add them to the work stack if they are not found
    bool ok = true;
    vector<int> input_indices;
    for(int i=0;i<P2->inputs.size() and ok;i++)
    {
      int index = get_index_for_id(P2->inputs[i]->id);
      input_indices.push_back(index);

      if (index == -1)
      {
	// we cannot add the current entry
	ok = false;

	polymorphic_cow_ptr<FreeParameterBase> free = dynamic_pointer_cast<FreeParameterBase>(P2->inputs[i]);
	if (not free)
	  std::abort();

	// we must put this input to the current entry on the work stack
	entries_to_add.push_back(free);
      }
    }

    if (ok) {
      if (entries_to_add.size() == 1)
	add_entry(name, *(P2->exemplar), input_indices, P2->id);
      else
	add_entry("", *(P2->exemplar), input_indices, P2->id);
      entries_to_add.pop_back();
    }
  }

  int index = get_index_for_id(P->id);
  assert(index != -1);
  return index;
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
    o<<F->entry_name(i);
    if (F->entry_has_name(i))
      o<<" ["<<i<<"]";
    o<<" = "<<values[i]->result_expression()<<"   ";
    if (completely_up_to_date(i))
      o<<"[*]";
    else if (completely_out_of_date(i))
      o<<"[!]";
    else
      o<<"[*!]";
    if (not values[i].unique())
      o<<"  shared";
    o<<"  id = "<<F->get_id_for_index(i);
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
// Q5: Would it be possible to do so automatically?
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
//   + Can we handle e.g. binding Parameter<DNA> to parameter<alphabet> automatically?
//   + Can we allow automatic conversion from Int to Double, but require explicit conversion from Double to Int?
//
// - Should I make all nodes take the form: Value<T>?
//   - Then it would not be hard to access 
//
// - What is the *conceptual* interface -- independent of the implementation of
//   * modifying individual objects (unsharing)
//   * 
//
// - Is there an elegant way of handling constants?
//   + The use SHOULD be able to manually create ValueBase objects, and submit them with add_entry.
//
// - If I have to dynamically case ValueBase to Value<T> then, I have to know T (the stored type)
//   not just that it can be converted to some type Y.

// Could I make a template function that take a c++ function (or function object)
// and makes a Value node and/or a 
int main(int argc,char* argv[])
{
  boost::shared_ptr<Formula> F(new Formula);

  State<Int> X("X",F);

  // What should this mean?
  // Parameter<int> X2 = X;

  Input<Double> Y("Y",F);
  State<Int> I("I",F);
  Parameter<Double> Z = X*Y;
  Parameter<Double> W = X*I;
  Parameter<Double> U = apply("pow",pow,X,Z);
  Parameter<Double> A = Constant<Double>(2);

  // Why don't these work?
  // const Parameter<Double>& A6 = 2.0;
  // Parameter<Double> A3 = 2.0;

  Parameter<Double> A2 ( 2 );

  Parameter<Double> I2(I);

  F->add_entry("Z",X*Y);
  F->add_entry("W",W);
  F->add_entry("U",U);
  F->add_entry("I2",I2);
  F->add_entry("U*2",U*A);

  Values V1(*F);

  cout<<"V1 = \n"<<V1.expression()<<endl;

  // set the value of the single state node
  V1.get_value_as<Int>(0) = 2;
  // state nodes need to be marked up-to-date, and are then assumed to stay that way.
  // FIXME - their should be a general method for marking only StateNodes & InputNodes up-to-date
  V1.mark_up_to_date(0);

  // set the value of the single state node
  V1.get_value_as<Double>(1) = 3;
  V1.mark_up_to_date(1);

  V1.get_value_as<Int>(2) = 3;
  V1.mark_up_to_date(2);

  cout<<"V1 = \n"<<V1.expression()<<endl;
  
  // Try to compute "Z"
  // What if the base values are not up-to-date?
  V1.calculate_value(3);
  V1.calculate_value(4);

  cout<<"V1 = \n"<<V1.expression()<<endl;

  Values V2 = V1;

  cout<<"V2 = \n"<<V2.expression()<<endl;

  V2.get_value_as<Int>(0) = 3;
  
  cout<<"V2 = \n"<<V2.expression()<<endl;
  V2.calculate_value(3);
  cout<<"V2 = \n"<<V2.expression()<<endl;
  V2.calculate_value(4);
  V2.calculate_value(6);
  V2.calculate_value(14);
  cout<<"V2 = \n"<<V2.expression()<<endl;
  cout<<"V1 = \n"<<V1.expression()<<endl;

  return 0;
}


/* 
 * The user should be able to
 *
 *  Create State and Input objects that exist in a specific formula.
 *
 *  Create Expression objects that do NOT exist in a specific formula.
 *
 *   (All of these objects should have unique global IDs.)
 *
 *  Add an expression object to a formula, and get a new object for the bound version.
 *
 *  Determine if a formula contains a version of an expression.
 *
 *  Add an expression object to a formula, but ONLY if the formula does not already contain that expression!
 * 
 */

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
 * 6. I need to be able to mark certain nodes as (temporarily) ignoring change events.
 *    I can use this to handle cases where X*Y does not change even though X and Y change, for example,
 * 
 * 7. The user should be able to manage invalidation and caching entirely on their own, if they want to.
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


/* Re: Parameter<T> objects, and free-floating expressions.
 *
 * For parameter nodes, I note that storing the relationships between expressions using pointers and a graph 
 * would not really cause problems with unsharing, since nothing is shared at the expression level. Sharing
 * is restricted to the values level.
 *
 * However, eventually there must be an assignment of finalized indices to any free-floating expressions.
 * Therefore, let us say that statenodes and input nodes must be firmly rooted in a formula - therefore,
 * they all have finalized indices, and must be internal associated with a specific formula expression.
 */


/* Re: lambda functions
 * 
 * Supposing I have a tuple g:
 *
 *   f= (lambda k) HKY(k,pi)
 *   g=(pi, v=(v[1],...,v[n]), , models=f(v))
 *
 * Then f does not have to be part of the tuple.  It can remain as a free expression.
 * Only when applying f to (say) v do we get a non-lambda expression.
 *
 * To implement this, we just define another free node that is a lambda node.  So, say
 *
 *   k = Lambda<double>();
 *   HKY(k,pi)
 * 
 * Then HKY is a lambda expression.  To unlambda the expression, we may need to walk the
 * entire expression tree, though.  If we are replacing some lambda node k with a non-lambda
 * node m, then every node upstream of the lambda gets replace with a comparable non-lambda node.
 * (This is just like unsharing entries in a Values tree.)
 *
 */

/* Re: Accessing value entries using BoundParameter<T> {index; (?)Formula?}
 *
 * I want for Bound and Free expression to be able to bind Parameter<T>
 */
