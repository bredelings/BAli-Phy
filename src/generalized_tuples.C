#include "generalized_tuples.H"
#include "util.H"
#include <iostream>
#include <cmath>
using namespace std;

int ParameterBase::total=0;

ParameterBase::ParameterBase()
  :id(ParameterBase::total++),
   type(state)
{
}

ParameterBase::ParameterBase(const string& s)
  :name(s),
   id(ParameterBase::total++),
   type(state)
{
}

ParameterBase::ParameterBase(const FormulaNode& fn, const std::vector<cow_ptr<ParameterBase> >& i)
  :id(ParameterBase::total++),
   type(state),
   formula_node(fn),
   inputs(i)
{
}

ParameterBase::ParameterBase(const string& s, const FormulaNode& fn, const std::vector<cow_ptr<ParameterBase> >& i)
  :name(s),
   id(ParameterBase::total++),
   type(state),
   formula_node(fn),
   inputs(i)
{
}

Parameter<double> operator*(Parameter<double>& p1,Parameter<double>& p2)
{
  std::vector<cow_ptr<ParameterBase> > pp;
  pp.push_back(p1.node);
  pp.push_back(p2.node);
  return Parameter<double>(MultiplyNode(2), pp);
}

Parameter<double> apply(double (f)(double, double), Parameter<double>& p1,Parameter<double>& p2)
{
  std::vector<cow_ptr<ParameterBase> > pp;
  pp.push_back(p1.node);
  pp.push_back(p2.node);
  return Parameter<double>(MultiplyNode(2), pp);
}

string Formula::expression_for_entry(int i) const
{
  if (not n_inputs(i)) return "";

  vector<string> input_names;
  for(int j=0;j<n_inputs(i);j++)
    input_names.push_back(entry_name(input_index(i,j)));

  return Nodes[i]->expression(input_names);
}

int Formula::add_entry(const string& name, const FormulaNode& Node)
{
  std::vector<int> empty;
  return add_entry(name,Node,empty);
}

int Formula::add_entry(const string& name, const FormulaNode& Node, const std::vector<int>& inputs)
{
  Nodes.push_back(polymorphic_cow_ptr<FormulaNode>(Node));
  Node_names.push_back(name);
  Node_inputs.push_back(inputs);
  Node_outputs.push_back(vector<int>());

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
    if (not includes(Node_outputs[j],k))
      Node_outputs[j].push_back(k);
  }

  return k;
}

// Question: when do we *implicitly* add objects?
// A1: only when they are temporaries.
//     ... But that would mean IMPLICITLY adding UNNAMED objects!
// A2: only when they are neither State nodes nor Input nodes.
int Formula::add_entry(const ParameterBase& P)
{
  std::abort();
  // Add this formula, then put formulae that are not found on a stack, and add them.
}

FormulaNode::FormulaNode(int n)
{
  for(int j=0;j<n;j++)
    input_names_.push_back(std::string("$")+convertToString(j+1));
}

// An entry (Node) in the ComputedTuple specifies
// - the name (string) of each input.
//   + and thus the number 
// - a method for computing a Value for each node

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

void Values::notify_x_of_change_in_y(int x, int y, const out_of_date_message_t& m)
{
  // if x is already of date, don't bother sending any more message about how its inputs have changed
  if (completely_out_of_date(x)) return;

  // Don't unshare x by calling notify_inputs_out_of_date( ) unless x will change!
  if (values[x]->ignored(m)) return;

  // Notify x of the changes, and find out how x has changed.
  message_list_t x_changes = values[x]->notify_input_out_of_date(*this, m, y);

  record_changes_no_deliver(x, x_changes);
}

void Values::process_messages()
{
  // For each index1 ...
  for(int index1=0; index1<size(); index1++)
  {
    const vector<int>& Node_outputs = F->output_indices(index1);

    // ... with unprocessed messages...
    if (unprocessed_messages[index1].empty()) continue;

    // ... consider each index2 that is directly downstream ...
    for(int j=0;j<Node_outputs.size();j++)
    {
      int index2 = Node_outputs[j];

      assert(index2 > index1);

      // ... and isn't already out of date.
      if (completely_out_of_date(index2)) continue;
      
      message_list_t& index1_all_changes = unprocessed_messages[index1];

      message_list_t index2_all_changes; // or at least all changes resulting from from index1

      // Consider each message from index1
      for(message_list_t::const_iterator m = index1_all_changes.begin(); 
	  m != index1_all_changes.end() and not completely_out_of_date(index2); m++)
      {
	notify_x_of_change_in_y(index2, index1, *(*m));
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
    o<<F->entry_name(i)<<" = "<<values[i]->expression()<<"   ";
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

Values::Values(const Formula& f)
  :values(f.size()),
   F(f),
   unprocessed_messages(f.size())
{
  for(int i=0;i<F->size();i++)
  {
    ValueBase* V = F->get_entry(i).create_new_value();
    values[i] = polymorphic_cow_ptr<ValueBase>(V);
  }
}

std::string MultiplyNode::expression(const vector<string>& args) const
{
  assert(args.size() == n_inputs());

  return join(args,'*');
}

MultiplyNode::MultiplyNode(int i)
  :FormulaNode(i),
   n(i)
{
};

void MultiplyValue::update(const Values& V, const std::vector<int>& mapping)
{
  assert(mapping.size()==n);
  value = 1;
  for(int i=0;i<mapping.size();i++)
  {
    int j = mapping[i];
    if (not V.completely_up_to_date(j)) return;

    value *= V.get_value_as<double>(j);
  }
  up_to_date = true;
}

std::string FunctionNode::expression(const vector<string>& args) const
{
  assert(args.size() == n_inputs());

  return name + "("+join(args,',')+")";
}

FunctionNode::FunctionNode(const string& s, double (*f)(double,double))
  :FormulaNode(2),
   name(s),
   function(f)
{ }

void FunctionValue::update(const Values& V, const std::vector<int>& mapping)
{
  double arg1 = V.get_value_as<double>(mapping[0]);
  double arg2 = V.get_value_as<double>(mapping[1]);

  value = function(arg1, arg2);

  up_to_date = true;
}

FunctionValue::FunctionValue(double (*f)(double,double))
  :function(f)
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
  Parameter<double> X("X");
  Parameter<double> Y("Y");
  Parameter<double> Z = X*Y;
  vector<cow_ptr<ParameterBase> > inputs;
  inputs.push_back(X.node);
  inputs.push_back(Z.node);
  Parameter<double> W("W",FunctionNode("pow",pow),inputs);


  Formula F;
  // Should I make this return a 'FormulaEntry' that could be used in (say) the expression X*Y
  //   or an extra function Multiply(X,Y,Z)?
  // Hmm... How would I handle Multiply(Plus(X,2),Pow(Y,3))?
  //   This kind of expression 
  F.add_entry("X",StateNode<double>());
  F.add_entry("Y",InputNode<double>());
  {
    std::vector<int> inputs(2);
    inputs[0] = 0; // X
    inputs[1] = 1; // Y
    F.add_entry("Z",MultiplyNode(2),inputs);
  }
  {
    std::vector<int> inputs(2);
    inputs[0] = 0; // X
    inputs[1] = 2; // Y
    F.add_entry("W",MultiplyNode(2),inputs);
  }

  Values V1(F);

  cout<<"V1 = \n"<<V1.expression()<<endl;

  // set the value of the single state node
  V1.get_value_as<double>(0) = 2;
  // state nodes need to be marked up-to-date, and are then assumed to stay that way.
  // FIXME - their should be a general method for marking only StateNodes & InputNodes up-to-date
  V1.mark_up_to_date(0);

  // set the value of the single state node
  V1.get_value_as<double>(1) = 3;
  V1.mark_up_to_date(1);

  cout<<"V1 = \n"<<V1.expression()<<endl;
  
  // Try to compute "Z"
  // What if the base values are not up-to-date?
  V1.calculate_value(3);

  cout<<"V1 = \n"<<V1.expression()<<endl;

  Values V2 = V1;

  cout<<"V2 = \n"<<V2.expression()<<endl;

  V2.get_value_as<double>(0) = 3;
  
  cout<<"V2 = \n"<<V2.expression()<<endl;
  V2.calculate_value(3);
  cout<<"V2 = \n"<<V2.expression()<<endl;
  cout<<"V1 = \n"<<V1.expression()<<endl;

  return 0;
}


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
