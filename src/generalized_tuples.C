#include "generalized_tuples.H"
#include "util.H"
#include <iostream>
#include <cmath>
using namespace std;

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

node_type_t Formula::node_type(int i) const
{
  return Nodes[i]->node_type();
}

polymorphic_cow_ptr<ValueBase> Formula::get_new_entry_value(int i) const
{
  return Nodes[i]->exemplar;
}

int Formula::get_id_for_index(int index) const
{
  return Nodes[index]->id;
}

int Formula::get_index_for_id(int i) const
{
  for(int index=0;index<size();index++)
    if (get_id_for_index(index) == i)
      return index;
  return -1;
}

int Formula::get_index_for_term_name(const string& name) const
{
  if (not name.size())
    throw myexception()<<"You can't search for a term via an empty name!";

  for(int index=0;index<size();index++)
    if (is_term(index) and expression_for_entry(index) == name)
      return index;
  return -1;
}

string Formula::expression_for_entry(int i) const
{
  return Nodes[i]->expression();
}

int Formula::add_entry(const polymorphic_cow_ptr<ParameterBase>& P, const std::vector<int>& inputs)
{
  string name = P->expression();

  if (get_index_for_id(P->id) != -1)
    throw myexception()<<"* Cannot add entry '"<<name<<"' with id="<<P->id<<": that id is already present.";

  if (P->is_term() and get_index_for_term_name(name) != -1)
    throw myexception()<<"* Cannot add node with name '"<<name<<"': a node with that name already exists.";

  Nodes.push_back(P);
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

int Formula::add_entry(const polymorphic_cow_ptr<ParameterBase>& P)
{
  string name = P->expression();

  if (P->is_term() and get_index_for_term_name(P->expression()) != -1)
    throw myexception()<<"Command add term with name '"<<name<<"': a term with that name already exists.";

  if (get_index_for_id(P->id) != -1)
    throw myexception()<<"Cannot add entry '"<<name<<"' with id="<<P->id<<": that id is already present.";

  vector<polymorphic_cow_ptr<ParameterBase> > entries_to_add;
  entries_to_add.push_back(P);

  while(not entries_to_add.empty())
  {
    polymorphic_cow_ptr<ParameterBase> P2 = entries_to_add.back();
    int id = get_index_for_id(P2->id);

    // work finished if we are already added: nothing to do.  Just remove from stack of remaining work.
    if (id != -1) {
      entries_to_add.pop_back();
      continue;
    }

    bool no_missing_children = true;

    vector<int> input_indices;

    if (polymorphic_cow_ptr<FreeParameterBase> F = dynamic_pointer_cast<FreeParameterBase>(P2))
    {
      // Find the indices for the inputs, and add them to the work stack if they are not found
      for(int i=0;i<F->inputs.size() and no_missing_children;i++)
      {
	int index = get_index_for_id(F->inputs[i]->id);
	input_indices.push_back(index);

	if (index == -1)
	{
	  // we cannot add the current entry
	  no_missing_children = false;

	  // we must put this input to the current entry on the work stack
	  entries_to_add.push_back(F->inputs[i]);
	}
      }

    }

    if (no_missing_children) {
      if (P2->is_term() and entries_to_add.size() != 1)
	throw myexception()<<"Trying to add entry '"<<name<<"' which depend on missing term '"<<P2->expression()<<"'";
      add_entry(P2, input_indices);
      entries_to_add.pop_back();
    }
  }

  int index = get_index_for_id(P->id);
  assert(index != -1);
  return index;
}

bool Formula::is_term(int i) const
{
  return Nodes[i]->is_term();
}

bool Formula::is_constant(int i) const
{
  return Nodes[i]->is_constant();
}

bool Formula::is_computed(int i) const
{
  return Nodes[i]->is_computed();
}


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

  Term<Int> X("X");

  // What should this mean?
  // Parameter<int> X2 = X;

  Term<Double> Y("Y");
  Term<Int> I("I");
  Parameter<Double> Z = X*Y;
  Parameter<Double> W = X*I;
  Parameter<Double> U = apply("pow",pow,X,Z);
  Parameter<Double> A = Constant<Double>(2);

  // Why don't these work?
  // const Parameter<Double>& A6 = 2.0;
  // Parameter<Double> A3 = 2.0;

  Parameter<Double> A2 ( 2 );

  Parameter<Double> I2(I);

  /// FIXME - make this 'add_term' ?
  F->add_entry(X);
  F->add_entry(Y);
  F->add_entry(I);

  F->add_entry(X*Y);
  F->add_entry(W);
  F->add_entry(U);
  F->add_entry(I2);
  F->add_entry(U*A);

  Values V1(*F);

  cout<<"V1 = \n"<<V1.expression()<<endl;

  // set the value of the single state node
  V1[ X ] = 2;
  // state nodes need to be marked up-to-date, and are then assumed to stay that way.
  // FIXME - their should be a general method for marking only StateNodes & InputNodes up-to-date
  V1.mark_up_to_date( V1.get_index(X) );

  // set the value of the single state node
  V1[ Y ]  = 3;
  V1.mark_up_to_date( V1.get_index(Y) );

  V1[ I ] = 3;
  V1.mark_up_to_date( V1.get_index( I ) );

  cout<<"V1 = \n"<<V1.expression()<<endl;
  
  // Try to compute "Z"
  // What if the base values are not up-to-date?
  V1.calculate_value( V1.get_index( I ));
  V1.calculate_value( V1.get_index(I2 ));

  cout<<"V1 = \n"<<V1.expression()<<endl;

  Values V2 = V1;

  cout<<"V2 = \n"<<V2.expression()<<endl;

  V2[ X ] = 3;
  
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
