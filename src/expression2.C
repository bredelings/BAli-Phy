#include "expression2.H"

using boost::shared_ptr;
using std::vector;
using std::string;
using std::pair;
using std::ostream;

bool term_ref::is_state() const {return F->is_state(index);}
bool term_ref::is_constant() const {return F->is_constant(index);}
bool term_ref::is_computed() const {return F->is_computed(index);}
string term_ref::print() const {return F->name_for_index(index);}
term_ref::term_ref(int i,const Formula& f):index(i),F(f.clone()) {}

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

term_ref Formula::add_term(const Term& t)
{
  int new_index = terms.size();
  terms.push_back(t);
  return term_ref(new_index,*this);
}

term_ref Formula::add_computed_node(const Operation& o, const vector<int>& indices)
{
  // compute the name of node we might add
  vector<string> input_names;
  for(int slot=0;slot<indices.size();slot++)
    input_names.push_back(terms[indices[slot]].name);

  Term t;
  t.op = shared_ptr<Operation>(o.clone());
  t.name = o.expression(input_names);
  t.input_indices = indices;

  // avoid adding duplicate calculations
  for(int index=0; index<size(); index++)
    if ((indices == terms[index].input_indices) and (typeid(o) == typeid(*terms[index].op)))
	return term_ref(index,*this);

  // FIXME - check that these indices actually exist

  term_ref new_index = add_term(t);

  for(int slot=0;slot<indices.size();slot++)
  {
    int input_index = indices[slot];
    set_directly_affects_in_slot(input_index,new_index,slot);
  }

  return new_index;
}

term_ref Formula::add_state_node(const string& name)
{
  Term t;
  t.name = name;
  return add_term(t);
}

term_ref Formula::add_state_node(const string& name, const Object& value)
{
  Term t;
  t.name = name;
  return add_term(t);
}

term_ref Formula::add_state_node(const string& name, shared_ptr<const Object> value)
{
  Term t(value);
  t.name = name;
  return add_term(t);
}

term_ref Formula::add_constant_node(const Object& value)
{
  return add_constant_node(value.print(), shared_ptr<const Object>(value.clone()));
}

term_ref Formula::add_constant_node(const string& name, const Object& value)
{
  return add_constant_node(name, shared_ptr<const Object>(value.clone()));
}

term_ref Formula::add_constant_node(shared_ptr<const Object> value)
{
  return add_constant_node(value->print(), shared_ptr<const Object>(value->clone()));
}

term_ref Formula::add_constant_node(const string& name, shared_ptr<const Object> value)
{
  for(int index=0;index<size();index++)
  {
    if (is_constant(index) and not value->possibly_different_from(*terms[index].default_value))
      return term_ref(index,*this);
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

string function_expression_(const string& name, const vector<string>& arguments)
{
  string output = name;
  output += "(" + join(arguments,", ") + ")";
  return output;
}

string Operation::expression(const vector<string>& inputs) const
{
  return function_expression_("[unknown]",inputs);
}

string IfThenElse::expression(const vector<string>& inputs) const
{
  if (inputs.size() != 3)
    throw myexception()<<"IfThenElse::expression - got "<<inputs.size()<<" arguments instead of 3.";

  return function_expression_("if",inputs);
}

struct expression: public Object
{
  polymorphic_cow_ptr<Formula> F;

  // an expression can be
  // - a term reference
  // - a constant (probably one w/o an index so far)
  // - an op taking expressions as arguments.
  // - a dummy var
  // - a lambda-expression binding dummies in its arguments

  expression* clone() const =0;

  virtual int n_args() const {return 0;}
  
  virtual int highest_unused_dummy() const {return 0;}

  // return NULL if no change
  virtual shared_ptr<const expression> substitute(int dummy, shared_ptr<const expression> E) const
  {
    return shared_ptr<const expression>();
  }

  virtual std::string print() const = 0;

  shared_ptr<const expression> apply(shared_ptr<const expression> E) const;

  shared_ptr<const expression> apply(const expression& E) const;

  expression() {}
  virtual ~expression() {}
};

shared_ptr<const expression> substitute(shared_ptr<const expression> E1, int dummy, shared_ptr<const expression> E2);

// a constant expression
struct constant_expression: public expression
{
  shared_ptr<const Object> value;
  constant_expression* clone() const {return new constant_expression(*this);}

  std::string print() const {return value->print();}
  constant_expression(const Object& o): value(o.clone()) {}
  constant_expression(shared_ptr<const Object> v): value(v) {}
};

// a term reference expression
struct term_ref_expression: public expression
{
  term_ref term;
  term_ref_expression* clone() const {return new term_ref_expression(*this);}
  std::string print() const {return term.print();}
  term_ref_expression(const term_ref& r):term(r) {}
  term_ref_expression(int i, polymorphic_cow_ptr<Formula> f):term(i,f) {}
};

// a dummy variable expression
struct dummy_expression: public expression
{
  int index;

  virtual int highest_unused_dummy() const {return index+1;}

  virtual shared_ptr<const expression> substitute(int dummy, shared_ptr<const expression> E) const
  {
    if (index == dummy) 
      return E;
    else
      return shared_ptr<const expression>();
  }

  dummy_expression* clone() const {return new dummy_expression(*this);}
  std::string print() const {return string("#")+convertToString(index);}
  dummy_expression(int i):index(i) {}
};

// a function expression
struct function_expression: public expression
{
  // what is the top-level operation?
  boost::shared_ptr<const Operation> op;

  // what expressions should be plugged in to each slot in the op?
  std::vector< shared_ptr<const expression> > args;

  int highest_unused_dummy() const 
  {
    int highest = 0;
    for(int i=0;i<args.size();i++)
      highest = std::max(highest, args[i]->highest_unused_dummy());
    return highest;
  }

  shared_ptr<const expression> substitute(int dummy, shared_ptr<const expression> E) const
  {
    std::vector< shared_ptr<const expression> > new_args(args.size());
    bool change = false;
    for(int i=0;i<args.size();i++)
    {
      new_args[i] = ::substitute(args[i], dummy, E);
      if (new_args[i] != args[i])
	change = true;
    }

    shared_ptr<const expression> result;

    if (change)
      result = shared_ptr<const expression>(new function_expression(op,new_args));

    return result;
  }

  function_expression* clone() const {return new function_expression(*this);}

  std::string print() const 
  {
    vector<string> arg_names;
    for(int i=0;i<args.size();i++)
      arg_names.push_back( args[i]->print() );

    return op->expression(arg_names);
  }

  function_expression(const Operation& O,const vector< shared_ptr<const expression> >& A)
    :op(O.clone()),
     args(A)
  { }

  function_expression(shared_ptr<const Operation> O,const vector< shared_ptr<const expression> >& A)
    :op(O->clone()),
     args(A)
  { }
};

struct lambda_expression: public expression
{
  dummy_expression dummy_variable;
  
  shared_ptr<const expression> quantified_expression;
  
  int highest_unused_dummy() const 
  {
    return quantified_expression->highest_unused_dummy();
  }
  
  // FIXME - If we're substituting a lambda expression we have to rename its dummys to make sure
  //         there is no overlap.
  shared_ptr<const expression> substitute(int dummy, shared_ptr<const expression> E) const;

  lambda_expression* clone() const {return new lambda_expression(*this);}

  std::string print() const 
  {
    return string("(lambda ") + dummy_variable.print() + ")(" + quantified_expression->print() + ")";
  }

  lambda_expression(int dummy, shared_ptr<const expression> E)
    :dummy_variable(dummy),
     quantified_expression(E)
  { }

  lambda_expression(const Operation& O)
    :dummy_variable(0)
  {
    int n = O.n_args();
    assert(n != -1);

    vector< shared_ptr<const expression> > A;
    for(int i=0;i<n;i++)
      A.push_back(shared_ptr<const expression>(new dummy_expression(i)));

    shared_ptr<const expression> E(new function_expression(O, A));

    for(int i=n-1;i>0;i--)
      E = shared_ptr<const expression>(new lambda_expression(i,E));

    quantified_expression = E;
  }
};

shared_ptr<const expression> lambda_expression::substitute(int dummy, shared_ptr<const expression> E) const
{
  if (dummy_variable.index == dummy)
    throw myexception()<<"Trying to substitution for dummy "<<dummy<<" in lambda express that quantifies it!";

  shared_ptr<const expression> result = quantified_expression->substitute(dummy,E);
  
  if (not result) return result;
  
  return shared_ptr<const expression>(new lambda_expression(dummy_variable.index,result));
}

shared_ptr<const expression> substitute(shared_ptr<const expression> E1, int dummy, shared_ptr<const expression> E2)
{
  shared_ptr<const expression> E3 = E1->substitute(dummy,E2);
  if (E3) 
    return E3;
  else
    return E1;
}

shared_ptr<const expression> apply(const expression& E,shared_ptr<const expression> arg)
{
  const lambda_expression* lambda = dynamic_cast<const lambda_expression*>(&E);
  if (not lambda)
    throw myexception()<<"Too many arguments to expression "<<E.print()<<".  (Is this a function at all?)";

  return substitute(lambda->quantified_expression, lambda->dummy_variable.index, arg);
}

shared_ptr<const expression> apply(const expression& E,const expression& arg)
{
  return apply(E,shared_ptr<const expression>(arg.clone()));
}

shared_ptr<const expression> apply(shared_ptr<const expression> E,
				   shared_ptr<const expression> arg)
{
  return apply(*E,arg);
}

shared_ptr<const expression> apply(shared_ptr<const expression> E,
				   const expression& arg)
{
  return apply(*E,shared_ptr<const expression>(arg.clone()));
}

shared_ptr<const expression> apply(shared_ptr<const expression> E,
				   const vector<shared_ptr<const expression> > args,
				   int i)
{
  shared_ptr<const expression> result1 = apply(E,args[i]);

  if (i<args.size())
    result1 = apply(result1, args, i+1);

  return result1;
}

shared_ptr<const expression> apply(shared_ptr<const expression> E,
				   const vector<shared_ptr<const expression> > args)
{
  return apply(E,args,0);
}

shared_ptr<const expression> expression::apply(shared_ptr<const expression> arg) const
{
  return ::apply(*this,arg);
}

shared_ptr<const expression> expression::apply(const expression& arg) const
{
  return ::apply(*this,arg);
}

struct expression_ref: public shared_ptr<const expression>
{
  expression_ref operator()(const expression_ref& arg) const
  {
    return apply(*this,arg);
  }

  expression_ref operator()(const expression_ref& arg1, const expression_ref& arg2) const
  {
    return apply(apply(*this,arg1),arg2);
  }

  expression_ref operator()(const expression_ref& arg1, 
			    const expression_ref& arg2,
			    const expression_ref& arg3) const
  {
    return apply(apply(apply(*this,arg1),arg2),arg3);
  }

  expression_ref(expression* v)
    :shared_ptr<const expression>(v) 
  {}

  expression_ref(const shared_ptr<const expression>& v)
    :shared_ptr<const expression>(v) 
  {}

  expression_ref(const term_ref& t)
    :shared_ptr<const expression>(new term_ref_expression(t)) 
  {}

  expression_ref(const Operation& o)
    :shared_ptr<const expression>(new lambda_expression(o))
  {}
};

template <typename T>
struct typed_expression_ref: expression_ref
{
public:
  typed_expression_ref(expression* v): expression_ref(v) {}
  typed_expression_ref(const shared_ptr<const expression>& v): expression_ref(v) {}
  typed_expression_ref(const term_ref& t): expression_ref(t) {}
};

term_ref Formula::add_computed_node(const expression_ref& e)
{
  shared_ptr<const lambda_expression> lambda = dynamic_pointer_cast<const lambda_expression>(e);
  if (lambda)
    throw myexception()<<"Lambda expressions cannot currently be calculated";

  shared_ptr<const constant_expression> constant = dynamic_pointer_cast<const constant_expression>(e);
  if (constant)
    return add_constant_node(constant->value->print(), constant->value);
  
  shared_ptr<const term_ref_expression> tr = dynamic_pointer_cast<const term_ref_expression>(e);
  if (tr)
    return tr->term;
  
  shared_ptr<const function_expression> func = dynamic_pointer_cast<const function_expression>(e);
  if (func)
  {
    vector<int> arg_indices;
    for(int i=0;i<func->args.size();i++)
      arg_indices.push_back( add_computed_node(func->args[i] ) );

    return add_computed_node(*(func->op), arg_indices);
  }

  std::abort();
}

template <typename T>
typed_expression_ref<T> operator*(typed_expression_ref<T> arg1, typed_expression_ref<T> arg2)
{
  expression_ref times = Multiply<T>();
  return times(arg1,arg2);
}

template <typename T>
typed_expression_ref<T> operator+(typed_expression_ref<T> arg1, typed_expression_ref<T> arg2)
{
  expression_ref plus = Add<T>();
  return plus(arg1,arg2);
}

template <typename T>
typed_expression_ref<T> operator>(typed_expression_ref<T> arg1, typed_expression_ref<T> arg2)
{
  expression_ref gt = GreaterThan<T>();
  return gt(arg1,arg2);
}

int main()
{
  Formula f;
  polymorphic_cow_ptr<Formula> F(f);
  term_ref x = F->add_state_node("X");
  term_ref y = F->add_state_node("Y");
  term_ref z = F->add_state_node("Z");
  term_ref w = F->add_state_node("W");
  term_ref one = F->add_constant_node(Double(1));

  typed_expression_ref<Double> X = x;
  typed_expression_ref<Double> Y = y;
  typed_expression_ref<Int> W = w;
  typed_expression_ref<Double> Z = z;
  typed_expression_ref<Double> One = one;

  F->add_constant_node(Double(1));

  expression_ref mul = Multiply<Double>();
  expression_ref muli = Multiply<Int>();
  expression_ref plus = Add<Double>();
  expression_ref gt = GreaterThan<Double>();
  expression_ref If = IfThenElse();

  std::cout<<"Demonstrate lambda functions\n";
  std::cout<<"mul = "<<mul->print()<<"\n";
  std::cout<<"mul(x) = "<<mul(x)->print()<<"\n";
  std::cout<<"mul(x)(y) = "<<mul(x)(y)->print()<<"\n";
  std::cout<<"mul(x,y) = "<<mul(x,y)->print()<<"\n\n\n";

  term_ref x_times_y_plus_one = F->add_computed_node( plus(mul(x)(y))(one) );

  term_ref z_gt_1 = F->add_computed_node(gt(z)(one));

  term_ref x_plus_y = F->add_computed_node(plus(x)(y));

  term_ref w_2 = F->add_computed_node( muli(w)(w) );

  term_ref cond = F->add_computed_node( If(z_gt_1, x_times_y_plus_one, w_2));

  // this should be a dup and do nothing
  F->add_computed_node( If( gt(z)(one) ) ( plus( mul(x)(y))(one) ) ( muli(w)(w) ) );
  // -- using multiple arguments instead of one at a time.  This works up to 3 arguments
  F->add_computed_node( If( gt(z, one) , plus( mul(x, y), one) , muli(w,w) ) );
  // -- using automatic creation of operators based on typed references
  F->add_computed_node( If( Z > One , X*Y+One , W*W ) );

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
