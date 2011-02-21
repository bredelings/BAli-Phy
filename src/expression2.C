#include "expression2.H"

using boost::shared_ptr;
using std::vector;
using std::string;
using std::ostream;

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
