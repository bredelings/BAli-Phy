#ifndef EXPRESSION_H
#define EXPRESSION_H

#include <boost/shared_ptr.hpp>
#include <vector>
#include <string>
#include "cow-ptr.H"
#include "object.H"

class Formula;

struct term_ref
{
  int index;
  polymorphic_cow_ptr<Formula> F;

  bool is_state() const;
  bool is_constant() const;
  bool is_computed() const;
  std::string print() const;

  operator int() const {return index;}
  term_ref():index(-1) {}
  term_ref(int i,polymorphic_cow_ptr<Formula> f):index(i),F(f) {}
  term_ref(int i,const Formula& f);
};

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
  virtual boost::shared_ptr<const expression> substitute(int dummy, boost::shared_ptr<const expression> E) const
  {
    return boost::shared_ptr<const expression>();
  }

  virtual std::string print() const = 0;

  boost::shared_ptr<const expression> apply(boost::shared_ptr<const expression> E) const;

  boost::shared_ptr<const expression> apply(const expression& E) const;

  expression() {}
  virtual ~expression() {}
};

boost::shared_ptr<const expression> substitute(boost::shared_ptr<const expression> E1, int dummy, boost::shared_ptr<const expression> E2);

// a constant expression
struct constant_expression: public expression
{
  boost::shared_ptr<const Object> value;
  constant_expression* clone() const {return new constant_expression(*this);}

  std::string print() const {return value->print();}
  constant_expression(const Object& o): value(o.clone()) {}
  constant_expression(boost::shared_ptr<const Object> v): value(v) {}
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

  boost::shared_ptr<const expression> substitute(int dummy, boost::shared_ptr<const expression> E) const;

  dummy_expression* clone() const {return new dummy_expression(*this);}
  std::string print() const;
  dummy_expression(int i):index(i) {}
};

class Operation;

// a function expression
struct function_expression: public expression
{
  // what is the top-level operation?
  boost::shared_ptr<const Operation> op;

  // what expressions should be plugged in to each slot in the op?
  std::vector< boost::shared_ptr<const expression> > args;

  int highest_unused_dummy() const 
  {
    int highest = 0;
    for(int i=0;i<args.size();i++)
      highest = std::max(highest, args[i]->highest_unused_dummy());
    return highest;
  }

  boost::shared_ptr<const expression> substitute(int dummy, boost::shared_ptr<const expression> E) const;

  function_expression* clone() const {return new function_expression(*this);}

  std::string print() const;

  function_expression(const Operation& O,const std::vector< boost::shared_ptr<const expression> >& A);

  function_expression(boost::shared_ptr<const Operation> O,const std::vector< boost::shared_ptr<const expression> >& A);
};

struct lambda_expression: public expression
{
  dummy_expression dummy_variable;
  
  boost::shared_ptr<const expression> quantified_expression;
  
  int highest_unused_dummy() const 
  {
    return quantified_expression->highest_unused_dummy();
  }
  
  // FIXME - If we're substituting a lambda expression we have to rename its dummys to make sure
  //         there is no overlap.
  boost::shared_ptr<const expression> substitute(int dummy, boost::shared_ptr<const expression> E) const;

  lambda_expression* clone() const {return new lambda_expression(*this);}

  std::string print() const 
  {
    return std::string("(lambda ") + dummy_variable.print() + ")(" + quantified_expression->print() + ")";
  }

  lambda_expression(int dummy, boost::shared_ptr<const expression> E)
    :dummy_variable(dummy),
     quantified_expression(E)
  { }

  lambda_expression(const Operation& O);
};

boost::shared_ptr<const expression> substitute(boost::shared_ptr<const expression> E1, int dummy, boost::shared_ptr<const expression> E2);

boost::shared_ptr<const expression> apply(const expression& E,boost::shared_ptr<const expression> arg);

boost::shared_ptr<const expression> apply(const expression& E,const expression& arg);

boost::shared_ptr<const expression> apply(boost::shared_ptr<const expression> E,
					  boost::shared_ptr<const expression> arg);

boost::shared_ptr<const expression> apply(boost::shared_ptr<const expression> E,
					  const expression& arg);
boost::shared_ptr<const expression> apply(boost::shared_ptr<const expression> E,
					  const std::vector<boost::shared_ptr<const expression> > args,
					  int i);

boost::shared_ptr<const expression> apply(boost::shared_ptr<const expression> E,
					  const std::vector<boost::shared_ptr<const expression> > args);

struct expression_ref: public boost::shared_ptr<const expression>
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
    :boost::shared_ptr<const expression>(v) 
  {}

  expression_ref(const boost::shared_ptr<const expression>& v)
    :boost::shared_ptr<const expression>(v) 
  {}

  expression_ref(const term_ref& t)
    :boost::shared_ptr<const expression>(new term_ref_expression(t)) 
  {}

  expression_ref(const Operation& o)
    :boost::shared_ptr<const expression>(new lambda_expression(o))
  {}
};

template <typename T>
struct typed_expression_ref: expression_ref
{
public:
  typed_expression_ref(expression* v): expression_ref(v) {}
  typed_expression_ref(const boost::shared_ptr<const expression>& v): expression_ref(v) {}
  typed_expression_ref(const term_ref& t): expression_ref(t) {}

  template <typename Y>
  typed_expression_ref(Y y):expression_ref(new constant_expression(POD<Y>(y))) {}
};

template <typename T>
typed_expression_ref<T> Constant(const T& t)
{
  return typed_expression_ref<T>(new constant_expression(t));
}

#endif
