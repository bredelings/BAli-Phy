#ifndef EXPRESSION_H
#define EXPRESSION_H

#include <boost/shared_ptr.hpp>
#include <vector>
#include <string>
#include "cow-ptr.H"
#include "object.H"
#include "operation.H"

class Formula;

// Eventually, many things should be 'expressions'.  Expressions must be
// generic, and therefore probably must use dynamic typing instead of static
// typing. Expressions can have constants plugged in to dummy variables.  However, C++0x/boost
// functions can also have that.  Furthermore, boost functions should also
// be able to be type safe, if they are constructed using templates. So,
// why don't I just use boost functions?
// - Well, boost functions do only one thing: calculate.
// - Probably one could CONSTRUCT a boost function from a 'expression'.

// Note: I want to be able to construct expressions simply.  For example, I want
// to have an exp(x) expression that does something like exp(Q) = exp_sym(pi(Q),S(Q))

// Note: it would be nice to have expressions be able to return pairs and other objects.
// That would make it much easier for functions to have single return values, since such
// a "single" value could contain multiple parts.

// Note: it is probably necessary for the different parts of a structure to be able to be
// dynamically computed.  A structure is, in some sense, also a function, if structures are
// s-expressions.

// Question: How does this relate to BEAST input files?
// - in BEAST, each object is described using a name, which is basically an ID.
// - expression variables are parameters, and are kept track of using pointers, which
//   I think is problematic.

// Make Operator expression more generic. Just store a pointer to the Operator object.
// This should allow unduplicating some code.

struct term_ref
{
  int index;
  polymorphic_cow_ptr<Formula> F;

  bool is_state() const;
  bool is_constant() const;
  bool is_computed() const;
  std::string print() const;

  operator int() const {return index;}
  term_ref();
  term_ref(int i, boost::shared_ptr<const Formula> f);
  term_ref(int i,const Formula& f);
};

struct expression: virtual public Object
{
  // An expression may or may not have sub-expressions
  std::vector< boost::shared_ptr<const expression> > args;

  // An expression can be
  // - a term reference
  // - a constant (probably one w/o an index so far)
  // - an op taking expressions as arguments.
  // - a dummy var
  // - a lambda-expression binding dummies in its arguments

  expression* clone() const =0;

  virtual int n_args() const {return args.size();}
  
  virtual int highest_unused_dummy() const;

  // return NULL if no change
  virtual boost::shared_ptr<const expression> substitute(int, boost::shared_ptr<const expression>) const;

  std::vector<std::string> print_arg_expressions() const;

  virtual std::string print() const = 0;

  boost::shared_ptr<const expression> apply(boost::shared_ptr<const expression> E) const;

  boost::shared_ptr<const expression> apply(const expression& E) const;

  expression() {}
  expression(int i):args(i) {}
  expression(const std::vector< boost::shared_ptr<const expression> >& A):args(A) { }
  virtual ~expression() {}
};

boost::shared_ptr<const expression> substitute(boost::shared_ptr<const expression> E1, int dummy, boost::shared_ptr<const expression> E2);

// a constant expression
struct constant_expression: public expression
{
  boost::shared_ptr<const Object> value;
  constant_expression* clone() const {return new constant_expression(*this);}

  std::string print() const {return value->print();}

  tribool compare(const Object& o) const;
  
  explicit constant_expression(const Object& o): value(o.clone()) {}
  explicit constant_expression(boost::shared_ptr<const Object> v): value(v) {}
};

struct named_parameter_expression: public expression
{
  std::string parameter_name;
  named_parameter_expression* clone() const {return new named_parameter_expression(*this);}
  std::string print() const {return parameter_name;}

  tribool compare(const Object& o) const;
  
  named_parameter_expression(const std::string& s):parameter_name(s) { }
};

// a dummy variable expression
struct dummy_expression: public expression
{
  int index;

  virtual int highest_unused_dummy() const {return index+1;}

  boost::shared_ptr<const expression> substitute(int dummy, boost::shared_ptr<const expression> E) const;

  dummy_expression* clone() const {return new dummy_expression(*this);}
  std::string print() const;

  tribool compare(const Object& o) const;
  
  dummy_expression(int i):index(i) {}
};

struct match_expression: public expression
{
  int index;

  match_expression* clone() const {return new match_expression(*this);}

  tribool compare(const Object& o) const;
  
  std::string print() const;

  match_expression(int i):index(i) { }
};

struct tuple_expression: public expression
{
  tuple_expression* clone() const {return new tuple_expression(*this);}

  std::string print() const;

  tribool compare(const Object& o) const;
  
  tuple_expression(int i);
  tuple_expression(const std::vector< boost::shared_ptr<const expression> >& A);
};

/// This is an abstract base class for applying an atomic operator to arguments
struct operator_expression: public expression
{
  /// The top-level operation?
  boost::shared_ptr<const Operator> op;

  virtual operator_expression* clone() const {return new operator_expression(*this);};

  // For printing, what is the operator name called?
  virtual const Operator* get_operator() const {return &(*op);}

  virtual std::string print() const;

  tribool compare(const Object& o) const;
  
  operator_expression(const Operator& O, const std::vector< boost::shared_ptr<const expression> >& A);

  operator_expression(const boost::shared_ptr<const Operator>& O, const std::vector< boost::shared_ptr<const expression> >& A);
};

/// An expression with a direct operation
struct operation_expression: public operator_expression
{
  operation_expression* clone() const {return new operation_expression(*this);}

  const Operation* get_operator() const {return dynamic_cast<const Operation*>(&(*op));}

  operation_expression(const Operation& O,const std::vector< boost::shared_ptr<const expression> >& A);

  operation_expression(boost::shared_ptr<const Operation> O,const std::vector< boost::shared_ptr<const expression> >& A);
};

#include "model.H"

struct model_expression: public operator_expression
{
  model_expression* clone() const {return new model_expression(*this);}

  const Model* get_operator() const {return dynamic_cast<const Model*>(&(*op));}

  model_expression(const Model& M);

  model_expression(boost::shared_ptr<const Model> M);
};

enum function_type_t {data_function_f, body_function_f, annotation_f};

struct Function: public Operator
{
  Function* clone() const {return new Function(*this);}

  tribool compare(const Object& o) const;
  
  std::string f_name;
  int n_args;

  boost::shared_ptr<const expression> eval_expression;

  function_type_t what_type;

  std::string name() const {return f_name;}
  bool is_data_constructor() const {return what_type == data_function_f;}
  bool is_function() const {return what_type == body_function_f;}
  bool is_annotation() const {return what_type == annotation_f;}
  Function(const std::string&, int n, function_type_t);
};

/// A data expression is evaluates to itself. (Is this different than a *constructor*)
struct function_expression: public operator_expression
{
  const Function* get_operator() const {return dynamic_cast<const Function*>(&(*op));}

  function_expression* clone() const {return new function_expression(*this);}

  function_expression(const Function& O,const std::vector< boost::shared_ptr<const expression> >& A);

  function_expression(boost::shared_ptr<const Function> O,const std::vector< boost::shared_ptr<const expression> >& A);
};

Function data_function(const std::string&, int);

Function annotation_function(const std::string& , int);

std::vector<std::string> find_named_parameters(boost::shared_ptr<const expression> e);

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

  lambda_expression(const Function& F);
};

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

  expression_ref(const term_ref& t);

  expression_ref(const Operation& o)
    :boost::shared_ptr<const expression>(new lambda_expression(o))
  {}

  expression_ref(const Function& f)
    :boost::shared_ptr<const expression>(new lambda_expression(f))
  {}

  expression_ref(const std::string& s)
    :boost::shared_ptr<const expression>(new named_parameter_expression(s))
  {}

  expression_ref(const char* s)
    :boost::shared_ptr<const expression>(new named_parameter_expression(s))
  {}

  expression_ref(const Model& M)
    :boost::shared_ptr<const expression>(new model_expression(M))
  { }

  explicit expression_ref(const int& i):boost::shared_ptr<const expression>(new constant_expression(Int(i))) {}
  explicit expression_ref(const Int& i):boost::shared_ptr<const expression>(new constant_expression(i)) {}
  explicit expression_ref(const double& d):boost::shared_ptr<const expression>(new constant_expression(Double(d))) {}
  explicit expression_ref(const Double& d):boost::shared_ptr<const expression>(new constant_expression(d)) {}
};

template <typename T>
struct typed_expression_ref: expression_ref
{
public:
  typed_expression_ref(const expression_ref& e):expression_ref(e) {}
  typed_expression_ref(expression* v): expression_ref(v) {}
  typed_expression_ref(const boost::shared_ptr<const expression>& v): expression_ref(v) {}
  typed_expression_ref(const term_ref& t): expression_ref(t) {}

  explicit typed_expression_ref(const int& i):expression_ref(i) {}
  explicit typed_expression_ref(const Int& i):expression_ref(i) {}
  explicit typed_expression_ref(const double& d):expression_ref(d) {}
  explicit typed_expression_ref(const Double& d):expression_ref(d) {}
};

template <typename T>
typed_expression_ref<T> Constant(const T& t)
{
  return typed_expression_ref<T>(new constant_expression(t));
}

template <typename T>
expression_ref E(const T& t) {return expression_ref(t);}

inline expression_ref operator~(const Operation& O) {return E(O);}

#ifdef STDCXXOX
// create an Op automatically from a function... using E(?)
// how does boost::bind work? boost::spirit?
#endif

#endif
