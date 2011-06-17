#ifndef EXPRESSION_H
#define EXPRESSION_H

#include <boost/shared_ptr.hpp>
#include <vector>
#include <string>
#include "cow-ptr.H"
#include "object.H"
#include "operation.H"

class Formula;

// However, C++0x/boost functions can also do binding.  How should expressions relate to them?
// How do you bind a 2nd argument in haskell? (currying?)  Can you use match arguments like _?
// Should I replace the match arguments with a (M x)(f x) operator to specify which dummies are for matching?

// Note: I want to be able to construct expressions simply.  For example, I want
// to have an exp(x) expression that does something like exp(Q) = exp_sym(pi(Q),S(Q))

// Note: Expressions can now return multiple values, in an s-expression... I think?

// Note: it is probably necessary for the different parts of a structure to be able to be
// dynamically computed.  A structure is, in some sense, also a function, if structures are
// s-expressions.

// Question: Do I really want objects to have names in the Formula?

class term_ref;

// Head objects include:
// - parameter(string)
// - dummy(int)
// - match(int)
// - operation
// - function / data
// - tuple should really be a data expression.
// - constant? Can we have (2,3) instead of (constant(2),constant(3))?
// - Model?
//
// - lambda(int)??????????

// TODO: make substitute, apply, and print_arg_expressions external functions, not member functions.

struct expression: virtual public Object
{
  // The operator or object
  // FIXME - if I made everything into an Operator, then (say) a parameter could specify&require only 0 arguments.
  boost::shared_ptr<const Object> head;

  // An expression may or may not have sub-expressions
  std::vector< boost::shared_ptr<const expression> > args;

  expression* clone() const {return new expression(*this);}
  tribool compare(const Object& o) const;
  std::string print() const;
  
  /// The number of terms after the head
  int n_args() const {return args.size();}
  
  std::vector<std::string> print_arg_expressions() const;

  boost::shared_ptr<const expression> apply(boost::shared_ptr<const expression> E) const;

  boost::shared_ptr<const expression> apply(const expression& E) const;

  expression() {}
  explicit expression(const Object& O):head(O.clone()) {}
  explicit expression(const boost::shared_ptr<const Object>& O):head(O) {}
  expression(const std::vector< boost::shared_ptr<const expression> >& A):args(A) { }
  expression(const Object& O, 
	     const std::vector< boost::shared_ptr<const expression> >& A):head(O.clone()), args(A) { }
  expression(const boost::shared_ptr<const Object>& O, 
	     const std::vector< boost::shared_ptr<const expression> >& A):head(O), args(A) { }
  virtual ~expression() {}
};

// a constant expression
struct constant: public Object
{
  boost::shared_ptr<const Object> value;
  constant* clone() const {return new constant(*this);}

  std::string print() const {return value->print();}

  tribool compare(const Object& o) const;
  
  explicit constant(const Object& o): value(o.clone()) {}
  explicit constant(boost::shared_ptr<const Object> v): value(v) {}
};

struct parameter: public Object
{
  std::string parameter_name;

  parameter* clone() const {return new parameter(*this);}
  std::string print() const {return parameter_name;}
  tribool compare(const Object& o) const;
  
  parameter(const std::string& s):parameter_name(s) { }
};

// a dummy variable expression
struct dummy_expression: public expression
{
  int index;

  dummy_expression* clone() const {return new dummy_expression(*this);}
  std::string print() const;

  tribool compare(const Object& o) const;
  
  dummy_expression(int i):index(i) {}
};

struct match: public Object
{
  int index;

  match* clone() const {return new match(*this);}

  tribool compare(const Object& o) const;
  
  std::string print() const;

  match(int i):index(i) { }
};

class expression_ref;

// This no longer needs to be a virtual member function, because we can now walk the expression from outside.
int n_match_results(const expression_ref&);

bool find_match(const expression_ref& pattern,const expression_ref& E, std::vector<boost::shared_ptr<const expression> >& results);

#include "model.H"

std::vector< boost::shared_ptr< const expression > > model_args(const Model& M);

enum function_type_t {data_function_f, body_function_f, annotation_f};

struct Function: public Operator
{
  Function* clone() const {return new Function(*this);}

  tribool compare(const Object& o) const;
  
  std::string f_name;
  int n_args_;

  int n_args() const {return n_args_;}

  boost::shared_ptr<const expression> eval_expression;

  function_type_t what_type;

  std::string name() const {return f_name;}
  bool is_data_constructor() const {return what_type == data_function_f;}
  bool is_function() const {return what_type == body_function_f;}
  bool is_annotation() const {return what_type == annotation_f;}
  Function(const std::string&, int n, function_type_t);
};

Function data_function(const std::string&, int);

Function annotation_function(const std::string& , int);

std::vector<std::string> find_named_parameters(boost::shared_ptr<const expression> e);

boost::shared_ptr<const expression> substitute(boost::shared_ptr<const expression> E1, int dummy, boost::shared_ptr<const expression> E2);

// Single-arg version
boost::shared_ptr<const expression> apply(const expression_ref& E,const expression_ref& arg);

// Multiple-arg version
boost::shared_ptr<const expression> apply(const expression_ref& E, const std::vector<boost::shared_ptr<const expression> > args);

expression_ref lambda_expression(const Operator& O);

// FIXME - If we're substituting a lambda expression we have to rename its dummys to make sure
//         there is no overlap.
struct lambda: public Object
{
  int dummy;
  
  lambda* clone() const {return new lambda(*this);}

  lambda(int d);
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
    :boost::shared_ptr<const expression>(lambda_expression(o))
  {}

  expression_ref(const Function& f)
    :boost::shared_ptr<const expression>(lambda_expression(f))
  {}

  expression_ref(const std::string& s)
    :boost::shared_ptr<const expression>(new expression(parameter(s)))
  {}

  expression_ref(const char* s)
    :boost::shared_ptr<const expression>(new expression(parameter(s)))
  {}

  expression_ref(const match& m)
    :boost::shared_ptr<const expression>(new expression(m))
  {}

  expression_ref(const Model& M)
    :boost::shared_ptr<const expression>(new expression(M,model_args(M)))
  { }

  expression_ref(const expression& e)
    :boost::shared_ptr<const expression>(e.clone())
  { }

  explicit expression_ref(const int& i):boost::shared_ptr<const expression>(new expression(constant(Int(i)))) {}
  explicit expression_ref(const Int& i):boost::shared_ptr<const expression>(new expression(constant(i))) {}
  explicit expression_ref(const double& d):boost::shared_ptr<const expression>(new expression(constant(Double(d)))) {}
  explicit expression_ref(const Double& d):boost::shared_ptr<const expression>(new expression(constant(d))) {}
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
  return typed_expression_ref<T>(new expression(constant(t)));
}

template <typename T>
expression_ref E(const T& t) {return expression_ref(t);}

inline expression_ref operator~(const Operation& O) {return E(O);}

#ifdef STDCXXOX
// create an Op automatically from a function... using E(?)
// how does boost::bind work? boost::spirit?
#endif

#endif
