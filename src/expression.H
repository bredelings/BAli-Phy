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

// A quote operator would affect how arguments were evaluated.
// - Do I want the question of whether the argument is an expression to affect its evaluation?
// - Perhaps I could evaluate all expression-type arguments.
// - Quoting would stop prevent this, and would take the place of constant(expression(...)).

// Question: Do I really want objects to have names in the Formula?

class expression_ref;

// Head objects include:
// - parameter(string)
// - dummy(int)
// - match(int)
// - lambda(int)
// - operation
// - function / data
//   + FIXME: should we evaluate these?

struct expression: virtual public Object
{
  // FIXME - replace args with sub, and make sub[0] = head, sub[i] = args[i-1]

  // The operator or object
  boost::shared_ptr<const Object> head;

  // An expression may or may not have sub-expressions
  std::vector< expression_ref > args;

  /// The number of terms after the head
  int n_args() const {return args.size();}
  
  // Methods from Object
  expression* clone() const {return new expression(*this);}
  tribool compare(const Object& o) const;
  std::string print() const;
  
  expression() {}
  explicit expression(const object_ref& O);
  expression(const object_ref& O, const expression_ref& args);
  expression(const object_ref& O, const std::vector< expression_ref >& A);

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
struct dummy: public Object
{
  int index;

  dummy* clone() const {return new dummy(*this);}
  std::string print() const;
  tribool compare(const Object& o) const;
  
  dummy(int i):index(i) {}
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

bool find_match(const expression_ref& pattern,const expression_ref& E, std::vector< expression_ref >& results);

enum function_type_t {data_function_f, body_function_f};

struct Function: public Operator
{
  Function* clone() const {return new Function(*this);}

  tribool compare(const Object& o) const;
  
  std::string f_name;
  int n_args_;

  int n_args() const {return n_args_;}

  function_type_t what_type;

  std::string name() const {return f_name;}
  bool is_data_constructor() const {return what_type == data_function_f;}
  bool is_function() const {return what_type == body_function_f;}
  Function(const std::string&, int n, function_type_t);
};

expression_ref Tuple(int);

extern expression_ref Cons;

extern expression_ref ListEnd;

Function data_function(const std::string&, int);

std::vector<std::string> find_named_parameters(const expression_ref&);

expression_ref substitute(const expression_ref& E1, int dummy, const expression_ref& E2);

// Single-arg version  (Is this a function at all?)";
expression_ref apply(const expression_ref& E,const expression_ref& arg);

// Multiple-arg version
expression_ref apply(const expression_ref& E, const std::vector< expression_ref > args);

expression_ref lambda_expression(const Operator& O);

// FIXME - If we're substituting a lambda expression we have to rename its dummys to make sure
//         there is no overlap.
struct lambda: public Object
{
  int dummy_index;
  
  lambda* clone() const {return new lambda(*this);}

  lambda(int d);
};

class term_ref;

struct expression_ref: public boost::shared_ptr<const Object>
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

  expression_ref operator()(const expression_ref& arg1, 
			    const expression_ref& arg2,
			    const expression_ref& arg3,
			    const expression_ref& arg4) const
  {
    return apply(apply(apply(apply(*this,arg1),arg2),arg3),arg4);
  }

  expression_ref() {}

  expression_ref(Object* v)
    :boost::shared_ptr<const Object>(v) 
  {}

  expression_ref(const boost::shared_ptr<const Object>& v)
    :boost::shared_ptr<const Object>(v) 
  {}

  template <typename T>
  expression_ref(const boost::shared_ptr<const T>& v)
    :boost::shared_ptr<const Object>(boost::static_pointer_cast<const Object>(v)) 
  {}

  expression_ref(const term_ref& t);

  expression_ref(const std::string& s)
    :boost::shared_ptr<const Object>(new String(s))
  {}

  expression_ref(const char* s)
    :boost::shared_ptr<const Object>(new String(s))
  {}

  expression_ref(const Object& o)
    :boost::shared_ptr<const Object>(o.clone())
  {}

  expression_ref(const int& i):boost::shared_ptr<const Object>(new Int(i)) {}
  expression_ref(const double& d):boost::shared_ptr<const Object>(new Double(d)) {}
};

template <typename T>
struct typed_expression_ref: expression_ref
{
public:
  typed_expression_ref() {}
  typed_expression_ref(const expression_ref& e):expression_ref(e) {}
  typed_expression_ref(expression* v): expression_ref(v) {}
  typed_expression_ref(const boost::shared_ptr<const Object>& v): expression_ref(v) {}
  typed_expression_ref(const term_ref& t): expression_ref(t) {}

  typed_expression_ref(const Object& o):expression_ref(o) {}
  typed_expression_ref(const int& i):expression_ref(i) {}
  typed_expression_ref(const double& d):expression_ref(d) {}
};

template <typename T>
typed_expression_ref<T> Constant(const T& t)
{
  return typed_expression_ref<T>(t.clone());
}

template <typename T>
expression_ref E(const T& t) {return expression_ref(t);}

inline expression_ref operator~(const Operation& O) {return E(O);}

/*
std::ostream& operator<<(std::ostream& o,const expression_ref& R)
{
  return o<<R->print();
}
*/
#ifdef STDCXXOX
// create an Op automatically from a function... using E(?)
// how does boost::bind work? boost::spirit?
#endif

#endif
