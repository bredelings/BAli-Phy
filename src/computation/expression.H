#ifndef EXPRESSION_H
#define EXPRESSION_H

#include <vector>
#include <valarray>
#include <set>
#include <string>
#include "object.H"
#include "computation/operator.H"

/// An S-expression: An Object, and possibly an array of Objects.
struct expression_ref;

/// This represents function application: a,b,c = a(b)(c) and not a(b(c)). The precedence is 18 (low).
expression_ref operator,(const expression_ref&, const expression_ref&);
/// This represents the Cons constructor: a&b = Cons a b = a:b.  The precedence is 3 (high).
expression_ref operator&(const expression_ref&, const expression_ref&);

/*
 *  By separating out the head from the children, each expression now represents
 *  a node in a directed acyclic graph, where each node has an Object on in.
 */

/// An array of Objects.
struct expression: public Object
{
  // The head of the expression.
  object_ptr<const Object> head;

  // An expression may or may not have sub-expressions
  std::vector< expression_ref > sub;

  /// The number of terms after the head
  int size() const {return sub.size();}
  
  // Methods from Object
  expression* clone() const {return new expression(*this);}
  tribool compare(const Object& o) const;
  std::string print() const;
  
  tribool operator==(const expression& E) const;

  template <typename T>
  object_ptr<const T> is_a() const
  {
    return boost::dynamic_pointer_cast<const T>(head);
  }

  bool is_exactly(const Object& O) const;

  expression() {}
  expression(const object_ref&);

  expression(const object_ref&, const std::vector< expression_ref >& S);

  virtual ~expression() {}
};

struct parameter: public Object
{
  std::string parameter_name;

  parameter* clone() const {return new parameter(*this);}
  std::string print() const {return parameter_name;}
  tribool compare(const Object& o) const;
  
  parameter(const std::string& s):parameter_name(s) { }
};

/// a de Bruijn indexed variable 
struct index_var: public Object
{
  int index;

  index_var* clone() const {return new index_var(*this);}
  std::string print() const;
  tribool compare(const Object& o) const;
  
  index_var(int i):index(i) {}
};

// a dummy variable expression
struct dummy: public Object
{
  int index;

  std::string name;

  bool operator==(const dummy&) const;
  dummy* clone() const {return new dummy(*this);}
  std::string print() const;
  tribool compare(const Object& o) const;
  
  bool operator<(const dummy&) const;

  dummy(int i):index(i) {}
  dummy(const std::string& s):index(-1),name(s) {}
};

struct Trim: public Object
{
  Trim* clone() const {return new Trim(*this);}
  tribool compare(const Object& o) const;

  std::string print() const;
};

bool is_index_var(const expression_ref& E);

bool is_dummy(const expression_ref&);

bool is_parameter(const expression_ref&);

bool is_wildcard(const expression_ref&);

bool is_irrefutable_pattern(const expression_ref& E);

struct match: public Object
{
  int index;

  match* clone() const {return new match(*this);}

  tribool compare(const Object& o) const;
  
  std::string print() const;

  match(int i):index(i) { }
};

struct let_obj: public Object
{
  let_obj* clone() const {return new let_obj(*this);}

  tribool compare(const Object& o) const;

  std::string print() const;
};

struct let2_obj: public Object
{
  let2_obj* clone() const {return new let2_obj(*this);}

  tribool compare(const Object& o) const;

  std::string print() const;
};

// This no longer needs to be a virtual member function, because we can now walk the expression from outside.
int n_match_results(const expression_ref&);

bool find_match(const expression_ref& pattern,const expression_ref& E, std::vector< expression_ref >& results);

struct constructor: public Operator
{
  constructor* clone() const {return new constructor(*this);}

  tribool compare(const Object& o) const;
  
  std::string f_name;
  int n_args_;

  int n_args() const {return n_args_;}

  assoc_type assoc;

  int prec;

  assoc_type associativity() const {return assoc;}
  virtual int precedence() const {return prec;}

  std::string name() const {return f_name;}
  constructor(const std::string&, int n);
};

constructor left_assoc_constructor(const std::string& s,int prec=0);

constructor right_assoc_constructor(const std::string& s,int prec=0);

std::set<std::string> find_named_parameters(const expression_ref&);

std::set<std::string> find_named_parameters(const std::vector<expression_ref>&);

expression_ref substitute(const expression_ref& E1, int dummy, const expression_ref& E2);

expression_ref substitute(const expression_ref& E1, const expression_ref& D, const expression_ref& E2);

/// Substitute for all matches found
expression_ref substitute(const expression_ref& E, const std::vector<expression_ref>& replace);

// Single-arg version  (Is this a function at all?)";
expression_ref apply(const expression_ref& E,const expression_ref& arg);

// Multiple-arg version
expression_ref apply(const expression_ref& E, const std::vector< expression_ref > args);

// Single-arg version  (Is this a function at all?)";
expression_ref apply_expression(const expression_ref& E,const expression_ref& arg);

// Multiple-arg version
expression_ref apply_expression(const expression_ref& E, const std::vector< expression_ref > args);

expression_ref lambda_expression(const Operator& O);
expression_ref lambda_quantify(int dummy_index, const expression_ref& E);
expression_ref lambda_quantify(const expression_ref& dummy, const expression_ref& E);

expression_ref indexify(const expression_ref&);

expression_ref deindexify(const expression_ref&);

expression_ref trim_normalize(const expression_ref&);

expression_ref trim(const expression_ref&);

expression_ref trim_unnormalize(const expression_ref&);

expression_ref untrim(const expression_ref&);

std::vector<int> get_free_index_vars(const expression_ref&);

struct lambda: public Object
{
  lambda* clone() const {return new lambda(*this);}
  tribool compare(const Object& o) const;
  std::string print() const;
};

struct lambda2: public Object
{
  lambda2* clone() const {return new lambda2(*this);}
  tribool compare(const Object& o) const;
  std::string print() const;
};

class formula_expression_ref;

struct expression_ref: public object_ptr<const expression>
{
  // FIXME - eliminate operator() in favor of operator,()
  expression_ref operator()(const expression_ref& arg) const
  {
    return apply(*this,arg);
  }

  formula_expression_ref operator()(const formula_expression_ref& arg1) const;

  formula_expression_ref operator()(const formula_expression_ref& arg1,
				    const formula_expression_ref& arg2) const;

  formula_expression_ref operator()(const formula_expression_ref& arg1,
				    const formula_expression_ref& arg2,
				    const formula_expression_ref& arg3) const;

  formula_expression_ref operator()(const formula_expression_ref& arg1,
				    const formula_expression_ref& arg2,
				    const formula_expression_ref& arg3,
				    const formula_expression_ref& arg4) const;

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

  expression_ref operator()(const std::vector<expression_ref>& args) const
  {
    return apply(*this,args);
  }

  template <typename T>
  object_ptr<const T> is_a() const
  {
    return boost::dynamic_pointer_cast<const T>((*this)->head);
  }

  bool is_exactly(const Object& O) const
  {
    return (*this)->is_exactly(O);
  }

  // Default
  expression_ref() {}

  // Expression arguments
  expression_ref(expression* e):object_ptr<const expression>(e) { }

  expression_ref(const expression* e):object_ptr<const expression>(e) { }

  expression_ref(const object_ptr<const expression>& e):object_ptr<const expression>(e) { }

  expression_ref(const object_ptr<expression>& e):object_ptr<const expression>(e) { }

  // Object arguments
  expression_ref(Object* v):expression_ref(new expression(v)) { }

  expression_ref(const object_ptr<const Object>& v):expression_ref(new expression(v)) { }

  expression_ref(const object_ptr<Object>& v)
    :expression_ref(new expression(object_ref(v)))
  {}

  template <typename T>
  expression_ref(const object_ptr<T>& v)
    :expression_ref(new expression(v))
  {}

  expression_ref(const Object& o)
  :expression_ref(new expression(o))
  {}

  expression_ref(const object_ref& o)
  :expression_ref(new expression(o))
  {}

  // Builin-type arguments
  expression_ref(const int& i):expression_ref(new Int(i)) {}
  expression_ref(const double& d):expression_ref(new Double(d)) {}
  expression_ref(const bool& b):expression_ref(new Bool(b)) {}
  expression_ref(const std::string& s):expression_ref(new String(s)) {}
  expression_ref(const char* s):expression_ref(new String(s)) {}
};

template <typename T>
object_ptr<const T> is_a(const expression& E)
{ 
  return E.is_a<T>();
}

template <typename T>
object_ptr<const T> is_a(const expression_ref& E)
{ 
  return E.is_a<T>();
}

bool is_exactly(const expression_ref&, const std::string&);

bool is_exactly(const expression_ref&, const Object& O);

bool same_head(const expression_ref&, const expression_ref&);

template <typename T>
struct typed_expression_ref: public expression_ref
{
public:
  explicit typed_expression_ref() {}
  explicit typed_expression_ref(const expression_ref& e):expression_ref(e) {}
  explicit typed_expression_ref(expression* v): expression_ref(v) {}
  explicit typed_expression_ref(const object_ref& v): expression_ref(v) {}
  explicit typed_expression_ref(const object_ptr<const Object>& v): expression_ref(v) {}
  explicit typed_expression_ref(const Object& o):expression_ref(o) {}
};

template <typename T>
struct typed_expression_ref<POD<T> >: public expression_ref
{
public:
  explicit typed_expression_ref() {}
  explicit typed_expression_ref(const expression_ref& e):expression_ref(e) {}
  explicit typed_expression_ref(expression* v): expression_ref(v) {}
  explicit typed_expression_ref(const object_ref& v): expression_ref(v) {}
  explicit typed_expression_ref(const object_ptr<const Object>& v): expression_ref(v) {}
  explicit typed_expression_ref(const Object& o):expression_ref(o) {}
  typed_expression_ref(const T& t):expression_ref(POD<T>(t)) {}
};

template <typename T>
struct typed_expression_ref<POD_math<T> >: public expression_ref
{
public:
  explicit typed_expression_ref() {}
  explicit typed_expression_ref(const expression_ref& e):expression_ref(e) {}
  explicit typed_expression_ref(expression* v): expression_ref(v) {}
  explicit typed_expression_ref(const object_ref& v): expression_ref(v) {}
  explicit typed_expression_ref(const object_ptr<const Object>& v): expression_ref(v) {}
  explicit typed_expression_ref(const Object& o):expression_ref(o) {}
  typed_expression_ref(const T& t):expression_ref(POD_math<T>(t)) {}
};

inline typed_expression_ref<Double> Constant(const double& d)
{
  return typed_expression_ref<Double>(Double(d));
}

inline typed_expression_ref<Int> Constant(const int& i)
{
  return typed_expression_ref<Int>(Int(i));
}

inline typed_expression_ref<Bool> Constant(const bool& b)
{
  return typed_expression_ref<Bool>(Bool(b));
}

extern expression_ref _;
extern expression_ref _1;
extern expression_ref _2;
extern expression_ref _3;
extern expression_ref _4;

std::string tuple_name(int n);
bool is_tuple_name(const std::string&);
constructor tuple_head(int n);

// Create a constructor for n-tuples.
expression_ref Tuple(int n);

template <typename First, typename Second, typename Third, typename... Rest>
auto apply(const First& first, const Second& second, const Third& third, const Rest&... rest) -> decltype(  apply(apply(first,second), third, rest...) )
{
  return apply(apply(first,second), third, rest...);
}

template <typename First, typename Second, typename... Rest>
auto Tuple(const First& first, const Second& second, const Rest&... rest) -> decltype( apply(Tuple(2+sizeof...(rest)) ,first,second,rest...) )
{
  return apply(Tuple(2+sizeof...(rest)) ,first,second,rest...);
}

// 2 Fields, a value, and a Cons or a ListEnd
extern expression_ref Cons;

// 0 fields
extern expression_ref ListEnd;

// 2 Fields: parameter, value
extern expression_ref default_value;

// 2 Fields: parameter, Bounds<double>
extern expression_ref var_bounds;

// 2 Fields: (random vars) (prob_density (parameter expressions))
extern expression_ref distributed;

// 1 Field: a value
extern expression_ref sys_print;

// 2 Fields: string1 string2
extern expression_ref concat;

// 1 Fields: the density expression
extern expression_ref prob;

// 3 Fields: definition, guard, body
extern expression_ref defun;

template <typename T,typename U>
std::valarray<T> get_varray(const expression_ref& E)
{
  if (not E)
    return std::valarray<T>();

  if (not E->size())
  {
    std::valarray<T> v2(1);
    v2[0] = *convert<const U>(E->head);
    return v2;
  }

  std::valarray<T> v2(E->size());
  for(int i=0;i<v2.size();i++)
    v2[i] = *convert<const U>(E->sub[i]->head);
  return v2;
}

template <typename T,typename U>
std::vector<T> get_vector(const expression_ref& E) 
{
  if (not E)
    return std::vector<T>();

  if (not E->size())
  {
    std::vector<T> v2(1);
    v2[0] = *convert<const U>(E->head);
    return v2;
  }
    
  std::vector<T> v2(E->size());
  for(int i=0;i<v2.size();i++)
    v2[i] = *convert<const U>(E->sub[i]->head);
  return v2;
}

std::vector<expression_ref> get_ref_vector_from_list(const expression_ref& E);

std::vector<expression_ref> get_ref_vector_from_tuple(const expression_ref& E);

template <typename T,typename U>
std::vector<T> get_vector_from_list(const expression_ref& E)
{
  std::vector<expression_ref> v1 = get_ref_vector_from_list(E);
  std::vector<T> v2;
  for(int i=0;i<v1.size();i++)
    v2.push_back(*convert<const U>(v1[i]->head));
  return v2;
}

template <typename T>
expression_ref get_tuple(const std::vector<T>& v)
{
  std::vector<expression_ref> v2;
  for(int i=0;i<v.size();i++)
    v2.push_back(v[i]);

  return get_tuple(v2);
}

template<> expression_ref get_tuple<>(const std::vector<expression_ref>& v);

template <typename T>
expression_ref get_tuple(const std::valarray<T>& v)
{
  std::vector<expression_ref> v2;
  for(int i=0;i<v.size();i++)
    v2.push_back(v[i]);

  return get_tuple(v2);
}

expression_ref get_list(const std::vector<expression_ref>& v);

expression_ref let_expression(const std::vector<expression_ref>& vars, const std::vector<expression_ref>& bodies, const expression_ref& T);

expression_ref let_expression(const expression_ref& var, const expression_ref& body, const expression_ref& T);

expression_ref case_expression(const expression_ref& T, const expression_ref& pattern, const expression_ref& body, const expression_ref& otherwise = expression_ref());

expression_ref case_expression(const expression_ref& T, const std::vector<expression_ref>& patterns, const std::vector<expression_ref>& bodies);

expression_ref make_case_expression(const expression_ref& T, const std::vector<expression_ref>& patterns, const std::vector<expression_ref>& bodies);

//expression_ref def_function(const expression_ref& pattern, const expression_ref& body);

//expression_ref def_function(const std::vector<expression_ref>& patterns, const std::vector<expression_ref>& bodies);

expression_ref def_function(const std::vector< std::vector<expression_ref> >& patterns, const std::vector<expression_ref>& bodies);

expression_ref launchbury_normalize(const expression_ref& E);
expression_ref launchbury_unnormalize(const expression_ref& E);

expression_ref let_float(const expression_ref&);

expression_ref add_prefix(const std::string&, const expression_ref& E);

bool parse_let_expression(const expression_ref& E, std::vector<expression_ref>& vars, std::vector<expression_ref>& bodies, expression_ref& T);

bool parse_indexed_let_expression(const expression_ref& E, std::vector<expression_ref>& bodies, expression_ref& T);

void parse_alternatives(const expression_ref& E, std::vector<expression_ref>& cases, std::vector<expression_ref>& results);

bool is_WHNF(const expression_ref& E);

int get_safe_binder_index(const expression_ref& E);

extern const expression_ref v0;
extern const expression_ref v1;
extern const expression_ref v2;
extern const expression_ref v3;
extern const expression_ref v4;
extern const expression_ref v5;
extern const expression_ref v6;

expression_ref operator^(const expression_ref& x, const expression_ref& T);
#endif
