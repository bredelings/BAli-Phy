#ifndef EXPRESSION_H
#define EXPRESSION_H

#include <vector>
#include <valarray>
#include <set>
#include <string>
#include <initializer_list>
#include "object.H"
#include "computation/operator.H"
#include "computation/type_constant.H"

/// An S-expression: An Object, and possibly an array of Objects.
struct expression;

class expression_ref
{
  object_ptr<const Object> O;
  union
  {
    char c;
    int i;
    double d;
    log_double_t ld;
  };
  type_constant type_ = null_type;

  bool is_object_type() const {return type_ > char_type;}

public:

  tribool operator==(const expression_ref& E2) const
  {
    if (type_ != E2.type())
      return false;
    
    switch(type_)
    {
    case int_type:
      return (i == E2.as_int());
      break;
    case double_type:
      return (d == E2.as_double());
      break;
    case log_double_type:
      return (ld == E2.as_log_double());
      break;
    case char_type:
      return (c == E2.as_char());
      break;
    default:
      return ptr()->compare(*E2.ptr());
    }
  }

  tribool operator==(const Object& O) const
  {
    if (not is_object_type())
      return false;
    else
      return ptr()->compare(O);
  }

  void clear() {O.reset();type_=null_type;}

  bool is_expression() const;

  bool is_atomic() const
  {
    return not is_expression();
  }

  type_constant type() const
  {
    return type_;
  }
  
  const expression& as_expression() const
  {
    assert(not is_atomic());
    return *convert<expression>(O.get());
  }

  inline const expression_ref& head() const;

  inline const std::vector<expression_ref>& sub() const;

  inline int size() const 
  {
    if (is_atomic())
      return 0;
    else
      return sub().size();
  }

  expression_ref operator()(const std::vector<expression_ref>& args) const;

  template <typename T>
  bool is_a() const
  {
    return (bool)boost::dynamic_pointer_cast<const T>(head().ptr());
  }

  template <typename T>
  object_ptr<const T> assert_is_a() const
  {
    return convert<const T>(head().ptr());
  }

  template <typename T>
  const T& as_() const
  {
    return *convert<const T>(head().ptr());
  }

  bool is_int() const
  {
    return type_ == int_type;
  }

  int as_int() const
  {
    return i;
  }

  bool is_double() const
  {
    return type_ == double_type;
  }

  double as_double() const
  {
    return d;
  }

  bool is_char() const
  {
    return type_ == char_type;
  }

  char as_char() const
  {
    return c;
  }

  bool is_log_double() const
  {
    return type_ == log_double_type;
  }

  log_double_t as_log_double() const
  {
    return ld;
  }
  
  inline const object_ptr<const Object>& ptr() const {return O;}

  inline explicit operator bool() const
  {
    return (type_ != null_type);
  }

  std::string print() const
  {
    switch(type_)
    {
    case null_type:
      return "[NULL]";
    case int_type:
      return convertToString(i);
      break;
    case double_type:
      return convertToString(d);
      break;
    case log_double_type:
      return convertToString(ld);
      break;
    case char_type:
      return std::string("'")+c+"'";
      break;
    default:
      return O->print();
    }
  }

  // Default
  expression_ref() {}

  expression_ref(expression* e);

  expression_ref(const expression* e);

  expression_ref(const object_ptr<const expression>& e):expression_ref(e.get()) { }

  expression_ref(const object_ptr<expression>& e):expression_ref(e.get()) { }

  expression_ref(const expression_ref& e, const std::initializer_list<expression_ref> v);

  expression_ref(const expression_ref& e, const std::vector<expression_ref>& v);

  // Object arguments
  expression_ref(Object* v):O(v),type_(v->type()) { }

  expression_ref(const Object* v):O(v),type_(v->type()) { }

  expression_ref(const object_ptr<Object>& v):expression_ref(v.get()) { }

  template <typename T>
  expression_ref(const object_ptr<T>& v):expression_ref(v.get()) { }

  expression_ref(const Object& o):expression_ref(o.clone()) { }

  // Builtin-type arguments
  expression_ref(const int& i2):i(i2),type_(int_type) {}
  expression_ref(const double& d2):d(d2),type_(double_type) {}
  expression_ref(const log_double_t& ld2):ld(ld2),type_(log_double_type) {}
  expression_ref(const bool& b);
  expression_ref(const std::string& s);
  expression_ref(const char* s);
  expression_ref(const char& c2):c(c2),type_(char_type) {}
  expression_ref(const String& s):expression_ref(s.clone()) { }
};


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
  expression_ref head;

  // An expression may or may not have sub-expressions
  std::vector< expression_ref > sub;

  /// The number of terms after the head
  int size() const {return sub.size();}
  
  // Methods from Object
  expression* clone() const {return new expression(*this);}
  tribool compare(const Object& o) const;
  std::string print() const;
  
  tribool operator==(const expression& E) const;

  type_constant type() const {return expression_type;}

  expression() {}

  expression(const expression_ref&);

  expression(const expression_ref&, const std::initializer_list< expression_ref > S);

  expression(const expression_ref&, const std::vector< expression_ref >& S);

  virtual ~expression() {}
};

inline const expression_ref& expression_ref::head() const 
{
  if (is_expression())
    return as_expression().head;
  else
    return *this;
}

inline bool expression_ref::is_expression() const
{
#ifndef NDEBUG
  bool e = dynamic_cast<const expression*>(O.get());
  assert(e == (type_ == expression_type));
#endif
  return (type_ == expression_type);
}

inline const std::vector<expression_ref>& expression_ref::sub() const {return as_expression().sub;}
//        object_ptr<const Object>& sub()       {return E->sub;}

inline expression_ref::expression_ref(expression* e):O(e),type_(expression_type) { }

inline expression_ref::expression_ref(const expression* e):O(e),type_(expression_type) { }

inline expression_ref::expression_ref(const expression_ref& e, const std::initializer_list<expression_ref> v):expression_ref(new expression(e,v)) { }

inline expression_ref::expression_ref(const expression_ref& e, const std::vector<expression_ref>& v):expression_ref(new expression(e,v)) { }

struct parameter: public Object
{
  std::string parameter_name;

  parameter* clone() const {return new parameter(*this);}
  std::string print() const {return parameter_name;}
  tribool compare(const Object& o) const;
  
  type_constant type() const {return parameter_type;}

  parameter(const std::string& s);
};

/// a de Bruijn indexed variable 
struct index_var: public Object
{
  int index;

  index_var* clone() const {return new index_var(*this);}
  std::string print() const;
  tribool compare(const Object& o) const;
  
  type_constant type() const {return index_var_type;}

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

  type_constant type() const {return dummy_type;}

  dummy(int i):index(i) {}
  dummy(const std::string& s):index(-1),name(s) { assert(name != "_");}
};

struct reg_var: public Object
{
  int target;

  reg_var* clone() const {return new reg_var(*this);}

  std::string print() const 
  {
    return "<" + convertToString(target) + ">";
  }

  tribool compare(const Object& o) const
  {
    const reg_var* E = dynamic_cast<const reg_var*>(&o);
    if (not E) 
      return false;

    return target == E->target;
  }

  reg_var():
    target(-1)
  { }

  reg_var(int t):
    target(t)
  { }
};

struct identifier: public Object
{
  std::string name;

  identifier* clone() const {return new identifier(*this);}

  std::string print() const
  {
    return name;
  }

  tribool compare(const Object& o) const
  {
    const identifier* V = dynamic_cast<const identifier*>(&o);
    if (not V)
      return false;

    return name == V->name;
  }

  identifier(const std::string& s);
};

struct Trim: public Object
{
  Trim* clone() const {return new Trim(*this);}
  tribool compare(const Object& o) const;

  type_constant type() const {return trim_type;}

  std::string print() const;
};

bool is_index_var(const expression_ref& E);

bool is_dummy(const expression_ref&);

bool is_parameter(const expression_ref&);

bool is_modifiable(const expression_ref&);

bool is_identifier(const expression_ref& R);

bool is_reglike(const expression_ref& R);

bool is_wildcard(const dummy& d);

bool is_wildcard(const expression_ref&);

bool is_irrefutable_pattern(const expression_ref& E);

struct let_obj: public Object
{
  let_obj* clone() const {return new let_obj(*this);}

  tribool compare(const Object& o) const;

  type_constant type() const {return let_type;}

  std::string print() const;
};

struct AST_node: public Object
{
  AST_node* clone() const {return new AST_node(*this);}
  std::string type;
  std::string value;
  tribool compare(const Object& o) const;

  std::string print() const;

  AST_node(const std::string&);
  AST_node(const std::string& t, const std::string& v);
};

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

  type_constant type() const {return constructor_type;}

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

inline expression_ref expression_ref::operator()(const std::vector<expression_ref>& args) const
{
  return apply(*this,args);
}

expression_ref lambda_expression(const Operator& O);
expression_ref lambda_quantify(int dummy_index, const expression_ref& E);
expression_ref lambda_quantify(const expression_ref& dummy, const expression_ref& E);

expression_ref indexify(const expression_ref&);

expression_ref deindexify(const expression_ref&, const std::vector<expression_ref>&);
expression_ref deindexify(const expression_ref&);

expression_ref trim_normalize(const expression_ref&);

expression_ref trim(const expression_ref&);

expression_ref trim_unnormalize(const expression_ref&);

expression_ref untrim(const expression_ref&);

void alpha_rename(object_ptr<expression>& E, const expression_ref& x, const expression_ref& y);

int max_index(const std::set<dummy>& s);

std::set<dummy> get_free_indices(const expression_ref& E);

std::vector<int> get_free_index_vars(const expression_ref&);

struct lambda: public Object
{
  lambda* clone() const {return new lambda(*this);}
  tribool compare(const Object& o) const;
  type_constant type() const {return lambda_type;}
  std::string print() const;
};

struct lambda2: public Object
{
  lambda2* clone() const {return new lambda2(*this);}
  tribool compare(const Object& o) const;

  type_constant type() const {return lambda2_type;}
  std::string print() const;
};

inline std::ostream& operator<<(std::ostream& o,const expression_ref& E)
{
  if (E)
    return o<<E.print();
  else
    return o<<"[NULL]";
}


bool has_constructor(const expression_ref&, const std::string&);

std::string tuple_name(int n);
bool is_tuple_name(const std::string&);
constructor tuple_head(int n);

// Create a constructor for n-tuples.
expression_ref Tuple(int n);

template <typename First, typename Second>
auto apply_subst(const First& first, const Second& second) -> decltype (first * second)
{
  return first * second;
}

template <typename First, typename Second, typename Third, typename... Rest>
auto apply_subst(const First& first, const Second& second, const Third& third, const Rest&... rest) -> decltype(  apply_subst(first*second, third, rest...) )
{
  return apply_subst(first*second, third, rest...);
}

template <typename First, typename Second, typename... Rest>
auto Tuple(const First& first, const Second& second, const Rest&... rest) -> decltype( apply_subst(Tuple(2+sizeof...(rest)) ,first,second,rest...) )
{
  return apply_subst(Tuple(2+sizeof...(rest)) ,first,second,rest...);
}

template <typename First>
First let(const First& first)
{
  return first;
}

template <typename First, typename Second, typename... Rest>
auto let(const First& first, const Second& second, const Rest&... rest) -> decltype( let_expression(first,second,let(rest...)) )
{
  return let_expression(first,second,let(rest...));
}

// 2 Fields, a value, and a Cons or a ListEnd
extern expression_ref Cons;

// 0 fields
extern expression_ref ListEnd;

inline expression_ref List() {return ListEnd;}

template <typename First>
auto List(const First& first) -> decltype(Cons*first*ListEnd)
{
  return Cons*first*ListEnd;
}

template <typename First, typename T2>
auto List(const First& first, const T2& t2) -> decltype(Cons*first*List(t2))
{
  return Cons*first*List(t2);
}

template <typename First, typename T2, typename T3>
auto List(const First& first, const T2& t2, const T3& t3) -> decltype(Cons*first*List(t2,t3))
{
  return Cons*first*List(t2,t3);
}

template <typename First, typename T2, typename T3, typename T4>
auto List(const First& first, const T2& t2, const T3& t3, const T4& t4) -> decltype(Cons*first*List(t2,t3,t4))
{
  return Cons*first*List(t2,t3,t4);
}

template <typename First, typename T2, typename T3, typename T4, typename T5>
auto List(const First& first, const T2& t2, const T3& t3, const T4& t4, const T5& t5) -> decltype(Cons*first*List(t2,t3,t4,t5))
{
  return Cons*first*List(t2,t3,t4,t5);
}


template <typename First, typename T2, typename T3, typename T4, typename T5, typename T6>
auto List(const First& first, const T2& t2, const T3& t3, const T4& t4, const T5& t5,const T6& t6) -> decltype(Cons*first*List(t2,t3,t4,t5,t6))
{
  return Cons*first*List(t2,t3,t4,t5,t6);
}


template <typename T,typename U>
std::vector<T> get_vector(const expression_ref& E) 
{
  if (not E)
    return std::vector<T>();

  if (not E.size())
  {
    std::vector<T> v2(1);
    v2[0] = E.as_<U>();
    return v2;
  }
    
  std::vector<T> v2(E.size());
  for(int i=0;i<v2.size();i++)
    v2[i] = E.sub()[i].as_<U>();
  return v2;
}

std::vector<expression_ref> get_ref_vector_from_list(const expression_ref& E);

template <typename T,typename U>
std::vector<T> get_vector_from_list(const expression_ref& E)
{
  std::vector<expression_ref> v1 = get_ref_vector_from_list(E);
  std::vector<T> v2;
  for(int i=0;i<v1.size();i++)
    v2.push_back(v1[i].as_<U>());
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

template <typename T>
expression_ref get_list(const std::vector<T>& v)
{
  std::vector<expression_ref> v2;
  for(int i=0;i<v.size();i++)
    v2.push_back(v[i]);

  return get_list(v2);
}

template<> expression_ref get_list<>(const std::vector<expression_ref>& v);

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
expression_ref unlet(const expression_ref& E);

expression_ref add_prefix(const std::string&, const expression_ref& E);

bool parse_case_expression(const expression_ref& E, expression_ref& T, std::vector<expression_ref>& patterns, std::vector<expression_ref>& bodies);

bool parse_let_expression(const expression_ref& E, std::vector<expression_ref>& vars, std::vector<expression_ref>& bodies, expression_ref& T);

bool parse_indexed_let_expression(const expression_ref& E, std::vector<expression_ref>& bodies, expression_ref& T);

void parse_alternatives(const expression_ref& E, std::vector<expression_ref>& cases, std::vector<expression_ref>& results);

bool is_WHNF(const expression_ref& E);

int get_safe_binder_index(const expression_ref& E);

expression_ref char_list(const std::string& s);

extern const expression_ref v0;
extern const expression_ref v1;
extern const expression_ref v2;
extern const expression_ref v3;
extern const expression_ref v4;
extern const expression_ref v5;
extern const expression_ref v6;
extern const expression_ref v7;
extern const expression_ref v8;

expression_ref operator^(const expression_ref& x, const expression_ref& T);

expression_ref operator+(const expression_ref& E1, const expression_ref& E2);

expression_ref operator*(const expression_ref& E1, const expression_ref& E2);

expression_ref parse_object(const std::string& s);

typedef Vector<expression_ref> EVector;
typedef std::pair<expression_ref,expression_ref> Epair;
typedef Pair<expression_ref,expression_ref> EPair;

template<> 
inline std::string Box<std::vector<expression_ref>>::print() const
{
  std::string s = "{";
  for(int i=0;i<size()-1;i++) {
    s += (*this)[i].print();
    s += ",";
  }
  s += this->back().print() + "}";
  return s;
}

template<> 
inline std::string Box<std::pair<expression_ref,expression_ref>>::print() const
{
  return std::string("(") + first.print() + "," + second.print() + ")";
}

std::vector<double> vec_to_double(const std::vector<expression_ref>& v);
std::vector<int> vec_to_int(const std::vector<expression_ref>& v);

#endif
