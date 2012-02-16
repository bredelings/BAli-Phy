#ifndef OPERATIONS_H
#define OPERATIONS_H

#include <boost/shared_ptr.hpp>
#include <vector>
#include <string>
#include "myexception.H"
#include "operation.H"
#include "computation.H"
#include "expression.H"


// In general, these operations simply ask for the value of each input
// from the OperationArgs object.  They therefore need have no knowledge of
// how these values are arrived at.

struct Apply: public Operation
{
  virtual Apply* clone() const {return new Apply(*this);}

  tribool compare(const Object& o) const 
  {
    const Apply* E = dynamic_cast<const Apply*>(&o);
    return E;
  }

  int precedence() const {return 10;}

  assoc_type associativity() const {return assoc_left;}

  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const;

  std::string name() const;

  Apply():Operation(2) {}
};

struct Case: public Operation
{
  virtual Case* clone() const {return new Case(*this);}

  tribool compare(const Object& o) const 
  {
    const Case* E = dynamic_cast<const Case*>(&o);
    return E;
  }

  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const;

  std::string name() const;

  Case():Operation(2) {}
};

struct Print: public Operation
{
  virtual Print* clone() const {return new Print(*this);}

  tribool compare(const Object& o) const 
  {
    const Print* E = dynamic_cast<const Print*>(&o);
    return E;
  }

  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    boost::shared_ptr<const Object> x = Args.evaluate(0);

    return boost::shared_ptr<String> ( new String(x->print() ) );
  }

  std::string name() const {return "sys_print";}

  Print():Operation(1) {}
};

struct Seq: public Operation
{
  virtual Seq* clone() const {return new Seq(*this);}

  tribool compare(const Object& o) const 
  {
    const Seq* E = dynamic_cast<const Seq*>(&o);
    return E;
  }

  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const;

  std::string name() const {return "seq";}

  Seq():Operation(2) {}
};

extern expression_ref seq;

struct Concat: public Operation
{
  virtual Concat* clone() const {return new Concat(*this);}

  tribool compare(const Object& o) const 
  {
    const Concat* E = dynamic_cast<const Concat*>(&o);
    return E;
  }

  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    boost::shared_ptr<const String> x = Args.evaluate_as<String>(0);
    boost::shared_ptr<const String> y = Args.evaluate_as<String>(1);

    const std::string& X = *x;
    const std::string& Y = *y;

    return boost::shared_ptr<String> ( new String(X+Y) );
  }

  std::string print_expression(const std::vector<std::string>& inputs) const
  {
    return print_infix_expression(inputs);
  }

  std::string name() const {return "++";}

  Concat():Operation(2) {}
};

struct MkArray: public Operation
{
  virtual MkArray* clone() const {return new MkArray(*this);}

  tribool compare(const Object& o) const 
  {
    const MkArray* E = dynamic_cast<const MkArray*>(&o);
    return E;
  }

  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const;

  std::string name() const {return "mkArray";}
  
  MkArray():Operation(2) {}
};

extern expression_ref mkArray;

struct ArrayBounds: public Operation
{
  virtual ArrayBounds* clone() const {return new ArrayBounds(*this);}

  tribool compare(const Object& o) const 
  {
    const ArrayBounds* E = dynamic_cast<const ArrayBounds*>(&o);
    return E;
  }

  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const;

  std::string name() const {return "bnds";}
  
  ArrayBounds():Operation(1) {}
};

extern expression_ref bnds;

struct GetIndex: public Operation
{
  virtual GetIndex* clone() const {return new GetIndex(*this);}

  tribool compare(const Object& o) const 
  {
    const GetIndex* E = dynamic_cast<const GetIndex*>(&o);
    return E;
  }

  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const;

  std::string print_expression(const std::vector<std::string>& inputs) const
  {
    return print_infix_expression(inputs);
  }

  std::string name() const {return "!";}
  
  GetIndex():Operation(2) {}
};

extern expression_ref getIndex;


template <typename T>
struct Multiply: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual Multiply* clone() const {return new Multiply(*this);}

  tribool compare(const Object& o) const 
  {
    const Multiply<T>* E = dynamic_cast<const Multiply<T>*>(&o);
    return E;
  }

  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    boost::shared_ptr<const T> x = Args.evaluate_as<T>(0);
    boost::shared_ptr<const T> y = Args.evaluate_as<T>(1);

    boost::shared_ptr<T> r ( new T );

    *r = (*x) * (*y);

    return r;
  }

  std::string print_expression(const std::vector<std::string>& inputs) const
  {
    return print_infix_expression(inputs);
  }

  std::string name() const {return "*";}

  Multiply():Operation(2) {}
};

struct IfThenElse: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual IfThenElse* clone() const {return new IfThenElse(*this);}

  tribool compare(const Object& o) const 
  {
    const IfThenElse* E = dynamic_cast<const IfThenElse*>(&o);
    return E;
  }

  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    boost::shared_ptr<const Bool> condition = Args.evaluate_as<Bool>(0);
    if (*condition)
      return Args.evaluate(1);
    else
      return Args.evaluate(2);
  }

  std::string expression(const std::vector<std::string>& inputs) const;

  std::string name() const {return "if";}

  IfThenElse():Operation(3) {}
};

template<typename T>
struct GreaterThan: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual GreaterThan* clone() const {return new GreaterThan(*this);}

  tribool compare(const Object& o) const 
  {
    const GreaterThan<T>* E = dynamic_cast<const GreaterThan<T>*>(&o);
    return E;
  }

  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    boost::shared_ptr<const T> x = Args.evaluate_as<T>(0);
    boost::shared_ptr<const T> y = Args.evaluate_as<T>(1);

    Bool condition = (*x > *y);

    return boost::shared_ptr<const Bool>(new Bool(condition));
  }

  std::string print_expression(const std::vector<std::string>& inputs) const
  {
    return print_infix_expression(inputs);
  }

  std::string name() const {return ">";}

  GreaterThan():Operation(2) {}
};

template<typename T>
struct LessThan: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual LessThan* clone() const {return new LessThan(*this);}

  tribool compare(const Object& o) const 
  {
    const LessThan<T>* E = dynamic_cast<const LessThan<T>*>(&o);
    return E;
  }

  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    boost::shared_ptr<const T> x = Args.evaluate_as<T>(0);
    boost::shared_ptr<const T> y = Args.evaluate_as<T>(1);

    Bool condition = (*x < *y);

    return boost::shared_ptr<const Bool>(new Bool(condition));
  }

  std::string print_expression(const std::vector<std::string>& inputs) const
  {
    return print_infix_expression(inputs);
  }

  std::string name() const {return "<";}

  LessThan():Operation(2) {}
};

template<typename T>
struct Equals: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual Equals* clone() const {return new Equals(*this);}

  tribool compare(const Object& o) const 
  {
    const Equals<T>* E = dynamic_cast<const Equals<T>*>(&o);
    return E;
  }

  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    boost::shared_ptr<const T> x = Args.evaluate_as<T>(0);
    boost::shared_ptr<const T> y = Args.evaluate_as<T>(1);

    Bool condition = (*x == *y);

    return boost::shared_ptr<const Bool>(new Bool(condition));
  }

  std::string print_expression(const std::vector<std::string>& inputs) const
  {
    return print_infix_expression(inputs);
  }

  std::string name() const {return "==";}

  Equals():Operation(2) {}
};

template<typename X, typename Y>
struct Conversion: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual Conversion* clone() const {return new Conversion(*this);}

  tribool compare(const Object& o) const 
  {
    const Conversion<X,Y>* E = dynamic_cast<const Conversion<X,Y>*>(&o);
    return E;
  }

  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    boost::shared_ptr<const X> x = Args.evaluate_as<X>(0);
    Y y = *x;

    return boost::shared_ptr<const Y>(new Y(y));
  }

  std::string name() const {return "Conversion";}

  Conversion():Operation(1) {}
};

template<typename T>
struct Add: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual Add* clone() const {return new Add(*this);}

  tribool compare(const Object& o) const 
  {
    const Add<T>* E = dynamic_cast<const Add<T>*>(&o);
    return E;
  }

  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    boost::shared_ptr<const T> x = Args.evaluate_as<T>(0);
    boost::shared_ptr<const T> y = Args.evaluate_as<T>(1);

    boost::shared_ptr<T> r ( new T );

    *r = (*x) + (*y);

    return r;
  }

  std::string print_expression(const std::vector<std::string>& inputs) const
  {
    return print_infix_expression(inputs);
  }

  std::string name() const {return "+";}

  Add():Operation(2) {}
};

#include <boost/type_traits/is_arithmetic.hpp>
#include <boost/utility/enable_if.hpp>

template<typename T >
struct Minus: public Operation
{
  typedef typename T::base_type base_type;

  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual Minus* clone() const {return new Minus(*this);}

  tribool compare(const Object& o) const 
  {
    const Minus<T>* E = dynamic_cast<const Minus<T>*>(&o);
    return E;
  }

  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    boost::shared_ptr<const T> x = Args.evaluate_as<T>(0);
    boost::shared_ptr<const T> y = Args.evaluate_as<T>(1);

    boost::shared_ptr<T> r ( new T );

    *r = (*x) - (*y);

    return r;
  }

  std::string print_expression(const std::vector<std::string>& inputs) const
  {
    return print_infix_expression(inputs);
  }

  std::string name() const {return "-";}

  Minus():Operation(2) {}
};

template<typename T>
struct Divide: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual Divide* clone() const {return new Divide(*this);}

  tribool compare(const Object& o) const 
  {
    const Divide<T>* E = dynamic_cast<const Divide<T>*>(&o);
    return E;
  }

  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    boost::shared_ptr<const T> x = Args.evaluate_as<T>(0);
    boost::shared_ptr<const T> y = Args.evaluate_as<T>(1);

    boost::shared_ptr<T> r ( new T );

    *r = (*x) / (*y);

    return r;
  }

  std::string print_expression(const std::vector<std::string>& inputs) const
  {
    return print_infix_expression(inputs);
  }

  std::string name() const {return "/";}

  Divide():Operation(2) {}
};

#define check_arithmetic(T) typename boost::enable_if<boost::is_arithmetic<T>,T>::type
#define check_arithmetic2(T1,T2) typename boost::enable_if<boost::is_arithmetic<T1>,T2>::type

#define def_binary_operator1(Op,M) \
template <typename T> \
typed_expression_ref<POD_math<check_arithmetic(T) > > operator Op(const expression_ref& E, const T& t) \
{ \
  return typed_expression_ref<POD_math<T> >((lambda_expression(M<POD_math<T> >()),E,t)); \
} \
 \
template <typename T> \
typed_expression_ref<POD_math<check_arithmetic(T)> > operator Op(const T& t, const expression_ref& E) \
{ \
  return typed_expression_ref<POD_math<T> >((lambda_expression(M<POD_math<T> >()),t,E)); \
} \
 \
template <typename T> \
typed_expression_ref<POD_math<T> > operator Op(const expression_ref& E1, const typed_expression_ref<POD_math<T> >& E2) \
{ \
  return typed_expression_ref<POD_math<T> >((lambda_expression(M<POD_math<T> >()),E1,E2)); \
} \
\
template <typename T> \
typed_expression_ref<POD_math<T> > operator Op(const typed_expression_ref<POD_math<T> >& E1, const expression_ref& E2) \
{ \
  return typed_expression_ref<POD_math<T> >((lambda_expression(M<POD_math<T> >()),E1,E2)); \
} \
 \
template <typename T> \
typed_expression_ref<POD_math<T> > operator Op(const T& t, const typed_expression_ref<POD_math<T> >& E2) \
{ \
  return typed_expression_ref<POD_math<T> >((lambda_expression(M<POD_math<T> >()),t,E2)); \
} \
\
template <typename T> \
typed_expression_ref<POD_math<T> > operator Op(const typed_expression_ref<POD_math<T> >& E1, const T& t) \
{ \
  return typed_expression_ref<POD_math<T> >((lambda_expression(M<POD_math<T> >()),E1,t)); \
} \
 \
template <typename T> \
typed_expression_ref<POD_math<T> > operator Op(const typed_expression_ref<POD_math<T> >& E1, const typed_expression_ref<POD_math<T> >& E2) \
{ \
  return typed_expression_ref<POD_math<T> >((lambda_expression(M<POD_math<T> >()),E1,E2)); \
}

def_binary_operator1(-,Minus)
def_binary_operator1(+,Add)
def_binary_operator1(*,Multiply)
def_binary_operator1(/,Divide)

#undef def_binary_operator1

#define def_binary_operator2(Op,M) \
template <typename T> \
 typed_expression_ref<check_arithmetic2(T,Bool)> operator Op(const expression_ref& E1, const T& t) \
{\
  return typed_expression_ref<Bool>((lambda_expression(M<POD_math<T> >()),E1,t)); \
} \
\
template <typename T> \
typed_expression_ref<check_arithmetic2(T,Bool)> operator Op(const T& t, const expression_ref& E2) \
{\
  return typed_expression_ref<Bool>((lambda_expression(M<POD_math<T> >()),t,E2)); \
}\
\
template <typename T> \
typed_expression_ref<Bool> operator Op(const expression_ref& E1, const typed_expression_ref<POD_math<T> >& E2) \
{\
  return typed_expression_ref<Bool>((lambda_expression(M<POD_math<T> >()),E1,E2)); \
}\
\
template <typename T> \
typed_expression_ref<Bool> operator Op(const typed_expression_ref<POD_math<T> >& E1, const expression_ref& E2) \
{\
  return typed_expression_ref<Bool>((lambda_expression(M<POD_math<T> >()),E1,E2)); \
}\
\
template <typename T> \
typed_expression_ref<Bool> operator Op(const T& t, const typed_expression_ref<POD_math<T> >& E2) \
{\
  return typed_expression_ref<Bool>((lambda_expression(M<POD_math<T> >()),t,E2)); \
}\
\
template <typename T> \
typed_expression_ref<Bool> operator Op(const typed_expression_ref<POD_math<T> >& E1, const T& t) \
{\
  return typed_expression_ref<Bool>((lambda_expression(M<POD_math<T> >()),E1,t)); \
}\
\
template <typename T> \
typed_expression_ref<Bool> operator Op(const typed_expression_ref<POD_math<T> >& E1, const typed_expression_ref<POD_math<T> >& E2) \
{\
  return typed_expression_ref<Bool>((lambda_expression(M<POD_math<T> >()),E1,E2)); \
}

def_binary_operator2(<,LessThan)
def_binary_operator2(>,GreaterThan)
def_binary_operator2(==,Equals)
#undef def_binary_operator2

#undef check_arithmetic
#undef check_arithmetic2

#endif
