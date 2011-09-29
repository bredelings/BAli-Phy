#ifndef OPERATIONS_H
#define OPERATIONS_H

#include <boost/shared_ptr.hpp>
#include <vector>
#include <string>
#include "myexception.H"
#include "operation.H"
#include "computation.H"
#include "expression.H"


struct Apply: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual Apply* clone() const {return new Apply(*this);}

  tribool compare(const Object& o) const 
  {
    const Apply* E = dynamic_cast<const Apply*>(&o);
    return E;
  }

  int precedence() const {return 10;}

  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const;

  std::string name() const;

  Apply():Operation(2) {}
};

struct Case: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
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
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
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

struct Concat: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
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

template<typename T>
struct Minus: public Operation
{
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

#define def_operators(T) \
inline typed_expression_ref<T> operator*(const typed_expression_ref<T>& arg1, const typed_expression_ref<T>& arg2)\
{\
  return lambda_expression( Multiply<T>() )(arg1, arg2);	\
}\
\
inline typed_expression_ref<T> operator/(const typed_expression_ref<T>& arg1, const typed_expression_ref<T>& arg2)\
{\
  return lambda_expression( Divide<T>() )(arg1, arg2);\
}\
\
inline typed_expression_ref<T> operator+(const typed_expression_ref<T>& arg1, const typed_expression_ref<T>& arg2)\
{\
  return lambda_expression( Add<T>() )(arg1, arg2);\
}\
\
inline typed_expression_ref<T> operator-(const typed_expression_ref<T>& arg1, const typed_expression_ref<T>& arg2)\
{\
  return lambda_expression( Minus<T>() )(arg1, arg2);\
}\
\
inline typed_expression_ref<Bool> operator>(const typed_expression_ref<T>& arg1, const typed_expression_ref<T>& arg2)\
{\
  return lambda_expression( GreaterThan<T>() )(arg1, arg2);\
}\
\
inline typed_expression_ref<Bool> operator<(const typed_expression_ref<T>& arg1, const typed_expression_ref<T>& arg2)\
{\
  return lambda_expression( LessThan<T>() )(arg1, arg2);\
}\
\
inline typed_expression_ref<Bool> operator==(const typed_expression_ref<T>& arg1, const typed_expression_ref<T>& arg2)\
{\
  return lambda_expression( Equals<T>() )(arg1, arg2);\
}\

def_operators(Double)

def_operators(Int)

def_operators(Log_Double)

#endif
