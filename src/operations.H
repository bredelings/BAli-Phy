#ifndef OPERATIONS_H
#define OPERATIONS_H

#include <boost/shared_ptr.hpp>
#include <vector>
#include <string>
#include "myexception.H"
#include "operation.H"
#include "computation.H"
#include "expression.H"

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

#include "formula.H"

template <typename T>
typed_expression_ref<T> operator*(typed_expression_ref<T> arg1, typed_expression_ref<T> arg2)
{
  expression_ref times = lambda_expression( Multiply<T>() );
  return times(arg1,arg2);
}

template <typename T>
typed_expression_ref<T> operator+(typed_expression_ref<T> arg1, typed_expression_ref<T> arg2)
{
  expression_ref plus = lambda_expression( Add<T>() );
  return plus(arg1,arg2);
}

template <typename T>
typed_expression_ref<T> operator>(typed_expression_ref<T> arg1, typed_expression_ref<T> arg2)
{
  expression_ref gt = lambda_expression( GreaterThan<T>() );
  return gt(arg1,arg2);
}

#endif
