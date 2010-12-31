#ifndef EXPRESSIONS_H
#define EXPRESSIONS_H

#include <string>
#include "value.H"
#include "cow-ptr.H"

class ParameterBase
{
  std::string name;
public:
  static int total;

  // A unique identifier for an expression - whether free or bound
  int id;

  /// The function / result object
  polymorphic_cow_ptr<ValueBase> exemplar;

  virtual node_type_t node_type() const {return exemplar->node_type();}

  bool is_constant() const {return node_type() == constant;}

  bool is_term() const {return node_type() == term;}

  bool is_computed() const {return node_type() == computed;}

  virtual ParameterBase* clone() const =0;

  const std::string& expression() const {return name;}

  ParameterBase(const std::string& s,const ValueBase& V);

  virtual ~ParameterBase() {}
};

template <typename T>
struct TermBase: public ParameterBase
{
  TermBase* clone() const {return new TermBase(*this);}

  TermBase(const std::string& s):ParameterBase(s,TermValue<T>() ) { }
};

template <typename T>
struct ConstantBase: public ParameterBase
{
  ConstantBase* clone() const {return new ConstantBase(*this);}

  ConstantBase(const T& t):ParameterBase(convertToString(t),ConstantValue<T>(t)) { }
};

struct FreeParameterBase: public ParameterBase
{
  FreeParameterBase* clone() const {return new FreeParameterBase(*this);}

  node_type_t node_type() const {return computed;}

  std::vector<polymorphic_cow_ptr<ParameterBase> > inputs;

  FreeParameterBase(const ValueBase& V, const std::vector<polymorphic_cow_ptr<ParameterBase> >& i);
};

template<typename T>
struct Constant;

/// This is the general type
template<typename T>
struct Parameter
{
  typedef T type;
  polymorphic_cow_ptr<ParameterBase> node;

  Parameter() { }

  // Copy exactly -- no conversion
  Parameter(const Parameter<T>& P):node(P.node) { }

  // Generate a conversion
  template <typename U>
  Parameter(const Parameter<U>& P);

  // Generate a constant?
  Parameter(const T& t)
  {
    *this = Constant<T>(t);
  }

  // Why doesn't this do anything?
  //  template <typename U>
  //  Parameter(const typename boost::enable_if_c< boost::is_convertible<U,T>::value,U>::type& u)
  //  {
  //    T t = u;
  //    *this = Constant<T>(t);
  //  }
};

/// This is the wrapper for a state node
template<typename T>
struct Term: public Parameter<T>
{
  Term(const std::string& s)
  {
    Parameter<T>::node = polymorphic_cow_ptr<ParameterBase>( TermBase<T>(s) );
  }
};

/// This is the wrapper for a state node
template<typename T>
struct Constant: public Parameter<T>
{
  Constant(const T& t)
  {
    Parameter<T>::node = polymorphic_cow_ptr<ParameterBase>( ConstantBase<T>(t) );
  }

  template <typename U>
  Constant(const U& u)
  {
    Parameter<T>::node = polymorphic_cow_ptr<ParameterBase>( ConstantBase<T>(u) );
  }
};

/// This is the wrapper for a computed node
template<typename T>
struct Expression: public Parameter<T>
{
  Expression(const ValueBase& V, const std::vector<polymorphic_cow_ptr<ParameterBase> >& inputs)
  {
    Parameter<T>::node = polymorphic_cow_ptr<ParameterBase>(new FreeParameterBase(V,inputs));
  }
};

#endif
