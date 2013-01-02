#ifndef OPERATIONS_H
#define OPERATIONS_H

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

  closure operator()(OperationArgs& Args) const;

  std::string name() const;

  Apply():Operation(-1) {}
};

struct Case: public Operation
{
  virtual Case* clone() const {return new Case(*this);}

  tribool compare(const Object& o) const 
  {
    const Case* E = dynamic_cast<const Case*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const;

  Case():Operation(2) {}
};

template<typename T, typename U>
struct Vector_From_List_Op: public Operation
{
  virtual Vector_From_List_Op* clone() const {return new Vector_From_List_Op(*this);}

  tribool compare(const Object& o) const 
  {
    const Vector_From_List_Op* E = dynamic_cast<const Vector_From_List_Op<T,U>*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const
  {
    object_ptr<Box<std::vector<T> > > v (new Box<std::vector<T> >);

    const closure* top = &Args.lazy_evaluate(0);
    while(top->exp->size())
    {
      assert(is_exactly(top->exp,":"));
      assert(top->exp->size() == 2);

      int element_index = assert_is_a<index_var>(top->exp->sub[0])->index;
      int element_reg = top->lookup_in_env( element_index );

      int next_index = assert_is_a<index_var>(top->exp->sub[1])->index;
      int next_reg = top->lookup_in_env( next_index );

      // Add the element to the list.
      v->t.push_back( *convert<const U>(Args.evaluate_reg(element_reg)) );
      // Move to the next element or end
      top = &Args.lazy_evaluate_reg_closure(next_reg);
    }
    assert(is_exactly(top->exp,"[]"));

    return v;
  }

  std::string name() const {return "Vector_From_List";}
  
  Vector_From_List_Op():Operation(1) {}
};

template <typename T,typename U>
expression_ref Vector_From_List()
{
  return lambda_expression(Vector_From_List_Op<T,U>());
}



struct StringSizeOp: public Operation
{
  virtual StringSizeOp* clone() const {return new StringSizeOp(*this);}

  tribool compare(const Object& o) const 
  {
    const StringSizeOp* E = dynamic_cast<const StringSizeOp*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const
  {
    const std::string& s = *Args.evaluate_as<String>(0);

    return Int(s.size());
  }

  std::string name() const {return "StringSize";}
  
  StringSizeOp():Operation(1) {}
};

struct BuiltinGetStringIndexOp: public Operation
{
  virtual BuiltinGetStringIndexOp* clone() const {return new BuiltinGetStringIndexOp(*this);}

  tribool compare(const Object& o) const 
  {
    const BuiltinGetStringIndexOp* E = dynamic_cast<const BuiltinGetStringIndexOp*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const
  {
    const std::string& s = *Args.evaluate_as<String>(0);
    int i = *Args.evaluate_as<Int>(1);

    return Char(s[i]);
  }

  std::string name() const {return "builtinGetStringIndex";}
  
  BuiltinGetStringIndexOp():Operation(2) {}
};

struct BuiltinNewStringOp: public Operation
{
  virtual BuiltinNewStringOp* clone() const {return new BuiltinNewStringOp(*this);}

  tribool compare(const Object& o) const 
  {
    const BuiltinNewStringOp* E = dynamic_cast<const BuiltinNewStringOp*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const
  {
    const int& length = *Args.evaluate_as<Int>(0);

    object_ptr<String> v (new String);

    v->t.resize(length);

    return v;
  }

  std::string name() const {return "builtinNewString";}
  
  BuiltinNewStringOp():Operation(1) {}
};

struct BuiltinSetStringIndexOp: public Operation
{
  virtual BuiltinSetStringIndexOp* clone() const {return new BuiltinSetStringIndexOp(*this);}

  tribool compare(const Object& o) const 
  {
    const BuiltinSetStringIndexOp* E = dynamic_cast<const BuiltinSetStringIndexOp*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const
  {
    object_ptr<const String> v = Args.evaluate_as<String>(0);
    int i = *Args.evaluate_as<Int>(1);
    char x = *Args.evaluate_as<Char>(2);

    const String* vv = &(*v);
    String* vvv = const_cast<String*>(vv);
    vvv->t[i] = x;

    return constructor("()",0);
  }

  std::string name() const {return "builtinSetStringIndex";}
  
  BuiltinSetStringIndexOp():Operation(3) {}
};

template<typename T>
struct VectorSizeOp: public Operation
{
  virtual VectorSizeOp* clone() const {return new VectorSizeOp(*this);}

  tribool compare(const Object& o) const 
  {
    const VectorSizeOp* E = dynamic_cast<const VectorSizeOp<T>*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const
  {
    object_ptr<const Box<std::vector<T> > > v = Args.evaluate_as<Box<std::vector<T> > >(0);

    const std::vector<T>& v2 = *v;
    object_ptr<const Int> r ( new Int(v2.size() ) );

    return r;
  }

  std::string name() const {return "VectorSize";}
  
  VectorSizeOp():Operation(1) {}
};

template <typename T>
expression_ref VectorSize()
{
  return lambda_expression(VectorSizeOp<T>());
}

template<typename T>
struct BuiltinNewVectorOp: public Operation
{
  virtual BuiltinNewVectorOp* clone() const {return new BuiltinNewVectorOp(*this);}

  tribool compare(const Object& o) const 
  {
    const BuiltinNewVectorOp* E = dynamic_cast<const BuiltinNewVectorOp<T>*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const
  {
    object_ptr<const Int> length = Args.evaluate_as<Int>(0);

    object_ptr<Vector<T>> v (new Vector<T>);

    v->t.resize(*length);

    return v;
  }

  std::string name() const {return "builtinNewVector";}
  
  BuiltinNewVectorOp():Operation(1) {}
};

template <typename T>
expression_ref BuiltinNewVector()
{
  return lambda_expression(BuiltinNewVectorOp<T>());
}


template<typename T, typename U>
struct BuiltinSetVectorIndexOp: public Operation
{
  virtual BuiltinSetVectorIndexOp* clone() const {return new BuiltinSetVectorIndexOp(*this);}

  tribool compare(const Object& o) const 
  {
    const BuiltinSetVectorIndexOp* E = dynamic_cast<const BuiltinSetVectorIndexOp<T,U>*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const
  {
    object_ptr<const Vector<T>> v = Args.evaluate_as<Vector<T>>(0);
    int i = *Args.evaluate_as<Int>(1);
    U x = *Args.evaluate_as<U>(2);

    const Vector<T>* vv = &(*v);
    Vector<T>* vvv = const_cast<Vector<T>*>(vv);
    vvv->t[i] = x;

    return constructor("()",0);
  }

  std::string name() const {return "builtinSetVectorIndex";}
  
  BuiltinSetVectorIndexOp():Operation(3) {}
};

template <typename T, typename U>
expression_ref BuiltinSetVectorIndex()
{
  return lambda_expression(BuiltinSetVectorIndexOp<T,U>());
}



template<typename T, typename U>
struct BuiltinGetVectorIndexOp: public Operation
{
  virtual BuiltinGetVectorIndexOp* clone() const {return new BuiltinGetVectorIndexOp(*this);}

  tribool compare(const Object& o) const 
  {
    const BuiltinGetVectorIndexOp* E = dynamic_cast<const BuiltinGetVectorIndexOp<T,U>*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const
  {
    object_ptr<const Vector<T>> v = Args.evaluate_as<Vector<T>>(0);
    int i = *Args.evaluate_as<Int>(1);

    return U(v->t[i]);
  }

  std::string name() const {return "builtinGetVectorIndex";}
  
  BuiltinGetVectorIndexOp():Operation(2) {}
};

template <typename T, typename U>
expression_ref BuiltinGetVectorIndex()
{
  return lambda_expression(BuiltinGetVectorIndexOp<T,U>());
}



template <typename T>
struct Iota_Op: public Operation
{
  Iota_Op* clone() const {return new Iota_Op(*this);}

  tribool compare(const Object& O) const
  {
    const Iota_Op* E = dynamic_cast<const Iota_Op<T>*>(&O);
    return E;
  }

  closure operator()(OperationArgs& Args) const
  {
    unsigned n = *Args.evaluate_as<Int>(0);

    object_ptr<Box<std::vector<T> > > V(new Box<std::vector<T> >() );

    V->t.resize(n);
    for(unsigned i=0;i<n;i++)
      V->t[i] = i;

    return V;
  }
 
  std::string name() const {return "Iota";}

  Iota_Op(): Operation(1) { }
};
 
template <typename T>
expression_ref Iota()
{
  return lambda_expression(Iota_Op<T>());
}

struct LExp_Op: public Operation
{
  LExp_Op* clone() const {return new LExp_Op(*this);}
  
  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;
    
    if (typeid(*this) != typeid(O)) return false;
    
    return true;
  }
  
  closure operator()(OperationArgs& Args) const;
  
  std::string name() const {return "LExp";}
  
  LExp_Op(): Operation(3) { }
};

extern expression_ref LExp;

struct Multiply: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual Multiply* clone() const {return new Multiply(*this);}

  tribool compare(const Object& o) const 
  {
    const Multiply* E = dynamic_cast<const Multiply*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const;

  std::string print_expression(const std::vector<std::string>& inputs) const
  {
    return print_infix_expression(inputs);
  }

  std::string name() const {return "*";}

  Multiply():Operation(2) {}
};

struct GreaterThan: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual GreaterThan* clone() const {return new GreaterThan(*this);}

  tribool compare(const Object& o) const 
  {
    const GreaterThan* E = dynamic_cast<const GreaterThan*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const;

  std::string print_expression(const std::vector<std::string>& inputs) const
  {
    return print_infix_expression(inputs);
  }

  std::string name() const {return ">";}

  GreaterThan():Operation(2) {}
};

struct LessThan: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual LessThan* clone() const {return new LessThan(*this);}

  tribool compare(const Object& o) const 
  {
    const LessThan* E = dynamic_cast<const LessThan*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const;

  std::string print_expression(const std::vector<std::string>& inputs) const
  {
    return print_infix_expression(inputs);
  }

  std::string name() const {return "<";}

  LessThan():Operation(2) {}
};

struct Equals: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual Equals* clone() const {return new Equals(*this);}

  tribool compare(const Object& o) const 
  {
    const Equals* E = dynamic_cast<const Equals*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const;

  std::string print_expression(const std::vector<std::string>& inputs) const
  {
    return print_infix_expression(inputs);
  }

  std::string name() const {return "==";}

  Equals():Operation(2) {}
};

struct NotEquals: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual NotEquals* clone() const {return new NotEquals(*this);}

  tribool compare(const Object& o) const 
  {
    const NotEquals* E = dynamic_cast<const NotEquals*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const;

  std::string print_expression(const std::vector<std::string>& inputs) const
  {
    return print_infix_expression(inputs);
  }

  std::string name() const {return "/=";}

  NotEquals():Operation(2) {}
};

struct Add: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual Add* clone() const {return new Add(*this);}

  tribool compare(const Object& o) const 
  {
    const Add* E = dynamic_cast<const Add*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const;

  std::string print_expression(const std::vector<std::string>& inputs) const
  {
    return print_infix_expression(inputs);
  }

  std::string name() const {return "+";}

  Add():Operation(2) {}
};

#include <boost/type_traits/is_arithmetic.hpp>
#include <boost/utility/enable_if.hpp>

struct Minus: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual Minus* clone() const {return new Minus(*this);}

  tribool compare(const Object& o) const 
  {
    const Minus* E = dynamic_cast<const Minus*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const;

  std::string print_expression(const std::vector<std::string>& inputs) const
  {
    return print_infix_expression(inputs);
  }

  std::string name() const {return "-";}

  Minus():Operation(2) {}
};

struct Divide: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual Divide* clone() const {return new Divide(*this);}

  tribool compare(const Object& o) const 
  {
    const Divide* E = dynamic_cast<const Divide*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const;

  std::string print_expression(const std::vector<std::string>& inputs) const
  {
    return print_infix_expression(inputs);
  }

  std::string name() const {return "/";}

  Divide():Operation(2) {}
};

struct Mod: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual Mod* clone() const {return new Mod(*this);}

  tribool compare(const Object& o) const 
  {
    const Mod* E = dynamic_cast<const Mod*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const;

  std::string print_expression(const std::vector<std::string>& inputs) const
  {
    return print_infix_expression(inputs);
  }

  std::string name() const {return "mod";}

  Mod():Operation(2) {}
};

#define check_arithmetic(T) typename boost::enable_if<boost::is_arithmetic<T>,T>::type
#define check_arithmetic2(T1,T2) typename boost::enable_if<boost::is_arithmetic<T1>,T2>::type

#define def_binary_operator1(Op,M) \
template <typename T> \
check_arithmetic2(T,expression_ref) operator Op(const expression_ref& E, const T& t) \
{ \
  return (lambda_expression(M()),E,t); \
} \
 \
template <typename T> \
check_arithmetic2(T,expression_ref) operator Op(const T& t, const expression_ref& E) \
{ \
  return (lambda_expression(M()),t,E); \
} \
expression_ref operator Op(const expression_ref& E1, const expression_ref& E2);


#define def_binary_operator2(Op,M) \
expression_ref operator Op(const expression_ref& E1, const expression_ref& E2) \
{ \
  return (lambda_expression(M()),E1,E2); \
}

def_binary_operator1(-,Minus)
def_binary_operator1(+,Add)
def_binary_operator1(*,Multiply)
def_binary_operator1(/,Divide)
def_binary_operator1(<,LessThan)
def_binary_operator1(>,GreaterThan)
//def_binary_operator1(==,Equals)
//def_binary_operator1(!=,NotEquals)
#undef def_binary_operator1

struct Sqrt_Op: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual Sqrt_Op* clone() const {return new Sqrt_Op(*this);}

  tribool compare(const Object& o) const;

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "sqrt";}

  Sqrt_Op():Operation(1) {}
};

extern const expression_ref Sqrt;

#endif
