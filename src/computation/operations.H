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

  closure operator()(OperationArgs& Args) const;

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

  closure operator()(OperationArgs& Args) const
  {
    object_ptr<const Object> x = Args.evaluate(0);

    return closure(object_ref( new String(x->print() ) ));
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

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "seq";}

  Seq():Operation(2) {}
};

struct Join: public Operation
{
  virtual Join* clone() const {return new Join(*this);}

  tribool compare(const Object& o) const 
  {
    const Join* E = dynamic_cast<const Join*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "join";}

  Join():Operation(2) {}
};

struct Reapply: public Operation
{
  virtual Reapply* clone() const {return new Reapply(*this);}

  tribool compare(const Object& o) const 
  {
    const Reapply* E = dynamic_cast<const Reapply*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "reapply";}

  Reapply():Operation(2) {}
};

extern expression_ref seq;

struct MkArray: public Operation
{
  virtual MkArray* clone() const {return new MkArray(*this);}

  tribool compare(const Object& o) const 
  {
    const MkArray* E = dynamic_cast<const MkArray*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const;

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

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "bounds";}
  
  ArrayBounds():Operation(1) {}
};

extern expression_ref bounds;

struct GetIndex: public Operation
{
  virtual GetIndex* clone() const {return new GetIndex(*this);}

  tribool compare(const Object& o) const 
  {
    const GetIndex* E = dynamic_cast<const GetIndex*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const;

  std::string print_expression(const std::vector<std::string>& inputs) const
  {
    return print_infix_expression(inputs);
  }

  std::string name() const {return "!";}
  
  GetIndex():Operation(2) {}
};

extern expression_ref getIndex;

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

  closure operator()(OperationArgs& Args) const
  {
    object_ptr<const POD_math<X> > x = Args.evaluate_as<POD_math<X> >(0);

    return object_ptr<const POD_math<Y>>(new POD_math<Y>(*x));
  }

  std::string name() const {return "Conversion";}

  Conversion():Operation(1) {}
};

struct Negate: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual Negate* clone() const {return new Negate(*this);}

  tribool compare(const Object& o) const 
  {
    const Negate* E = dynamic_cast<const Negate*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "negate";}

  Negate():Operation(1) {}
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
def_binary_operator1(==,Equals)
#undef def_binary_operator1

struct Log_Op: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual Log_Op* clone() const {return new Log_Op(*this);}

  tribool compare(const Object& o) const;

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "log";}

  Log_Op():Operation(1) {}
};

extern const expression_ref Log;

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

struct Exp_Op: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual Exp_Op* clone() const {return new Exp_Op(*this);}

  tribool compare(const Object& o) const;

  closure operator()(OperationArgs& Args) const;

  std::string name() const {return "sqrt";}

  Exp_Op():Operation(1) {}
};

#endif
