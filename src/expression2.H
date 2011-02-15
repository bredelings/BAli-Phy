/*---------------------------------------------------------------------*/
#include <boost/shared_ptr.hpp>
#include <vector>
#include <string>
#include <iostream>
#include "cow-ptr.H"
#include "object.H"
#include "util.H"
#include "myexception.H"

using boost::shared_ptr;
using std::vector;
using std::string;
using std::pair;

struct Computation;
struct Operation;

struct Formula: public Object
{
  struct Term
  {
    boost::shared_ptr<Operation> op;
    std::vector<int> input_indices;
    std::vector<std::pair<int,int> > affected_slots;
    std::vector<int> affected_indices;
    std::string name;
    shared_ptr<const Object> constant_value;

    Term() {}
    Term(shared_ptr<const Object> O):constant_value(O) { }
  };

  std::vector<Term> terms;

public:
  Formula* clone() const {return new Formula(*this);}

  int size() const {return terms.size();}

  const std::vector<int>& input_indices(int index) const {return terms[index].input_indices;}

  int n_input_indices(int index) const {return input_indices(index).size();}

  const std::vector<int>& affected_indices(int index) const {return terms[index].affected_indices;}

  const std::vector<pair<int,int> >& affected_slots(int index) const {return terms[index].affected_slots;}

  int n_affected_indices(int index) const {return terms[index].affected_indices.size();}

  boost::shared_ptr<const Operation> operation(int s) const {return terms[s].op;}

  bool has_inputs(int index) const;

  bool is_constant(int index) const;

  bool directly_affects(int index1, int index2) const;

  bool directly_affects_in_slot(int index1, int index2, int slot) const;

  void set_directly_affects_in_slot(int index1, int index2, int slot);

  void add_computed_node(const Operation& o, const std::vector<int>& indices);

  void add_state_node(const std::string& name);

  void add_constant_node(const std::string& name, const Object&  value);

  void add_constant_node(const std::string& name, boost::shared_ptr<const Object> value);
};

// Values
struct Context: public Object
{
  // member type
  struct value: public Object
  {
    value* clone() const {return new value(*this);}

    bool computed;

    boost::shared_ptr<const Computation> computation;
    boost::shared_ptr<const Object> result;

    value():computed(false) { }
  };

  // data members
  polymorphic_cow_ptr<Formula> F;

  std::vector< boost::shared_ptr<value> > values;

  // member functions
  Context* clone() const {return new Context(*this);}

  boost::shared_ptr<const Object> evaluate(int slot_to_eval);

  void set_value(int index, boost::shared_ptr<const Object> O);

  void set_value(int index, const Object& O);

  int size() const {return F->size();}

  bool index_may_affect_index(int, int) const;

  Context(const polymorphic_cow_ptr<Formula>&);
};

std::ostream& operator<<(std::ostream&, const Context& C);

struct Computation: public Object
{
  Computation* clone() const {return new Computation(*this);}

  vector<shared_ptr<const Object> > used_values;
  vector<int> slots_used_order;

  Computation(int n_inputs):used_values(n_inputs) { }
};

struct OperationArgs
{
  Context& CTX;
  shared_ptr<Computation> computation;
  int index_of_caller;

  OperationArgs(Context& A, int i)
    :CTX(A), index_of_caller(i) 
  { 
    int n_input_slots = CTX.F->n_input_indices(index_of_caller);

    computation = boost::shared_ptr<Computation>( new Computation(n_input_slots) );
  }

  shared_ptr<const Object> evaluate(int slot)
  {
    int index_to_evaluate = CTX.F->input_indices(index_of_caller)[slot];
    if (not computation->used_values[slot])
    {
      computation->used_values[slot] = CTX.evaluate(index_to_evaluate);
      computation->slots_used_order.push_back(slot);
    }

    assert( computation->used_values[slot] );

    return computation->used_values[slot];
  }

  template <typename T>
  shared_ptr<const T> evaluate_as(int slot)
  {
    return boost::dynamic_pointer_cast<const T>(evaluate(slot));
  }

};

struct Operation: public Object
{
  virtual Operation* clone() const=0;
  virtual shared_ptr<const Object> operator()(OperationArgs& Args) const =0;
  virtual std::string expression(const std::vector<std::string>& inputs) const;
};

struct Multiply: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual Multiply* clone() const {return new Multiply(*this);}

  shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    shared_ptr<const Double> x = Args.evaluate_as<Double>(0);
    shared_ptr<const Double> y = Args.evaluate_as<Double>(1);

    shared_ptr<Double> r ( new Double );

    *r = (*x) * (*y);

    return r;
  }

  std::string expression(const std::vector<std::string>& inputs) const;
};

struct IfThenElse: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual IfThenElse* clone() const {return new IfThenElse(*this);}

  shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    shared_ptr<const Bool> condition = Args.evaluate_as<Bool>(0);
    if (condition)
      return Args.evaluate(1);
    else
      return Args.evaluate(2);
  }
};

template<typename X, typename Y>
struct GreaterThan: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual IfThenElse* clone() const {return new IfThenElse(*this);}

  shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    shared_ptr<const X> x = Args.evaluate_as<X>(0);
    shared_ptr<const Y> y = Args.evaluate_as<Y>(1);

    Bool condition = (*x > *y);

    return shared_ptr<const Bool>(new Bool(condition));
  }
};

template<typename X, typename Y>
struct Conversion: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual IfThenElse* clone() const {return new IfThenElse(*this);}

  shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    shared_ptr<const X> x = Args.evaluate_as<X>(0);
    Y y = *x;

    return shared_ptr<const Y>(new Y(y));
  }
};

struct Add: public Operation
{
  // The 'context' should include the knowledge of the input slots
  // so that the computation doesn't need to know what input slots it is connecting to what output slot.
  virtual Add* clone() const {return new Add(*this);}

  shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    shared_ptr<const Double> x = Args.evaluate_as<Double>(0);
    shared_ptr<const Double> y = Args.evaluate_as<Double>(1);

    shared_ptr<Double> r ( new Double );

    *r = (*x) + (*y);

    return r;
  }

  std::string expression(const std::vector<std::string>& inputs) const;
};

/*-----------------------------------------------------------------------------*/


