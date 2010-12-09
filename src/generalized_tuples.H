#ifndef GENERALIZED_TUPLES_H
#define GENERALIZED_TUPLES_H

#include <vector>
#include <list>
#include <string>
#include "cow-ptr.H"
#include "util.H"
#include "owned-ptr.H"

class Formula;
class Values;
class ValueBase;

class ParameterBase;
class FreeParameterBase;
class BoundParameterBase;
template <typename T> class Parameter;

enum node_type_t {constant=0, input=1, state=2, computed=3, undeclared=4};

struct affected_index_t {
  int index;
  int slot;
  affected_index_t(int i, int s):index(i),slot(s) {}
};

/// A Formula for a generic tuple, where some values are computed from others
class Formula
{
  /// The ordered list of entries, some of which have others as "inputs".
  std::vector<polymorphic_cow_ptr<ValueBase> > Nodes;

  /// The name of each Node
  std::vector<std::string> Node_names;

  /// The ordered indices of the Nodes that are input for each computed Node
  std::vector< std::vector<int> > Node_inputs;

  /// The nodes that are directly affected by changing each node.
  std::vector< std::vector< affected_index_t > > Nodes_affected;
public:
  Formula* clone() const {return new Formula(*this);}

  /// How many entries in this tuple?
  int size() const {return Nodes.size();}

  /// How many input objects are there for the i-thentry?
  int n_inputs(int i) const {return Node_inputs[i].size();}

  /// Get the j-th input index for entry i
  int input_index(int i,int j) const {return Node_inputs[i][j];}

  // What type is entry i?
  node_type_t node_type(int i) const;

  /// Get the indices of the inputs for the i-th entry.
  const std::vector<int>& input_indices(int i) const {return Node_inputs[i];}

  /// Get the indices of the entries affected by changing the i-th entry.
  const std::vector<affected_index_t>& affected_indices(int i) const {return Nodes_affected[i];}

  /// Get a (string) expression for the ith entry
  std::string expression_for_entry(int i) const;

  /// The name of the i-th entry
  std::string entry_name(int i) const {return Node_names[i];}

  polymorphic_cow_ptr<ValueBase> get_new_entry_value(int i) const;

  bool is_input_entry(int i) const;

  bool is_state_entry(int i) const;

  bool is_constant_entry(int i) const;

  bool is_computed_entry(int i) const;

  /// Make an additional entry called name and of type Node, with no inputs.
  int add_entry(const std::string& name, const ValueBase& Node);

  /// Make an additional entry called name and of type Node, computed from indices inputs
  int add_entry(const std::string& name, const ValueBase& Node,const std::vector<int>& inputs);

  int add_entry(const std::string& name, const ParameterBase& P);

  template <typename T>
  int add_entry(const std::string& name, const Parameter<T>& P);
};

//FIXME -- but a constant node actually needs a VALUE, right?
//         And that value is part of the FORMULA, right?

/// A base class for structs that carry information about which parts of multi-part Values are out of date
class out_of_date_message_t {
public:
  virtual out_of_date_message_t* clone() const {return new out_of_date_message_t(*this);}
};

typedef std::list<owned_ptr<out_of_date_message_t> > message_list_t;

class Object {
public:
  virtual Object* clone() const =0;
  virtual ~Object() {}
};

template<typename T>
class POD: public Object 
{
  T t;
public:
  POD<T>* clone() const {return new POD<T>(*this);}
  operator T() const {return t;}
  operator T&() {return t;}
  POD() {}
  POD(T t2):t(t2) {}
};

typedef POD<double> Double;
typedef POD<int> Int;
typedef POD<unsigned> Unsigned;

/// A generic Value for each entry that will be either constant, input, state, and computed
class ValueBase
{
protected:
  bool up_to_date;

  owned_ptr<Object> data;

  node_type_t intent;

  bool ignoring_messages;
public:
  virtual ValueBase* clone() const=0;

  /// Is the cached value able to be used?
  virtual bool completely_up_to_date() const {return up_to_date;}

  /// Is it true that no information is cached?
  virtual bool completely_out_of_date() const {return not up_to_date;}

  /// Mark this value as being already computed
  virtual void mark_up_to_date() {up_to_date=true;}

  /// How many inputs does this function node take?
  virtual int n_inputs() const = 0;

  virtual std::vector<std::string> input_names() const;

  /// Is this node a constant node?
  bool is_constant_node() const {return intent == constant;}

  /// Is this node a state node?
  bool is_state_node() const {return intent == state;}

  /// Is this node an input node?
  bool is_input_node() const {return intent == input;}

  /// Is this node a computed node?
  bool is_computed_node() const {return intent == computed;}

  /// What type of node is this?
  node_type_t node_type() const {return intent;}  /// Will this class ignore a message m

  virtual bool ignored(const out_of_date_message_t& /*m*/, int /*slot*/) const { return ignoring_messages; }

  /// FIXME: how does the delivery of messages related to their being ignored?
  /// If we queue messages and then mark this node as recieving messages, then they won't be ignored at the
  /// time they are recieved.
  /// Therefore, if we start ignoring messages, we need to first flush the queue to recieve un-ignored messages
  /// Also, if we stop ignoring messages, we need to first flush the queue to not (later) recieve ignored messages.
  /// So, this must be handle partly from the Values class.

  /// Mark this value as being out-of-date
  virtual message_list_t mark_self_out_of_date()
  {
    if (not completely_up_to_date())
    {
      up_to_date = false;

      owned_ptr<out_of_date_message_t> message = claim(new out_of_date_message_t);
      return message_list_t(1,message);
    }
    else
    {
      up_to_date = false;
      return message_list_t();
    }
  }

  /// Notify this value that certain of its inputs are out-of-date
  virtual message_list_t notify_input_out_of_date(const Values&, const out_of_date_message_t&, int /*slot*/)
  {
    return mark_self_out_of_date();
  }

  /// Recompute the value
  virtual void update(const Values&,const std::vector<int>&)=0;

  virtual std::string result_expression() const=0;

  virtual std::string formula_expression(const std::vector<std::string>&) const=0;

  template <typename T>
  const T& get_value_as() const {return *data.as<const T>();}

  template <typename T>
        T& get_value_as()       {return *data.as<      T>();}

  bool has_value() const {return data;}

  template<typename T>
  void set_value(const T& t) {data = t;}

  ValueBase()
    :up_to_date(false),
     intent(undeclared),
     ignoring_messages(false)
  {}

  template <typename T>
  ValueBase(const T& t)
    :up_to_date(false),
     data(t),
     intent(undeclared),
     ignoring_messages(false)
  { }

  virtual ~ValueBase() {}
};

template <typename T>
struct Value: public ValueBase
{
  Value<T>* clone() const=0;

  std::string result_expression() const
  {
    if (has_value())
      return convertToString(get_value_as<T>());
    else
      return "[undefined]";
  }
  // Value() {}  FIXME: this shouldn't crash...
  Value():ValueBase( T() ) {}
  Value(const T& t):ValueBase(t) {}
};

/*------------------- StateValue / StateNode -----------------*/
// A state node cannot be out of date: it holds the state!

template <typename T>
struct StateValue: public Value<T>
{
  StateValue<T>* clone() const {return new StateValue<T>(*this);}

  int n_inputs() const {return 0;}

  void update(const Values&, const std::vector<int>&) { std::abort(); };

  // Abort: we don't have any slots!
  bool ignored(const out_of_date_message_t&,int) const { std::abort();}

  // Abort: States cannot be out of date!
  message_list_t mark_self_out_of_date() {return message_list_t();}

  std::string formula_expression(const std::vector<std::string>&) const {return "[State]";}

  const T& get_value() const {return ValueBase::get_value_as<T>();}
        T& get_value()       {return ValueBase::get_value_as<T>();}

  StateValue() {}
  StateValue(const T& t):ValueBase(t) {ValueBase::up_to_date = true;}
};


/*------------------- InputValue / InputNode -----------------*/

template <typename T>
struct InputValue: public Value<T>
{
  InputValue<T>* clone() const {return new InputValue<T>(*this);}

  int n_inputs() const {return 0;}

  void update(const Values&, const std::vector<int>&) { std::abort(); };

  // Abort: we don't have any slots!
  bool ignored(const out_of_date_message_t&,int) const { std::abort();}

  std::string formula_expression(const std::vector<std::string>&) const {return "[Input]";}

  const T& get_value() const {return ValueBase::get_value_as<T>();}
        T& get_value()       {return ValueBase::get_value_as<T>();}

  InputValue() {}
  InputValue(const T& t):ValueBase(t) {}
};

/*------------------- MultiplyValue / MultiplyNode -----------------*/

class MultiplyValue: public Value<Double>
{
  int n;
public:
  MultiplyValue* clone() const {return new MultiplyValue(*this);}

  int n_inputs() const {return n;}

  std::string formula_expression(const std::vector<std::string>& args) const;

  void update(const Values& V, const std::vector<int>& mapping);

  MultiplyValue(int i):n(i) {}
};

class FunctionValue: public Value<Double>
{
  std::string name;
  double (*function)(double, double);
public:
  FunctionValue* clone() const {return new FunctionValue(*this);}

  int n_inputs() const {return 2;}

  std::string formula_expression(const std::vector<std::string>&) const;

  void update(const Values& V, const std::vector<int>& mapping);

  FunctionValue(const std::string& s, double (*f)(double,double));
};

// The user should never see the object with undelivered messages, I think.

class Values
{
  std::vector<polymorphic_cow_ptr<ValueBase> > values;
  
  polymorphic_cow_ptr<const Formula> F;

  std::vector<message_list_t> unprocessed_messages;

protected:
  const ValueBase& get_value(int i) const {return *values[i];}
        ValueBase& get_value(int i)       {return *values[i];}

  void process_messages();

  void record_changes_no_deliver(int x, message_list_t&);

  void notify_x_of_change_in_slot_y(int x, int y, const out_of_date_message_t&);

public:  
  Values* clone() const {return new Values(*this);}

  int size() const {return values.size();}

  bool completely_up_to_date(int i) const {return get_value(i).completely_up_to_date();}
  bool completely_out_of_date(int i) const {return get_value(i).completely_out_of_date();}

  void mark_out_of_date(int i);

  void mark_up_to_date(int i) {get_value(i).mark_up_to_date();}

  void calculate_value(int i);

  std::string expression() const;

  bool is_input_entry(int i) const;

  bool is_state_entry(int i) const;

  bool is_constant_entry(int i) const;

  bool is_computed_entry(int i) const;

  template <typename T>
  const T& get_value_as(int i) const {return values[i]->get_value_as<T>();}

  // we might want to modify the value of an intermediate node in doing calculations
  template <typename T>
  T& get_value_as(int i)       {mark_out_of_date(i);return values[i]->get_value_as<T>();}

  // we might want to modify the value of an intermediate node in doing calculations
  template <typename T>
          T& get_value_no_invalidate_as(int i)  {return values[i]->get_value_as<T>();}

  Values(const Formula&);
};

struct ParameterBase
{
  static int total;
  int id;

  // boost::shared_ptr<Formula> F;
  // Is this parameter associated with a formula?
  // (Also, do we want to be able to see if two nodes are associated with the SAME formula?
  //  That might cause problems when forumlae that are on the stack go out of scope.
  //  Also, if we are using smart pointers, they will duplicate the stack object and refer to the duplicate!)
  // (We want to used shared_ptr<> because we want changes made outside the parameter object to be reflected in
  //  the parameter object).

  virtual node_type_t node_type() const=0;

  virtual ParameterBase* clone() const =0;

  ParameterBase();
  virtual ~ParameterBase() {}
};

struct BoundParameterBase: public ParameterBase
{
  boost::shared_ptr<Formula> F;
  int index;

  BoundParameterBase* clone() const {return new BoundParameterBase(*this);}

  node_type_t node_type() const {return F->node_type(index);}
  std::string name() const {return F->entry_name(index);}
  BoundParameterBase(boost::shared_ptr<Formula> f, int i)
    :F(f),index(i)
  { }
};

struct FreeParameterBase: public ParameterBase
{
  FreeParameterBase* clone() const {return new FreeParameterBase(*this);}

  // boost::shared_ptr<Formula> F;
  // Is this parameter associated with a formula?
  // (Also, do we want to be able to see if two nodes are associated with the SAME formula?
  //  That might cause problems when forumlae that are on the stack go out of scope.
  //  Also, if we are using smart pointers, they will duplicate the stack object and refer to the duplicate!)
  // (We want to used shared_ptr<> because we want changes made outside the parameter object to be reflected in
  //  the parameter object).

  node_type_t node_type() const {return computed;}

  polymorphic_cow_ptr<ValueBase> exemplar;
  std::vector<polymorphic_cow_ptr<ParameterBase> > inputs;

  FreeParameterBase* clone() {return new FreeParameterBase(*this);}

  FreeParameterBase();
  FreeParameterBase(const ValueBase& V, const std::vector<polymorphic_cow_ptr<ParameterBase> >& i);
};


/// This is the general type
template<typename T>
struct Parameter
{
  typedef T type;
  polymorphic_cow_ptr<ParameterBase> node;

  Parameter() { }
};

/// This is the wrapper for a state node
template<typename T>
struct State: public Parameter<T>
{
  State(const std::string& s, boost::shared_ptr<Formula>& F)
  {
    int index = F->add_entry(s,StateValue<T>());
    Parameter<T>::node = polymorphic_cow_ptr<ParameterBase>(new BoundParameterBase(F,index));
  }
};

/// This is the wrapper for a state node
template<typename T>
struct Input: public Parameter<T>
{
  Input(const std::string& s, boost::shared_ptr<Formula>& F)
  {
    int index = F->add_entry(s,InputValue<T>());
    Parameter<T>::node = polymorphic_cow_ptr<ParameterBase>(new BoundParameterBase(F,index));
  }
};

/// This is the wrapper for a computed node
template<typename T>
struct Expression: public Parameter<T>
{
  Expression(const ValueBase& V, const std::vector<polymorphic_cow_ptr<ParameterBase> >& i)
  {
    Parameter<T>::node = polymorphic_cow_ptr<ParameterBase>(new FreeParameterBase(V,i));
  }
};

// Question: when do we *implicitly* add objects?
// A1: we should add (copies) of all intermediate nodes.
// A2: this should not change the actually expressions, though.

template <typename T>
int Formula::add_entry(const std::string& name, const Parameter<T>& P)
  {
    // This doesn't work yet
    std::abort();
    polymorphic_cow_ptr<FreeParameterBase> free = P.node;
    polymorphic_cow_ptr<BoundParameterBase> bound = P.node;
    std::vector<int> inputs;
    if (free) {
      return add_entry(name, *(free->exemplar),inputs);
    }
    else
      std::abort();
  }
Parameter<Double> operator*(const Parameter<Double>& p1, const Parameter<Double>& p2);
#endif
