#ifndef GENERALIZED_TUPLES_H
#define GENERALIZED_TUPLES_H

#include <vector>
#include <list>
#include <string>
#include "cow-ptr.H"
#include "util.H"
#include "owned-ptr.H"

class Formula;
class FormulaNode;
class Values;
class ValueBase;

/// A Formula for a generic tuple, where some values are computed from others
class Formula
{
  /// The ordered list of entries, some of which have others as "inputs".
  std::vector<polymorphic_cow_ptr<FormulaNode> > Nodes;

  /// The name of each Node
  std::vector<std::string> Node_names;

  /// The ordered indices of the Nodes that are input for each computed Node
  std::vector< std::vector<int> > Node_inputs;

  /// The nodes that are directly affected by changing each node.
  std::vector< std::vector<int> > Node_outputs;
public:
  Formula* clone() const {return new Formula(*this);}

  /// How many entries in this tuple?
  int size() const {return Nodes.size();}

  /// How many input objects are there for the i-thentry?
  int n_inputs(int i) const {return Node_inputs[i].size();}

  /// Get the j-th input index for entry i
  int input_index(int i,int j) const {return Node_inputs[i][j];}

  /// Get the indices of the inputs for the i-th entry.
  const std::vector<int>& input_indices(int i) const {return Node_inputs[i];}

  /// Get the indices of the entries affected by changing the i-th entry.
  const std::vector<int>& output_indices(int i) const {return Node_outputs[i];}

  /// Get a (string) expression for the ith entry
  std::string expression_for_entry(int i) const;

  /// The name of the i-th entry
  std::string entry_name(int i) const {return Node_names[i];}

  const FormulaNode& get_entry(int i) const {return *Nodes[i];}
        FormulaNode& get_entry(int i)       {return *Nodes[i];}

  /// Make an additional entry called name and of type Node, with no inputs.
  int add_entry(const std::string& name, const FormulaNode& Node);

  /// Make an additional entry called name and of type Node, computed from indices inputs
  int add_entry(const std::string& name, const FormulaNode& Node,const std::vector<int>& inputs);
};

/// An entry for a specific index in a Formula
class FormulaNode
{
protected:
  /// Each input is marked as having a name for the purpose of computing values of this formula node
  std::vector<std::string> input_names_;
public:
  virtual FormulaNode* clone() const =0;

  /// What are the names of the arguments for this FormulaNode?
  const std::vector<std::string>& input_names() const {return input_names_;}

  /// How many inputs does a FormulaNode of this type take?
  int n_inputs() const {return input_names_.size();}

  /// Get a new Value object of this type
  virtual ValueBase* create_new_value() const=0;

  /// Describe this expression
  virtual std::string expression(const std::vector<std::string>& argument_names) const=0;

  virtual ~FormulaNode() {}
};

/// A base class for structs that carry information about which parts of multi-part Values are out of date
class out_of_date_message_t {
public:
  virtual out_of_date_message_t* clone() const {return new out_of_date_message_t(*this);}
};

typedef std::list<owned_ptr<out_of_date_message_t> > message_list_t;

/// A generic Value for each FormulaNode that will be either (i) a state or (ii) computed from other Values
class ValueBase
{
protected:
  bool up_to_date;
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

  /// Will this class ignore a message m
  virtual bool ignored(const out_of_date_message_t& m) const { return false; }

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
  virtual message_list_t notify_input_out_of_date(const Values&, const out_of_date_message_t&, int)
  {
    return mark_self_out_of_date();
  }

  /// Recompute the value
  virtual void update(const Values&,const std::vector<int>&)=0;

  virtual std::string expression() const=0;

  ValueBase():up_to_date(false) {}

  virtual ~ValueBase() {}
};

template <typename T>
struct Value: public ValueBase
{
  Value<T>* clone() const=0;
  T value;
  std::string expression() const
  {
    return convertToString(value);
  }
  Value() {}
  Value(const T& t):value(t) {}
};

/*------------------- StateValue / StateNode -----------------*/
// A state node cannot be out of date: it holds the state!

template <typename T>
struct StateValue: public Value<T>
{
  StateValue<T>* clone() const {return new StateValue<T>(*this);}
  int n_inputs() const {return 0;}
  void update(const Values&, const std::vector<int>&) { std::abort(); };

  // A state value ignores all messages: it can't be out of date!
  // FIXME: Should we abort, though?
  bool ignored(const out_of_date_message_t&) const {return true;}

  // A state value ignores all messages: it can't be out of date!
  // FIXME: Should we abort, though?
  message_list_t mark_self_out_of_date() {return message_list_t();}

  StateValue() {}
  StateValue(const T& t):Value<T>(t) {Value<T>::up_to_date = true;}
};

template <typename T>
class StateNode: public FormulaNode
{
  StateNode<T>* clone() const {return new StateNode<T>(*this);}
public:
  StateValue<T>* create_new_value() const {return new StateValue<T>;}
  std::string expression(const std::vector<std::string>&) const { std::abort(); }
};

/*------------------- InputValue / InputNode -----------------*/

template <typename T>
struct InputValue: public Value<T>
{
  InputValue<T>* clone() const {return new InputValue<T>(*this);}
  int n_inputs() const {return 0;}
  void update(const Values&, const std::vector<int>&) { std::abort(); };
};

template <typename T>
class InputNode: public FormulaNode
{
  InputNode<T>* clone() const {return new InputNode<T>(*this);}
public:
  InputValue<T>* create_new_value() const {return new InputValue<T>;}
  std::string expression(const std::vector<std::string>&) const { std::abort(); }
};

/*------------------- MultiplyValue / MultiplyNode -----------------*/

class MultiplyValue: public Value<double>
{
  int n;
public:
  MultiplyValue* clone() const {return new MultiplyValue(*this);}
  int n_inputs() const {return n;}
  void update(const Values& V, const std::vector<int>& mapping);
  MultiplyValue(int i):n(i) {}
};

class MultiplyNode: public FormulaNode
{
  int n;
public:
  MultiplyNode* clone() const {return new MultiplyNode(*this);}

  MultiplyValue* create_new_value() const {return new MultiplyValue(n);}

  std::string expression(const std::vector<std::string>&) const;

  MultiplyNode(int i);
};


class Values
{
  std::vector<polymorphic_cow_ptr<ValueBase> > values;
  
  polymorphic_cow_ptr<const Formula> F;

  std::vector<message_list_t> unprocessed_messages;

protected:
  void process_messages(); // user should NEVER need to call this manually...

public:  
  Values* clone() const {return new Values(*this);}

  const ValueBase& get_value(int i) const {return *values[i];}
        ValueBase& get_value(int i)       {return *values[i];}

  int size() const {return values.size();}

  bool completely_up_to_date(int i) const {return get_value(i).completely_up_to_date();}
  bool completely_out_of_date(int i) const {return get_value(i).completely_out_of_date();}

  void record_changes_no_deliver(int x, message_list_t&);

  void notify_x_of_change_in_y(int x, int y, const out_of_date_message_t&);

  void mark_out_of_date(int i);

  void mark_up_to_date(int i) {get_value(i).mark_up_to_date();}

  void calculate_value(int i);

  std::string expression() const;

  template <typename T>
  const T& get_value_as(int i) const {return dynamic_cast<const Value<T>&>(*values[i]).value;}

  // we might want to modify the value of an intermediate node in doing calculations
  template <typename T>
  T& get_value_as(int i)       {mark_out_of_date(i);return dynamic_cast< Value<T>&>(*values[i]).value;}

  // we might want to modify the value of an intermediate node in doing calculations
  template <typename T>
          T& get_value_no_invalidate_as(int i)  {return dynamic_cast<      T&>(*values[i]);}

  Values(const Formula&);
};
#endif
