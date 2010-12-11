#ifndef VALUE_H
#define VALUE_H

#include <vector>
#include <string>
#include <list>
#include "object.H"
#include "owned-ptr.H"
#include "util.H"

/// A base class for structs that carry information about which parts of multi-part Values are out of date
class out_of_date_message_t {
public:
  virtual out_of_date_message_t* clone() const {return new out_of_date_message_t(*this);}
};

typedef std::list<owned_ptr<out_of_date_message_t> > message_list_t;

enum node_type_t {constant=0, term=1, computed=2, undeclared=3};

class Values;

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
  bool is_constant() const {return intent == constant;}

  /// Is this node a state node?
  bool is_term() const {return intent == term;}

  /// Is this node a computed node?
  bool is_computed() const {return intent == computed;}

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
  bool value_is_a() const 
  {
    const T* ptr = data.as<const T>();
    return ptr;
  }

  template <typename T>
  const T& get_value_as() const 
  {
    const T* ptr = data.as<const T>();
    if (!ptr)
      std::abort();
    return *ptr;
  }

  template <typename T>
        T& get_value_as()
  {
    T* ptr = data.as<T>();
    if (!ptr)
      std::abort();
    return *ptr;
  }

  bool has_value() const {return data;}

  template<typename T>
  void set_value(const T& t) {data = t;}

  ValueBase(node_type_t node_type)
    :up_to_date(false),
     intent(node_type),
     ignoring_messages(false)
  {}

  template <typename T>
  ValueBase(const T& t, node_type_t node_type)
    :up_to_date(false),
     data(t),
     intent(node_type),
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
  // Value() {}  FIXME: this shouldn't crash...  how to handle unset values?
  Value(node_type_t node_type):ValueBase( T(), node_type ) {}
  Value(const T& t, node_type_t node_type):ValueBase(t, node_type) {}
};

/*------------------- StateValue / StateNode -----------------*/
// A state node cannot be out of date: it holds the state!

template <typename T>
struct TermValue: public Value<T>
{
  TermValue<T>* clone() const {return new TermValue<T>(*this);}

  int n_inputs() const {return 0;}

  void update(const Values&, const std::vector<int>&) { std::abort(); };

  // Abort: we don't have any slots!
  bool ignored(const out_of_date_message_t&,int) const { std::abort();}

  // Abort: Terms cannot be out of date!
  message_list_t mark_self_out_of_date() {return message_list_t();}

  std::string formula_expression(const std::vector<std::string>&) const {return "[Term]";}

  const T& get_value() const {return ValueBase::get_value_as<T>();}
        T& get_value()       {return ValueBase::get_value_as<T>();}

  TermValue():Value<T>(term) {}
  TermValue(const T& t):Value<T>(t,term) {ValueBase::up_to_date = true;}
};


/*------------------- InputValue / InputNode -----------------*/

template <typename T>
struct ConstantValue: public Value<T>
{
  ConstantValue<T>* clone() const {return new ConstantValue<T>(*this);}

  int n_inputs() const {return 0;}

  void update(const Values&, const std::vector<int>&) { std::abort(); };

  // Abort: we don't have any slots!
  bool ignored(const out_of_date_message_t&,int) const { std::abort();}

  std::string formula_expression(const std::vector<std::string>&) const {return "[Constant]";}

  const T& get_value() const {return ValueBase::get_value_as<T>();}
        T& get_value()       {std::abort();}

  ConstantValue(const T& t):Value<T>(t,constant) {ValueBase::up_to_date=true;}
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

  MultiplyValue(int i):Value<Double>(computed),n(i) {}
};

/*------------------------------------------------------------------*/

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

/*------------------------------------------------------------------*/
template <typename T, typename U>
struct ConversionValue: public Value<T>
{
  ConversionValue<T,U>* clone() const {return new ConversionValue<T,U>(*this);}

  int n_inputs() const {return 1;}

  void update(const Values& V, const std::vector<int>& mapping);

  std::string formula_expression(const std::vector<std::string>& args) const 
  {
    return std::string("conversion(")+args[0]+")";
  }

  ConversionValue():Value<T>(computed) {}
};


#endif 
