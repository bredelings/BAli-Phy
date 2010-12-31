#ifndef VALUES_H
#define VALUES_H

#include "expression.H"
#include "value.H"

#include "cow-ptr.H"

struct affected_index_t {
  int index;
  int slot;
  affected_index_t(int i, int s):index(i),slot(s) {}
};

//FIXME -- but a constant node actually needs a VALUE, right?
//         And that value is part of the FORMULA, right?

// The user should never see the object with undelivered messages, I think.

class Formula;

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

  int get_index_for_id(int id) const;

  template <typename T>
  int get_index(const Parameter<T>& P) const
  {
    return get_index_for_id(P.node->id);
  }

  bool completely_up_to_date(int i) const {return get_value(i).completely_up_to_date();}
  bool completely_out_of_date(int i) const {return get_value(i).completely_out_of_date();}

  void mark_out_of_date(int i);

  void mark_up_to_date(int i) {get_value(i).mark_up_to_date();}

  void calculate_value(int i);

  std::string expression() const;

  bool is_term(int i) const;

  bool is_constant(int i) const;

  bool is_computed(int i) const;

  template <typename T>
  const T& get_value_as(int i) const {return values[i]->get_value_as<T>();}

  // we might want to modify the value of an intermediate node in doing calculations
  template <typename T>
  T& get_value_as(int i)       {mark_out_of_date(i);return values[i]->get_value_as<T>();}

  // we might want to modify the value of an intermediate node in doing calculations
  template <typename T>
          T& get_value_no_invalidate_as(int i)  {return values[i]->get_value_as<T>();}

  template <typename T>
  void set_value(int index, const T& t) {values[index]->set_value(t);}

  template <typename T>
  const T& operator[](const Parameter<T>& P) const
  {
    int index = get_index_for_id(P.node->id);
    return get_value_as<T>(index);
  }

  template <typename T>
        T& operator[](const Parameter<T>& P)
  {
    int index = get_index_for_id(P.node->id);
    return get_value_as<T>(index);
  }

  Values(const Formula&);
};

#include "value.HC"
#endif
