#ifndef GENERALIZED_TUPLES_H
#define GENERALIZED_TUPLES_H

#include <vector>
#include <list>
#include <string>
#include "cow-ptr.H"
#include "util.H"
#include "owned-ptr.H"
#include <boost/type_traits.hpp>
#include <boost/utility/enable_if.hpp>

#include "values.H"


class Formula;
class Values;
class ValueBase;

class ParameterBase;
class FreeParameterBase;
template <typename T> class Parameter;

template <typename T>
struct entry_tag
{
  int index;
  entry_tag(int i):index(i) {}
};

/// A Formula for a generic tuple, where some values are computed from others
class Formula
{
  /// The ordered list of entries, some of which have others as "inputs".
  std::vector<polymorphic_cow_ptr<ParameterBase> > Nodes;

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

  /// Get the id for an object
  int get_id_for_index(int) const;

  /// Look up an object by id
  int get_index_for_id(int) const;

  /// Look up an object by id
  int get_index_for_term_name(const std::string&) const;

  // What type is entry i?
  node_type_t node_type(int i) const;

  /// Get the indices of the inputs for the i-th entry.
  const std::vector<int>& input_indices(int i) const {return Node_inputs[i];}

  /// Get the indices of the entries affected by changing the i-th entry.
  const std::vector<affected_index_t>& affected_indices(int i) const {return Nodes_affected[i];}

  /// Get a (string) expression for the ith entry
  std::string expression_for_entry(int i) const;

  polymorphic_cow_ptr<ValueBase> get_new_entry_value(int i) const;

  bool is_term(int i) const;

  bool is_constant(int i) const;

  bool is_computed(int i) const;

  int add_entry(const polymorphic_cow_ptr<ParameterBase>& P, const std::vector<int>&);

  int add_entry(const polymorphic_cow_ptr<ParameterBase>& P);

  template <typename T>
  int add_entry(const Parameter<T>& P)
  {
    return add_entry(P.node);
  }
};

template <typename T> struct Constant;

// Question: when do we *implicitly* add objects?
// A1: we should add (copies) of all intermediate nodes.
// A2: this should not change the actually expressions, though.


// try to allow downcasting from Derived to Base
// template <typename T>
// template <typename U>
// Parameter<T>::Parameter(const Parameter<U>& P)
// {
//   node = P.node;
// }
//
// See boost::enable_if ??  
// Problem is probably that two alternative implementions need to be mutually exclusive - order doesn't matter.

/// Do I really want to do this?  This might allow SILENT casting from double -> int, as well as int -> double
template <typename T>
template <typename U>
Parameter<T>::Parameter(const Parameter<U>& P)
{
  std::vector<polymorphic_cow_ptr<ParameterBase> > inputs;
  inputs.push_back(P.node);
  node = polymorphic_cow_ptr<FreeParameterBase>(new FreeParameterBase(ConversionValue<T,U>(),inputs));
}

#endif
