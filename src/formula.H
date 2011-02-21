#ifndef FORMULA_H
#define FORMULA_H

#include <boost/shared_ptr.hpp>
#include <vector>
#include <string>
#include <utility>
#include "cow-ptr.H"
#include "object.H"
#include "expression.H"

struct Formula;

struct Computation;
struct Operation;
struct expression_ref;

// Eventually the Formula should be the secondary object, cuz the evaluation tree
//  may change as the Context changes.

struct Formula: public Object
{
  struct Term
  {
    boost::shared_ptr<Operation> op;
    std::vector<int> input_indices;
    std::vector<std::pair<int,int> > affected_slots;
    std::vector<int> affected_indices;
    std::string name;
    boost::shared_ptr<const Object> default_value;
    bool constant;

    Term():constant(false) {}
    Term(boost::shared_ptr<const Object> O):default_value(O),constant(false) { }
  };

  std::vector<Term> terms;

  std::vector<int> state_indices;

  term_ref find_computation(const Operation& o, const std::vector<int>& indices) const;

  term_ref add_term(const Term&);

public:
  Formula* clone() const {return new Formula(*this);}

  int size() const {return terms.size();}

  const std::vector<int>& input_indices(int index) const {return terms[index].input_indices;}

  int n_input_indices(int index) const {return input_indices(index).size();}

  const std::vector<int>& affected_indices(int index) const {return terms[index].affected_indices;}

  const std::vector<std::pair<int,int> >& affected_slots(int index) const {return terms[index].affected_slots;}

  int n_affected_indices(int index) const {return terms[index].affected_indices.size();}

  boost::shared_ptr<const Operation> operation(int s) const {return terms[s].op;}

  bool has_inputs(int index) const;

  bool is_constant(int index) const;

  int n_state_nodes() const;

  bool is_state(int index) const;

  bool is_computed(int index) const;

  std::string name_for_index(int index) const;

  bool directly_affects(int index1, int index2) const;

  bool directly_affects_in_slot(int index1, int index2, int slot) const;

  void set_directly_affects_in_slot(int index1, int index2, int slot);

  term_ref add_computed_node(const Operation& o, const std::vector<int>& indices);

  term_ref add_computed_node(const expression_ref& e);

  term_ref add_state_node(const std::string& name);

  term_ref add_state_node(const std::string& name, const Object&);

  term_ref add_state_node(const std::string& name, boost::shared_ptr<const Object> value);

  term_ref add_constant_node(const std::string& name, const Object&  value);

  term_ref add_constant_node(const Object&  value);

  term_ref add_constant_node(const std::string& name, boost::shared_ptr<const Object> value);

  term_ref add_constant_node(boost::shared_ptr<const Object> value);
};

#endif
