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
//  may change as the Context changes.  Still, the Formula represents the (entire, current)
// model, even if it changes

// Goal: Allow other objects without values that may reference objects that can take values.
//       - bounds / domains
//       - default values (default_value X v)
//            We could allow v to be a computed expression.  It would have to not depend on X, though.
//            As a computed expression, it could refer to a constant node.  We would evaluate that node
//             to get its value.
//       - assertions
//       - unique names for computed values.  (e.g. Z = X*Y)
//         + how does this relate to the unique INTEGER names?
//         + how to separate the expression description "X*Y" from the *name* "Z"?
//         + can I merge Z=X*Y and W=X*Y even though that have different names?
//           - The second one could be a nickname.  But lets abort/complain, for now.
//         + solution: let's make names an external tag.  That way, an expression could have multiple names.
//       - attributes (e.g. var X)
//       - transition kernels,
//       - priors
//       - default priors
//       - logging routines
//       - random number generators
//       - IO and file reading or writing
//       - ... and general procedures!
//
// Querying?  E.g. Does X have a default value? -> Is there a (default_value X v) expression?
//
// 

// Cycles are probably prohibited... as we do not need self-reference in that way.
//   - Hmm... recursive functions are useful, though.
//
// Also, note that for some computed values, we do not need to cache values:
//   - e.g. f(g(h(x)).  Just cache the final result.

// Hmm... if expressions can be used in modal operators, then it might matter if they are
//  top-level expressions, or just a sub-expression of another expression.
// For example, default_prior(x~Normal(0,1)) would have x~Normal(0,1) as a sub-expression.
// For new perhaps just let sub-models set the prior.

// TODO: We need to know now to *combine* formulas into a larger formula, in order to make
//       model creation easier.  e.f. f(x), x=1, x~Normal(0,sigma^2)

// Q: What is the simplest way to introduce an unlimited number of different term types?
// A: Can I make all terms be expressions?
// 1. That way I don't have to invent new terms, to get new expressions.
// 2. Each expressions can be decomposed so that it only references other terms.
// - I could restrict the kind of expressions that are allowed.
//   (I should replace "state nodes" with named_parameter_expression.
//
// - Currently, I allow
//    + constant terms (holds value)
//    + variable (hold: nothing? default value? bounds?)
//    + operation expressions (holds operation)
//
// - all terms have:
//    * list of inputs
//      - cached: list of outputs
//      - cached: list of outputs with slots
//    * how do I modify expression to make the idea of sub-expressions general?
//    * how do I handle (f x) really being of type f and having just 1 sub-expression, x, when this is true?
//
// - I can disallow lambda expressions. (do not opt-in)
//

// I could separate expressions (in the Formula) from FACTORED expression (in the Context?)
// Then expressions would be only modified to use term_refs as arguments for computational purposes...

struct Formula: virtual public Object
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

  /// The list of terms that compose this Formula
  std::vector<Term> terms;

  /// cached list of which terms are variables
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
