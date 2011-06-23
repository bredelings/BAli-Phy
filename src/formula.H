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

// Cycles won't occur in expressions, since each expression naturally uses its sub-expressions as arguments.
//
// Also, note that for some computed values, we do not need to cache values:
//   - e.g. f(g(h(x)).  Just cache the final result.

// TODO: mark expressions that were directly added as top-level expressions.
//
// TODO: We need to know now to *combine* formulas into a larger formula, in order to make
//       model creation easier.  e.f. f(x), x=1, x~Normal(0,sigma^2)

// TODO: We need to know how to evaluate data expressions.
// - use promises?? hopefully not...

// DONE: Implement matching, in order to search for default values, and for bounds.

// DONE: change expression_refs to an object pointer.
// TODO: allow storing (non-functional) prior expresssions, so that we can write out the dependence relations.
// TODO: allow constructing prior EXPRESSIONS (e.g. these may specify constants OR variables).
// TODO: allow tuples to specify arguments to priors.
// TODO: allow lists... for what?

class Formula;

struct term_ref
{
  int index;
  polymorphic_cow_ptr<Formula> F;

  bool is_parameter() const;
  bool is_constant() const;
  bool is_computed() const;
  std::string print() const;

  operator int() const {return index;}
  term_ref();
  term_ref(int i, boost::shared_ptr<const Formula> f);
  term_ref(int i,const Formula& f);
};

class Formula: virtual public Object
{
  struct Term
  {
    expression_ref E;

    std::vector<int> input_indices;
    std::vector<std::pair<int,int> > affected_slots;
    std::vector<int> affected_indices;
    bool top_level;

    Term(const expression_ref& e):E(e),top_level(false) {}
  };

  /// The list of terms that compose this Formula
  std::vector<Term> terms;

  /// cached list of which terms are variables
  std::vector<int> parameter_indices;

  term_ref find_computation(const Operation& o, const std::vector<int>& indices) const;

  term_ref find_term_with_name(const std::string&) const;

  term_ref find_constant_with_value(const object_ref& value) const;

public:
  Formula* clone() const {return new Formula(*this);}

  int size() const {return terms.size();}

  const expression_ref& operator[](int i) const {return terms[i].E;}

  const std::vector<int>& input_indices(int index) const {return terms[index].input_indices;}

  int n_input_indices(int index) const {return input_indices(index).size();}

  const std::vector<int>& affected_indices(int index) const {return terms[index].affected_indices;}

  const std::vector<std::pair<int,int> >& affected_slots(int index) const {return terms[index].affected_slots;}

  int n_affected_indices(int index) const {return terms[index].affected_indices.size();}

  boost::shared_ptr<const Operation> operation(int s) const;

  boost::shared_ptr<const Function> function(int s) const;

  bool has_inputs(int index) const;

  bool is_constant(int index) const;

  int n_parameters() const;

  bool is_parameter(int index) const;

  bool is_computed(int index) const;

  std::string name_for_index(int index) const;

  bool directly_affects(int index1, int index2) const;

  bool directly_affects_in_slot(int index1, int index2, int slot) const;

  void set_directly_affects_in_slot(int index1, int index2, int slot);

  term_ref add_expression(const expression_ref& e);

  term_ref find_expression(const expression_ref& e) const;

  bool find_match_expression(const expression_ref& e, int index, std::vector< expression_ref >&) const;

  term_ref find_match_expression(const expression_ref& e, std::vector< expression_ref >&) const;

  bool find_match2(const expression_ref& e, int index, std::vector<int>&) const;

  term_ref find_match_expression2(const expression_ref& e, std::vector<int>&) const;
};

#endif
