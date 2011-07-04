#include "myexception.H"

#include "expression.H"
#include "formula.H"
#include "operation.H"

#include "util.H"

#include <iostream>

using boost::shared_ptr;
using boost::dynamic_pointer_cast;
using std::vector;
using std::string;
using std::pair;
using std::cerr;

bool term_ref::is_parameter() const {return F->is_parameter(index);}
bool term_ref::is_constant() const {return F->is_constant(index);}
bool term_ref::is_computed() const {return F->is_computed(index);}
string term_ref::print() const {return F->name_for_index(index);}
term_ref::term_ref():index(-1) { }
term_ref::term_ref(int i,const Formula& f):index(i),F(f.clone()) {}
term_ref::term_ref(int i,boost::shared_ptr<const Formula> f):index(i),F(f) {}


string Formula::name_for_index(int index) const
{
  return terms[index].E->print();
}

bool Formula::has_inputs(int index) const 
{
  bool is_internal = (n_input_indices(index) > 0);
  if (not is_internal)
    assert(not operation(index));
  else
    assert(operation(index));

  return is_internal;
}

boost::shared_ptr<const Operation> Formula::operation(int index) const
{
  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(terms[index].E);
  if (not E)
    return shared_ptr<const Operation>();

  return dynamic_pointer_cast<const Operation>(E->sub[0]);
}

boost::shared_ptr<const Function> Formula::function(int index) const
{
  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(terms[index].E);
  if (not E)
    return shared_ptr<const Function>();

  return dynamic_pointer_cast<const Function>(E->sub[0]);
}

bool Formula::is_constant(int index) const
{
  if (dynamic_pointer_cast<const expression>(terms[index].E)) return false;

  if (dynamic_pointer_cast<const parameter>(terms[index].E)) return false;

  if (dynamic_pointer_cast<const dummy>(terms[index].E)) return false;

  assert(not is_computed(index));
  return true;
}

bool Formula::is_parameter(int index) const
{
  return (not is_computed(index) and not is_constant(index));
}

int Formula::n_parameters() const
{
  return parameter_indices.size();
}

int Formula::parameter_index(int i) const
{
  return parameter_indices[i];
}

string Formula::parameter_name(int i) const
{
  return terms[parameter_index(i)].E->print();
}

void Formula::rename_parameter(int i, const string& s)
{
  assert(is_parameter(i));
  expression_ref old_p = terms[i].E;
  expression_ref new_p = parameter(s);

  for(int i=0;i<terms.size();i++)
    terms[i].E = substitute(terms[i].E,old_p,new_p);
}

bool Formula::is_computed(int index) const
{
  if (has_inputs(index))
  {
    assert(operation(index));
    return true;
  }
  else
  {
    assert(not operation(index));
    return false;
  }
}

bool Formula::directly_affects(int index1, int index2) const
{
  return includes(affected_indices(index1), index2);
}

bool Formula::directly_affects_in_slot(int index1, int index2, int slot) const
{
  return terms[index2].input_indices[slot] == index1;
}

void Formula::set_directly_affects_in_slot(int index1, int index2, int slot)
{
  if (not directly_affects(index1,index2))
    terms[index1].affected_indices.push_back(index2);

  pair<int,int> p(index2,slot);
  if (not includes(terms[index1].affected_slots, p))
    terms[index1].affected_slots.push_back(p);
}

/// Check to see if this computation already exists
term_ref Formula::find_computation(const Operation& o, const vector<int>& indices) const
{
  // avoid adding duplicate calculations
  for(int index=0; index<size(); index++)
    if ((indices == terms[index].input_indices) and (typeid(o) == typeid(*operation(index))))
	return term_ref(index,*this);

  return term_ref();
}

term_ref Formula::add_expression(const expression_ref& R)
{
  return add_sub_expression(R,true);
}

term_ref Formula::add_sub_expression(const expression_ref& R, bool top)
{
#ifndef NDEBUG
  {
    expression_ref R2 (R->clone());
    if (R->compare(*R2) != true)
      std::cerr<<"Warning: expression "<<R->print()<<" does not compare equal to itself! ("<<R->compare(*R2)<<")\n";
  }
#endif

  // If the expression already exists, then return a reference to the existing term
  int index = find_expression(R);
  if (index != -1) return term_ref(index,*this);

  // Create a term for this expression
  Term t(R);
  t.top_level = top;

  if (shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(t.E))
  {
    // FIXME - how about the head?
    for(int i=1;i<E->size();i++)
      t.input_indices.push_back( add_sub_expression(E->sub[i] ) );
  }

  int new_index = terms.size();

  for(int slot=0;slot<t.input_indices.size();slot++)
  {
    int input_index = t.input_indices[slot];
    set_directly_affects_in_slot(input_index,new_index,slot);
  }

  // Warn about duplicate names
  int same_name = find_term_with_name(R->print());
  if (same_name != -1)
    std::cerr<<"Warning ["<<new_index<<"]: term with name '"<<R->print()<<"' already exists at index "<<same_name<<".!\n";

  // Update ref for parameters
  if (dynamic_pointer_cast<const parameter>(t.E))
    parameter_indices.push_back(new_index);

  // Actually add the term
  terms.push_back(t);

  // Index the top-level expressions.
  if (top) top_level_expressions.push_back(new_index);

  return term_ref(new_index,*this);
}

term_ref Formula::find_term_with_name(const string& name) const
{
  for(int i=0;i<size();i++)
    if (terms[i].E->print() == name)
      return term_ref(i,*this);

  return term_ref();
}

term_ref Formula::find_expression(const expression_ref& E) const
{
  for(int i=0;i<terms.size();i++)
  {
    tribool same = terms[i].E->compare(*E);
    if (same == indeterminate)
      std::cerr<<"Warning: '"<<E->print()<<"' and '"<<terms[i].E->print()<<"' are unsure if they are equal.";
    if (same)
      return term_ref(i, *this);
  }


  return term_ref();
}

bool Formula::find_match2(const expression_ref& query, int index, std::vector<int>& results) const
{
  // if this is a match expression, then succeed, and store E as the result of the match
  shared_ptr<const match> M = dynamic_pointer_cast<const match>(query);
  if (M) 
  {
    if (M->index >= 0)

    {
      if (results.size() < M->index+1) results.resize(M->index+1);

      if (results[M->index]) throw myexception()<<"Match expression  contains match index "<<M->index<<"' more than once!";

      results[M->index] = index;
    }

    return true;
  }

  const expression_ref& E = terms[index].E;

  shared_ptr<const expression> query_exp = dynamic_pointer_cast<const expression>(query);

  // If this is a leaf constant, then check if E is equal to it.
  if (not query_exp)
    return (query->compare(*E) == true);

  // If pattern is an expression but E is not, then there is no match.
  shared_ptr<const expression> E_exp = dynamic_pointer_cast<const expression>(E);
  if (not E_exp) return false;

  // Expressions must have the same number of arguments
  if (query_exp->size() != E_exp->size()) return false;

  // The heads have to compare equal.  There is no matching there. (Will there be, later?)
  if (query_exp->sub[0]->compare(*E_exp->sub[0]) != true)
    return false;

  for(int i=1;i<query_exp->size();i++)
    if (not find_match2(query_exp->sub[i], terms[index].input_indices[i-1], results))
      return false;

  return true;
  
}

bool Formula::find_match_expression(const expression_ref& E, int index, std::vector< expression_ref >& results) const
{
  results.clear();
  bool success = find_match(E, (*this)[index], results);

  if (not success)
    results.clear();

  return success;
}

term_ref Formula::find_match_expression(const expression_ref& E, std::vector< expression_ref >& results) const
{
  for(int i=0;i<terms.size();i++)
    if (find_match_expression(E, i, results))
      return term_ref(i, *this);

  return term_ref();
}

term_ref Formula::find_match_expression2(const expression_ref& query, std::vector<int>& results) const
{
  for(int i=0;i<terms.size();i++)
  {
    results.clear();
    if (find_match2(query, i, results))
      return term_ref(i, *this);
  }

  results.clear();
  return term_ref();
}

boost::shared_ptr<Formula> combine(const boost::shared_ptr<const Formula>& F1, const boost::shared_ptr<const Formula>& F2)
{
  shared_ptr<Formula> F (F1->clone());
  for(int i=0;i<F2->n_exp();i++)
    F->add_expression(F2->exp(i));
  return F;
}

boost::shared_ptr<Formula> prefix_formula(const std::string& prefix,const boost::shared_ptr<const Formula>& F)
{
  shared_ptr<Formula> F2 (F->clone());
  for(int i=0;i<F2->n_parameters();i++)
    F2->rename_parameter(F2->parameter_index(i),prefix + "::" + F2->parameter_name(i));
  return F2;
}

std::ostream& operator<<(std::ostream& o, const Formula& F)
{
  for(int index=0;index<F.n_exp();index++)
    o<<index<<" "<<"[sub="<<F.exp_sub_index(index)<<"]  "<<F.exp(index)->print()<<"\n";
  return o;
}

