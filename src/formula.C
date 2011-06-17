#include "myexception.H"

#include "expression.H"
#include "formula.H"
#include "operation.H"

#include "util.H"

using boost::shared_ptr;
using std::vector;
using std::string;
using std::pair;

bool term_ref::is_state() const {return F->is_state(index);}
bool term_ref::is_constant() const {return F->is_constant(index);}
bool term_ref::is_computed() const {return F->is_computed(index);}
string term_ref::print() const {return F->name_for_index(index);}
term_ref::term_ref():index(-1) { }
term_ref::term_ref(int i,const Formula& f):index(i),F(f.clone()) {}
term_ref::term_ref(int i,boost::shared_ptr<const Formula> f):index(i),F(f) {}


string Formula::name_for_index(int index) const
{
  return terms[index].name;
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
  return dynamic_pointer_cast<const Operation>(terms[index].E->head);
}

boost::shared_ptr<const Function> Formula::function(int index) const
{
  return dynamic_pointer_cast<const Function>(terms[index].E->head);
}

bool Formula::is_constant(int index) const
{
  if (dynamic_pointer_cast<const constant_expression>(terms[index].E))
  {
    assert(not is_computed(index));
    return true;
  }
  else
    return false;
}

bool Formula::is_state(int index) const
{
  return (not is_computed(index) and not is_constant(index));
}

int Formula::n_state_nodes() const
{
  return state_indices.size();
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

term_ref Formula::add_term(const Term& t)
{
  int new_index = terms.size();

  {
    shared_ptr<const expression> E (t.E->clone());
    if (t.E->compare(*E) == indeterminate)
      std::cerr<<"Warning: expression "<<t.E->print()<<" does not compare equal to itself! ("<<t.E->compare(*E)<<")\n";
  }

  // Check if the expression already exists
  int index = find_expression(t.E);
  if (index != -1) {
    //    std::cerr<<"   term "<<t.E->print()<<" already present\n";
    return term_ref(index,*this);
  }

  // Warn about duplicate names
  int same_name = find_term_with_name(t.name);
  if (same_name != -1)
    std::cerr<<"Warning ["<<new_index<<"]: term with name '"<<t.name<<"' already exists at index "<<same_name<<".!\n";

  // Update ref for parameters
  if (dynamic_pointer_cast<const parameter>(t.E->head))
    state_indices.push_back(new_index);

  // Check new computed nodes, mark slots as being affected
  if (t.E->n_args() or t.input_indices.size())
  {
    assert(t.input_indices.size() == t.E->n_args());

    for(int slot=0;slot<t.input_indices.size();slot++)
    {
      int input_index = t.input_indices[slot];
      set_directly_affects_in_slot(input_index,new_index,slot);
    }
  }

  // Actually add the term
  terms.push_back(t);
  //  std::cerr<<"adding term "<<t.E->print()<<"\n";
  return term_ref(new_index,*this);
}

term_ref Formula::add_expression(const expression_ref& e)
{
  shared_ptr<const lambda_expression> lambda = boost::dynamic_pointer_cast<const lambda_expression>(e);
  // could lambda expressions just evaluate to themselves?  With non-dependent sub-expressions evaluated?
  if (lambda)
    throw myexception()<<"Lambda expressions cannot currently be calculated";

  shared_ptr<const constant_expression> constant = boost::dynamic_pointer_cast<const constant_expression>(e);
  if (constant)
    return add_term(Term(e));
  
  shared_ptr<const parameter> var = boost::dynamic_pointer_cast<const parameter>(e->head);
  if (var)
    // If we add Term(e,value), then value becomes the default value.
    return add_term(Term(e));

  shared_ptr<const Operator> op = boost::dynamic_pointer_cast<const Operator>(e->head);
  if (op)
  {
    vector<int> arg_indices;
    for(int i=0;i<e->args.size();i++)
      arg_indices.push_back( add_expression(e->args[i] ) );

    Term t(e);
    t.input_indices = arg_indices;
    return add_term(t);
  }

  std::abort();
}


term_ref Formula::find_term_with_name(const string& name) const
{
  for(int i=0;i<size();i++)
    if (terms[i].name == name)
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
