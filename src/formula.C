#include "myexception.H"

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
term_ref::term_ref(int i,const Formula& f):index(i),F(f.clone()) {}

string Formula::name_for_index(int index) const
{
  return terms[index].name;
}

bool Formula::has_inputs(int index) const 
{
  bool is_internal = (n_input_indices(index) > 0);
  if (not is_internal)
    assert(not terms[index].op);
  else
    assert(terms[index].op);

  return is_internal;
}

bool Formula::is_constant(int index) const
{
  if (terms[index].constant)
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
    assert(terms[index].op);
    return true;
  }
  else
  {
    assert(not terms[index].op);
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
    if ((indices == terms[index].input_indices) and (typeid(o) == typeid(*terms[index].op)))
	return term_ref(index,*this);

  return term_ref();
}

term_ref Formula::add_term(const Term& t)
{
  int new_index = terms.size();

  term_ref ref;

  // check new computed nodes, mark their inputs.
  if (t.op) 
  {
    ref = find_computation(*t.op, t.input_indices);
    if (ref.index != -1)
      return ref;

    for(int slot=0;slot<t.input_indices.size();slot++)
    {
      int input_index = t.input_indices[slot];
      set_directly_affects_in_slot(input_index,new_index,slot);
    }
  }
  else if (t.input_indices.size())
    throw myexception()<<"Can't have input indices with no operation!";
  else
  {
    if (not t.constant)
    {
      for(int i=0;i<size();i++)
	if (terms[i].name == t.name)
	{
	  if (is_state(i))
	    return term_ref(i,*this);
	  else
	    throw myexception()<<"Can't insert a new state variable named '"<<t.name<<"': a term with that name already exists at index "<<i<<".";
	}

      state_indices.push_back(new_index);
    }
    else 
    {
      if (not t.default_value)
	throw myexception()<<"Constant node must provide a value!";

      for(int index=0;index<size();index++)
      {
	if (is_constant(index) and t.default_value->equals(*terms[index].default_value))
	  return term_ref(index,*this);
      }
    }

  }

  terms.push_back(t);
  return term_ref(new_index,*this);
}

term_ref Formula::add_computed_node(const Operation& o, const vector<int>& indices)
{
  // compute the name of node we might add
  vector<string> input_names;
  for(int slot=0;slot<indices.size();slot++)
    input_names.push_back(terms[indices[slot]].name);

  Term t;
  t.op = shared_ptr<Operation>(o.clone());
  t.name = o.expression(input_names);
  t.input_indices = indices;

  term_ref new_index = add_term(t);

  return new_index;
}

term_ref Formula::add_state_node(const string& name)
{
  Term t;
  t.name = name;
  return add_term(t);
}

term_ref Formula::add_state_node(const string& name, const Object& value)
{
  Term t;
  t.name = name;
  t.default_value = shared_ptr<const Object>(value.clone());
  return add_term(t);
}

term_ref Formula::add_state_node(const string& name, shared_ptr<const Object> value)
{
  Term t(value);
  t.name = name;
  t.default_value = shared_ptr<const Object>(value->clone());
  return add_term(t);
}

term_ref Formula::add_constant_node(const Object& value)
{
  return add_constant_node(value.print(), shared_ptr<const Object>(value.clone()));
}

term_ref Formula::add_constant_node(const string& name, const Object& value)
{
  return add_constant_node(name, shared_ptr<const Object>(value.clone()));
}

term_ref Formula::add_constant_node(shared_ptr<const Object> value)
{
  return add_constant_node(value->print(), shared_ptr<const Object>(value->clone()));
}

term_ref Formula::add_constant_node(const string& name, shared_ptr<const Object> value)
{
  Term t(value);
  t.name = name;
  t.constant = true;
  return add_term(t);
}

