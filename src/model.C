/*
   Copyright (C) 2004-2006,2009 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

#include <set>
#include <map>

#include "util.H"
#include "myexception.H"
#include "model.H"
#include "computation/expression.H"
#include "computation/operations.H"
#include "computation/formula_expression.H"
#include "distribution-operations.H"

using std::vector;
using std::string;

using boost::dynamic_pointer_cast;

/* IDEA: Make the function we use to create FormulaModel( ) -- that is
   Model::Model(const vector<expression_ref>&) -- into a generic function
   to add a formula_expression to a model.
   
   For a regular model (not a SuperModel) this involves just making sure that
   The number of parameters matches the length of fixed, bounds, and changed.

   
 */

string parameter_name(const string& prefix, int i,int n) 
{
  if (i>=n)
    throw myexception()<<"substitution model: referred to parameter "<<i<<" but there are only "<<n<<" parameters.";
  return prefix + convertToString(i);
}

Parameter::Parameter(const string& n)
  :name(n), fixed(false)
{
}

Parameter::Parameter(const string& n, object_ptr<const Object> v)
  :name(n), value(v), fixed(false)
{
}

Parameter::Parameter(const string& n, object_ptr<const Object> v, const Bounds<double>& b, bool f)
  :name(n), value(v), bounds(b), fixed(f)
{
}

Parameter::Parameter(const string& n, const Object& v)
  :name(n), value(v), fixed(false)
{
}

Parameter::Parameter(const string& n, const Object& v, bool f)
  :name(n), value(v), fixed(f)
{
}

Parameter::Parameter(const string& n, const Object& v, const Bounds<double>& b, bool f)
  :name(n), value(v), bounds(b), fixed(f)
{
}

vector<expression_ref> model_parameter_expressions(const Model& M)
{
  vector< expression_ref > sub;
  for(int i=0;i<M.n_parameters();i++) 
    sub.push_back( parameter(M.parameter_name(i)) );
  return sub;
}

void Model::validate() const
{
  valid = true;
  for(int i=0;i<n_parameters();i++)
    changed[i] = false;
}

void Model::invalidate() const
{
  valid = false;
}

void Model::modify_parameter(int i) const
{
  changed[i] = true;

  invalidate();
}

void Model::modify_parameters(const vector<int>& indices)
{
  for(int i=0;i<indices.size();i++)
    modify_parameter(indices[i]);
}

void Model::modify_all_parameters() const
{
  for(int i=0;i<n_parameters();i++)
    modify_parameter(i);
}

vector<int> Model::modified_parameters() const
{
  vector<int> changed_parameters;

  for(int i=0;i<n_parameters();i++)
    if (changed[i])
      changed_parameters.push_back(i);

  return changed_parameters;
}

void Model::recalc_all() 
{
  modify_all_parameters();
  update();
}

int Model::add_parameter(const Parameter& P)
{
  for(int i=0;i<n_parameters();i++)
    if (parameter_name(i) == P.name)
      throw myexception()<<"A parameter with name '"<<P.name<<"' already exists - cannot add another one.";

  int index = n_parameters();

  C.add_parameter(P.name);
  changed.push_back(true);
  bounds.push_back(P.bounds);
  fixed.push_back(P.fixed);
  prior_note_index.push_back(-1);

  if (P.value)
    C.set_parameter_value(index, *P.value);
  return index;
}

std::vector< object_ptr<const Object> > Model::get_parameter_values() const
{
  std::vector< object_ptr<const Object> > values(n_parameters());

  for(int i=0;i<values.size();i++)
    values[i] = get_parameter_value(i);
    
  return values;  
}

std::vector< object_ptr<const Object> > Model::get_parameter_values(const std::vector<int>& indices) const
{
  std::vector< object_ptr<const Object> > values(indices.size());
    
  for(int i=0;i<values.size();i++)
    values[i] = get_parameter_value(indices[i]);
  
  return values;  
}

std::string Model::parameter_name(int i) const
{
  return C.parameter_name(i);
}

void Model::rename_parameter(int i, const std::string& s)
{
  C.rename_parameter(i,s);
}

int Model::find_parameter(const string& s) const
{
  return C.find_parameter(s);
}

int Model::add_note(const expression_ref& E)
{
  int index = C.add_note(E);

  // Quit if we've seen this already
  if (index != C.n_notes()-1) return index;

  // 1. Check to see if this expression adds a bound.
  expression_ref query = (var_bounds, match(0), match(1));

  vector<expression_ref> results;
  if (find_match(query, C.get_note(index), results))
  {
    object_ptr<const parameter> var = is_a<parameter>(results[0]);
    object_ptr<const Bounds<double> > b = C.evaluate_expression_as<Bounds<double> >(results[1]->head);
    int param_index = find_parameter(var->parameter_name);
    if (param_index == -1)
      throw myexception()<<"Cannot add bound '"<<E<<"' on missing variable '"<<var->parameter_name<<"'";
    set_bounds(param_index , *b);
  }

  // 2. Check to see if this expression adds a prior
  results.clear();
  query = (distributed, _1, _2);
  if (find_match(query, C.get_note(index), results))
  {
    // Extract the density operation
    expression_ref x = results[0];
    expression_ref D = results[1];

    expression_ref density = dummy(0);
    expression_ref args = dummy(1);
    expression_ref _ = dummy(-1);

    // Create an expression for calculating the density of these random variables given their inputs
    expression_ref Pr_new = case_expression(D, Tuple((prob_density,_,density,_),args), (density, x, args));
    
    // Record that this variable is random, and has this prior.
    // THIS would be the right place to determine what other random variables and parameters are being depended on.
    // THIS would be the right place to check that dependencies are not cyclic.
    for(const auto& name : find_named_parameters(x) )
    {
      int p_index = find_parameter(name);

      if (prior_note_index[p_index] != -1)
	throw myexception()<<"Variable '"<<name<<"': new prior '"<<show_probability_expression(C.get_note(index))
			   <<"' on top of original prior '"<<show_probability_expression(C.get_note(prior_note_index[p_index]))<<"'?";
      else
	prior_note_index[p_index] = index;
    }

    // Extend the probability expression to include this term also.
    // (FIXME: a balanced tree could save computation time)
    if (prior_index == -1)
      prior_index = C.add_compute_expression( Pr_new );
    else
    {
      typed_expression_ref<Log_Double> Pr ( C.get_expression(prior_index));
      C.set_compute_expression(prior_index, Pr_new * Pr);
    }
  }

  return index;
}

bool Model::is_random_variable(int i) const
{
  return prior_note_index[i] != -1;
}

bool Model::is_fixed(int i) const
{
  return fixed[i];
}

void Model::set_fixed(int i,bool f)
{
  fixed[i] = f;
}

const Bounds<double>& Model::get_bounds(int i) const 
{
  return bounds[i];
}

void Model::set_bounds(int i,const Bounds<double>& b) 
{
  bounds[i] = b;
}

object_ptr<const Object> Model::get_parameter_value(int i) const
{
  return C.get_parameter_value(i);
}

object_ptr<const Object> Model::get_parameter_value(const std::string& p_name) const 
{
  return C.get_parameter_value(p_name);
}

void Model::write_value(int i,const object_ptr<const Object>& value)
{
  C.set_parameter_value(i,value);
  modify_parameter(i);
}

void Model::set_parameter_value(int i,Double value) 
{
  set_parameter_value(i, object_ptr<const Object>( value.clone()) );
}

void Model::set_parameter_value(int i,const object_ptr<const Object>& value) 
{
  set_parameter_values(vector<int>(1,i), vector< object_ptr<const Object> >(1, value) );
}

void Model::set_parameter_value(const string& p_name,const object_ptr<const Object>& value) 
{
  int i = find_parameter(p_name);
  if (i == -1)
    throw myexception()<<"Cannot find parameter called '"<<p_name<<"'";
    
  set_parameter_values(vector<int>(1,i), vector< object_ptr<const Object> >(1, value) );
}

void Model::set_parameter_values(const vector<int>& indices,const vector<Double>& p)
{
  vector< object_ptr<const Object> > p2(p.size());
  for(int i=0;i<p.size();i++)
    p2[i] = object_ptr<const Object>( p[i].clone() );

  set_parameter_values(indices,p2);
}

void Model::set_parameter_values(const vector<int>& indices,const vector<object_ptr<const Object> >& p)
{
  assert(indices.size() == p.size());

  for(int i=0;i<indices.size();i++)
    write_value(indices[i], p[i]);

  update();
}

void Model::set_parameter_values(const vector<Double>& p) 
{
  assert(p.size() == n_parameters());
  set_parameter_values(iota<int>(n_parameters()), p);
}

void Model::set_parameter_values(const vector<object_ptr<const Object> >& p) 
{
  assert(p.size() == n_parameters());
  set_parameter_values(iota<int>(n_parameters()), p);
}

unsigned Model::n_parameters() const 
{
  return C.n_parameters();
}

efloat_t Model::prior() const
{
  if (prior_index == -1) return 1.0;

  object_ptr<const Log_Double> R = C.evaluate_as<Log_Double>(prior_index);
  return *R;
}

vector<string> Model::show_priors() const
{
  return show_probability_expressions(C);
}

Model::Model()
  :valid(false),prior_index(-1)
{ }

int add_probability_expression(context& C);

Model::Model(const vector<expression_ref>& notes)
  :valid(false)
{
  // 1. Create the parameters
  std::set<string> names = find_named_parameters(notes);
  
  for(const auto& name: names)
    add_parameter(name);

  C.alphabetize_parameters();

  // 2. Add the notes refering to the parameters.
  for(int i=0;i<notes.size();i++)
    add_note(notes[i]);

  // 3. Then set all default values.
  for(int i=0;i<n_parameters();i++)
    set_parameter_value(i, C.default_parameter_value(i));

  // 4. Set bounds.
  for(int i=0;i<n_parameters();i++)
  {
    expression_ref var = parameter(parameter_name(i));
    vector<expression_ref> results;
    expression_ref query = (var_bounds, var, match(0));
    int found = C.find_match_notes(query, results, 0);
    if (found != -1)
    {
      assert(results.size());
      object_ptr<const Bounds<double> > b = C.evaluate_expression_as<Bounds<double> >(results[0]);
      set_bounds(i,*b);
    }
  }

  // 5. Create the prior
  prior_index = add_probability_expression(C);

#ifndef NDEBUG
  std::cout<<C<<"\n";
  std::cout<<"prior_index = "<<prior_index<<"\n";
  std::cout<<"prior = "<<log(prior())<<"\n";
  std::cout<<C<<std::endl;
#endif
}

object_ptr<const Object> Model::result() const
{
  std::abort();
}

void Model::update()
{
  if (not is_valid())
  {
    recalc(modified_parameters());
    validate();
  }
}

int SuperModel::add_parameter(const Parameter& P)
{
  int index = Model::add_parameter(P);
  model_slots_for_index.push_back(vector<model_slot>());
  return index;
}

int SuperModel::n_submodels() const 
{
  return slot_expressions_for_submodel.size();
}

bool SuperModel::parameter_is_used_by_model(int index, int m) const
{
  for(int i=0;i<model_slots_for_index[index].size();i++)
    if (model_slots_for_index[index][i].model_index == m)
      return true;

  return false;
}

bool SuperModel::is_super_parameter(int index) const
{
  return parameter_is_used_by_model(index,-1);
}

int SuperModel::n_super_parameters() const 
{
  int n=0;
  for(int i=0;i<n_parameters();i++)
    if (is_super_parameter(i))
      n++;

  return n;
}

// apparent the super-parameters are the first ones
int SuperModel::add_super_parameter(const Parameter& P)
{
  int index = add_parameter(P);
  model_slots_for_index[index].push_back(model_slot());

  return index;
}

int SuperModel::register_last_submodel(const vector<arg_expression>& args)
{
  int m_index = slot_expressions_for_submodel.size()-1;

  // The number of slots should match the number of slots in the model expression
  assert(SubModels(m_index).n_parameters() == args.size());

  // An argument should not refer to a parent slot that does not exist.
  for(int i=0;i<args.size();i++)
    if (args[i].is_term_ref())
      assert(args[i].parent_index >=0 and args[i].parent_index < n_parameters());
    
  // Record for each arg that its used in this submodel
  for(int slot=0;slot<args.size();slot++)
    if (args[slot].is_term_ref())
    {
      int index = args[slot].parent_index;
      model_slots_for_index[index].push_back(model_slot(m_index,slot));
    }

  // Set the submodel parameters
  write();

  return m_index;
}

int SuperModel::register_submodel(const vector<arg_expression>& args)
{
  // bump the number of submodels
  slot_expressions_for_submodel.push_back( vector<arg_expression>() );

  return register_last_submodel(args);
}

int SuperModel::register_submodel(const string& prefix)
{
  // bump the number of submodels
  slot_expressions_for_submodel.push_back( vector<arg_expression>() );

  int m_index = slot_expressions_for_submodel.size()-1;

  const Model& M = SubModels(m_index);

  // Create the top-level parameters, and the list of references to them
  vector<arg_expression> args;

  for(int i=0;i<M.n_parameters();i++)
  {
    string name = prefix + "::" + M.parameter_name(i);
    object_ptr<const Object> value = M.get_parameter_value(i);
    Bounds<double> bounds = M.get_bounds(i);
    bool fixed = M.is_fixed(i);

    int index = add_parameter(Parameter(name,value,bounds,fixed));
    args.push_back(index);
  }

  return register_last_submodel(args);
}

// can I write the supermodel so that it actually SHARES the values of the sub-models?
void SuperModel::write_value(int index, const object_ptr<const Object>& p)
{
  assert(index < n_parameters());

  Model::write_value(index, p);

  const vector<model_slot>& model_slots = model_slots_for_index[index];

  // For each model that uses this top-level index...
  for(int i=0;i<model_slots.size();i++)
  {
    int m = model_slots[i].model_index;
    int s = model_slots[i].slot;

    //... write it down into a sub-model, if the usage is not from the top-level model.
    if (m != -1) 
      SubModels(m).write_value(s,p);
  }
}

void SuperModel::write() 
{
  for(int i=0;i<n_parameters();i++)
    write_value(i, C.get_parameter_value(i) );
}

efloat_t SuperModel::prior() const {
  efloat_t  P = super_prior();
  for(int i=0;i<n_submodels();i++)
    P *= SubModels(i).prior();
  return P;
}

vector<string> SuperModel::show_priors() const 
{
  vector<string> pr_exp = Model::show_priors();
  for(int i=0;i<n_submodels();i++)
  {
    vector<string> pe = SubModels(i).show_priors();
    pr_exp.insert(pr_exp.end(), pe.begin(), pe.end());
  }
  return pr_exp;
}

void SuperModel::check() const
{
#ifndef NDEBUG
  for(int m=0;m<n_submodels(); m++)
  {
    // Read the current argument lists for each sub-model
    const vector<arg_expression>& arg_expressions = slot_expressions_for_submodel[m];

    for(int i=0;i<arg_expressions.size();i++)
    {
      if (arg_expressions[i].is_term_ref())
      {
	int index = arg_expressions[i].parent_index;
	assert(SubModels(m).get_parameter_value(i)->equals( *get_parameter_value(index)) );
      }
      else
	assert(SubModels(m).get_parameter_value(i)->equals( *arg_expressions[i].constant_value ) );
    }
  }
#endif
}

void SuperModel::update()
{
  for(int i=0;i<n_submodels();i++)
    SubModels(i).update();

  Model::update();
}

SuperModel::SuperModel()
{ }

void show_parameters(std::ostream& o,const Model& M) {
  for(int i=0;i<M.n_parameters();i++) {
    o<<"    ";
    if (M.is_fixed(i)) 
      o<<"*";
    o<<M.parameter_name(i)<<" = ";
    string output="[NULL]";
    if (M.get_parameter_value(i))
    {
      output=M.get_parameter_value(i)->print();
      if (output.find(10) != string::npos or output.find(13) != string::npos)
	output = "[multiline]";
    }
    o<<output;
  }
  o<<"\n";
}

/// \brief Check if the model M has a parameter called name
///
/// \param M      The model
/// \param name   A parameter name
///
bool has_parameter(const Model& M, const string& name)
{
  for(int i=0;i<M.n_parameters();i++)
    if (M.parameter_name(i) == name)
      return true;
  return false;
}

/// \brief Check if the string s1 matches a pattern s2
///
/// \param s1   The string
/// \param s2   The pattern
///
bool match(const string& s1, const string& s2)
{
  if (s2.size() and s2[s2.size()-1] == '*') {
    int L = s2.size() - 1;
    if (L > s1.size()) return false;
    return (s1.substr(0,L) == s2.substr(0,L));
  }
  else
    return s1 == s2;
}

bool operator<(const vector<string>& p1, const vector<string>& p2)
{
  // less than
  if (p1.size() > p2.size()) return true;
  // greater than
  if (p1.size() < p2.size()) return false;
  
  for(int i=0;i<p1.size();i++)
  {
    int cmp = p1[i].compare(p2[i]);
    // less than
    if (cmp < 0) return true;
    // greater than
    if (cmp > 0) return false;
  }
  
  // equal
  return false;
}

typedef std::set< vector<string> > path_set_t;

/// Does this path have the given prefix?
bool path_has_prefix(const vector<string>& path, const vector<string>& path_prefix)
{
  if (path_prefix.size() > path.size()) return false;

  for(int i=0;i<path_prefix.size();i++)
    if (path[i] != path_prefix[i])
      return false;

  return true;
}

/// Are the paths all distinguishable from each other?
bool overlap(const path_set_t& set1, const path_set_t& set2)
{
  if (set1.empty() or set2.empty()) return false;

  path_set_t::const_iterator it1 = set1.begin(), it1End = set1.end();
  path_set_t::const_iterator it2 = set2.begin(), it2End = set2.end();

  if(*it1 > *set2.rbegin() || *it2 > *set1.rbegin()) return false;

  while(it1 != it1End && it2 != it2End)
  {
    if(*it1 < *it2)
      it1++; 
    else if (*it1 > *it2)
      it2++; 
    else
      return true;
  }

  return false;
}

/// Remove the nodes in paths that are direct children of the path_prefix
void remove_prefix(vector< vector<string> >& paths, const  vector<string>& path_prefix)
{
  for(int i=0;i<paths.size();i++)
  {
    if (not path_has_prefix(paths[i], path_prefix)) continue;

    paths[i].erase(paths[i].begin()+path_prefix.size()-1);
  }
}

/// Remove (internal) child paths if grandchild paths are not shared with any other child.
void check_remove_grandchildren(vector< vector<string> >& paths, const vector<string>& path_prefix)
{
  // construct the child paths and their locations
  typedef std::map<string, path_set_t> path_map_t;
  path_map_t grandchild_paths;

  int L = path_prefix.size();

  // find the grandchild paths for each child
  for(int i=0;i<paths.size();i++)
    if (path_has_prefix(paths[i], path_prefix))
    {
      // We don't consider leaf child paths
      if (paths[i].size() == path_prefix.size() + 1)
	continue;

      string child_name = paths[i][L];

      vector<string> grandchild_path = paths[i];
      grandchild_path.erase(grandchild_path.begin(),grandchild_path.begin()+L+1);

      grandchild_paths[child_name].insert(grandchild_path);
      assert(grandchild_path.size());
    }

  // check of the grandchild paths of any child overlap with the grandchild paths of any other child
  for(path_map_t::const_iterator i = grandchild_paths.begin();i != grandchild_paths.end();i++)
  {
    bool unique = true;
    for(path_map_t::const_iterator j = grandchild_paths.begin();j != grandchild_paths.end();j++)
    {
      if (i->first == j->first) continue;

      if (overlap(i->second,j->second)) unique = false;
    }
    if (unique) {
      vector<string> child_prefix = path_prefix;
      child_prefix.push_back(i->first);
      remove_prefix(paths, child_prefix);
    }
  }
}

// We can think of this collection of name lists as a tree.
// - Each name list is a path from the root to a tip.
// - Each node (except the root) has a string name associated with it.
// We consider all child nodes of internal node
//  If the set of grandchild lists under child node C does not overlap with the
//   grandchild lists under any other child node, then we can remove node C.
// We should always prefer to remove deeper nodes first.
//  Thus, leaf nodes should never be removed.
// We therefore consider all internal nodes of the tree, starting
//  with the ones furthest from the root, and remove their children
//  if it is allowable.

vector<string> short_parameter_names(vector<string> names)
{
  // for any sequence n[0] n[1] ... n[i-1] n[i] n[i+1] ..... N[L]
  // If we select all the sequences where where  n[0].... n[i-1] are the same
  //  Then we can get rid of n[i] if the sequences n[i+1]...N[L] are all different

  // construct the name paths
  vector< vector<string> > paths;
  for(int i=0;i<names.size();i++)
    paths.push_back(split(names[i],"::"));

  for(int i=0;i<paths.size();i++)
  {
    vector<string> prefix = paths[i];
    while(prefix.size())
    {
      prefix.pop_back();
      check_remove_grandchildren(paths, prefix);
    }
  }
  
  for(int i=0;i<names.size();i++)
    names[i] = join(paths[i],"::");

  return names;
}

vector<string> parameter_names(const Model& M)
{
  vector<string> names;
  for(int i=0;i<M.n_parameters();i++)
    names.push_back( M.parameter_name(i) );

  return names;
}

vector<string> short_parameter_names(const Model& M)
{
  return short_parameter_names( parameter_names( M ) );
}

// The key may have '*', '?', 'text*', and 'text'.  Here
// - '?' matches exactly one element.
// - '*' matches 0 or more elements
// - text* matches 1 element beginning with 'text'
// - test matches 1 element that is exactly 'text'.
bool path_match(const vector<string>& key, int i, const vector<string>& pattern, int j)
{
  assert(i <= key.size());

  assert(j <= pattern.size());

  // 1. If the key is empty
  if (i==key.size())
  {
    // nothing ~ nothing
    if(j==pattern.size()) return true;

    // nothing !~ something
    if (j<pattern.size()) return false;
  }

  assert(i<key.size());

  // 2. If the key is '*'
  if (key[i] == "*") 
  {
    // (* x y z ~ [nothing]) only (if x y z ~ [nothing])
    if (j == pattern.size())
      return path_match(key,i+1,pattern,j);

    // (* matches 0 components of j) or (* matches 1 or more components of j)
    return path_match(key,i+1,pattern,j) or path_match(key,i,pattern,j+1);
  }

  // For anything else, FAIL if we are matching against [nothing], and key != '*' only.
  if (j == pattern.size()) return false;

  assert(j<pattern.size());

  // 3. If the key is '?'
  if (key[i] == "?")
    return path_match(key,i+1,pattern,j+1);

  // 4. If the key is 'text*' or 'text'
  if (match(pattern[j],key[i]))
    return path_match(key,i+1,pattern,j+1);
  else
    return false;
}

bool path_match(const vector<string>& key, const vector<string>& pattern)
{
  return path_match(key,0,pattern,0);
}

/// \brief Find the index of model parameters that match the pattern name
///
/// \param M      The model
/// \param name   The pattern
///
vector<int> parameters_with_extension(const Model& M, string name)
{
  vector<int> indices;

  const vector<string> key = split(name,"::");

  if (not key.size()) return indices;

  vector<string> skeleton;

  for(int i=0;i<M.n_parameters();i++)
  {
    vector<string> pattern = split(M.parameter_name(i),"::");

    if (not path_match(key, pattern)) continue;

    // check that all matching parameters have the same basename
    vector<string> this_skeleton = pattern;
    this_skeleton.pop_back();

    if (not indices.size())
      skeleton = this_skeleton;
    else if (skeleton != this_skeleton)
      throw myexception()<<"Key '"<<name<<"' matches both "<<join(skeleton,"::")<<" and "<<join(this_skeleton,"::")<<".";

    indices.push_back(i);
  }

  return indices;
}

string show_probability_expression(const expression_ref& E)
{
  expression_ref query = (distributed, _2, Tuple((prob_density, _1 , _, _), _3));

  // If its a probability expression, then...
  vector<expression_ref> results; 
  if (not find_match(query, E, results))
    throw myexception()<<"Expression '"<<E<<"' is not a probability expression.";
  
  // Extract the density operation
  object_ptr<const String> name = is_a<String>(results[0]);
  
  string prob_exp;
  {
    expression_ref rand_var = results[1];
    if (is_exactly(rand_var->head,":"))
    {
      vector<expression_ref> rand_vars = get_ref_vector_from_list(rand_var);
      prob_exp += get_tuple(rand_vars)->print();
    }
    else
      prob_exp += rand_var->print();
  }
  
  prob_exp += " ~ " + string(*name);
  if (results[2]->size())
    prob_exp += results[2]->print();
  else
    prob_exp += "(" + results[2]->print() + ")";
  
  return prob_exp;
}

vector<string> show_probability_expressions(const context& C)
{
  vector<string> expressions;

  // Check each expression in the Formula
  for(int i=0;i<C.n_notes();i++)
    if (is_exactly(C.get_note(i),"~"))
      expressions.push_back( show_probability_expression(C.get_note(i)) );

  return expressions;
}
