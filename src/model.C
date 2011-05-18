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

#include "model.H"
#include "myexception.H"
#include "util.H"
#include <set>
#include <map>

using std::vector;
using std::string;

string parameter_name(const string& prefix, int i,int n) 
{
  if (i>=n)
    throw myexception()<<"substitution model: referred to parameter "<<i<<" but there are only "<<n<<" parameters.";
  return prefix + convertToString(i);
}

Parameter::Parameter(const string& n, Double v)
  :name(n), value(v), fixed(false)
{
}

Parameter::Parameter(const string& n, Double v, bool f)
  :name(n), value(v), fixed(f)
{
}

Parameter::Parameter(const string& n, Double v, const Bounds<double>& b, bool f)
  :name(n), value(v), bounds(b), fixed(f)
{
}

void Model::recalc_one(int p)
{
  recalc(vector<int>(1,p));
}

void Model::recalc_all() 
{
  vector<int> indices(n_parameters());
  for(int i=0;i<indices.size();i++)
    indices[i] = i;

  recalc(indices);
}

int Model::add_parameter(const Parameter& P)
{
  for(int i=0;i<n_parameters();i++)
    if (parameters_[i].name == P.name)
      throw myexception()<<"A parameter with name '"<<P.name<<"' already exists - cannot add another one.";

  parameters_.push_back(P);
  return parameters_.size()-1;
}

std::vector<Double> Model::get_parameter_values() const
{
  vector<Double> values(n_parameters());

  for(int i=0;i<values.size();i++)
    values[i] = *parameters_[i].value;

  return values;
}

std::vector<Double> Model::get_parameter_values(const std::vector<int>& indices) const
{
  vector<Double> values(indices.size());

  for(int i=0;i<values.size();i++)
    values[i] = *parameters_[indices[i]].value;

  return values;
}

void Model::set_parameter_value(int i,Double value) 
{
  assert(0 <= i and i < n_parameters());
  *parameters_[i].value = value;
  recalc(vector<int>(1,i));
}

void Model::set_parameter_values(const vector<int>& indices,const vector<Double>& p)
{
  assert(indices.size() == p.size());
  vector<Double>::const_iterator b = p.begin();
  set_parameter_values(indices,b);
}

void Model::set_parameter_values(const vector<int>& indices,vector<Double>::const_iterator& p)
{
  assert(indices.size() <= parameters_.size());

  for(int i=0;i<indices.size();i++,p++)
    *parameters_[indices[i]].value = *p;

  recalc(indices);
}

void Model::set_parameter_values(const vector<Double>& p) 
{
  assert(parameters_.size() == p.size()) ; 
  for(int i=0;i< parameters_.size() ; i++)
    *parameters_[i].value = p[i];
  recalc_all();
}

string Model::header() const
{
  vector<string> names;
  const int n = n_parameters();
  for(int i=0;i<n;i++)
    names.push_back(parameter_name(i));
  return join(names,'\t');
}

string Model::state() const
{
  return join<Double>(get_parameter_values(),'\t');
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
  // skip initial super-only parameters
  int I=0;
  while(I < n_parameters() and
	model_slots_for_index[I].size() == 1 and 
	is_super_parameter(I))
    I++;

  // Add the new parameter and shift the ones after it
  parameters_.insert(parameters_.begin()+I           ,P);

  // Register the new parameter as being used at the top level, and shift the ones after it
  model_slots_for_index.insert(model_slots_for_index.begin()+I     ,vector<model_slot>(1,model_slot()) );

  // For each model...
  for(int m=0;m < n_submodels(); m++)
  {
    // ... for each of its arguments ...
    vector<arg_expression>& slot_expressions = slot_expressions_for_submodel[m];
    for(int i=0;i < slot_expressions.size(); i++)
      // ... that takes its value from a top-level variable that is affected ...
      if (slot_expressions[i].is_term_ref() and slot_expressions[i].parent_index >= I)
	// ... correct the reference to the parent index.
	slot_expressions[i].parent_index++;
  }

  return I;
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
  write_to_submodel(m_index);

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
    Parameter P = M.get_parameter(i);
    P.name = prefix + "::" + P.name;
    int index = add_parameter(P);
    args.push_back(index);
  }

  return register_last_submodel(args);
}

// can I write the supermodel so that it actually SHARES the values of the sub-models?
void SuperModel::write_value(int index, Double p)
{
  assert(index < n_parameters());

  *parameters_[index].value = p;

  const vector<model_slot>& model_slots = model_slots_for_index[index];

  // For each model that uses this top-level index...
  for(int i=0;i<model_slots.size();i++)
  {
    int m = model_slots[i].model_index;
    int s = model_slots[i].slot;

    //... write it down into a sub-model, if the usage is not from the top-level model.
    if (m != -1)
      SubModels(m).set_parameter_value(s,p);
  }
}

// can I write the supermodel so that it actually SHARES the values of the sub-models?
/// \todo This only writes the VALUES I think.
void SuperModel::write_values(const vector<int>& indices,vector<Double>::const_iterator& p)
{
  vector<vector<int> > model_slots(n_submodels());
  vector<vector<Double> > model_values(n_submodels());

  // Set the parameter values in the top level
  for(int i=0;i<indices.size();i++)
  {
    int index = indices[i];
    Double value = *(p+i);

    // check that all the indices are in range
    assert(index < n_parameters());

    // set the values
    *parameters_[index].value = value;

    // record the revelant slots and values for each submodel
    for(int j=0;j<model_slots_for_index[index].size();j++)
    {
      int m = model_slots_for_index[index][j].model_index;
      int s = model_slots_for_index[index][j].slot;

      if (m != -1)
      {
	model_slots[m].push_back(s);
	model_values[m].push_back(value);
      }
    }
  }

  // Write the changes down into submodels AND recalculate them.
  for(int m=0;m < n_submodels();m++)
  {
    if (not model_slots.size()) continue;

    SubModels(m).set_parameter_values(model_slots[m], model_values[m]);
  }
}

void SuperModel::write() 
{
  for(int m=0;m<n_submodels(); m++)
    write_to_submodel(m);
}

void SuperModel::write_to_submodel(int m) 
{
  // Read the current argument lists for each sub-model
  vector<Double> sub_args = SubModels(m).get_parameter_values();

  // Determine how these arguments should be computed from top level parameters
  const vector<arg_expression>& arg_expressions = slot_expressions_for_submodel[m];

  // Modify the sub_args
  for(int i=0;i<arg_expressions.size();i++)
  {
    if (arg_expressions[i].is_term_ref())
    {
      int index = arg_expressions[i].parent_index;
      sub_args[i] = get_parameter_value(index);
    }
    else
      sub_args[i] = arg_expressions[i].constant_value;
  }

  SubModels(m).set_parameter_values(sub_args);
}

void SuperModel::read_from_submodel(int m)
{
  for(int i=0;i<n_parameters();i++)
  {
    for(int j=0;j<model_slots_for_index[i].size();j++)
    {
      if (model_slots_for_index[i][j].model_index != m) continue;

      int s = model_slots_for_index[i][j].slot;
      *parameters_[i].value = SubModels(m).get_parameter_value(s);
    }
  }
}

void SuperModel::read()
{
  for(int m = 0;m < n_submodels(); m++)
    read_from_submodel(m);

  check();
}

efloat_t SuperModel::prior() const {
  efloat_t  P = super_prior();
  for(int i=0;i<n_submodels();i++)
    P *= SubModels(i).prior();
  return P;
}

void SuperModel::set_parameter_value(int p,Double value) 
{
  write_value(p,value);
  recalc_one(p);
}

void SuperModel::set_parameter_values(const vector<int>& indices,vector<Double>::const_iterator& p)
{
  assert(indices.size() <= n_parameters());

  write_values(indices,p);

  recalc(indices);
}

void SuperModel::set_parameter_values(const vector<Double>& p) 
{
  assert(n_parameters() == p.size()) ; 

  for(int i=0; i<n_parameters(); i++)
    *parameters_[i].value = p[i];

  write();
  recalc_all();
}

void SuperModel::set_parameter_values(const vector<int>& indices,const vector<Double>& p)
{
  assert(indices.size() == p.size());
  vector<Double>::const_iterator b = p.begin();
  set_parameter_values(indices,b);
}

void SuperModel::check() const
{
  for(int m=0;m<n_submodels(); m++)
  {
    // Read the current argument lists for each sub-model
    const vector<arg_expression>& arg_expressions = slot_expressions_for_submodel[m];

    for(int i=0;i<arg_expressions.size();i++)
    {
      if (arg_expressions[i].is_term_ref())
      {
	int index = arg_expressions[i].parent_index;
	assert(SubModels(m).get_parameter_value(i) == get_parameter_value(index));
      }
      else
	assert(SubModels(m).get_parameter_value(i) == arg_expressions[i].constant_value );
    }
  }
}

SuperModel::SuperModel()
{ }

int find_parameter(const Model& M,const string& name) {
  for(int i=0;i<M.n_parameters();i++) 
    if (M.parameter_name(i) == name)
      return i;
  return -1;
}
 
void show_parameters(std::ostream& o,const Model& M) {
  for(int i=0;i<M.n_parameters();i++) {
    o<<"    ";
    if (M.is_fixed(i)) 
      o<<"*";
    o<<M.parameter_name(i)<<" = "<<M.get_parameter_value(i);
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


bool path_match(const vector<string>& key, const vector<string>& pattern)
{
  int active_piece = 0;

  // require key[0] to match pattern[0] if key[0] starts w/ ^
  if (key[0].size() and key[0][0] == '^')
  { 
    int L = key[0].size()-1;
      
    if (not pattern.size())
      return false;
    
    if (not match(pattern[0], key[0].substr(1,L) ))
      return false;

    active_piece = 1;
  }

  // otherwise look for the pieces in sequential order
  for(int i=0;i<pattern.size() and active_piece < key.size();i++)
    if (match(pattern[i], key[active_piece]))
      active_piece++;

  return active_piece == key.size();
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

