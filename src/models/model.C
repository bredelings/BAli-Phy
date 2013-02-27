/*
   Copyright (C) 2004-2006,2009-2012 Benjamin Redelings

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
#include "models/model.H"
#include "computation/program.H"
#include "computation/expression.H"
#include "computation/module.H"
#include "computation/formula_expression.H"
#include "parser/desugar.H"

using std::vector;
using std::string;

using boost::dynamic_pointer_cast;

string parameter_name(const string& prefix, int i,int n) 
{
  if (i>=n)
    throw myexception()<<"substitution model: referred to parameter "<<i<<" but there are only "<<n<<" parameters.";
  return prefix + convertToString(i);
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
  assert(i >= 0 and i< n_parameters());
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

int Model::add_parameter(const string& name)
{
  assert(changed.size() == n_parameters());

  for(int i=0;i<n_parameters();i++)
    if (parameter_name(i) == name)
      throw myexception()<<"A parameter with name '"<<name<<"' already exists - cannot add another one.";

  int index = n_parameters();

  C.add_parameter(name);
  changed.push_back(true);
  bounds.push_back(-1);
  prior_note_index.push_back(-1);

  return index;
}

int Model::add_parameter(const string& name, const object_ref& o)
{
  int index = add_parameter(name);

  if (o)
    C.set_parameter_value(index, o);

  return index;
}

int Model::add_parameter(const string& name, const object_ref& o, const Bounds<double>& b)
{
  int index = add_parameter(name,o);

  set_bounds(index, b);

  return index;
}

std::vector< object_ptr<const Object> > Model::get_parameter_values() const
{
  std::vector< object_ptr<const Object> > values(n_parameters());

  for(int i=0;i<values.size();i++)
    values[i] = get_parameter_value(i);
    
  return values;  
}

vector<int> Model::add_submodel(const Module& M)
{
  // 1. Load the module, perform imports, and resolve its symbols.
  int old_n_notes = n_notes();
  C += M;

  // 3. Check that no parameters are declared via notes, here.
  assert( find_declared_parameters(M).empty() );

  // \todo FIXME:cleanup - Try to merge with add_submodel(context&,N)
  // 4. Account for any parameters that were added.
  vector<int> new_parameters;
  for(int i=changed.size();i<n_parameters();i++)
  {
    new_parameters.push_back( i );
    changed.push_back(true);
    bounds.push_back(-1);
    prior_note_index.push_back(-1);
  }

  for(int i=old_n_notes;i<n_notes();i++)
    process_note(i);

  return new_parameters;
}

vector<int> Model::add_submodel(const Model_Notes& N)
{
  vector<int> new_parameters;

  int first_note = n_notes();

  // 3. Find and add the declared names that don't exist yet.
  std::set<string> declared_parameter_names = find_declared_parameters(N);
  for(const auto& name: declared_parameter_names)
    if (find_parameter(name) == -1)
    {
      int index = add_parameter(name);
      new_parameters.push_back(index);
    }
    else
      throw myexception()<<"Submodel declares existing parameter '"<<name<<"'!";
  
  // 4. Add the notes from this model to the current model.
  int old_n_parameters=n_parameters();
  for(const auto& n: N.get_notes())
    add_note(n);

  for(int i=old_n_parameters;i<n_parameters();i++)
    new_parameters.push_back( i );
  
  // 5. Set default values.
  //   [Technically the parameters with default values is a DIFFERENT set than the declared parameters.]
  set_default_values_from_notes(C, first_note, n_notes());
  
  assert(changed.size() == n_parameters());

  return new_parameters;
}

int Model::add_compute_expression(const expression_ref& E)
{
  return C.add_compute_expression(E);
}

object_ref Model::evaluate(int index) const
{
  return C.evaluate(index);
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
  // This check cannot be in Context:: because there we check if operators are already defined as parameters.
  assert(is_haskell_var_name(s));

  return C.find_parameter(s);
}

int Model::add_note(const expression_ref& E)
{
  int old_n_notes = n_notes();

  int index = C.add_note(E);

  for(int i=changed.size();i<n_parameters();i++)
  {
    changed.push_back(true);
    bounds.push_back(-1);
    prior_note_index.push_back(-1);
  }

  // Only process notes that we haven't seen already.
  for(int i= old_n_notes;i < n_notes();i++)
    process_note(i);

  return index;
}

void Model::process_note(int index)
{
  const expression_ref& note = C.get_note(index);

  // 1. Check to see if this expression adds a bound.
  expression_ref query = constructor("VarBounds",2) + match(0) + match(1);

  vector<expression_ref> results;
  if (find_match(query, note, results))
  {
    object_ptr<const parameter> var = is_a<parameter>(results[0]);
    int param_index = find_parameter(var->parameter_name);
    if (param_index == -1)
      throw myexception()<<"Cannot add bound '"<<note<<"' on missing variable '"<<var->parameter_name<<"'";
    set_bounds(param_index , results[1]);
  }

  // 2. Check to see if this expression adds a prior
  if (is_exactly(note,":~") or is_exactly(note,":=~"))
  {
    // Extract the density operation
    expression_ref x = note->sub[0];
    expression_ref D = note->sub[1];

    // Create an expression for calculating the density of these random variables given their inputs
    expression_ref Pr_new = (identifier("Distributions.density"), D, x);
    
    // Record that this variable is random, and has this prior.
    // THIS would be the right place to determine what other random variables and parameters are being depended on.
    // THIS would be the right place to check that dependencies are not cyclic.
    const auto& params = find_named_parameters(x);

    if (is_exactly(note,":~"))
    {
      for(const auto& name : params )
      {
	int p_index = find_parameter(name);
	if (p_index == -1)
	  throw myexception()<<"Trying to add prior to parameter '"<<name<<"' which doesn't exist!";
	
	if (prior_note_index[p_index] != -1)
	  throw myexception()<<"Variable '"<<name<<"': new prior '"<<show_probability_expression(note)
			     <<"' on top of original prior '"<<show_probability_expression(C.get_note(prior_note_index[p_index]))<<"'?";
	else
	  prior_note_index[p_index] = index;
      }
    }
    else if (not params.empty())
      throw myexception()<<"Data note '"<<note<<"' contains parameters!";

    // Extend the probability expression to include this term also.
    // (FIXME: a balanced tree could save computation time)
    if (prior_index == -1)
      prior_index = C.add_compute_expression( Pr_new );
    else
    {
      expression_ref Pr = C.get_expression(prior_index);
      C.set_compute_expression(prior_index, (identifier("*"),Pr_new,Pr));
    }

    if (auto p = x.is_a<parameter>())
    {
      int p_index = find_parameter(p->parameter_name);
      if (p_index != -1 and bounds[p_index] == -1)
	set_bounds(p_index,(identifier("Distributions.distRange"),D));
    }
  }
}

bool Model::is_random_variable(int i) const
{
  return prior_note_index[i] != -1;
}

bool Model::has_bounds(int i) const 
{
  if (bounds[i] == -1) return false;

  return dynamic_pointer_cast<const Bounds<double>>(C.evaluate(bounds[i]));
}

const Bounds<double>& Model::get_bounds(int i) const 
{
  if (bounds[i] == -1)
    throw myexception()<<"parameter '"<<parameter_name(i)<<"' doesn't have bounds.";

  return *C.evaluate_as<Bounds<double>>(bounds[i]);
}

void Model::set_bounds(int i,const expression_ref& b) 
{
  if (auto B = b.is_a<Bounds<double>>())
  {
    set_bounds(i,*B);
    return;
  }

  expression_ref E = (identifier("Range.getBounds"),b);
  if (bounds[i] == -1)
    bounds[i] = C.add_compute_expression(E);
  else
    C.set_compute_expression(bounds[i],E);
}

void Model::set_bounds(int i,const Bounds<double>& b) 
{
  if (bounds[i] == -1)
    bounds[i] = C.add_compute_expression(b);
  else
    C.set_compute_expression(bounds[i],b);
}

object_ref Model::get_modifiable_value(int i) const
{
  return C.get_modifiable_value(i);
}

std::vector< object_ref > Model::get_modifiable_values(const std::vector<int>& indices) const
{
  std::vector< object_ref > values(indices.size());
    
  for(int i=0;i<values.size();i++)
    values[i] = get_modifiable_value(indices[i]);
  
  return values;  
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

void Model::set_modifiable_value(int m, int p, const object_ref& value) 
{
  C.set_modifiable_value(m, value);
  if (p != -1)
    modify_parameter(p);
  invalidate();
  update();
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

int Model::n_notes() const {return C.n_notes();}

const std::vector<expression_ref>& Model::get_notes() const 
{
  return C.get_notes();
}

const expression_ref Model::get_note(int i) const 
{
  return C.get_note(i);
}

int Model::find_match_notes(const expression_ref& e, std::vector<expression_ref>& results, int start) const
{
  return C.find_match_notes(e, results, start);
}

Model::Model(const module_loader& L)
  :Model(L, {})
{ }

Model::Model(const module_loader& L, const vector<expression_ref>& notes)
  :valid(false),C(L)
{
  // 1. Create the parameters
  std::set<string> names = find_declared_parameters(notes);
  if (not includes(names, find_named_parameters(notes)))
    throw myexception()<<"Some parameter is referenced, but not declared, at model creation!";
  
  for(const auto& name: names)
    add_parameter(name);

  C.alphabetize_parameters();

  // 2. Add the notes refering to the parameters.
  for(int i=0;i<notes.size();i++)
    add_note(notes[i]);

  // 3. Then set all default values.
  for(int i=0;i<n_parameters();i++)
    modify_parameter(i);

  set_default_values_from_notes(C, 0, C.n_notes());

  // 4. Set bounds.
  for(int i=0;i<n_parameters();i++)
  {
    expression_ref var = parameter(parameter_name(i));
    vector<expression_ref> results;
    expression_ref query = constructor("VarBounds",2) + var + match(0);
    int found = C.find_match_notes(query, results, 0);
    if (found != -1)
    {
      assert(results.size());
      set_bounds(i,results[0]);
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

void Model::update()
{
  if (not is_valid())
  {
    recalc(modified_parameters());
    validate();
  }
}

void show_parameters(std::ostream& o,const Model& M) {
  for(int i=0;i<M.n_parameters();i++) {
    o<<"    ";
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
bool pattern_match(const string& s1, const string& s2)
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
    paths.push_back(split(names[i],"."));

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
    names[i] = join(paths[i],".");

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
  if (pattern_match(pattern[j],key[i]))
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
vector<int> parameters_with_extension(const vector<string>& M, string name)
{
  vector<int> indices;

  const vector<string> key = split(name,".");

  if (not key.size()) return indices;

  vector<string> skeleton;

  for(int i=0;i<M.size();i++)
  {
    vector<string> pattern = split(M[i],".");

    if (not path_match(key, pattern)) continue;

    indices.push_back(i);
  }

  return indices;
}

/// \brief Find the index of model parameters that match the pattern name
///
/// \param M      The model
/// \param name   The pattern
///
vector<int> parameters_with_extension(const Model& M, string name)
{
  return parameters_with_extension(parameter_names(M), name);
}

string show_probability_expression(const expression_ref& E)
{
  if (is_exactly(E, ":~"))
  {
    expression_ref x = E->sub[0];
    expression_ref d = E->sub[1];

    return x->print() + " ~ " + d->print();
  }
  else if (is_exactly(E, ":=~"))
  {
    expression_ref x = E->sub[0];
    expression_ref d = E->sub[1];

    return x->print() + " =~ " + d->print();
  }
  else
    throw myexception()<<"Expression '"<<E<<"' is not a probability expression.";
}

vector<string> show_probability_expressions(const context& C)
{
  std::map<string,string> simplify = get_simplified_names(C.get_Program());

  vector<string> expressions;

  // Check each expression in the Formula
  for(int i=0;i<C.n_notes();i++)
    if (is_exactly(C.get_note(i),":~") or is_exactly(C.get_note(i),":=~"))
    {
      expression_ref note = map_symbol_names(C.get_note(i), simplify);
      expressions.push_back( show_probability_expression(note) );
    }

  return expressions;
}
