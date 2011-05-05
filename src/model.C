#undef NDEBUG
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

using std::vector;
using std::string;

string parameter_name(const string& prefix, int i,int n) 
{
  if (i>=n)
    throw myexception()<<"substitution model: referred to parameter "<<i<<" but there are only "<<n<<" parameters.";
  return prefix + convertToString(i);
}

Parameter::Parameter(const string& n, double v)
  :name(n), value(v), fixed(false)
{
}

Parameter::Parameter(const string& n, double v, bool f)
  :name(n), value(v), fixed(f)
{
}

Parameter::Parameter(const string& n, double v, const Bounds<double>& b, bool f)
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

std::vector<double> Model::get_parameter_values() const
{
  vector<double> values(n_parameters());

  for(int i=0;i<values.size();i++)
    values[i] = parameters_[i].value;

  return values;
}

std::vector<double> Model::get_parameter_values(const std::vector<int>& indices) const
{
  vector<double> values(indices.size());

  for(int i=0;i<values.size();i++)
    values[i] = parameters_[indices[i]].value;

  return values;
}

void Model::set_parameter_value(int i,double value) 
{
  assert(0 <= i and i < n_parameters());
  parameters_[i].value = value;
  recalc(vector<int>(1,i));
}

void Model::set_parameter_values(const vector<int>& indices,const vector<double>& p)
{
  assert(indices.size() == p.size());
  vector<double>::const_iterator b = p.begin();
  set_parameter_values(indices,b);
}

void Model::set_parameter_values(const vector<int>& indices,vector<double>::const_iterator& p)
{
  assert(indices.size() <= parameters_.size());

  for(int i=0;i<indices.size();i++,p++)
    parameters_[indices[i]].value = *p;

  recalc(indices);
}

void Model::set_parameter_values(const vector<double>& p) 
{
  assert(parameters_.size() == p.size()) ; 
  for(int i=0;i< parameters_.size() ; i++)
    parameters_[i].value = p[i];
  recalc_all();
}


void Model::set_parameter(int i, const Parameter& P) 
{
  assert(0 <= i and i < n_parameters());
  parameters_[i] = P;
  recalc(vector<int>(1,i));
}

void Model::set_parameters(const vector<Parameter>& P) 
{
  assert(P.size() == n_parameters());
  parameters_ = P;
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
  return join<double>(get_parameter_values(),'\t');
}

int SuperModel::add_parameter(const Parameter& P)
{
  int m = ((int)first_index_of_model.size())-1;

  model_of_index.push_back(m);

  int index = Model::add_parameter(P);
  short_parameter_names.push_back(P.name);
  assert(parameters_.size() == short_parameter_names.size());
  return index;
}

int SuperModel::n_submodels() const 
{
  return first_index_of_model.size();
}

bool SuperModel::parameter_is_used_by_model(int index, int m) const
{
  if (m == -1)
    return (index < n_super_parameters());
  else
    return (index >= first_index_of_model[m] and 
	    (m+1 >= n_submodels() or index < first_index_of_model[m+1]));
}

bool SuperModel::is_super_parameter(int index) const
{
  return parameter_is_used_by_model(index,-1);
}

int SuperModel::n_super_parameters() const 
{
  if (n_submodels() == 0)
    return n_parameters();
  else
    return first_index_of_model[0];
}

// apparent the super-parameters are the first ones
int SuperModel::add_super_parameter(const Parameter& P)
{
  int I = n_super_parameters();

  parameters_.insert(parameters_.begin()+I           ,P);

  short_parameter_names.insert(short_parameter_names.begin()+I ,P.name);

  model_of_index.insert(model_of_index.begin()+I     ,-1);

  for(int i=0;i<first_index_of_model.size();i++)
    first_index_of_model[i]++;

  return I;
}

void SuperModel::prefix_names() 
{
  assert(n_parameters() == short_parameter_names.size());

  if (n_submodels() <= 1) return;

  vector<int> add_prefix(n_submodels(),0);

  for(int i=0;i<n_parameters();i++) {
    for(int j=0;j<i;j++) {
      if (short_parameter_names[i] == short_parameter_names[j]) 
      {
	int m1 = model_of_index[i];
	int m2 = model_of_index[j];

	if (m1 != -1) add_prefix[m1] = 1;
	if (m2 != -1) add_prefix[m2] = 1;
      }
    }
  }

  for(int m=0;m<n_submodels();m++) {
    int n = SubModels(m).n_parameters();
    int index = first_index_of_model[m];

    string prefix = model_prefix[m];

    for(int i=0;i<n;i++)
      if (add_prefix[m])
	parameters_[index+i].name = prefix + short_parameter_names[index+i];
      else
	parameters_[index+i].name = short_parameter_names[index+i];
  }
}

int SuperModel::register_last_submodel()
{
  int m_index = first_index_of_model.size() - 1;

  const Model& M = SubModels(m_index);

  // store the parameter name
  for(int i=0;i<M.n_parameters();i++)
    add_parameter(M.get_parameter(i));

  prefix_names();

  // check for duplicate names
  for(int i=first_index_of_model.back();i<n_parameters();i++)
    for(int j=0;j<first_index_of_model.back();j++)
      if (parameter_name(i) == parameter_name(j)) {
	if (model_of_index[i] != -1)
	  parameters_[i].name = model_prefix[model_of_index[i]]+parameter_name(i);
	if (model_of_index[j] != -1)
	  parameters_[j].name = model_prefix[model_of_index[j]]+parameter_name(j);
      }

  return m_index;
}

int SuperModel::register_submodel(const string& prefix)
{
  int m_index = first_index_of_model.size();

  // store the prefix of this model
  model_prefix.push_back(prefix+"::");

  // store the first index of this model
  first_index_of_model.push_back(n_parameters());

  return register_last_submodel();
}

void SuperModel::read() 
{
  for(int m=0;m<n_submodels();m++) 
  {
    unsigned offset = first_index_of_model[m];
    const vector<Parameter>& sub = SubModels(m).get_parameters();

    for(int i=0;i<sub.size();i++)
      parameters_[i+offset] = sub[i];
  }

  check();
}

// can I write the supermodel so that it actually SHARES the values of the sub-models?
void SuperModel::write_one(int index, const Parameter& P)
{
  assert(index < n_parameters());

  parameters_[index] = P;

  int m=model_of_index[index];
  if (m == -1) return;

  // push value down into the sub-model
  int offset = first_index_of_model[m];
  SubModels(m).set_parameter(index - offset,P);
}

// can I write the supermodel so that it actually SHARES the values of the sub-models?
void SuperModel::write_value(int index, double p)
{
  Parameter P = get_parameter(index);
  P.value = p;
  write_one(index,P);
}

// can I write the supermodel so that it actually SHARES the values of the sub-models?
/// \todo This only write the VALUE I think.
void SuperModel::write_values(const vector<int>& indices,vector<double>::const_iterator& p)
{
  for(int i=0;i<indices.size();i++) {
    assert(indices[i] < n_parameters());
    if (i > 0)
      assert(indices[i-1] < indices[i]);
    parameters_[indices[i]].value = *(p+i);
  }

  int i=0;
  while(i<indices.size() and model_of_index[indices[i]] == -1)
  {
    i++;
    p++;
  }

  // push values down into sub-models
  for(;i<indices.size();) 
  {
    // find the first model that changes
    int m= model_of_index[indices[i]];
    int offset = first_index_of_model[m];

    vector<int> sub_indices;
    for(;i<indices.size() and model_of_index[indices[i]] == m;i++)
      sub_indices.push_back(indices[i]-offset);

    SubModels(m).set_parameter_values(sub_indices,p);
  }
}

void SuperModel::write() 
{
  for(int m=0;m<n_submodels(); m++)
    write_to_submodel(m);
}

void SuperModel::write_to_submodel(int m) 
{
  vector<Parameter> sub = SubModels(m).get_parameters();

  for(int i=0;i<sub.size();i++)
    sub[i] = parameters_[i+first_index_of_model[m]];

  SubModels(m).set_parameters(sub);
}

efloat_t SuperModel::prior() const {
  efloat_t  P = super_prior();
  for(int i=0;i<n_submodels();i++)
    P *= SubModels(i).prior();
  return P;
}

void SuperModel::set_parameter_value(int p,double value) 
{
  write_value(p,value);
  recalc_one(p);
}

void SuperModel::set_parameter_values(const vector<int>& indices,vector<double>::const_iterator& p)
{
  assert(indices.size() <= n_parameters());

  write_values(indices,p);

  recalc(indices);
}

void SuperModel::set_parameter_values(const vector<double>& p) 
{
  assert(n_parameters() == p.size()) ; 

  for(int i=0; i<n_parameters(); i++)
    parameters_[i].value = p[i];

  write();
  recalc_all();
}

void SuperModel::set_parameter(int i, const Parameter& P) 
{
  write_one(i,P);
  recalc_one(i);
}

void SuperModel::set_parameters(const vector<Parameter>& p) 
{
  for(int i=0;i<n_parameters();i++)
    parameters_[i] = p[i];

  write();
  recalc_all();
}

void SuperModel::set_parameter_values(const vector<int>& indices,const vector<double>& p)
{
  assert(indices.size() == p.size());
  vector<double>::const_iterator b = p.begin();
  set_parameter_values(indices,b);
}

void SuperModel::check() const
{
  for(int m=0;m<n_submodels(); m++)
  {
    // Read the current argument lists for each sub-model
    vector<Parameter> sub_args = SubModels(m).get_parameters();

    for(int i=0;i<sub_args.size();i++)
    {
      int index = first_index_of_model[m] + i;
      assert(sub_args[i].value == parameters_[index].value);
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

