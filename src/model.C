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

void Model::recalc_one(int p)
{
  recalc(vector<int>(1,p));
}

void Model::recalc_all() 
{
  vector<int> indices(parameters().size());
  for(int i=0;i<indices.size();i++)
    indices[i] = i;

  recalc(indices);
}

void Model::add_parameter(const string& name,double value)
{
  parameter_names_.push_back(name);
  parameters_.push_back(value);
  fixed_.push_back(false);
}

std::vector<double> Model::parameters(const std::vector<int>& indices) const
{
  return read(parameters_,indices);
}

void Model::parameter(int p,double value) {
  parameters_[p] = value;
  recalc(vector<int>(1,p));
}

void Model::parameters(const vector<int>& indices,const vector<double>& p)
{
  assert(indices.size() == p.size());
  vector<double>::const_iterator b = p.begin();
  parameters(indices,b);
}

void Model::parameters(const vector<int>& indices,vector<double>::const_iterator& p)
{
  assert(indices.size() <= parameters_.size());

  for(int i=0;i<indices.size();i++,p++)
    parameters_[indices[i]] = *p;

  recalc(indices);
}

void Model::parameters(const vector<double>& p) 
{
  assert(parameters_.size() == p.size()) ; 
  parameters_=p; 
  recalc_all();
}


string Model::header() const
{
  vector<string> names;
  const int n = parameters().size();
  for(int i=0;i<n;i++)
    names.push_back(parameter_name(i));
  return join(names,'\t');
}

string Model::state() const
{
  return join<double>(parameters(),'\t');
}

int SuperModel::n_submodels() const 
{
  return first_index_of_model.size();
}

int SuperModel::n_super_parameters() const 
{
  if (n_submodels() == 0)
    return n_parameters();
  else
    return first_index_of_model[0];
}

void SuperModel::add_parameter(const string& name,double value)
{
  int m = ((int)first_index_of_model.size())-1;

  model_of_index.push_back(m);

  Model::add_parameter(name,value);
  short_parameter_names.push_back(name);
  assert(parameter_names_.size() == short_parameter_names.size());
}

void SuperModel::add_super_parameter(const string& name,double value)
{
  int I = n_super_parameters();

  parameters_.insert(parameters_.begin()+I           ,value);

  parameter_names_.insert(parameter_names_.begin()+I ,name);

  short_parameter_names.insert(short_parameter_names.begin()+I ,name);

  fixed_.insert(fixed_.begin()+I                     ,false);

  model_of_index.insert(model_of_index.begin()+I     ,-1);

  for(int i=0;i<first_index_of_model.size();i++)
    first_index_of_model[i]++;
}

void SuperModel::prefix_names() 
{
  assert(parameter_names_.size() == short_parameter_names.size());

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
	parameter_names_[index+i] = prefix + short_parameter_names[index+i];
      else
	parameter_names_[index+i] = short_parameter_names[index+i];
  }
}


void SuperModel::add_submodel(const string& prefix,const Model& M)
{
  // store the prefix of this model
  model_prefix.push_back(prefix+"::");

  // store the first index of this model
  first_index_of_model.push_back(n_parameters());

  // store the parameter name
  for(int i=0;i<M.n_parameters();i++)
    add_parameter(M.parameter_name(i),M.parameter(i));

  prefix_names();

  // check for duplicate names
  for(int i=first_index_of_model.back();i<n_parameters();i++)
    for(int j=0;j<first_index_of_model.back();j++)
      if (parameter_name(i) == parameter_name(j)) {
	if (model_of_index[i] != -1)
	  parameter_names_[i] = model_prefix[model_of_index[i]]+parameter_names_[i];
	if (model_of_index[j] != -1)
	  parameter_names_[j] = model_prefix[model_of_index[j]]+parameter_names_[j];
      }
}

void SuperModel::read() 
{
  for(int m=0;m<n_submodels();m++) 
  {
    unsigned offset = first_index_of_model[m];
    const vector<double>& sub_p = SubModels(m).parameters();

    for(int i=0;i<sub_p.size();i++) {
      parameters_[i+offset] = sub_p[i];
      fixed_[i+offset] = SubModels(m).fixed(i);
    }
  }
}

// can I write the supermodel so that it actually SHARES the values of the sub-models?

void SuperModel::write(int index,double p)
{
  assert(index < parameters().size());

  parameters_[index] = p;

  int m=model_of_index[index];
  if (m == -1) return;

  // push value down into the sub-model
  int offset = first_index_of_model[m];
  SubModels(m).parameter(index - offset,p);
}

// can I write the supermodel so that it actually SHARES the values of the sub-models?

void SuperModel::write(const vector<int>& indices,vector<double>::const_iterator& p)
{
  for(int i=0;i<indices.size();i++) {
    assert(indices[i] < parameters().size());
    if (i > 0)
      assert(indices[i-1] < indices[i]);
    parameters_[indices[i]] = *(p+i);
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
    SubModels(m).parameters(sub_indices,p);
  }
}

void SuperModel::write() 
{
  // write parameters into each sub-model
  int total=n_super_parameters();

  for(int m=0;m<n_submodels();m++) {
    vector<double> sub_p = SubModels(m).parameters();
    vector<bool> sub_f = SubModels(m).fixed();

    for(int i=0;i<sub_p.size();i++) {
      sub_p[i] = parameters_[i+total];
      sub_f[i] = fixed_[i+total];
    }
    SubModels(m).parameters(sub_p);
    SubModels(m).fixed(sub_f);

    total += sub_p.size();
  }
}

efloat_t SuperModel::prior() const {
  efloat_t  P = super_prior();
  for(int i=0;i<n_submodels();i++)
    P *= SubModels(i).prior();
  return P;
}

void SuperModel::parameter(int p,double value) 
{
  write(p,value);
  recalc_one(p);
}

void SuperModel::parameters(const vector<int>& indices,vector<double>::const_iterator& p)
{
  assert(indices.size() <= parameters_.size());

  write(indices,p);

  recalc(indices);
}

void SuperModel::parameters(const vector<double>& p) {
  assert(parameters_.size() == p.size()) ; 
  parameters_=p; 
  write();
  recalc_all();
}

void SuperModel::parameters(const vector<int>& indices,const vector<double>& p)
{
  assert(indices.size() == p.size());
  vector<double>::const_iterator b = p.begin();
  parameters(indices,b);
}

SuperModel::SuperModel()
{
}

int find_parameter(const Model& M,const string& name) {
  for(int i=0;i<M.parameters().size();i++) 
    if (M.parameter_name(i) == name)
      return i;
  return -1;
}
 
void show_parameters(std::ostream& o,const Model& M) {
  for(int i=0;i<M.parameters().size();i++) {
    o<<"    ";
    if (M.fixed(i)) 
      o<<"*";
    o<<M.parameter_name(i)<<" = "<<M.parameters()[i];
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
  for(int i=0;i<M.parameters().size();i++)
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


/// \brief Find the index of model parameters that match the pattern name
///
/// \param M      The model
/// \param name   The pattern
///
vector<int> parameters_with_extension(const Model& M, string name)
{
  bool complete_match = false;
  if (name.size() and name[0] == '^') {
    complete_match = true;
    name = name.substr(1,name.size()-1);
  }

  vector<int> indices;

  const vector<string> path2 = split(name,"::");

  if (not path2.size()) return indices;

  for(int i=0;i<M.n_parameters();i++)
  {
    vector<string> path1 = split(M.parameter_name(i),"::");

    if (not path2[0].size()) 
      path1.erase(path1.begin());
    else if (path2.size() > path1.size())
      continue;
    else if (not complete_match)
    {
      int n = path1.size() - path2.size();
      path1.erase(path1.begin(),path1.begin() + n);
    }

    if (not match(path1.back(),path2.back())) continue;

    path1.pop_back();

    vector<string> temp = path2;
    temp.pop_back();

    if (path1 == temp) indices.push_back(i);
  }
  return indices;
}

