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


void Model::set_n_parameters(int n) {
  parameters_.resize(n);

  int s = fixed_.size();
  fixed_.resize(n);
  for(int i=s;i<fixed_.size();i++)
    fixed_[i] = false;
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


void SuperModel::set_super_parameters(int n) 
{
  n_super_parameters = n;

  // initialize first_index_of_model
  first_index_of_model.push_back(n_super_parameters);
  for(int m=0;m<n_submodels();m++) 
  {
    int next = first_index_of_model.back() + SubModels(m).parameters().size();
    first_index_of_model.push_back(next);
  }

  // setup parameters_ and fixed_
  Model::set_n_parameters(first_index_of_model.back());

  //initialize model_of_index
  model_of_index.resize(parameters_.size());

  for(int i=0;i<first_index_of_model[0];i++)
    model_of_index[i] = -1;

  for(int m=0;m<n_submodels();m++) 
    for(int i=first_index_of_model[m];i<first_index_of_model[m+1];i++)
      model_of_index[i] = m;
}

void SuperModel::read() 
{
  // load parameters from each sub-model
  int total=n_super_parameters;

  for(int m=0;m<n_submodels();m++) {
    const vector<double>& sub_p = SubModels(m).parameters();

    for(int i=0;i<sub_p.size();i++) {
      parameters_[i+total] = sub_p[i];
      fixed_[i+total] = SubModels(m).fixed(i);
    }

    total += sub_p.size();
  }
  assert(total == parameters_.size());
}

// can I write the supermodel so that it actually SHARES the values of the sub-models?

void SuperModel::write(int index,double p)
{
  assert(index < parameters().size());

  parameters_[index] = p;

  // calculate model indices
  vector<int> model_indices;
  model_indices.push_back(n_super_parameters);
  for(int m=0;m<n_submodels();m++) 
  {
    int next = model_indices.back() + SubModels(m).parameters().size();
    model_indices.push_back(next);
  }

  if (index < model_indices[0]) return;

  // push value down into the sub-model
  int m=0;
  while(index <= model_indices[m+1])
    m++;
  assert(m < n_submodels());
  assert(model_indices[m] <= index and index < model_indices[m+1]);
  SubModels(m).parameter(index-model_indices[m],p);
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

  // calculate model indices
  vector<int> model_indices;
  model_indices.push_back(n_super_parameters);
  for(int m=0;m<n_submodels();m++) 
  {
    int next = model_indices.back() + SubModels(m).parameters().size();
    model_indices.push_back(next);
  }

  // push values down into sub-models
  int i=0;
  while(i< indices.size() and indices[i]<model_indices[0])
  {
    i++;
    p++;
  }

  for(;i<indices.size();) 
  {
    // find the first model that changes
    int m=0;
    while(indices[i] >= model_indices[m+1])
      m++;
    assert(m < n_submodels());
    assert(model_indices[m] <= indices[i] and indices[i] < model_indices[m+1]);

    vector<int> sub_indices;
    vector<double> sub_p;
    for(;i<indices.size() and indices[i] < model_indices[m+1];i++)
      sub_indices.push_back(indices[i]-model_indices[m]);
    SubModels(m).parameters(sub_indices,p);
  }
}

void SuperModel::write() 
{
  // write parameters into each sub-model
  int total=n_super_parameters;

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

string SuperModel::parameter_name(int p) const 
{
  assert(0 <= p and p < parameters_.size());
  if (p<n_super_parameters)
    return super_parameter_name(p);
  p -= n_super_parameters;

  for(int i=0;i<n_submodels();i++) {
    if (p<SubModels(i).parameters().size())
      return SubModels(i).parameter_name(p);
    p -= SubModels(i).parameters().size();
  }
  return super_parameter_name(p);
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

int find_parameter(const Model& M,const string& name) {
  for(int i=0;i<M.parameters().size();i++) 
    if (M.parameter_name(i) == name)
      return i;
  return -1;
}
 
