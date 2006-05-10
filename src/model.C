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

void Model::set_n_parameters(int n) {
  parameters_.resize(n);

  int s = fixed_.size();
  fixed_.resize(n);
  for(int i=s;i<fixed_.size();i++)
    fixed_[i] = false;
}

void Model::parameters(const vector<int>& indices,const vector<double>& p)
{
  assert(indices.size() == p.size());
  assert(indices.size() <= parameters_.size());
  for(int i=0;i<indices.size();i++)
    parameters_[indices[i]] = p[i];
  recalc();
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


void SuperModel::read() {
  // load super_parameters
  for(int i=0;i<super_parameters_.size();i++)
    parameters_[i] = super_parameters_[i];

  // load parameters from each sub-model
  int total=super_parameters_.size();
  for(int m=0;m<n_submodels();m++) {
    const std::vector<double>& sub_p = SubModels(m).parameters();

    for(int i=0;i<sub_p.size();i++) {
      parameters_[i+total] = sub_p[i];
      fixed_[i+total] = SubModels(m).fixed(i);
    }

    total += sub_p.size();
  }
  assert(total == parameters_.size());
}

void SuperModel::write() {

  // write super_parameters
  for(int i=0;i<super_parameters_.size();i++)
    super_parameters_[i] = parameters_[i];

  // write parameters into each sub-model
  int total=super_parameters_.size();
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

string SuperModel::parameter_name(int p) const {
  assert(0 <= p and p < parameters_.size());
  if (p<super_parameters_.size())
    return super_parameter_name(p);
  p -= super_parameters_.size();

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

void SuperModel::recalc() {
  write();
}

int find_parameter(const Model& M,const string& name) {
  for(int i=0;i<M.parameters().size();i++) 
    if (M.parameter_name(i) == name)
      return i;
  return -1;
}
 
