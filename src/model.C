#include "model.H"

using std::vector;
using std::string;

void Model::set_n_parameters(int n) {
  parameters_.resize(n);

  int s = fixed.size();
  fixed.resize(s);
  for(int i=s;i<fixed.size();i++)
    fixed[i] = false;
}

void SuperModel::read() {
  // load super_parameters
  for(int i=0;i<super_parameters_.size();i++)
    parameters_[i] = super_parameters_[i];

  // load parameters from each sub-model
  int total=super_parameters_.size();
  for(int m=0;m<n_submodels();m++) {
    const std::vector<double>& sub_p = SubModels(m).parameters();

    for(int i=0;i<sub_p.size();i++)
      parameters_[i+total] = sub_p[i];

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

    for(int i=0;i<sub_p.size();i++)
      sub_p[i] = parameters_[i+total];
    SubModels(m).parameters(sub_p);
    
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

double SuperModel::prior() const {
  double P = super_prior();
  for(int i=0;i<n_submodels();i++)
    P += SubModels(i).prior();
  return P;
}

void SuperModel::fiddle() {
  for(int m=0;m<n_submodels();m++)
    SubModels(m).fiddle();

  read();

  super_fiddle();
}

void SuperModel::recalc() {
  write();
}

