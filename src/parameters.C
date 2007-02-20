#include "parameters.H"
#include "rng.H"
#include "substitution.H"
#include "substitution-index.H"
#include "likelihood.H"
#include "util.H"
#include "proposals.H"

using std::cerr;
using std::endl;

const IndelModel& data_partition::IModel() const
{
  if (has_IModel()) return *IModel_;
  std::abort();
}

IndelModel& data_partition::IModel()
{
  if (has_IModel()) return *IModel_;
  std::abort();
}

void data_partition::recalc_imodel() 
{
  if (not has_IModel()) return;

  for(int b=0;b<branch_HMMs.size();b++) 
  {
    // use the length, unless we are unaligned
    double t = T.branch(b).length();
    if (branch_HMM_type[b] == 1)
      t = -1;
    
    // compute and cache the branch HMM
    branch_HMMs[b] = IModel_->get_branch_HMM(t);
  }
}

void data_partition::recalc_smodel() 
{
  // set the rate to one
  SModel_->set_rate(1);

  //invalidate cached conditional likelihoods in case the model has changed
  LC.invalidate_all();

  //invalidate the cached transition probabilities in case the model has changed
  MC.recalc(T,*SModel_);
}

void data_partition::setlength(int b, double l)
{
  l *= branch_mean();

  MC.setlength(b,l,T,*SModel_); 

  if (has_IModel()) {
    double t = T.branch(b).length();
    if (branch_HMM_type[b] == 1)
      t = -1;
    branch_HMMs[b] = IModel_->get_branch_HMM(t);
  }
  LC.invalidate_branch(T,b);
}

void data_partition::recalc(const vector<int>& indices)
{
  recalc_smodel();
}

double data_partition::branch_mean() const 
{
  return parameter(0);
}

void data_partition::branch_mean(double mu)
{
  parameter(0,mu);
}

string data_partition::name() const 
{
  return partition_name;
}

efloat_t data_partition::prior_no_alignment() const 
{
  // prior on mu, the mean branch length
  return pow(efloat_t(branch_mean()),-1.0);
}

efloat_t data_partition::prior_alignment() const 
{
  if (IModel_) 
    return ::prior_HMM(*this);
  else
    return 1;
}

efloat_t data_partition::prior() const 
{
  
  return prior_alignment() * prior_no_alignment();
}


efloat_t data_partition::likelihood() const 
{
  if (SModel_->full_tree)
    return substitution::Pr(*this);
  else
    return substitution::Pr_star(*this);
}

efloat_t data_partition::heated_likelihood() const 
{
  return pow(likelihood(),beta[0]);
}

efloat_t data_partition::heated_probability() const 
{
  return prior() * heated_likelihood();
}

data_partition::data_partition(const string& n, const alignment& a,const SequenceTree& t,
			       const substitution::MultiModel& SM,const IndelModel& IM)
  :IModel_(IM),
   SModel_(SM),
   partition_name(n),
   A(a),
   T(t),
   MC(t,SM),
   LC(T,SModel()),
   branch_HMMs(t.n_branches()),
   branch_HMM_type(t.n_branches(),0),
   beta(2, 1.0)
{
  add_parameter("mu", 0.1);
}

data_partition::data_partition(const string& n, const alignment& a,const SequenceTree& t,
			       const substitution::MultiModel& SM)
  :SModel_(SM),
   partition_name(n),
   A(a),
   T(t),
   MC(t,SM),
   LC(T,SModel()),
   branch_HMMs(t.n_branches()),
   branch_HMM_type(t.n_branches(),0),
   beta(2, 1.0)
{
  add_parameter("mu", 0.1);
}

//-----------------------------------------------------------------------------//


efloat_t Parameters::prior_no_alignment() const 
{
  efloat_t Pr = ::prior_no_alignment(*this);

  for(int i=0;i<data_partitions.size();i++) 
    Pr *= data_partitions[i].prior_no_alignment();

  return Pr;
}

efloat_t Parameters::prior_alignment() const 
{
  efloat_t Pr = 1;

  for(int i=0;i<data_partitions.size();i++) 
    Pr *= data_partitions[i].prior_alignment();

  return Pr;
}

efloat_t Parameters::prior() const 
{
  return prior_no_alignment() * prior_alignment();
}

efloat_t Parameters::likelihood() const 
{
  efloat_t Pr = 1;
  for(int i=0;i<data_partitions.size();i++) 
    Pr *= data_partitions[i].likelihood();
  return Pr;
}

efloat_t Parameters::heated_likelihood() const 
{
  efloat_t Pr = 1;

  for(int i=0;i<data_partitions.size();i++) 
    Pr *= data_partitions[i].heated_likelihood();

  return Pr;
}

efloat_t Parameters::heated_probability() const 
{
  return prior() * heated_likelihood();
}

const IndelModel& Parameters::IModel() const
{
  if (has_IModel()) return *IModel_;
  std::abort();
}

IndelModel& Parameters::IModel()
{
  if (has_IModel()) return *IModel_;
  std::abort();
}

void Parameters::recalc_imodel() 
{
  for(int i=0;i<data_partitions.size();i++) 
  {
    // copy our IModel down into the data partition
    data_partitions[i].IModel_ = IModel_;

    // recompute cached computations
    data_partitions[i].recalc_imodel();
  }
}

void Parameters::recalc_smodel() 
{
  // set the rate to one
  SModel_->set_rate(1);
  read();

  for(int i=0;i<data_partitions.size();i++) 
  {
    // copy our IModel down into the data partition
    data_partitions[i].SModel_ = SModel_;

    // recompute cached computations
    data_partitions[i].recalc_smodel();
  }
}

void Parameters::select_root(int b)
{
  for(int i=0;i<data_partitions.size();i++)
    ::select_root(data_partitions[i].T, b, data_partitions[i].LC);
}

void Parameters::set_root(int node)
{
  for(int i=0;i<data_partitions.size();i++)
    data_partitions[i].LC.root = node;
}

void Parameters::tree_propagate()
{
  for(int i=0;i<n_data_partitions();i++) 
  {
    data_partitions[i].T = T;
    double mu = data_partitions[i].branch_mean();
    for(int b=0;b<T.n_branches();b++) {
      double L = T.branch(b).length();
      data_partitions[i].T.branch(b).set_length(L*mu);
    }
  }
}

void Parameters::invalidate_subA_index_branch(int b)
{
  for(int i=0;i<n_data_partitions();i++)
    ::invalidate_subA_index_branch(data_partitions[i].A,data_partitions[i].T,b);
}

void Parameters::LC_invalidate_branch(int b)
{
  for(int i=0;i<n_data_partitions();i++)
    data_partitions[i].LC.invalidate_branch(data_partitions[i].T,b);
}

void Parameters::recalc(const vector<int>& indices)
{
  bool s_changed = false;
  bool i_changed = false;

  for(int i=0;i<indices.size();i++) 
  {
    int m = model_of_index[indices[i]];
    if (m == 0)
      s_changed=true;
    else if (m == 1 and has_IModel())
      i_changed=true;
  }

  if (s_changed)
    recalc_smodel();

  if (i_changed)
    recalc_imodel();
}

Model& Parameters::SubModels(int i)
{
  if (i>=n_submodels())
    throw myexception()<<"Parameters: There is no sub-model #"<<i<<"!";

  if (i==0) 
    return SModel();
  else
    i--;

  if (has_IModel())
    if (i==0)
      return IModel();
    else
      i--;

  return data_partitions[i];
}

const Model& Parameters::SubModels(int i) const
{
  if (i>=n_submodels())
    throw myexception()<<"Parameters: There is no sub-model #"<<i<<"!";

  if (i==0) 
    return SModel();
  else
    i--;

  if (has_IModel())
    if (i==0)
      return IModel();
    else
      i--;

  return data_partitions[i];
}

int Parameters::n_submodels() const
{
  int n_submodels = 1;
  if (has_IModel()) n_submodels++;
  n_submodels += n_data_partitions();

  return n_submodels;
}

void Parameters::setlength(int b,double l) 
{
  T.branch(b).set_length(l);
  for(int i=0;i<data_partitions.size();i++) 
    data_partitions[i].setlength(b,l);
}

double Parameters::branch_mean() const 
{
  return 1.0;
}

Parameters::Parameters(const vector<alignment>& A, const SequenceTree& t,
		       const substitution::MultiModel& SM,const IndelModel& IM)
  :IModel_(IM),
   SModel_(SM),
   T(t),
   TC(star_tree(t.get_sequences())),
   branch_HMM_type(t.n_branches(),0),
   beta(2, 1.0),
   features(0)
{
  constants.push_back(-1);

  add_submodel("S-model",SModel());
  if (has_IModel())
    add_submodel("I-model",IModel());

  read();

  for(int b=0;b<TC.n_branches();b++)
    TC.branch(b).set_length(-1);

  for(int i=0;i<A.size();i++) {
    string name = string("part") + convertToString(i+1);
    data_partitions.push_back(data_partition(name,A[i],T,SM,IM));
    add_submodel(name,data_partitions[i]);
  }
}

Parameters::Parameters(const vector<alignment>& A, const SequenceTree& t,
		       const substitution::MultiModel& SM)
  :SModel_(SM),
   T(t),
   TC(star_tree(t.get_sequences())),
   branch_HMM_type(t.n_branches(),0),
   beta(2, 1.0),
   features(0)
{
  constants.push_back(-1);

  add_submodel("S-model",SModel());
  if (has_IModel())
    add_submodel("I-model",IModel());

  read();

  for(int b=0;b<TC.n_branches();b++)
    TC.branch(b).set_length(-1);

  for(int i=0;i<A.size();i++) {
    string name = string("part") + convertToString(i+1);
    data_partitions.push_back(data_partition(name,A[i],T,SM));
    add_submodel(name,data_partitions[i]);
  }
}

bool accept_MH(const Parameters& P1,const Parameters& P2,double rho)
{
  efloat_t p1 = P1.heated_probability();
  efloat_t p2 = P2.heated_probability();

  double ratio = rho*double(p2/p1);

  if (ratio >= 1 or myrandomf() < ratio) 
    return true;
  else
    return false;
}

bool accept_MH_same_alignment(const Parameters& P1,const Parameters& P2,double rho) 
{
  efloat_t p1 = P1.heated_likelihood() * P1.prior_no_alignment();
  efloat_t p2 = P1.heated_likelihood() * P2.prior_no_alignment();

  double ratio = rho*(p2/p1);

  if (ratio >= 1 or myrandomf() < ratio) 
    return true;
  else
    return false;
}

