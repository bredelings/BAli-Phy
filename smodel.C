#include <fstream>
#include "smodel.H"
#include "exponential.H"
#include "rng.H"
#include "util.H"
#include <gsl/gsl_randist.h>
#include <gsl/gsl_sf.h>
#include "logsum.H"

namespace substitution {

  Model::Model():full_tree(true)
  { }

  Matrix Gamma_Branch_Model::transition_p(double t) const {
    double beta = parameters_[parameters_.size()-1];

    return gamma_exp(SubModel().getS(),SubModel().getD(),t/beta,beta);
  }

  void Gamma_Branch_Model::super_fiddle() {
    const double sigma = 0.05;
    double beta = parameters_[parameters_.size()-1] + gaussian(0,sigma);
    if (beta<0) beta = -beta;
    if (beta >0) parameters_[parameters_.size()-1] = beta;
  }

  double Gamma_Branch_Model::super_prior() const {
    const double mu = 0.1;
    double beta = parameters_[parameters_.size()-1];
    return -log(mu) - beta/mu;
  }

  string Gamma_Branch_Model::name() const {
    return string("GammaBranch(") + sub_model->name() + ")";
  }


  // Q(i,j) = S(i,j)*pi[j]   for i!=j
  // Q(i,i) = -sum_{i!=j} S(i,j)*pi[j]

  // We want to set S(i,i) so that Q(i,j) = S(i,j)*pi[j] for all i,j
  // Then Q = S*D, and we can easily compute the exponential
  // So, S(i,j) = Q(i,i)/pi[i]

  string MarkovModel::name() const { return "MarkovModel";}

  string ReversibleMarkovModel::name() const  {
    return MarkovModel::name() + "::ReversibleMarkovModel";
  }



  void ReversibleMarkovModel::recalc() {

    // Set S(i,i) so that Q(i,i) = S(i,i)*pi[i]
    for(int i=0;i<S.size1();i++) {
      double sum=0;
      for(int j=0;j<S.size2();j++) {
	if (i==j) continue;
	sum += S(i,j)*pi[j];
      }
      S(i,i) = -sum/pi[i];
    }

    // Rescale so that expected mutation rate is 1
    double scale=0;
    for(int i=0;i<S.size1();i++) 
      scale += pi[i]*S(i,i)*pi[i];

    S /= -scale;

    // Move from 'S' to 'S+F'
    for(int i=0;i<S.size1();i++)
      for(int j=0;j<S.size2();j++)
	Q(i,j) = S(i,j)*pi[j];


    // Rescale so expected that mutation rate is 1
    scale = 0;
    for(int i=0;i<S.size1();i++) 
      scale += pi[i]*Q(i,i);

#ifndef NDEBUG
    std::cerr<<"scale = "<<scale<<endl;
#endif
    // Maybe assert that 
    //  A) the sum_j Q_ij = 0
    //  B) sum_i pi_i Q_ij = pi_j

  }

  Matrix ReversibleMarkovModel::getD() const {
    BMatrix D(a.size(),a.size());
    for(int i=0;i<a.size();i++)
      D(i,i) = pi[i];

    return D;
  }

  Matrix ReversibleMarkovModel::transition_p(double t) const {
    return exp(S,getD(),t);
  }

  string HKY::name() const {
    return ReversibleMarkovModel::name() + "::HKY[" + Alphabet().name + "]";
  }

  void HKY::fiddle() {
    const double sigma = 0.05;
    double k = kappa() + gaussian(0,sigma);
    if (k<0) k = -k;
    if (k==0) k = kappa();

    kappa(k);
  }

  /// return the LOG of the prior
  double HKY::prior() const {
    return log(gsl_ran_lognormal_pdf(kappa(),0,0.1));
  }

  void HKY::recalc() {
    assert(a.size()==4);

    S(A,G) = kappa();
    S(A,C) = 1;
    S(A,T) = 1;

    S(G,A) = kappa();
    S(G,C) = 1;
    S(G,T) = 1;

    S(C,A) = 1;
    S(C,G) = 1;
    S(C,T) = kappa();

    S(T,A) = 1;
    S(T,G) = 1;
    S(T,C) = kappa();

    ReversibleMarkovModel::recalc();
  }

  void HKY::setup_alphabet() {
    A = a['A'];
    G = a['G'];
    C = a['C'];
    try {
      T = a['T'];
    }
    catch (bad_letter& e) {
      T = a['U'];
    }
  }

  string EQU::name() const {
    return ReversibleMarkovModel::name() + "::EQU[" + Alphabet().name + "]";
  }

  void EQU::recalc() {
    for(int i=0;i<a.size();i++)
      for(int j=0;j<a.size();j++)
	S(i,j) = 1;

    ReversibleMarkovModel::recalc();
  }

  string Empirical::name() const {
    return ReversibleMarkovModel::name() + "::Empirical/" + modelname +"[" + Alphabet().name + "]";
  }

  void Empirical::recalc() {
    ReversibleMarkovModel::recalc();
  }

  void Empirical::load_file(const string& filename) {
    modelname = filename;

    std::ifstream ifile(filename.c_str());

    if (not ifile)
      throw myexception(string("Couldn't open file '")+filename+"'");

    for(int i=0;i<a.size();i++)
      for(int j=0;j<i;j++) {
	ifile>>S(i,j);
	S(j,i) = S(i,j);
      }

    for(int i=0;i<a.size();i++)
      ifile>>pi[i];
  }


  void NestedModel::recalc() {
    vector<double> sub_p = SubModel().parameters();
    for(int i=0;i<sub_p.size();i++)
      sub_p[i] = parameters_[i];

    SubModel().parameters(sub_p);
  }

  void MultiRateModel::recalc() {
    double mean=0;
    for(int i=0;i<nrates();i++)
      mean += rates_[i]*distribution_[i];

    for(int i=0;i<nrates();i++)
      rates_[i] /= mean;

    NestedModel::recalc();
  }


  string SingleRateModel::name() const {
    return sub_model->name();
  }

  /*--------------- Distribution-based Model----------------*/

  string DistributionRateModel::name() const {
    return string("Distribution(") + convertToString(rates_.size()) + ")(" + sub_model->name() + ")";
  }

  double DistributionRateModel::super_prior() const {
    return D->prior();
  }

  void DistributionRateModel::super_fiddle() {
    D->fiddle();
    for(int i=0;i<D->parameters().size();i++)
      parameters_[sub_model->parameters().size() + i] = D->parameters()[i];
    
    recalc();
  }

  // This is supposed to push things out from parameters_
  void DistributionRateModel::recalc() {
    vector<double> temp(D->parameters().size());
    for(int i=0;i<D->parameters().size();i++)
      temp[i] = parameters_[sub_model->parameters().size() + i];
    D->parameters(temp);

    for(int i=0;i<nrates();i++)
      rates_[i] = D->quantile( double(2*i+1)/(2.0*nrates()) );

    MultiRateModel::recalc();
  }

  DistributionRateModel& DistributionRateModel::operator=(const DistributionRateModel& M) {
    MultiRateModelOver<ReversibleModel>::operator=(M);
    if (D) delete D;

    D = M.distribution().clone();

    return (*this);
  }


  DistributionRateModel::DistributionRateModel(const DistributionRateModel& M)
    :MultiRateModelOver<ReversibleModel>(M),
     D(M.distribution().clone())
  { }
  

  DistributionRateModel::DistributionRateModel(const ReversibleModel& M,const RateDistribution& RD, int n)
    :MultiRateModelOver<ReversibleModel>(M,RD.parameters().size(),n),
     D(RD.clone())
  {
    // This never changes - since we use quantiles for the bins
    for(int i=0;i<nrates();i++)
      distribution_[i] = 1.0/nrates();

    // Read in the parameters from the distribution
    for(int i=0;i<D->parameters().size();i++)
      parameters_[sub_model->parameters().size() + i] = D->parameters()[i];

    recalc();
  }

  DistributionRateModel::~DistributionRateModel() {
    if (D)
      delete D;
  }

  /*--------------- Gamma Sites Model----------------*/

  string GammaRateModel::name() const {
    return string("Gamma(") + convertToString(rates_.size()) + ")(" + sub_model->name() + ")";
  }

  GammaRateModel::GammaRateModel(const ReversibleModel& M,int n)
    :DistributionRateModel(M,Gamma(),n)
  {}


  /*--------------- Invariant Sites Model----------------*/

  string INV_Model::name() const {
    return string("INV(") + sub_model->name() + ")";
  }

  INV_Model::INV_Model(const MultiRateModel& M)
    :MultiRateModelOver<MultiRateModel>(M,2,M.nrates()+1)
  {
    parameters_[ parameters_.size()-2 ] = 0.01;
    parameters_[ parameters_.size()-1 ] = 0.01;

    recalc();
  }    


  double INV_Model::super_prior() const {
    double p = parameters()[parameters().size()-2];

    const double frac_mode = 0.05;
    const double N = 20;
    const double a  = 1.0 + N * frac_mode;
    const double b  = 1.0 + N * (1.0 - frac_mode);

    if (p <= 0.0 or p >= 1.0)
      return log_0;
    else 
      return log(gsl_ran_beta_pdf(p,a,b));
  }

  void INV_Model::super_fiddle() {
    double &p = parameters_[parameters_.size()-2];

    // fiddle Invariant fraction
    const double sigma = 0.04;
    p += gaussian(0,sigma);

    p = wrap(p,1.0);

    recalc();
  }

  void INV_Model::recalc() {
    double p = parameters_[parameters_.size()-2];

    // Thus, r is the RELATIVE rate to the other model
    // Should we try to specify an absolute rate?
    rates_[nrates()-1] = 0;
    distribution_[nrates()-1] = p;
  
    for(int r=0;r<SubModel().nrates();r++) {
      rates_[r] = SubModel().rates()[r];
      distribution_[r] = SubModel().distribution()[r]*(1.0-p);
    }

    MultiRateModel::recalc();
  }

}
