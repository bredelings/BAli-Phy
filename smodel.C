#include <fstream>
#include "smodel.H"
#include "exponential.H"
#include "rng.H"
#include "util.H"
#include <gsl/gsl_randist.h>
#include <gsl/gsl_sf.h>
#include "logsum.H"

using std::valarray;
using std::string;
using std::vector;

namespace substitution {

  Model::Model(int s)
    :parameters_(s),
     full_tree(true)
  {}

  Model::Model()
    :full_tree(true)
  { }

  //--------------------- Gamma_Branch_Model -----------------------------//
  Matrix Gamma_Branch_Model::transition_p(double t) const {
    double beta = parameters_.back();

    return gamma_exp(SubModel().getS(),SubModel().getD(),t/beta,beta);
  }

  void Gamma_Branch_Model::super_fiddle(const valarray<bool>& fixed) {
    const double sigma = 0.05;
    if (not fixed[parameters_.size()-1]) {
      double beta = parameters_.back() + gaussian(0,sigma);
      if (beta<0) beta = -beta;
      if (beta >0) parameters_.back() = beta;
    }
  }

  double Gamma_Branch_Model::super_prior() const {
    const double mu = 0.1;
    double beta = parameters_.back();
    return -log(mu) - beta/mu;
  }

  string Gamma_Branch_Model::name() const {
    return string("GammaBranch(") + sub_model->name() + ")";
  }

  //-------------------- Gamma_Stretched_Branch_Model ----------------------//

  // E T = t
  // sigma/mu = parameter[0]
  Matrix Gamma_Stretched_Branch_Model::transition_p(double t) const {
    double signal_to_noise = parameters_.back();

    double alpha = 1.0/pow(signal_to_noise,2);
    double beta = t/alpha;

    return gamma_exp(SubModel().getS(),SubModel().getD(),alpha,beta);
  }

  void Gamma_Stretched_Branch_Model::super_fiddle(const valarray<bool>& fixed) {
    vector<double> v = parameters_;
    double& p = v.back();

    if (fixed[0]) return;

    const double sigma = 0.04;
    double p2 = p + gaussian(0,sigma);
    if (p2 < 0) p2 = -p2;

    double alpha = 1.0/(p2*p2);
    if (alpha < 10000)
      p = p2;

    parameters(v);
  }

  double Gamma_Stretched_Branch_Model::super_prior() const {
    const double mean_stddev = 0.01;
    return log(mean_stddev) - parameters_.back()/mean_stddev; 
  }

  string Gamma_Stretched_Branch_Model::name() const {
    return string("GammaStretchedBranch(") + sub_model->name() + ")";
  }



  // Q(i,j) = S(i,j)*pi[j]   for i!=j
  // Q(i,i) = -sum_{i!=j} S(i,j)*pi[j]

  // We want to set S(i,i) so that Q(i,j) = S(i,j)*pi[j] for all i,j
  // Then Q = S*D, and we can easily compute the exponential
  // So, S(i,j) = Q(i,i)/pi[i]

  string MarkovModel::name() const { return "MarkovModel";}

  string ReversibleMarkovModel::name() const  {
    return "ReversibleMarkovModel";
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
    BMatrix D(pi.size(),pi.size());
    for(int i=0;i<pi.size();i++)
      D(i,i) = pi[i];

    return D;
  }

  Matrix ReversibleMarkovModel::transition_p(double t) const {
    return exp(S,getD(),t);
  }

  void NestedModel::recalc() {
    vector<double> sub_p = SubModel().parameters();
    for(int i=0;i<sub_p.size();i++)
      sub_p[i] = parameters_[i];

    SubModel().parameters(sub_p);
  }

  string HKY::name() const {
    return "HKY[" + Alphabet().name + "]";
  }

  void HKY::fiddle(const valarray<bool>& fixed) {
    double k = log(kappa());

    const double sigma = 0.15;
    k += gaussian(0,sigma);

    if (not fixed[0])
      kappa(exp(k));
  }

  /// return the LOG of the prior
  double HKY::prior() const {
    return log(gsl_ran_lognormal_pdf(kappa(), log(2), 1.0));
  }

  void HKY::recalc() {
    assert(Alphabet().size()==4);

    for(int i=0;i<Alphabet().size();i++)
      for(int j=0;j<Alphabet().size();j++) {
	if (i==j) continue;
	if (Alphabet().transversion(i,j))
	  S(i,j) = 1;
	else
	  S(i,j) = kappa();
      }

    ReversibleMarkovModel::recalc();
  }

  string EQU::name() const {
    return "EQU[" + Alphabet().name + "]";
  }

  void EQU::recalc() {
    for(int i=0;i<Alphabet().size();i++)
      for(int j=0;j<Alphabet().size();j++)
	S(i,j) = 1;

    ReversibleMarkovModel::recalc();
  }

  string Empirical::name() const {
    return "Empirical/(" + modelname +")[" + Alphabet().name + "]";
  }

  void Empirical::recalc() {
    ReversibleMarkovModel::recalc();
  }

  void Empirical::load_file(const string& filename) {
    modelname = filename;

    std::ifstream ifile(filename.c_str());

    if (not ifile)
      throw myexception(string("Couldn't open file '")+filename+"'");

    for(int i=0;i<Alphabet().size();i++)
      for(int j=0;j<i;j++) {
	ifile>>S(i,j);
	S(j,i) = S(i,j);
      }

    for(int i=0;i<Alphabet().size();i++)
      ifile>>pi[i];
  }

  //------------------------ Codon Models -------------------//
  double YangCodonModel::prior() const {
    double P = 0;
    P += log(gsl_ran_lognormal_pdf(kappa(), log(2), 1.0));
    P += log(gsl_ran_lognormal_pdf(omega(), 0, 1.0));
    return P;
  }

  void YangCodonModel::fiddle(const valarray<bool>& fixed) {
    double k = log( kappa() );
    double w = log( omega() );

    k += gaussian(0,0.15);
    w += gaussian(0,0.15);

    if (not fixed[0])
      parameters_[0] = exp(k);

    if (not fixed[1])
      parameters_[1] = exp(w);

    recalc();
  }

  string YangCodonModel::name() const {
    return "Yang-94[" + Alphabet().name + "]";
  }


  const valarray<double>& YangCodonModel::frequencies() const {
    return ReversibleMarkovModel::frequencies();
  }

  void YangCodonModel::frequencies(const valarray<double>& pi_) {
    assert(pi_.size() == frequencies().size());

    assert(pi_.size() == frequencies().size());
    for(int i=0;i<pi_.size();i++) {
      if (T.stop_codon(i)) {
	std::cerr<<"Stop codon!\n";
	if (pi_[i] >0)
	  throw myexception()<<"Giving non-zero frequency to stop codon "<<Alphabet().lookup(i)<<"!";
      }
    }

    ReversibleMarkovModel::frequencies(pi_);
  }

  void YangCodonModel::recalc() {
    for(int i=0;i<Alphabet().size();i++) {
      for(int j=0;j<i;j++) {

	int nmuts=0;
	int pos=-1;
	for(int p=0;p<3;p++)
	  if (Alphabet().sub_nuc(i,p) != Alphabet().sub_nuc(j,p)) {
	    nmuts++;
	    pos=p;
	  }
	assert(nmuts>0);
	assert(pos >= 0 and pos < 3);

	double rate=1.0;
	if (nmuts > 1) rate=0;

	int l1 = Alphabet().sub_nuc(i,pos);
	int l2 = Alphabet().sub_nuc(j,pos);
	assert(l1 != l2);

	if (Alphabet().getNucleotides().transition(l1,l2))
	  rate *= kappa();
	
	if (AminoAcid(i) != AminoAcid(j))
	  rate *= omega();
	
	S(i,j) = S(j,i) = rate;
      }
    }

    ReversibleMarkovModel::recalc();
  }

  YangCodonModel::YangCodonModel(const Translation_Table& T1)
      :Model(2),ReversibleMarkovModel(T1.getCodons()),ModelWithAlphabet<Codons>(T1.getCodons()),T(T1)
    {
      parameters_[0] = 2.0;
      parameters_[1] = 1.0;
      recalc();
    }
  YangCodonModel::~YangCodonModel() {}

  /*--------------- MultiRate Models ----------------*/


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

  void DistributionRateModel::super_fiddle(const valarray<bool>& fixed) {
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

  DistributionRateModel::DistributionRateModel(const ReversibleModel& M,const RateDistribution& RD, int n)
    :MultiRateModelOver<ReversibleModel>(M,RD.parameters().size(),n),
     D(RD)
  {
    // This never changes - since we use quantiles for the bins
    for(int i=0;i<nrates();i++)
      distribution_[i] = 1.0/nrates();

    // Read in the parameters from the distribution
    for(int i=0;i<D->parameters().size();i++)
      parameters_[sub_model->parameters().size() + i] = D->parameters()[i];

    recalc();
  }

  /*--------------- Gamma Sites Model----------------*/

  string GammaRateModel::name() const {
    return sub_model->name() + " + Gamma(" + convertToString(rates_.size()) + ")";
  }

  GammaRateModel::GammaRateModel(const ReversibleModel& M,int n)
    :DistributionRateModel(M,Gamma(),n)
  {}


  /*--------------- LogNormal Sites Model----------------*/

  string LogNormalRateModel::name() const {
    return sub_model->name() + " + LogNormal(" + convertToString(rates_.size()) + ")";
  }

  LogNormalRateModel::LogNormalRateModel(const ReversibleModel& M,int n)
    :DistributionRateModel(M,LogNormal(),n)
  {}


  //--------------- Invariant Sites Model----------------//

  string INV_Model::name() const {
    return sub_model->name() + " + INV";
  }

  INV_Model::INV_Model(const MultiRateModel& M)
    :MultiRateModelOver<MultiRateModel>(M,1,M.nrates()+1)
  {
    parameters_.back() = 0.01;

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

  void INV_Model::super_fiddle(const valarray<bool>& fixed) {
    if (fixed[parameters_.size()-1] )
      return;

    double &p = parameters_.back();

    // fiddle Invariant fraction
    const double sigma = 0.04;
    p += gaussian(0,sigma);

    p = wrap(p,1.0);

    recalc();
  }

  void INV_Model::recalc() {
    double p = parameters_.back();

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
