#include <fstream>
#include "smodel.H"
#include "exponential.H"
#include "rng.H"
#include "util.H"
#include <gsl/gsl_randist.h>
#include <gsl/gsl_sf.h>
#include "logsum.H"
#include "likelihood.H"
#include "probability.H"

using std::vector;
using std::valarray;
using std::string;

namespace substitution {

  string s_parameter_name(int i,int n) {
    if (i>=n)
      throw myexception()<<"substitution model: refered to parameter "<<i<<" but there are only "<<n<<" parameters.";
    return string("pS") + convertToString(i);
  }

  //--------------------- Gamma_Branch_Model -----------------------------//
  Matrix Gamma_Branch_Model::transition_p(double t) const {
    double beta = parameters_.back();

    return gamma_exp(SubModel().getS(),SubModel().getD(),t/beta,beta);
  }

  string Gamma_Branch_Model::super_parameter_name(int i) const {
    return "sigma/mu";
  }

  void Gamma_Branch_Model::super_fiddle() {
    const double sigma = 0.05;
    if (not fixed(0)) {
      double beta = parameters_.back() + gaussian(0,sigma);
      if (beta<0) beta = -beta;
      if (beta >0) parameters_.back() = beta;
    }
  }

  efloat_t Gamma_Branch_Model::super_prior() const {
    const double mu = 0.1;
    double beta = parameters_.back();
    return expe(-beta/mu)/mu;
  }

  string Gamma_Branch_Model::name() const {
    return string("GammaBranch(") + SubModel().name() + ")";
  }

  //-------------------- Gamma_Stretched_Branch_Model ----------------------//

  string Gamma_Stretched_Branch_Model::super_parameter_name(int i) const {
    return "sigma/mu";
  }

  // E T = t
  // sigma/mu = parameter[0]
  Matrix Gamma_Stretched_Branch_Model::transition_p(double t) const {
    double signal_to_noise = parameters_.back();

    double alpha = 1.0/pow(signal_to_noise,2);
    double beta = t/alpha;

    return gamma_exp(SubModel().getS(),SubModel().getD(),alpha,beta);
  }

  void Gamma_Stretched_Branch_Model::super_fiddle() {
    vector<double> v = parameters_;
    double& p = v.back();

    if (fixed(0)) return;

    const double sigma = 0.04;
    double p2 = p + gaussian(0,sigma);
    if (p2 < 0) p2 = -p2;

    double alpha = 1.0/(p2*p2);
    if (alpha < 10000)
      p = p2;

    parameters(v);
  }

  efloat_t Gamma_Stretched_Branch_Model::super_prior() const {
    const double mean_stddev = 0.01;
    return expe(-parameters_.back()/mean_stddev) / mean_stddev; 
  }

  string Gamma_Stretched_Branch_Model::name() const {
    return string("GammaStretchedBranch(") + SubModel().name() + ")";
  }



  // Q(i,j) = S(i,j)*pi[j]   for i!=j
  // Q(i,i) = -sum_{i!=j} S(i,j)*pi[j]

  // We want to set S(i,i) so that Q(i,j) = S(i,j)*pi[j] for all i,j
  // Then Q = S*D, and we can easily compute the exponential
  // So, S(i,j) = Q(i,i)/pi[i]

  double ReversibleMarkovModel::rate() const {
    // Rescale so that expected mutation rate is 1
    double scale=0;
    for(int i=0;i<S.size1();i++) 
      scale -= pi[i]*Q(i,i);

    return scale;
  }

  void ReversibleMarkovModel::set_rate(double r)  {
    double scale = r/rate();
    Q *= scale;
    S *= scale;
    for(int i=0;i<eigensystem.Diagonal().size();i++)
      eigensystem.Diagonal()[i] *= scale ;
  }

  void ReversibleMarkovModel::recalc() {
    double f = parameters_[0];

    vector<double> pf(pi.size());
    for(int i=0;i<pf.size();i++)
      pf[i] = pow(pi[i],f);

    // Set S(i,i) so that Q(i,i) = S(i,i)*pi[i]
    for(int i=0;i<S.size1();i++) {
      double sum=0;
      for(int j=0;j<S.size2();j++) {
	if (i==j) continue;
	Q(i,j) = S(i,j)*pf[i]*pf[j]/pi[i];
	sum += Q(i,j);
      }
      Q(i,i) = -sum;
      S(i,i) = Q(i,i)*pi[i]/(pf[i]*pf[i]);
    }

#ifndef NDEBUG
    std::cerr<<"scale = "<<rate()<<endl;
#endif

    // Maybe assert that 
    //  A) the sum_j Q_ij = 0
    //  B) sum_i pi_i Q_ij = pi_j

    //---------- OK, calculate and cache eigensystem ----------//
    int n = pi.size();

    double DB[n];
    for(int i=0;i<n;i++)
      DB[i] = pow(pi[i],f - 0.5);
    
    SMatrix S2 = S;
    for(int i=0;i<S2.size1();i++)
      for(int j=0;j<=i;j++)
	S2(i,j) *= DB[i]*DB[j];

    eigensystem = EigenValues(S2);
  }

  Matrix ReversibleMarkovModel::getD() const {
    BMatrix D(pi.size(),pi.size());
    for(int i=0;i<pi.size();i++)
      D(i,i) = pi[i];

    return D;
  }

  Matrix ReversibleMarkovModel::transition_p(double t) const {
    //return exp(SMatrix(S),getD(),t);
    double f = parameters_[0];
    return exp(eigensystem,getD(),t,f);
  }

  efloat_t ReversibleMarkovModel::prior() const {
    valarray<double> q(1.0/frequencies().size(),frequencies().size());
    return expe( dirichlet_log_pdf(frequencies(),q,10) );
  }

  //---------------------- INV_Model --------------------------//

  void INV_Model::set_rate(double r)  {
    assert(std::abs(r) == 0);
  }


  Matrix INV_Model::transition_p(double t) const 
  {
    return P;
  }

  string INV_Model::name() const {
    return "INV";
  }

  string INV_Model::parameter_name(int i) const {
    if (i==0)
      return "RMM::f";
    else
      return s_parameter_name(i,1);
  }

  INV_Model::INV_Model(const alphabet& a)
    :ReversibleMarkovModel(a,0),ModelWithAlphabet<alphabet>(a),
     P(S.size1(),S.size2())
  {
    // Calculate S matrix
    for(int i=0;i<S.size1();i++)
      for(int j=0;j<S.size2();j++)
	if (i==j)
	  S(i,j) = 1;
	else
	  S(i,j) = 0;

    P = S;

    // Calculate Q matrix once-for-all
    ReversibleMarkovModel::recalc();
  }
      

  //------------------------- HKY -----------------------------//

  string HKY::name() const {
    return "HKY";
  }

  string HKY::parameter_name(int i) const {
    if (i==0)
      return "RMM::f";
    else if (i==1)
      return "HKY::kappa";
    else
      return s_parameter_name(i,2);
  }

  void HKY::fiddle() {
    if (fixed(0)) return;

    double k = log(kappa());

    const double sigma = 0.15;
    k += gaussian(0,sigma);

    kappa(exp(k));
  }

  /// return the LOG of the prior
  efloat_t HKY::prior() const {
    double k = log(kappa());
    efloat_t P = shift_laplace_pdf(k, log(2), 0.25);
    P *= ReversibleMarkovModel::prior();
    return P;
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

  string TNY::name() const {
    return "TNY";
  }

  void TNY::fiddle() {
    const double sigma = 0.15;

    if (not fixed(0)) {
      double k = kappa1() * exp(gaussian(0,sigma));
      kappa1(k);
    }

    if (not fixed(1)) {
      double k = kappa2() * exp(gaussian(0,sigma));
      kappa2(k);
    }
  }

  // This should be OK - the increments are linear combinations of gaussians...

  /// return the LOG of the prior
  efloat_t TNY::prior() const {
    double k1 = log(kappa1());
    double k2 = log(kappa2());
    
    double alpha = (k1+k2)/2;
    double beta  = (k1-k2)/2;

    efloat_t P = ReversibleMarkovModel::prior();
    P *= shift_laplace_pdf(alpha, log(2), 0.25);
    P *= shift_laplace_pdf(beta, 0, 0.10);
    return P;
  }

  void TNY::recalc() {
    assert(Alphabet().size()==4);

    for(int i=0;i<Alphabet().size();i++)
      for(int j=0;j<Alphabet().size();j++) {
	if (i==j) continue;
	if (Alphabet().transversion(i,j))
	  S(i,j) = 1;
	else if (Alphabet().purine(i))
	  S(i,j) = kappa1();
	else
	  S(i,j) = kappa2();
      }

    ReversibleMarkovModel::recalc();
  }

  string TNY::parameter_name(int i) const {
    assert(i==0 or i==1);
    if (i==0)
      return "RMM::f";
    if (i==1)
      return "TNY::kappa(pur)";
    else if (i==2)
      return "TNY::kappa(pyr)";
    else
      return s_parameter_name(i,2);
  }

  string EQU::parameter_name(int i) const {
    return s_parameter_name(i,0);
  }

  string EQU::name() const {
    return "EQU";
  }

  void EQU::recalc() {
    for(int i=0;i<Alphabet().size();i++)
      for(int j=0;j<Alphabet().size();j++)
	S(i,j) = 1;

    ReversibleMarkovModel::recalc();
  }

  void Empirical::fiddle() {
    if (not fixed(0)) {

      double& f = parameters_[0];

      // fiddle RMM::f
      const double sigma = 0.04;
      f += gaussian(0,sigma);

      f = wrap(f,1.0);
    }
    
    // recalc() not needed because f() value not cached
  }

  string Empirical::name() const {
    return "Empirical(" + modelname +")";
  }

  string Empirical::parameter_name(int i) const {
    if (i==0)
      return "RMM::f";
    else
      return s_parameter_name(i,1);
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

  CodonModel::CodonModel(const Codons& C)
    :ReversibleMarkovModel(C),ModelWithAlphabet<Codons>(C)
  { }

  CodonModel::~CodonModel() {}

    /// Get the parameter 'omega' (non-synonymous/synonymous rate ratio)
  double YangM0::omega() const {
    return super_parameters_[1];
  }

  /// Set the parameter 'omega' (non-synonymous/synonymous rate ratio)
  void YangM0::omega(double w) {
    super_parameters_[1]=w;
    read();
    recalc();
  }

  void YangM0::super_fiddle() {
    double sigma = 0.15;
    if (not fixed(1))
      super_parameters_[1] *= exp(gaussian(0,sigma));

    read();
    recalc();
  }

  void YangM0::recalc() {
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

	if (AminoAcid(i) != AminoAcid(j))
	  rate *= omega();
	
	S(i,j) = S(j,i) = rate * SubModel().getS()(l1,l2);
      }
    }

    ReversibleMarkovModel::recalc();
  }

  efloat_t YangM0::super_prior() const {
    efloat_t P = ReversibleMarkovModel::prior();
    P *= shift_laplace_pdf(log(omega()), 0, 0.1);
    return P;
  }

  efloat_t YangM0::prior() const {
    return NestedModelOver<ReversibleMarkovNucleotideModel>::prior();
  }

  string YangM0::name() const {
    return SubModel().name() + " * YangM0";
  }

  string YangM0::super_parameter_name(int i) const {
    if (i==0)
      return "YangM0::f";
    if (i==1)
      return "YangM0::omega";
    else
      return s_parameter_name(i,2);
  }

  //  const valarray<double>& YangM0::frequencies() const {
  //    return ReversibleMarkovModel::frequencies();
  //  }

  //  void YangM0::frequencies(const valarray<double>& pi_) {
  //    assert(pi_.size() == frequencies().size());

    /*
    for(int i=0;i<pi_.size();i++) {
      if (T.stop_codon(i)) {
	std::cerr<<"Stop codon!\n";
	if (pi_[i] >0)
	  throw myexception()<<"Giving non-zero frequency to stop codon "<<Alphabet().lookup(i)<<"!";
      }
      }*/

  //    ReversibleMarkovModel::frequencies(pi_);
  //  }

  YangM0::YangM0(const Codons& C,const ReversibleMarkovNucleotideModel& M)
    :CodonModel(C),NestedModelOver<ReversibleMarkovNucleotideModel>(M,2)
  { 
    super_parameters_[0] = 1.0;
    super_parameters_[1] = 1.0;
    read();
    omega(1.0);
  }

  YangM0::~YangM0() {}

  /*--------------- MultiRate Models ----------------*/

  double MultiModel::rate() const {
    double r=0;
    for(int m=0;m<n_base_models();m++)
      r += distribution()[m]*base_model(m).rate();
    return r;
  }

  void MultiModel::set_rate(double r)  {
    double scale = r/rate();
    for(int m=0;m<n_base_models();m++) {
      base_model(m).set_rate(base_model(m).rate()*scale);
    }
  }

  // This is per-branch, per-column - doesn't pool info about each branches across columns
  Matrix MultiModel::transition_p(double t) const {
    Matrix P = distribution()[0] * transition_p(t,0);
    for(int m=1;m<n_base_models();m++)
      P += distribution()[m] * transition_p(t,m);
    return P;
  }

  Matrix frequency_matrix(const MultiModel& M) {
    Matrix f(M.n_base_models(),M.Alphabet().size());
    for(int m=0;m<f.size1();m++)
      for(int l=0;l<f.size2();l++)
	f(m,l) = M.base_model(m).frequencies()[l];
    return f;
  }

  string UnitModel::name() const {
    return string("[") + SubModel().name() + "]";
  }

  string UnitModel::super_parameter_name(int i) const {
    return s_parameter_name(i,0);
  }

  //------------- MultiFrequencyModel ---------------//
  valarray<double> MultiFrequencyModel::get_a(int l) const 
  {
    valarray<double> al(fraction.size());

    for(int m=0;m<al.size();m++)
      al[m] = a(m,l);

    return al;
  }

  void MultiFrequencyModel::set_a(int l,const valarray<double>& al) 
  {
    for(int m=0;m<al.size();m++)
      a(m,l) = al[m];
  }

  valarray<double> get_varray(const vector<double>& v1,int start, int n) 
  {
    assert(start>=0);
    assert(n>0);
    assert(start + n <= v1.size());

    valarray<double> v2(n);
    for(int i=0;i<v2.size();i++)
      v2[i] = v1[start+i];
    return v2;
  }

  void set_varray(vector<double>& v1,int start,const valarray<double>& v2) 
  {
    assert(start>=0);
    assert(v2.size() > 0);
    assert(start + v2.size() <= v1.size());

    //copy from valarray
    for(int i=0;i<v2.size();i++)
      v1[start + i] = v2[i];
  }



  void dirichlet_fiddle(vector<double>& v,int start, int n,double sigma) 
  {
    valarray<double> fract = get_varray(v,start,n);

    // fiddle
    fract = ::dirichlet_fiddle(fract,sigma);

    set_varray(v,start,fract);

  }

  void dirichlet_fiddle(vector<double>& v,double sigma) 
  {
    dirichlet_fiddle(v,0,v.size(),sigma);
  }

  void MultiFrequencyModel::super_fiddle() 
  {
    for(int l=0;l<Alphabet().size();l++) {
      valarray<double> a = get_a(l);
      a = ::dirichlet_fiddle(a,0.10);
      set_a(l,a);
    }
    read();
    recalc();
  }

  efloat_t MultiFrequencyModel::super_prior() const 
  {
    valarray<double> flat(10.0,fraction.size());

    double Pr = 0;
    for(int l=0;l<Alphabet().size();l++) 
      Pr += dirichlet_log_pdf(get_a(l),flat);

    return expe(Pr);
  }

  const MultiModel::Base_Model_t& MultiFrequencyModel::base_model(int m) const {
    int i = m / SubModel().n_base_models();
    int j = m % SubModel().n_base_models();

    return sub_parameter_models[i]->base_model(j);
  }

  MultiModel::Base_Model_t& MultiFrequencyModel::base_model(int m) {
    int i = m / SubModel().n_base_models();
    int j = m % SubModel().n_base_models();

    return sub_parameter_models[i]->base_model(j);
  }
  
  vector<double> MultiFrequencyModel::distribution() const {
    vector<double> dist(n_base_models());

    for(int m=0;m<dist.size();m++) {
      int i = m / SubModel().n_base_models();
      int j = m % SubModel().n_base_models();

      dist[m] = fraction[i]*sub_parameter_models[i]->distribution()[j];
    }

    return dist;
  }

  /// Get the equilibrium frequencies
  const std::valarray<double>& MultiFrequencyModel::frequencies() const {
    return SubModel().frequencies();
  }

  /// Set the equilibrium frequencies
  void MultiFrequencyModel::frequencies(const std::valarray<double>& f) {
    SubModel().frequencies(f);
    recalc();
  }
  

  void MultiFrequencyModel::recalc() 
  {
    valarray<double> f = frequencies();

    // calculate probability of each sub-model
    for(int m=0;m<fraction.size();m++) {
      fraction[m] = 0;
      for(int l=0;l<Alphabet().size();l++)
	fraction[m] += a(m,l)*f[l];
    }

    // recalc sub-models
    valarray<double> fm(Alphabet().size());
    for(int m=0;m<fraction.size();m++) {
      
      for(int l=0;l<fm.size();l++)
	fm[l] = a(m,l)*f[l]/fraction[m];

      // get a new copy of the sub-model and set the frequencies
      sub_parameter_models[m] = &SubModel();
      sub_parameter_models[m]->frequencies(fm);
    }
  }

  string MultiFrequencyModel::name() const {
    return SubModel().name() + " + multi_freq[" + 
      convertToString(fraction.size()) + "]";
  }

  string MultiFrequencyModel::super_parameter_name(int i) const 
  {
    if (i < fraction.size()*Alphabet().size()) {
      int l = i/fraction.size();
      int m = i%fraction.size();
      string s = "a";
      s += Alphabet().lookup(l);
      s += convertToString(m);
      return s;
    }
    else if (i==fraction.size()*Alphabet().size()) {
      
    }

    return s_parameter_name(i,super_parameters_.size());
  }

  MultiFrequencyModel::MultiFrequencyModel(const MultiModel& M,int n)
    :ReversibleWrapperOver<MultiModel>(M,n*M.Alphabet().size()+1),
     sub_parameter_models(vector<OwnedPointer<MultiModel> >(n,M)),
     fraction(n)
  { 
    for(int l=0;l<Alphabet().size();l++)
      for(int m=0;m<n;m++)
	a(m,l) = 1.0/n;

    read();
    recalc();
  }

  //----------------------------//
  const MultiModel::Base_Model_t& MultiParameterModel::base_model(int m) const {
    int i = m / SubModel().n_base_models();
    int j = m % SubModel().n_base_models();

    return sub_parameter_models[i]->base_model(j);
  }

  MultiModel::Base_Model_t& MultiParameterModel::base_model(int m) {
    int i = m / SubModel().n_base_models();
    int j = m % SubModel().n_base_models();

    return sub_parameter_models[i]->base_model(j);
  }
  
  vector<double> MultiParameterModel::distribution() const {
    vector<double> dist(n_base_models());

    for(int m=0;m<dist.size();m++) {
      int i = m / SubModel().n_base_models();
      int j = m % SubModel().n_base_models();

      dist[m] = fraction[i]*sub_parameter_models[i]->distribution()[j];
    }

    return dist;
  }

    /// Get the equilibrium frequencies
  const std::valarray<double>& MultiParameterModel::frequencies() const {
    return SubModel().frequencies();
  }

  /// Set the equilibrium frequencies
  void MultiParameterModel::frequencies(const std::valarray<double>& f) {
    SubModel().frequencies(f);
    recalc();
  }
  

  // Um, summed-over parameter lives on as its MEAN

  void MultiParameterModel::recalc() {

    // recalc sub-model
    //NestedModel::recalc(); called from parent!

    // recalc sub-models
    vector<double> params = SubModel().parameters();
    for(int b=0;b<fraction.size();b++) {
      sub_parameter_models[b] = &SubModel();

      if (p_change == -1)
	sub_parameter_models[b]->set_rate(p_values[b]);
      else {
	params[p_change] = p_values[b];
	sub_parameter_models[b]->parameters(params);
      }
    }
  }

  MultiParameterModel::MultiParameterModel(const MultiModel& M,int dp,int p,int n) 
    :ReversibleWrapperOver<MultiModel>(M,dp),
     sub_parameter_models(vector<OwnedPointer<MultiModel> >(n,M)),
     fraction(n),
     p_change(p),
     p_values(n)
  { 
    // start with sane values for p_values
    for(int m=0;m<p_values.size();m++)
      if (p_change == -1)
	p_values[m] = M.rate();
      else
	p_values[m] = M.parameters()[p_change];

    SubModel().fixed(p_change,true);
  }

  /*--------------- Distribution-based Model----------------*/

  efloat_t DistributionParameterModel::super_prior() const {
    return D->prior();
  }

  void DistributionParameterModel::super_fiddle() {
    D->fiddle();

    super_parameters_ = D->parameters();

    read();

    recalc();
  }

  // This is supposed to push things out from parameters_
  void DistributionParameterModel::recalc() {
    write();

    D->parameters(super_parameters_);

    for(int i=0;i<p_values.size();i++)
      p_values[i] = D->quantile( double(2*i+1)/(2.0*p_values.size()) );
    

    MultiParameterModel::recalc();
  }

  string DistributionParameterModel::name() const {
    string p_name = "rate";
    if (p_change > -1)
      p_name = SubModel().parameter_name(p_change);

    string dist_name = p_name + "~" + D->name() + "(" + convertToString(p_values.size()) + ")";
    return SubModel().name() + " + " + dist_name;
  }

  string DistributionParameterModel::super_parameter_name(int i) const {
    return D->parameter_name(i);
  }

  DistributionParameterModel::DistributionParameterModel(const MultiModel& M,const RateDistribution& RD, int p, int n)
    :MultiParameterModel(M,RD.parameters().size(),p,n),
     D(RD)
  {
    // This never changes - since we use quantiles for the bins
    for(int i=0;i<p_values.size();i++)
      fraction[i] = 1.0/p_values.size();

    super_parameters_ = D->parameters();

    read();

    recalc();
  }

  /*--------------- Gamma Sites Model----------------*/

  GammaParameterModel::GammaParameterModel(const MultiModel& M,int n)
    :DistributionParameterModel(M,Gamma(),-1,n)
  {}


  /*--------------- LogNormal Sites Model----------------*/

  LogNormalParameterModel::LogNormalParameterModel(const MultiModel& M,int n)
    :DistributionParameterModel(M,LogNormal(),-1,n)
  {}


  //--------------- Invariant Sites Model----------------//

  /// Get the equilibrium frequencies
  const std::valarray<double>& WithINV::frequencies() const {
    return NestedModelOver<MultiModel>::SubModel().frequencies();
  }

  void WithINV::recalc() {
    INV->frequencies(SubModel().frequencies());
    write();
  }

  /// Set the equilibrium frequencies
  void WithINV::frequencies(const std::valarray<double>& f) {
    SubModel().frequencies(f);
    INV->frequencies(SubModel().frequencies());
  }
  

  string WithINV::name() const {
    return SubModel().name() + " + INV";
  }

  WithINV::WithINV(const MultiModel& M)
    :ReversibleWrapperOver<MultiModel>(M,1),
     INV(INV_Model(M.Alphabet()))
  {
    super_parameters_[0] = 0.01;

    read();

    recalc();
  }


  efloat_t WithINV::super_prior() const {
    double p = super_parameters_[0];

    return expe(beta_log_pdf(p, 0.02, 20));
  }

  void WithINV::super_fiddle() {
    if (not fixed(0)) {

      double &p = parameters_[0];

      // fiddle Invariant fraction
      const double sigma = 0.03;
      // p = ILOD(LOD(p) + gaussian(0,sigma));
      p = wrap( p + gaussian(0,sigma),1.0);
      assert( 0 <= p and p <= 1.0);
    }

    recalc();
  }

    /// Access the base models
  const MultiModel::Base_Model_t& WithINV::base_model(int m) const {
    if (m<SubModel().n_base_models())
      return SubModel().base_model(m);
    else
      return *INV;
  }

  MultiModel::Base_Model_t& WithINV::base_model(int m) {
    if (m<SubModel().n_base_models())
      return SubModel().base_model(m);
    else
      return *INV;
  }

  string WithINV::super_parameter_name(int i) const {
    if (i==0)
      return "INV::p";
    else
      std::abort();
  }

  vector<double> WithINV::distribution() const {
    double p = super_parameters()[0];

    vector<double> dist = SubModel().distribution();
    for(int i=0;i<dist.size();i++)
      dist[i] *= (1-p);

    dist.push_back(p);
    return dist;
  }

  void YangM2::super_fiddle() {
    // dirichlet fiddle the first 3 parameters, sigma = ?
    dirichlet_fiddle(super_parameters_, 0, 3, 0.1);

    // log-laplace fiddle the 4th parameter, wrapped so that it is always >= 1
    super_parameters_[3] *= exp(shift_laplace(0,0.2));
    if (super_parameters_[3] < 1)
      super_parameters_[3] = 1.0/super_parameters_[3];
  }

  void YangM2::recalc() {
    p_values[0] = 0;
    p_values[1] = 1;
    p_values[2] = super_parameters()[3];
  }

  efloat_t YangM2::super_prior() const {
    //What prior on the fractions?
    //What prior on the positive rates -> tend towards w=1?
    valarray<double> p(3);
    p[0] = super_parameters()[0];
    p[1] = super_parameters()[1];
    p[2] = super_parameters()[2];
    valarray<double> q(3);
    q[0] = 0.01;
    q[1] = 0.98;
    q[2] = 0.01;
    double P = dirichlet_log_pdf(p,q,10);

    double omega = super_parameters()[3];
    P += exponential_log_pdf(log(omega),0.1);
    return expe(P);
  }

  string YangM2::name() const {
    return SubModel().name() + " + YangM2";
  }

  string YangM2::super_parameter_name(int i) const {
    if (i==0)
      return "YangM2::f[AA INV]";
    else if (i==1)
      return "YangM2::f[Neutral]";
    else if (i==2)
      return "YangM2::f[Selected]";
    else if (i==3)
      return "YangM2::omega";
    else
      return s_parameter_name(i,4);
  }

  /// Get the probability of each base models
  std::vector<double> YangM2::distribution() const {
    vector<double> dist(3);
    dist[0] = super_parameters()[0];
    dist[1] = super_parameters()[1];
    dist[2] = super_parameters()[2];

    return dist;
  }

  YangM2::YangM2(const YangM0& M1) 
    :MultiParameterModel(UnitModel(M1),4,1,3)
  {
    super_parameters_[0] = 1.0/3;
    super_parameters_[1] = 1.0/3;
    super_parameters_[2] = 1.0 - super_parameters_[0] - super_parameters_[1];
    super_parameters_[3] = 1.0;

    read();

    recalc();
  }

  int MixtureModel::n_base_models() const 
  {
    int total=0;
    for(int i=0; i<n_submodels(); i++)
      total += SubModels(i).n_base_models();
    return total;
  }


  void MixtureModel::recalc() 
  {
    //recalculate submodels;
    SuperModel::recalc();

    //recalculate pi
    pi = 0;
    int sm_total = n_submodels();
    for(int sm=0;sm<sm_total;sm++)
      pi += super_parameters_[sm]*SubModels(sm).frequencies();
  }

  efloat_t MixtureModel::super_prior() const 
  {
    valarray<double> p = get_varray(super_parameters_,0,n_submodels());
    valarray<double> n = get_varray(super_parameters_,n_submodels(),n_submodels());

    n *= 10;

    return expe(dirichlet_log_pdf(p,n));
  }

  void MixtureModel::super_fiddle() 
  {
    valarray<double> p = get_varray(super_parameters_,0,n_submodels());

    const double sigma = 0.10;
    ::dirichlet_fiddle(p,sigma);

    set_varray(super_parameters_,0,p);
    
    recalc();
  }

  const MultiModel::Base_Model_t& MixtureModel::base_model(int m) const 
  {
    assert(m >= 0);

    int sm_total = n_submodels();
    for(int sm=0;sm<sm_total;sm++) {
      if (m < SubModels(sm).n_base_models())
	return SubModels(sm).base_model(m);
      else
	m -= SubModels(sm).n_base_models();
    }

    // we don't even have that many base models...
    std::abort();
  }

  MultiModel::Base_Model_t& MixtureModel::base_model(int m) 
  {
    assert(m >= 0);

    int sm_total = n_submodels();
    for(int sm=0; sm<sm_total; sm++) {
      if (m < SubModels(sm).n_base_models())
	return SubModels(sm).base_model(m);
      else
	m -= SubModels(sm).n_base_models();
    }

    // we don't even have that many base models...
    std::abort();
  }

  vector<double> MixtureModel::distribution() const 
  {
    int sm_total = n_submodels();

    vector<double> dist(n_base_models());

    for(int sm=0,m=0; sm<sm_total; sm++) {
      double f = super_parameters()[sm];
      for(int i=0;i<SubModels(sm).n_base_models();i++)
	dist[m++] = f*SubModels(0).distribution()[i];
    }
    
    return dist;
  }

  string MixtureModel::super_parameter_name(int i) const 
  {
    if (i < n_submodels()) {
      string name = "Mixture::p";
      name += convertToString(i);
      return name;
    }
    else if (i<2*n_submodels()) {
      i -= n_submodels();
      string name = "Mixture::prior";
      name += convertToString(i);
      return name;
    }

    return s_parameter_name(i,2*n_submodels());
  }

  string MixtureModel::name() const {
    string name = "MixtureModel(";
    int n = n_submodels();
    for(int i=0;i<n;i++) {
      name += SubModels(i).name();
      if (i != n-1)
	name += ", ";
    }
    name += ")";

    return name;
  }

  void MixtureModel::frequencies(const std::valarray<double>& f) 
  { 
    for(int i=0;i<n_submodels();i++)
      SubModels(i).frequencies(f);

    recalc();
  }

  MixtureModel::MixtureModel(const std::vector<OwnedPointer<MultiModel> >& models)
    :SuperDerivedModelOver<MultiModel,MultiModel>(models,2*models.size())
  {
    for(int i=0;i<models.size();i++)
      super_parameters_[i] = 1.0/models.size();

    for(int i=0;i<models.size();i++)
      super_parameters_[i+models.size()] = 1.0/models.size();

    pi.resize(Alphabet().size());

    read();

    recalc();
  }
}
