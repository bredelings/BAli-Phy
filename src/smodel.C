#include <algorithm>
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
#include "proposals.H"

using std::vector;
using std::valarray;
using std::string;

namespace substitution {

  string s_parameter_name(int i,int n) {
    if (i>=n)
      throw myexception()<<"substitution model: referred to parameter "<<i<<" but there are only "<<n<<" parameters.";
    return string("pS") + convertToString(i);
  }

  template <typename T>
  valarray<T> get_varray(const vector<T>& v1,int start, int n) 
  {
    assert(start>=0);
    assert(n>0);
    assert(start + n <= v1.size());

    valarray<T> v2(n);
    for(int i=0;i<v2.size();i++)
      v2[i] = v1[start+i];
    return v2;
  }

  template <typename T>
  inline valarray<T> get_varray(const vector<T>& v1) 
  {
    return get_varray(v1,0,v1.size());
  }

  template <typename T>
  void set_varray(vector<T>& v1,int start,const valarray<T>& v2) 
  {
    assert(start>=0);
    assert(v2.size() > 0);
    assert(start + v2.size() <= v1.size());

    //copy from valarray
    for(int i=0;i<v2.size();i++)
      v1[start + i] = v2[i];
  }

  template <typename T>
  inline valarray<T> set_varray(const vector<T>& v1,const valarray<T>& v2) 
  {
    return set_varray(v1,v2);
  }


  log_double_t dirichlet_fiddle(vector<double>& v,vector<bool>& fixed, int start, int n,double N) 
  {
    valarray<double> fract = get_varray(v,start,n);
    valarray<bool> mask = not get_varray(fixed,start,n);

    // fiddle
    log_double_t ratio = ::dirichlet_fiddle(fract,N,mask);

    set_varray(v,start,fract);

    return ratio;
  }

  log_double_t dirichlet_fiddle(vector<double>& v,int start, int n,double N) 
  {
    valarray<double> fract = get_varray(v,start,n);

    // fiddle
    log_double_t ratio = ::dirichlet_fiddle(fract,N);

    set_varray(v,start,fract);

    return ratio;
  }

  log_double_t dirichlet_fiddle(vector<double>& v,vector<bool>& fixed, double N) 
  {
    return dirichlet_fiddle(v, fixed, 0, v.size(), N);
  }

  log_double_t dirichlet_fiddle(vector<double>& v,double N) 
  {
    return dirichlet_fiddle(v,0,v.size(),N);
  }

  efloat_t dirichlet_pdf(const vector<double>& p1,int start, int n, const valarray<double>& q)
  {
    valarray<double> p2 = get_varray(p1,start,n);

    return ::dirichlet_pdf(p2,q);
  }

  efloat_t dirichlet_pdf(const vector<double>& p1,const valarray<double>& q)
  {
    return dirichlet_pdf(p1,0,p1.size(),q);
  }

  efloat_t dirichlet_pdf(const vector<double>& p1,int start, int n, double N)
  {
    valarray<double> p2 = get_varray(p1,start,n);

    return ::dirichlet_pdf(p2,N);
  }

  efloat_t dirichlet_pdf(const vector<double>& p1,double N)
  {
    return dirichlet_pdf(p1,0,p1.size(),N);
  }



  ExchangeModel::ExchangeModel(const alphabet& a)
    :S(a.size(),a.size())
  {}

  //----------------------- Frequency Models ------------------------//

  ReversibleFrequencyModel::ReversibleFrequencyModel(const alphabet& a)
    :R(a.size(),a.size()),
     pi(1.0/a.size(),a.size())
  { }

  void SimpleFrequencyModel::frequencies(const valarray<double>& pi2) 
  {
    // set the frequency parameters
    for(int i=0;i<size();i++)
      parameters_[i+1] = pi2[i];

    // recompute everything
    recalc();
  }

  void SimpleFrequencyModel::recalc() 
  {
    // compute frequencies
    pi = get_varray(parameters_,1,size());
    pi /= pi.sum();
    
    // compute transition rates
    valarray<double> pi_f(size());
    for(int i=0;i<size();i++)
      pi_f[i] = pow(pi[i],f());

    for(int i=0;i<size();i++)
      for(int j=0;j<size();j++)
	R(i,j) = pi_f[i]/pi[i] * pi_f[j];

    // diagonal entries should have no effect
    for(int i=0;i<size();i++)
      R(i,i) = 0;
  }

  efloat_t SimpleFrequencyModel::prior() const 
  {
    // uniform prior on f
    efloat_t Pr = 1;

    // uniform - 1 observeration per letter
    return dirichlet_pdf(parameters_, 1, size(), 1.0);
  }

  double SimpleFrequencyModel::fiddle(int) 
  {
    // propose new 'f' value
    if (not fixed(0)) {
      parameters_[0] += gaussian(0, 0.1);
      parameters_[0] = wrap(parameters_[0],1.0);
    }

    recalc();

    return 1.0;
  }

  string SimpleFrequencyModel::name() const {
    return "pi";
  }
  
  string SimpleFrequencyModel::parameter_name(int i) const {
    if (i == 0)
      return "f";
    else if (i-1<size())
      return string("pi") + Alphabet().letter(i-1);
    else
      return s_parameter_name(i,size()+1);
  }
  
  SimpleFrequencyModel::SimpleFrequencyModel(const alphabet& a)
    :ReversibleFrequencyModel(a),
     ModelWithAlphabet<alphabet>(a)
  {
    set_n_parameters(a.size() + 1);

    // Start with *f = 1
    parameters_[0] = 1.0;
    fixed_[0] = true;

    // Start with frequencies = (1/n, ... , 1/n)
    for(int i=0;i<size();i++)
      parameters_[i+1] = 1.0/size();

    // initialize everything
    recalc();
  }

  SimpleFrequencyModel::SimpleFrequencyModel(const alphabet& a,const valarray<double>& pi)
    :ReversibleFrequencyModel(a),
     ModelWithAlphabet<alphabet>(a)
  {
    set_n_parameters(a.size() + 1);

    // Start with *f = 1
    parameters_[0] = 1.0;
    fixed_[0] = true;

    // Start with frequencies = (1/n, ... , 1/n)
    valarray<double> f = pi;
    f /= f.sum();
    for(int i=0;i<size();i++)
      parameters_[i+1] = f[i];

    // initialize everything
    recalc();
  }


  //------------------- Triplet Frequency Model -----------------//
  TripletFrequencyModel::TripletFrequencyModel(const Triplets& T)
    :ReversibleFrequencyModel(T),
     ModelWithAlphabet<Triplets>(T)
  { }
    
  valarray<double> triplet_from_singlet_frequencies(const Triplets& T,const SimpleFrequencyModel& N)
  {
    if (not dynamic_cast<const Nucleotides*>(&N.Alphabet()))
      throw myexception()<<"Singlet frequencies are not nucleotide frequencies.";

    valarray<double> sub_pi = N.frequencies();

    valarray<double> pi(1.0,T.size());

    for(int i=0;i<T.size();i++) 
      for(int j=0;j<3;j++)
	pi[i] *= sub_pi[T.sub_nuc(i,j)];

    pi /= pi.sum();
    
    return pi;
  }

  void IndependentNucleotideFrequencyModel::recalc()
  {
    write();

    //------------------ compute triplet frequencies ------------------//
    pi = triplet_from_singlet_frequencies(Alphabet(),SubModel());

    vector<double> sub_parameters = SubModel().parameters();

    vector<double> triplet_parameters(size()+1);
    triplet_parameters[0] = sub_parameters[0];
    set_varray(triplet_parameters,1,pi);

    triplets->parameters(triplet_parameters);

    for(int i=0;i<size();i++)
      for(int j=0;j<size();j++)
	R(i,j) = (*triplets)(i,j);
  }

  string IndependentNucleotideFrequencyModel::super_parameter_name(int i) const
  {
    return ::parameter_name("",i,0);
  }

  string IndependentNucleotideFrequencyModel::name() const
  {
    return "pi=nucleotides";
  }

  
  IndependentNucleotideFrequencyModel::IndependentNucleotideFrequencyModel(const Triplets& T) 
    : TripletFrequencyModel(T),
      ::NestedModelOver<SimpleFrequencyModel>(SimpleFrequencyModel(T.getNucleotides()),0),
      triplets(SimpleFrequencyModel(T))
  {
    
  }


  void TripletsFrequencyModel::recalc() 
  {
    write();

    valarray<double> nu = get_varray(super_parameters_, 1, size());

    //------------- compute frequencies ------------------//
    pi = triplet_from_singlet_frequencies(Alphabet(),SubModel());

    pi *= nu;

    pi /= pi.sum();


    //------------ compute transition rates -------------//
    double g = super_parameters_[0];

    valarray<double> nu_g(size());
    for(int i=0;i<size();i++)
      nu_g[i] = pow(nu[i],g);


    // FIXME - can we really handle two mutations?
    // Should the restriction on 1 mutation be HERE?
    for(int i=0;i<size();i++)
      for(int j=0;j<size();j++) {
	R(i,j) = nu_g[i]/nu[i]*nu_g[j];
	for(int k=0;k<3;k++) {
	  int n1 = Alphabet().sub_nuc(i,k);
	  int n2 = Alphabet().sub_nuc(j,k);
	  if (n1 != n2)
	    R(i,j) *= SubModel()(n1,n2);
	}
      }

    // diagonal entries should have no effect
    for(int i=0;i<size();i++)
      R(i,i) = 0;
  }

  efloat_t TripletsFrequencyModel::super_prior() const 
  {
    return dirichlet_pdf(super_parameters_,1,size(),1.0);
  }

  double TripletsFrequencyModel::super_fiddle(int)
  {
    const double N = 10;

    if (not fixed(0)) {
      super_parameters_[0] += gaussian(0, 0.1);
      super_parameters_[0] = wrap(super_parameters_[0],1.0);
    }

    // propose new frequencies
    double ratio = dirichlet_fiddle(super_parameters_, fixed_, 1, size(), N);

    read();
    recalc();

    return ratio;
  }

  string TripletsFrequencyModel::name() const 
  {
    return "pi=triplets";
  }

  string TripletsFrequencyModel::super_parameter_name(int i) const 
  {
    if (i == 0)
      return "g";
    else if (i-1<size())
      return string("v") + Alphabet().letter(i-1);
    else
      return s_parameter_name(i,size()+1);
  }

  TripletsFrequencyModel::TripletsFrequencyModel(const Triplets& T)
    : TripletFrequencyModel(T),
      ::NestedModelOver<SimpleFrequencyModel>(SimpleFrequencyModel(T.getNucleotides()),T.size()+1)

  {
    super_parameters_[0] = 1; // g

    for(int i=0;i<size();i++)
      super_parameters_[i+1] = 1.0/size();

    read();

    recalc();
  }

  //------------------- Codon Frequency Model -----------------//

  void CodonFrequencyModel::recalc() 
  {
    write();

    double c = parameters_[0];

    //------------- compute frequencies ------------------//
    valarray<double> aa_pi = get_varray(super_parameters_, 2, aa_size());

    // get codon frequencies of sub-alphabet
    valarray<double> sub_pi = SubModel().frequencies();

    // get aa frequencies of sub-alphabet
    valarray<double> sub_aa_pi(0.0,aa_size());
    for(int i=0;i<size();i++)
      sub_aa_pi[Alphabet().translate(i)] += sub_pi[i];

    // get factors by which to multiply sub-alphabet frequencies
    valarray<double> factor(size());
    for(int i=0;i<size();i++) 
    {
      int j = Alphabet().translate(i);
      factor[i] = pow(aa_pi[j]/sub_aa_pi[j],c);
    }

    // compute aa-aware codon frequencies
    for(int i=0;i<size();i++) 
      pi[i] = sub_pi[i] * factor[i];

    // scale so as to sum to 1
    pi /= pi.sum();


    //------------ compute transition rates -------------//
    double h = super_parameters_[1];

    valarray<double> factor_h(size());
    for(int i=0;i<size();i++)
      factor_h[i] = pow(factor[i],h);


    for(int i=0;i<size();i++)
      for(int j=0;j<size();j++)
	R(i,j) = SubModel()(i,j) * factor_h[i]/factor[i]*factor_h[j];

    // diagonal entries should have no effect
    for(int i=0;i<size();i++)
      R(i,i) = 0;
  }

  efloat_t CodonFrequencyModel::super_prior() const 
  {
    return dirichlet_pdf(super_parameters_, 2, aa_size(), 1.0);
  }

  double CodonFrequencyModel::super_fiddle(int)
  {
    const double N = 10;

    if (not fixed(0)) {
      super_parameters_[0] += gaussian(0, 0.1);
      super_parameters_[0] = wrap(super_parameters_[0],1.0);
    }

    if (not fixed(1)) {
      super_parameters_[1] += gaussian(0, 0.1);
      super_parameters_[1] = wrap(super_parameters_[1],1.0);
    }

    // propose new frequencies
    double ratio = dirichlet_fiddle(super_parameters_, fixed_, 2, aa_size(), N);

    read();
    recalc();

    return ratio;  
  }

  string CodonFrequencyModel::name() const 
  {
    return "pi=codons";
  }

  string CodonFrequencyModel::super_parameter_name(int i) const 
  {
    if (i == 0)
      return "c";
    else if (i == 1)
      return "h";
    else if (i-2<Alphabet().getAminoAcids().size())
      return string("b_") + Alphabet().getAminoAcids().letter(i-2);
    else
      return s_parameter_name(i,Alphabet().getAminoAcids().size()+2);
  }

  CodonFrequencyModel::CodonFrequencyModel(const Codons& C)
    : ReversibleFrequencyModel(C),
      ::NestedModelOver<TripletsFrequencyModel>(TripletsFrequencyModel(C), C.getAminoAcids().size() + 2),
      ModelWithAlphabet<Codons>(C)
  {
    super_parameters_[0] = 0.5; // c
    super_parameters_[1] = 0.5; // h

    for(int i=0;i<C.getAminoAcids().size();i++)
      super_parameters_[i+2] = 1.0/C.getAminoAcids().size();

    read();

    recalc();
  }

  //----------------------- ReversibleMarkovModel --------------------------//
  // Q(i,j) = S(i,j)*pi[j]   for i!=j
  // Q(i,i) = -sum_{i!=j} S(i,j)*pi[j]

  // We want to set S(i,i) so that Q(i,j) = S(i,j)*pi[j] for all i,j
  // Then Q = S*D, and we can easily compute the exponential
  // So, S(i,j) = Q(i,i)/pi[i]

  double ReversibleMarkovModel::rate() const {
    // Rescale so that expected mutation rate is 1
    double scale=0;
    for(int i=0;i<Q.size1();i++) 
      scale -= frequencies()[i]*Q(i,i);

    return scale/Alphabet().width();
  }

  void ReversibleMarkovModel::set_rate(double r)  {
    double scale = r/rate();
    if (rate() == 0)
      if (r == 0)
	scale = 1;
      else
	throw myexception()<<"Model rate is 0, can't set it to "<<r<<".";
    Q *= scale;
    for(int i=0;i<eigensystem.Diagonal().size();i++)
      eigensystem.Diagonal()[i] *= scale ;
  }

  /*
   * 1. pi[i]*Q(i,j) = pi[j]*Q(j,i)         - Because Q is reversible
   * 2. Q(i,j)/pi[j] = Q(j,i)/pi[i] = S1(i,j)
   * 3. pi[i]^1/2 * Q(j,i) / pi[j]^1/2 = S2(i,j)
   * 4. exp(Q) = pi^-1.2 * exp(pi^1/2 * Q * pi^-1/2) * pi^1/2
   *           = pi^-1.2 * exp(S2) * pi^1/2
   */

  void ReversibleMarkovModel::recalc() 
  {
#ifndef NDEBUG
    std::cerr<<"scale = "<<rate()<<endl;
#endif

    const unsigned n = size();

    //--------- Compute pi[i]**0.5 and pi[i]**-0.5 ----------//
    vector<double> sqrt_pi(n);
    vector<double> inverse_sqrt_pi(n);
    for(int i=0;i<n;i++) {
      sqrt_pi[i] = sqrt(frequencies()[i]);
      inverse_sqrt_pi[i] = 1.0/sqrt_pi[i];
    }

    //--------------- Calculate eigensystem -----------------//
    SMatrix S(n,n);
    for(int i=0;i<n;i++)
      for(int j=0;j<=i;j++)
	S(i,j) = Q(i,j) * sqrt_pi[i] * inverse_sqrt_pi[j];

    //---------------- Compute eigensystem ------------------//
    eigensystem = EigenValues(S);
  }

  Matrix ReversibleMarkovModel::transition_p(double t) const 
  {
    vector<double> pi(size());
    const valarray<double> f = frequencies();
    for(int i=0;i<pi.size();i++)
      pi[i] = f[i];
    return exp(eigensystem,pi,t);
  }

  ReversibleMarkovModel::ReversibleMarkovModel(const alphabet& a)
    :MarkovModel(a), 
     eigensystem(a.size())
  { }

  void ReversibleMarkovSuperModel::recalc()
  {
    // write any changes down to sub-models
    SuperModel::recalc();

    // recompute rate matrix
    for(int i=0;i<size();i++) {
      double sum=0;
      for(int j=0;j<size();j++) {
	if (i==j) continue;
	Q(i,j) = (*S)(i,j) * (*R)(i,j);
	sum += Q(i,j);
      }
      Q(i,i) = -sum;
    }

    // recompute eigensystem
    ReversibleMarkovModel::recalc();
  }
  string ReversibleMarkovSuperModel::name() const {
    return S->name() + " * " + R->name();
  }

  string ReversibleMarkovSuperModel::super_parameter_name(int i) const {
    return ::parameter_name("",i,0);
  }

  /// Construct a reversible Markov model on alphabet 'a'
  ReversibleMarkovSuperModel::ReversibleMarkovSuperModel(const ExchangeModel& S1,const ReversibleFrequencyModel& R1)
    :ReversibleMarkovModel(S1.Alphabet()),
     ModelWithAlphabet<alphabet>(S1.Alphabet()),
     S(S1),
     R(R1)
  {
    set_n_parameters(S->parameters().size() + R->parameters().size());
    read();
    recalc();
  }
    


  void SimpleReversibleMarkovModel::frequencies(const valarray<double>& pi) 
  {
    SimpleFrequencyModel* R2 = dynamic_cast<SimpleFrequencyModel*>(R.get());
    R2->frequencies(pi);
    read();
    recalc();
  }

  SimpleReversibleMarkovModel::SimpleReversibleMarkovModel(const ExchangeModel& E)
      :ReversibleMarkovSuperModel(E,SimpleFrequencyModel(E.Alphabet()))
  { }

  SimpleReversibleMarkovModel::
  SimpleReversibleMarkovModel(const ExchangeModel& E,const valarray<double>& pi)
      :ReversibleMarkovSuperModel(E,SimpleFrequencyModel(E.Alphabet(),pi))
  { }

  //---------------------- INV_Model --------------------------//

  string INV_Model::name() const {
    return "INV";
  }

  string INV_Model::parameter_name(int i) const {
    if (i==0)
      return "INV::f";
    else
      return s_parameter_name(i,1);
  }

  INV_Model::INV_Model(const alphabet& a)
    :ExchangeModel(a),ModelWithAlphabet<alphabet>(a)
  {
    // Calculate S matrix
    for(int i=0;i<S.size1();i++)
      for(int j=0;j<S.size2();j++)
	S(i,j) = 0;
  }
      
  //----------------------- EQU -------------------------//

  string EQU::parameter_name(int i) const {
    return s_parameter_name(i,0);
  }

  string EQU::name() const {
    return "EQU";
  }

  EQU::EQU(const alphabet& a) 
    :ExchangeModel(a),ModelWithAlphabet<alphabet>(a)
  {
    for(int i=0;i<size();i++)
      for(int j=0;j<size();j++)
	S(i,j) = 1;
  }

  //----------------------- Empirical -------------------------//

  string Empirical::name() const {
    return "Empirical(" + modelname +")";
  }

  string Empirical::parameter_name(int i) const {
    return s_parameter_name(i,0);
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

    // the file has frequencies as well... where would we put them?
  }

  /// Construct an Empirical model on alphabet 'a' with matrix from 'filename'
  Empirical::Empirical(const alphabet& a,const string& filename) 
    :ExchangeModel(a),ModelWithAlphabet<alphabet>(a)
  { 
    load_file(filename); 
    recalc();
  }

  //------------------------- HKY -----------------------------//
  string HKY::name() const {
    return "HKY";
  }

  string HKY::parameter_name(int i) const {
    if (i==0)
      return "HKY::kappa";
    else
      return s_parameter_name(i,1);
  }

  double HKY::fiddle(int) {
    if (not fixed(0))
      kappa( kappa() * exp(gaussian(0,0.15)) );

    return 1;
  }

  efloat_t HKY::prior() const 
  {
    return shift_laplace_pdf(log(kappa()), log(2), 0.25);
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
  }

  /// Construct an HKY model on alphabet 'a'
  HKY::HKY(const Nucleotides& N)
    : NucleotideExchangeModel(N)
  { 
    set_n_parameters(1);
    kappa(2);
  }

  //------------------------- TN -----------------------------//
  string TN::name() const {
    return "TN";
  }

  double TN::fiddle(int) 
  {
    const double sigma = 0.15;

    if (not fixed(0)) {
      double k = kappa1() * exp(gaussian(0,sigma));
      kappa1(k);
    }

    if (not fixed(1)) {
      double k = kappa2() * exp(gaussian(0,sigma));
      kappa2(k);
    }
    return 1;
  }

  // This should be OK - the increments are linear combinations of gaussians...

  /// return the LOG of the prior
  efloat_t TN::prior() const {
    double k1 = log(kappa1());
    double k2 = log(kappa2());
    
    double alpha = (k1+k2)/2;
    double beta  = (k1-k2)/2;

    efloat_t P = 1;
    P *= shift_laplace_pdf(alpha, log(2), 0.25);
    P *= shift_laplace_pdf(beta, 0, 0.10);
    return P;
  }

  void TN::recalc() {
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
  }

  string TN::parameter_name(int i) const 
  {
    if (i==0)
      return "TN::kappa(pur)";
    else if (i==1)
      return "TN::kappa(pyr)";
    else
      return s_parameter_name(i,2);
  }

    /// Construct an HKY model on alphabet 'a'
  TN::TN(const Nucleotides& N)
    : NucleotideExchangeModel(N)
  { 
    set_n_parameters(2);
    kappa1(2);
    kappa2(2);
  }

  string GTR::name() const {
    return "GTR";
  }

  double GTR::fiddle(int) 
  {
    const double N = 10;
    return dirichlet_fiddle(parameters_,fixed_, N);
  }

  // This should be OK - the increments are linear combinations of gaussians...

  /// return the LOG of the prior
  efloat_t GTR::prior() const 
  {
    valarray<double> q(6);

    q[0] = 2; // AG - transition

    q[1] = 1; // AT - transversion

    q[2] = 1; // AC - transversion

    q[3] = 1; // GT - transversion

    q[4] = 1; // GC - transversion

    q[5] = 2; // TC - transition

    q *= 4;

    return dirichlet_pdf(parameters_, q);
  }

  void GTR::recalc() {
    assert(Alphabet().size()==4);

    S(0,1) = parameters_[0]; // AG
    S(0,2) = parameters_[1]; // AT
    S(0,3) = parameters_[2]; // AC

    S(1,2) = parameters_[3]; // GT
    S(2,1) = parameters_[4]; // GC

    S(2,3) = parameters_[5]; // TC
  }

  string GTR::parameter_name(int i) const 
  {
    if (i==0)
      return "GTR::AG";
    else if (i==1)
      return "GTR::AT";
    else if (i==2)
      return "GTR::AC";

    else if (i==3)
      return "GTR::GT";
    else if (i==4)
      return "GTR::GC";

    else if (i==5)
      return "GTR::TC";

    else
      return s_parameter_name(i,6);
  }

  GTR::GTR(const Nucleotides& N)
      : NucleotideExchangeModel(N)
    { 
      set_n_parameters(6);

      for(int i=0;i<parameters_.size();i++)
	parameters_[i] = 1.0/6;

      recalc();
    }

  //------------------------ Triplet Models -------------------//

  TripletExchangeModel::TripletExchangeModel(const Triplets& T)
    :ExchangeModel(T),ModelWithAlphabet<Triplets>(T)
  { }

  void SingletToTripletExchangeModel::recalc() 
  {
    write();

    for(int i=0;i<Alphabet().size();i++)
      for(int j=0;j<i;j++) 
      {
	int nmuts=0;
	int pos=-1;
	for(int p=0;p<3;p++)
	  if (Alphabet().sub_nuc(i,p) != Alphabet().sub_nuc(j,p)) {
	    nmuts++;
	    pos=p;
	  }
	assert(nmuts>0);
	assert(pos >= 0 and pos < 3);
	
	S(i,j) = 0;

	if (nmuts == 1) {

	  int l1 = Alphabet().sub_nuc(i,pos);
	  int l2 = Alphabet().sub_nuc(j,pos);
	  assert(l1 != l2);

	  S(i,j) = SubModel()(l1,l2);
	}
      }
  }

  string SingletToTripletExchangeModel::name() const {
    string n = SubModel().name();
    n += "x3";
    return n;
  }
  
  string SingletToTripletExchangeModel::super_parameter_name(int i) const {
    return ::parameter_name("",i,0);
  }

  SingletToTripletExchangeModel::SingletToTripletExchangeModel(const Triplets& T,const NucleotideExchangeModel& N)
    :TripletExchangeModel(T),::NestedModelOver<NucleotideExchangeModel>(N,0)
  { 
    read();
    recalc();
  }

  //------------------------ Codon Models -------------------//

  CodonExchangeModel::CodonExchangeModel(const Codons& C)
    :ExchangeModel(C),ModelWithAlphabet<Codons>(C)
  { }

  /// Get the parameter 'omega' (non-synonymous/synonymous rate ratio)
  double YangM0::omega() const {
    return super_parameters_[0];
  }

  /// Set the parameter 'omega' (non-synonymous/synonymous rate ratio)
  void YangM0::omega(double w) {
    super_parameters_[0]=w;
    read();
    recalc();
  }

  double YangM0::super_fiddle(int) 
  {
    double ratio = 1;

    if (not fixed(0))
      ratio = scale_gaussian(super_parameters_[0], 0.2);

    read();
    recalc();

    return ratio;
  }

  void YangM0::recalc() {
    write();

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

	double rate=0.0;

	if (nmuts == 1) {

	  int l1 = Alphabet().sub_nuc(i,pos);
	  int l2 = Alphabet().sub_nuc(j,pos);
	  assert(l1 != l2);

	  rate = SubModel()(l1,l2);

	  if (AminoAcid(i) != AminoAcid(j))
	    rate *= omega();	
	}

	S(i,j) = S(j,i) = rate;
      }
    }
  }

  efloat_t YangM0::super_prior() const {
    return shift_laplace_pdf(log(omega()), 0, 0.1);
  }

  efloat_t YangM0::prior() const {
    return ::NestedModelOver<NucleotideExchangeModel>::prior();
  }

  string YangM0::name() const {
    return string("YangM0[") + SubModel().name() + "]";
  }

  string YangM0::super_parameter_name(int i) const {
    if (i==0)
      return "YangM0::omega";
    else
      return s_parameter_name(i,1);
  }

  YangM0::YangM0(const Codons& C,const NucleotideExchangeModel& N)
    :CodonExchangeModel(C),::NestedModelOver<NucleotideExchangeModel>(N,1)
  { 
    omega(1.0);
  }

  YangM0::~YangM0() {}

  //--------------- MultiRate Models ----------------//

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

  double MultiFrequencyModel::super_fiddle(int) 
  {
    // FIXME - ??? Does this still work after modifying dirichlet_fiddle?
    const double N = 10;

    // get factor by which to modify bin frequencies
    valarray<double> C(fraction.size());
    for(int m=0;m<fraction.size();m++)
      C[m] = exp(gaussian(0,0.1));

    int n1 =(int)( myrandomf()*Alphabet().size());
    int n2 =(int)( myrandomf()*Alphabet().size());

    double ratio = 1;

    for(int l=0;l<Alphabet().size();l++) 
    {
      valarray<double> a = get_a(l);
      a *= C;
      a /= a.sum();
      if (l==n1 or l==n2)
	ratio *= ::dirichlet_fiddle(a,N);
      set_a(l,a);
    }

    read();
    recalc();

    return ratio;
  }

  efloat_t MultiFrequencyModel::super_prior() const 
  {
    // uniform - 10 counts per bin
    efloat_t Pr = 1;
    for(int l=0;l<Alphabet().size();l++) 
      Pr *= ::dirichlet_pdf(get_a(l),10);

    return Pr;
  }

  const MultiModel::Base_Model_t& MultiFrequencyModel::base_model(int i) const {
    return *sub_parameter_models[i];
  }

  MultiModel::Base_Model_t& MultiFrequencyModel::base_model(int i) {
    return *sub_parameter_models[i];
  }
  
  vector<double> MultiFrequencyModel::distribution() const {
    return fraction;
  }

  /// Get the equilibrium frequencies
  const std::valarray<double>& MultiFrequencyModel::frequencies() const {
    return SubModel().frequencies();
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

  MultiFrequencyModel::MultiFrequencyModel(const ExchangeModel& E,int n)
    :ReversibleWrapperOver<SimpleReversibleMarkovModel>(SimpleReversibleMarkovModel(E),n*E.Alphabet().size()+1),
     //     sub_parameter_models(vector<OwnedPointer<SimpleReversibleMarkovModel> >(n,SimpleReversibleMarkovModel(E))),
     fraction(n)
  { 
    sub_parameter_models.resize(n);
    for(int i=0;i<n;i++)
      sub_parameter_models[i] = SubModel();

    for(int l=0;l<Alphabet().size();l++)
      for(int m=0;m<n;m++)
	a(m,l) = 1.0/n;

    read();
    recalc();
  }

  //---------------------------- class MultiModel --------------------------//
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

    if (p_change != -1)
      SubModel().fixed(p_change,true);
  }

  /*--------------- Distribution-based Model----------------*/

  efloat_t DistributionParameterModel::super_prior() const {
    return D->prior();
  }

  double DistributionParameterModel::super_fiddle(int i) {
    double rho = D->fiddle(i);

    super_parameters_ = D->parameters();

    read();

    recalc();

    return rho;
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

  const double WithINV::inv_frac_mean = 0.1;
  const double WithINV::max_inv_rate = 0.01;

  /// Get the equilibrium frequencies
  const std::valarray<double>& WithINV::frequencies() const {
    return NestedModelOver<MultiModel>::SubModel().frequencies();
  }

  void WithINV::recalc() {
    INV->frequencies(SubModel().frequencies());
    write();
  }

  string WithINV::name() const {
    return SubModel().name() + " + INV";
  }

  WithINV::WithINV(const MultiModel& M)
    :ReversibleWrapperOver<MultiModel>(M,1),
     INV(SimpleReversibleMarkovModel(INV_Model(M.Alphabet())))
  {
    super_parameters_[0] = 0.01;

    read();

    recalc();
  }


  efloat_t WithINV::super_prior() const {
    double p = super_parameters_[0];

    return beta_pdf(p, 0.02, 20);
  }

  double WithINV::super_fiddle(int) {
    if (not fixed(0)) {

      double &p = parameters_[0];

      // fiddle Invariant fraction
      const double sigma = 0.03;
      // p = ILOD(LOD(p) + gaussian(0,sigma));
      p = wrap( p + gaussian(0,sigma),1.0);
      assert( 0 <= p and p <= 1.0);
    }

    recalc();

    return 1;
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

  double YangM2::super_fiddle(int) 
  {
    const double N = 10;
    // dirichlet fiddle the first 3 parameters, sigma = ?
    double ratio = 1.0;

    ratio *= dirichlet_fiddle(super_parameters_, fixed_, 0, 3, N);

    // log-laplace fiddle the 4th parameter, wrapped so that it is always >= 1
    double scale = exp(shift_laplace(0,0.2));
    ratio *= scale;
    super_parameters_[3] *= scale;
    if (super_parameters_[3] < 1)
      super_parameters_[3] = 1.0/super_parameters_[3];

    read();
    recalc();

    return ratio;
  }

  void YangM2::recalc() 
  {
    // push values out from parameters to superparameters and sub-models
    write();

    fraction[0] = super_parameters()[0];
    fraction[1] = super_parameters()[1];
    fraction[2] = super_parameters()[2];

    p_values[0] = 0;
    p_values[1] = 1;
    p_values[2] = super_parameters()[3];

    MultiParameterModel::recalc();
  }

  efloat_t YangM2::super_prior() const 
  {
    // prior on frequencies
    valarray<double> q(3);
    q[0] = 0.01;
    q[1] = 0.98;
    q[2] = 0.01;
    efloat_t P = dirichlet_pdf(super_parameters_, 0, 3, 100.0*q);

    // prior on omega
    double omega = super_parameters()[3];
    P *= exponential_pdf(log(omega),0.05);
    return P;
  }

  string YangM2::name() const {
    return SubModel().name() + " + YangM2";
  }

  string YangM2::super_parameter_name(int i) const 
  {
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

  YangM2::YangM2(const YangM0& M1,const ReversibleFrequencyModel& R) 
    :MultiParameterModel(UnitModel(ReversibleMarkovSuperModel(M1,R)),
			 4,0,3)
  {
    super_parameters_[0] = 1.0/3;
    super_parameters_[1] = 1.0/3;
    super_parameters_[2] = 1.0 - super_parameters_[0] - super_parameters_[1];
    super_parameters_[3] = 1.0;

    read();

    recalc();
  }

  int any_set(const vector<bool>& mask,int i1,int i2) 
  {
    int inc = (i2 > i1)?1:-1;
      
    for(int i=i1;i!=i2;i+=inc) {
      if (mask[i])
	return i;
    }
    return -1;
  }

  //YangM3

  double YangM3::omega(int i) const {
    return super_parameters_[fraction.size() + i];
  }

  /// Set the parameter 'omega' (non-synonymous/synonymous rate ratio)
  void YangM3::omega(int i,double w) {
    super_parameters_[fraction.size()+i]=w;
    read();
    recalc();
  }

  double reflect_left(double x, double min) {
    if (x < min)
      x = min + (min-x);
    return x;
  }

  double reflect_right(double x, double max) {
    if (x > max)
      x = max - (x-max);
    return x;
  }

  double YangM3::super_fiddle(int) 
  {
    const double N = 10;

    double ratio=1;

    // dirichlet fiddle the frequency parameters
    ratio *= dirichlet_fiddle(super_parameters_, fixed_, 0, fraction.size(), N);

    // log-laplace fiddle the omega parameters
    for(int i=0;i<fraction.size();i++)
      if (not fixed(i+fraction.size())) {
	double scale = shift_laplace(0,0.1);
	ratio *= exp(scale);
	double w = log(omega(i)) + scale;

	double max = 0;
	double min = 0;

	int wmin = any_set(fixed_, fraction.size() + i - 1, fraction.size()-1);
	if (wmin != -1) {
	  wmin -= fraction.size();
	  min = log(omega(wmin));
	}
	    
	int wmax = any_set(fixed_, fraction.size() + i + 1, 2*fraction.size());
	if (wmax != -1) {
	  wmax -= fraction.size();
	  max = log(omega(wmax));
	}

	if (wmin != -1 and wmax != -1)
	  w = wrap(w,min,max);
	else if (wmin != -1) 
	  w = reflect_left(w,min);
	else if (wmax != -1) 
	  w = reflect_right(w,max);

	super_parameters_[i+fraction.size()] = exp(w);
      }

    // we really should SORT the parameters now...
    std::sort(super_parameters_.begin()+fraction.size(),
	      super_parameters_.begin()+2*fraction.size());
    read();
    recalc();
    return ratio;
  }

  void YangM3::recalc() 
  {
    // push values out from parameters to superparameters and sub-models
    write();

    for(int i=0;i<fraction.size();i++)
      fraction[i] = super_parameters()[i];

    for(int i=0;i<fraction.size();i++)
      p_values[i] = super_parameters()[fraction.size()+i];

    MultiParameterModel::recalc();
  }

  efloat_t YangM3::super_prior() const 
  {
    efloat_t P = 1;

    // prior on frequencies
    P *= dirichlet_pdf(super_parameters_, 0, fraction.size(), 4);

    // prior on rates
    for(int i=0;i<fraction.size();i++) {
      double omega = super_parameters()[i+fraction.size()];
      P *= shift_laplace_pdf(log(omega), 0, 0.1);
    }
    return P;
  }

  string YangM3::name() const {
    return SubModel().name() + " + YangM3[" + convertToString(fraction.size()) + "]";
  }

  string YangM3::super_parameter_name(int i) const {
    if (i<fraction.size())
      return "YangM3::f" + convertToString(i);

    else if (i<2*fraction.size())
      return "YangM3::omega" + convertToString(i-fraction.size());

    else
      return s_parameter_name(i,2*fraction.size());
  }

  YangM3::YangM3(const YangM0& M1,const ReversibleFrequencyModel& R, int n) 
    :MultiParameterModel(UnitModel(ReversibleMarkovSuperModel(M1,R)),
			 2*n,0,n)
  {
    // p
    for(int i=0;i<n;i++)
      super_parameters_[i] = 1.0/n;

    // omega
    for(int i=0;i<n;i++)
      super_parameters_[i+n] = 1.0;

    read();

    recalc();
  }



  YangM7::YangM7(const YangM0& M1,const ReversibleFrequencyModel& R, int n) 
    :DistributionParameterModel(UnitModel(ReversibleMarkovSuperModel(M1,R)),
				Beta(),0,n)
  { }

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
    valarray<double> q = get_varray(super_parameters_,n_submodels(),n_submodels());

    return dirichlet_pdf(super_parameters_, 0, n_submodels(), 10.0*q);
  }

  double MixtureModel::super_fiddle(int) 
  {
    const double N = 10;

    // prior on sub-model frequencies
    double ratio = dirichlet_fiddle(super_parameters_, fixed_, 0, n_submodels(), N);

    read();
    recalc();

    return ratio;
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
