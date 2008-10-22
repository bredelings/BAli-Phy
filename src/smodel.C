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



  ExchangeModel::ExchangeModel(unsigned n)
    :S(n,n)
  {}

  efloat_t SimpleExchangeModel::prior() const {
    return laplace_pdf(log(rho()), -3, 1);
  }

  void SimpleExchangeModel::recalc(const vector<int>&)
  {
    double r = rho();
    for(int i=0;i<n_states();i++) {
      for(int j=0;j<n_states();j++)
	S(i,j) = r;

      S(i,i) = 0;       // this is NOT a rate away.
    }
  }

  string SimpleExchangeModel::name() const 
  {
    return "SimpleExchangeModel";
  }

  SimpleExchangeModel::SimpleExchangeModel(unsigned n)
    :ExchangeModel(n)
  {
    add_parameter("rho",exp(-4));
    recalc_all();
  }


  AlphabetExchangeModel::AlphabetExchangeModel(const alphabet& a)
    :ExchangeModel(a.size())
  {}

  //----------------------- Frequency Models ------------------------//

  ReversibleFrequencyModel::ReversibleFrequencyModel(const alphabet& a)
    :R(a.size(),a.size()),
     pi(1.0/a.size(),a.size())
  { }

  void UniformFrequencyModel::frequencies(const valarray<double>&) 
  { }

  void UniformFrequencyModel::recalc(const vector<int>&)
  {
    for(int i=0;i<n_letters();i++)
      pi[i] = 1;
    pi /= pi.sum();
    
    // compute transition rates
    for(int i=0;i<n_letters();i++)
      for(int j=0;j<n_letters();j++)
	R(i,j) = 1;

    // diagonal entries should have no effect
    for(int i=0;i<n_letters();i++)
      R(i,i) = 0;
  }

  string UniformFrequencyModel::name() const {
    return "UF";
  }
  
  UniformFrequencyModel::UniformFrequencyModel(const alphabet& a)
    :ReversibleFrequencyModel(a),
     ModelWithAlphabet<alphabet>(a)
  {
    // initialize everything
    recalc_all();
  }

  UniformFrequencyModel::UniformFrequencyModel(const alphabet& a,const valarray<double>&)
    :ReversibleFrequencyModel(a),
     ModelWithAlphabet<alphabet>(a)
  {
    // initialize everything
    recalc_all();
  }


  void SimpleFrequencyModel::frequencies(const valarray<double>& pi2) 
  {
    // set the frequency parameters
    for(int i=0;i<n_letters();i++)
      parameters_[i+1] = pi2[i];

    // recompute everything
    recalc_all();
  }

  void SimpleFrequencyModel::recalc(const vector<int>&)
  {
    // compute frequencies
    pi = get_varray(parameters_,1,n_letters());
    pi /= pi.sum();
    
    // compute transition rates
    valarray<double> pi_f(n_letters());
    for(int i=0;i<n_letters();i++)
      pi_f[i] = pow(pi[i],f());

    for(int i=0;i<n_letters();i++)
      for(int j=0;j<n_letters();j++)
	R(i,j) = pi_f[i]/pi[i] * pi_f[j];

    // diagonal entries should have no effect
    for(int i=0;i<n_letters();i++)
      R(i,i) = 0;
  }

  efloat_t SimpleFrequencyModel::prior() const 
  {
    // uniform prior on f
    efloat_t Pr = 1;

    // uniform - 1 observeration per letter
    return dirichlet_pdf(parameters_, 1, n_letters(), 1.0);
  }

  string SimpleFrequencyModel::name() const {
    return "pi";
  }
  
  SimpleFrequencyModel::SimpleFrequencyModel(const alphabet& a)
    :ReversibleFrequencyModel(a),
     ModelWithAlphabet<alphabet>(a)
  {
    // Start with *f = 1
    add_parameter("f",1.0);
    fixed_[0] = true;

    for(int i=0;i<n_letters();i++) {
      string pname = string("pi") + Alphabet().letter(i);
      add_parameter(pname, 1.0/n_letters());
    }

    // initialize everything
    recalc_all();
  }

  SimpleFrequencyModel::SimpleFrequencyModel(const alphabet& a,const valarray<double>& pi)
    :ReversibleFrequencyModel(a),
     ModelWithAlphabet<alphabet>(a)
  {
    // Start with *f = 1
    add_parameter("f",1.0);
    fixed_[0] = true;

    valarray<double> f = pi;
    f /= f.sum();
    for(int i=0;i<n_letters();i++) {
      string pname = string("pi") + Alphabet().letter(i);
      add_parameter(pname, f[i]);
    }

    // initialize everything
    recalc_all();
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

  void IndependentNucleotideFrequencyModel::recalc(const vector<int>&)
  {
    //------------------ compute triplet frequencies ------------------//
    pi = triplet_from_singlet_frequencies(Alphabet(),SubModels(0));

    vector<double> sub_parameters = SubModels(0).parameters();

    vector<double> triplet_parameters(n_letters()+1);
    triplet_parameters[0] = sub_parameters[0];
    set_varray(triplet_parameters,1,pi);

    triplets->parameters(triplet_parameters);

    for(int i=0;i<n_letters();i++)
      for(int j=0;j<n_letters();j++)
	R(i,j) = (*triplets)(i,j);
  }

  string IndependentNucleotideFrequencyModel::name() const
  {
    return "pi=nucleotides";
  }

  
  IndependentNucleotideFrequencyModel::IndependentNucleotideFrequencyModel(const Triplets& T) 
    : TripletFrequencyModel(T),
      triplets(SimpleFrequencyModel(T))
  {
    insert_submodel("1",SimpleFrequencyModel(T.getNucleotides()));
    recalc_all();
  }


  void TripletsFrequencyModel::recalc(const vector<int>&)
  {
    valarray<double> nu = get_varray(parameters_, 1, n_letters());

    //------------- compute frequencies ------------------//
    pi = triplet_from_singlet_frequencies(Alphabet(),SubModels(0));

    pi *= nu;

    pi /= pi.sum();


    //------------ compute transition rates -------------//
    double g = parameters_[0];

    valarray<double> nu_g(n_letters());
    for(int i=0;i<n_letters();i++)
      nu_g[i] = pow(nu[i],g);


    // FIXME - can we really handle two mutations?
    // Should the restriction on 1 mutation be HERE?
    for(int i=0;i<n_letters();i++)
      for(int j=0;j<n_letters();j++) {
	R(i,j) = nu_g[i]/nu[i]*nu_g[j];
	for(int k=0;k<3;k++) {
	  int n1 = Alphabet().sub_nuc(i,k);
	  int n2 = Alphabet().sub_nuc(j,k);
	  if (n1 != n2)
	    R(i,j) *= SubModels(0)(n1,n2);
	}
      }

    // diagonal entries should have no effect
    for(int i=0;i<n_letters();i++)
      R(i,i) = 0;
  }

  efloat_t TripletsFrequencyModel::super_prior() const 
  {
    return dirichlet_pdf(parameters_,1,n_letters(),4.0);
  }

  string TripletsFrequencyModel::name() const 
  {
    return "pi=triplets";
  }

  TripletsFrequencyModel::TripletsFrequencyModel(const Triplets& T)
    : TripletFrequencyModel(T)
  {
    add_super_parameter("g", 1);

    for(int i=0;i<n_letters();i++) {
      string pname = string("v") + Alphabet().letter(i);
      add_super_parameter(pname, 1.0/n_letters());
    }

    insert_submodel("1",SimpleFrequencyModel(T.getNucleotides()));

    read();
    recalc_all();
  }

  //------------------- Codon Frequency Model -----------------//
  CodonFrequencyModel::CodonFrequencyModel(const Codons& C)
    :ReversibleFrequencyModel(C),
     ModelWithAlphabet<Codons>(C)
  { }


  void AACodonFrequencyModel::recalc(const vector<int>&)
  {
    //----------- get amino acid frequencies and counts ------------//
    valarray<double> f_aa = SubModels(0).frequencies();
    vector<int> n_aa(aa_size(),0);
    for(int i=0;i<Alphabet().size();i++) {
      int aa = Alphabet().translate(i);
      n_aa[aa]++;
    }
      
    //------------------ compute triplet frequencies ------------------//
    for(int i=0;i<pi.size();i++) {
      int aa = Alphabet().translate(i);
      pi[i] = f_aa[aa]/n_aa[aa];
    }

    vector<double> codon_parameters(n_letters()+1);
    codon_parameters[0] = SubModels(0).parameter(0);
    set_varray(codon_parameters,1,pi);

    codons->parameters(codon_parameters);

    for(int i=0;i<n_letters();i++)
      for(int j=0;j<n_letters();j++)
	R(i,j) = (*codons)(i,j);
  }

  string AACodonFrequencyModel::name() const
  {
    return "pi=amino-acids";
  }

  
  AACodonFrequencyModel::AACodonFrequencyModel(const Codons& C) 
    : CodonFrequencyModel(C),
      codons(SimpleFrequencyModel(C))
  {
    recalc_all();

    insert_submodel("1",SimpleFrequencyModel(C.getAminoAcids()));
  }




  //------------------- Codons Frequency Model -----------------//

  void CodonsFrequencyModel::recalc(const vector<int>&)
  {
    double c = parameters_[0];

    //------------- compute frequencies ------------------//
    valarray<double> aa_pi = get_varray(parameters_, 2, aa_size());

    // get codon frequencies of sub-alphabet
    valarray<double> sub_pi = SubModels(0).frequencies();

    // get aa frequencies of sub-alphabet
    valarray<double> sub_aa_pi(0.0,aa_size());
    for(int i=0;i<n_letters();i++)
      sub_aa_pi[Alphabet().translate(i)] += sub_pi[i];

    // get factors by which to multiply sub-alphabet frequencies
    valarray<double> factor(n_letters());
    for(int i=0;i<n_letters();i++) 
    {
      int j = Alphabet().translate(i);
      factor[i] = pow(aa_pi[j]/sub_aa_pi[j],c);
    }

    // compute aa-aware codon frequencies
    for(int i=0;i<n_letters();i++) 
      pi[i] = sub_pi[i] * factor[i];

    // scale so as to sum to 1
    pi /= pi.sum();


    //------------ compute transition rates -------------//
    double h = parameters_[1];

    valarray<double> factor_h(n_letters());
    for(int i=0;i<n_letters();i++)
      factor_h[i] = pow(factor[i],h);


    for(int i=0;i<n_letters();i++)
      for(int j=0;j<n_letters();j++)
	R(i,j) = SubModels(0)(i,j) * factor_h[i]/factor[i]*factor_h[j];

    // diagonal entries should have no effect
    for(int i=0;i<n_letters();i++)
      R(i,i) = 0;
  }

  efloat_t CodonsFrequencyModel::super_prior() const 
  {
    return dirichlet_pdf(parameters_, 2, aa_size(), 2.0);
  }

  string CodonsFrequencyModel::name() const 
  {
    return "pi=codons";
  }

  CodonsFrequencyModel::CodonsFrequencyModel(const Codons& C)
    : CodonFrequencyModel(C)
  {
    add_super_parameter("c", 0.5);
    add_super_parameter("h", 0.5);

    for(int i=0;i<C.getAminoAcids().size();i++) {
      string pname = string("b_") + Alphabet().getAminoAcids().letter(i);
      add_super_parameter(pname, 1.0/C.getAminoAcids().size());
    }

    insert_submodel("1",TripletsFrequencyModel(C));

    read();
    recalc_all();
  }

  //------------------- Codons Frequency Model 2 -----------------//

  void CodonsFrequencyModel2::recalc(const vector<int>&)
  {
    //------------- compute frequencies ------------------//
    valarray<double> aa_pref_ = get_varray(parameters_, 1, aa_size());

    valarray<double> aa_pref(n_letters());
    for(int i=0;i<n_letters();i++)
      aa_pref[i] = aa_pref_[Alphabet().translate(i)];

    // get codon frequencies of sub-alphabet
    valarray<double> sub_pi = SubModels(0).frequencies();

    // scale triplet frequencies by aa prefs
    for(int i=0;i<n_letters();i++) 
      pi[i] = sub_pi[i] * aa_pref[i];

    // scale so as to sum to 1
    pi /= pi.sum();


    //------------ compute transition rates -------------//
    double h = parameters_[0];

    valarray<double> aa_pref_h(n_letters());
    for(int i=0;i<n_letters();i++)
      aa_pref_h[i] = pow(aa_pref[i], h);


    for(int i=0;i<n_letters();i++)
      for(int j=0;j<n_letters();j++)
	R(i,j) = SubModels(0)(i,j) * aa_pref_h[i]/aa_pref[i] * aa_pref_h[j];

    // diagonal entries should have no effect
    for(int i=0;i<n_letters();i++)
      R(i,i) = 0;
  }

  efloat_t CodonsFrequencyModel2::super_prior() const 
  {
    return dirichlet_pdf(parameters_, 1, aa_size(), 2.0);
  }

  string CodonsFrequencyModel2::name() const 
  {
    return "pi=codons";
  }

  CodonsFrequencyModel2::CodonsFrequencyModel2(const Codons& C)
    : CodonFrequencyModel(C)
  {
    add_super_parameter("h", 0.5);

    for(int i=0;i<C.getAminoAcids().size();i++) {
      string pname = string("b_") + Alphabet().getAminoAcids().letter(i);
      add_super_parameter(pname, 1.0/C.getAminoAcids().size());
    }

    insert_submodel("1",TripletsFrequencyModel(C));

    read();
    recalc_all();
  }


  /// Construct a Makov model on alphabet 'a'
  MarkovModel::MarkovModel(const alphabet& a)
    :Q(a.size(),a.size()),state_letters_(a.size())
  {
    for(int i=0;i<a.size();i++)
      state_letters_[i] = i;
  }
  
  //----------------------- ReversibleMarkovModel --------------------------//
  // Q(i,j) = S(i,j)*pi[j]   for i!=j
  // Q(i,i) = -sum_{i!=j} S(i,j)*pi[j]

  // We want to set S(i,i) so that Q(i,j) = S(i,j)*pi[j] for all i,j
  // Then Q = S*D, and we can easily compute the exponential
  // So, S(i,j) = Q(i,i)/pi[i]

  double ReversibleMarkovModel::rate() const 
  {
    const unsigned N = n_states();
    
    double scale=0;

    if (N == Alphabet().size()) 
    {
      for(int i=0;i<Q.size1();i++) 
	scale -= frequencies()[i]*Q(i,i);
    }
    else 
    {
      const vector<unsigned>& smap = state_letters();

      for(int s1=0;s1<N;s1++)
      {
	double temp = 0;
	for(int s2=0;s2<N;s2++)
	  if (smap[s1] != smap[s2])
	    temp += Q(s1,s2);

	scale += temp*frequencies()[s1];
      }
    }

    return scale/Alphabet().width();
  }

  void ReversibleMarkovModel::set_rate(double r)  {
    double scale = r/rate();
    if (rate() == 0) {
      if (r == 0)
	scale = 1;
      else
	throw myexception()<<"Model rate is 0, can't set it to "<<r<<".";
    }
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

  void ReversibleMarkovModel::recalc_eigensystem()
  {
    const unsigned n = n_states();

#ifdef DEBUG_RATE_MATRIX
    std::cerr<<"scale = "<<rate()<<endl;

    assert(std::abs(frequencies().sum()-1.0) < 1.0e-6);
    for(int i=0;i<n;i++) {
      double sum = 0;
      for(int j=0;j<n;j++)
	sum += Q(i,j);
      assert(abs(sum) < 1.0e-6);
    }
#endif

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
      for(int j=0;j<=i;j++) {
	S(i,j) = Q(i,j) * sqrt_pi[i] * inverse_sqrt_pi[j];

#ifdef DEBUG_RATE_MATRIX
	// check reversibility of rate matrix
	if (i != j) {
	  assert (S(i,j) >= 0);
	  double p12 = Q(i,j)*frequencies()[i];
	  double p21 = Q(j,i)*frequencies()[j];
	  assert (abs(p12-p21) < 1.0e-12*(1.0+abs(p12)));
	}
	else
	  assert (Q(i,j) <= 0);
#endif
      }

    //---------------- Compute eigensystem ------------------//
    eigensystem = EigenValues(S);
  }

  Matrix ReversibleMarkovModel::transition_p(double t) const 
  {
    vector<double> pi(n_states());
    const valarray<double> f = frequencies();
    assert(pi.size() == f.size());
    for(int i=0;i<pi.size();i++)
      pi[i] = f[i];
    return exp(eigensystem,pi,t);
  }

  ReversibleMarkovModel::ReversibleMarkovModel(const alphabet& a)
    :MarkovModel(a), 
     eigensystem(a.size())
  { }

  void ReversibleMarkovSuperModel::recalc(const vector<int>&)
  {
    const unsigned N = n_states();

    // recompute rate matrix
    for(int i=0;i<N;i++) {
      double sum=0;
      for(int j=0;j<N;j++) {
	if (i==j) continue;
	Q(i,j) = (*S)(i,j) * (*R)(i,j);
	sum += Q(i,j);
      }
      Q(i,i) = -sum;
    }

    recalc_eigensystem();
  }

  string ReversibleMarkovSuperModel::name() const {
    return S->name() + " * " + R->name();
  }

  /// Construct a reversible Markov model on alphabet 'a'
  ReversibleMarkovSuperModel::ReversibleMarkovSuperModel(const AlphabetExchangeModel& S1,const ReversibleFrequencyModel& R1)
    :ReversibleMarkovModel(S1.Alphabet()),
     ModelWithAlphabet<alphabet>(S1.Alphabet()),
     S(S1),
     R(R1)
  {
    add_submodel("S",*S);
    add_submodel("R",*R);

    read();
    recalc_all();
  }
    


  void SimpleReversibleMarkovModel::frequencies(const valarray<double>& pi) 
  {
    SimpleFrequencyModel* R2 = dynamic_cast<SimpleFrequencyModel*>(R.get());
    R2->frequencies(pi);
    read();
    recalc_all();
  }

  SimpleReversibleMarkovModel::SimpleReversibleMarkovModel(const AlphabetExchangeModel& E)
      :ReversibleMarkovSuperModel(E,SimpleFrequencyModel(E.Alphabet()))
  { }

  SimpleReversibleMarkovModel::
  SimpleReversibleMarkovModel(const AlphabetExchangeModel& E,const valarray<double>& pi)
      :ReversibleMarkovSuperModel(E,SimpleFrequencyModel(E.Alphabet(),pi))
  { }

  //---------------------- INV_Model --------------------------//

  string INV_Model::name() const 
  {
    return "INV";
  }

  INV_Model::INV_Model(const alphabet& a)
    :AlphabetExchangeModel(a),ModelWithAlphabet<alphabet>(a)
  {
    add_parameter("INV::f",1);

    // Calculate S matrix
    for(int i=0;i<S.size1();i++)
      for(int j=0;j<S.size2();j++)
	S(i,j) = 0;
  }
      
  //----------------------- EQU -------------------------//

  string EQU::name() const {
    return "EQU";
  }

  EQU::EQU(const alphabet& a) 
    :AlphabetExchangeModel(a),ModelWithAlphabet<alphabet>(a)
  {
    for(int i=0;i<n_states();i++)
      for(int j=0;j<n_states();j++)
	S(i,j) = 1;
  }

  //----------------------- Empirical -------------------------//

  string Empirical::name() const {
    return "Empirical(" + modelname +")";
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
    :AlphabetExchangeModel(a),ModelWithAlphabet<alphabet>(a)
  { 
    load_file(filename); 
  }

  //------------------------- HKY -----------------------------//
  string HKY::name() const {
    return "HKY";
  }

  efloat_t HKY::prior() const 
  {
    return laplace_pdf(log(kappa()), log(2), 0.25);
  }

  void HKY::recalc(const vector<int>&) {
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
    add_parameter("HKY::kappa", 2);
    recalc_all();
  }

  //------------------------- TN -----------------------------//
  string TN::name() const {
    return "TN";
  }

  // This should be OK - the increments are linear combinations of gaussians...

  /// return the LOG of the prior
  efloat_t TN::prior() const 
  {
    efloat_t P = 1;
    P *= laplace_pdf(log(kappa1()), log(2), 0.25);
    P *= laplace_pdf(log(kappa2()), log(2), 0.25);
    return P;
  }

  void TN::recalc(const vector<int>&) { 
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

  /// Construct a TN model on alphabet 'a'
  TN::TN(const Nucleotides& N)
    : NucleotideExchangeModel(N)
  { 
    add_parameter("TN::kappa(pur)",2);
    add_parameter("TN::kappa(pyr)",2);
    recalc_all();
  }

  string GTR::name() const {
    return "GTR";
  }

  // This should be OK - the increments are linear combinations of gaussians...

  /// return the LOG of the prior
  efloat_t GTR::prior() const 
  {
    valarray<double> n(6);

    n[0] = 2; // AG - transition

    n[1] = 1; // AT - transversion

    n[2] = 1; // AC - transversion

    n[3] = 1; // GT - transversion

    n[4] = 1; // GC - transversion

    n[5] = 2; // TC - transition

    n *= 4;

    return dirichlet_pdf(parameters_, n);
  }

  void GTR::recalc(const vector<int>&) 
  {
    assert(Alphabet().size()==4);

    double total = 0;
    for(int i=0;i<6;i++)
      total += parameters_[i];

    for(int i=0;i<6;i++)
      parameters_[i] /= total;

    S(0,1) = parameters_[0]; // AG
    S(0,2) = parameters_[1]; // AT
    S(0,3) = parameters_[2]; // AC

    S(1,2) = parameters_[3]; // GT
    S(1,3) = parameters_[4]; // GC

    S(2,3) = parameters_[5]; // TC
  }

  GTR::GTR(const Nucleotides& N)
      : NucleotideExchangeModel(N)
    { 
      add_parameter("GTR::AG", 1.0/6);
      add_parameter("GTR::AT", 1.0/6);
      add_parameter("GTR::AC", 1.0/6);
      add_parameter("GTR::GT", 1.0/6);
      add_parameter("GTR::GC", 1.0/6);
      add_parameter("GTR::TC", 1.0/6);

      recalc_all();
    }

  //------------------------ Triplet Models -------------------//

  TripletExchangeModel::TripletExchangeModel(const Triplets& T)
    :AlphabetExchangeModel(T),ModelWithAlphabet<Triplets>(T)
  { }

  void SingletToTripletExchangeModel::recalc(const vector<int>&)
  {
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

	  S(i,j) = SubModels(0)(l1,l2);
	}
      }
  }

  string SingletToTripletExchangeModel::name() const {
    string n = SubModels(0).name();
    n += "x3";
    return n;
  }
  
  SingletToTripletExchangeModel::SingletToTripletExchangeModel(const Triplets& T,const NucleotideExchangeModel& N)
    :TripletExchangeModel(T)
  { 
    insert_submodel("1",N);

    read();
    recalc_all();
  }

  //------------------------ Codon Models -------------------//

  CodonAlphabetExchangeModel::CodonAlphabetExchangeModel(const Codons& C)
    :AlphabetExchangeModel(C),ModelWithAlphabet<Codons>(C)
  { }

  /// Get the parameter 'omega' (non-synonymous/synonymous rate ratio)
  double M0::omega() const {
    return parameter(0);
  }

  /// Set the parameter 'omega' (non-synonymous/synonymous rate ratio)
  void M0::omega(double w) {
    parameter(0,w);
  }

  void M0::recalc(const vector<int>&)
  {
    for(int i=0;i<Alphabet().size();i++) 
    {
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

	  rate = SubModels(0)(l1,l2);

	  if (AminoAcid(i) != AminoAcid(j))
	    rate *= omega();	
	}

	S(i,j) = S(j,i) = rate;
      }
    }
  }

  efloat_t M0::super_prior() const {
    return laplace_pdf(log(omega()), 0, 0.1);
  }

  efloat_t M0::prior() const {
    return SuperModelOver<NucleotideExchangeModel>::prior();
  }

  string M0::name() const {
    return string("M0[") + SubModels(0).name() + "]";
  }

  M0::M0(const Codons& C,const NucleotideExchangeModel& N)
    :CodonAlphabetExchangeModel(C)
  { 
    add_super_parameter("M0::omega", 1.0);
    insert_submodel("1",N);
    recalc_all();
  }

  M0::~M0() {}

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
    Matrix f(M.n_base_models(),M.n_states());
    for(int m=0;m<f.size1();m++)
      for(int l=0;l<f.size2();l++)
	f(m,l) = M.base_model(m).frequencies()[l];
    return f;
  }

  string UnitModel::name() const {
    return string("[") + SubModels(0).name() + "]";
  }

  UnitModel::UnitModel(const Base_Model_t& M)
    :ReversibleWrapperOver<Base_Model_t>(M)
  { }

  //---------------------- MultiFrequencyModel -----------------------//
  efloat_t MultiFrequencyModel::super_prior() const 
  {
    const int n = fraction.size();

    efloat_t Pr = 1;

    for(int l=0;l<Alphabet().size();l++) 
    {
      valarray<double> a_l(n);
      for(int m=0;m<a_l.size();m++)
	a_l[m] = a(m,l);

      Pr *= ::dirichlet_pdf(a_l, n/2.0);
    }

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

  void MultiFrequencyModel::recalc(const vector<int>&) 
  {
    // get underlying frequencies from our submodel
    valarray<double> f = frequencies();

    // calculate probability of each sub-model
    for(int m=0;m<fraction.size();m++) 
    {
      // Pr(m) = sum_l Pr(m|l)*Pr(l)
      fraction[m] = 0;
      for(int l=0;l<Alphabet().size();l++)
	fraction[m] += a(m,l)*f[l];
    }

    if (std::abs(sum(fraction) - 1.0) > 1.0e-5) std::cerr<<"ERROR: sum(fraction) = "<<sum(fraction)<<endl;

    // recalculate sub-models
    valarray<double> fm(Alphabet().size());
    for(int m=0;m<fraction.size();m++) 
    {
      // Pr(l|m) = Pr(m|l)*Pr(l)/Pr(m)
      for(int l=0;l<fm.size();l++)
	fm[l] = a(m,l)*f[l]/fraction[m];

      if (std::abs(fm.sum() - 1.0) > 1.0e-5) std::cerr<<"ERROR[m="<<m<<"]: fm.sum() = "<<fm.sum()<<endl;

      // get a new copy of the sub-model and set the frequencies
      sub_parameter_models[m] = &SubModel();
      sub_parameter_models[m]->frequencies(fm);
    }
  }

  string MultiFrequencyModel::name() const {
    return SubModels(0).name() + " + multi_freq[" + 
      convertToString(fraction.size()) + "]";
  }

  MultiFrequencyModel::MultiFrequencyModel(const AlphabetExchangeModel& E,int n)
    :ReversibleWrapperOver<SimpleReversibleMarkovModel>(SimpleReversibleMarkovModel(E)),
     fraction(n)
  { 
    sub_parameter_models.resize(n);
    for(int i=0;i<n;i++)
      sub_parameter_models[i] = SubModel();

    // Set up variable names
    //   - initial probability that a letter l is in a submodel of type m = 1/n
    for(int l=0;l<Alphabet().size();l++) 
    {
      string letter = Alphabet().lookup(l);

      for(int m=0;m<n;m++) 
      {
	string index = convertToString(m+1);
	string pname = string("a") + letter + index;
	add_super_parameter(pname, 1.0/n);
      }
    }

    read();
    recalc_all();
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

  void MultiParameterModel::recalc_submodel_instances()
  {
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

  MultiParameterModel::MultiParameterModel(const MultiModel& M,int p,int n) 
    :ReversibleWrapperOver<MultiModel>(M),
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

  //--------------- Dirichlet-based Model----------------//

  efloat_t DirichletParameterModel::super_prior() const
  {
    efloat_t Pr  = 1;

    // Prior on the fractions
    double n_f = 1.0 + p_values.size()/2.0;
    Pr *= dirichlet_pdf(parameters_,0              ,p_values.size(),n_f);

    // Prior on the rates
    double n_r = 2.0; // + p_values.size()/2.0;
    Pr *= dirichlet_pdf(parameters_,p_values.size(),p_values.size(),n_r);

    return Pr;
  }

  void DirichletParameterModel::recalc(const vector<int>&) 
  {
    /*
    // sort bins to enforce monotonically increasing order
    vector<double> values;
    for(int i=0;i<p_values.size();i++)
      values.push_back(parameters_[i+p_values.size()]);
    
    vector<int> order = iota<int>(p_values.size());

    sort(order.begin(), order.end(), sequence_order<double>(values));

    vector<double> p2 = parameters_;
    for(int i=0;i<p_values.size();i++) {
      p2[i] = parameters_[order[i]];
      p2[i+p_values.size()] = parameters_[order[i]+p_values.size()];
    }
    parameters_ = p2;
    */

    // write parameter values to fraction / p_values
    for(int i=0;i<p_values.size();i++)
    {
      fraction[i] = parameters_[i];
      p_values[i] = parameters_[i+p_values.size()];
    }
    
    // We need to do this when either P_values changes, or the SUBMODEL changes
    recalc_submodel_instances();
  }

  string DirichletParameterModel::name() const {
    string p_name = "rate";
    if (p_change > -1)
      p_name = SubModels(0).parameter_name(p_change);

    string dist_name = p_name + "~Dirichlet[" + convertToString(p_values.size()) + "]";
    return SubModels(0).name() + " + " + dist_name;
  }

  DirichletParameterModel::DirichletParameterModel(const MultiModel& M, int p, int n)
    :MultiParameterModel(M,p,n)
  {
    // bin frequencies
    for(int i=0;i<n;i++) {
      string pname = "DP::f" + convertToString(i+1);
      add_super_parameter(pname, 1.0/n);
    }

    // bin values
    string p_name = "DP::rate";
    if (p >= 0) p_name = string("DP::") + M.parameter_name(p);
    for(int i=0;i<n;i++)
      add_super_parameter(p_name+convertToString(i+1), 1.0);

    read();
    recalc_all();
  }

  //--------------- Distribution-based Model----------------//

  efloat_t DistributionParameterModel::super_prior() const
  {
    if (not good_enough)
      return 0.0;
    else
      return 1.0;
  }

  Distribution& DistributionParameterModel::D()
  {
    return SubModelAs<Distribution>(1);
  }

  const Distribution& DistributionParameterModel::D() const
  {
    return SubModelAs<Distribution>(1);
  }

  void DistributionParameterModel::recalc(const vector<int>&) 
  {
    // We only need to do this when the DISTRIBUTION changes (?)
    Discretization d(p_values.size(),D());
    double ratio = d.scale()/D().mean();

    good_enough = (ratio > 1.0/1.5 and ratio < 1.5);

    d.scale(1.0/ratio);

    fraction = d.f;
    p_values = d.r;
    
    // We need to do this when either P_values changes, or the SUBMODEL changes
    recalc_submodel_instances();
  }

  string DistributionParameterModel::name() const {
    string p_name = "rate";
    if (p_change > -1)
      p_name = SubModels(0).parameter_name(p_change);

    string dist_name = p_name + "~" + D().name() + "(" + convertToString(p_values.size()) + ")";
    return SubModels(0).name() + " + " + dist_name;
  }

  DistributionParameterModel::DistributionParameterModel(const MultiModel& M,const Distribution& RD, int p, int n)
    :MultiParameterModel(M,p,n),
     good_enough(false)
  {
    insert_submodel("DIST",RD);

    read();
    recalc_all();
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
    return SubModel().frequencies();
  }

  void WithINV::recalc(const vector<int>&) {
    INV->frequencies(SubModel().frequencies());
  }

  string WithINV::name() const {
    return SubModel().name() + " + INV";
  }

  efloat_t WithINV::super_prior() const {
    double p = parameter(0);

    return beta_pdf(p, 1, 2);
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

  vector<double> WithINV::distribution() const {
    double p = parameter(0);

    vector<double> dist = SubModel().distribution();
    for(int i=0;i<dist.size();i++)
      dist[i] *= (1-p);

    dist.push_back(p);
    return dist;
  }

  WithINV::WithINV(const MultiModel& M)
    :ReversibleWrapperOver<MultiModel>(M),
     INV(SimpleReversibleMarkovModel(INV_Model(M.Alphabet())))
  {
    add_super_parameter("INV::p", 0.01);

    read();
    recalc_all();
  }


  //--------------- Invariant Sites Model 2 ----------------//

  const double WithINV2::inv_frac_mean = 0.1;
  const double WithINV2::max_inv_rate = 0.01;

  /// Get the equilibrium frequencies
  //  const std::valarray<double>& WithINV2::frequencies() const {
  //    return VAR->frequencies();
  //  }

  const MultiModel& WithINV2::VAR() const {
    return SubModelAs<MultiModel>(0);
  }

  MultiModel& WithINV2::VAR() {
    return SubModelAs<MultiModel>(0);
  }

  const SimpleReversibleMarkovModel& WithINV2::INV() const {
    return SubModelAs<SimpleReversibleMarkovModel>(1);
  }

  SimpleReversibleMarkovModel& WithINV2::INV() {
    return SubModelAs<SimpleReversibleMarkovModel>(1);
  }

  void WithINV2::recalc(const vector<int>&) 
  {
    double p = parameter(0);

    freq = (1-p)*VAR().frequencies() + p*INV().frequencies();
  }

  string WithINV2::name() const 
  {
    return VAR().name() + " + INV2";
  }

  efloat_t WithINV2::super_prior() const 
  {
    double p = parameter(0);

    return beta_pdf(p, 1, 2);
  }

    /// Access the base models
  const MultiModel::Base_Model_t& WithINV2::base_model(int m) const {
    if (m<VAR().n_base_models())
      return VAR().base_model(m);
    else
      return INV();
  }

  MultiModel::Base_Model_t& WithINV2::base_model(int m) {
    if (m<VAR().n_base_models())
      return VAR().base_model(m);
    else
      return INV();
  }

  vector<double> WithINV2::distribution() const {
    double p = parameter(0);

    vector<double> dist = VAR().distribution();
    for(int i=0;i<dist.size();i++)
      dist[i] *= (1-p);

    dist.push_back(p);
    return dist;
  }

  const valarray<double>& WithINV2::frequencies() const {
    return freq;

  }

//  NOTE: Shouldn't we have a generic 'frequencies' calculation if we need one?
//  ????: And do we need one?

  WithINV2::WithINV2(const MultiModel& M)
    :freq(M.frequencies())
  {
    add_super_parameter("INV::p", 0.01);
    insert_submodel("VAR", M);
    insert_submodel("INV", SimpleReversibleMarkovModel(INV_Model(M.Alphabet())));

    read();
    recalc_all();
  }



  //-------------------- M2 --------------------//
  void M2::recalc(const vector<int>&) 
  {
    fraction[0] = parameter(0);
    fraction[1] = parameter(1);
    fraction[2] = parameter(2);

    p_values[0] = 0;
    p_values[1] = 1;
    p_values[2] = parameter(3);

    recalc_submodel_instances();
  }

  efloat_t M2::super_prior() const 
  {
    // prior on frequencies
    valarray<double> n(3);
    n[0] = 1;
    n[1] = 98;
    n[2] = 1;
    efloat_t P = dirichlet_pdf(parameters_, 0, 3, n);

    // prior on omega
    double omega = parameter(3);
    P *= exponential_pdf(log(omega),0.05);
    return P;
  }

  string M2::name() const {
    return SubModels(0).name() + " + M2";
  }

  M2::M2(const M0& M1,const ReversibleFrequencyModel& R) 
    :MultiParameterModel(UnitModel(ReversibleMarkovSuperModel(M1,R)),0,3)
  {
    add_super_parameter("M2::f[AA INV]",   1.0/3);
    add_super_parameter("M2::f[Neutral]",  1.0/3);
    add_super_parameter("M2::f[Selected]", 1.0 - parameters_[0] - parameters_[1]);
    add_super_parameter("M2::omega", 1.0);

    read();
    recalc_all();
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

  //M3

  double M3::omega(int i) const {
    return parameter(fraction.size() + i);
  }

  /// Set the parameter 'omega' (non-synonymous/synonymous rate ratio)
  void M3::omega(int i,double w) {
    parameter(fraction.size()+i,w);
  }

  //NOTE: we used to enforce ORDER for the M3 rates!

  void M3::recalc(const vector<int>&) 
  {
    for(int i=0;i<fraction.size();i++)
      fraction[i] = parameter(i);

    for(int i=0;i<fraction.size();i++)
      p_values[i] = parameter(fraction.size()+i);

    recalc_submodel_instances();
  }

  efloat_t M3::super_prior() const 
  {
    efloat_t P = 1;

    // prior on frequencies
    P *= dirichlet_pdf(parameters_, 0, fraction.size(), 4);

    // prior on rates
    for(int i=0;i<fraction.size();i++)
      P *= laplace_pdf(log(omega(i)), 0, 0.1);

    return P;
  }

  string M3::name() const {
    return SubModels(0).name() + " + M3[" + convertToString(fraction.size()) + "]";
  }

  M3::M3(const M0& M1,const ReversibleFrequencyModel& R, int n) 
    :MultiParameterModel(UnitModel(ReversibleMarkovSuperModel(M1,R)),0,n)
  {
    // p
    for(int i=0;i<n;i++) {
      string pname = "M3::f" + convertToString(i+1);
      add_super_parameter(pname, 1.0/n);
    }

    // omega
    for(int i=0;i<n;i++) {
      string pname = "M3::omega" + convertToString(i+1);
      add_super_parameter(pname, 1.0);
    }

    read();
    recalc_all();
  }



  M7::M7(const M0& M1,const ReversibleFrequencyModel& R, int n) 
    :DistributionParameterModel(UnitModel(ReversibleMarkovSuperModel(M1,R)),
				Beta(),0,n)
  { 
  }

  int MixtureModel::n_base_models() const 
  {
    int total=0;
    for(int i=0; i<n_submodels(); i++)
      total += SubModels(i).n_base_models();
    return total;
  }


  void MixtureModel::recalc(const vector<int>&) 
  {
    //recalculate pi
    pi = 0;
    int sm_total = n_submodels();
    for(int sm=0;sm<sm_total;sm++)
      pi += parameter(sm)*SubModels(sm).frequencies();
  }

  efloat_t MixtureModel::super_prior() const 
  {
    valarray<double> p = get_varray(parameters_,0,n_submodels());
    valarray<double> q = get_varray(parameters_,n_submodels(),n_submodels());

    return dirichlet_pdf(parameters_, 0, n_submodels(), 10.0*q);
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
      double f = parameter(sm);
      for(int i=0;i<SubModels(sm).n_base_models();i++)
	dist[m++] = f*SubModels(0).distribution()[i];
    }
    
    return dist;
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
  {
    for(int i=0;i<models.size();i++) {
      string pname = string("Mixture::p") + convertToString(i+1);
      add_super_parameter(pname, 1.0/models.size());
      insert_submodel(string("M")+convertToString(i+1),*models[i]);
    }

    for(int i=0;i<models.size();i++) {
      string pname = string("Mixture::prior") + convertToString(i+1);
      add_super_parameter(pname, 1.0/models.size());
    }

    pi.resize(Alphabet().size());

    read();
    recalc_all();
  }


  string ModulatedMarkovModel::name() const 
  {
    return M->name() + "+" + S->name();
  }

  // We want Q(mi -> mj) = Q[m](i -> j)   for letter exchange
  //         Q(mi -> ni) = R(m->n)        for model exchange
  // and     Q(mi -> nj) = 0              for all other pairs

  // We assume that R(m->n) = S(m,n) * M->distribution()[n]

  // This should result in a Markov chain where the frequencies are
  //  frequencies()[mi] = pi[i] * f[m] 
  // with pi = M->frequencies() 
  // and   f = M->distribution()

  // PROBLEM: I don't have a good way of defining the switching rate.
  // Right now, I have S(m,n) = rho, S(m,m) = 0
  // But, the S(m,n) do not correspond to switching rates exactly.
  // Instead, the switching rate is now rho*f[n], which is going to
  // be something like rho*(n-1)/n if there are n categories.
  
  // ADDITIONALLY, depending on how fine-grained the categories are,
  // a switching rate has a different interpretation.

  // HOWEVER, I think the current approach works for now, because it
  // approximates the model that at rate 'rho' the rate is randomly
  // re-drawn from the underlying distribution.  A lot of the time it
  // will fall in the same bin, giving a lower observed switching rate
  // when the discrete approximation to the continuous distribution has
  // low resolution.

  void ModulatedMarkovModel::recalc(const vector<int>&) 
  {
    const int n_models = M->n_base_models();

    M->set_rate(1);

    const valarray<double>& M_pi = M->frequencies();
    const vector<double>&   M_f  = M->distribution();

    // calculate pi[ ] for each state
    unsigned T = 0;
    for(int m=0; m < n_models; m++) {
      unsigned N = M->base_model(m).n_states();
      for(int s=0; s < N; s++) 
	pi[T+s] = M_pi[s] * M_f[m];
      T += N;
    }
    

    // initially zero out the matrix
    for(int i=0;i<Q.size1();i++)
      for(int j=0;j<Q.size2();j++)
	Q(i,j) = 0;

    // rates for within-model transitions
    T=0;
    for(int m=0; m < n_models; m++) 
    {
      const ReversibleMarkovModel* RM = dynamic_cast<const ReversibleMarkovModel*>(&M->base_model(m));
      if (not RM)
	throw myexception()<<"Can't construct a modulated Markov model from non-Markov model '"<<M->base_model(m).name()<<"'";

      unsigned N = RM->n_states();
      

      const Matrix& QM = RM->transition_rates();
      for(int s1=0; s1 < N; s1++) 
	for(int s2=0; s2 < N; s2++)
	  Q(T+s1,T+s2) = QM(s1,s2);

      T += N;
    }

    // rates for between-model transitions
    unsigned T1=0;
    for(int m1=0; m1 < n_models; m1++) 
    {
      const ReversibleMarkovModel* RM1 = dynamic_cast<const ReversibleMarkovModel*>(&M->base_model(m1));
      unsigned N1 = RM1->n_states();

      unsigned T2=0;
      for(int m2=0; m2 < n_models; m2++) 
      {
	const ReversibleMarkovModel* RM2 = dynamic_cast<const ReversibleMarkovModel*>(&M->base_model(m2));
	unsigned N2 = RM2->n_states();
	assert(N1 == N2);

	if (m1 != m2) {
	  double S12 = (*S)(m1,m2);
	  for(int s1=0;s1<N1;s1++)
	    Q(T1+s1,T2+s1) = S12*M_f[m2];
	}

	T2 += N2;
      }
      T1 += N1;
    }

    // recompute diagonals 
    for(int i=0;i<Q.size1();i++) 
    {
      double sum=0;
      for(int j=0;j<Q.size2();j++)
	if (i!=j)
	  sum += Q(i,j);
      Q(i,i) = -sum;
    }

    recalc_eigensystem();
  }

  ModulatedMarkovModel::ModulatedMarkovModel(const MultiModel& MM, const ExchangeModel& EM)
    :ReversibleMarkovModel(MM.Alphabet()),M(MM),S(EM)
  {
    unsigned T = 0;
    for(int m=0; m < M->n_base_models(); m++) 
    {
      const ReversibleMarkovModel* RM = dynamic_cast<const ReversibleMarkovModel*>(&M->base_model(m));
      if (not RM)
	throw myexception()<<"Can't construct a modulated Markov model from non-Markov model '"<<M->base_model(m).name()<<"'";
      T += RM->n_states();
    }

    // resize for larger state set
    pi.resize(T);

    Q.resize(T,T);

    state_letters_.resize(T);

    // calculate the state_letters() map here!

    T = 0;
    for(int m=0; m < M->n_base_models(); m++) 
    {
      unsigned N = M->base_model(m).n_states();
      for(int i=0; i<N; i++)
	state_letters_[T+i] = M->base_model(m).state_letters()[i];

      T += N;
    }

    add_submodel("M",*M);
    add_submodel("S",*S);

    read();
    recalc_all();
  }
  
}
